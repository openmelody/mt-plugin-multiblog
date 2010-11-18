package MultiBlog::Plugin;

use strict;
use warnings;
use base qw( MT::Plugin );

sub DENIED  () { 1 }
sub ALLOWED () { 2 }

sub plugin {
    return MT->component('multiblog');
}

sub trigger_loop {
    [
        {
            trigger_key  => 'entry_save',
            trigger_name => plugin()->translate('saves an entry'),
        },
        {
            trigger_key  => 'tagged_entry_save',
            trigger_name => plugin()->translate('saves an entry with tag'),
        },
        {
            trigger_key  => 'entry_pub',
            trigger_name => plugin()->translate('publishes an entry'),
        },
        {
            trigger_key  => 'tagged_entry_pub',
            trigger_name => plugin()->translate('publishes an entry with tag'),
        },
        {
            trigger_key  => 'comment_pub',
            trigger_name => plugin()->translate('publishes a comment'),
        },
        {
            trigger_key  => 'tb_pub',
            trigger_name => plugin()->translate('publishes a TrackBack'),
        },
    ];
}

sub add_trigger {
    my $app = shift;

    return plugin()->translate("Permission denied.")
        unless $app->user->is_superuser() ||
               ($app->blog && $app->user->permissions($app->blog->id)->can_administer_blog());

    my $blog_id = $app->blog->id;

    my $actions = plugin()->registry->{multiblog_actions};
    my $action_loop = [ map { {action_id => $_, action_name => $actions->{$_}->{label}} } keys %$actions ];

    my $dialog_tmpl = plugin()->load_tmpl('dialog_create_trigger.tmpl');
    my $tmpl = $app->listing({
        template => $dialog_tmpl,
        type => 'blog',
        code => sub {
            my ($obj, $row) = @_;
            if ($obj) {
                $row->{label} = $obj->name;
                $row->{link} = $obj->site_url;
            }
        },
        terms => {
            id => [ $blog_id ],
        },
        args => {
            not => { id => 1 },
        },
        params => {
            panel_type => 'blog',
            dialog_title => plugin()->translate('MultiBlog'),
            panel_title => plugin()->translate('Create Trigger'),
            panel_label => plugin()->translate("Weblog Name"),
            search_prompt => plugin()->translate("Search Weblogs") . ':',
            panel_description => plugin()->translate("Description"),
            panel_multi => 0,
            panel_first => 1,
            panel_last => 1,
            panel_searchable => 1,
            multiblog_trigger_loop => trigger_loop(),
            multiblog_action_loop => $action_loop,
            list_noncron => 1,
            trigger_caption => plugin()->translate('When this'),
        },
    });
    if (!$app->param('search')) {
        if (my $loop = $tmpl->param('object_loop')) {
            unshift @$loop, {
                id => '_all',
                label => plugin()->translate('* All Weblogs'),
                description => plugin()->translate('Select to apply this trigger to all weblogs'),
            };
        }
    }
    return $app->build_page($tmpl);
}


sub load_config {
    my $plugin = shift;
    my ($args, $scope) = @_;

    plugin()->SUPER::load_config(@_);

    if ( $scope =~ /blog:(\d+)/ ) {
        my $blog_id = $1;

        require MT::Blog;

	my $actions = plugin()->registry->{multiblog_actions};

        $args->{multiblog_trigger_loop} = trigger_loop();
        my %triggers =
            map { $_->{trigger_key} => $_->{trigger_name} }
                @{ $args->{multiblog_trigger_loop}};

        my $rebuild_triggers = $args->{rebuild_triggers};
        my @rebuilds = map {
            my ( $action, $id, $trigger, $trigger_tags ) = split ( /:/, $_ );
            if ($id eq '_all') {
                {
                    action_name   => $actions->{$action}->{label},
                    action_value  => $action,
                    blog_name     => plugin()->translate('* All Weblogs'),
                    blog_id       => $id,
                    trigger_name  => $triggers{$trigger},
                    trigger_value => $trigger,
		    trigger_tags  => $trigger_tags,
                };
            } elsif (my $blog = MT::Blog->load($id, { cached_ok => 1 })) {
                {
                    action_name   => $actions->{$action}->{label},
                    action_value  => $action,
                    blog_name     => $blog->name,
                    blog_id       => $id,
                    trigger_name  => $triggers{$trigger},
                    trigger_value => $trigger,
		    trigger_tags  => $trigger_tags,
                };
            } else {
                ();
            }
        } split ( /\|/, $rebuild_triggers );
        $args->{rebuilds_loop} = \@rebuilds;
    }
    my $app = MT->instance;
    if ($app->isa('MT::App')) {
        $args->{blog_id} = $app->blog->id if $app->blog;
    }
}

sub save_config {
    my $plugin = shift;
    my ($args, $scope) = @_;

    $plugin->SUPER::save_config(@_);

    my ($blog_id);
    if ( $scope =~ /blog:(\d+)/ ) {
        $blog_id = $1;

        # Save blog-level content aggregation policy to single 
        # system config hash for easy lookup
        my ($cfg_old, $cfg_new) = 0;
        my $override = 
            $plugin->get_config_value( 'access_overrides', "system" ) || {};
        $cfg_new = $args->{blog_content_accessible};
        if ( exists $override->{$blog_id} ) {
            $cfg_old = $override->{$blog_id};
        }
        if ( $cfg_old != $cfg_new ) {
            $override->{$blog_id} = $cfg_new 
                or delete $override->{$blog_id};
            $plugin->set_config_value( 'access_overrides'
                                     , $override
                                     , 'system' );
        }

        # Fiddle with rebuild triggers...
        my $rebuild_triggers = $args->{rebuild_triggers};
        my $old_triggers     = $args->{old_rebuild_triggers};

        # Check to see if the triggers changed
        if ( $old_triggers ne $rebuild_triggers ) {
            # If so, remove all references to the current blog from the triggers cached in other blogs
            foreach ( split ( /\|/, $old_triggers ) ) {
                my ( $action, $id, $trigger, $trigger_tags ) = split ( /:/, $_ );
                my $name = $id eq '_all' ? "all_triggers" : "other_triggers";
                my $scope = $id eq '_all' ? "system" : "blog:$id";
                my $d = $plugin->get_config_value($name, $scope);
                next unless exists $d->{$trigger}{$blog_id};
                delete $d->{$trigger}{$blog_id};
                $plugin->set_config_value($name, $d, $scope);
            }
        }
        foreach ( split ( /\|/, $rebuild_triggers ) ) {
            my ($action, $id, $trigger, $trigger_tags) = split ( /:/, $_ );
            my $name = $id eq '_all' ? "all_triggers" : "other_triggers";
            my $scope = $id eq '_all' ? "system" : "blog:$id";
            my $d = $plugin->get_config_value($name, $scope) || {};
	    if ($trigger_tags && $trigger_tags ne '') {
		if ($d->{$trigger}{$blog_id}{$action}{tags}) {
		    $d->{$trigger}{$blog_id}{$action}{tags} .= ',' . $trigger_tags;
		} else {
		    $d->{$trigger}{$blog_id}{$action}{tags} = $trigger_tags;
		}
	    } else {
		    $d->{$trigger}{$blog_id}{$action} = 1;
	    }

            $plugin->set_config_value($name, $d, $scope);
        }
    }
}

sub reset_config {
    my $plugin = shift;
    my ($args, $scope) = @_;

    if ( $scope =~ /blog:(\d+)/ ) {
        my $blog_id = $1;

        # Get the blogs this one triggers from and update them
        # And then save the triggers this blog runs
        my $other_triggers =
            plugin()->get_config_value( 'other_triggers', $scope );
        my $rebuild_triggers =
            plugin()->get_config_value( 'rebuild_triggers', $scope );
        my $all_triggers =
            plugin()->get_config_value( 'all_triggers', 'system' );

        foreach ( split ( /\|/, $rebuild_triggers ) ) {
            my ( $action, $id, $trigger ) = split ( /:/, $_ );
            next if $id eq '_all';
            my $d = plugin()->get_config_value( 'other_triggers', "blog:$id" );
            delete $d->{$trigger}{$blog_id}
                if exists $d->{$trigger}{$blog_id};
            plugin()->set_config_value( 'other_triggers', $d, "blog:$id" );
        }
        # remove this blog from the 'all_triggers'
        if ($all_triggers) {
            my $changed = 0;
            foreach my $trigger (keys %$all_triggers) {
                if (exists $all_triggers->{$trigger}{$blog_id}) {
                    delete $all_triggers->{$trigger}{$blog_id};
                    $changed = 1;
                }
            }
            if ($changed) {
                plugin()->set_config_value('all_triggers', $all_triggers, 'system');
            }
        }
        plugin()->SUPER::reset_config(@_);
        plugin()->set_config_value( 'other_triggers', $other_triggers,
            "blog:$blog_id" );
    }
    else {
        # reset should not alter the 'all_triggers' element which is
        # configured through the blog-level settings
        my $all_triggers = plugin()->get_config_value('all_triggers');
        plugin()->SUPER::reset_config(@_);
        plugin()->set_config_value('all_triggers', $all_triggers, 'system');
    }
}

sub preprocess_native_tags {
    my ( $ctx, $args, $cond ) = @_;
    my $plugin = plugin();

    my $tag = lc $ctx->stash('tag');

    # If we're running under MT-Search, set the context based on the search
    # parameters available.
    unless ($args->{blog_id} || $args->{include_blogs} || $args->{exclude_blogs}) {
        my $app = MT->instance;
        if ($app->isa('MT::App::Search')) {
            if (my $excl = $app->{searchparam}{ExcludeBlogs}) {
                $args->{exclude_blogs} ||= join ',', keys %$excl;
            } elsif (my $incl = $app->{searchparam}{IncludeBlogs}) {
                $args->{include_blogs} = join ',', keys %$incl;
            } 
            if (($args->{include_blogs} || $args->{exclude_blogs}) && $args->{blog_id}) {
                delete $args->{blog_id};
            }
        }
    }

    # Filter through MultiBlog's access controls.  If no blogs
    # are accessible given the specified attributes, we return
    # NULL or an error if one occured.
    if ( ! filter_blogs_from_args($plugin, $ctx, $args) ) {
        return $ctx->errstr ? $ctx->error($ctx->errstr) : '';
    }
    # Explicity set blog_id for MTInclude if not specified
    # so that it never gets a multiblog context from MTMultiBlog
    elsif ($tag eq 'include' and ! exists $args->{blog_id}) {
        my $local_blog_id = $ctx->stash('local_blog_id');
        if (defined $local_blog_id) {
            $args->{blog_id} = $ctx->stash('local_blog_id');
        }
    }
    # If no include_blogs/exclude_blogs specified look for a 
    # previously set MTMultiBlog context
    elsif ( my $mode = $ctx->stash('multiblog_context') ) {
        $args->{$mode} = $ctx->stash('multiblog_blog_ids');        
    }

    # Save local blog ID for all tags other than
    # MTMultiBlog since that tag handles it itself.
    local $ctx->{__stash}{local_blog_id} = $ctx->stash('blog_id') 
        unless $ctx->stash('multiblog_context');

    # Remove local blog ID from MTTags since it is cross-blog
    # and hence MTMultiBlogIfLocalBlog doesn't make sense there.    
    local $ctx->{__stash}{local_blog_id} = 0 if $tag eq 'tags';

    # Call original tag handler with new args
    defined(my $result = $ctx->super_handler( $args, $cond ))
        or return $ctx->error($ctx->errstr);

    return $result;
}


sub post_feedback_save {
    my $plugin = shift;
    my ( $trigger, $eh, $feedback ) = @_;
    if ( $feedback->visible ) {
        my $blog_id = $feedback->blog_id;
        my $app = MT->instance;
        foreach my $scope ("blog:$blog_id", "system") {
            my $d = plugin()->get_config_value( $scope eq 'system' ? 'all_triggers' : 'other_triggers', $scope );
            while ( my ( $id, $a ) = each( %{ $d->{$trigger} } ) ) {
                next if $id == $blog_id;
                perform_mb_action( $app, $id, $_, { Feedback => $feedback } ) foreach keys %$a;
            }
        }
    }
}

sub post_comment_save {
    my $plugin = shift;
    return $plugin->post_feedback_save(@_,'comment_pub');
}

sub post_tb_save {
    my $plugin = shift;
    return $plugin->post_feedback_save(@_,'tb_pub');
}

sub in_array {
    my ($needle, @haystack) = @_;
    foreach (@haystack) { return 1 if $needle eq $_; }
    return 0;
}

sub _has_tags {
    my ($obj, $tags) = @_;
    my @tags = split(",",$tags);
    foreach (@tags) {
	return 1 if in_array($_, $obj->tags);
    }
    return 0;
}

sub post_entry_save {
    my ( $eh, $app, $entry ) = @_;
    my $blog_id = $entry->blog_id;

    foreach my $scope ("blog:$blog_id", "system") {
        my $d = plugin()->get_config_value( $scope eq 'system' ? 'all_triggers' : 'other_triggers', $scope );
        
        while ( my ( $id, $a ) = each( %{ $d->{'tagged_entry_save'} } ) ) {
            next if $id == $blog_id;
            foreach (keys %$a) {
                my $tags = $a->{$_}{tags};
                if (_has_tags($entry,$tags)) {
                    perform_mb_action( $app, $id, $_, { Entry => $entry } );
                }
            }
        }
        
        while ( my ( $id, $a ) = each( %{ $d->{'entry_save'} } ) ) {
            next if $id == $blog_id;
            perform_mb_action( $app, $id, $_, { Entry => $entry} ) foreach keys %$a;
        }

        require MT::Entry;
        if ( ( $entry->status || 0 ) == MT::Entry::RELEASE() ) {
            while ( my ( $id, $a ) = each( %{ $d->{'entry_pub'} } ) ) {
                next if $id == $blog_id;
                perform_mb_action( $app, $id, $_, { Entry => $entry } ) foreach keys %$a;
            }
            while ( my ( $id, $a ) = each( %{ $d->{'tagged_entry_pub'} } ) ) {
                next if $id == $blog_id;
                foreach (keys %$a) {
                    my $tags = $a->{$_}{tags};
                    if (_has_tags($entry,$tags)) {
                        perform_mb_action( $app, $id, $_, { Tags => $tags, Entry => $entry } );
                    }
                }
            }
        }
    }
}

sub post_entry_pub {
    my ( $eh, $app, $entry ) = @_;
    my $blog_id = $entry->blog_id;

    foreach my $scope ("blog:$blog_id", "system") {
        my $d = plugin()->get_config_value( $scope eq 'system' ? 'all_triggers' : 'other_triggers', $scope );
        require MT::Entry;
        if ( ( $entry->status || 0 ) == MT::Entry::RELEASE() ) {
            while ( my ( $id, $a ) = each( %{ $d->{'entry_pub'} } ) ) {
                next if $id == $blog_id;
                perform_mb_action( $app, $id, $_, { Entry => $entry } ) foreach keys %$a;
            }
            while ( my ( $id, $a ) = each( %{ $d->{'tagged_entry_pub'} } ) ) {
                next if $id == $blog_id;
                foreach (keys %$a) {
                    my $tags = $a->{$_}{tags};
                    if (_has_tags($entry,$tags)) {
                        perform_mb_action( $app, $id, $_, { Tags => $tags, Entry => $entry } );
                    }
                }
            }
        }
    }
}

sub action_raa {
    my $app = shift;
    my ($blog_id, $args) = @_;

    require MT::Request;
    my $r = MT::Request->instance;
    my $rebuilt = $r->stash('multiblog_rebuilt') || {};
    my $entry = $args->{'Entry'};
    unless (exists $rebuilt->{$blog_id}) {
        $app->rebuild_archives( 
            BlogID => $blog_id,
            Recip => { Author => $entry->author_id } 
        );
        $rebuilt->{$blog_id} = 1;
        $r->stash('multiblog_rebuilt', $rebuilt);
    }
}

sub action_ri {
    my $app = shift;
    my ($blog_id, $args) = @_;

    require MT::Request;
    my $r = MT::Request->instance;
    my $rebuilt = $r->stash('multiblog_rebuilt') || {};
    unless (exists $rebuilt->{$blog_id}) {
        $app->rebuild_indexes( BlogID => $blog_id );
        $rebuilt->{$blog_id} = 1;
        $r->stash('multiblog_rebuilt', $rebuilt);
    }
}

sub action_rip {
    my $app = shift;
    my ($blog_id, $args) = @_;
    
    action_ri($app, @_);
    
    require MT::Request;
    my $r = MT::Request->instance;
    my $pinged  = $r->stash('multiblog_pinged') || {};
    unless (exists $pinged->{$blog_id}) {
        $app->ping( BlogID => $blog_id );
        $pinged->{$blog_id} = 1;
        $r->stash('multiblog_pinged', $pinged);
    }
}

sub perform_mb_action {
    my ( $app, $blog_id, $action, $args ) = @_;

    # Find handler
    my $hdlr = plugin()->registry->{multiblog_actions}->{$action}->{handler};
    # TODO - error check

    unless (ref($hdlr)) {
        $hdlr = MT->handler_to_coderef($hdlr);
    }
    return $hdlr->( $app, $blog_id, $args );
}

sub filter_blogs_from_args { 
    my ($plugin, $ctx, $args) = @_;

    # SANITY CHECK ON ARGUMENTS
    my $err;
    # Set and clean up working variables
    my $incl = $args->{include_blogs} || $args->{blog_id} || $args->{blog_ids};
    my $excl = $args->{exclude_blogs};
    for ($incl,$excl) {
        next unless $_;
        s{\s+}{}g ; # Remove spaces
    }
    
    # If there are no multiblog arguments to filter, we don't need to be here
    return 1 unless $incl or $excl;

    # Only one multiblog argument can be used
    my $arg_count = scalar grep { $_ and $_ ne '' } $args->{include_blogs}, 
                                                    $args->{blog_id}, 
                                                    $args->{blog_ids}, 
                                                    $args->{exclude_blogs};
    if ($arg_count > 1) {
        $err = plugin()->translate('The include_blogs, exclude_blogs, blog_ids and blog_id attributes cannot be used together.');
    }
    # exclude_blogs="all" is not allowed
    elsif ($excl and $excl =~ /all/i) {
        $err = plugin()->translate('The attribute exclude_blogs cannot take "all" for a value.');
    }
    # blog_id only accepts a single blog ID
    elsif ($args->{blog_id} and $args->{blog_id} !~ /^\d+$/) {
        $err = plugin()->translate('The value of the blog_id attribute must be a single blog ID.');
    }
    # Make sure include_blogs/exclude_blogs is valid
    elsif (($incl || $excl) ne 'all' 
        and ($incl || $excl) !~ /^\d+([,-]\d+)*$/) {
        $err =  plugin()->translate('The value for the include_blogs/exclude_blogs attributes must be one or more blog IDs, separated by commas.');
    }
    return $ctx->error($err) if $err;

    # Prepare for filter_blogs
    my ($attr, $val, @blogs);
    if ($incl) {
        ($attr, $val) = ('include_blogs', $incl);
    } else {
        ($attr, $val) = ('exclude_blogs', $excl);
    }

    if ($val =~ m/-/) {
        my @list = split /\s*,\s*/, $val;
        foreach my $id (@list) {
            if ($id =~ m/^(\d+)-(\d+)$/) {
                push @blogs, $_ for $1..$2;
            } else {
                push @blogs, $id;
            }
        }
    }
    else {
        @blogs = split(/\s*,\s*/, $val);
    }

    # Filter the blogs using the MultiBlog access controls
    ($attr, @blogs) = filter_blogs($plugin, $ctx, $attr, @blogs);
    return unless $attr && @blogs;
    
    # Rewrite the args to the modifed value
    delete $args->{blog_ids} if exists $args->{blog_ids};  # Deprecated
    if ($args->{blog_id}) {
        $args->{'blog_id'} = $blogs[0];
    } else {
        delete $args->{include_blogs};
        delete $args->{exclude_blogs};
        $args->{$attr} = join(',', @blogs);
    }
    1;
}

## Get a mode (include/exclude) and list of blogs
## Process list using system default access setting and
## any blog-level overrides.
## Returns empty list if no blogs can be used
sub filter_blogs {
    my $plugin = shift;
    my ( $ctx, $is_include, @blogs ) = @_;

    # Set flag to indicate whether @blogs are to be included or excluded
    $is_include = $is_include eq 'include_blogs' ? 1 : 0;

    # Set local blog
    my $this_blog = $ctx->stash('blog_id') || 0;

    # Get the MultiBlog system config for default access and overrides
    my $default_access_allowed = 
        plugin()->get_config_value( 'default_access_allowed', 'system' );
    my $access_overrides = 
        plugin()->get_config_value( 'access_overrides', 'system' ) || {};

    # System setting allows access by default
    if ($default_access_allowed) {

        # include_blogs="all"
        if ($is_include and $blogs[0] eq "all") {
            # Check for any deny overrides. 
            # If found, switch to exclude_blogs="..."
            my @deny = grep {     $_ != $this_blog 
                              and exists $access_overrides->{$_} 
                              and $access_overrides->{$_} == DENIED 
                            } keys %$access_overrides;
            return @deny ? ('exclude_blogs', @deny)
                         : ('include_blogs', 'all');
         }
         # include_blogs="1,2,3,4"
         elsif ($is_include and @blogs) {
            # Remove any included blogs that are specifically deny override
            # Return undef is all specified blogs are deny override
            my @allow = grep {    $_ == $this_blog 
                               or ! exists $access_overrides->{$_} 
                               or $access_overrides->{$_} == ALLOWED
                             } @blogs;
            return @allow ? ('include_blogs', @allow) : undef;
         }
         # exclude_blogs="1,2,3,4"
         else {
             # Add any deny overrides blogs to the list and de-dupe
             push(@blogs, grep { $_ != $this_blog
                                 and $access_overrides->{$_} == DENIED 
                               } keys %$access_overrides);
            my %seen;
            @seen{@blogs} = ();
            @blogs = keys %seen;
            return ('exclude_blogs', @blogs);
         }
    }
    # System setting does not allow access by default
    else {
        # include_blogs="all"
        if ($is_include and $blogs[0] eq "all") {
            # Enumerate blogs from allow override
            # Hopefully this is significantly smaller than @all_blogs
            my @allow = grep { $_ == $this_blog 
                               or $access_overrides->{$_} == ALLOWED
                             } ($this_blog, keys %$access_overrides);
            return @allow ? ('include_blogs', @allow) : undef;
        }
        # include_blogs="1,2,3,4"
        elsif ($is_include and @blogs) {
            # Filter @blogs returning only those with allow override
            my @allow = grep {    $_ == $this_blog
                               or ( exists $access_overrides->{$_} 
                                    and $access_overrides->{$_} == ALLOWED)
                             } @blogs;
            return @allow ? ('include_blogs', @allow) : undef;
        }
        # exclude_blogs="1,2,3,4"
        else {
            # Get allow override blogs and then omit 
            # the specified excluded blogs.
            my @allow = grep { $_ == $this_blog
                               or $access_overrides->{$_} == ALLOWED
                             } ($this_blog, keys %$access_overrides);
            my %seen;
            @seen{@blogs} = ();
            @blogs = grep { ! $seen{$_} } @allow;
            return @blogs ? ('include_blogs', @blogs) : undef;
        }
    }
}

1;
