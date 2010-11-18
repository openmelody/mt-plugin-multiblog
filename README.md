#
# MultiBlog Plugin for Movable Type
#

MultiBlog is a plugin that allows administrators to setup publishing
rules for blogs that instruct Movable Type to republish specific blogs
in response to events occurring within the system.

MULTIBLOG 2.1 vs 2.2

The MultiBlog plugin is a plugin that comes bundled with Movable Type.
The plugin you see here, is an upgrade to that plugin. At the time
this readme was authored, the current version of MultiBlog that ships
with Movable Type is version 2.1, while this plugin is version 2.2.

Differences between 2.1 and 2.2:

* the ability to register custom MultiBlog actions via the registry.
* the ability to trigger MultiBlog actions in response to entries
  with specific tags being saved or published.
* the ability to trigger the republishing of author archives only.
* enhanced publishing logic to minimize work done to perform actions.

MULTIBLOG ACTIONS

Here is an excerpt from a sample config.yaml which shows how to 
register custom actions for MultiBlog:

    multiblog_actions:
      raa:
        label: 'rebuild author archives.'
        handler: $multiblog::MultiBlog::Plugin::action_raa
      ri:
        label: 'rebuild indexes.'
        handler: $multiblog::MultiBlog::Plugin::action_ri
      rip:
        label: 'rebuild indexes and send pings.'
        handler: $multiblog::MultiBlog::Plugin::action_rip

Input Parameters:

* **label** - the display name of the MultiBlog action. This is what
              will appear in the pull down menu on the "Create Trigger"
              dialog.
* **handler** - a code reference to the subroutine that will process 
                and perform the action.

Here is a sample handler for one of those actions:

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

Each action handler takes as input the Blog ID of the blog
whose action is being triggered. In addition it takes as a parameter an
argument hash whose contents varies depending upon the action
being performed. Possible arguments are:

* Entry
* Tags

