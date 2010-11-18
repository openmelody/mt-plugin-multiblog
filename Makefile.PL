use ExtUtils::MakeMaker;

use YAML::Any qw(LoadFile);
my $file = "plugins/MultiBlog/config.yaml";
my $version = LoadFile($file)->{version} ||
    die "No version found in $file";

WriteMakefile(
    NAME            => 'MultiBlog',
    VERSION         => $version,
    DISTNAME        => 'MultiBlog',
);
