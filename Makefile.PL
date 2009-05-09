use strict;
use inc::Module::Install;

name "RSSycklr";
license "perl";
all_from "lib/RSSycklr.pm";

requires "Moose";
requires "XML::LibXML";
requires "XML::Feed";
requires "Template";
requires "Template::Provider::Encoding" => "0.09";
requires "Template::Stash::ForceUTF8";
requires "YAML";
requires "HTML::Truncate";
requires "HTML::Entities";
requires "DateTime";
requires "URI";
requires "Encode";
requires "Pod::Usage";
requires "Getopt::Long";
requires "File::ShareDir";
requires "Hash::Merge::Simple";

system('pod2text lib/Yesh.pm > README');

install_script "bin/rssycklr";
install_share "dtd";

auto_install;
WriteAll;

__END__
