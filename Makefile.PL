use strict;
use inc::Module::Install;

name "RSSycklr";
license "perl";
all_from "lib/RSSycklr.pm";

requires "Mouse";
requires "XML::LibXML";
requires "XML::Feed";
requires "Template";
requires "Template::Provider::Encoding" => "0.09";
requires "Template::Stash::ForceUTF8";
requires "YAML";
requires "HTML::Truncate";
requires "DateTime";
requires "URI";
requires "Encode";
requires "Pod::Usage";
requires "GetOpt::Long";
requires "File::ShareDir";


install_script "bin/rssycklr";
install_share "dtd";

auto_install;
WriteAll;

__END__
