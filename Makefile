Debian/AutoBuilder/Version.hs: debian/changelog Makefile
	/bin/echo -e "module Debian.AutoBuilder.Version where\nversion :: String\nversion = \"$(shell dpkg-parsechangelog | sed -n 's/^Version:[ \t]*//p')\"" > $@
