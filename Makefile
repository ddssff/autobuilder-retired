HS =	$(wildcard *.hs) \
	$(wildcard BuildTarget/*.hs) \
	Debian/AutoBuilder/Version.hs

# Filter out unused and machine generated source files, and DryRunIO
# which has a construct that haddock can't handle.
NODOCHS = Documentatiion/CodeReview.hs Setup.hs Verify.hs Debian/AutoBuilder/Version.hs \
	  NewDist.hs WebPage.hs Test.hs

DOCHS = $(filter-out $(NODOCHS), $(HS))

GHC = ghc --make -fglasgow-exts -W -O2 -fasm -threaded

all: autobuilder webpage newdist test doc

install: all
	sudo cp webpage /usr/lib/cgi-bin/autobuilder

autobuilder: $(HS) AutoBuilder.hs Makefile Debian/AutoBuilder/Target.hs
	$(GHC) -o $@ AutoBuilder.hs

webpage: $(HS) WebPage.hs Makefile
	$(GHC) -o $@ WebPage.hs

newdist: $(HS) NewDist.hs Makefile
	$(GHC) -o $@ NewDist.hs

test: $(HS) Test.hs Makefile
	$(GHC) -o $@ Test.hs

Debian/AutoBuilder/Version.hs: debian/changelog Makefile
	/bin/echo -e "module Debian.AutoBuilder.Version where\nversion :: String\nversion = \"$(shell dpkg-parsechangelog | sed -n 's/^Version:[ \t]*//p')\"" > $@

# Config.hs GenBuildDeps.hs My.hs MyTypes.hs Strictness.hs Target.hs

doc: doc/index.html

doc/index.html: $(DOCHS) AutoBuilder.hs NewDist.hs WebPage.hs Makefile
	#rm -rf doc; mkdir -p doc
	mkdir -p doc
	haddock -B $(shell ghc --print-libdir) --optghc=-cpp -v --html -o doc $(DOCHS) 2>/tmp/haddock.out || { cat /tmp/haddock.out && exit 1; }

little: little.hs
	$(GHC) -o $@ little.hs

autobuilder-builddeps: autobuilder-builddeps.hs
	$(GHC) -o $@ autobuilder-builddeps.hs

isosize: isosize.hs
	$(GHC) -o $@ isosize.hs

clean:
	rm -f autobuilder webpage newdist test \
		*.hi *.o */*.hi */*.o */*/*.hi */*/*.o */*/*/*.hi */*/*/*.o \
		Documentation/*.aux Documentation/*.dvi Documentation/*.log

.PHONY: all doc clean
