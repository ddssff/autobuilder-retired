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
	/bin/echo -e "module Version where\nversion :: String\nversion = \"$(shell dpkg-parsechangelog | sed -n 's/^Version:[ \t]*//p')\"" > $@

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
# DO NOT DELETE: Beginning of Haskell dependencies
GenBuildDeps.o : GenBuildDeps.hs
SourcesList.o : SourcesList.hs
DistroCache.o : DistroCache.hs
DistroCache.o : SourcesList.hi
Progress.o : Progress.hs
PackageDeprecated.o : PackageDeprecated.hs
My.o : My.hs
Revision.o : Revision.hs
Revision.o : My.hi
Dependencies.o : Dependencies.hs
Dependencies.o : My.hi
Dependencies.o : Revision.hi
Dependencies.o : PackageDeprecated.hi
DebianTypes.o : DebianTypes.hs
UploadFile.o : UploadFile.hs
UploadFile.o : DebianTypes.hi
ChangesFile.o : ChangesFile.hs
ChangesFile.o : DebianTypes.hi
Repository.o : Repository.hs
Repository.o : My.hi
Repository.o : DebianTypes.hi
Repository.o : UploadFile.hi
Repository.o : ChangesFile.hi
Repository.o : BinaryPackage.hi
Repository.o : SourcePackage.hi
Repository.o : Progress.hi
OSImage.o : OSImage.hs
OSImage.o : My.hi
OSImage.o : Progress.hi
OSImage.o : SourcesList.hi
OSImage.o : Repository.hi
OSImage.o : DistroCache.hi
DebianSourceTree.o : DebianSourceTree.hs
DebianSourceTree.o : My.hi
DebianSourceTree.o : ChangesFile.hi
DebianSourceTree.o : Progress.hi
AptImage.o : AptImage.hs
AptImage.o : My.hi
AptImage.o : DebianSourceTree.hi
AptImage.o : SourcesList.hi
AptImage.o : Progress.hi
Target.o : Target.hs
Target.o : My.hi
Target.o : Dependencies.hi
Target.o : Progress.hi
Target.o : SourceSpec.hi
Target.o : DebianSourceTree.hi
Target.o : OSImage.hi
Config.o : Config.hs
Params.o : Params.hs
Params.o : Repository.hi
Params.o : OSImage.hi
Params.o : DistroCache.hi
Params.o : My.hi
Params.o : Config.hi
Params.o : Progress.hi
VersionTag.o : VersionTag.hs
Version.o : Debian/AutoBuilder/Version.hs
AutoBuilder.o : AutoBuilder.hs
AutoBuilder.o : Version.hi
AutoBuilder.o : VersionTag.hi
AutoBuilder.o : ChangesFile.hi
AutoBuilder.o : Target.hi
AutoBuilder.o : SourceSpec.hi
AutoBuilder.o : Revision.hi
AutoBuilder.o : Params.hi
AutoBuilder.o : My.hi
AutoBuilder.o : GenBuildDeps.hi
AutoBuilder.o : Dependencies.hi
AutoBuilder.o : Progress.hi
AutoBuilder.o : Repository.hi
AutoBuilder.o : DebianSourceTree.hi
AutoBuilder.o : SourcesList.hi
AutoBuilder.o : PackageDeprecated.hi
AutoBuilder.o : OSImage.hi
AutoBuilder.o : DistroCache.hi
AutoBuilder.o : AptImage.hi
# DO NOT DELETE: End of Haskell dependencies
