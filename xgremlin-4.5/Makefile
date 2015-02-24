####################################
#
# top level make file for Xgremlin
#
####################################

#
# the subdirectories with secondary make files
#
SUBDIRS = mpar Widgets Fsrc Csrc Common tools psplot
BINDIR = /usr/local/bin
APPDEF = /usr/X11R6/lib/X11/app-defaults
MAKE = make

versnum:=$(shell cat ./Csrc/version)

ifeq (config.user,$(wildcard config.user))
include config.user
endif

default-target:
	@echo
	@echo "   **************************************************************"
	@echo
	@echo "   You must specify the target computer type when you start make:"
	@echo
	@echo "   **************************************************************"
	@echo
	@echo "        make <target-machine>"
	@echo
	@echo "   where <target-machine> must be one of the following:"
	@echo
	@echo "        sunos4-sparc    : SunOS 4.1.x for Sparc processors"
	@echo "        solaris2-sparc  : Solaris 2.x for Sparc processors"
	@echo "        aix3-rs6000     : AIX 3.x for IBM Power processors"
	@echo "        linux-i386      : Linux for x86 processors"
	@echo "        macosx          : Mac OS X (PPC or Intel processors)"
	@echo "        cygwin          : cygwin for Windows "
	@echo
	@echo "   Example:"
	@echo "        make sunos4-sparc"
	@echo

linux-debug:
	@echo
	@echo "  ********************************"
	@echo "  Creating Linux debugging version"
	@echo "  ********************************"
	@echo
	$(MAKE) -C mpar -f Makefile.debug
	$(MAKE) -C Widgets -f Makefile.debug
	$(MAKE) -C Common -f Makefile.debug
	$(MAKE) -C Fsrc -r -f Makefile.debug
	$(MAKE) -C Csrc -f Makefile.debug

linux-debug-clean:
	$(MAKE) -C mpar -f Makefile.debug clean
	$(MAKE) -C Widgets -f Makefile.debug clean
	$(MAKE) -C Common -f Makefile.debug clean
	$(MAKE) -C Fsrc -r -f Makefile.debug clean
	$(MAKE) -C Csrc -f Makefile.debug clean

cygwin:
	rm -f config.system
	ln -s ./configurations/cygwin config.system
	echo "cygwin" > ./Csrc/.bin_name
	$(MAKE) all-versions

macosx:
	rm -f config.system
	ln -s ./configurations/macosx config.system
	echo "macosx" > ./Csrc/.bin_name
	$(MAKE) all-versions

sunos4-sparc:
	rm -f config.system
	ln -s ./configurations/sunos4-sparc config.system
	echo "sunos4-sparc" > ./Csrc/.bin_name
	$(MAKE) all-versions

solaris2-sparc:
	rm -f config.system
	ln -s ./configurations/solaris2-sparc config.system
	echo "solaris2-sparc" > ./Csrc/.bin_name
	$(MAKE) all-versions

aix3-rs6000:
	rm -f config.system
	ln -s ./configurations/aix3-rs6000 config.system
	echo "aix3-rs6000" > ./Csrc/.bin_name
	$(MAKE) all-versions

linux-i386:
	ln -sf ./configurations/linux-i386 config.system
	echo "linux-i386" > ./Csrc/.bin_name
	$(MAKE) all-versions

linux-axp:
	ln -sf ./configurations/linux-axp config.system
	echo "linux-axp" > ./Csrc/.bin_name
	$(MAKE) all-versions

linux-sparc:
	ln -sf ./configurations/linux-sparc config.system
	echo "linux-sparc" > ./Csrc/.bin_name
	$(MAKE) all-versions

all-versions:
	$(MAKE) -C mpar
	$(MAKE) -C Widgets
	$(MAKE) -C Common
	$(MAKE) -C Fsrc
	$(MAKE) -C Csrc
	$(MAKE) -C tools fits_tools
        ifdef LLTOOLS
	   $(MAKE) -C tools linelist_tools
        endif
        ifdef PSPLOT
	   $(MAKE) -C psplot -r
        endif
	@echo
	@echo "Done it all !"
	@echo
	@echo "Install the program with:           sudo make install"
	@echo "Create a binary distribution with:  make distribution"
	@echo


distribution:
	$(MAKE) -C Csrc distribution


install:
	install -m 755 ./Csrc/xgremlin-$(versnum) $(BINDIR)
	rm -f $(BINDIR)/xgremlin
	ln -s $(BINDIR)/xgremlin-$(versnum) $(BINDIR)/xgremlin
        ifdef LLTOOLS
	   $(MAKE) -C tools install_linelist_tools
        endif
        ifdef PSPLOT
	   install -m 755 ./psplot/psplot $(BINDIR)
        endif
	install -m 644 ./configurations/Xgremlin $(APPDEF)
#	install -m 755 ./Csrc/helpindex /usr/local/bin
	if [ ! -d /usr/local/xgremlin ]; then mkdir /usr/local/xgremlin ; fi


install-doc:
	if [ ! -d /usr/local/xgremlin ]; then mkdir /usr/local/xgremlin ; fi
	install -m 644 ./doc/xgremlin.1 $(MANLOC)
	install -m 644 ./doc/psplot.1 $(MANLOC)
	install -m 644 ./doc/dumplin.1 $(MANLOC)
	install -m 644 ./doc/mergelin.1 $(MANLOC)
#	install -m 644 ./doc/xgremlin.help /usr/local/xgremlin
#	install -m 644 ./doc/xgremlin.help.index /usr/local/xgremlin
	install -m 644 ./xgremlin.conf /usr/local/xgremlin
	cp -r -p  ./doc/html/*  /usr/local/xgremlin/html

clean:
	for i in $(SUBDIRS); do $(MAKE) -C $$i clean ; done 
	rm -f config.system config.user f.flags f.make f.libs ./mpar/Makefile
