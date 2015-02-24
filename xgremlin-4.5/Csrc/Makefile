
##############################################################################
#
# Makefile for the C part of Xgremlin and the final program
#
##############################################################################

MAKE = make
AWK = awk
BINDIR = ./xgremlin-binaries

include ../config.system
include ../config.user
include ../f.libs

# EXTRA DEFINES
###############
# These are optional parameters which influence the way in which the program
# is compiled. Add your options to the EXTRA macro as in the following example:
# EXTRA = -DUSE_FQDN
#
# The following options are available:
#
# -DUSE_FQDN 
#	if the fully qualified domain name is to be used in the title
#       of the program.
#
# -D__NO_MATH_INLINES
#       may be used on i386 machines which have John C. Bowman's inline math
#       function library installed to suppress inlining of math functions.
#
EXTRA =

##############################################################################
# 
# No changes should be required below this line
#
##############################################################################

LIBLOC = -L$(XLIBLOC) -L../Fsrc -L../Widgets -L../Common -L../mpar $(EXTRALIBS) 
INCLOC = -I../Widgets -I../Common -I$(XINCLOC)
LIBS = -lWidgets $(XAWLIB) -lXt -lXext -lSM -lICE -lXmu -lX11 -lgremlin -lMPAR -lCommon $(FLIBS) -lm

SS = xgremlin.c OptionMenu.c Help.c Functions.c Buttons.c Miscellaneous.c \
     Edit.c ByteOrder.c FortranWrappers.c PlotMode.c SignalHandlers.c \
     Phase.c Hfs.c Scratch.c argtype.c arralloc.c Version.c

O = $(SS:.c=.o)

all: ostype.h xgremlin

xgremlin: $(O) ../Fsrc/init.o ../Fsrc/libgremlin.a ../Widgets/libWidgets.a
	$(CC) $(LDFLAGS) -o xgremlin  $(O) ../Fsrc/init.o $(LIBLOC) $(LIBS)
	mv ./xgremlin ./xgremlin-`cat version`
#	strip ./xgremlin-`cat version`


tags: $(S)
	etags $(S)

%.o : %.c
	$(CC) $(CFLAGS) $(INCLOC) $(EXTRA) -c $<

ostype.h:
	echo "#define SYSNAME     \"`hostname`\"" > ostype.h
	echo "#define OSNAME      \"`uname`/`uname -m`\"" >> ostype.h
	echo "#define OSVERSION   \"`uname -mr`\"" >> ostype.h
	$(CC) -v 2> cc-version
	echo "#define CCVERSION   \"`cat cc-version|tail -1 ` \""  >> ostype.h
	rm -f cc-version
	echo "#define FFVERSION \"`cat ff-version`\"" >> ostype.h

clean:
	rm -f *.o *~* ostype.h ff-version core .size xgremlin-*

################################################################
#
# the following targets are for creating binary distributions
#
################################################################

version: Version.h
	$(AWK) -fversion.awk Version.h > version


#
# generic target for building binary distributions
#
distribution:
	mkdir $(BINDIR)
	mv ./xgremlin-`cat version` $(BINDIR)
	mv ../tools/fits2nso $(BINDIR)
	cp ../doc/fits2nso.1 $(BINDIR)
        ifdef LLTOOLS
	   mv ../tools/dumplin  $(BINDIR)
	   mv ../tools/mergelin $(BINDIR)
	   cp ../doc/dumplin.1 $(BINDIR)
	   cp ../doc/mergelin.1 $(BINDIR)
        endif
        ifdef PSPLOT
	   mv ../psplot/psplot $(BINDIR)
	   cp ../doc/psplot.1 $(BINDIR)
	   cp ../doc/psplot.init $(BINDIR)
        endif
	cp ../configurations/Xgremlin $(BINDIR)/Xgremlin
	cp Makefile.binaries $(BINDIR)/Makefile
	cp version $(BINDIR)
	cp ../config.user $(BINDIR)
	cp ../Howto-Install-Binaries.html $(BINDIR)
	cp ../ChangeLog $(BINDIR)
	@echo
	@echo "Bundeling documentation ..."
	@echo
	( cd ../doc ; tar cvf - ./html ) > $(BINDIR)/xgremlin-html.tar
	cp ../doc/xgremlin.1 $(BINDIR)
	@echo
	@echo "Creating tar archive ..."
	@echo
	tar cvf xgremlin-`cat version`-`cat .bin_name`.tar $(BINDIR)
	rm -rf $(BINDIR)
	@echo
	@echo "Compressing tar archive ...."
	@echo
	gzip -9q xgremlin-`cat version`-`cat .bin_name`.tar
	mv xgremlin-`cat version`-`cat .bin_name`.tar.gz ..
	rm .bin_name
	@echo
	@echo "Done."
	@echo

# Revision history:
# -----------------
# $Log$
