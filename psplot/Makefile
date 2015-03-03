#
# make the atlas plotting program
#

# include configuration files
include ../config.system
include ../config.user
include ../f.flags
include ../f.libs


F = psplot.f pssub.f psmainit.f plmain.f plboss.f \
    cheat.f option.f

O = psplot.o pssub.o psmainit.o plmain.o plboss.o \
    cheat.o option.o


psplot : $(O)
	$(FLINK) -o psplot $(O) $(EXTRALIBS) $(FLIBS) -L../Common -lCommon $(FELIB)
	strip psplot

clean:
	rm -f *.o *~*~ cheat.c option.c plboss.c plmain.c psmainit.c psplot.c pssub.c psplot core

include ../f.make
 
