#
# creates the polished library  libgremlin.a
#

#include make configuration file
include ../config.system
include ../f.flags

F = apoption.f arminmax.f cardio.f cmplx.f sort.f chebychev.f \
    fft.f findl.f fourier.f ftsin.f ftsout.f funct.f gboss.f gdataio.f \
    ghelp.f gmainit.f gopen.f infio.f infioalt.f strutil.f headers.f \
    lineio.f linel.f lsq.f modify.f multif.f multir.f option.f integrate.f  \
    page.f plot.f plotr.f plot2.f prnio.f random.f read.f stds.f \
    phase.f trans.f voigt.f vwidth.f wavcon.f dispatch.f gremlininit.f  \
    writeout.f svd.f readasc.f atlas.f response.f quad.f vquad.f background.f \
    tspack.f minpack.f zminmax.f voigtfunct.f blas.f fitpack.f srwli.f \
    hfs.f hfssub.f hfsmisc.f hfsdisp.f fitshape.f fit.f 

O = $(F:.f=.o)

#
# Note: init.o must be loaded first at the linking stage to ensure that
# the block data subprogram is handled correctly
#
all : libgremlin.a init.o

libgremlin.a : $(O)
	ar rv libgremlin.a $?
	ranlib libgremlin.a

clean:
	rm -f *~* *.bak *.o *.c core *.a

include ../f.make


