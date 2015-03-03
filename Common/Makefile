#
# makefile for libCommon
#

include ../config.system
include ../config.user

C = List.c byteorder.c timer.c regexpr.c fits_io.c

O = $(C:.c=.o)

libCommon.a: $(O)
	ar rv libCommon.a $?
	ranlib libCommon.a

%.o : %.c
	$(CC) -c $(CFLAGS) $<

clean:
	rm -f *.o *~*~ libCommon.a

