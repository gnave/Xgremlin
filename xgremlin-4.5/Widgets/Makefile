#
# Display widget library
#

#include configuration file
include ../config.system
include ../config.user

INCDIR = -I. -I../Csrc -I$(XINCLOC)

SRCS=Display.c dps.c misc.c LiteClue.c

OBJS=$(SRCS:.c=.o)

all: libWidgets.a

libWidgets.a: $(OBJS)
	ar rv libWidgets.a $(OBJS) 
	ranlib libWidgets.a

%.o : %.c
	$(CC) $(CFLAGS) $(INCDIR) -c $<

clean:
	rm -f *~*~ *.o *.a libWidgets.a


