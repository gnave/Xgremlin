#
# creates the BOMEM file conversion tool
#

CC = gcc
CFLAGS = -O2

C = bomem2dat.c nso6.c timer.c

O = $(C:.c=.o)

bomem2dat : $(O)
	gcc -o bomem2dat $(O) -lm

%.o : %.c
	$(CC) -c $(CFLAGS) $<

clean:
	rm -f *.o *~*~ bomem2dat
 
install:
	install -m 755 bomem2dat /usr/local/bin



