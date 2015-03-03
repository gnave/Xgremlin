#
# makefile for tools
#

include ../config.system
include ../config.user

C = ../Common/byteorder.c getopt.c

conversion_tools: unix2dos dos2unix

linelist_tools: dumplin mergelin

file_tools: bsplit bmerge

fits_tools: fits2nso nso2fits

unix2dos : unix2dos.c
	$(CC) $(LDFLAGS) $(CFLAGS) -o unix2dos $< -lc

dos2unix : dos2unix.c
	$(CC) $(LDFLAGS) $(CFLAGS) -o dos2unix $< -lc

dumplin: dumplin.c getopt.c
	$(CC) $(LDFLAGS) $(CFLAGS) -I../Common -L../Common -o dumplin dumplin.c $(C)\
        -lCommon -lm -lc

mergelin: mergelin.c getopt.c
	$(CC) $(LDFLAGS) $(CFLAGS) -I../Common -L../Common -o mergelin mergelin.c $(C)\
        -lCommon -lm -lc

bsplit: bsplit.c
	$(CC) $(LDFLAGS) $(CFLAGS) -o bsplit $< -lc

bmerge: bmerge.c
	$(CC) $(LDFLAGS) $(CFLAGS) -o bmerge $< -lc

nso2fits: nso2fits.c
	$(CC) $(CFLAGS) -I../Common -L../Common -o nso2fits nso2fits.c \
         -lCommon -lm 

fits2nso: fits2nso.c
	$(CC) $(CFLAGS) -I../Common -L../Common -o fits2nso fits2nso.c \
         -lCommon -lm 
 
clean:
	rm -f *.o *~*~ mergelin dumplin unix2dos dos2unix fits2nso

install: install_conversion_toos  install_linelist_tools  install_file_tools fits_tools
 
install_conversion_tools:
	install -m 755 unix2dos /usr/local/bin
	install -m 755 dos2unix /usr/local/bin

install_linelist_tools:
	install -m 755 dumplin  /usr/local/bin
	install -m 755 mergelin /usr/local/bin

install_file_tools:
	install -m 755 bsplit  /usr/local/bin
	install -m 755 bmerge  /usr/local/bin
 
install_fits_tools:
	install -m 755 fits2nso /usr/local/bin

