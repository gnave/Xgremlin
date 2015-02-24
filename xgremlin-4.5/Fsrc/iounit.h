c \grammy\src\iounit.inc

*                         wrtbuf added for output in C
c Revision 2.02 03/16/93  idum1 changed to rdmode; idum2 changed to infile
c Revision 2.01 02/08/89  punch changed to cardout; lineda, tpdin dropped
c Version  1.00  16.04.91
 
	integer input, output, cardout, plot
	integer datain, lineio, dataot, altdat, scrtch
	integer spool, stdin, stdout, cardin
	integer logfil, rawin
        integer rdmode, infile
	integer lorec(3), hirec(3)
	integer uopen(30), filchr, linlen
	logical linbak, lfirst

	common /iounit/ filchr, input, output, cardout, plot
	common /iounit/ datain, lineio, dataot, altdat, scrtch
	common /iounit/ spool, stdin, stdout, cardin
	common /iounit/ logfil, rawin
        common /iounit/ rdmode, infile
	common /iounit/ lorec, hirec
	common /iounit/ uopen, linlen
	common /iounit/ linbak, lfirst
 
	character*256 fnames(30), wrtbuf, tmpnam
	common /iochar/ fnames, wrtbuf, tmpnam
