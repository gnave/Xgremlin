*
* general block data initialization
*
	BLOCK DATA

	include 'datetc.h'
	include 'iounit.h'
	include 'infmtn.h'
	include 'altinf.h'
	include 'set.h'
	include 'linparms.h'
	include 'plcommon.h'
	include 'temp.h'

c Max number of chars in a file name.
	data filchr /255/

c bottom of .lin file:
	data nbot/320/

c initialize set parameters:
	data keybd/.true./, batch/.false./, warn/.true./, lecho/.true./

c turn off auto-normalize; write out all headers, info, etc.
	data rdnorm/.false./, rdfctr/1.0/, verbos/.true./

c read refs are from first point; set default read length; no stretch
	data rdref/0/, rdlen/2048/, istret/1/

c boss parameters
	data nc/0/, kl/0/

	data input/5/,  stdin/5/,   output/6/,stdout/6/,  plot/4/, 
     &       cardin/7/, cardout/8/, spool/9/, logfil/10/,
     &       rawin/12/, infile/13/, 
     &       datain/21/,lineio/23/, dataot/24/, altdat/27/, scrtch/30/ 

* the following flag is used in the creation of line list backup files
	data linbak/.true./    ! line list is backed up when .true.

c  14 is used for menu
c  19 is used for temporary storage
c	INPUT is the fortran unit number for command input.
c	STDIN is the unit number always associated with primary command input.
c	OUTPUT is the unit number where output is to be directed.
c	STDOUT is always associated with the primary output (the terminal).
c	CARDIN is used for auxilary command input...a card image file
c		for instance.
c	INFILE is used for other formatted input (typically data or params)
c	CARDOUT is the unit number used for card image output
c		linelist uses cardout as a formatted output file.
c	SPOOL is the unit number associated with a temporary spool file
c		This is used for directing output to a line printer.
c	LOGFIL is used for logging commands
c	HLPFIL is the file used by the help command, contains documentation
c		on the commands.
c	DATAIN  is the primary data input unit(read only).
c		21 is .hdr, 22 is .dat, 23 is .lin
c	DATAOT  is the only data output unit...
c		24 is .hdr, 25 is .dat, 26 is .lin
c	ALTDAT  is the alternate data input unit(read only).
c		27 is .hdr, 28 is .dat, 29 is .lin
c	SCRTCH is the unit associated with the save and recall
c		commands.
 
c    nrtr is the maximum number of points which can be usefully processed
c	in the r array.  Because of the algorithms used the actual 
c	dimension of the r array must be at least 2 more than nrtr;
c	because of the fft nrtr must be a power of 2.
c	Nlins is the maximum number of lines which can be simultaneously fit.

c	data (p(i),i= 1, 26) - used for voigt area calculations
	data p	/  1.064467, 1.081922, 1.099638, 1.117619,
     *    1.135864, 1.154377, 1.173157, 1.192206, 1.211524, 1.231108,
     *    1.250959, 1.271072, 1.291443, 1.312067, 1.332934, 1.354034,
     *    1.375351, 1.396866, 1.418555, 1.440383, 1.462309, 1.484277,
     *    1.506214, 1.528029, 1.549605, 1.570797/
c note that p(b1=0.05) = 1.108595

c	data (bgg(i),i=   1,  51)
	data bgg	 / .360674, .352999, .345341, .337700,
     *    .330075, .322468, .314878, .307305, .299750, .292213, .284693,
     *    .277192, .269708, .262243, .254797, .247369, .239960, .232570,
     *    .225199, .217847, .210515, .203202, .195909, .188636, .181382,
     *    .174148, .166935, .159742, .152569, .145416, .138284, .131172,
     *    .124080, .117009, .109959, .102929, .095919, .088930, .081961,
     *    .075013, .068084, .061176, .054288, .047420, .040573, .033747,
     *    .026944, .020164, .013411, .006687,0.000000/

	end
