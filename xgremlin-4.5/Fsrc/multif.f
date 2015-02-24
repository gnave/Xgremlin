c \decomp\src\multif.for  Ver. 1.0

c *******************NOT CONVERTED YET******************

	subroutine multif

        include 'datetc.h'

	double precision wstrt, wstp

	character*20 string
	integer nch

	if (nstrng .le. 0) go to 999
	nch = nalpha(1)
	string = alpha(1)(1:nalpha(1))

	call lcase(string,nch)

	if (string .eq. 'ratio') 	go to 6000

	go to 999

c ratio
c *****
c command: multifile ratio wstrt wstp

c ratio a data file on "datain" to a data file on altdata;
c altdata is the lower resolution file.
c ratio assumes:	open datain filename1 
c			open altdata filename2
c			create filename3
c have previously been executed.

6000	if (ifl .ne. 2) go to 998
	wstrt = xnum(1)
	wstp = xnum(2)

        return

998	call wrtstr(' syntax error for multifile command')
	return

999	call wrtstr(' specified multifile command not implemented')
	return

	end
