c \gremlin\src\cardio.for   Ver. 2.2

c  Revision 2.25 99/10/24   Allow read of 2*nrtr in rdcard
c  Revision 2.24 95/08/03   Writefile "format" write fixed
c  Revision 2.23 94/06/26   Writefile writes binned unnormalized x,y pairs
c  Revision 2.22 94/01/01   Allow writefile to write unnormalized x,y pairs
c  Revision 2.21 90/02/17   use nrtr+2, not 4  
c  Revision 2.2  89/02/05  


	subroutine rdcard(r)

	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'

	real r(*)

	character*80 format, unit
	integer ionum, maxpts, j, lun
	external ionum

	maxpts = 2*nrtr + 2

	if (ifx .gt. 0) nop = inum(1)
	if (nstrng .ge. 1) format = alpha(1)(1:nalpha(1))

c					do we switch input to somewhere else?
	lun = input
	if (nstrng .ge. 2) unit = alpha(2)(1:nalpha(2))

c					map unit-name to fortran unit number

	if (nstrng .ge. 2) lun = ionum(unit,nalpha(2))
	if (lun .eq. -1) then
		write (*,*) 'No match to legal unit'
		 return
	end if

c					format was not provided on cmnd. line.
c					assume its the 1st line in the file.

	if (nstrng .eq. 0) read (lun, 9000, end=999) format
9000	format(a)

c					don't allow buffer overrun.
	if (nop .gt. maxpts) nop = maxpts

	read (lun, format, end=999, err=1999) (r(j), j= 1, nop)
999	return
1999	write (*,*) 'Error on readcards - data may not be valid'
	return
	end
c **************************************************************
c writefile  ['xy|bin']  filnam  [nop|decimation] ['format']
c use 'xy' flag to write unnormalized x,y pairs
c use 'xybin' flag to write binned unnormalized x,y pairs

	subroutine writec(r)

	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'

	real r(*)

	character*80 format, filnam

	mode = 1
	if (alpha(1)(1:2) .eq. 'xy') then
	    mode = 2
	    if (alpha(1)(3:5) .eq. 'bin') mode = 3
	    nstrng = nstrng - 1
	    alpha(1) = alpha(2)
	    nalpha(1) = nalpha(2)
	    alpha(2) = alpha(3)
	    nalpha(2) = nalpha(3)
	endif

	filnam = ' '
	filnam = alpha(1)(1:nalpha(1))

	format = '(8f10.4)'
c  format was provided on cmnd. line - use it
	if (nstrng .eq. 2) format = alpha(2)(1:nalpha(2))

      	open (40,file=filnam,access='sequential',
     *		 form='formatted',status='unknown',err=1998)

	nout = nop
	if (mode .ne. 3 .and. ifx .gt. 0) nout = inum(1)
	if (mode .eq. 2) nout = 2*nout
	if (mode .eq. 3) nout = 2*(nop/inum(1))

	write (40, 199) id(1:69),wref, delw, nout, format
199	format ('info id ',1h',a69,1h'/ 'info wref ', f15.6/ 
     *          'info delw ', e20.11/ 'length ',i6/
     *          'readdata "',a60,'"')
	if (mode .eq. 1) then
	    call momnts
	    fnorm = 0.001*absmx
	    write (40,format,err=1999)(r(j)/fnorm,j=1,nout)
	    write (40,201) fnorm
201	    format ('multiply ',e16.8)

	elseif (mode .eq. 2) then
	    write (40, format, err=1999) 
     *		((wref + delw*(j-1)),r(j),j=1,nout/2)
	else
	    nbin = inum(1)
	    rtop = 1.0e20
	    rbot = -1.0e20
	    rcur = 0.0
	    do i=1,nop,nbin
		rmax = r(i)
		rmin = r(i)
		do j=i,i-1+nbin
		    if (rmax .gt. r(j)) rmax = r(j)
		    if (rmin .lt. r(j)) rmin = r(j)
		enddo
		ww = wref + delw*(i-1+0.5*nbin)
		if (rbot .ge. rmax) then	! go to next top
	            write (40, format, err=1999) ww,rmax,ww,rmin
		    rcur = rmin
		elseif (rtop .le. rmin) then	! go to next bottom
	            write (40, format, err=1999) ww,rmin,ww,rmax
		    rcur = rmax
		else				! go to closest
		    if (abs(rcur-rmax) .gt. abs(rcur-rmin)) then
	                write (40, format, err=1999) ww,rmin,ww,rmax
		        rcur = rmax
		    else
	        	write (40, format, err=1999) ww,rmax,ww,rmin
			rcur = rmin
		    end if
		end if
	    end do
	end if

	write (40,*) 'keyboard' 
2000	close (40)
	return

1998	write (*,*) 'Error on writecards - file open failed'
	go to 2000

1999	write (*,*) 'Error on writecards - data may not be valid'
	go to 2000
	end
