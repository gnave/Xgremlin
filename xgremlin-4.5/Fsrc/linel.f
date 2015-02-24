c \grammy\src\linel.for   Ver. 2.2

c  Revision 2.22 93/03/20  Changed to transparent access for Grammy
c			   Ascii file, intensity correctiom dropped
c  Revision 2.21 91/05/02  Allow linelist to set damping, width or holds

	subroutine linel(r,tr)

c------------------------------------------------------------------------
c linelist smin scale 
c linelist smin scale wstrt wend
c linelist smin scale wstrt wend mode 
c linelist 'sethold' ihold  (1 = dmp, 2 = width, 3 = both)
c linelist 'setdamping' damping  (0-1.0)
c linelist 'setwidth' width  (mK)

c 	smin, scale: are required
c 	wstrt, wend: are optional; default is whole file
c 	mode is optional and may be dropped.

c 	Linelist produces a findlines type list over an extended range.
c 	smin	- minimum line strength to be listed.
c 	scale	- the factor by which the rescaled data is multiplied
c 	wstrt	- the wavenumber of the first point.
c 	wend	- the wavenumber of the last point
c 	mode	- specifies method used to determine wavelength(number).
c 		0  (default) average of peak(minimum) and second derivative
c 		1  peak(minimum) position
c 		2  second derivative
c------------------------------------------------------------------------
*
* Data layout in a .lin file record:
* ----------------------------------
*
* variable    type           size/bytes
* --------    ----           ----------
* sig         real*8         8
* xint        real           4
* width       real           4
* dmping      real           4
* itn         integer*2      2
* ihold       integer*2      2
* tags        character*4    4
* epstot      real           4
* epsevn      real           4
* epsodd      real           4
* epsran      real           4
* spare       real           4
* ident       character*32   32
*

	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'
	include 'set.h'
	include 'linparms.h'

	real r(*),tr(*)

	double precision temp, sigma
	double precision wstrt,wend
	integer nbstart, i
	integer mode
	integer*2 itn,ihold
	real xint, width, dmping
	real epstot, epsevn, epsodd, epsran, spare
	integer itrdref
	integer idmpflg, iwidflg

	character filnam*60
	integer itrflag

	double precision dpbuf(10)
	character*1 chbuf(80)
	integer ibuf(20), ihdr(20)
	real buf(20), hbuf(20)
        character tags*4, ident*32

	equivalence (dpbuf(1),chbuf(1), ibuf(1),buf(1))
	equivalence (sigma, chbuf( 1)), (xint,  chbuf( 9))
	equivalence (width, chbuf(13)), (dmping,chbuf(17))
	equivalence (itn,   chbuf(21)), (ihold, chbuf(23))
	equivalence (tags,  chbuf(25)), (epstot,chbuf(29))
	equivalence (epsevn,chbuf(33)), (epsodd,chbuf(37))
	equivalence (epsran,chbuf(41)), (spare, chbuf(45))
	equivalence (ident, chbuf(49))
	equivalence (ihdr(1), hbuf(1))

* initialize some of the variables ( replace data statements )
* ( a data statement does probably not what was intended here )
	idmpflg = 0
	iwidflg = 0
	itn     = 0
	epstot  = 0.0
	epsevn  = 0.0
	epsodd  = 0.0
	epsran  = 0.0

c Test for flags to set
	if (nstrng .eq. 1 .and. ifl .lt. 2) then
	    if (alpha(1)(1:7) .eq. 'sethold') then
		ihold = inum(1)
	    end if
	    if (alpha(1)(1:7) .eq. 'setdamp') then
		dmpl = xnum(1)*25.0 + 1.0
		idmpflg = 1
	    end if
	    if (alpha(1)(1:8) .eq. 'setwidth') then
		widl = xnum(1)
		iwidflg = 1
	    end if
	    return
	end if

	call itron
	linum = 0
	smin = xnum(1)
	wstrt = wstart
	wend = wstop
	wcorr = 0.0d+0
	scale = 0.0
	if (ifl .ge. 2) then
	   wstrt = xnum(2)
	   wend  = xnum(3)
	end if
	mode = 0
	if(ifx .ge. 1) mode=inum(1)
	ident = ' '
	tags(1:4) = '   F'

*
* I/O UNITS:
* datain   : header file
* datain+1 : data file
* lineio : line list file (also unit lineio)
*
	if (uopen(lineio) .eq. 0) then
	   ik = index(fnames(datain),' ') - 1 ! cobble together file name
	   filnam = ' '
	   filnam = fnames(datain)(1:ik)
	   filnam(ik+1:ik+4) = '.lin'   
	   open (unit=lineio,file=filnam,access='direct',recl=80,
     &		 status='replace',iostat=ierr)
	   if (ierr .ne. 0) then
	      call wrtstr(' .lin file create failed - aborted')
	      uopen(lineio) = 0
	      return
	   else
	      call wrtstr(' new line file opened')
	      uopen(lineio) = 1
	      fnames(lineio) = filnam
	      linbak = .false.	! no backup is needed for new file
	   endif
	end if

c As a temporary storage, open FILE 39
	open (39,file='linelist_temp',status='replace',
     &        form='unformatted')

c replace datain delw
	delw = disp
 
c find start of read
	if (wstrt .lt. wstart) wstrt = wstart + 16.0*delw
	if (wstrt .ge. wstop) go to 9996
c adjust end point if necessary
	if (wend .gt. wstop) wend = wstop
c initialize loop, parameters for read. Save rdref (replace at end)
        ifl = 1
        ifx = 1
	nstrng = 0
	rdnorm = .false.
	itrdref = rdref
	rdref = 0
c read raw data for start 
        xnum(1) = wstrt - 16.0*delw
        inum(1) = 4096
        call read(r,tr)
        if (ipara .ne. 0) go to 996
	call disper
	aps = xpara
	ipara = 15
c start loop
	call wrtstr(' Writing raw lines to scratch file')
2300    continue
	call procpend
	if (itrflag() .eq. 1) go to 2800
   	if (ipara .gt. 3800) go to 2600
    	iparc = 1
	iparb = mode
	ipara = ipara - 1
	call lnseek(r)
c Input starting location in ipara. Returns ready for next line
c iparb = 2 for 2nd deriv determination, 1 for 1st deriv, 0 for average
c Refined location returned in iparb - iparb=0 means out of range.  
c iparc=0 if line nearest ipara wanted, iparc=1 if next line wanted.  
c Return parameters - strength in xpara, exact position in xparb, 
c  width in xparc.  damping of 0.10 (dmp=3.5) assumed.
	if (iparb .eq. 0) then
	   if (nop .ne. 4096) go to 2900
	   go to 2600
	end if
	xint = xpara
	if (xint .lt. smin) go to 2300
	temp = xparb
	call interp(r)
	xparb = temp
	call ptow
	width = 1000.0 * aps * xparc
	sigma = xparb
        if (sigma .ge. wend)  go to 2900

c write record
	linum = linum + 1
        if (mod(linum,50) .eq. 0) then
	   write (wrtbuf,'(2h +,i5,6h lines)') linum
	   call prevline(wrtbuf)
	end if
	write (39) sigma,xint,width

 2600	if (ipara .gt. 3800) then
	   xparb = ipara
	   call ptow
	   xnum(1) = xparb - 16.0*delw
	   inum(1) = 4096
	   call read(r,tr)
	   if (ipara .ne. 0) go to 2800
	   call disper
	   ipara = 15
	end if
        go to 2300

2800    continue
	call wrtstr(
     &        ' Abnormal read termination - processing existing lines')
2900    write (wrtbuf,'(1x,i5,a)') linum,
     &        ' lines written to scratch file'
	call wrtstr(wrtbuf)

	call wrtstr(' Writing lines to permanent file')

	rewind(39)

* first write the header of the .lin file
* save linum, ntop (= address of NEXT write, not last byte)
	call linhdr(linum,nerr,real(smin),real(scale),real(wcorr))
	if ( nerr .ne. 0 ) goto 997
	nbstart = 320

	ident = 'no id'

	do 5490 i=1,linum

	   read (39) sigma,xint,width

	   if (iwidflg .eq. 1) then
	      width = widl
	      if (widl .lt. 1.0) width = 1000.0*widl*sigma
	   endif
	   if (idmpflg .eq. 1) then
	      dmping = dmpl
	   endif
	   
* write a line to .lin (all variables equiv. to buf)
	   call wrtline(lineio,nbstart,ibuf,nerr)
	   if (nerr .ne. 0) go to 997
	   nbstart = nbstart + 80
 5490	continue

	write(wrtbuf,5495) linum
	call wrtout(output,wrtbuf)
5495	format(1x,i6,' lines saved in .lin file')

	close(39,status='delete')

	rdref = itrdref
	call itroff
	return

 9996	write(wrtbuf,'(a)') ' READ Abort -- Wstrt outside file range'
	call wrtout(output,wrtbuf)
	call itroff
	return
 996	call wrtstr(' Error :  .lin file read failed')
	call itroff
        return
 997	call wrtstr(' Error :  .lin file write failed')
	call itroff
        return
	end



