c  gremlin\src\ftsout.for  Ver 1.0

c Revision 1.07  05.26.93 Demand real filenames ending in 'r'
c Revision 1.06  05.15.93 If no flags are set, do amplitude for snap
c Revision 1.05  01.09.91 Handling of wavelimits changed
c Revision 1.04  14.08.91 Delete handling of even aliases here
c Revision 1.03  04.06.91 .spe changed to .dat
c Revision 1.02  26.05.91 nwpp handling updated; 'save' added; stats float
c Revision 1.01  16.05.91 lores added here; min. 128 pts; stat* if output
c Version  1.00  04.04.91 Major reconstruction

	subroutine ftsout(r,ffta)

	include 'outparms.h'
	include 'inparms.h'
	include 'datetc.h'
	include 'iounit.h'
	include 'infmtn.h'
	include 'altinf.h'
	include 'extinf.h'

	real r(*), ffta(*)

	common/leftout/sigma0,sigmax
	double precision sigma0,sigmax
	double precision delwp,wi,t1,t2
	logical lex

c nowrt is set in stats at output time 
	logical nowrt
	character*10 ia1,ia2
	character*80 filnam

c prepare to write a file in Decomp format (pure binary)
	if (nstrng .eq. 1) then
	   k = nalpha(1)
	   filnam = alpha(1)(1:k)
	   filnam(k+1:k+4) = '.dat'
	else
	   call wrtstr(' Output file name <filename>.dat missing.')
	   return
	end if

* check if output file exists already and write a warning
	inquire( file=filnam, exist=lex )
	if ( lex ) then
	   call wrtstr(
     &     ' Warning :  existing output file is overwritten.')
	   return
	end if

	open (34,file=filnam,form='unformatted',status='unknown',
     &        access='direct',recl=16384,err=9101)
	go to 9105

 9101	call wrtstr(' File open error - aborting')
	return

 9105	call remark('Output phase.  Options:')

	ia1 = 'amplitude '
	if (realfl)  ia1 = 'real      '
	if (imagfl)  ia1 = 'real      '
	if (cmplex)  ia1 = 'complex   '
	ia2 = 'No'
	if (acorrf) ia2 = 'Do'

	write(logfil,2000) ia1
	write( wrtbuf,2000) ia1
	call wrtout(6,wrtbuf)
	write(logfil,2001) ia2
	write( wrtbuf,2001) ia2
	call wrtout(6,wrtbuf)
 2000	format(' Type of output: ',a10)
 2001   format(' ',a2,' air correction')

c   set up constants for output blocks
	call oscrread(18)
	nop = 4096
	if (cmplex) then
	   nwpp = 2
	else
	   nwpp = 1
	end if

	aliashi = freespec*alias
	aliaslo = aliashi - freespec
c if not set, default wavelimits are alias limits
	if (outlo .eq. 0.0) outlo = aliaslo
	if (outhi .eq. 0.0) outhi = aliashi
c wavelimits command values override header values or defaults
	if (xwstart .gt. 0.0) outlo = xwstart
	if (xwstop .gt. 0.0) outhi = xwstop
	wstart = outlo
	wstop = outhi

	write(logfil,2002) wstart,wstop
	write(wrtbuf,2002) wstart,wstop
	call wrtout(6,wrtbuf)
2002	format(' Output limits:',f10.2,' to ',f10.2)

	delw = freespec/(noptex/2)
	delwp = delw

	call page(2)

c write header for statistics
	write(wrtbuf,1000) ia1,delw,sigma0,sigmax
	call wrtout(6,wrtbuf)
	write(logfil,1000) ia1,delw,sigma0,sigmax
 1000	format(' ',a10,'  delw =',f12.9,', sigma0 ='
     *        ,f11.4,', max. sigma =',f11.4)

	write(logfil,1001)
 1001 format (1x,'Block',4x,'Wref',7x,
     *      'Realmax',4x,'Realmean',5x,'RealRMS',6x,
     *       'Im/Re',5x,'ImagRMS')

	if(nseq .lt. 2) return

c full transform output. 
c Initialize counter  and lengths
	nrc = 0  			! output block counter in stats
	iptcnt = 1
	lnth = blksiz
	lnthr = blksiz/2
	nblks = noptex/lnth
	nblout = 0
	write (wrtbuf,101)
	call wrtstr(wrtbuf)
	write (wrtbuf,102) nblks
	call wrtstr(wrtbuf)
 101	format(' Writing transformed data to output file')
 102	format(' of',i4,' complex blocks to be processed')
	lpg = 55
	if (cmplex) lpg = 28

	do 150 k=1,nblks				! START LOOP

c   transfer a complex block from the huge array
	    call rmove (blksiz,ffta(iptcnt),r(1))

c print stats, write to output
c real/imag/complex handling 

	   if (cmplex) then
	      call stats(delwp,lnthr,nowrt,nrc,r)
	      if (.not. nowrt) then
		 nblout = nblout + 1
		 if (nblout .eq. 1) wi = wref
		 write (34,rec=nblout) (r(i),i=1,lnthr)
	      end if
	      do i=1,lnthr
		 r(i) =  r(i+lnthr)
	      end do
	      call stats(delwp,lnthr,nowrt,nrc,r)
	      if (.not. nowrt) then
		 nblout = nblout + 1
		 if (nblout .eq. 1) wi = wref
		 write (34,rec=nblout) (r(i),i=1,lnthr)
	      end if
	   else
	      call stats(delwp,lnth,nowrt,nrc,r)
	      if (imagfl) then
		 do i=1,lnthr
		    r(i) = r(2*i)
		 end do
	      end if
	      if (realfl) then
		 do i=1,lnthr
		    r(i) = r(2*i-1)
		 end do
	      end if
c       if no flags are set, do amplitude for snap
	      if (.not. realfl .and. .not. imagfl) then
		 do i=1,lnthr
		    r(i) = sqrt(r(2*i-1)**2 + r(2*i)**2)
		 end do
	      end if
	      if (.not. nowrt) then
		 nblout = nblout + 1
		 if (nblout .eq. 1) wi = wref
		 write (34,rec=nblout) (r(i),i=1,lnthr)
	      end if
	   end if

c increment counters, loop
 105	   iptcnt = iptcnt + blksiz

	   write (wrtbuf,'(2h +,10x,i4)') k
	   call prevline(wrtbuf)
	   if ( mod(k,lpg) .eq. 0) then
	      call page(2)
	      write(logfil,1001)
	   end if
 150	continue		! END LOOP
	call wrtstr(' >>> Finished writing output file')
	close(34)
	 
	call wrtstr(' >>> Writing output file header')
	k = index(filnam,'dat')
	filnam(k:k+2) = 'hdr'
	open (34,file=filnam,form='formatted',access='sequential')
	t1 = wstart
	t2 = wstop
	ntrans = noptex
	npo =  nblout*4096
	wstart = wi
	wstop = wi + delwp*( (npo/nwpp) - 1)
	bocode = mbocode	! set it to machine byte order code
	fboffs = 0
	xaxis_is = 'Wavenumber'
	call infwrt(34)
	close(34)
	delw = delwp
	wstart = t1
	wstop = t2
	 
	return 
	end

c-------------------------------------------------------------------

	subroutine stats(delwp,lnth,nowrt,nrc,r)

c   calculate fft block reducer statistics 

	include 'datetc.h'
	include 'infmtn.h'
	include 'outparms.h'
	include 'iounit.h'

	real r(*)

	common/leftout/sigma0,sigmax
	double precision sigma0,sigmax
	double precision delwp
	logical nowrt

	absamax = 0.0
	absbmax = 0.0
	amean = 0.0
	bmean = 0.0
	rmsa = 0.0
	rmsb = 0.0
	nnop = lnth/2

	do i=1,lnth,2
	    if(absamax .lt. abs(r(i))) then
		amax = r(i)
		absamax = abs(amax)
	    end if
	    if(absbmax .lt. abs(r(i+1))) then
		bmax = r(i+1)
		absbmax = abs(bmax)
	    end if
	    amean = amean + r(i)
	    bmean = bmean + r(i+1)
	    rmsa = rmsa + r(i)**2
	    rmsb = rmsb + r(i+1)**2
	end do

c	absmax = max1(absamax,absbmax)
	amean = amean/nnop
	rmsa = dsqrt(rmsa/nnop)
	bmean = bmean/nnop
	rmsb = dsqrt(rmsb/nnop)

	nrc = nrc + 1
	wref = sigma0 + (delwp*nop*(nrc-1))/nwpp
	wlast = wref + (nop-1)*delwp/nwpp

c   apply air index correction if necessary
	if (acorrf) call aircor(delwp)

c write stats
	nowrt = .false.
	if ((wstop .lt. wref) .or. (wstart .gt. wlast)) nowrt = .true.

	if (nowrt) then
	    write(logfil,1001) nrc, wref,amax,amean,
     *               rmsa,bmean/amean,rmsb
1001	    format (1x,i3,f12.4,f13.0,2f12.0,f10.4,f12.0)
	else
	    write(logfil,1003) nrc, wref,amax,amean,
     *               rmsa,bmean/amean,rmsb
1003	    format (1x,i3,'*',f11.4,f13.0,2f12.0,f10.4,f12.0)
	endif

	return
	end

c----------------------------------------------------------------------

c Read from ffta; transform, phcorr, write to output

	subroutine lores(r,ffta,phz)

	include 'altinf.h'
	include 'datetc.h'
	include 'inparms.h'
	include 'outparms.h'
	include 'infmtn.h'
	include 'iounit.h'
	common/leftout/sigma0,sigmax
	double precision sigma0,sigmax

	real r(*), ffta(*), phz(*)

	character*60 filnam

	if (imax .gt. ngot .or. imax .lt. 0) then
	   write(logfil,9023) imax
	   write(wrtbuf,9023) imax
	   call wrtstr(wrtbuf)
9023  format(i9,' Location of central fringe is missing or incorrect.',
     *		'  Low resolution will be skipped.')
	   return
	end if

c  zero r in case we can't fill it

	do j=1,nlores
	    r(j) = 0.0
	end do

c check wanted limits against those we can actually get:

	nw1 = imax - nlores/2
	ng1 = nw1
	if (nw1 .lt. 1) ng1 = 1
	nleft = imax - ng1

	nw2 = imax + nlores/2 - 1
	ng2 = nw2
	if (nw2 .gt. npo) ng2 = npo
	nright = ng2 - imax + 1

	if (nleft .lt. nright) ng2 = imax + nleft - 1   ! keep symmetry

	if (ng1 .ne. nw1 .or. ng2 .ne. nw2) then
	   write (logfil,11)
	   write (wrtbuf,11)
	   call wrtstr(wrtbuf)
11	    format(' Imax too close to end for full low-resolution ',
     *     'transform.  Remainder of array filled with zeroes.')
	endif

	n = ng2 - ng1 + 1
	if (n .lt. 128) go to 1000

	call rmove (n,ffta(ng1),r(1 + ng1 - nw1))

	i = nstrng 
	nstrng = 0
	nop = nlores
	call oscrwrite(19)
	nstrng = i

c  apodize by multiplying with the appropriate masked Gaussian
c   nwidth is the FWHM of the gaussian; nrise is for the cosine bell mask
	nwd = 2*nlores/5 			! nlores/2.5
	nrise = nlores/50
	gcon = (0.5*nwd)/sqrt(log(2.0))
	ccon = 6.2831853/nrise
	r(1) = 0.0
	do j = 2,nlores/2
	    xp = (float(j - nlores/2 + 1)/gcon)**2
	    xx = 0.0
	    if (xp .lt. 35.0) xx = exp(-xp)
	    if (j .le. nrise) xx = xx*sin( float(j-1)*ccon)**2
	    r(j) = xx*r(j)
	    r(nlores+2-j) = xx*r(nlores+2-j)
	end do

c   do complex transform; multiply by nlores/2 to keep old normalization

	call strtrn
	call fast(r(1),np)
	do i = 1,nlores
	    r(i) = r(i) * nlores/2
	end do

c   do phase correction

	k = nphz/nlores
	if (k*nlores .ne. nphz) then
	    call wrtstr(
     &      ' Error :  Phase array smaller than lores - no correction')
	else
	  do i=1,nlores,2
	    jpts = 1 + ((i-1)/2)*k
	    arg = -phz(jpts)
	    sa = sin(arg)
	    ca = cos(arg)
	    temp = r(i)*ca - r(i+1)*sa
	    r(i+1) = r(i+1)*ca + r(i)*sa
	    r(i) = temp
	  end do
	end if

c calculate statistics for later printing

	absamax = 0.0
	absbmax = 0.0
	amean = 0.0
	bmean = 0.0
	rmsa = 0.0
	rmsb = 0.0
	amax = 0.0
	bmax = 0.0
	nnop = nlores/2

	do 100 i=1,nlores,2
	   if(absamax .lt.  abs(r(i))) then
	      amax = r(i)
	      absamax = abs(amax)
	   end if
	   if(absbmax .lt.  abs(r(i+1))) then
	      bmax = r(i+1)
	      absbmax = abs(bmax)
	   end if
	   amean = amean + r(i)
	   bmean = bmean + r(i+1)
	   rmsa = rmsa + r(i)**2
	   rmsb = rmsb + r(i+1)**2
 100	continue
	absmax = max1(absamax,absbmax)
	amean = amean/nnop
	rmsa = sqrt(rmsa / nnop)
	bmean = bmean / nnop
	rmsb = sqrt(rmsb/nnop)

	stalow(4) = absmax
	stalow(5) = amax
	stalow(6) = amean
	stalow(7) = rmsa
	stalow(8) = bmax
	stalow(9) = bmean
	stalow(10) = rmsb
	wref = sigma0
	stalow(3) = wref

c write the lores file in PCDecomp format (pure binary)
	if (nstrng .eq. 1) then
	   k = nalpha(1)
	   filnam = alpha(1)(1:k)
	   filnam(k+1:k+4) = '.dat'
	else
	   call wrtstr('Output file name <filename>.dat) missing.')
	   return
	end if
	open (34,file=filnam,form='unformatted',
     *		access='direct', recl=4*nlores)

	write (34,rec=1) (r(i),i=1,nlores)
	call wrtstr(' Finished writing lowres file')
	close(34)
	call wrtstr(' Writing lowres file header')
	k = index(filnam,'dat')
	filnam(k:k+2) = 'hdr'
	open (34,file=filnam,form='formatted',
     *		access='sequential')

	xpara = wstart
	xparb = wstop
	xparc = resolutn
	i = npo

	inftype = 4
	nrec = 0
	npo = nlores
	npt = 1
	nwpp = 2
	delw = freespec/nlores
	resolutn = delw
	wstart = sigma0
	wstop = sigmax
	call infwrt(34)

	wstart = xpara
	wstop = xparb
	resolutn = xparc
	npo = i

      return


1000	write (wrtbuf,1001)
	call wrtstr(wrtbuf)
	write (logfil,1001)
1001  format(' No points saved for low-resolution transform.',
     *       ' Transform skipped.')
	return
      end
c -------------------------------------------------------------------

