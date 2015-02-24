c gremlin\src\ftsin.for  Ver. 1.0

c Revision 1.17  06.12.93  Common variables nphzmax, nfftamax used
c Revision 1.16  02.12.93  phcorr parameters slim, tol changed
c Revision 1.15  09/11/93  drop inputonly (nseq = 0)
c Revision 1.14  04/11/93  phcorr: max S/N=0.001; badpt fname from rawin
c			     nphz auto to 131072; noise as fraction
c Revision 1.13  16/10/93  Nrise set to nphz/2 in amparg
c Revision 1.12  06/08/93  Remove nrtr = nphz????
c Revision 1.11  06/07/93  ntrans = 0 defaults to 'extend 2'
c Revision 1.10  05/29/93  Allow phcorr for n < nphz
c Revision 1.09  05/26/93  logamp on finer scale; delw, wref set in 18
c Revision 1.08  05/20/93  revert to 2**21
c Revision 1.07  05/06/93  Auto inskip, phase translation errors fixed
c Revision 1.06  30.10.92  Allow 2**22 points in transform
c Revision 1.05  01.09.91  change handling of transform length
c Revision 1.04  16.08.91  Correct handling of inskip, short left side
c Revision 1.03  14.08.91  Delete handling of even aliases
c Revision 1.02  17.05.91  Add auto centering if input skip
c Revision 1.01  16.05.91  Auto lores dropped; skip phase if nocorr
c Version  1.00  06.04.91  Block structure deleted

	subroutine ftsin(ieret,r,tr,ffta,phz)

	include 'datetc.h'
	include 'inparms.h'
	include 'outparms.h'
	include 'infmtn.h'
	include 'altinf.h'
	include 'iounit.h'
	include 'set.h'
	include 'phase.h'
	include 'integrate.h'
	include 'chebychev.h'

	parameter(nkill = 2048)

	real r(*), tr(*), ffta(*), phz(*)

	double precision sigma0,sigmax,tmpwref, tmpdelw

	common /leftout/sigma0,sigmax

	real cutfrac,epsi,slim,tol,chisqr,ampmax
	real am(512),omc(512),csq(512)
	integer nterms,mode,kill(nkill),klm,nraw,npnts,kmax,nx(512)
	integer ibig(32768)
	character badfile*256
	common/ftp/cutfrac,epsi,slim,tol,chisqr,ampmax,am,omc,csq
	common/ftp/nterms,mode,kill,klm,nraw,npnts,kmax,nx
	common/ftp/ibig,badfile

c input        read in n points, following skip if necessary
c *****		OR read central fringe area only to r for processing
c       	OR just multiply whole interferogram by -1.0
c		calls:  input; input n; input center; input inverted

	ieret = 0			!  ieret = 0 for normal return

c recall header, adjust parameters for reading data
	call oscrread(18)

c change the sign of the interferogram if phase is too close to +-pi
	if ( alpha(1)(1:8) .eq. 'inverted' .and. invflg .eq. 0) then
	    do i=1,npo
	       ffta(i) = -ffta(i)
	    enddo 
	    invflg = 1
	    return
	endif

! 1. Center position
c   Decide on mechanism for locating center, set by value of incntr:
c     originally -1 (= use ncenter from info); or
c     from an NCENTER call:
c 	if 0, find peak from data
c 	if >0, use incntr from command

	if (incntr .eq. -1) imax = ncenter
	if (incntr .eq. 0) imax = 0
	if (incntr .gt. 0) then
	   imax = incntr
	   ncenter = incntr
	end if

	if (incntr .eq. 0) then		! if 0, look for peak
	   call wrtstr(' Looking for peak')
	   rmin =  10.e36
	   rmax = -10.e36
	   imax = 1
	   imin = 1
	   do i=1,nint
	      if (ffta(i) .gt. rmax) then
		 imax = i
		 rmax = ffta(i)
	      elseif (ffta(i) .lt. rmin) then
		 imin = i
		 rmin = ffta(i)
	      end if
	   end do
	   if (abs(rmin) .gt. abs(rmax)) then
	      rmax = rmin
	      imax = imin
 	   endif
	   write (wrtbuf,'(a,i8)') ' Peak found at imax = ',imax
	   call wrtstr(wrtbuf)
	   write (logfil,'(a,i8)') ' Peak found at imax = ',imax
	end if

c Now do a Fourier interpolate (8x) about the chosen center to see if we
c were fooled by unfortunate sampling.  Move center to r 
	IF (incntr.le.0) THEN  ! only if not explicitly set
	   call wrtstr(' Interpolating for peak check')
	   write (logfil,'(a)') ' Interpolating for peak check'
	   nn = 4096
	   nf = 32768
	   nstart = (imax - nn/2)
	   call rmove (nn,ffta(nstart),r(1))
c       mask, rise width iparc, half width iparb
	   iparb = nn/2
	   iparc = nn/32
	   call mask(r)
	   call fast(r(1),nn)
	   do i=nn+1,nf+2
	      r(i) = 0.0
	   end do
	   call fsst(r(1),nf)
c find max of stretched data
	   trmin =  10.e36
	   trmax = -10.e36
	   imx = 1
	   imn = 1
	   do i=1,nf
	      if (r(i) .gt. trmax) then
		 imx = i
		 trmax = r(i)
	      elseif (r(i) .lt. trmin) then
		 imn = i
		 trmin = r(i)
	      end if
	   end do
	   if (abs(trmin) .gt. abs(trmax)) then
	      trmax = trmin
	      imx = imn
	   endif
	   xpara = float(nstart) + float(imx - 1)*nn/nf
	   imx = xpara +0.5d0
	   if (imx .ne. imax) then
	      write (wrtbuf,111) imax, imx
 111	      format (' imax moved from ',i5,' to ',i5)
	      call wrtstr(wrtbuf)
	      write (logfil,111) imax, imx
	      imax = imx
	      rmax = trmax
	   endif
	ENDIF

! 2. Length
c   Decide on mechanism for determining number of input points:
c    if inskip=0 and nwanted=0, take all
c    if inskip>0 and nwanted=0, take rest after skipping
c    if inskip=0 and nwanted>0, take centered nwanted (must follow ncenter)
c    if inskip>0 and nwanted>0, take nwanted after skipping (ignore imax)
	nwanted = 0
	if (ifx .ge. 1 .and. inum(1) .ne. 0) nwanted = inum(1)
	nposbl = nint - inskip
	if (nwanted .gt. nposbl) nwanted = nposbl
	if (nint .ne. 0 .and. nwanted .eq. 0) nwanted = nposbl
	if (inskip.eq.0 .and. nwanted.gt.0 .and. .not.onesidfl) then
	   nposbl = 2*min(ncenter,nint - ncenter)
	   if (nwanted .gt. nposbl) nwanted = nposbl
	   inskip = ncenter - nwanted/2
	endif

c center now fixed - correct for skip
	imax = imax - inskip

c adjust range parameters
	sigma0 = (alias - 1)*freespec
	sigmax = sigma0 + freespec

	call oscrwrite(18)			! save info in 18

	ngot = 0
	if (nphi .ge. 2) then
	   nphz = iphparms(2)
	else
	   nop = imax - 1
	   if (nop .gt. npo - inskip - imax) nop = npo - inskip - imax
	   nop = 2*nop
	   call strtrn
	   nphz = np
	endif
	if (nphz .gt. nphzmax) nphz = nphzmax
	write (wrtbuf,'(a,i8)') ' Nphz set to:',nphz
	call wrtstr(wrtbuf)
	nh = nphz/2

	ishftflg = 0
c if center only wanted, bypass usual input
	if (alpha(1)(1:nalpha(1)) .eq. 'center') then
	   nstart = (imax + inskip -1 - nh)
	   ngot = nwanted
	   call remark(' Reading central fringe area only')
	   goto 300
	end if

	call infout 				! print info block

c Set pointer just past initial skip if one was requested
	inptr = inskip + 1
	xcentr = -1.0
	nout = 0
	rsum = 0.0
c initialize bad point count
	klm = 0

	if (iprnt .gt. 0) then
	   write (wrtbuf,101) (nwanted + blksiz - 1)/blksiz
	   call wrtstr(wrtbuf)
 101	   format (' Processing raw data file: ',i4,' blocks expected')
	endif

	if (.not.lglitch) then
	   call wrtstr(
     &        ' >>>>> NO AUTOMATIC GLITCH REMOVAL <<<<<')
	   write (logfil,'(a)')
     &        '>>>>> NO AUTOMATIC GLITCH REMOVAL <<<<<' 
	end if

	curblk = 0
150	curblk = curblk + 1				! START INPUT LOOP

c Do a full fetch from big array if possible; else zerofill.  Sum.
	nn = nwanted - ngot				! number remaining
	if (nn .gt. blksiz) nn = blksiz
	if (nn .gt. 0) then
	   call rmove(nn,ffta(inptr),r(1))
	   do ia=1,nn
	      rsum = rsum + r(ia)
	   end do
	   ngot = ngot + nn
	   if (nn .ne. blksiz) then
	      do i=nn+1,blksiz
		 r(i) = 0.0
	      end do
	   endif
	endif

c this is the main reason for retaining a "block" structure
	if (lglitch) then
	   call glitch(r)	                        ! check glitches
	end if
	call rmove(blksiz,r(1),ffta(nout+1))		! replace shifted

	if (iprnt.gt.0) then 
	   write (wrtbuf,'(2h +,10x,i4)') curblk
	   call prevline(wrtbuf)
	end if

	nout = ngot
	inptr = inptr + blksiz
	if (ngot .lt. nwanted) go to 150		! read next block
							! END LOOP*******
	call wrtstr(' Finished reading input')
	nused = ngot

	ishftflg = 1
c Data has now been de-glitched and shifted to account for any skip
c Calculate final average and set max pt. (+ or -)
  	rsum = rsum/ngot
	if (incntr .eq. -1 .and. abs(rmin-rsum) .gt. abs(rmax-rsum))then
	   rmax = rmin
	   imax = imin
	   ncenter = imax + inskip
	endif

c Calculate required size for fft.  noptex is the extended number of pts
c use header value if given; if 0, do a default 'extend 2'
	if (ntrans .gt. 0) then
	   m1 = ntrans
	elseif (ntrans .eq. 0 .and. ixtend .eq. 1) then
	   ixtend = 2
	endif
	if (iprnt .gt. 2) then
	   write (wrtbuf,'(a,i8,a,i8)') ' NTRANS = ',ntrans,
     &                                  ' IXTEND = ',ixtend
	   call wrtstr(wrtbuf)
	end if
c but override if 'extend' has been used
	if (ixtend .gt. 1) then
	   if (onesidfl) then
	      m1 = 2*ixtend*(ngot-imax) - 1
	   else 
	      m1 = ixtend*ngot - 1
	   end if
	end if

	mpwr = 0
	m1 = m1 -1
190	mpwr = mpwr + 1
	m1 = m1/2
	if (m1 .gt. 0) go to 190

	noptex = 2**mpwr
	if (noptex.gt.ffta_size) then
	   call wrtstr(
     &     ' Error :  ffta array not enough to hold interferogram.')
	   call wrtstr(
     &     '          Use the alloc command to enlarge ffta array.')
	   return
	end if

c write zeroes to the end of the huge array to bring the number of 
c points up to a power of two. 
	
        if (noptex .ne. ngot) then
	   do j=ngot+1,noptex
	      ffta(j) = 0.0
	   end do
	end if

c Finally, calculate logamp display and save in 18; 
	if (ngot .gt. 0) then
	   call wrtstr(' Calculating logamp display, saving in 18')
	end if
	call oscrread(18)
	k = 1
	do j=1,ngot,512
	   r(k) = 0.0
	   do i=j,j+511
	      r(k) = r(k) + (ffta(i) - rsum)**2
	   end do
	   r(k) = 0.5*alog(r(k))
	   k = k + 1
	end do
	nop = k - 1
	tmpwref = wref
	tmpdelw = delw
	delw = 512.
	wref = 256.
	call oscrwrite(18)
	delw = tmpdelw
	wref = tmpwref

	go to 200

	entry newcentr(ieret)
c	*********************   entry for new choice of ncenter
	ncenter = incntr
	imax = ncenter - inskip

c if ncenter was set by command, fetch value at that point
200	if (incntr .ne. -1) rmax = ffta(imax)

c print input summary
	if (iprnt .gt. 0) then 
	    write(logfil,201)
201 	    format(' Input results: ')
	    call wrtstr(' Input results:' )
	    write(logfil,2071) ngot,noptex
	    write(wrtbuf,2071) ngot,noptex
	    call wrtout(6,wrtbuf)
	    write(logfil,2072) rsum
	    write(wrtbuf,2072) rsum
	    call wrtout(6,wrtbuf)
	    write(logfil,2073) rmax,ncenter,imax
	    write(wrtbuf,2073) rmax,ncenter,imax
	    call wrtout(6,wrtbuf)
 2071	    format(1x,i12,' points read from rawin, extended to',i12,
     &             ' points') 
 2072	    format(' Mean of ',1p,e16.8)
 2073	    format(' Center value ',1p,e16.8,' at raw point',
     &            i12,', final point',i12)
	endif

	if ( ((ngot-imax) .lt. (ngot/2) ) .and. onesidfl) then
	   write(wrtbuf,211)
	   call wrtstr(wrtbuf)
	   write(logfil,211)
	endif
211	format(' Long side of interferogram preceded short side',
     &         ' - weighting will be incorrect.')

c Unless nocorr, begin phase determination - replace main id block
300	call oscrread(18)

        if (.not. phcorfl) go to 1000           ! start phase correction
	
	nop = nphz

	write (wrtbuf,'(a,i8)') ' Using ncenter =',ncenter
	call wrtstr(wrtbuf)

c check availability of a central fringe
	nstart = imax + inskip - 1 - nphz/2
	if (ishftflg .eq. 1) nstart = imax - 1 - nphz/2

	if (onesidfl) then
	   if (nstart .lt. 0) then		! Onesided, nstart < 0
c ***************** this needs re-working (much like the rest :-) **********
	      nnn = 2*imax + 1
	      nphz = 512
310	      nphz = nphz*2
	      if (nphz .le. nnn) go to 310
c move short center to r and subtract mean, mask 
	      iptcnt = 1
	      call rmove (nnn,ffta(iptcnt),r(1))
	      call zerocor(r(1),nnn,iptcnt)
	      nop = nnn
	      iparc = nnn/10
	      iparb = nnn/2
	      call mask(r)
c shift to center and zero ends
	      do i=nnn+1,nphz
		 r(i) = 0.0
	      end do
	      ks = 1 + nphz/2 - imax  
	      do i=nnn,1,-1
		j = i + ks
		r(j) = r(i)
	      end do
	      do i=1,ks
		r(i) = 0.0
	      end do
	   else
c move full center to r and subtract mean of whole interferogram
	      iptcnt = nstart + 1
	      call rmove (nphz,ffta(iptcnt),r(1))
	      call zerocor(r(1),nphz,iptcnt)
	   end if

	else                          ! twosided

	   if (ngot .gt. nphz) then   ! long enough
	      if (nstart .lt. 0 .or. imax+nphz/2 .gt. ngot) then
		write(wrtbuf,315)
		call wrtstr(wrtbuf)
		write(logfil,315)
315		format(
     &            ' Imax too close to end - NO PHASE ARRAY CREATED')
 		return
	      else
c move center to r and subtract mean of whole interferogram
		iptcnt = nstart + 1
		call rmove(nphz,ffta(iptcnt),r(1))
		call zerocor(r(1),nphz,iptcnt)
	      end if

	   else		! this is a short interferogram - mask and center

	      call rmove(nphz,ffta(1),r(1))
	      call zerocor(r(1),ngot,1)
c mask 5% 			mask, rise width iparc, half width iparb
	      nop = ngot
	      iparb = nop/2
	      iparc = nop/2
	      call mask(r)
c move to center of phz array
	      nop = nphz
	      ifx = 1
	      inum(1) = nphz/2 + 1 - imax
	      call shft(r,tr)
	   end if

	end if

c process the central fringe: apodize, transform, save phase
	if (phcorfl) then
	    call remark(' Starting phase determination')
	    nop = nphz
	    if (l_bad) then      ! save any buffered bad points
	       call markbad
	    end if
	    call findphz(r,tr,phz)
	end if

* At this point the variable 'nop' contains the number of points
* in the phase curve. Save it because 'nop' is needed to plot
* the spectrum.
	nopph = nop
	delwph = freespec/nopph

* restore the low res spectrum from temporary file to r array for display
	noppl = nphz/2
	open(unit=44,file=tmpnam,form='unformatted')
	read(44) (r(i), i=1,noppl)
	close(44,status='delete')! remove the temp. file

* set up parameters for phase plot
 	nop = noppl
	wref = aliaslo
	delwpl = freespec/nop
	delw = delwpl

c set auto apodization to 5% of right side
1000	if (apod1 .eq. -1.0) apod1 = 10.0*((ngot - imax)/200)
	write(wrtbuf,'(1x,f12.4,a)') apod1,
     &        ' point cosine bell to be used'
	call wrtstr(wrtbuf)
	return

	end

c---------------------------------------------------------------

	subroutine findphz(r,tr,phz)

	include 'altinf.h'
	include 'infmtn.h'
	include 'inparms.h'
	include 'datetc.h'
	include 'iounit.h'
	include 'integrate.h'
	include 'chebychev.h'
	include 'phase.h'

	parameter (nkill = 2048)

	real r(*), tr(*), phz(*)

	real cutfrac,epsi,slim,tol,chisqr,ampmax
	real am(512),omc(512),csq(512)
	integer nterms,mode,kill(nkill),klm,nraw,npnts,kmax
	integer nx(512)
	integer ibig(32768)
	character*256 badfile
	common/ftp/cutfrac,epsi,slim,tol,chisqr,ampmax,am,omc,csq
	common/ftp/nterms,mode,kill,klm,nraw,npnts,kmax,nx
	common/ftp/ibig,badfile

	data pi/3.141592654/,twopi/6.283185307/
 
c The points are in r; amparg will process them to tr:
c     1. apodize by multiplying with the appropriate function
c     2. transform, form amp and arg
c Apodization width (nwidth) is the base width of the apodizing function
c Now using Filler 3-5-7 with minumum sidelobes
c First iteration is standard very low resolution, to provide a 
c template for continuous phase later

c for temporary diagnostics, save several arrays.  1st 8192 in 17:
	nop = nphz
	if (nphz .gt. 8192) nop = 8192
	call oscrwrite(17)
c 							Raw r in 0:
	k = (nphz - nop)/2
	do i=1,8192
	    r(i) = r(i+k)
	end do
	id = 'raw center'
	call oscrwrite(0)
	call oscrread(17)			! replace 1st 8192

	lowidth = 64
	write (wrtbuf,'(a,i8)') ' Starting very low res Amparg',lowidth
	call wrtstr(wrtbuf)

	if (.not. phapdz) then
	   nphzsav = nphz
	   nphz = 16384
	endif
	call amparg(lowidth,ampmax,kmax,r,tr)

c This is very smooth, so it is straightforward to construct 
c a continuous phase function.  nh was set in amparg

	tr(2) = 0.0
	do i=1,nh
	   phz(i) = tr(2*i)
	end do
	do i=2,nh
	   if (abs(phz(i)) .gt. 10.0*pi) then 
	      write(wrtbuf,'(1x,i8,i8)') i,phz(i)
	      call wrtstr(wrtbuf)
	   end if
 55	   if ((phz(i)-phz(i-1)) .gt. pi) then
	      phz(i) = phz(i) - twopi
	      go to 55
	   endif
 56	   if ((phz(i)-phz(i-1)) .lt. -pi) then
	      phz(i) = phz(i) + twopi
	      go to 56
	   endif
	end do
	
c bring phase of max. point inside +-Pi; copy back to tr
	dphase = 0.0
	kpmax = (kmax + 1)/2
70	if (phz(kpmax)+dphase .gt. pi) then
	   dphase = dphase - twopi
	   go to 70
	endif
80	if (phz(kpmax)+dphase .lt. -pi) then
	   dphase = dphase + twopi
	   go to 80
	endif

	do i=1,nh
	   phz(i) = phz(i) + dphase
	   tr(2*i) = phz(i)
	end do

c save lo-lo-res phase in 1, scaled by 1/pi (points)
	nop = 8192
	km = nphz/(2*nop)
	do i=1,nop
	    r(i) = phz(km*i-km+1)/pi
	end do
	id = 'Very low resolution phase; Y is in POINTS'
	wref = (alias - 1)*freespec
	delw = freespec/8192.
	call oscrwrite(1)
	call oscrread(17)			! replace 1st 8192
	if (.not. phapdz) then
	   nphz = nphzsav 
	endif

c Now do the medium resolution version in preparation for phase fit,
c using a much wider function (nwidth)
c default is 512 (old default was FWHM 256 Gaussian)

	if (phapdz) then
	   if (nphap .ne. 0) nwidth = nphap
	   if (nphap .eq. 0) nwidth = 512
	else
	   nwidth = nphz
	endif

	write (wrtbuf,'(a,i8)') 
     &         ' Starting medium resolution Amparg',nwidth
	call wrtstr(wrtbuf)
	call amparg(nwidth,ampmax,kmax,r,tr)
	phz(1) = 0.0

c This is NOT smooth, so the continuous phase function is based on the
c previous one if we are only smoothing:

	if (phapdz) then
	   do i=2,nh
	      if ((tr(2*i)-phz(i)) .gt.  pi) tr(2*i) = tr(2*i)-twopi
	      if ((tr(2*i)-phz(i)) .lt. -pi) tr(2*i) = tr(2*i)+twopi
	      phz(i) = tr(2*i)
	   end do
	endif

c save med-res phase in 3, amplitude in 2
	nop = 8192
	do i=1,nop
	   r(i) = phz(km*i-km+1)/pi
	end do
	id = 'Medium resolution phase - Y is in POINTS'
	wref = (alias - 1)*freespec
	delw = freespec/nop
	call oscrwrite(3)
	id = 'Normalized medium resolution amplitude'
	do i=1,nop
	   r(i) = tr(2*km*(i-1) + 1)/ampmax
	end do
	call oscrwrite(2)
	call oscrread(17)			! replace 1st 8192

c if this is all that is wanted, quit
	if (phapdz) then     ! leave amplitude in r, phase in tr
	   do i=1,nop
	      tr(i) = phz(km*i-km+1)/pi
	   end do
	   return
	end if

c fit wanted.  Do first iteration
	mode = 1

	call page(2)
	call remark(' Starting phase fit')
	call fitphase(tr,phz)

	if (nterms .ne. 0) then
	   if ( phctyp .eq. 1 ) then
	      goodfit = chisqr/(npnts-nterms)
	      write (logfil,101) npnts-1
	      write (wrtbuf,101) npnts-1
	      call wrtstr(wrtbuf)
	      write (logfil,102) goodfit
	      write (wrtbuf,102) goodfit
	      call wrtstr(wrtbuf)
 101	      format(' Degrees of freedom of fit  =',i5)
 102	      format(' Chi^2 / Degrees of freedom =',f8.1)
	      write (logfil,111)
 111	      format (' Chebychev coefficients / uncertainties:')
	      do i=1,nterms
		 write (logfil,113) i,chcoef(i),std(i)
 113		 format (i2,2x,f9.5,2x,f9.5)
	      end do
	   end if
	   if (phctyp .eq. 2) then
	      write (logfil,'(a)') ' Spline phase fit completed.'
	      call wrtstr(' Spline phase fit completed.')
	   end if
	else
	   call wrtstr(' No phase fit requested.')
	end if

c leave final phase fit in phz (nphz/2 points)
	nop = nphz/2
	wref = aliaslo
	delw = freespec/nop 

	return
	end

c-------------------------------------------------------------------------

	subroutine amparg(nwidth,ampmax,km,r,tr)

c Starting with nphz raw points in r, move to tr and apodize.
c Then transform in place; form amp and phase in tr, alternating

	include 'infmtn.h'
	include 'inparms.h'
	include 'datetc.h'
	include 'iounit.h'

	real r(*), tr(*)

	common/leftout/sigma0,sigmax
	double precision sigma0,sigmax

	data twopi/6.283185308/

c   the points are in r; move to tr and start processing .
	do i=1,nphz
	    tr(i) = 0.0
	enddo
	nh = nphz/2
	nhp = nh+1
	ia = nhp - nwidth/2
	call rmove (nwidth,r(ia),tr(ia))

c   first apodize by multiplying with the appropriate function (BHB 3-5-7 min)
	tr(1) = 0.0
	con = twopi/nwidth
	do j = 2,nh
	    xp = con*(j - nhp)
	    xx = 0.355766 + 0.487395*cos(xp) + 0.144234*cos(2.0*xp)
     &		 + 0.012605*cos(3.0*xp)
	    tr(j) = xx*tr(j)
	    tr(nphz+2-j) = xx*tr(nphz+2-j)
	end do

c   do complex transform; multiply by nphz/2 to keep old normalization
c   do complex transform

	write (wrtbuf,'(a,i8,a)') ' Transforming ',nphz,' points'
	call wrtstr(wrtbuf)
	if (nphz .le. 32768) then
	   call fast(tr(1),nphz)
	else
	   call ffak(tr(1),nphz)
	endif

c if nphz <= nphzmax, save complex in upper tr for diagnostics
	if (nphz .le. nphzmax) then
	   do i = 1,nphz
	      tr(i+nphzmax) = tr(i)
	   end do
	end if

c  calculate amp and phase and leave in tr, in alternate locations
c  find ampmax as well, skipping first 4 points 
	ampmax = 0.
	iz = 0
	do j=1,nphz,2
	   if (tr(j).eq.0.0 .and. tr(j+1).eq.0.0) then
	      xtmp = 0.0
	      tr(j) = 1.0e-6
	      iz = iz + 1
	   else
	      xtmp = atan2(tr(j+1),tr(j))
	      tr(j) =  sqrt(tr(j+1)**2 + tr(j)**2)
	   endif
	   tr(j+1) =  xtmp
	   if (tr(j).gt.ampmax .and. j.gt.8) then
	      ampmax = tr(j)
	      km = j
	   endif
	end do
	if (iz .gt. 0) then
	   write (wrtbuf,'(a,i8,a)') ' Warning: zero amp for ',iz,
     &                               ' points'
	   call wrtstr(wrtbuf)
	end if
	
* Save the amplitude spectrum in a temporary file for later display
	call mktmp(tmpnam)         ! get a temp file name
	open(unit=44,file=tmpnam,form='unformatted')
	write(44) (tr(j)/ampmax, j=1,nphz,2)
	close(44)

	delw = 2.0*sigmax/nphz
	wref = sigma0
c not right for even orders yet ?????????

	return
	end

c --------------------------------------------------------------

	subroutine fitphase(tr,phz)

	include 'set.h'
	include 'datetc.h'
	include 'altinf.h'
	include 'infmtn.h'
	include 'iounit.h'
	include 'inparms.h'
	include 'phase.h'
	include 'integrate.h'
	include 'chebychev.h'

	real tr(*), phz(*)

	integer maxpts
	parameter (maxpts=512,    ! max number of phase points
     &             nkill = 2048)   

	real cutfrac,epsi,slim,tol,chisqr,ampmax
	real am(maxpts),omc(maxpts),csq(maxpts)

	integer nterms,mode,kill(nkill),klm,nraw,npnts
        integer kmax,nx(maxpts)
	integer ibig(32768), lenfn, length, ierr
	character badfile*256, chfit*256
	common/ftp/cutfrac,epsi,slim,tol,chisqr,ampmax,am,omc,csq
	common/ftp/nterms,mode,kill,klm,nraw,npnts,kmax,nx
	common/ftp/ibig,badfile
	integer indx(8+1), nn(8)  ! for phase point octants
	real x, chebev, smot
	double precision pvar(ncoef)
	equivalence (pvar(1),pvar1)

	external length, chepol, chebev

c fit a polynomial to all points in the phase array (even tr) for 
c which the amplitude (odd tr) exceeds cutfrac*rmax, using nterms.
c   sig = epsi + slim*r(i) / r(i), which -> slim for large r
c   mode = 0 means "do not force phase fit to use zero; no iteration"
c   mode < 0 means "DO force phase fit to use zero"
c   mode > 0 means "read n points from bad point file; iterate"
c	tpi = 6.283185308
	ph_std = -1.0
	npnts = 0
	if (mode .lt. 0) then
	   npnts = 1
	   xint(1) = 1.0 - (nphz/4)
	   yint(1) = 0.0
	end if
	if (mode .gt. 0) then
	   badfile = fnames(rawin)
	   lenfn = length(badfile)
	   badfile(lenfn+1:lenfn+6) = '.badpt'
	   do i=1,nkill
	      kill(i) = 0
	   end do
	   open (19,file=badfile)
	   klm = 0
	   do i=1,nkill
	      read (19,'(5x,i7)',end=120) kill(i)
	      klm = klm + 1
	   enddo
 120	   continue
	   write(wrtbuf,'(1x,i8,a)') klm,
     &           ' points read from bad point file'
	   call wrtstr(wrtbuf)
	   close(19)
	end if

c Find cutoff
	nterms = iphparms(1)    !      number of terms in polynomial
	if ( nterms .gt. ncoef ) then
	   call wrtstr( ' Error :  order too high for phase polynomial' )
	   return
	end if
	cutfrac = phparms(1)
	cutoff = cutfrac*ampmax
	epsfrac = phparms(2)	!	noise level as fraction of ampmax
	epsi = epsfrac*ampmax	!	absolute noise level
	slim = 0.001		!	slim = 1.0/snmax
	if (nphr .gt. 2) slim = phparms(3)
	tol = 1.0e-6
	if (nphr .gt. 3) tol = phparms(4)

c save indices of useable points, count them
	nraw = 0
	do i=9,nphz - 8,2
	   if (tr(i) .gt. cutoff) then

C Pick the maxiumum one in the line profile (GN, Feb 2015)
C Width of line profile is controlled by swid        
              if(ibig(nraw) .ge. i-2*swid) then
                   if(tr(i) .ge. tr(ibig(nraw))) ibig(nraw)=i
              else 
	         nraw = nraw + 1
	         ibig(nraw) = i
              endif
	      if (nraw .eq. 32768) goto 4105
	   end if
	end do
C END OF REVISION Feb 2015
 4105	write (wrtbuf,'(a,i8)') ' Phase points above threshold: ',nraw
	call wrtstr(wrtbuf)

c now choose 512 of these, by octants to assure coverage. Octant indices:
	indx(1) = ibig(1)
	indx(9) = ibig(nraw)
	indx(5) = (indx(1) + indx(9))/2
	indx(3) = (indx(1) + indx(5))/2
	indx(7) = (indx(9) + indx(5))/2
	indx(2) = (indx(1) + indx(3))/2
	indx(4) = (indx(5) + indx(3))/2
	indx(6) = (indx(5) + indx(7))/2
	indx(8) = (indx(9) + indx(7))/2

c count and print out number in each octant
	k = 1
	do io=1,8
	    ncnt = 0
	    do j=k,nraw
	    	jmax = j
	    	if (ibig(j) .ge. indx(io+1)) go to 4126
		ncnt = ncnt + 1
	    end do
4126	    nn(io) = ncnt
	    k = jmax
	end do
	write (wrtbuf,4131) indx
	call wrtstr(wrtbuf)
4131	format (' Octant indices:', 9i7)
	write (wrtbuf,4133) nn
	call wrtstr(wrtbuf)
4133	format (' Octant counts: ', 7x, 8i7)

c see if some octants are not using their allocation
	ntot = 0
	nful = 0
	do j=1,8
	   if (nn(j) .lt. 64) then
	      ntot = ntot + nn(j)
	   else
	      ntot = ntot + 64
	      nful = nful +  1
	   endif
	end do
	nextra = 0
	if (nful .gt. 0) nextra = (512 - ntot)/nful
	write (wrtbuf,'(a,i8)') ' ntot=',ntot
	call wrtstr(wrtbuf)
	write (wrtbuf,'(a,i8)') ' nful=',nful
	call wrtstr(wrtbuf)
	write (wrtbuf,'(a,i8)') ' nextra=',nextra
	call wrtstr(wrtbuf)
	
c finally, for each octant, choose the largest nn(i) points
	nelim = 0
	nlo = 1
	do 4180 io=1,8
	    nhi = nlo + nn(io) - 1
	    ntoget = nn(io)
	    if (ntoget .ge. 64) ntoget = 64 + nextra
	    if (ntoget .lt. 64) then
c not overfull octant - take all, UNLESS IN BAD POINT LIST
	       do j=nlo,nhi
		  i = ibig(j)
		  do na=1,klm                         ! check through bad points list
		     if (i .eq. kill(na)) then
			nelim = nelim + 1
			go to 4161
		     endif
		  enddo
		  npnts = npnts + 1
		  xint(npnts) = real(i)             ! prev. xx / point
		  yint(npnts) = tr(i+1)             ! prev. yy / phase
		  sigma(npnts) = slim + epsi/tr(i)  ! prev. sg
		  nx(npnts) = i
		  am(npnts) = tr(i)
 4161		  continue
	       end do

	    else

c overfull octant - take 64 + nextra
	       do 4160 j=nlo,nhi
		  ngrtr = 0
		  i = ibig(j)
		  rtst = tr(i)
		  do jj=j+1,nhi
		     k = ibig(jj)
		     if (rtst .lt. tr(k)) then
			ngrtr = ngrtr + 1
			if (ngrtr .ge. ntoget) go to 4160
		     endif
		  end do

* this one OK - save it, BUT ONLY IF NOT IN LIST OF BAD POINTS
		  do na=1,klm                         ! check through bad points list
		     if (i .eq. kill(na)) then
			nelim = nelim + 1
			go to 4160
		     endif
		  enddo
		  npnts = npnts + 1
		  xint(npnts) = real(i)               ! prev. xx / point
		  yint(npnts) = tr(i+1)               ! prev. yy / phase
		  sigma(npnts) = slim + epsi/tr(i)    ! prev. sg
		  nx(npnts) = i
		  am(npnts) = tr(i)
		  ntoget = ntoget - 1
		  if (ntoget .eq. 0) go to 4170
 4160	       continue
	    end if
 4170	    nlo = nlo + nn(io)
 4180	 continue
	 write (wrtbuf,'(a,i8,a)') ' npnts=',npnts,' used'
	 call wrtstr(wrtbuf)

 4181	 write (wrtbuf,'(a,i8)') ' Bad phase points eliminated: ',nelim
	 call wrtstr(wrtbuf)

c find Chebychev polynomial coefficients
	 if ( phctyp.eq.1 .and. nterms.ne.0 ) then

	    bma = 0.5 * real( nphz - 1 )
	    bpa = 0.5 * real( nphz + 1 )
 	    call svdlsq(xint,yint,sigma,npnts,chcoef,nterms,uu,vv,ww,
     &                  chisqr,std,tol,chepol)
	    write (wrtbuf,'(a,i8,a,g14.6)') ' nterms= ',nterms,
     &                                   '     Chi^2= ',chisqr
	    call wrtstr(wrtbuf)
	    write (wrtbuf,'(a,i8,a,2f12.2)') ' npnts= ',npnts,
     &                    ' first/last= ',xint(1),xint(npnts)
	    call wrtstr(wrtbuf)
	    call wrtstr(' Chebychev coefficients / uncertainties:')
	    do i=1,nterms
	       write (wrtbuf,'(1x,i2,2f15.6)') i,chcoef(i),std(i)
	       call wrtstr(wrtbuf)
	    end do
	    write (logfil,*) 'nterms= ',nterms,'     Chi^2= ',chisqr
	    write (logfil,*) ' npnts= ',npnts,' first/last= ',
     *			xint(1),xint(npnts)
	    write (logfil,4191) chcoef
 4191	    format (5x,4f15.6) 

	    modephzc = 2	 ! 2 = cheb. fit
	    pvar0 = nterms
	    do i=1,nterms
	       pvar(i) = chcoef(i) ! equivalenced to pvar1
	    end do

c calculate the resulting polynomial, put in phz
	    bma = 0.5 * ( real( nphz )/2.0 - 1.0 )
	    bpa = 0.5 * ( real( nphz )/2.0 + 1.0 )
	    do i=1,nphz/2
	       x = real(i)
	       phz(i) = chebev(chcoef,nterms,x)
	    end do

c leave diff and chi elements in omc and csq, calc std. deviation
	    ph_std = 0.0
	    do ia=1,npnts
	       i = nx(ia)
	       omc(ia) = yint(ia) - phz((i+1)/2) ! o-c
	       ph_std = ph_std + omc(ia)**2
	       csq(ia) = omc(ia)/sigma(ia) ! chisq
	    end do
	    ph_std = sqrt( ph_std / real(npnts-1) )

* find cubic phase spline, put in phz
	 else if (phctyp.eq.2 .and. nterms.ne.0) then

	    smot = smof * real(npnts)
        write(*,*) "npnts=",npnts,"smot=",smot,"stol=",stol,"sten=",sten
	    call curvs(npnts,xint,yint,sigma,0,smot,
     &                 stol,ys,yp,sten,wrkspc,ierr)

	    if (ierr .eq. 0) then
	       call wrtstr(' Spline fit was successful.')
	       write(wrtbuf,'(a,f8.5,a,f8.5)')
     &         ' Tolerance: ',stol,'  Tension: ',sten
	       call wrtstr(wrtbuf)
	    else
	       call wrtstr(' Error during spline fit.')
	       if (ierr .eq. 1)
     &            call wrtstr(' *** Too few points for spline.')	      
	       if (ierr .eq. 4)
     &            call wrtstr(' *** xvalues not strictly increasing.')	      
	       if (ierr .eq. 6)
     &            call wrtstr(' *** Spline fit was aborted.')	      
	       return
	    end if

	    modephzc = 3             ! 3 = spline fit

* calculate the resulting spline, put in phz
* map [1,nphz/2] --> [1,nphz]
	    do i=1,nphz/2
	       x = (2.0 * (nphz - 1) * i - (nphz + 4)) /
     &             real(nphz - 2)
	       phz(i) = curv2(x,npnts,xint,ys,yp,sten)
	    end do

* leave diff and chi elements in omc and csq, calc std. deviation
	    ph_std = 0.0
	    do ia=1,npnts
	       i = nx(ia)
	       omc(ia) = yint(ia) - phz((i+1)/2) ! o-c
	       ph_std = ph_std + omc(ia)**2
	       csq(ia) = omc(ia)/sigma(ia) ! chisq
	    end do
	    ph_std = sqrt( ph_std / real(npnts-1) )	    

* no phase fit was requested
	 else

	    do i=1,nphz/2
	       phz(i) = 0.0
	    end do
	    do ia=1,npnts
	       i = nx(ia)
	       omc(ia) = yint(ia)
	       csq(ia) = 0.0
	    end do

	 end if

	 chfit = fnames(rawin)	  ! cobble together phase data file name
	 lenfn = length(chfit)
	 chfit(lenfn+1:lenfn+1+6) = '.phase'
	 open (19,file=chfit,form='formatted',status='replace')
	 write (19,4301) 'n','i','yy','o-c','sg','chi','r','wavno'
 4301	 format (a5,1x,a8,6(1x,a10,4x))
	 ddd = freespec/nphz
	 do i=1,npnts
	    n = nx(i)
	    write (19,4302) i,n,yint(i),omc(i),sigma(i),
     &  	   csq(i),1000.0*am(i)/ampmax,((n-1)*ddd+aliaslo)
 4302	    format (i5,1x,i8,6(1x,g14.6))
	 end do
	 close(19)
	 
	 return
	 end

c----------------------------------------------------------------

	subroutine markbad

	include 'datetc.h'
	include 'iounit.h'
	include 'infmtn.h'
	include 'phase.h'

	parameter (nkill = 2048)

	logical lex
	integer bsearch, nfil
	integer nump,n,m,lenfn,length
	real w
	double precision wnum

	real cutfrac,epsi,slim,tol,chisqr,ampmax
	real am(512),omc(512),csq(512)
	integer nterms,mode,kill(nkill),klm,nraw,npnts,kmax,nx(512)
	real ykill(nkill)
	integer ibig(32768),kflag
	character*256 badfile
	common/ftp/cutfrac,epsi,slim,tol,chisqr,ampmax,am,omc,csq
	common/ftp/nterms,mode,kill,klm,nraw,npnts,kmax,nx
	common/ftp/ibig,badfile

	external bsearch, length

	include 'transform.h'

	klm = 0

	badfile = fnames(rawin)
	lenfn = length(badfile)
	badfile(lenfn+1:lenfn+6) = '.badpt'
	inquire( file=badfile, exist=lex )
	if ( .not. lex ) goto 201

* read in first 2 columns, point and observed phase
	open (19,file=badfile,status='old',err=400)	   
 100	klm = klm+1
	read (19,'(5x,i7,7x,f7.4)', end=200) kill(klm),ykill(klm)
	go to 100
 200	klm = klm - 1
	close(19)

* add phase points which are not yet in bad points file
	nfil = klm                           ! points read from file
 201	do m=1,nphpts
	   if (ipgood(m) .eq. 1) goto 300    ! phase point is good - skip
	   if (bsearch(kill,nfil,ph_pt(m)) .ne. 0) goto 300
	   do j=nfil+1, klm                  ! check unordered new points
	      if ( kill(j) .eq. ph_pt(m) ) goto 300
	   end do
	   klm = klm + 1	      
	   kill(klm)  = ph_pt(m)
	   ykill(klm) = ph_ob(m)
 300	   continue
	end do

* sort to get new points in order
	kflag = 2
	call issort(kill, ykill, klm, kflag)
	if ( kflag .eq. -99 )  then
	   call wrtstr(' Error :  failed to sort bad points')
	   return
	end if

* write out a fresh bad points file
	open(19,file=badfile,status='replace',form='formatted')
	do j=1,klm
	   m=kill(j)
	   write (19,'(5x,i7,7x,f7.4,5x,f12.3)') m,ykill(j),wnum(m)/2.0
	enddo
	close (19)

	l_bad = .false.    ! all bad points saved
	write(wrtbuf,'(a,i4,a,i4,a)') 
     &       ' Number of bad points saved =',klm,
     &       ' ( previously read from file: ',nfil,')' 
	call wrtstr(wrtbuf)
	return

 400	call wrtstr(
     &       ' Error :  could not open/create bad points file')
	return

	end

*-------------------------------------------------------------------------

	integer function bsearch( iarr, n, item )
*
* searches an ordered array 'iarr' of length 'n' for 'item'
* returns index of 'item' in 'iarr' if found, 0 otherwise
* Algorithm: binary search (repeated bisection)
*
	integer iarr(*), n, item
	integer iupp, ilow, imid

        ilow = 0
        iupp = n+1

	if ( n .eq. 0 ) then
	   bsearch = 0
	   return
	end if

 10     if ( iupp - ilow .gt. 1 ) then
           imid = ( ilow + iupp ) / 2
           if ( item .gt. iarr(imid) ) then
              ilow = imid
           else
              iupp = imid
           end if
           goto 10
        end if

        if ( iarr(imid) .eq. item ) then
           bsearch = imid
        else
           bsearch = 0
        end if

        return
	end

*-------------------------------------------------------------------------
	   
	
