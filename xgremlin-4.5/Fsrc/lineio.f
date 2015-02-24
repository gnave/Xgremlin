c \gremlin\src\lineio.for   Ver. 2.2

c  Revision 2.40 93/10/05  Add intcorr to writelines
c  Revision 2.39 93/07/24  Add setnoise
c  Revision 2.38 93/03/19  Install in Grammy
c  Revision 2.37 92/08/19  More use of verbos in getlin; fix printparams wd
c  Revision 2.36 92/06/01  Start eps output
c  Revision 2.35 92/05/31  Counter added to writelines; delw replaces xpara
c  Revision 2.34 92/04/08  Getlines checks for 'verbos'
c  Revision 2.33 91/12/08  printparams checks ESC
c  Revision 2.32 91/11/20  wavelength tacked on to the end of writelines
c  Revision 2.31 91/07/03  Readlines (syn) holds width, damping
c  Revision 2.30 91/07/01  Function beta made available (gaussian width)

	subroutine prprms(point,amp,wd,dmp,eps1,
     &                    eps4,eps5,nit,nhold,ctag,dent)

	include 'infmtn.h'
	include 'iounit.h'
	include 'set.h'
	include 'linparms.h'
	include 'datetc.h'

	integer nit(*), nhold(*)
        real amp(*),wd(*),dmp(*)
	real eps1(*),eps4(*),eps5(*)
        double precision point(*)
        character ctag(*)*4, dent(*)*32
  
	integer itrflag
	real xreal
	integer*2 ireal2(2),i1
	equivalence (xreal,ireal2)
 
	integer istrt, istop
	double precision dw, wavel, sigma
	real smul, ew, xw, x, wl

	external itrflag
 
c SYNTAX: printparams [ dw scal ] [ istrt istop ] ["any char"]
 
500	call itron
	dw = 0.0
	if (nol .eq. 0) go to 9000
	if (ifl .ge. 1) dw = xnum(1)
	smul = 1.0
	if (ifl .eq. 2) smul = xnum(2)
 
	istrt = 1
	if (ifx .ge. 2) istrt = inum(1)
	istop = nol
	if (ifx .ge. 2) istop = inum(2)
	if (istop .gt. nol) istop = nol
c					istrt > istop?
	if (istrt .gt. istop) istrt = istop
 
510	ipara = 3
	if (ifx .eq. 1 .or. ifx .gt. 2) ipara = 2
	if (ifx .eq. 1 .or. ifx .gt. 2 .or. batch) call page(2)

   	if (verbos .and. batch) then
	   write (wrtbuf,511) idcard
	   call wrtout(output,wrtbuf)
	end if
511	format ( 1x,a80 )

	if (verbos  .and.  nwos .ne. -1) then
	   write (wrtbuf,513)
	   call wrtout(output,wrtbuf)
	end if
513	format(' line  wavelength   peak ht. ',
c               i234 234567.901234 23456.8901
     *     ' width(ma) damping  itn hld tags',
c            2345.789 2345.789 2345 234 2345
     *     '    ew(ma)    wavenumber  identification')
c     	     234567.901 23456789.1234 23456789012345678901234567890123

	if (verbos  .and.  nwos .eq. -1  .and.  nstrng .eq. 0) then
	   write (wrtbuf,515)
	   call wrtout(output,wrtbuf)
	end if
515	format(' line  wavenumber   peak ht. ',
c               i234 234567.901234 23456.8901
     *     ' width(mk) damping  itn hld tags',
c            2345.789 2345.789 2345 234 2345
     *     '    ew(mk)    wavelength  identification')
c     	     234567.901 23456789.1234 23456789012345678901234567890123

	if (verbos  .and.  nwos .eq. -1  .and.  nstrng .gt. 0) then
	   write (wrtbuf,9002)
	   call wrtout(output,wrtbuf)
	end if
9002	format(' line  wavenumber   peak ht. ',
c               i234 234567.901234 23456.8901
     *     'Gauss-(mk)-Lorentz  itn hld tags',
c            2345.789 2345.789 2345 234 2345
     *     '    ew(mk)    wavelength  identification')
c     	     234567.901 23456789.1234 23456789012345678901234567890123

	call disper
	do 520 i= istrt, istop
	   itn = nit(i)
	   temp = smul * amp(i)
	   xparb = point(i)
	   call ptow
	   xw = 2000.0 * delw * wd(i)
	   xparb = xparb*(1.0 + dw)
	   wavel = xparb
	   if (nwos .ne. -1) call wtos
	   if (nwos .eq. -1) call stow
	   sigma = xparb
	   j = dmp(i)
	   x = dmp(i) - j
	   ew = p(26)
	   if (j .ne. 26) ew = p(j) + x * (p(j+1) - p(j))
	   ew = ew * xw * temp

c   eps1 holds sigf, dsig
	   xreal = eps1(i)
	   i1 = ireal2(1)
	   ireal2(1) = 0
	   eps = xreal
	   ireal2(2) = i1
	   deps = xreal
c   eps4 holds dsigma, da
	   xreal = eps4(i)
	   i1 = ireal2(1)
	   ireal2(1) = 0
	   dsig = xreal
	   ireal2(2) = i1
 	   da = xreal
c   eps5 holds dwidth, db
	   xreal = eps5(i)
	   i1 = ireal2(1)
 	   ireal2(1) = 0
	   dwd = xreal
	   ireal2(2) = i1
	   db = xreal
	   call procpend
	   if (itrflag() .eq. 1) then
	      call itroff
	      return
	   end if
c						resolve widths?
	   wl = (dmp(i) -1.0)/25.0
	   wg = xw
	   if (nstrng .ne. 0) then
		wl = xw * wl
		wg = xw * beta(dmp(i))
	   endif
 
	   write (wrtbuf,521) i, wavel, temp, wg, wl, itn, nhold(i),
     &          ctag(i),ew,sigma,dent(i)
	   call wrtout(output,wrtbuf)
 520	continue
 521	format(i4,f14.6,f11.4,f9.3,f9.3,i5,i4,1x,
     &         a4,f11.3,f14.4,1x,a)
 
	return

9000	call wrtstr(' Warning :  no lines in list')
	call itroff
	return
	end

c ----------------------------------------------------------------------

	function beta(damping)

	include 'linparms.h'

c calculate the gaussian fractional linewidth from the damping (1.-26.)
c bgg index runs from 1 to 51, not 1 to 26
	   x = 2.0*damping - 1.0
	   j = x
	   x = x - j
	   beta = sqrt(bgg(j) + x * (bgg(j+1) - bgg(j))) / 0.600561
	return
	end

c ----------------------------------------------------------------------

	subroutine rdprms(r,point,amp,wd,dmp,eps1,eps2,eps3,
     &                    eps4,eps5,nit,nhold,ctag,dent)

c  if ipara is set to -999999 the program is supposed to terminate.
	
	include 'infmtn.h'
	include 'iounit.h'
	include 'set.h'
	include 'linparms.h'
	include 'datetc.h'

	integer nit(*), nhold(*)
        real r(*),amp(*),wd(*),dmp(*)
	real eps1(*),eps2(*),eps3(*),eps4(*),eps5(*)
        double precision point(*)
        character ctag(*)*4, dent(*)*32
  
	integer ionum
	external ionum
	integer oldinp
	double precision dw, xco, wlo, whigh
	real smul, xw 


c					SYNTAX
c	readparams [ 'tag' ] 
c	readparams [ 'tag' ] dw  
c	readparams [ 'tag' ] dw  scal  
c	readparams [ 'tag' ] dw  scal  wlo 
c	readparams [ 'tag' ] "unit name"   etc.


	oldinp = input
	dw = 0.0
	if (ifl .ge. 1) dw = xnum(1)
	smul = 1.0
	if (ifl .ge. 2) smul = xnum(2)
	wlo = 0.
	if (ifl .ge. 3) wlo = xnum(3)
	whigh = 1.e+09
	if (ifl .ge. 4) whigh = xnum(4)
	if (nstrng .lt. 2) go to 10
c					decode unit name to number
	lu = ionum(alpha(2)(1:nalpha(2)),nalpha(2))
c					bad name?
	if (lu .eq. -1) go to 380
c					see if the unit is open.
	if (uopen(lu) .ne. 1) go to 385
c					make sure reads only are made
c					on formatted units.
	if (lu .ne. cardin  .and.  lu .ne. cardout  .and.
     &		lu .ne. stdin) go to 390
c					everythings ok.
c					start at beginning of file.
	if (lu .ne. stdin  .and.  oldinp .ne. cardin) rewind lu
	input = lu

10	nol = 0
        call linevis( 0 )   ! flag line markers invisible
        call llerase        ! wipe out old plotter line list
 
	xco = -10000.0
	call disper
	read (input,301,end=370) idcard
301	format (a)
310	read (input,301,end=370) kin
	if (kin(1:3) .eq. 'end') go to 350
	write (lp,301) kin
  	call frmchk(lp,10)
  	if (ipara .ne. 1) go to 350
c					check for internal table
c					overflow.
	if (nol .ge. nlins) go to 310
	nol = nol + 1
	i = nol
	read (lp,411) xparb, amp(i), xw,dmp(i), nit(i),nhold(i),
     &		ctag(i),dent(i)
411	format (f11.5,f9.4,f8.2,f7.4,i3,i3,a4,9x,a25)

c					check for range
	if (xparb .ge. wlo  .and.  xparb .le. whigh) go to 315
	nol = nol - 1
	kin = ' '
	lp = ' '
	if (xparb .gt. whigh) go to 350
	go to 310
315	if (nstrng .ne. 0) ctag(i)(1:1) = alpha(1)(1:1)
	nit(i) = -abs(nit(i))
	amp(i) = smul * amp(i)
	wd(i) = xw / (2000.0 * xpara)
	dmp(i) = 25.0*dmp(i) + 1.0
	xparb = xparb*(1.0 + dw)
	xnum(1) = xparb
	call wtop
	point(i) = xparb
	if (xparb .lt. xco) go to 320
	xco = xparb
	go to 310
c					card out of order
320	xnum(2) = amp(i)
	xnum(3) = xw
	xnum(4) = dmp(i) 
	alpha(1)(1:32) = dent(i)
	inum(1) = 0
	nol = nol - 1
c	l = 2
	call inslin(nl,r,point, amp, wd, dmp,
     &               eps1,eps2,eps3,eps4,eps5,nit,nhold,ctag,dent)
	go to 310
c					finished - print list
c					reset input unit.
350	input = oldinp
	dw = 0.0
	smul = 1.0
c	istrt = 1
c	istop = nol
c ********for now, just call prprms
	call prprms(point, amp, wd, dmp,
     &              eps1,eps4,eps5,nit, nhold, ctag, dent)
	return

370	if (batch) ipara = -999999
	input = stdin
	if ( .not. batch) go to 350
	return

380	write (wrtbuf,381) alpha(2)(1:nalpha(2))
	call wrtout(output,wrtbuf)
381	format(' Error :  unknown unit name  ',a)
	return

385	write (wrtbuf,386) alpha(2)(1:nalpha(2))
	call wrtout(output,wrtbuf)
386	format(' Error :  ',a,' is not open.')
	return

390	write (wrtbuf,391) alpha(2)(1:nalpha(2))
	call wrtout(output,wrtbuf)
391	format(' Error :  ',a,' is not an acceptable unit name.')
	return

	end

c ----------------------------------------------------------------------

	subroutine puprms(point,amp,wd,dmp,nit,nhold,ctag,dent)

	include 'iounit.h'
	include 'linparms.h'
	include 'datetc.h'

	integer nit(*), nhold(*)
	real amp(*), wd(*), dmp(*)
	double precision point(*)
	character ctag(*)*4, dent(*)*32

	double precision dw 
	real smul, ew, xw, x

c SYNTAX: 
c	writeparams
c	writeparams [ dw ]
c	writeparams  dw  [ scal ]


c check to make sure the cardout unit is open.
	if (uopen(cardout) .ne. 1) go to 420
	dw = 0.0
	if (ifl .ge. 1) dw = xnum(1)
	smul = 1.0
	if (ifl .ge. 2) smul = xnum(2)

	write (cardout,9000)
9000	format('readparams')

	write (cardout,401) idcard(1:60) , cdat,ctim
401	format(a,a,a)
	call disper
	do  i=1,nol
	   xparb = point(i)
	   call ptow
	   xw = 2000.0 * xpara * wd(i)
	   bt = (dmp(i) - 1.0)/25.0
	   xparb = xparb*(1.0 + dw)
	   temp = smul * amp(i)
	   j = dmp(i)
	   x = dmp(i) - j
	   ew = p(26)
	   if (j .eq. 26) go to 408
	   ew = p(j) + x * (p(j + 1) - p(j))
408	   ew = ew * xw * temp
	   write (cardout,411) xparb, temp, xw, bt, nit(i),nhold(i),
     *		ctag(i),ew,dent(i)
	end do
411	format (f11.5,f9.4,f8.2,f7.4,i3,i3,a4,f9.2,a25)

	write (cardout,9003)
9003	format(' end-of-params: keyboard')
	return

 420	call wrtstr(' Error :  outfile unit is not open.')
	return
	end

c ----------------------------------------------------------------------

	subroutine dispar(point,amp,wd,dmp,
     &                    eps1,eps2,eps3,nit,nhold,ctag)
 
	include 'iounit.h'
	include 'linparms.h'
	include 'datetc.h'

	integer nit(*), nhold(*)
	real amp(*), wd(*), dmp(*)
	real eps1(*), eps2(*), eps3(*)
	double precision point(*)
	character ctag(*)*4

	integer j, nstrt, nstop, i, itrflag
	real xw, ew, x
	double precision wavel

	external itrflag

c print parameter list on display
c stop at inum(2) or nol 
	call itron
	nstrt = 1
	nstop = nol
	if (ifx .eq. 1) then
	   nstrt = inum(1)
	   nstop = nstrt
	end if
	if (ifx .eq. 2) then
	   nstrt = inum(1)
	   nstop = inum(2)
	end if
	if (nstop .gt. nol) nstop = nol
	call disper
	write (wrtbuf,11)
	call wrtout(output,wrtbuf)
11	format (' No.    Sigma      Amp      Wid(mK)   Dmp Itn',
     &     ' H   T    Ew      rms      even      odd')
	do i=nstrt,nstop
	    xparb = point(i)
	    call ptow
	    xw = 2000. * xpara * wd(i)
            if (wd(i) .eq. 0.0) then
	       call wrtstr(' Warning: zero width.')
	    end if
            if (dmp(i) .lt. 1.0) dmp(i) = 1.0           !!!!!!!!!!!!!!!
            if (dmp(i) .gt. 26.0) dmp(i) = 26.0         !!!!!!!!!!!!!!!
	    bt = (dmp(i) - 1.0)/25.0
	    wavel = xparb
	    j = dmp(i)
	    x = dmp(i) - j
	    ew = p(26)
	    if (j .ne. 26) ew = p(j) + x * (p(j+1)-p(j))
	    ew = ew * xw * amp(i)
	    write (wrtbuf,120) i, wavel, amp(i), xw, bt, nit(i),  
     &        nhold(i), ctag(i), ew, eps1(i), eps2(i), eps3(i)
	    call wrtout(output,wrtbuf)
120	    format(i4, f12.4, 1x, g10.4, 1x, f7.1, f6.3, i3, i3,
     &         a4, 1x, g9.3, 3(1x,e8.3))
	    call procpend
	    if ( itrflag() .eq. 1 ) then
	       call itroff
	       call wrtstr(' Warning :  dispar interrupted.')
	       return
	    end if
	enddo
	call itroff
	return
	end

c ----------------------------------------------------------------------

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
	subroutine getlin(r,point,amp,wd,dmp,eps1,eps2,eps3,
     &                    eps4,eps5,nit,nhold,ctag,dent)
 
	include 'infmtn.h'
	include 'iounit.h'
	include 'set.h'
	include 'linparms.h'
	include 'datetc.h'

	integer nit(*), nhold(*)
        real r(*),amp(*),wd(*),dmp(*)
	real eps1(*),eps2(*),eps3(*),eps4(*),eps5(*)
        double precision point(*)
        character ctag(*)*4, dent(*)*32
  
 	double precision sigma
	double precision wstrt,wend
	integer nbstart, nerr, itrflag
	integer*2 itn, ihold
	real xint, width, dmping
	real epstot, epsevn, epsodd, epsran, spare

	double precision dpbuf(10)
	character*1 chbuf(80)
	integer ibuf(20)
	real buf(20)
	character*4 tags
	character*32 ident
	equivalence ( dpbuf(1), chbuf(1), ibuf(1),  buf(1) )
	equivalence (sigma, chbuf( 1)), (xint,  chbuf( 9))
	equivalence (width, chbuf(13)), (dmping,chbuf(17))
	equivalence (itn,   chbuf(21)), (ihold, chbuf(23))
	equivalence (tags,  chbuf(25)), (epstot,chbuf(29))
	equivalence (epsevn,chbuf(33)), (epsodd,chbuf(37))
	equivalence (epsran,chbuf(41)), (spare, chbuf(45))
	equivalence (ident, chbuf(49))

	external itrflag

* test if unit is open
	if ( uopen(lineio) .eq. 0 ) then
	   call wrtstr(' Error :  unit lineio not open - aborted.')
	end if

c test for active getlines:
	nactv = 0
	npltl = 0
	if (nstrng .ne. 0) then
	   do k=1,nstrng
	      if (alpha(k)(1:nalpha(k)) .eq. 'active' ) nactv = 1
	      if (alpha(k)(1:nalpha(k)) .eq. 'noplot' ) npltl = 1
	   end do
	endif
c fetch linum, ntop
	call readline(lineio, 0, ibuf, nerr)
	linum = ibuf(1)
	ntop  = ibuf(2)

c  Fetch the lines between sigo and sigf from the .lin file, nlins max
	wstrt = wref
	wend = wref + (nop - 1)*delw
        if (ifl .eq. 2) then
	   wstrt = xnum(1)
	   wend = xnum(2)
        end if

c search for bottom of region
c determine maximum no. of forks needed
 	me = 0
	nt = linum/2
	ndel = 1
10	if (nt .eq. 0) go to 20
	me = me + 1
	nt = nt/2
	ndel = 2*ndel
	go to 10

20	nt = linum/2
	ndel = (nt + 1)/2
	do i=1,me-1
            nbstart = nbot + linrec_size*nt
	    call readline(lineio,nbstart,ibuf,nerr)
	    if (nerr .ne. 0) go to 1290
	    nmul = -1
	    if (sigma .lt. wstrt) nmul = 1
	    nt = nt + ndel*nmul
	    ndel = (ndel+1)/2
	    if (nt .lt. 0) nt = 0
	end do

c search for exact bottom of region
	nol = 0
	nlgot = 0
        call linevis( 0 )   ! flag line markers invisible
        call llerase        ! wipe out old plotter line list
	nbfirst = ntop
        nbstart = nbot + linrec_size*(nt - ndel)
	if (nt .lt. ndel) nbstart = nbot
1010	call readline(lineio,nbstart,ibuf,nerr)
	if (nerr .ne. 0) go to 1290
	nbstart = nbstart + linrec_size
	if (nbstart .ge. ntop) go to 1291
	if (sigma .lt. wstrt) go to 1010

c accept lines until top or nlins
	nbstart = nbstart - linrec_size
	nbfirst = nbstart                  ! byte position of first line
	go to 1030
1020	call readline(lineio,nbstart,ibuf,nerr)
	if (nerr .ne. 0) go to 1290

1030	if (sigma .gt. wend) go to 1200

	nol = nol + 1
	xparb = sigma
	call wtop
	point(nol) = xparb
	amp(nol)   = xint
	wd(nol)    = 0.0005*width/delw
	dmp(nol)   = dmping
	nit(nol)   = itn
	nhold(nol) = ihold
	ctag(nol)  = tags
	eps1(nol)  = epstot
	eps2(nol)  = epsevn
	eps3(nol)  = epsodd
	eps4(nol)  = epsran
	eps5(nol)  = spare
	dent(nol)  = ident

	nbstart = nbstart + linrec_size
	if (nbstart .eq. ntop) go to 1200
	if (nol .lt. nlins) go to 1020
c hit line limit
	wend = sigma
	call wrtstr(' Line table limit reached - exiting getlines')
c normal exit
1200	if (verbos) then
	   write (wrtbuf,'(i4,a)') nol,'  lines fetched from .lin file.'
	   call wrtstr(wrtbuf)
	end if
	nlgot = nol
	xeoa = neoa
	lstlin = 1                 ! reset the last line register
	if (npltl .eq. 0) then     ! plot line markers by default
           if (nol .gt. 0) then
	      call synclin(point,amp,wd,dmp,dent) 
	      call linevis(1)
	      call plotr(1,r,r,r,r)! must be normal plot mode, no phase
	   end if
	end if
	if (nactv .eq. 1) then
	   if (verbos) then
	      call wrtstr(' Activating - this may take a few moments')
	   end if
	   call itron    ! activate stop button
	   do nl=1,nol
	      if (nit(nl) .gt. 0) then
		 call addlin(r,point(nl),wd(nl),amp(nl),dmp(nl),-xeoa)
		 call lineact(nl,1)
	      end if
	      call procpend
	      if ( itrflag().eq.1 ) goto 1201
	   end do
 1201	   call itroff
	else
	   do nl=1,nol
	      if (nit(nl) .gt. 0) nit(nl) = - nit(nl)
	   end do
	end if
	return

c errors
 1290	if (verbos) write (wrtbuf,'(1x,a,i4)') 
     &		' Error in line file access at position',nbstart
	call wrtstr(wrtbuf)  
	return
1291	if (verbos) then
           call wrtstr(' abort - top of file reached before wstrt')
	end if

	return
	end

c ----------------------------------------------------------------------

	subroutine putlin(point,amp,wd,dmp,eps1,eps2,eps3,
     &                    eps4,eps5,nit,nhold,ctag,dent)

	include 'infmtn.h'
	include 'iounit.h'
	include 'linparms.h'
	include 'datetc.h'

	integer nit(*), nhold(*)
        real amp(*),wd(*),dmp(*)
	real eps1(*),eps2(*),eps3(*),eps4(*),eps5(*)
        double precision point(*)
        character ctag(*)*4, dent(*)*32

	double precision sigma
	integer nbstart, ngap, nreq

	integer*2 itn, ihold     ! these are in an equivalence statement. SIGH ...
	real xint, width, dmping
	real epstot, epsevn, epsodd, epsran, spare

	double precision dpbuf(10)
	character*1 chbuf(80) 
	character llname*256
	integer ibuf(20)
	integer ifnl
	real buf(20)
	character*4 tags
	character*32 ident
	equivalence ( dpbuf(1), chbuf(1), ibuf(1), buf(1) )
	equivalence (sigma, chbuf( 1)), (xint,  chbuf( 9)) 
	equivalence (width, chbuf(13)), (dmping,chbuf(17))
	equivalence (itn,   chbuf(21)), (ihold, chbuf(23))
	equivalence (tags,  chbuf(25)), (epstot,chbuf(29))
	equivalence (epsevn,chbuf(33)), (epsodd,chbuf(37))
	equivalence (epsran,chbuf(41)), (spare, chbuf(45))
	equivalence (ident, chbuf(49))

* Algorithm:
* A number of line data records has been read into the internal line list
* and lines have been added or removed. The modified internal list is written back
* after the gap in the line list (from where data were copied to the internal
* buffer) has been enlarged or shrunk to fit the new size of the internal list. 
*
* nol     : number of lines in internal line list
* nlgot   : number of lines fetched from line list file (with last 'getlines'
* nlfirst : first line from .lin file that is in the buffer
* nbfirst : byte position in line file of first line in internal buffer
* ngap    : size of the gap in the file in bytes
* nreq    : required number of bytes in the file

* check if there is anything to do
	if (nol .eq. 0 .and. nlgot .eq. 0) then
	   call wrtstr(' Internal line buffer is empty')
	   return
	endif

* make a backup if none exists
	ifnl = length( fnames(lineio) )      ! line list file name
	llname = fnames(lineio)(1:ifnl)
	if ( linbak ) then                   ! make a backup of the line list file
	   call dotbak( llname, nerr )
	   if ( nerr .ne. 0 ) then
	      call wrtstr(' Error : backup of .lin file failed.')
	      return
	   end if
	end if

* retrieve some line list data from the line list file
	call readline(lineio, 0, ibuf, nerr)
	linum = ibuf(1)                          ! number of lines in list
	ntop  = ibuf(2)                          ! amount of data

	ngap = nlgot * linrec_size               ! amount of space in file
	nreq = nol   * linrec_size               ! required space in file

* the internal buffer fits exactly into the gap in the line file
	if (ngap .eq. nreq) goto 3000            ! line number not changed

* check if the new lines are to be added at the end of the line file
	if (nbfirst .eq. ntop) goto 3000         ! add lines at top of file

* this is the general case: the line list buffer has to be inserted into
* the line list file, the gap in the file needs to be enlarged or shrunk
	call adjgap( ntop, nbfirst, ngap, nreq, nerr)
	if ( nerr .ne. 0 ) then
	   call wrtstr(' Error :  line list file read / write failed')
	   call wrtstr(' LINE LIST FILE IS PROBABLY CORRUPTED')
	   return
	end if

* write line records from internal buffer (back) to file
3000	nbstart = nbfirst
     	do i=1,nol
	   xparb = point(i) 
	   call ptow
	   sigma = xparb 
	   xint = amp(i) 
	   width = 2000.0*delw*wd(i) 
	   dmping = dmp(i) 
	   itn = nit(i) 
	   ihold = nhold(i) 
	   tags = ctag(i) 
	   epstot = eps1(i) 
	   epsevn = eps2(i) 
	   epsodd = eps3(i) 
	   epsran = eps4(i) 
	   spare = eps5(i) 
	   ident = dent(i) 
	   call wrtline(lineio,nbstart,ibuf,nerr)
	   if (nerr .ne. 0) go to 4290
	   nbstart = nbstart + linrec_size
	end do
    	write (wrtbuf,'(1x,i6,a)') nol,' lines returned to .lin file'
	call wrtstr(wrtbuf)

* update line list file header 
	if (nol .ne. nlgot) then
	   linum = linum + nol - nlgot
	   ntop  = 320 + linum * linrec_size
	   ibuf(1) = linum
	   ibuf(2) = ntop
	   call wrtline(lineio, 0, ibuf, nerr)
	end if

	nlgot = nol    ! file and buffer are now in sync

	return

4290	call wrtstr(' abort - error in line file access')
	return
	end

c ----------------------------------------------------------------------

	subroutine wrtlin(r)

	include 'infmtn.h'
	include 'altinf.h'
	include 'iounit.h'
	include 'linparms.h'
	include 'datetc.h'

	real r(*)

	double precision sigma, wavel
	double precision wstrt,wend
	double precision rilaser,rindex
	integer nbstart, nerr, nl
	integer acflg, wcflg
	integer*2 itn, ihold
	real xint, width, dmping
	real epstot, epsevn, epsodd, epsran, spare

	character filnam*256
	double precision dpbuf(10)
	character*1 chbuf(80)
	integer itrflag
	integer ibuf(20)
	real buf(20)
	character*4 tags
	character*32 ident
	equivalence ( dpbuf(1), chbuf(1), ibuf(1), buf(1) )
	equivalence (sigma, chbuf( 1)), (xint,  chbuf( 9))
	equivalence (width, chbuf(13)), (dmping,chbuf(17))
	equivalence (itn,   chbuf(21)), (ihold, chbuf(23))
	equivalence (tags,  chbuf(25)), (epstot,chbuf(29))
	equivalence (epsevn,chbuf(33)), (epsodd,chbuf(37))
	equivalence (epsran,chbuf(41)), (spare, chbuf(45))
	equivalence (ident, chbuf(49))

	external itrflag

	filnam = ' '
	filnam = alpha(1)(1:nalpha(1))
      	open (19,file=filnam,access='sequential',
     *		     form='formatted',status='unknown',err=1998)

c If intensity correction is requested, the intensity correction 
c function must be in r at start;	internally saved in 19.
	call itron

	icflg = 0  
	acflg = 0
	wcflg = 0

* check for correction flags
	do i=2,nstrng
	   if (alpha(i)(1:3) .eq. 'int') then
	      icflg = 1
	      wo = wref - delw
	   end if
	   if (alpha(i)(1:3) .eq. 'air') then
	      acflg = 1
	   end if
	   if (alpha(i)(1:3) .eq. 'wav') then
 	      wcflg = 1
	   end if
	end do

c fetch linum
	call readline(lineio, 0, ibuf, nerr)
	linum = ibuf(1)

c  Write the lines between sigo and sigf from the .lin file
	wstrt = 0.0d0
	wend = 1.0d20
	if (ifl .ge. 2) then
	   wstrt = xnum(1)
	   wend = xnum(2)
	end if
	smin = -1.0e20
	smax =  1.0e20
	if (ifl .eq. 1) smin = xnum(1)
	if (ifl .eq. 3) smin = xnum(3)
	if (ifl .eq. 4) smax = xnum(4)

* indicate corrections applied to lines
	if (wcflg .eq. 1) then
	   call wrtstr(' WAVENUMBER CORRECTION APPLIED.')
	   write(19,*) ' WAVENUMBER CORRECTION APPLIED: wavcorr = ',
     &                 wavcorr
	else
	   call wrtstr(' NO wavenumber correction applied.')
	   write(19,*) ' NO wavenumber correction applied. (wavcorr = ',
     &                 wavcorr,' not used.)'
	endif

	if (acflg .eq. 1) then
	   rilaser = rindex(pspect,tspect,refwavno,hspect)
	   call wrtstr(' AIR CORRECTION APPLIED TO WAVELENGTHS.')
	   write(19,*) ' AIR CORRECTION APPLIED TO WAVELENGTHS: ',
     &                 'n_air(',refwavno,'/cm) = ',rilaser
	else
	   call wrtstr(' NO air correction applied to wavelengths.')
	   write(19,*) ' NO air correction applied to wavelengths.'
	endif

	if ( icflg .eq. 1 ) then
	   call wrtstr(' INTENSITY CALIBRATION APPLIED.')
	   write(19,*) ' INTENSITY CALIBRATION APPLIED.'
	else
	   call wrtstr(' NO intensity calibration.')
	   write(19,*) ' NO intensity calibration.'
	end if
	
	write (19,1089)
1089	format('  line    wavenumber      peak    width      dmp',
     &         '   eq width   itn   H tags     epstot     epsevn',
     &         '     epsodd     epsran  identification')  

	nl = 0
        nbstart = nbot
	do 1100 i=1,linum
	    call readline(lineio,nbstart,ibuf,nerr)
	    if (nerr .ne. 0) go to 1290
	    nbstart = nbstart + linrec_size
	    call procpend
	    if (itrflag() .eq. 1) go to 1200
	    if (xint .lt. smin .or. xint .gt. smax) go to 1100
	    if (sigma .lt. wstrt) go to 1100
	    if (sigma .gt. wend) go to 1200
	    nl = nl + 1

	    j = dmping
	    x = dmping - j
	    ew = p(26)
	    if (j .ne. 26) ew = p(j) + x * (p(j+1)-p(j))
	    ew = ew * width * xint

	    dmping = (dmping - 1.0)/25.0
	    if (acflg .eq. 1) then
	       sigma=sigma*rilaser/rindex(pspect,tspect,sigma,hspect)
	    end if
	    if (wcflg .eq. 1) then
	       sigma = sigma*(1.0d0 + wavcorr)
	    end if
	    xparb = sigma
	    if (sigma .ne. 0) call stow
	    wavel = xparb

c  For intensity correction, do linear interpolation in efficiency function
	    if (icflg .eq. 1) then
		xparb = (sigma - wo) / delw
		j = xparb
		x = xparb - j
		if (r(j).eq.0.0 .or. r(j+1).eq.0.0) then
		   goto 1100    ! skip this line
		else
		   eff = r(j) + x * (r(j+1) - r(j))
		   ew = ew/eff
		end if
	    endif

	    write (19,1099)
     &	       nl, sigma, xint, width, dmping, ew, itn, ihold, tags,
     &	       epstot, epsevn, epsodd, epsran, ident, wavel
1099	format (1x,i5,1x,f13.6,1x,e9.4,1x,f8.2,1x,f8.4,1x,e10.4,1x,i5, 
     &         1x,i3,1x,a4,4(1x,e10.5),1x,a30,f11.6)
        if (mod(linum,50) .eq. 0) then
	   write (wrtbuf,'(2h +,i6)') linum
	   call prevline(wrtbuf)
	end if
1100	continue

c normal exit
1200	write (wrtbuf,'(i4,24h lines written from .lin)') nl
	call wrtstr(wrtbuf)
	close (19)
	call itroff
	return

c errors
1290	call wrtstr(' abort - error in line file access')
	close (19)
	call itroff
	return

1998	call wrtstr(' Error in writelines - file open failed')
	call itroff
	return

c ----------------------------------------------------------------

	entry rdlines
c       *************  readlines inputfilename  <old> or <syn> smul

c read either a writelines file or a synthetic file into a .lin file
	if (nstrng .eq. 0) then
	   call wrtstr('syntax: readlines infile (<old> or <syn>) smul')
	   return
	endif 
	if(alpha(2)(1:3) .ne. 'syn'.and.alpha(2)(1:3) .ne. 'old') then
	   call wrtstr(' illegal file type - abort')
	   return
	endif

c create the .lin file, back up old one if one exists
	ik = index(fnames(datain),' ') - 1   ! cobble together file name
	filnam = ' '
	filnam = fnames(datain)(1:ik)
	filnam(ik+1:ik+4) = '.lin'   
	if (uopen(lineio) .eq. 1) then
	   call wrtstr(' saving existing .lin file to .lin.save')
	   close(lineio) 
	   call mkbackup( filnam, '.save', ierr )
	   if ( ierr .ne. 0 ) then
	      call wrtstr( ' Error :  backup of .lin file failed.')
	      return
	   end if
	end if
	open (lineio,file=filnam,access='direct',recl=80,
     &        status='replace',iostat=ierr)
	if (ierr .ne. 0) then
	   call wrtstr(' .lin file create failed - aborted')
	   uopen(lineio) = 0
	   return
	else
	   call wrtstr(' new line file created')
	   uopen(lineio) = 1
	   linbak = .false.        ! no backup is needed for fresh file
	endif

c open the input line file

	filnam = ' '
	filnam = alpha(1)(1:nalpha(1))
	open (19,file=filnam,access='sequential',
     &		 form='formatted',status='old',err=2900,iostat=i)

c initialize unfilled variables; hold width, damping if "syn"
	itn = 0
	ihold = 0
	if (alpha(2)(1:3) .eq. 'syn') ihold = 3
	tags = '    '
	epstot = 0.0
	epsevn = 0.0
	epsodd = 0.0
	epsran = 0.0
	spare = 0.0
	ident = ' '
c skip first nbot (320) bytes; used for storing linum, ntop, etc.
	nbstart = nbot

	do i=1,500000
	   if (alpha(2)(1:3) .eq. 'syn') then
 2000	      read (19,2101,err=2000,end=2200) ident,sigma,xint,
     &			width,dmping
 2101	      format (a15, 2x,f12.5,f10.4,f9.2,f8.4 )
	      if (sigma .lt. 500.0d0) go to 2000
	      if (xint .eq. 0.0) go to 2000
	      tags = ident(3:5)//'S'
	   endif
	   if (alpha(2)(1:3) .eq. 'old') then
 2103	      read (19,2105,err=2103,end=2200) nl, sigma, xint, width, 
     &			dmping, itn, ihold, tags,
     &			epstot, epsevn, epsodd, epsran, ident
 2105	      format (1x, i5, f13.6, f10.4, f9.2, f8.4, 10x, 
     &                    i5, i3, 1x, a4, 4f8.4, 1x, a30)
	      if (nl .le. 0) go to 2103
	   endif
	   dmping = 25.0*dmping + 1.0
	   sigma = sigma*(1.0d0 - wavcorr)    ! possible BUG. ( 1 + wavcorr) ?
	   if (mod(i,100) .eq. 0) then
	      write (wrtbuf,'(2h +,i4)') i
	      call prevline(wrtbuf)
	   end if

c write a line to .lin (all variables equiv. to chbuf)
	   call wrtline(lineio,nbstart,ibuf,nerr)
	   if (nerr .ne. 0) go to 2997
	   nbstart = nbstart + linrec_size
	end do

 2200	linum = i-1
	write(wrtbuf,'(i4,13h lines found.)') linum
	call wrtstr(wrtbuf)

c save linum, ntop (= address of NEXT write, not last byte)
	ibuf(1) = linum
	ibuf(2) = nbstart
	buf(3) = 0.0
	buf(4) = 0.0
	buf(5) = wavcorr
	call wrtline(lineio,0,ibuf,nerr)

	uopen(lineio) = 0
	close(lineio)
	return

2900	call wrtstr(' Error :  cannot open line file')
	return
2997	call wrtstr(' Error :  line file write failed')
	return

	end
c------------------------------------------------------------------------

        subroutine setnoise(r,point,amp,wd,dmp,eps1,eps2,eps3,
     &                      eps4,eps5,nit,nhold,ctag,dent)

	include 'linparms.h'
	include 'datetc.h'

	integer nit(*), nhold(*)
        real r(*),amp(*),wd(*),dmp(*)
	real eps1(*),eps2(*),eps3(*),eps4(*),eps5(*)
        double precision point(*)
        character ctag(*)*4, dent(*)*32

c syntax:  setnoise  wstart eps1 wstop eps2

	epsii = xnum(2)
	epsif = xnum(4)
	wref = xnum(1)
	xnum(2) = xnum(3)
	delw = 1.0	
	ifl = 2
	call disper
	nstrng = 1
	nalpha(1) = 8
	alpha(1)(1:8) = 'inactive'
	call getlin(r,point,amp,wd,dmp,eps1,eps2,eps3,
     &              eps4,eps5,nit,nhold,ctag,dent)
	if ( point(nol) .eq. point(1) ) then
	   call wrtstr( ' Error :  no lines from .lin file present.' )
	   return
	end if
	slp = (epsif - epsii)/(point(nol) - point(1))
	do i=1,nol
	   eps4(i) = epsii + slp*(point(i) - point(1))
	enddo
	call putlin(point,amp,wd,dmp,eps1,eps2,eps3,
     &                    eps4,eps5,nit,nhold,ctag,dent)
	return

	end

* ----------------------------------------------------------------------

	subroutine dotbak( name, ierr )
*
* makes a backup of the line list file
*
	
	include 'iounit.h'

	character name*(*)
	integer ierr

	close(lineio)
	call mkbackup( name, '.bak', ierr )
	if ( ierr .ne. 0 ) return
	call wrtstr('  ')
	call wrtstr( 
     &   ' Created backup .lin.bak of existing line list file.' )
	open(lineio,file=name,access='direct',recl=80,
     &       form='unformatted',iostat=ierr)
	linbak = .false.

	return
	end

*----------------------------------------------------------------------

	subroutine adjgap(lfsize,nfirst,ngap,nreq,ierr)
*
* adjust the data gap in the line list file before data from internal buffer
* are written to the file. The ffta array is used for buffering.
*
* lfsize  : current size of the line list file (before adjustment)
* nfirst  : byte position of first line record in buffer (start of gap)
* ngap    : current size of gap
* nreq    : requested gap size
*
* NOTE: in Xgremlin the line list file is opened with a record length of 80 bytes
*
	include 'datetc.h'            ! for buffer
	include 'iounit.h'
	include 'linparms.h'

	integer lb_size
	parameter( lb_size = 20480 )    ! buffer for 256 line records
	integer lbuffer(lb_size)
	integer lfsize, ngap, nreq
	integer nbmove                  ! number of bytes to move
	integer nbshft                  ! by that much
	integer nchunk                  ! number of copy actions with full buffer needed
	integer nbrest                  ! bytes in last copy action
	integer nbuffs                  ! buffer size
	integer j, k, ipos, ibuf, idx, ierr

* calculate parameters of adjustment
	nbmove = lfsize - nfirst - ngap ! data 'right' of the gap
	nbshft = nreq - ngap            ! shift data right of the gap by 'nbshft' bytes
	nbuffs = linrec_size * ( lb_size / linrec_size ) 
	nchunk = nbmove / nbuffs        ! copy that many full buffers
	nbrest = mod( nbmove, nbuffs )  ! size of last copy action

	ierr = 0

* enlarge the gap --> start copying at the top
	if ( nbshft .gt. 0 ) then
	   idx = lfsize - nbuffs        ! read / write pointer
	   do k=1,nchunk
	      ibuf = 1                  ! index into ffta array
	      do j=1, nbuffs, linrec_size
		 ipos = idx + j - 1
		 call readline(lineio, ipos, lbuffer(ibuf), ierr)
		 if ( ierr .ne. 0 ) return
		 ibuf = ibuf + linrec_size / 4   ! assume 4 byte integers
	      end do
	      ibuf = 1
	      do j=1, nbuffs, linrec_size
		 ipos = idx + nbshft + j - 1
		 call wrtline(lineio, ipos, lbuffer(ibuf), ierr)
		 if ( ierr .ne. 0 ) return
		 ibuf = ibuf + linrec_size / 4
	      end do
	      idx = idx - nbuffs
	   end do
	   idx = nfirst + ngap
	   ibuf = 1
	   do j=1, nbrest, linrec_size
	      ipos = idx + j - 1
	      call readline(lineio, ipos, lbuffer(ibuf), ierr)
	      if ( ierr .ne. 0 ) return
	      ibuf = ibuf + linrec_size / 4
	   end do
	   ibuf = 1
	   do j=1, nbrest, linrec_size
	      ipos = idx + nbshft + j - 1
	      call wrtline(lineio, ipos, lbuffer(ibuf), ierr)
	      if ( ierr .ne. 0 ) return
	      ibuf = ibuf + linrec_size / 4
	   end do
	end if

* shrink the gap --> start copying at the bottom
	if ( nbshft .lt. 0 ) then
	   idx = nfirst + ngap                   ! read / write pointer
	   do k=1,nchunk
	      ibuf = 1                           ! index into ffta array
	      do j=1, nbuffs, linrec_size
		 ipos = idx + j - 1
		 call readline(lineio, ipos, lbuffer(ibuf), ierr)
		 if ( ierr .ne. 0 ) return
		 ibuf = ibuf + linrec_size / 4   ! assume 4 byte integers
	      end do
	      ibuf = 1
	      do j=1, nbuffs, linrec_size
		 ipos = idx + nbshft + j - 1
		 call wrtline(lineio, ipos, lbuffer(ibuf), ierr)
		 if ( ierr .ne. 0 ) return
		 ibuf = ibuf + linrec_size / 4
	      end do
	      idx = idx + nbuffs
	   end do
	   idx = lfsize - nbrest
	   ibuf = 1
	   do j=1, nbrest, linrec_size
	      ipos = idx + j - 1
	      call readline(lineio, ipos, lbuffer(ibuf), ierr)
	      if ( ierr .ne. 0 ) return
	      ibuf = ibuf + linrec_size / 4
	   end do
	   ibuf = 1
	   do j=1, nbrest, linrec_size
	      ipos = idx + nbshft + j - 1
	      call wrtline(lineio, ipos, lbuffer(ibuf), ierr)
	      if ( ierr .ne. 0 ) return
	      ibuf = ibuf + linrec_size / 4
	   end do
	end if

	return
	end

*----------------------------------------------------------------------

	subroutine linhdr( linum, nerr, smin, scale, wcorr )
*
* creates the header of a line list (.lin) file
*
* linum: number of lines to be saved in file
* smin : minimum wavenumber 
* scale: 
* wcorr:
*
	include 'iounit.h'

	integer linum, nerr
	real smin, scale, wcorr
	integer ibuf(20)
	real abuf(20)
	equivalence( ibuf(1), abuf(1) )

	nerr = 0

        ibuf(1) = linum
        ibuf(2) = 320 + 80 * linum
        abuf(3) = smin
        abuf(4) = scale
        abuf(5) = wcorr
        do i=6,20
           ibuf(i) = 0
        end do
        call wrtline(lineio,0,ibuf,nerr)
        if (nerr .ne. 0) return
	
* skip first nbot (=320) bytes; used for storing linum, ntop, etc.
        do i=1,5
           ibuf(i) = 0
        end do
        do i=1,3  ! this is probably not necessary but just in case ...
           call wrtline(lineio,i*80,ibuf,nerr)
	   if ( nerr .ne. 0 ) return
        end do

	return
	end
