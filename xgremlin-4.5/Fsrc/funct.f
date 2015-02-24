c \grammy\src\funct.for  Ver. 1.0

c  Version  1.07  05.08.93   Allow moves within ffta; moments: peak in ipara
c  Version  1.06  24.07.93   Add mean range to area
c  Version  1.05  30.06.93   Add decimate
c  Version  1.04  19.03.93   Updated to Decomp versions
c  Version  1.02  16.05.91   Add movearray
c  Version  1.01  06.05.91   noise subroutines to libfuncs
c  Version  1.00  10.04.91   Combining array, funct, noise

c ---------------------------------------------------------------------
c	compute mean, sum, RMS and C.G.  of the r array

	subroutine area(r)

	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'
	include 'set.h'

	real r(*)

	double precision rsum, rsq, rcg
	real rmean, rms, ar
	integer  i, nstrt, nstop, nmid

	nstrt = 1
	nstop = nop
	if (ifx .eq. 2) then
	  nstrt = inum(1)
	  nstop = inum(2)
	end if
	nmid = (nstrt + nstop)/2

	rsum = 0.
    	do i = nstrt, nstop
	     rsum = rsum + r(i)
	end do
	rmean  = rsum / float(nstop - nstrt + 1)
 	xreg(9) = rmean
	ar = rsum*delw

	rsq = 0.
	rcg = 0.
    	do i = nstrt, nstop
	    rcg = rcg + r(i)*(i - nmid)
	    rsq = rsq + (r(i) - rmean)**2
	end do
	rms  = sqrt( rsq / float(nstop - nstrt + 1) )
	if (rsum .ne. 0.0) rcg = nmid + rcg/rsum
	call disper
	xparb = nmid
	call ptow
	xmid = xparb
	xparb = rcg
	call ptow
	write(wrtbuf,'(21h Sum of r(i)       = ,g12.5)') rsum 
	call wrtout(output,wrtbuf)
	write(wrtbuf,'(21h Area              = ,g12.5,
     &        12h wavenumbers)') ar
	call wrtout(output,wrtbuf)
	write(wrtbuf,'(21h Mean of r         = ,g12.5)' ) rmean
	call wrtout(output,wrtbuf)
	write(wrtbuf,'(21h RMS dev from mean = ,g12.5)') rms
	call wrtout(output,wrtbuf)
	write(wrtbuf,'(21h Centre of gravity = ,g12.5,
     &        7h points)') rcg
	call wrtout(output,wrtbuf)
	write(wrtbuf,'(21h                     ,g12.5,
     &        12h wavenumbers)') xparb
	call wrtout(output,wrtbuf)
	write(wrtbuf,'(21h Mean of range     = ,g12.5)') xmid
	call wrtout(output,wrtbuf)

	return
	end

c ---------------------------------------------------------------------

	subroutine decimate( r )
*
* decimate the array in r by inum(1)
*
	include 'infmtn.h'
	include 'datetc.h'

	real r(*)

	idec = inum(1)
	if (idec .lt. 2) return

	ia = 0
	do i = 1,nop,idec
	    ia = ia + 1
	    r(ia) = r(i)
	end do
	nop = ia
	delw = delw*idec

	return

	end

c ---------------------------------------------------------------------

	subroutine movearray( r, tr, ffta, phz )

* move byte strings from one array to another

	include 'datetc.h'
	include 'infmtn.h'

	real r(*), tr(*), ffta(*), phz(*)

	if (ifx .eq. 2 ) then
	   inum(3) = nop
	   ifx = 3
	end if
	if (ifx .ne. 3 .or. nstrng .ne. 2 .or.
     &	.not. ( (alpha(1)(1:1) .eq. 'r'    .or. 
     &	         alpha(1)(1:2) .eq. 'tr'   .or. 
     &	         alpha(1)(1:3) .eq. 'phz'  .or. 
     &	         alpha(1)(1:4) .eq. 'ffta') .and.
     &	        (alpha(2)(1:1) .eq. 'r'    .or. 
     &	         alpha(2)(1:2) .eq. 'tr'   .or. 
     &	         alpha(2)(1:3) .eq. 'phz'  .or. 
     &	         alpha(2)(1:4) .eq. 'ffta') ) )   then
	    call wrtstr(' Syntax error for move - should be:')
	    call wrtstr('   move <src> srcidx <dest> destidx npts')
	    call wrtstr('   where <src> and <dest> are:')
	    call wrtstr('   r, tr, ffta or phz')
	    return
	endif
	is  = inum(1)     ! source start index
	it  = inum(2)     ! target (destination) start index
	npm = inum(3)     ! number of points to move

	  if (alpha(1)(1:1) .eq. 'r' .and. alpha(2)(1:2) .eq. 'tr') 
     &	     call arrmov(npm,r(is),tr(it),is,it,r_size,tr_size)
	  if (alpha(1)(1:1) .eq. 'r' .and. alpha(2)(1:3) .eq. 'phz') 
     &	     call arrmov(npm,r(is),phz(it),is,it,r_size,phz_size)
	  if (alpha(1)(1:1) .eq. 'r' .and. alpha(2)(1:4) .eq. 'ffta') 
     &	     call arrmov(npm,r(is),ffta(it),is,it,r_size,ffta_size)
	  if (alpha(1)(1:2) .eq. 'tr' .and. alpha(2)(1:1) .eq. 'r') 
     &	     call arrmov(npm,tr(is),r(it),is,it,tr_size,r_size)
	  if (alpha(1)(1:2) .eq. 'tr' .and. alpha(2)(1:3) .eq. 'phz') 
     &	     call arrmov(npm,tr(is),phz(it),is,it,tr_size,phz_size)
	  if (alpha(1)(1:2) .eq. 'tr' .and. alpha(2)(1:4) .eq. 'ffta') 
     &	     call arrmov(npm,tr(is),ffta(it),is,it,tr_size,ffta_size)
	  if (alpha(1)(1:3) .eq. 'phz' .and. alpha(2)(1:1) .eq. 'r') 
     &       call arrmov(npm,phz(is),r(it),is,it,phz_size,r_size)
	  if (alpha(1)(1:3) .eq. 'phz' .and. alpha(2)(1:2) .eq. 'tr') 
     &	     call arrmov(npm,phz(is),tr(it),is,it,phz_size,tr_size)
	  if (alpha(1)(1:3) .eq. 'phz' .and. alpha(2)(1:4) .eq. 'ffta') 
     &	     call arrmov(npm,phz(is),ffta(it),is,it,phz_size,ffta_size)
	  if (alpha(1)(1:4) .eq. 'ffta' .and. alpha(2)(1:1) .eq. 'r') 
     &       call arrmov(npm,ffta(is),r(it),is,it,ffta_size,r_size)
	  if (alpha(1)(1:4) .eq. 'ffta' .and. alpha(2)(1:2) .eq. 'tr') 
     &	     call arrmov(npm,ffta(is),tr(it),is,it,ffta_size,tr_size)
	  if (alpha(1)(1:4) .eq. 'ffta' .and. alpha(2)(1:3) .eq. 'phz') 
     &	     call arrmov(npm,ffta(is),phz(it),is,it,ffta_size,phz_size)
	  if (alpha(1)(1:4).eq.'ffta' .and. alpha(2)(1:4).eq.'ffta') 
     &	   call arrmov(npm,ffta(is),ffta(it),is,it,ffta_size,ffta_size)

	return
	end

c ---------------------------------------------------------------------

	subroutine sinc( r )	 ! create a sinc function in r-array

	include 'datetc.h'
	include 'infmtn.h'
	
	real r(*)

	integer i, icentr
	real x, pi

	parameter ( pi = 3.1415926535898 )

	if (ifl .eq. 0) then
	   call wrtstr(' Error :  missing parameter')
	   return
	end if
	xpara = xnum(1)
	if (ifx .eq. 0) inum(1) = nop
	if ( inum(1) .gt. rtr_size ) then
	   call wrtstr(' Error :  too many points for r array')
	   return
	end if
	nop = inum(1)
					
	nwpp = 1                 ! data are real
	icentr = (nop / 2) + 1   ! center of the sinc funcion
	do i = 1, nop
	   if (i .eq. icentr) then
	      r(i) = 1.0
	   else
	      x = pi * (float(i - icentr) / xpara)
	      r(i) = sin(x) / x
	   end if
	end do

	call disper

	return
	end

c ---------------------------------------------------------------------

	subroutine sincos(r,tr)
*
* calculate sin in r and cos in tr
*
* syntax:   sincos  period  nop
*
	include 'datetc.h'
	include 'infmtn.h'

	real r(*), tr(*)
	double precision can, san, angle, period, pi
	parameter (pi = 3.1415926535898d0 )

	integer i

	if (ifl .eq. 0) then
	   call wrtstr(' Error :  missing parameter')
	   return
	end if
	period = xnum(1)

	if (ifx .eq. 0) inum(1) = nop
	if ( inum(1) .gt. rtr_size ) then
	   call wrtstr(' Error :  too many points for r array')
	   return
	end if
	nop = inum(1)
	if ( nop .lt. 2 ) then
	   call wrtstr(' Error :  too few points for r array.')
	   return
	end if

	nwpp = 1
	angle = 2.d0 * pi / period
	san = sin(angle)
	can = cos(angle)
	r(1) = 0.0
	tr(1) = 1.0
	do i=2,nop
	   r(i) = can * r(i-1)  + san * tr(i-1)
	   tr(i)= can * tr(i-1) - san * r(i-1)
	end do

	call disper

	return
	end

c ---------------------------------------------------------------------

	subroutine setdata(r,tr,fft,phz)
*
* modify a range of data in any one of the arrays
*
	include 'datetc.h'
	include 'infmtn.h'

	real r(*), tr(*), fft(*), phz(*)
	integer i

	ipara = inum(1)
	iparb = inum(1)
	iparc = 1
	if (ifx .ge. 2) iparb = inum(2)
	if (ifx .eq. 3) iparc = inum(3)

	if (nstrng .eq. 0 .or. alpha(1)(1:1) .eq. 'r') then
	   do i = ipara, iparb, iparc
	      r(i) = xnum(1)
	   end do
	end if
	if (alpha(1)(1:2) .eq. 'tr') then
	   do i = ipara, iparb, iparc
	      tr(i) = xnum(1)
	   end do
	end if
	if (alpha(1)(1:3) .eq. 'fft') then
	   do i = ipara, iparb, iparc
	      fft(i) = xnum(1)
	   end do
	end if
	if (alpha(1)(1:3) .eq. 'phz') then
	   do i = ipara, iparb, iparc
	      phz(i) = xnum(1)
	   end do
	end if

	return
	end

c ---------------------------------------------------------------------

	subroutine deriv( r, tr )

c	Take the first derivative of the r array. uses code from lnseek.
c	The derivative is left in r while the original data is in tr.  
c	A region of half the kernel length at each end is zero filled.

	include 'datetc.h'
	include 'infmtn.h'

	real r(*), tr(*)

	integer  i, j, k, m
					
	i = 12  
	j = i + 1
	k = nop - i
	do 110 m = j, k
	tr(m) = neoa*( 0.941764534*( r(m +1) - r(m -1) )
     &	-.393313931*(r(m + 2)-r(m - 2)) +.194249418*(r(m + 3)-r(m - 3))
     &	-.095723222*(r(m + 4)-r(m - 4)) +.044626032*(r(m + 5)-r(m - 5))
     &	-.019220854*(r(m + 6)-r(m - 6)) +.007552247*(r(m + 7)-r(m - 7))
     &	-.002686700*(r(m + 8)-r(m - 8)) +.000861165*(r(m + 9)-r(m - 9))
     &	-.000247875*(r(m +10)-r(m -10)) +.000063919*(r(m +11)-r(m -11))
     &	-.000014741*(r(m +12)-r(m -12)) )
110	continue
	do 120 m = k+1, nop
120	tr(m) = 0.
	do 130 m = 1, i
130	tr(m) = 0.
					! exchange r and tr
	call rext(r,tr)
	return
	end

c ---------------------------------------------------------------------

	subroutine conect( r )

c  replace a section of data in R by a straight line connecting two
c  specified points.

	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'

	real r(*)

	double precision temp
	real del
	integer i, nstrt, nstop

	if (ifx .ne. 2) then
	   call wrtstr(' Error :  missing parameter(s).')
	   return
	end if
	if (inum(1) .ge. inum(2)) then
	   call wrtstr(' Error :  first point > last point.')
	   return
	end if
	nstrt = inum(1)
	nstop = inum(2)
	if ( nstrt.ge.rtr_size .or. nstop.ge.rtr_size ) then
	   call wrtstr(' Error :  outside array bounds.')
	   return
	end if

	del = (r(nstop) - r(nstrt)) / (nstop - nstrt)
	temp = r(nstrt) - del
	do i = nstrt, nstop
	   temp = temp + del
	   r(i) = temp
	end do
	return
	end

c ---------------------------------------------------------------------
c				create a Planck function in r-array
c				call - planck  sigi sigf dels temp
	subroutine planck( r )

	include 'datetc.h'
	include 'infmtn.h'

	real r(*)

	integer i
	real c, tpi

c	data h/6.626176e-34/
	data c/2.99792458e10/,tpi/6.283185308/

	if ( ifl .ne. 4 ) then
	   call wrtstr(' Error :  wrong number of parameters')
	   return
	end if

	nwpp = 1
	sigi = xnum(1)
	sigf = xnum(2)
	delw = xnum(3)
	temp = xnum(4)
	wref = sigi
	nop = 1 + (sigf - sigi)/delw
	if (nop .gt. nrtr) nop = nrtr
	call disper

	do i = 1, nop
	    xparb = i
	    call ptow
	    r(i) = tpi*c*xparb**2/(exp(1.438786*xparb/temp) -1.0)
	end do

	call disper

	write (id,1811) temp
1811	format (' Planck function for a temperature of ',f7.1,
     &          ' in photons/sec /cm**2 /cm-1',6x)
	return
	end

c ---------------------------------------------------------------------

	subroutine rext( r, tr )

	include 'datetc.h'

	real r(*), tr(*)

	real tmp
	integer i

*  exchange r and tr.
	do i = 1, nrtr+2
	   tmp = tr(i)
	   tr(i) = r(i)
	   r(i) = tmp
	end do

	return
	end

c ---------------------------------------------------------------------

	subroutine expr( r )

	include 'datetc.h'
	include 'infmtn.h'

	real r(*)

	logical lunder, lover
	integer i

	lunder = .false.
	lover  = .false.
	do i = 1, nop
	   if (r(i) .gt.  84.0) then
	      r(i) = 84.0
	      lover = .true.
	   else if (r(i) .lt. -84.0) then
	      r(i) = 0.0
	      lunder = .true.
	   else
	      r(i) = exp(r(i))
	   end if
	end do
	if (lover)  call wrtstr(' Warning :  floating point overflow.')
	if (lunder) call wrtstr(' Warning :  floating point underflow.')
	return
	end

c ---------------------------------------------------------------------

	subroutine norm( r )

	include 'datetc.h'
	include 'infmtn.h'

	real r(*)

	integer i
	real temp

c  normalize r to at most a quadratic function

	do i = 1, nop
	    temp = float(i)
	    r(i) = r(i) / (xpara + temp*(xparb + xparc*temp))
	end do
	return
	end

c ---------------------------------------------------------------------

	subroutine mask( r )

	include 'datetc.h'
	include 'infmtn.h'

	real r(*)

	integer i, ib
	real temp

c  mask, rise width iparc, half width iparb

	if (iparc .le. 0) return
	do i = 1, nop
	   ib = iabs(i-iparb-1) - iparb
	   if (ib .gt. 0) then
	      r(i) = 0.0
	      go to 100
	   endif
	   ib = ib + iparc
	   if (ib .le. 0) go to 100
	   temp = (3.1415927 * ib) / iparc
	   xpara = 0.5 * (1.0 + cos(temp))
	   r(i) = r(i) * xpara
 100	   continue
	end do
	return
	end

c ---------------------------------------------------------------------

	subroutine momnts( r )

	include 'infmtn.h'
	include 'datetc.h'

	real r(*)

	integer i
	real x, temp

	call strtrn
	secm = 0.0
	absmx = 0.0
	cgr = 0.0
	avg = 0.0
  
	do i = 1, nop
	   x = i - nh
	   avg = avg + r(i)
	   temp =  abs(r(i))
	   if (absmx .lt. temp) then
	      xpara = temp
	      absmx = temp
	      ipara = i
	   endif
	   temp = x * r(i)
	   cgr = cgr + temp
	   secm = secm + x*temp
	end do
  
	cgr = cgr / avg + nh
	secm = secm/avg
	avg = avg/nop
	return
	end

c ---------------------------------------------------------------------

	subroutine shiftr( tr )

c  multiply transform by complex exponential to shift data xpara points

	include 'datetc.h'

	real tr(*)

	double precision temp

	xpara = 3.1415926535898d+0 * xpara / np
  
	do i = 1, np, 2
	   xparc = (i - 1) * xpara
	   xparb = sin(xparc)
	   xparc = cos(xparc)
	   temp = tr(i) * xparc  -  tr(i+1) * xparb
	   tr(i+1) = tr(i+1) * xparc  +  tr(i) * xparb
	   tr(i) = temp
	end do
  
	tr(np+2) = 0.0
	tr(np+1) = 0.0
	return
	end

c ---------------------------------------------------------------------

	subroutine norml( r )

	include 'datetc.h'
	include 'iounit.h'
	
	real r(*)

	integer ia, ib, i1, i2, i3
	real y1, y2, y3, dy12, dy13, di12, di13, di23, si12, si13
	real al, au, temp

	if (ifx .eq. 3  .and. ifl .eq. 3) go to 60
	ia = inum(1)
	ib = inum(2)
	xparc = 0.0
	xparb = 0.0
	call momnts(r)
	xpara = absmx
	if (ifl .eq. 0) go to 40
	xpara = xpara/xnum(1)
	temp = r(ia)/xnum(1)
	if (ifx .eq. 1) go to 30
	if (ifx .gt. 1) go to 20
	if (ifl .eq. 1) go to 40
	if (ifl .eq. 2) go to 10
  
	xparc = xnum(3)
10	xpara = xnum(1)
	xparb = xnum(2)
	go to 40
20	xparb = ib - ia
	xparb = (r(ib) / xnum(2) - temp) / xparb
30	xpara = temp - xparb * ia
c  normalize
40	call norm(r)
	if (ifl .eq. 3) go to 50
	write (wrtbuf, 9000) xpara, xparb
	call wrtout(output,wrtbuf)
	return
50	write (wrtbuf, 9001) xpara, xparb, xparc
	call wrtout(output,wrtbuf)
	return
60	i1 = inum(1)
	i2 = inum(2)
	i3 = inum(3)
	y1 = r(i1) / xnum(1)
	y2 = r(i2) / xnum(2)
	y3 = r(i3) / xnum(3)
	dy12 = y1 - y2
	dy13 = y1 - y3
	di12 = i1 - i2
	di13 = i1 - i3
	di23 = i2 - i3
	si12 = i1 + i2
	si13 = i1 + i3      ! s13 not used: *BUG* ?
	au = dy12 * di13 - dy13 * di12
	al = di12 * di13 * di23
	xparc = au / al
	xparb = dy12 / di12 - xparc * si12
	xpara = y1 - xparc * i1 * i1 - xparb * i1
	go to 40
  
9000	format (' Data divided by',f22.5, ' +',f11.8, '*i')
9001	format (' Data divided by',f22.5,' +',f11.8, '*i + ',e15.6,
     &          '*i*i')
	end
c ---------------------------------------------------------------------

	subroutine noise( r )

	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'

	integer i,k
	real r(*)
	real uni, temp, ampl

c noise nop
c *****************
c produce a noise record in nop points, add 12 random numbers in interval
c [0,1] with 6.0 subtracted to give zero mean, rms = 1.0
c The random number generator is initialized during program startup
c and can be re-initialized with  'noise nnn init'

    	if (ifx .eq. 0) go to 998

	if (nstrng.eq.1 .and. alpha(1)(1:4).eq.'init') then
	   temp = uni( inum(1) )
	   call wrtstr(' Random number generator is initialized.')
	   return
	end if

	ampl = 1.0
	if ( ifl.ne.0 ) then
	   ampl = xnum(1)
	end if

	if (nstrng.eq.1 .and. alpha(1)(1:4).eq.'add') then
	   nop = inum(1)
	   do i=1,nop
	      temp = 0.0
	      do k=1,12
		 temp = temp + uni(0)
	      end do
	      r(i) = r(i) + ampl * ( temp - 6.0 )
	   end do
	else
	   nop = inum(1)
	   do i=1,nop
	      temp = 0.0
	      do k=1,12
		 temp = temp + uni(0)
	      end do
	      r(i) = ampl * ( temp - 6.0 )
	   end do
	end if

	return

998	call wrtout(output,
     &              ' Error :  syntax error for noise command')
	return
	end

* ---------------------------------------------------------------------

	subroutine arrmov( npts, src, dst, ns, nd, nssiz, ndsiz )
*
* like 'rmove' but checks for array overruns
* used only in the interactive 'move' command
*
* npts : number of points to move
* src  : source array
* dst  : destination array
* ns   : first index in source array
* nd   : first index in destination array
* nssiz: size of source array
* ndsiz: size of destination array
*
	integer npts, ns, nd, nssiz, ndsiz
	integer i
	real src(*), dst(*)

	if ( npts .gt. nssiz - ns + 1 ) then
	   call wrtstr(' Error :  source array overrun')
	   return
	end if
	if ( npts .gt. ndsiz - nd + 1 ) then
	   call wrtstr(' Error :  destination array overrun')
	   return
	end if

	do i=1,npts
	   dst(i) = src(i)
	end do

	return
	end

