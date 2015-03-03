c \grammy\src\wavcon.for  Ver. 1.0

c  Version  1.00  14.04.91

	subroutine disper

	include 'datetc.h'
	include 'infmtn.h'

	integer n,nump
	real w
	double precision wnum
	include 'transform.h'

c find wavelength scale constants
c if npt=1, wref = wavelength of 1st point, otherwise wavelength of ref. line
c if delw=0, use natural scale, otherwise linear scale based on delw
c returns - 	xpara=aps (angstroms/step), 
c		xparb =corr (quadratic coeff),
c		xparc=pref

	if (npt .eq. 0) npt = 1
	pref = npt
	ipara = npt
	if (npt .gt. 1) then
	   pref = dble( nump(real(wref)) )
	end if
	xpara = delw
	xparb = 0.0d+0
	xparc = pref
	if (delw .ne. 0.0d+0) go to 30

c delw = 0.0 is a flag for a non-linear scale; conversion code should be
c installed here

	call wrtstr(' Error :  non-linear scale not possible')

30	aps = xpara
	corr = xparb
	return
	end

c ----------------------------------------------------------------
	subroutine ptow

c convert from point scale to wavelength scale, in and out on xparb

	include 'datetc.h'
	include 'infmtn.h'

        xparb = aps * (xparb - pref)
        xparb = wref + xparb * (1.0d+0 - corr * xparb)
 
	return
	end

c ----------------------------------------------------------------

	subroutine wtop

c convert from wavelength scale to point scale, in and out on xparb

	include 'datetc.h'
	include 'infmtn.h'

        xparb = xparb - wref
        xparb = pref + xparb * (1.0d+0 + corr * xparb) / aps
  
	return
	end

c ----------------------------------------------------------------

	subroutine interp( r )

c use quintic interpolation to find r at xparb.  return in xparb

	include 'datetc.h'
	include 'infmtn.h'

	real r(*)
	double precision temp, x, s

	i = xparb
	x = xparb - i
	s = (12.0d0 + x * (8.0d0 - x * (7.0d0 + x * (2.0d0 - x))))
     &		/ 12.0d0
	temp = 0.2d0 * (r(i+3) / (x - 3.0d+0) - r(i-2) / (x + 2.0d0))
	temp = r(i+2) / (2.0d0 - x) + r(i-1)/ (x + 1.0d0) + temp
	xparb = s*(r(i) + x*(r(i+1) - r(i) + 0.5d0*temp*(x-1.0d0)))
	return
	end

c ----------------------------------------------------------------

	subroutine wtos

* convert from wavelength in nm to sigma (wavenumber) 
* in and out on xparb
*
* refractive index of air as function of wavenumber taken from 
* B. Edlen, The Refractive Index of Air, Metrologia 2, 7 (1966) 
* Birch and Downs, Metrologia 30, 155 (1993)
*
* To obtain the refractive index as a function of wavenumber the average
* value of  n = 1.00027895  was used to convert wavelengths to wavenumbers
* before calculating the refractive index using Edlen's formula with the
* corrections by Birch and Downs. This is possible because the refractive 
* index varies only slowly with wavelength.
*
	include 'datetc.h'
	include 'infmtn.h'

	double precision rair
	parameter ( rair = 1.00027895d0 )

	double precision z, ri

	if ( lvacuum ) then
	   xparb = 1.d+7 / xparb
	   return
	else
	   z = 1.d+7 / ( rair * xparb )
	   z = z * z
	   ri = 1.d0 + 8343.05d-8 + 
     &          15999.d0 / ( 3.89d+9 - z ) +
     &          2406294.d0 / ( 130.0d+8 - z ) 
	   xparb = 1.0d7 / (ri * xparb)
	   return
	end if

	end

c ----------------------------------------------------------------

	subroutine stow

* convert from sigma (in cm^-1) to air or vacuum wavelength in nm 
* in and out on xparb
*
* refractive index of air as function of wavenumber taken from 
* B. Edlen, The Refractive Index of Air, Metrologia 2, 7 (1966) 
* and Birch and Downs, Metrologia 30, 155 (1993)
*
* Note: this formula is known to be valid below 1um
*
	include 'datetc.h'
	include 'infmtn.h'

	double precision z, ri

	if ( xparb .eq. 0.d0 ) return   ! avoid floating point exceptions

	if ( lvacuum ) then
	   xparb = 1.d+7 / xparb
	   return
	else
	   z = xparb * xparb
	   ri = 1.d0 + 8343.05d-8 + 
     &          15999.d0 / ( 3.89d+9 - z ) +
     &          2406294.d0 / ( 130.0d+8 - z ) 
	   xparb = 1.0d7 / (ri * xparb)
	   return
	end if

	end

c ----------------------------------------------------------------

	subroutine intpl(r,tr)

	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'

	real r(*),tr(*)

	integer ia, i, ib
	integer n
	double precision wo, wf, temp

	call disper
	delw = abs(xnum(1))
	wo = xnum(2)
	n = inum(1)
	if (ifl .ge. 3) go to 4645
	if (ifl .eq. 2) go to 4650
	ia = 3
	n = nrtr
	if (ifx .eq. 0) go to 4655
	if (ifx .eq. 1) go to 4640
	n = inum(2)
4640	ia = inum(1)
	if (ia.lt.3) ia = 3
	go to 4655
4645	n = (xnum(3) -xnum(2) +xnum(1))/xnum(1)
	if ( n .le. 0 ) then  ! bail out
	   call wrtstr(
     &     ' Error :  wavelength/wavenumber arguments in wrong order')
	   return
	end if
c test range
4650	xparb = wo
	if (xnum(1) .lt. 0.0 .and. nwos .eq. 1) call stow
	if (xnum(1) .lt. 0.0 .and. nwos .eq. -1) call wtos
	call wtop
	ia = 3
	if (xparb .ge. 3.0) go to 4660
4655	xparb = ia
	call ptow
	if (xnum(1) .lt. 0.0 .and. nwos .eq. 1) call wtos
	if (xnum(1) .lt. 0.0 .and. nwos .eq. -1) call stow
	wo = xparb
4660	if (n .gt. nrtr) n = nrtr
	temp = nop -3
c interpolate
	nop = 0
	do 4680 i=1,n
	  xparb = wo +(i-1)*xnum(1)
	  if (xnum(1) .lt. 0.0 .and. nwos .eq. 1) call stow
	  if (xnum(1) .lt. 0.0 .and. nwos .eq. -1) call wtos
	  call wtop
	  if (xparb.gt.temp) go to 4685
	  call interp(r)
	  tr(i) = xparb
4680	  nop = i
c exchange r and tr
4685	call rext(r,tr)
	npt = 1
	wref = wo
	id(41:50) = '  interped'
	wf = wo +(nop-1)*xnum(1)
	write (wrtbuf,4686) id(41:50), wo, wf, nop
	call wrtout(output,wrtbuf)
4686	format ( 1x,a10, f13.6, ' to', f13.6, ',',i5, ' points.')

	if (xnum(1) .lt. 0.0) then
c          id(41:50) = 'wavenumber'
	   nwos = -nwos
	   wref = wf
	   ia = nop/2
	   do i=1,ia
	      ib = nop+1-i
	      temp = r(i)
	      r(i) = r(ib)
	      r(ib) = temp
	   enddo
	end if
	call disper
	call momnts(r)
	return
	end

c ----------------------------------------------------------------

c correct a wavenumber observed in air to vacuum, using parameters
c from the .hdr file

      double precision function aircorr(s)

      double precision s,rindex
      include 'altinf.h'

      aircorr = s * rindex(pspect,tspect,refwavno,hspect)
     &		  / rindex(pspect,tspect,s,hspect)

      return

      end
c ----------------------------------------------------------------

      double precision function rindex(p,t,s,hoh)

      double precision s
      double precision rmo, dfactr, pp, tt, pw

      pp = dble(p)
      tt = dble(t)
      pw = dble(hoh)

* rmo = refractive index minus 1.0 as a function of sigma (cm-1)
* for STP (Edlen, Metrologia 2, 71 (1966)
      rmo = 8.34305d-5 + (2406294.d0 / (130.d8 - s*s)) +
     &      (15999.d0 / (38.9d8 - s*s) )

c next is the density factor as a function of t (deg. C) and p (torr)
c (Edlen's Eq. 10)

      dfactr = pp*(1.d0 + pp*(0.817d-6 - tt*0.0133d-6)) /
     &         ( 720.775d0*(1.d0 + tt*0.0036610d0) )

c finally, the index is corrected for the partial pressure of water:
c ( hoh  in torr)

      rindex = 1.0d0 + rmo*dfactr - pw*(5.7224d-8 - s*s*0.0457d-16)

      return

      end

* -------------------------------------------------------------------------

	double precision function n2index(p,t,s)
*
* return the refractive index of dry nitrogen gas
* p in Torr and t in deg C
*
* Ulf Griesmann and John H. Burnett, Optics Letters vol. 24, p. 1699 (1999)
* and
* Peck and Khanna, J. Opt. Soc. Am. vol 56, p. 1059 (1966)
*
	real p, t
	double precision s

	double precision nm1, s2, z1, z2, dfactr

	s2 = s * s * 1.d-8       ! cm^-1  -->  um^-1
	if (s .gt. 36000.) then  ! use our VUV formula
	   nm1 = 1.d-6 * (1.9662731d6/(22086.66d0  - s2) + 
     &                    2.7450825d4/(133.85688d0 - s2) )
	else                     ! use Peck and Khanna formula
	   nm1 = 1.d-8*(6855.2d0 + 3243157.0d0/(144.d0 - s2))
	end if

	z1 = 1.d0 - 1.d-5 * p * (317.6d0 - (t + 273.16d0))
	z2 = 1.d0 - 1.d-5 * 760.d0 * (317.6d0 - 273.16d0)
	dfactr = p * 273.16d0 * z2 /(760.d0 * t * z1)
	n2index = 1.d0 + nm1 * dfactr

	return

	end

* -------------------------------------------------------------------------

      subroutine vactoair(sigma, p, t, pw)
*
* apply an air correction to a vacuum wavenumber.
* 
      include 'iounit.h'

      double precision sigma, p, t, pw
      double precision ri, lambda, rindex

      external rindex

      ri = rindex(real(p), real(t), sigma, real(pw) )
      lambda = 1.d7 / sigma  ! vacuum wavelength in nm

      write(wrtbuf,'(a26,f11.6)')  ' Vacuum wavelength / nm : ',lambda
      call wrtstr(wrtbuf)
      write(wrtbuf,'(a26,f11.6)')  ' Air wavelength / nm    : ',
     &             lambda / ri
      call wrtstr(wrtbuf)
      write(wrtbuf,'(a26,f10.8)') ' Refractive index of air: ',ri
      call wrtstr(wrtbuf)

      return 
      end
