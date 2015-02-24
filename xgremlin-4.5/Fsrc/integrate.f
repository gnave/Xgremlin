*-------------------------------------------------------------------------------
*
* This file is part of Xgremlin
*
* subroutines required for determination of line intensities, branching ratios
* subtracting continua, fitting various profiles etc.
*
*-------------------------------------------------------------------------------
*
* Copyright (C) 1994, 1995
* Ulf Griesmann, Gaithersburg MD 20878, U.S.A.
*
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the
* Free Software Foundation; either version 2, or (at your option) any
* later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* (see file COPYING) along with this program; if not, write to the 
* Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
*

*
* $Id: integrate.f,v 1.36 1996/07/15 00:38:54 ulf Exp $
*

*-------------------------------------------------------------------------------

      subroutine defprof
*
* set the type of line profile to be used
*
      include 'datetc.h'
      include 'iounit.h'
      include 'integrate.h'

      character*7 pty(5)
      integer i,k

      data pty/'none   ',
     &         'spline ',
     &         'gauss  ',
     &         'voigt  ',
     &         'lorentz'/


      if (nstrng .eq. 0) then
         write(wrtbuf,'(a,a)') 
     &         ' Current line profile type :  ',pty(ptype)
         call wrtstr(wrtbuf)
         return
      end if

      if ( alpha(1)(1:1) .eq. '?' ) then  ! display parameters
         call shwxpar
         return
      end if
      
      if ( alpha(1)(1:1) .eq. 'h' ) then  ! hold parameters
         lhold = .true.
         do i=1,ifx
            do k=1,npar
               if ( k .eq. inum(i) ) then
                  ihold(k) = 1
                  goto 11
               end if
            end do
 11         continue
         end do

      else if ( alpha(1)(1:1) .eq. 'f' ) then
         if ( ifx .eq. 0 ) then
            do i=1,maxpar       ! free all parameters
               ihold(i) = 0
               mapx(i) = i
            end do
            lhold = .false.     ! do not hold any parameters
            return
         end if

* free the parameters
         do i=1,ifx
            do k=1,npar
               if ( k .eq. inum(i) ) then
                  ihold(k) = 0
                  goto 12
               end if
            end do
 12         continue
         end do


      else if ( alpha(1) .eq. 'set' ) then  ! modify parameters
         call setxpar

      else
         if ( alpha(1)(1:4) .eq. 'none' )  ptype = inone
         if ( alpha(1)(1:5) .eq. 'gauss' ) ptype = igauss
         if ( alpha(1)(1:5) .eq. 'voigt' ) ptype = ivoigt
         if ( alpha(1)(1:5) .eq. 'loren' ) ptype = ilorentz
         if ( alpha(1)(1:5) .eq. 'splin' ) then
            ptype = ispline
            if ( ifl.eq.0 ) then
               smooth = 0.0
            else
               smooth = real(xnum(1))
               if ( smooth .gt. 85.0 ) smooth = 85.0
            end if
         end if
         write(wrtbuf,'(a,a)') 
     &         ' Line profile type set to :  ',pty(ptype)
         call wrtstr(wrtbuf)
         do i=1,maxpar      ! reset hold flags
            ihold(i) = 0
            mapx(i) = i
         end do
      end if

      return
      end

*-------------------------------------------------------------------------------

      subroutine integprof(r,tr,phz)
*
* integrate a profile previously defined with 'profile' command
* in a range defined by markers
*
      include 'datetc.h'
      include 'iounit.h'
      include 'infmtn.h'
      include 'set.h'
      include 'plot.h'
      include 'integrate.h'
      include 'color.h'

      real r(*),tr(*),phz(*)

      logical lper, luni
      parameter (lper = .false., luni = .true. )

      character*4 holstat(2)
      integer iclick, ipoint, i, k, niter, ierr, npts
      integer iclick1, iclick2
      integer n, nump, nfree
      integer info, kflag
      real ax1, ay1, a, b, tsintl
      real sm, smtol, w 
      double precision wnum
      double precision xpar(maxpar)     ! parameters
      integer iwa(maxpar)               ! work space for minpack
      double precision sum,ait,tolminp,argmnt,fwhm,varea,vpeak
      double precision dvoigt, vwidth, vquad

      equivalence ( iclick, iclick1 )

      save xpar

      external gaussfcn
      external voigtfcn
      external dvoigt, vwidth, vquad

      data holstat/'free','hold'/

      include 'transform.h'

* check that the correct number of mouse buttons was pressed
      call dclick( button1, iclick1 )
      call dclick( button2, iclick2 )

      if ( ptype.eq.inone .or. ptype.eq.ispline ) then
         if ( iclick1 .ne. 2 ) then
            call wrtstr(' Error :  specify range with 2 markers' )
            return
         end if
      else
         if ( wdth .eq. 0.0 ) then
            call wrtstr(' Error :  width not set')
            return
         end if

         if (iclick1.eq.0.and.iclick2.eq.0.and.lhold.eqv..true.) then
            goto 999
         end if

         lhold = .false.         

         if ( iclick1 .lt. 2 ) then
            call wrtstr(
     &      ' Error :  set at least 2 markers for continuum.' )
            return
         end if
         if ( mod(iclick1,2) .ne. 0 ) then
            call wrtstr(
     &      ' Error :  # of markers must be multiple of 2.' )
            return
         end if
         if ( iclick2 .eq. 0 ) then
            call wrtstr(' Error :  no line positions specified')
            return
         end if
         if ( iclick1 .gt. maxfit ) then
            call wrtstr(' Error :  too many lines marked')
            return
         end if
      end if

* copy over data into fitting arrays ( if type not 'none' )
      do i=1,iclick1
         call dmouse( button1, i-1, ax, ay, ipoint )
         pbuf(i) = ipoint
         wbuf(i) = ax
         ybuf(i) = ay
      end do
      kflag = 2
      call issort(pbuf, ybuf, iclick1, kflag)

* copy over selected data to fitting array
      npts=0
      do k=1,iclick1,2

         if ( ptype.eq.inone .or. ptype.eq.ispline ) then
            do i=pbuf(k), pbuf(k+1)
               npts = npts+1
               xint(npts) = wnum(i) ! use wave number scale
               yint(npts) = r(i)
               if ( npts .gt. mxpts ) then
                  call wrtstr(
     &            ' Error :  too many points in selected range.')
                  return
               end if
            end do

         else
            do i=pbuf(k), pbuf(k+1)
               npts = npts+1
               dxint(npts) = dble(wnum(i)) ! MINPACK requires double precision
               dyint(npts) = dble(r(i))
               if ( npts .gt. mxpts/2 ) then
                  call wrtstr(
     &            ' Error :  too many points in selected range.')
                  return
               end if
            end do

         end if
      end do

      icbeg = pbuf(1)           ! save the range
      icend = pbuf(iclick1)
      
* start here if no mouse parameters are to be used
 999  continue

* integrate by summation (similar to area command) ----------------------------
      if ( ptype .eq. inone ) then               ! just sum up everything (=area)

         sum = 0.d0
         do i=icbeg, icend
            sum = sum + dble( r(i) )
         end do
         ait = sum*delw
         call wrtstr(' Integration by summation')
         write(logfil,*) ' Integration by summation'
         call wrtstr(' Range of summation:')
         write(logfil,*) ' Range of summation'
         write(wrtbuf,'(1x,4x,i8,a,4x,i8,a)') icbeg,' - ',
     &                                        icend,' points'
         call wrtstr(wrtbuf)
         write(logfil,*) icbeg,' - ',icend,' points'
         if ( nwos .eq. -1 ) then
            write(wrtbuf,'(1x,f12.4,a,f12.4,a)') 
     &            wbuf(1),' - ',wbuf(iclick1),' /cm'
            write(logfil,*) wbuf(1),' - ',wbuf(iclick1),' /cm'
         else
            write(wrtbuf,'(1x,f12.4,a,f12.4,a)') 
     &            wbuf(1),' - ',wbuf(iclick1),' nm'
            write(logfil,*) wbuf(1),' - ',wbuf(iclick1),' nm'
         end if
         call wrtstr(wrtbuf)
         write(wrtbuf,'(a,g12.6)')   ' Sum of r(i) = ',sum
         call wrtstr(wrtbuf)
         write(logfil,*) ' Sum of r(i) = ',sum
         if ( nwos .eq. -1 ) then
            write(wrtbuf,'(a,g12.6,a)')   '  Integral(r)  = ',ait,' /cm'
            write(logfil,*) ' Integral(r) = ',ait,' /cm'
         else
            write(wrtbuf,'(a,g12.6,a)')   '  Integral(r)  = ',ait,' nm'
            write(logfil,*) ' Integral(r) = ',ait,' nm'
         end if
         call wrtstr(wrtbuf)
         call mclear( button1 )


* integrate by fitting a spline under tension -----------------------------------
      else if ( ptype .eq. ispline ) then        

* set up calculation of spline knots
         if ( rvar .ne. 0.0 ) then
            do i=1,npts
               weigh(i) = 1.0/rvar    ! it's not clear what to choose here
            end do
         else
            do i=1,npts
               weigh(i) = 1.0
            end do
         end if

         sm = real(npts)
         smtol = 0.9            ! large tolerance
         sigma(1) = smooth      ! tension factor
         
         call tspss( npts, xint, yint, lper, luni, weigh, sm,
     &               smtol, lenwrk, wrkspc, sigma, ys, yp, niter, ierr )
         if ( ierr .lt. 0 ) then
            call wrtstr(' Error :  spline fit failed.')
            return
         end if
         
* plot fitted spline and residuum
         call mclear( button1 )     ! remove markers

         k = 0                      ! fitted spline in green
         do i=icbeg, icend
            k = k+1
            t = xint(k)             ! xint(k) = wnum(i)
            yaux(k) = hval(t,npts,xint,yint,yp,sigma,ierr)
         end do
         call inhibit
         call pltaux(npts,xint,yaux,col_green) ! plot into same plot

         k = 0                      ! residuum in red
         do i=icbeg, icend
            k = k+1
            yaux(k) = r(i) - yaux(k)
         end do
         call pltaux(npts,xint,yaux,col_red) ! plot into same plot
         call update

* calculate the integral and print
         a = wnum(icbeg)
         b = wnum(icend)
         ait = tsintl( a, b, npts, xint, yint, yp, sigma, ierr )

         call wrtstr(' Spline integration')
         write(logfil,*) ' Spline integration'
         call wrtstr(' Range of integration:')
         write(logfil,*) ' Range of integration:'
         write(wrtbuf,'(1x,4x,i8,a,4x,i8,a)') icbeg,' - ',
     &                                        icend,' points'
         call wrtstr(wrtbuf)
         write(logfil,*) icbeg,' - ',icend,' points'
         if ( nwos .eq. -1 ) then
            write(wrtbuf,'(1x,f12.4,a,f12.4,a)') 
     &            wbuf(1),' - ',wbuf(iclick1),' /cm'
            write(logfil,*) wbuf(1),' - ',wbuf(iclick1),' /cm'
         else
            write(wrtbuf,'(1x,f12.4,a,f12.4,a)') 
     &            wbuf(1),' - ',wbuf(iclick1),' nm'
            write(logfil,*) wbuf(1),' - ',wbuf(iclick1),' nm'
         end if
         call wrtstr(wrtbuf)
         if ( nwos .eq. -1 ) then
            write(wrtbuf,'(a,g12.6,a)')   '  Integral(r) = ',ait,' /cm'
            write(logfil,*) ' Integral(r) = ',ait,' /cm'
         else
            write(wrtbuf,'(a,g12.6,a)')   '  Integral(r) = ',ait,' nm'
            write(logfil,*) ' Integral(r) = ',ait,' nm'
         end if
         call wrtstr(wrtbuf)

*-------------------------------------------------------------------
* for the following profile types integration is achieved by fitting
* one or more line profiles to the experimental data using the
* Levenberg-Marquardt nonlinear solver from Argonne Labs.
*-------------------------------------------------------------------


* profile type GAUSS ----------------------------------------

      else if ( ptype .eq. igauss ) then
*
* fits a set of Gaussian profiles to data ( 3 parameters / line )
* f(x) = sum[ A * exp( -4 * (x-B)^2 / width^2 ) ]
*         

* set up starting parameters for fit
         if ( .not. lhold ) then
            npar = 3 * iclick2             ! 3 parameters per line
            nfree = npar
            k = 0                          ! mouse click counter
            do i=1,npar,3
               call dmouse( button2, k, ax1, ay1, ipoint )
               xpar(i  ) = dble(ax1)  ! B : position
               xpar(i+1) = dble(ay1)  ! A : intensity
               xpar(i+2) = dble(wdth) ! full width at half maximum
               k = k+1
            end do
         end if

* check if sufficient data for fit
        if ( npts .lt. nfree ) then
           call wrtstr(' Error :  too few data points in fit')
           return
        end if

* before fitting parameters must be re-arranged in the xpar array
* so that the free parameters come first. At the same time a parameter
* mapping array is set up which allows to evaluate the functions with
* re-arranged parameters. (NOTE: it would be more elegant not to 
* re-arrange all parameters every time the solver is called but it
* is a lot easier that way - Keep It Simple)
         if ( lhold ) then
            call permpar( nfree, xpar )
        end if

* call the solver (calculate Jacobian numerically for now)
        tolminp = dble(lsqtol)
        call lmdif1(gaussfcn,npts,nfree,xpar,dys,tolminp,
     &               info,iwa,wrkspc,lenwrk)

* return parameters to correct order for printing (by sorting )
         if ( lhold ) then
            kflag = 2
            call idsort(msortx, xpar, npar, kflag)
            do i=1,npar
               mapx(i) = i
            end do
         end if

* now print out error condition / chi^2 / line parameters / integrals
         call wrtstr(' Gauss profile fit / integration')
         write(logfil,*) ' Gauss profile fit / integration'
         call pfitinfo( info )

         sum = 0.0
         do i=1,npts
            sum = sum + dys(i)**2
         end do
         sum = sum / dble(npts+1)
         write(wrtbuf,'(a,g12.4)') ' Chi^2 : ',sum
         write(logfil,*) ' Chi^2 : ',sum
         call wrtstr(wrtbuf)
         call wrtstr(' ')
      
         if ( nwos .eq. -1 ) then
	    call wrtstr(
     &   ' Line Position/cm-1     Peak       FWHM/cm-1 Integral/cm-1')
	    write(logfil,*) 
     &   ' Line Position/cm-1     Peak       FWHM/cm-1 Integral/cm-1'
         else
	    call wrtstr(
     &   ' Line   Position/nm     Peak       FWHM/nm   Integral/nm')
	    write(logfil,*) 
     &   ' Line   Position/nm     Peak       FWHM/nm   Integral/nm'
         end if
         k = 1
         do i=1,npar,3
            write(wrtbuf,'(3x,i2,f14.6,g14.4,f10.4,g14.6)')
     &         k, xpar(i),xpar(i+1),xpar(i+2), 
     &         sqrtpi2 * xpar(i+1) * xpar(i+2) / sqrtln2
            call wrtstr(wrtbuf)
            write(logfil,'(3x,i2,f14.6,g14.4,f10.4,g14.6)')
     &         k, xpar(i),xpar(i+1),xpar(i+2), 
     &         sqrtpi2 * xpar(i+1) * xpar(i+2) / sqrtln2
            k = k + 1
         end do

* plot the result and the residuum on top of the data
         call mclear( button1 )       ! re-plot the data
         call mclear( button2 )
         l_resc = .false.
         call plotr(1,r,r,tr,phz)
      
         npts = 0                     ! plot data without gaps
         do i=icbeg,icend
            npts = npts+1
            sigma(npts) = wnum(i)     ! s.p. for pltaux
            dxint(npts) = dble(sigma(npts))
         end do
         
         call inhibit
         do k=1,npar,3
            do i=1,npts
               argmnt = sqrtln2*(dxint(i)-xpar(k))/xpar(k+2)
               yaux(i) = real(xpar(k+1)*dexp( -4.0d0 * argmnt**2 ))
            end do
            call pltaux(npts,sigma,yaux,col_green) ! individual gauss functions
         end do
         
         call gaussfcn(npts,npar,xpar,dys,i)   ! residuum
         do i=1,npts
            yaux(i) = real(dys(i))
         end do
         call pltaux(npts,sigma,yaux,col_red)      

         do i=1,npts                           ! sum of Gauss functions
            yaux(i) = yaux(i) + real(dyint(i)) ! residuum is in yaux
         end do
         call pltaux(npts,sigma,yaux,col_blue)     
         call update

* profile type VOIGT --------------------------------------

      else if ( ptype .eq. ivoigt ) then

* set up starting parameters for fit
         if ( .not. lhold ) then
            npar = 4 * iclick2             ! 4 parameters per line
            nfree = npar
            k = 0                          ! mouse click counter
            do i=1,npar,4
               call dmouse( button2, k, ax1, ay1, ipoint )
               xpar(i  ) = dble(ax1)            ! B : position
               xpar(i+1) = dble(ay1)            ! A : intensity
               xpar(i+2) = dble(wdth)           ! gaussian width
               xpar(i+3) = sqrtln2 * dble(damp) ! damping parameter
               k = k+1
            end do
         end if

* check if sufficient data for fit
        if ( npts .lt. nfree ) then
           call wrtstr(' Error :  too few data points in fit')
           return
        end if

* permute parameters to move free ones to the front
         if ( lhold ) then
            call permpar( nfree, xpar )
        end if

* call the solver (calculate Jacobian numerically for now)
        tolminp = dble(lsqtol)
        call lmdif1(voigtfcn,npts,nfree,xpar,dys,tolminp,
     &              info,iwa,wrkspc,lenwrk)

* return parameters to correct order for printing (by sorting )
         if ( lhold ) then
            kflag = 2
            call idsort(msortx, xpar, npar, kflag)
            do i=1,npar
               mapx(i) = i
            end do
         end if

* now print out error condition / chi^2 / line parameters / integrals
         call wrtstr(' Voigt profile fit / integration')
         write(logfil,*) ' Voigt profile fit / integration'
         call pfitinfo( info )

         sum = 0.0
         do i=1,npts
            sum = sum + dys(i)**2
         end do
         sum = sum / dble(npts+1)
         write(wrtbuf,'(a,g12.4)') ' Chi^2 : ',sum
         write(logfil,*) ' Chi^2 : ',sum
         call wrtstr(wrtbuf)
         call wrtstr(' ')
      
         if ( nwos .eq. -1 ) then
	    call wrtstr(
     &        ' Line Position/cm-1     Peak       Gwidth/cm-1 damping FWHM/cm-1 I
     &ntegral/cm-1')
	    write(logfil,*) 
     &        ' Line Position/cm-1     Peak       Gwidth/cm-1 damping FWHM/cm-1 I
     &ntegral/cm-1'
         else
	    call wrtstr(
     &        ' Line   Position/nm     Peak       Gwidth/nm damping FWHM/nm   Int
     &egral/nm')
	    write(logfil,*) 
     &        ' Line   Position/nm     Peak       Gwidth/nm damping FWHM/nm   Int
     &egral/nm'
         end if
         k = 1
         do i=1,npar,4
            fwhm = 2.0d0 * xpar(i+2)*vwidth(xpar(i+3))/sqrtln2
            vpeak = xpar(i+1) *
     &              dvoigt(xpar(i),xpar(i),xpar(i+2),xpar(i+3))
            varea= xpar(i+1)*vquad(xpar(i),fwhm,xpar(i+2),xpar(i+3))
            write(wrtbuf,'(3x,i2,f14.6,g14.4,f12.5,f8.4,f10.4,g14.6)')
     &      k, xpar(i),vpeak,xpar(i+2),xpar(i+3)/sqrtln2,fwhm, varea
            call wrtstr(wrtbuf)
            write(logfil,'(3x,i2,f14.6,g14.4,f12.5,f8.4,f10.4,g14.6)')
     &      k, xpar(i),vpeak,xpar(i+2),xpar(i+3)/sqrtln2,fwhm, varea
            k = k + 1
         end do

* plot the result and the residuum on top of the data
         call mclear( button1 )       ! re-plot the data
         call mclear( button2 )
         l_resc = .false.
         call plotr(1,r,r,tr,phz)
      
         npts = 0                     ! plot data without gaps
         do i=icbeg,icend
            npts = npts+1
            sigma(npts) = wnum(i)     ! s.p. for pltaux
            dxint(npts) = dble(sigma(npts))
         end do
         
         call inhibit
         do k=1,npar,4
            do i=1,npts
               yaux(i) = real( xpar(k+1) *
     &            dvoigt(dxint(i),xpar(k),xpar(k+2),xpar(k+3)) )
            end do
            call pltaux(npts,sigma,yaux,col_green) ! individual gauss functions
         end do
         
         call voigtfcn(npts,npar,xpar,dys,i)   ! residuum
         do i=1,npts
            yaux(i) = real(dys(i))
         end do
         call pltaux(npts,sigma,yaux,col_red)      

         do i=1,npts                           ! sum of Gauss functions
            yaux(i) = yaux(i) + real(dyint(i)) ! residuum is in yaux
         end do
         call pltaux(npts,sigma,yaux,col_blue)     
         call update

* profile type LORENTZ --------------------------------------

         else if ( ptype .eq. ilorentz ) then
            call wrtstr(
     &           ' Error :  Lorentz profiles not yet implemented.')

* unsupported profile type
      else
         call wrtstr(' Internal error :  profile type not implemented' )
      end if

      return

*----------------------------------------------------------

      entry setxpar
*
* used to set parameters via profile command
* (requires access to local variables in 'integprof'
*
      if ( ifx .ne. ifl ) then
         call wrtstr(' Error :  missing parameters')
         return
      end if
      do i=1,ifx
         xpar(inum(i)) = xnum(i)
      end do
      return

*----------------------------------------------------------

      entry shwxpar
*
* used to display the parameter array xpar
* (requires access to local variables in 'integprof'
*
      if ( ptype.eq.igauss .or. ptype.eq.ilorentz ) then         
         do k=1,npar,3
            write(wrtbuf,771) 
     &           k,   xpar(k),   holstat(ihold(k)+1),
     &           k+1, xpar(k+1), holstat(ihold(k+1)+1),
     &           k+2, xpar(k+2), holstat(ihold(k+2)+1)
            call wrtstr(wrtbuf)
         end do
      end if

      if ( ptype .eq. ivoigt ) then         
         do k=1,npar,4
            write(wrtbuf,772) 
     &           k,   xpar(k),   holstat(ihold(k)+1),
     &           k+1, xpar(k+1), holstat(ihold(k+1)+1),
     &           k+2, xpar(k+2), holstat(ihold(k+2)+1),
     &           k+3, xpar(k+3), holstat(ihold(k+3)+1)
            call wrtstr(wrtbuf)
         end do
      end if

 771  format(1x,3(i2,': ',g14.6,1x,a4,3x))
 772  format(1x,4(i2,': ',g14.6,1x,a4,3x))

      return

      end

*-------------------------------------------------------------------------------

      subroutine permpar( nfree, xpar )
*
* permute parameters so that free parameters come first
*
* nfree : number of free parameters
* xpar  : parameter vector
*
      include 'integrate.h'

      integer i, nfree
      integer m
      double precision xpar(*) 
      double precision sxpar(maxpar)

      do i=1,npar               ! save them first
         sxpar(i) = xpar(i)
      end do
      nfree = 0
      do i=1,npar               ! collect free parameters first
         if ( ihold(i) .eq. 0 ) then
            nfree = nfree + 1   ! number of free parameters
            xpar(nfree) = sxpar(i) ! re-arranged parameters
            msortx(nfree) = i   ! also store sorting order
            mapx(i) = nfree     ! mapping array
         end if
      end do
      m = nfree
      do i=1,npar               ! now collect fixed parameters
         if ( ihold(i) .eq. 1 ) then
            m = m+1
            xpar(m) = sxpar(i)  ! re-arranged parameters
            msortx(m) = i       ! sorting order
            mapx(i) = m         ! mapping array
         end if
      end do

      return
      end 

*-------------------------------------------------------------------------------

      subroutine pfitinfo( info )
*
* print out MINPACK fit info
*
      include 'iounit.h'

      integer info

      write(wrtbuf,'(a,i2)') ' Info variable : ',info
      call wrtstr(wrtbuf)
      write(logfil,*) ' Info variable :  ',info
      if ( info .eq. 0 ) then
         call wrtstr(' Error :  improper fit parameters')
         write(logfil,*) ' Error :  improper fit parameters'
         return
      end if
      if ( info.ge.1 .and. info.le.3 ) then
         call wrtstr(' Fit successful within tolerance')
         write(logfil,*) ' Fit successful within tolerance'
      end if
      if ( info.eq.4 ) then
         call wrtstr(' Warning :  fit has not succeeded')
         write(logfil,*) ' Warning :  fit has not succeeded'
      end if
      if ( info.eq.5 ) then
         call wrtstr(' Warning :  number of iterations > 200')
         write(logfil,*) ' Warning :  number of iterations > 200'
      end if
      if ( info.eq.6 .or. info.eq.7) then
         call wrtstr(' Warning :  fit tolerance is too small')
         write(logfil,*) ' Warning :  fit tolerance is too small'
      end if

      return
      end

*-------------------------------------------------------------------------------

      subroutine gaussfcn( npts, nfreepar, xpar, fvec, iflag )
*
* fits a sum of npar/3 Gauss profiles
*
* xpar(k  )  line position
* xpar(k+1)  line intensity
* xpar(k+2)  full width at half maximum
*
* NOTE 1:
* the line width parameter in Xgremlin means the width of the Gauss function
* at half maximum which is what spectroscopists normally mean. The width parameter
* (often 'sigma') in the Gauss function is defined differently. The relation between
* the two is:
*               FWHM = (2*sigma) * sqrt( ln(2) )
*
* This is the reason for the parameter 'sqrtln2' by which the experimental 
* width is divided in the evaluation of the Gauss function
*
* NOTE 2:
* The parameter 'npar' (number of parameters) is passed in a common block!
* The number of parameters 'nfreepar' which is handed down by MINPACK
* is ignored.
*
      include 'integrate.h'

      integer npts, nfreepar, iflag
      double precision xpar(*), fvec(*)
      double precision sumexp
      double precision  arg

      integer i,k

      do i=1,npts
         sumexp = 0.0d0
         do k=1,npar,3    ! sum must run over ALL parameters, not just nparm
            arg = sqrtln2 * ( dxint(i) - xpar(mapx(k)) )/xpar(mapx(k+2))
            sumexp = sumexp + xpar(mapx(k+1)) * dexp( -4.0d0 * arg**2 )
         end do
         fvec(i) = sumexp - dyint(i)
      end do

      return
      end

*-------------------------------------------------------------------------------

      subroutine voigtfcn( npts, nfreepar, xpar, fvec, iflag )
*
* fits a sum of npar/4 Voigt profiles
*
* xpar(k  ) = line position
* xpar(k+1) = line intensity
* xpar(k+2) = gaussian width
* xpar(k+3) = damping parameter
*
* NOTE 1:
* The damping parameter is here defined as  
* sqrt(ln2) * lorentz_width / gauss_width !
*
      include 'integrate.h'

      integer npts, nfreepar, iflag
      double precision xpar(*), fvec(*)
      double precision sumexp
      double precision dvoigt
      double precision gwidth

      external dvoigt

      integer i,k

      do i=1,npts
         sumexp = 0.0d0
         do k=1,npar,4    ! sum must run over ALL parameters, not just nparm
            gwidth = xpar(mapx(k+2))
            sumexp = sumexp + xpar(mapx(k+1)) * 
     &      dvoigt(dxint(i),xpar(mapx(k)),gwidth,xpar(mapx(k+3)))
         end do
         fvec(i) = sumexp - dyint(i)
      end do

      return
      end

*-------------------------------------------------------------------------------

      subroutine fitpoly(r,tr,phz,nord,nsubtr)
* 
* fit a Chebychev polynomial to a stretch of data in the r array and subtract from
* r array if desired
*
* nord   : order of polynomial <= 10
* nsubtr : = 0 : do not subtract from r array
*          = 1 : subtract fit from r array
*
      include 'datetc.h'
      include 'set.h'
      include 'infmtn.h'
      include 'iounit.h'
      include 'plot.h'
      include 'integrate.h'
      include 'chebychev.h'
      include 'color.h'

      real r(*),tr(*),phz(*)

      integer nsubtr, nord, icoef
      integer iclick, ipoint, npts, i, k, kflag
      real chisq,t,pcoef(ncoef)
      real w
      double precision wnum

      external chepol

      include 'transform.h'

* nord == -1 : restore the r array
      if ( nord .eq. -1 ) then  ! restore r array
         do i=icbeg,icend
            r(i) = tr(i)
         end do
         goto 1000
      end if

      if ( nord .gt. ncoef-1 )  nord = ncoef-1

* check number of mouse markers 
      call dclick( button1, iclick )
      if ( iclick .lt. 2 ) then
         call wrtstr(
     &        ' Error :  set at least 2 markers for continuum.' )
         return
      end if
      if ( mod(iclick,2) .ne. 0 ) then
         call wrtstr(' Error :  # of markers must be multiple of 2.' )
         return
      end if

* sort mouseclicks        
         do i=1,iclick
            call dmouse( button1, i-1, ax, ay, ipoint )
            pbuf(i) = ipoint      
            ybuf(i) = ay
         end do
         kflag = 2
         call issort(pbuf, ybuf, iclick, kflag)

* copy over selected data to fitting array
         npts=0
         do k=1,iclick,2
            do i=pbuf(k), pbuf(k+1)
               npts = npts+1
               xint(npts) = wnum(i)           ! use wave number scale
               yint(npts) = r(i)
               if ( npts .gt. mxpts ) then
                  call wrtstr( 
     &            ' Error :  too many points in selected range.')
                  return
               end if
            end do
         end do

* fit a Chebychev polynomial to the background continuum and subtract 
* from r array
* Algorithm:
* two ranges of the background continuum are marked, one left and one right 
* of the line in question (requires 4 markers). A polynomial is then fitted 
* through all points in the marked ranges and subtracted from the r array.
*

* fit Chebychev polynomials
      do i=1,npts
         sigma(i) = 1.0
      end do

      bma = 0.5 * ( xint(npts) - xint(1) )
      bpa = 0.5 * ( xint(npts) + xint(1) )

      icoef = nord + 1
      if ( npts .lt. icoef ) then
         call wrtstr(' Error :  too few points for Chebychev fit.')
         return
      end if
      call svdlsq(xint,yint,sigma,npts,chcoef,icoef,uu,vv,ww,
     &            chisq,std,svdtol,chepol)
      
* calculate (and subtract) fit
      call dclick( button2, iclick2 )  ! any blue mouse markers ?
      if ( iclick2 .gt. 0 ) then
         if ( iclick2 .eq. 2 ) then
            call dmouse( button2, 0, ax, ay, icbeg )
            call dmouse( button2, 1, ax, ay, icend )
         else
            call wrtstr(' Error :  must set exactly 2 blue markers.' )
         end if
      else
         icbeg = pbuf(1)        ! begin of a range
         icend = pbuf(iclick)
      end if
      if ( icbeg .gt. icend )  call iswap( icbeg, icend )

      k = 0                     ! k = number of evaluated points
      if (nsubtr .eq. 0) then         
         do i=icbeg, icend
            k = k+1
            t = wnum(i)
            xint(k) = t         ! save fit for plotting
            yaux(k) = chebev( chcoef, icoef, t )
         end do
      else
         do i=icbeg, icend
            k = k+1
            tr(i) = r(i)        ! save affected section of r in tr
            t = wnum(i)
            xint(k) = t         ! save fit for plotting
            yaux(k) = chebev( chcoef, icoef, t )
            r(i) = r(i) - yaux(k)
         end do
      end if         

      rvar = chisq               ! estimated noise of data

      call mclear( button1 )
      call mclear( button2 )
      l_resc = .false.
      call plotr(1,r,r,tr,phz)
      call inhibit
      call pltaux(k,xint,yaux,col_blue) ! plot into same plot
      call update

* print out polynomial  coefficients of fit
      call chebpc( chcoef, pcoef, icoef)
      call pcshft( pcoef, icoef )    ! transform [-1,1] --> [a,b]
      call wrtstr(' Polynomial coefficients of fit:')
      write(logfil,*) ' Polynomial coefficients of fit:'
      do k=1,icoef
         write(wrtbuf,777) k-1,pcoef(k)
         call wrtstr(wrtbuf)
         write(logfil,777) k-1,pcoef(k)
      end do
 777  format(1x,'a[',i2,'] = ',g15.7)

 1000 continue
      return
      end

*-------------------------------------------------------------------------------
      
      subroutine moments( n, r )
*
* calculate and print the first n moments of a line
*
* Algorithm:
* two markers are set, one to mark the centre of the line, a second one to
* mark the range of integration. 
*
      include 'datetc.h'
      include 'infmtn.h'
      include 'iounit.h'
      include 'plot.h'

      integer n
      real r(*)
      
      integer iclick, ipoint1, ipoint2, numpt, ibeg, iend
      integer k
      real ax1, ay1, ax2, ay2
      double precision amom(20)

      call dclick( button1, iclick )
      if ( iclick .ne. 2 ) then
         call wrtstr(' Error :  exactly 2 makers must be set.')
         return
      end if

      call dmouse( button1, 0, ax1, ay1, ipoint1 )
      call dmouse( button1, 1, ax2, ay2, ipoint2 )
      if ( r(ipoint2) .gt. r(ipoint1) ) then  ! first point marks center
         call aswap( ax1, ax2 )
         call aswap( ay1, ay2 )
         call iswap( ipoint1, ipoint2 )
      end if

      if ( n.gt.20 ) n = 20                     ! just to be on the safe side
      numpt = int(0.5 + abs(ax1 - ax2) / delw)
      ibeg = ipoint1 - numpt
      iend = ipoint1 + numpt
      do k=1,20
         amom(k) = 0.d0
      end do
      do i=ibeg,iend
         amom(1) = amom(1) + dble(r(i) * delw)
         do k=2,n
            amom(k) = amom(k)+r(i)*dble( delw*real(i - ipoint1) )**(k-1)
         end do
      end do

      call wrtstr('  ')
      if (nwos .eq. -1) then
         write(wrtbuf,'(a,f12.4,a)') ' Moments of line at : ',ax1,' /cm'
         write(logfil,*) ' Moments of line at : ',ax1,' /cm'
      else
         write(wrtbuf,'(a,f12.4,a)') ' Moments of line at : ',ax1,' nm'
         write(logfil,*) ' Moments of line at : ',ax1,' nm'
      end if
      call wrtstr(wrtbuf)
      call wrtstr('            absolute         relative to E(1)')
      do k=1,n
         write(wrtbuf,'(a,i2,a,g12.6,8x,g12.6)') ' E(',k,') = ',amom(k),
     &   amom(k)/amom(1)
         call wrtstr(wrtbuf)
         write(logfil,*) ' E(',k,') = ',amom(k),'   ',amom(k)/amom(1)
      end do
      
      call mclear( button1 )
      
      return
      end

*-------------------------------------------------------------------------------

      subroutine aswap( a, b )
      real a, b, tmp
      tmp = a
      a = b
      b = tmp
      return
      end

*-------------------------------------------------------------------------------

      subroutine iswap( ia, ib )
      integer ia, ib, itmp
      itmp = ia
      ia = ib
      ib = itmp
      return
      end

*-------------------------------------------------------------------------------

* Revision history:
* -----------------
* $Log: integrate.f,v $
* Revision 1.36  1996/07/15 00:38:54  ulf
* .inc --> .h
*
* Revision 1.35  1996/07/08 01:41:26  ulf
* removed 'nord' and 'icoef' variables from 'chebychev.h'
*
* Revision 1.34  1996/07/03 03:52:00  ulf
* use new 'svdlsq' for Chebychev fits
*
* Revision 1.33  1996/07/02 01:21:05  ulf
* change 'usvd.h' --> 'chebychev.h'
*
* Revision 1.32  1996/06/30 03:47:01  ulf
* put subroutines for calculation and evaluation of Chebychev polynomials
* into a separate module
*
* Revision 1.31  1996/06/30 01:03:14  ulf
* overlooked one call to sorting in 'fitpoly'
*
* Revision 1.30  1996/06/30 00:45:53  ulf
* one more sorting routine changed
*
* Revision 1.29  1996/06/30 00:43:54  ulf
* use public domain sorting routines instead of 'Numerical Recipes'
*
* Revision 1.28  1996/06/25 01:57:13  ulf
* added interactive Voigt profile fitting
*
* Revision 1.27  1996/06/11 01:31:08  ulf
* correctly declared array 'msortx' in 'integrate.h'
*
* Revision 1.26  1996/04/26 02:38:39  ulf
* fixed a typo in the call of usvdfit: coef --> ncoef. now 'fitpoly' works again.
*
* Revision 1.25  1996/03/22 18:22:57  ulf
* pass phase array to subroutine 'plotr'
*
* Revision 1.24  1996/03/17  16:57:16  ulf
* fixed call of 'usvdfit': w --> ww
*
* Revision 1.23  1996/03/13  18:02:59  ulf
* modified for dynamic arrays
*
* Revision 1.22  1995/10/26  16:10:34  ulf
* moved nonlinear fitting from single precision MINPACK to double precision
* MINPACK.
*
* Revision 1.21  1995/10/25  08:28:53  ulf
* another fix: remove a \n in the text output of 'fitpoly'
*
* Revision 1.20  1995/10/25  08:27:47  ulf
* added calls to 'inhibit' and 'update' around the call to 'pltaux' in subroutine
* 'fitpoly'
*
* Revision 1.19  1995/10/24  09:59:33  ulf
* fixed the output of the integrate command with profile type 'none'
* and 'spline'
*
* Revision 1.18  1995/10/22  14:11:46  ulf
* finished support for fitting of multiple Gauss profiles interactively
* using the MINPACK nonlinear solver from Argonne Labs.
*
* Revision 1.17  1995/10/15  13:55:24  ulf
* small bug fix
*
* Revision 1.16  1995/10/15  13:53:36  ulf
* added support for fitting and integrating of Gauss functions. Still a bit
* rough round the edges and it is not yet possible to hold or set
* individual parameters.
*
* Revision 1.15  1995/09/26  14:11:32  ulf
* fixed a line that was too long
*
* Revision 1.14  1995/09/26  13:16:11  ulf
* run through the 'spag' source code analyzer from Polyhedron Software:
* fixed unneeded includes and a 'variable used but not assigned' bug.
*
* Revision 1.13  1995/08/01  02:54:04  ulf
* extend subroutine 'subpoly' to allow for extrapolation using the blue
* mouse markers.
*
* Revision 1.12  1995/07/01  17:35:19  ulf
* simplify plotting of integrated curve and residuum in spline
* integration.
*
* Revision 1.11  1995/06/25  23:53:28  ulf
* Now use a 'g' format in subroutine 'fitpoly' for writing out polynomial coefficients
* in order to avoid those ****** stars.
*
* Revision 1.10  1995/06/25  23:11:03  ulf
* Fixed buglet in the calculation of polynomial coefficients in 'pcshft.f'.
*
* Revision 1.9  1995/06/24  19:01:11  ulf
* Background subtraction (aka. polynomial fitting) still did not work correctly.
* Finally found and fixed the error in the 'chebev.f' subroutine.
*
* Revision 1.8  1995/06/23  17:00:47  ulf
* Oh deary me.. forgot to transform polynomial coefficients from interval [-1,1] to [a,b]
* fixed.
*
* Revision 1.7  1995/06/23  15:41:32  ulf
* Print out the polynomial coefficients after a continuum fit to make it easier
* to judge the quality of the fit.
*
* Revision 1.6  1995/06/23  15:07:33  ulf
* Oops.... Continuum subtraction did not work correctly if more than one
* range of points from the continuum was selected. fixed.
*
* Revision 1.5  1995/06/22  17:26:58  ulf
* spline integration had acquired a bug: ait must not be multiplied
* with delw after integration because integration is carried out on
* correct wavelength scale now.
*
* Revision 1.4  1995/06/22  03:50:39  ulf
* cm^-1 --> /cm
*
* Revision 1.3  1995/06/21  04:02:23  ulf
* include 'parameters.h' where necessary
*
* Revision 1.2  1995/06/18  02:04:42  ulf
* fixed a bug in background subtraction. subtracted polynomial is
* now plotted correctly. Fixed a bug in 'chepol' subroutine which
* calculates the Chebychev polynomials --> does work now.
*
* Revision 1.1  1995/06/14  04:08:59  ulf
* Initial revision
*

