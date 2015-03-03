      subroutine response(r,tr)
*
* receive the measured spectrum of a standard lamp in the 'r' array and
* turn it into a weighting function in 'r' with the help of radiance data
* to be used with a 'writelines' command for intensity calibration.
*
* Usage:
*         response  <filename>  [<tension factor>]  <'photon'|'intensity'>
*
*  <filename>   : name of the file with standard lamp radiance data (either in 
*                 the current directory or in /usr/local/xgremlin. The
*                 radiance data MUST be in  W / cm^2 nm sr against nm !
*  tens. fact.  : tension factor for the spline fit 0.0 < tens. fact < 85.0
*  'photon'     : the result is proportional to photons per second
*  'intensity'  : the result is proportional to intensity
*
* Note on the algorithm:
* A spline interpolation is used to interpolate the radiance data in WAVELENGTH space
* before transforming them into wavenumber space. The interpolation is carried
* out on the logarithm of the radiance data to make it numerically more stable because
* the radiance data vary over 6 orders of magnitude.
*
      include 'datetc.h'
      include 'infmtn.h'
      include 'iounit.h'
      include 'integrate.h'        ! use arrays declared therein

      real r(*), tr(*)

      logical lsigmul, lex, lper, luni, lextra, lnocal
      integer i, n, len, nf, npts, niter, ierr, nump, kflag
      integer getenv
      real sm, smtol, w, rmin, rmax, hval, tensfac
      real wlen, ri, s, t, wb, we
      character*256 rdfnam, string, path
      character line*80, ch*1, fields(2)*32
      double precision wnum

      external getenv, hval

      include 'transform.h'

* defaults
      lper    = .false.         ! not a periodic function
      luni    = .false.         ! spline tension is not required to be uniform
      lnocal  = .false.         ! calibration available for entire spectrum
      npts    = 0               ! re-set counter

* check command parameters
      if ( nstrng .ne. 2 ) then
         call wrtstr(' Error :  a string parameter is missing.')
         return
      end if
      if (alpha(2)(1:4).eq.'phot')  then
         lsigmul = .true.
      else if (alpha(2)(1:4).eq.'inte')  then
         lsigmul = .false.
      else
         call wrtstr(' Error :  response curve type missing.')
         return
      end if
      if ( ifl .ne. 0 ) then
         tensfac = xnum(1)
         if (tensfac .gt. 85.0) then
            call wrtstr(' Error :  tension factor > 85.')
            return
         end if
         if (tensfac .lt. 0.0) then
            call wrtstr(' Error :  tension factor < 0.')
            return
         end if
      else
         tensfac = 0.0
      end if

* find radiance data file and read it in
      rdfnam = alpha(1)
      inquire(file=rdfnam,exist=lex)
      if ( .not.lex ) then
         path = 'XGREMLIN_CONF_PATH'
         len = getenv(path,string)
         if ( len .eq. 0 ) then
            rdfnam = '/usr/local/xgremlin/'//alpha(1)
         else
            l = length(string)
            if (string(l:l) .ne. '/') then
               l = l+1
               string(l:l) = '/'
            endif
            rdfnam = string(1:l)//alpha(1)
         end if
         inquire(file=rdfnam,exist=lex)
         if ( .not.lex ) then
            call wrtstr(' Error :  radiance data file not found.')
            return
         end if
      end if

      open(77,file=rdfnam,status='old')
 10   read(77,'(a80)',end=20) line
      ch = line(1:1)
      if ( ch.eq.';' .or. ch.eq.'#' .or. ch.eq.'!' ) then
         goto 10
      end if

      call split(line, fields, nf, 2)
      npts = npts+1
      read(fields(1),*,err=91) xint(npts)
      read(fields(2),*,err=91) t
      yint(npts) = log(t)
      goto 10

 20   close(77)

* sort radiance data in ascending order
      kflag = 2                 ! sort wavelengths and carry radiances along
      call ssort(xint, yint, npts, kflag)
      write(wrtbuf,'(a,f12.4,a,f12.4)')
     &      ' Approx. wavenumber range of radiance data : ',
     &      1.0e7/xint(npts),' - ',1.0e7/xint(1)
      call wrtstr(wrtbuf)

* calculate spline knots for radiance data
      do i=1,npts
         weigh(i) = 1.0
      end do

      sm = real(npts)
      smtol = 0.9               ! large tolerance
      sigma(1) = tensfac        ! moderate tension factor
         
      call tspss(npts, xint, yint, lper, luni, weigh, sm,
     &           smtol, lenwrk, wrkspc, sigma, ys, yp, niter, ierr)
      if ( ierr .lt. 0 ) then
         call wrtstr(' Error :  spline interpolation/fit failed.')
         return
      end if

* begin and end of calibration wavelength range
      wb = xint(1)
      we = xint(npts)

* calculate weighting function in r and normalize to a peak value of 1
      lextra = .false.
      do i=1,nop
         s = wnum(i)                                   ! wavenumber
	 if (s .eq. 0.0) then
            r(i) = 1.0
            goto 30 
         end if
         wlen = 1.0e7 / s                              ! vacuum wavelength
         if (.not.lvacuum) then
            ri = rindex(760.0, 20.0, dble(s), 0.0)     ! refractive index
            wlen = wlen / ri                           ! air wavelength (std. cond.)
         end if
         if (wlen.le.wb .or. wlen.ge.we) then          ! no calibration available
            r(i) = 0.0
            lnocal = .true.
            goto 30
         end if
         t = hval(wlen,npts,xint,yint,yp,sigma,ierr)   ! interpolate radiance
         tr(i) = exp(t)                                ! store radiance in tr
         r(i) = s*s * r(i) / tr(i)                     ! weight function in r
         if (lsigmul) then
            r(i) = s * r(i)                            ! sigma weighting
         end if
         if ( ierr .eq. 1 ) lextra = .true.
 30      continue
      end do
      if (lsigmul) then
         call wrtstr(' Response curve was weighted with wavenumber.')
      end if
      if ( lextra ) then
         call wrtstr(' Warning :  radiance data were extrapolated.')
      end if
      if ( lnocal ) then
         call wrtstr
     &     (' WARNING :  CALIBRATION DOES NOT OVERLAP SPECTRUM')
         call wrtstr
     &     (' WRITELINES will omit lines outside calibration range.')
      end if

* normalize resulting response (weight) function to a peak value of 1.0
      call arminmax(r, nop, rmin, rmax)
      do i=1,nop
         r(i) = r(i) / rmax
      end do

* and get the hell out of here
      return

* error messages
 91   call wrtstr(' Error :  invalid number in radiance data file.')

      return
      end

* Revision history:
* -----------------
* $Log: response.f,v $
* Revision 1.12  1997/08/20 21:37:58  ulf
* write out wavenumber range in calibration file after sorting.
*
* Revision 1.9  1996/07/15 00:37:31  ulf
* *** empty log message ***
*
* Revision 1.8  1996/03/23 06:21:42  ulf
* make 'photon' or 'energy' parameters mandatory
*
* Revision 1.7  1996/03/17  17:19:27  ulf
* fixed a typo: string(n:n) must be string(l:l)
*
* Revision 1.6  1996/03/13  18:02:59  ulf
* modified for dynamic arrays
*
* Revision 1.5  1996/03/06  16:05:37  ulf
* fixed a typo: weighed --> weighted
*
* Revision 1.4  1996/03/05  18:12:24  ulf
* modified for dynamic arrays
*
* Revision 1.3  1996/03/04  14:55:05  ulf
* correct weighting with wavenumber for dimensionless
* response function for photon and energy detectors.
*
* Revision 1.2  1996/02/28  16:24:20  ulf
* fixed comments in revision history
*
* Revision 1.1  1996/02/28  16:22:26  ulf
* Initial revision
*


