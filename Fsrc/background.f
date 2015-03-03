      subroutine background(r,tr,phz,nbin,minbin,thresh,imed,iact)
*
* determine and optionally subtract the background continuum from
* a spectrum in the r array.
*
* Algorithm:
*   1) bin the spectrum 
*   2) compute maximum, minimum and median datum in each bin.
*   3) if the bin contains spectral lines then select a sub-bin
*      until a piece of background without lines is found.
*   4) discard all bins where min. and max. differ by more
*      than some threshold value. These bins contain lines.
*   5) Fit a spline through the medians or averages of the 
*      remaining bins. This should be the background.
*
* Storage:
*   real spectrum in the 'r' array, 
*   the fft array is used to store bin data.
*   the tr array is used for temp storage
*
* Variables:
*   nbin   : number of bins
*   minbin : the minimum number of points in a bin
*   thresh : max. allowed difference between min. and max. in bin
*   iact   : =0 : calculate background only; =1 : subtract background.
*            =2 : subtract background only, don't re-calculate
*   imed   : =1 : use median as background estimator; =0 use average
*
* Author: Ulf Griesmann
*
      logical lper, luni
      parameter (lper = .false., luni = .true. )
 
      include 'iounit.h'
      include 'infmtn.h'
      include 'integrate.h'   ! needed for the spline workspace arrays
      include 'color.h'
      include 'background.h'

      integer nbin, minbin, iact
      real thresh
      real r(*), tr(*), phz(*)

      logical ldone   ! T if a good bin was found
      integer nppb    ! number of points per bin
      integer nstart  ! begin of a bin in the r array
      integer idx, i, k, n, m, nump, niter
      real w,sm,smtol
      double precision wnum

      include 'transform.h'

* init variables
      l_backg = .false.

* stop if r-array is empty
      if (nop .lt.minbin) then
         call wrtstr(' Error :  no spectrum in the r-array.')
         return
      end if

* "onlysubtract"
      if (iact .eq. 2) goto 100

* require at least 10 bins
      if (nbin .lt. 10) then
         call wrtstr(' Error :  too few bins (min. 10).')
         return
      end if
      if (nbin .gt. mxpts) then
         call wrtstr(' Error :  too many bins (max. 4096).')
         return
      end if

* bin data, calculate medians, bins without lines
      call itron                      ! following loop can be interrupted
      ldone = .false.
      nppb = 0.5 + real(nop) / real(nbin)
      xint(1) = 1.0                   ! tie spline down at the ends
      yint(1) = r(1)
      idx = 2
      do nstart=1,nop-nppb,nppb
         do i=1,nppb                  ! copy bin into temp storage
            tr(i) = r(nstart+i-1)
         end do
         call findbg(tr,nppb,minbin,thresh,xint(idx),yint(idx),
     &               imed,ldone)
         if (ldone) then
            xint(idx) = xint(idx) + real(nstart) - 1.0
            idx = idx + 1
         end if
         if ( mod(idx,20) .eq. 0) then
            write (wrtbuf,'(2h +,10x,i4)') idx
            call prevline(wrtbuf)
            call procpend
            if (itrflag() .eq. 1) then
               call itroff
               return
            end if
         end if
      end do
      nbb = idx
      xint(nbb) = real(nop)            ! tie spline down at the end
      yint(nbb) = r(nop)
      call itroff
      write(wrtbuf,'(a,i4)') ' Number of background bins found :',nbb-2
      call wrtstr(wrtbuf)
      if (nbb .lt. 4) then
         call wrtstr(' Error :  too few bins for background.')
         return
      end if

* now fit a spline through the background
      do i=1,nbb
         weigh(i) = 1.0
      end do
       
      sm = real(nbb)
      smtol = 0.9                         ! large tolerance
      sigma(1) = 10.0                     ! tension factor

      call tspss( nbb, xint, yint, lper, luni, weigh, sm,
     &     smtol, lenwrk, wrkspc, sigma, ys, yp, niter, ierr )
      if ( ierr .lt. 0 ) then
         call wrtstr(' Error :  spline fit failed.')
         call itroff
         return
      end if

      if (iact .eq. 0) then
         l_backg = .true.                 ! over-plot background
         call plotr(1,r,r,tr,phz)         ! plot a clean spectrum
      end if

 100  continue                            ! subtract only

* subtract continuum from r-array
      if (iact .ne. 0) then
         call wrtstr(' Subtracting background.')
         k = 0
         m = nop / 10
         do i=1,nop
            r(i) = r(i) - hval(real(i),nbb,xint,ys,yp,sigma,ierr)
            if ( i .gt. m ) then 
               k = k + 10
               m = m + m
               write (wrtbuf,'(1h ,i4,1h%)') k
               call prevline(wrtbuf)
               call procpend
            end if
         end do
         l_backg = .false.
         call plotr(1,r,r,tr,phz)
      end if
  
      return
      end

****************************************************************************

      subroutine findbg(arr,npib,minbin,thresh,x,y,imed,ldone)
*
* find a background bin by successive division of the initial bin until
* a background bin within the initial bin has been found.
*
* arr    : array containing the initial bin
* nppb   : number of points in the initial bin
* minbin : minimum number of data points in a bin
* thresh : discriminator for selecting a background bin
* x      : center point of background bin
* y      : median of data in background bin
* imed   : =1, use median as background estimator; =0, use bin average
* ldone  : T if a background bin was found, F otherwise
*
      logical ldone
      integer npib,imed
      real arr(*), thresh, x, y, dummy

      integer nbins, nppb, nstart

* set up initial bin
      ldone = .false.
      nbins = 1
      nppb = npib

* stop if too few points in bin
      if (nppb .le. 10) then
         return
      end if

* loop over sub-binning iterations
 10   continue

      do k=1,nbins
         nstart = nppb * ( k - 1 ) + 1
         call ssort( arr(nstart), dummy, nppb, 1)
         if (abs(arr(nstart)-arr(nstart+nppb-1)) .le. thresh ) then
            x = nstart + nppb/2
            if ( imed .eq. 1 ) then
               y = arr(nstart + nppb/2 - 1)  ! median
            else
               y = aravrg(arr,nppb)          ! average
            end if
            ldone = .true.
            return
         end if
      end do

      nbins = 2 * nbins      ! halve the size of all bins
      nppb = npib / nbins
      if (nppb .lt. minbin) then ! give up
         return
      end if

* try to find a background bin among the smaller bins
      goto 10

      end

****************************************************************************

      subroutine plotbg
*
* plot the background over a spectrum
*
      include 'infmtn.h'
      include 'plot.h'
      include 'integrate.h'
      include 'background.h'
      include 'color.h'

      integer npwid, npoint, i, ncurr, n, nump
      real dp, curr, w
      double precision wnum

      include 'transform.h'

* find out how many pixels are available in the plotter window.
      call getpwid(npwid)
      npwid = npwid-2
      npoint= npwid / 2                ! half as many points as pixels

* calculate the background for the plotted part of the spectrum
      dp = real(n_right - n_left + 1) / real(npoint-1)   ! point increment
      ncurr = n_left
      curr = real(ncurr)
      do i=1,npoint
         ys(i) = wnum(ncurr)
         yaux(i) = hval(real(ncurr),nbb,xint,ys,yp,sigma,ierr)
         curr = curr + dp
         ncurr = int(0.5 + curr)
      end do

* and plot it
      call inhibit
      call pltaux(npoint,ys,yaux,col_red) ! plot into same plot
      call update

      return
      end

****************************************************************************

* Revision history:
* -----------------
* $Log$







