*     -*-Fortran-*-
      subroutine arminmax(arr, n, rmin, rmax)
*
* determines minimum and maximum in an array
*
      implicit none

      integer i,n
      real arr(n), rmin, rmax, rminloc, rmaxloc

      rminloc = arr(1)
      rmaxloc = arr(1)
      do i=2,n
         if ( arr(i) .gt. rmaxloc ) rmaxloc = arr(i)
         if ( arr(i) .lt. rminloc ) rminloc = arr(i)
      end do

      rmin = rminloc
      rmax = rmaxloc

      return
      end

*--------------------------------------------------------------------------

      subroutine arposmax(arr, n, rmin, imin, rmax, imax)
*
* determines minimum and maximum in an array and returns them
* together with the positions in the array.
*
      implicit none

      integer i,n, imin, imax, minloc, maxloc
      real arr(n), rmin, rmax, rminloc, rmaxloc

      minloc = 1
      maxloc = 1
      rminloc = arr(1)
      rmaxloc = arr(1)
      do i=2,n
         if ( arr(i) .gt. rmaxloc ) then
            rmaxloc = arr(i)
            maxloc = i
         end if
         if ( arr(i) .lt. rminloc ) then
            rminloc = arr(i)
            minloc = i
         end if
      end do

      imin = minloc
      imax = maxloc
      rmin = rminloc
      rmax = rmaxloc


      return
      end

*--------------------------------------------------------------------------

      real function aravrg(arr, n)
*
* calculates the average of the data points in the array
*
      integer i,n
      real sum,arr(*)

      sum = 0.0
      do i=1,n
         sum = sum + arr(i)
      end do
      aravrg = sum / n

      return
      end
