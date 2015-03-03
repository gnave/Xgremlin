*     -*-Fortran-*-
      subroutine zminmax( zarr, n, zrmin, zrmax, zimin, zimax )
*
* determines minima and maxima of real and imag parts in a complex array
*
      implicit none

      integer i,n
      complex zarr(n) 
      real zrmin, zrmax, zimin, zimax, re, im

      zrmin = real( zarr(1) )
      zrmax = real( zarr(1) )
      zimin = aimag( zarr(1) )
      zimax = aimag( zarr(1) )

      do i=2,n
         re = real( zarr(i) )         ! makes it faster with f2c
         im = aimag( zarr(i) )
         if ( re .gt. zrmax ) zrmax = re
         if ( re .lt. zrmin ) zrmin = re
         if ( im .gt. zimax ) zimax = im
         if ( im .lt. zimin ) zimin = im
      end do

      return
      end

*--------------------------------------------------------------------------

      subroutine zsum( c, ia, ib, are, aim )
*
* calculates the sum of real parts and the sum of imaginary parts in 
* complex array 'c' from point 'ia' to point 'ib'
*
      complex c(*)
      integer ia, ib
      real are, aim

      integer i
      
      are = 0.0
      aim = 0.0
      do i=ia, ib
         are = are + real( c(i) )
         aim = aim + aimag( c(i) )
      end do

      return
      end

