*     -*-Fortran-*-
      real function spmpar(i)
*
*  portable, dynamic single-precision machine constants
*  r1mach(1) = b**(-t), the smallest relative spacing.
*  r1mach(2) = b**(emin-1), the smallest positive magnitude.
*  r1mach(3) = b**emax*(1 - b**(-t)), the largest magnitude.
*
*  Author: Ulf Griesmann
*
      implicit none

      integer i,ibeta,it,irnd,ngrd,machep,negep,iexp,minexp,maxexp
      real eps,epsneg,xmin,xmax,rmach(3)
      logical lcalc
      data lcalc/.false./
      save lcalc,rmach

      if (.not.lcalc) then
         lcalc = .true.
         call smachar(ibeta,it,irnd,ngrd,machep,negep,iexp,
     1                minexp,maxexp,eps,epsneg,xmin,xmax)
         rmach(1) = eps
         rmach(2) = xmin
         rmach(3) = xmax
      end if
      spmpar = rmach(i)
      return
      end
