*     -*-Fortran-*-
      double precision function dpmpar(i)
*
*  portable, dynamic double-precision machine constants
*  d1mach(1) = b**(-t), the smallest relative spacing.
*  d1mach(2) = b**(emin-1), the smallest positive magnitude.
*  d1mach(3) = b**emax*(1 - b**(-t)), the largest magnitude.
*
*  Author: Ulf Griesmann
*
      implicit none

      integer i,ibeta,it,irnd,ngrd,machep,negep,iexp,minexp,maxexp
      double precision eps,epsneg,xmin,xmax,dmach(3)
      logical lcalc
      data lcalc/.false./
      save lcalc,dmach

      if (.not.lcalc) then
         lcalc = .true.
         call dmachar(ibeta,it,irnd,ngrd,machep,negep,iexp,
     1                minexp,maxexp,eps,epsneg,xmin,xmax)
         dmach(1) = eps
         dmach(2) = xmin
         dmach(3) = xmax
      end if
      dpmpar = dmach(i)
      return
      end
