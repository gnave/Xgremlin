*     -*-Fortran-*-
      double precision function d1mach(i)
*
*  portable, dynamic double-precision machine constants
*  d1mach(1) = b**(emin-1), the smallest positive magnitude.
*  d1mach(2) = b**emax*(1 - b**(-t)), the largest magnitude.
*  d1mach(3) = b**(-t), the smallest relative spacing.
*  d1mach(4) = b**(1-t), the largest relative spacing.
*  d1mach(5) = dlog10(b)
*
*  Author: Ulf Griesmann
*
      implicit none

      integer i,ibeta,it,irnd,ngrd,machep,negep,iexp,minexp,maxexp
      double precision eps,epsneg,xmin,xmax,dmach(5)
      logical lcalc
      data lcalc/.false./
      save lcalc,dmach

      if (.not.lcalc) then
         lcalc = .true.
         call dmachar(ibeta,it,irnd,ngrd,machep,negep,iexp,
     1                minexp,maxexp,eps,epsneg,xmin,xmax)
         dmach(1) = xmin
         dmach(2) = xmax
         dmach(3) = eps
         dmach(4) = dble(ibeta) * eps
         dmach(5) = log10( real(ibeta) )
      end if
      d1mach = dmach(i)
      return
      end
