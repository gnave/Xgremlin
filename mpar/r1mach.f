*     -*-Fortran-*-
      real function r1mach(i)
*
*  portable, dynamic single-precision machine constants
*  r1mach(1) = b**(emin-1), the smallest positive magnitude.
*  r1mach(2) = b**emax*(1 - b**(-t)), the largest magnitude.
*  r1mach(3) = b**(-t), the smallest relative spacing.
*  r1mach(4) = b**(1-t), the largest relative spacing.
*  r1mach(5) = log10(b)
*
*  Author: Ulf Griesmann
*
      implicit none

      integer i,ibeta,it,irnd,ngrd,machep,negep,iexp,minexp,maxexp
      real eps,epsneg,xmin,xmax,rmach(5)
      logical lcalc
      data lcalc/.false./
      save lcalc,rmach

      if (.not.lcalc) then
         lcalc = .true.
         call smachar(ibeta,it,irnd,ngrd,machep,negep,iexp,
     1                minexp,maxexp,eps,epsneg,xmin,xmax)
         rmach(1) = xmin
         rmach(2) = xmax
         rmach(3) = eps
         rmach(4) = real(ibeta) * eps
         rmach(5) = log10( real(ibeta) )
      end if
      r1mach = rmach(i)
      return
      end
