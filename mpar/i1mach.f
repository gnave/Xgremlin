*     -*-Fortran-*-
      integer function i1mach(i)
*
*  portable, dynamic integer machine constants
*
*  i/o unit numbers.
*    i1mach( 1) = the standard input unit.
*    i1mach( 2) = the standard output unit.
*    i1mach( 3) = the standard punch unit.
*    i1mach( 4) = the standard error message unit.
*
*  words.
*    i1mach( 5) = the number of bits per integer storage unit.
*    i1mach( 6) = the number of characters per character storage unit.
*                 for fortran 77, this is always 1.  for fortran 66,
*                 character storage unit = integer storage unit.
*
*  integers.
*    assume integers are represented in the s-digit, base-a form
*               sign ( x(s-1)*a**(s-1) + ... + x(1)*a + x(0) )
*               where 0 .le. x(i) .lt. a for i=0,...,s-1.
*    i1mach( 7) = a, the base.
*    i1mach( 8) = s, the number of base-a digits.
*    i1mach( 9) = a**s - 1, the largest magnitude.
*
*  floating-point numbers.
*    assume floating-point numbers are represented in the t-digit,
*    base-b form
*               sign (b**e)*( (x(1)/b) + ... + (x(t)/b**t) )
*               where 0 .le. x(i) .lt. b for i=1,...,t,
*               0 .lt. x(1), and emin .le. e .le. emax.
*    i1mach(10) = b, the base.
*
*  single-precision
*    i1mach(11) = t, the number of base-b digits.
*    i1mach(12) = emin, the smallest exponent e.
*    i1mach(13) = emax, the largest exponent e.
*
*  double-precision
*    i1mach(14) = t, the number of base-b digits.
*    i1mach(15) = emin, the smallest exponent e.
*    i1mach(16) = emax, the largest exponent e.
*
*  Author: Ulf Griesmann
*
      implicit none

      integer i,imach(16)
      integer ibeta,it,irnd,ngrd,machep,negep,iexp,minexp,maxexp
      real eps,epsneg,xmin,xmax
      double precision deps,depsneg,dxmin,dxmax
      logical lcalc
      data lcalc/.false./
      save lcalc,imach

      if (.not.lcalc) then
         lcalc = .true.

* the following entries must be changed if not used on a 32-bit machine !
         imach(1 ) = 5
         imach(2 ) = 6
         imach(3 ) = 7
         imach(4 ) = 0
         imach(5 ) = 32          ! 32-bit integers
         imach(6 ) = 1
         imach(7 ) = 2           ! integer base
         imach(8 ) = 31
         imach(9 ) = 2147483647  ! largest integer

* the following entries are machine independent !
         call smachar(ibeta,it,irnd,ngrd,machep,negep,iexp,
     1                minexp,maxexp,eps,epsneg,xmin,xmax)
         imach(10) = ibeta
         imach(11) = it
         imach(12) = minexp
         imach(13) = maxexp

         call dmachar(ibeta,it,irnd,ngrd,machep,negep,iexp,
     1                minexp,maxexp,deps,depsneg,dxmin,dxmax)
         imach(14) = it
         imach(15) = minexp
         imach(16) = maxexp
      end if
      i1mach = imach(i)
      return
      end
