*------------------------------------------------------------------
*
* NOTE (U. Griesmann):
* The implementation of the FFT in the following subroutines goes
* back to:
*   G. D. Bergland and M. T. Dolan
*   Bell Laboratories, Murray Hill, New Jersey 07974
*   1970's
*
*------------------------------------------------------------------
*
*  12/28/1997 increased max. number of points to 16M (U.G.)
c  Revision  1.03  02/10/93 Increased to 4096K
c  Revision  1.02  02/23/92  Sun conversion
c  Version  1.01  10/08/91 Combines 'fsst' (<=32k), 'ffsk' (>32k)
c  Version  1.00  10/04/91 Combines 'fast' (<=32k), 'ffak' (>32k)

c-----------------------------------------------------------------------
c subroutine:  fast
c replaces the real vector b(k), for k=1,2,...,n,
c with its finite discrete fourier transform
c MODIFIED to move origin to n/2 + 1 instead of 1
c-----------------------------------------------------------------------

	subroutine fast(b, n)

c the dc term is returned in location b(1) with b(2) set to 0.
c thereafter the jth harmonic is returned as a complex
c number stored as  b(2*j+1) + i b(2*j+2).
c the n/2 harmonic is returned in b(n+1) with b(n+2) set to 0.
c hence, b must be dimensioned to size n+2.
c the subroutine is called as  fast(b,n) where n=2**m and
c b is the real array described above.

	real b(*)

	do i=1,15
	   m = i
	   nt = 2**i
	   if (n .eq. nt) go to 20
	end do
	call wrtstr(' Error : n is not a power of two for fast')
	return

20	n4pow = m/2

c scale the input by 2/n
	con = 2.0/real(n)
	do i=1,n
	   b(i) = b(i)*con
	enddo

c do a radix 2 iteration first if one is required.
	nn = 1
	if (m-n4pow*2 .le. 0) go to 50
	nn = 2
	int = n/nn
	call fr2tr(int, b(1), b(int+1))

c perform radix 4 iterations.
50	if (n4pow .eq. 0) go to 70

	do it=1,n4pow
	   nn = nn*4
	   int = n/nn
	   call fr4tr(int, nn, b(1), b(int+1), b(2*int+1), b(3*int+1),
     &                b(1), b(int+1), b(2*int+1), b(3*int+1))
	end do

c perform in-place reordering.
70	call ord1(m, b)
	call ford2(m, b)
	t = b(2)
	b(2) = 0.
	b(n+1) = t
	b(n+2) = 0.
	do it=3,n,4
	   b(it)   = -b(it)
	   b(it+1) = -b(it+1)
	end do

	return
	end

c-----------------------------------------------------------------------
c subroutine:  fr2tr		 radix 2 iteration subroutine
c-----------------------------------------------------------------------

	subroutine fr2tr(int, b0, b1)
	dimension b0(2), b1(2)

	do k=1,int
	   t     = b0(k) + b1(k)
	   b1(k) = b0(k) - b1(k)
	   b0(k) = t
	end do

	return
	end

c-----------------------------------------------------------------------
c subroutine:  fr4tr		 radix 4 iteration subroutine
c-----------------------------------------------------------------------

	subroutine fr4tr(int, nn, b0, b1, b2, b3, b4, b5, b6, b7)

	parameter(pii = 3.14159265358979,   ! pi
     &            pi8 = 0.39269908169872,   ! pi/8
     &            p7  = 0.70710678118655,   ! 1/sqrt(2)
     &            c22 = 0.92387953251129,   ! cos(pi/8)
     &            s22 = 0.38268343236509,   ! sin(pi/8)
     &            pi2 = 6.28318530717959,   ! 2 * pi
     &		  p7two = 1.41421356237310) ! sqrt(2)

	dimension l(15), b0(2),b1(2),b2(2),b3(2),b4(2),b5(2),b6(2),b7(2)
	equivalence (l15,l(1)), (l14,l(2)), (l13,l(3)), (l12,l(4)),
     *    (l11,l(5)), (l10,l(6)), (l9,l(7)), (l8,l(8)), (l7,l(9)),
     *    (l6,l(10)), (l5,l(11)), (l4,l(12)), (l3,l(13)), (l2,l(14)),
     *    (l1,l(15))

c jthet is a reversed binary counter, jr steps two at a time to
c locate the real parts of intermediate results, and ji locates
c the imaginary part corresponding to jr.

	l(1) = nn/4
	do 40 k=2,15
	   if (l(k-1)-2) 10, 20, 30
 10	   l(k-1) = 2
 20	   l(k) = 2
	   go to 40
 30	   l(k) = l(k-1)/2
 40	continue

	piovn = pii/float(nn)
	ji = 3
	jl = 2
	jr = 2

	do 120 j1=2,l1,2
	do 120 j2=j1,l2,l1
	do 120 j3=j2,l3,l2
	do 120 j4=j3,l4,l3
        do 120 j5=j4,l5,l4
        do 120 j6=j5,l6,l5
	do 120 j7=j6,l7,l6
	do 120 j8=j7,l8,l7
	do 120 j9=j8,l9,l8
	do 120 j10=j9,l10,l9
	do 120 j11=j10,l11,l10
	do 120 j12=j11,l12,l11
	do 120 j13=j12,l13,l12
	do 120 j14=j13,l14,l13
	do 120 jthet=j14,l15,l14
	   th2 = jthet - 2
	   if (th2) 50, 50, 90
 50	   do k=1,int
	      t0    = b0(k) + b2(k)
	      t1    = b1(k) + b3(k)
	      b2(k) = b0(k) - b2(k)
	      b3(k) = b1(k) - b3(k)
	      b0(k) = t0 + t1
	      b1(k) = t0 - t1
	   end do

	   if (nn-4) 120, 120, 70
 70	   k0 = int*4 + 1
	   kl = k0 + int - 1
	   do k=k0,kl
	      pr = p7*(b1(k)-b3(k))
	      pi = p7*(b1(k)+b3(k))
	      b3(k) = b2(k) + pi
	      b1(k) = pi - b2(k)
	      b2(k) = b0(k) - pr
	      b0(k) = b0(k) + pr
	   end do
	   go to 120

 90	   arg = th2*piovn
	   c1 = cos(arg)
	   s1 = sin(arg)
	   c2 = c1*c1 - s1*s1
	   s2 = c1*s1 + c1*s1
	   c3 = c1*c2 - s1*s2
	   s3 = c2*s1 + s2*c1
	   
	   int4 = int*4
	   j0 = jr*int4 + 1
	   k0 = ji*int4 + 1
	   jlast = j0 + int - 1
	   do j=j0,jlast
	      k = k0 + j - j0
	      r1 = b1(j)*c1 - b5(k)*s1
	      r5 = b1(j)*s1 + b5(k)*c1
	      t2 = b2(j)*c2 - b6(k)*s2
	      t6 = b2(j)*s2 + b6(k)*c2
	      t3 = b3(j)*c3 - b7(k)*s3
	      t7 = b3(j)*s3 + b7(k)*c3
	      t0 = b0(j) + t2
	      t4 = b4(k) + t6
	      t2 = b0(j) - t2
	      t6 = b4(k) - t6
	      t1 = r1 + t3
	      t5 = r5 + t7
	      t3 = r1 - t3
	      t7 = r5 - t7
	      b0(j) = t0 + t1
	      b7(k) = t4 + t5
	      b6(k) = t0 - t1
	      b1(j) = t5 - t4
	      b2(j) = t2 - t7
	      b5(k) = t6 + t3
	      b4(k) = t2 + t7
	      b3(j) = t3 - t6
	   end do

	   jr = jr + 2
	   ji = ji - 2
	   if (ji-jl) 110, 110, 120
 110	   ji = 2*jr - 1
	   jl = jr
 120	continue
	return
	end

c-----------------------------------------------------------------------
c subroutine:  ford2		 in-place reordering subroutine
c-----------------------------------------------------------------------

	subroutine ford2(m, b)
	dimension l(15), b(2)
	equivalence (l15,l(1)), (l14,l(2)), (l13,l(3)), (l12,l(4)),
     &    (l11,l(5)), (l10,l(6)), (l9,l(7)), (l8,l(8)), (l7,l(9)),
     &    (l6,l(10)), (l5,l(11)), (l4,l(12)), (l3,l(13)), (l2,l(14)),
     &    (l1,l(15))
	n = 2**m
	l(1) = n
	do k=2,m
	   l(k) = l(k-1)/2
	end do
	do k=m,14
	   l(k+1) = 2
	end do
	ij = 2
	do 40 j1=2,l1,2
	do 40 j2=j1,l2,l1
	do 40 j3=j2,l3,l2
	do 40 j4=j3,l4,l3
	do 40 j5=j4,l5,l4
	do 40 j6=j5,l6,l5
	do 40 j7=j6,l7,l6
	do 40 j8=j7,l8,l7
	do 40 j9=j8,l9,l8
	do 40 j10=j9,l10,l9
	do 40 j11=j10,l11,l10
	do 40 j12=j11,l12,l11
	do 40 j13=j12,l13,l12
	do 40 j14=j13,l14,l13
	do 40 ji=j14,l15,l14
	   if (ij-ji) 30, 40, 40
 30	   t = b(ij-1)
	   b(ij-1) = b(ji-1)
	   b(ji-1) = t
	   t = b(ij)
	   b(ij) = b(ji)
	   b(ji) = t
 40	   ij = ij + 2

	return
	end

c-----------------------------------------------------------------------
c subroutine:  fsst		 fourier synthesis subroutine
c-----------------------------------------------------------------------

c Revision 1.0  8/26/88 handling of nn changed near 50 to enable 8192 pts

	subroutine fsst(b, n)

c this subroutine synthesizes the real vector b(k), for
c k=1,2,...,n, from the fourier coefficients stored in the
c b array of size n+2.  the dc term is in b(1) with b(2) equal
c to  0.  the jth harmonic is stored as b(2*j+1) + i b(2*j+2).
c the n/2 harmonic is in b(n+1) with b(n+2) equal to 0.
c the subroutine is called as fsst(b,n) where n=2**m and
c b is the real array discussed above.

	real b(*)

	do i=1,15
	   m = i
	   nt = 2**i
	   if (n.eq.nt) go to 20
	end do
	call wrtstr(' Error :  n is not a power of two for fsst')
	return

 20	continue

c move origin
	do i=3,n,4
	   b(i) = -b(i)
	   b(i+1) = -b(i+1)
	end do
	b(2) = b(n+1)

c scale the input
	do i=1,n
	   b(i) = 0.5*b(i)
	end do
	n4pow = m/2

c scramble the inputs
	call ford2(m, b)
	call ord1(m, b)

	if (n4pow.eq.0) go to 60
	nn = n
	do it=1,n4pow
	   int = n/nn
	   call fr4syn(int, nn, b(1), b(int+1), b(2*int+1), b(3*int+1),
     *                 b(1), b(int+1), b(2*int+1), b(3*int+1))
	   nn = nn/4
	end do

c do a radix 2 iteration if one is required
 60	if (m-n4pow*2) 80, 80, 70
 70	int = n/2
	call fr2tr(int, b(1), b(int+1))

 80	return
	end

c-----------------------------------------------------------------------
c subroutine:  fr4syn		 radix 4 synthesis
c-----------------------------------------------------------------------


	subroutine fr4syn(int, nn, b0, b1, b2, b3, b4, b5, b6, b7)

	parameter(pii = 3.14159265358979,   ! pi
     &            pi8 = 0.39269908169872,   ! pi/8
     &            p7  = 0.70710678118655,   ! 1/sqrt(2)
     &            c22 = 0.92387953251129,   ! cos(pi/8)
     &            s22 = 0.38268343236509,   ! sin(pi/8)
     &            pi2 = 6.28318530717959,   ! 2 * pi
     &		  p7two = 1.41421356237310) ! sqrt(2)

	dimension l(15),b0(2),b1(2),b2(2),b3(2),b4(2),b5(2),b6(2),
     *    b7(2)
	equivalence (l15,l(1)), (l14,l(2)), (l13,l(3)), (l12,l(4)),
     *    (l11,l(5)), (l10,l(6)), (l9,l(7)), (l8,l(8)), (l7,l(9)),
     *    (l6,l(10)), (l5,l(11)), (l4,l(12)), (l3,l(13)), (l2,l(14)),
     *    (l1,l(15))

	l(1) = nn/4
	do 40 k=2,15
	   if (l(k-1)-2) 10, 20, 30
 10	   l(k-1) = 2
 20	   l(k) = 2
	   go to 40
 30	   l(k) = l(k-1)/2
 40	continue

	piovn = pii/float(nn)
	ji = 3
	jl = 2
	jr = 2

	do 120 j1=2,l1,2
	do 120 j2=j1,l2,l1
	do 120 j3=j2,l3,l2
	do 120 j4=j3,l4,l3
	do 120 j5=j4,l5,l4
	do 120 j6=j5,l6,l5
	do 120 j7=j6,l7,l6
	do 120 j8=j7,l8,l7
	do 120 j9=j8,l9,l8
	do 120 j10=j9,l10,l9
	do 120 j11=j10,l11,l10
	do 120 j12=j11,l12,l11
	do 120 j13=j12,l13,l12
	do 120 j14=j13,l14,l13
	do 120 jthet=j14,l15,l14
	   th2 = jthet - 2
	   if (th2) 50, 50, 90
 50	   do 60 k=1,int
	      t0 = b0(k) + b1(k)
	      t1 = b0(k) - b1(k)
	      t2 = b2(k)*2.0
	      t3 = b3(k)*2.0
	      b0(k) = t0 + t2
	      b2(k) = t0 - t2
	      b1(k) = t1 + t3
	      b3(k) = t1 - t3
 60	   continue

	   if (nn-4) 120, 120, 70
 70	   k0 = int*4 + 1
	   kl = k0 + int - 1
	   do 80 k=k0,kl
	      t2 = b0(k) - b2(k)
	      t3 = b1(k) + b3(k)
	      b0(k) = (b0(k)+b2(k))*2.0
	      b2(k) = (b3(k)-b1(k))*2.0
	      b1(k) = (t2+t3)*p7two
	      b3(k) = (t3-t2)*p7two
 80	   continue
	   go to 120
 90	   arg = th2*piovn
	   c1 = cos(arg)
	   s1 = -sin(arg)
	   c2 = c1*c1 - s1*s1
	   s2 = c1*s1 + c1*s1
	   c3 = c1*c2 - s1*s2
	   s3 = c2*s1 + s2*c1

	   int4 = int*4
	   j0 = jr*int4 + 1
	   k0 = ji*int4 + 1
	   jlast = j0 + int - 1
	   do 100 j=j0,jlast
	      k = k0 + j - j0
	      t0 = b0(j) + b6(k)
	      t1 = b7(k) - b1(j)
	      t2 = b0(j) - b6(k)
	      t3 = b7(k) + b1(j)
	      t4 = b2(j) + b4(k)
	      t5 = b5(k) - b3(j)
	      t6 = b5(k) + b3(j)
	      t7 = b4(k) - b2(j)
	      b0(j) = t0 + t4
	      b4(k) = t1 + t5
	      b1(j) = (t2+t6)*c1 - (t3+t7)*s1
	      b5(k) = (t2+t6)*s1 + (t3+t7)*c1
	      b2(j) = (t0-t4)*c2 - (t1-t5)*s2
	      b6(k) = (t0-t4)*s2 + (t1-t5)*c2
	      b3(j) = (t2-t6)*c3 - (t3-t7)*s3
	      b7(k) = (t2-t6)*s3 + (t3-t7)*c3
 100	   continue
	   jr = jr + 2
	   ji = ji - 2
	   if (ji-jl) 110, 110, 120
 110	   ji = 2*jr - 1
	   jl = jr
 120    continue

	return
	end

c ------------------------------------------------------------------
c Subroutine:  ffak              Fast fourier analysis subroutine
c-------------------------------------------------------------------
c  L. Delbouille, July 26, 1987; modified by JWB 26/10/92 4096k,
c  U. Griesmann, Dec 27, 1997 modified for up to 16M points
c-------------------------------------------------------------------

	subroutine ffak (b, nfft)

c This subroutine replaces the real vector b(k), (k=1,2,...,n), with 
c its finite discrete fourier transform.  The dc term is returned in 
c location b(1) with b(2) set to 0.  Thereafter, the jth harmonic is
c returned as a complex number stored as b(2*j+1) + i b(2*j+2). Note
c that the n/2 harmonic is returned in b(n+1) with b(n+2) set to 0. 
c Hence, b must be dimensioned to size n+2.
c The subroutine is called as ffak (b,n) where n=2**m and b is an
c n term real array.  A real-valued, radix 8  algorithm is used with
c in-place reordering and the trig functions are computed as needed.
c MODIFIED to move origin to nfft/2 + 1 instead of 1 *************

	real b(*)
	integer itrflag
	include 'iounit.h'

	external itrflag
	
	call itron        ! allow transform to be interrupted

	n = 1
	do i=1,24
	   m = i
	   n = n*2
	   if (n.eq.nfft) go to 20
	end do
	call wrtstr(' Error :  nfft not a power of 2 for ffak')
	return

 20	n8pow = m/3

c do a radix 2 or radix 4 iteration first if one is required
	if (m-n8pow*3-1) 50, 40, 30
 30	nn = 4
	int = n/nn
	call r4tr (int, b(1), b(int+1), b(2*int+1), b(3*int+1))
	go to 60
 40	nn = 2
	int = n/nn
	call fr2tr (int, b(1), b(int+1))
	go to 60
 50	nn = 1

 					! perform radix 8 iterations
 60	if (n8pow .gt. 0) then
	   if (n8pow .gt. 4) then 
	      write (wrtbuf,61) nfft,n8pow
	      call wrtstr( wrtbuf )
	   end if
 61	   format (' FFA: performing radix-8 iterations for',
     &                i8,' points (',i2,' iterations)' )
	   do it=1,n8pow
	      nn = nn*8
	      int = n/nn
	      call r8trk(int, nn, 
     &           b(1),      b(  int+1),b(2*int+1),b(3*int+1),
     &           b(4*int+1),b(5*int+1),b(6*int+1),b(7*int+1), 
     &           b(1),      b(  int+1),b(2*int+1),b(3*int+1),
     &           b(4*int+1),b(5*int+1),b(6*int+1),b(7*int+1))
	   end do
	end if

c perform in-place reordering
 90	call ord1(m, b)
	call ord2k(m, b)
	t = b(2)
	b(2)      = 0.0
	b(nfft+1) = t
	b(nfft+2) = 0.0

	do i = 3,nfft,4
	   b(i  ) = -b(i)
	   b(i+1) = -b(i+1)
	end do
	call itroff

	return
	end

c-----------------------------------------------------------------------
c subroutine:  r4tr                 radix 4 iteration subroutine
c-----------------------------------------------------------------------

	subroutine r4tr (int, b0, b1, b2, b3)

	dimension b0(*), b1(*), b2(*), b3(*)

	do k=1,int
	   r0 = b0(k) + b2(k)
	   r1 = b1(k) + b3(k)
	   b2(k) = b0(k) - b2(k)
	   b3(k) = b1(k) - b3(k)
	   b0(k) = r0 + r1
	   b1(k) = r0 - r1
	end do

	return
	end

c-----------------------------------------------------------------------
c subroutine: r8trk                   radix 8 iteration subroutine
c for a number of points greater than 32k, up to 16M points
c-----------------------------------------------------------------------

	subroutine r8trk (int,nn,br0,br1,br2,br3,br4,br5,br6,br7,
     &                           bi0,bi1,bi2,bi3,bi4,bi5,bi6,bi7)

	parameter(pii = 3.14159265358979,   ! pi
     &            pi8 = 0.39269908169872,   ! pi/8
     &            p7  = 0.70710678118655,   ! 1/sqrt(2)
     &            c22 = 0.92387953251129,   ! cos(pi/8)
     &            s22 = 0.38268343236509,   ! sin(pi/8)
     &            pi2 = 6.28318530717959,   ! 2 * pi
     &		  p7two = 1.41421356237310) ! sqrt(2)

	real br0(*),br1(*),br2(*),br3(*),br4(*),br5(*),br6(*),br7(*), 
     &       bi0(*),bi1(*),bi2(*),bi3(*),bi4(*),bi5(*),bi6(*),bi7(*)

	integer int, l(24),
     &          l1, l2, l3, l4, l5, l6, l7, l8, l9, l10,l11,l12,
     &          l13,l14,l15,l16,l17,l18,l19,l20,l21,l22,l23,l24

	equivalence 
     &    (l24,l(1)),  (l23,l(2)),  (l22,l(3)),  (l21,l(4)),
     &    (l20,l(5)),  (l19,l(6)),  (l18,l(7)),  (l17,l(8)), 
     &    (l16,l(9)),  (l15,l(10)), (l14,l(11)), (l13,l(12)), 
     &    (l12,l(13)), (l11,l(14)), (l10,l(15)), (l9,l(16)), 
     &    (l8,l(17)),  (l7,l(18)),  (l6,l(19)),  (l5,l(20)), 
     &    (l4,l(21)),  (l3,l(22)),  (l2,l(23)),  (l1,l(24))

c set up counters such that jthet steps through the arguments of w,
c jr steps through starting locations for the real part of the
c intermediate results and ji steps through starting locations
c of the imaginary part of the intermediate results.

	l(1) = nn/8
	do 40 k=2,24
	   if (l(k-1)-2) 10, 20, 30
 10	   l(k-1) = 2
 20	   l(k) = 2
	   go to 40
 30	   l(k) = l(k-1)/2
 40	continue

	piovn = pii/float(nn)
	ji = 3
	jl = 2
	jr = 2
	do 120 j1=2,l1,2
	do 120 j2=j1,l2,l1
	do 120 j3=j2,l3,l2
	do 120 j4=j3,l4,l3
	do 120 j5=j4,l5,l4
	do 120 j6=j5,l6,l5
	do 120 j7=j6,l7,l6
	do 120 j8=j7,l8,l7
	do 120 j9=j8,l9,l8
	do 120 j10=j9,l10,l9
	do 120 j11=j10,l11,l10
	do 120 j12=j11,l12,l11
	do 120 j13=j12,l13,l12
	do 120 j14=j13,l14,l13
	do 120 j15=j14,l15,l14
	do 120 j16=j15,l16,l15
	do 120 j17=j16,l17,l16
	do 120 j18=j17,l18,l17
	do 120 j19=j18,l19,l18
	do 120 j20=j19,l20,l19
	do 120 j21=j20,l21,l20
	do 120 j22=j21,l22,l21
	do 120 j23=j22,l23,l22
	do 120 jthet=j23,l24,l23
	   th2 = jthet - 2
	   if (th2 .gt. 0) go to 90
	   do k=1,int
	      t0 = br0(k) + br4(k)
	      t1 = br1(k) + br5(k)
	      t2 = br2(k) + br6(k)
	      t3 = br3(k) + br7(k)
	      t4 = br0(k) - br4(k)
	      t5 = br1(k) - br5(k)
	      t6 = br2(k) - br6(k)
	      t7 = br3(k) - br7(k)
	      br2(k) = t0 - t2
	      br3(k) = t1 - t3
	      t0 = t0 + t2
	      t1 = t1 + t3
	      br0(k) = t0 + t1
	      br1(k) = t0 - t1
	      pr = p7*(t5-t7)
	      pi = p7*(t5+t7)
	      br4(k) = t4 + pr
	      br7(k) = t6 + pi
	      br6(k) = t4 - pr
	      br5(k) = pi - t6
	   end do
	   if (nn-8 .le. 0) go to 120

	   k0 = int*8 + 1
	   kl = k0 + int - 1
	   do k=k0,kl
	      pr = p7*(bi2(k)-bi6(k))
	      pi = p7*(bi2(k)+bi6(k))
	      tr0 = bi0(k) + pr
	      ti0 = bi4(k) + pi
	      tr2 = bi0(k) - pr
	      ti2 = bi4(k) - pi
	      pr = p7*(bi3(k)-bi7(k))
	      pi = p7*(bi3(k)+bi7(k))
	      tr1 = bi1(k) + pr
	      ti1 = bi5(k) + pi
	      tr3 = bi1(k) - pr
	      ti3 = bi5(k) - pi
	      pr = tr1*c22 - ti1*s22
	      pi = ti1*c22 + tr1*s22
	      bi0(k) = tr0 + pr
	      bi6(k) = tr0 - pr
	      bi7(k) = ti0 + pi
	      bi1(k) = pi - ti0
	      pr = -tr3*s22 - ti3*c22
	      pi =  tr3*c22 - ti3*s22
	      bi2(k) = tr2 + pr
	      bi4(k) = tr2 - pr
	      bi5(k) = ti2 + pi
	      bi3(k) = pi - ti2
	   end do
	   go to 120
	   
 90	   arg = th2*piovn
	   c1 = cos(arg)
	   s1 = sin(arg)
	   c2 = c1*c1 - s1*s1
	   s2 = c1*s1 + c1*s1
	   c3 = c1*c2 - s1*s2
	   s3 = c2*s1 + s2*c1
	   c4 = c2*c2 - s2*s2
	   s4 = c2*s2 + c2*s2
	   c5 = c2*c3 - s2*s3
	   s5 = c3*s2 + s3*c2
	   c6 = c3*c3 - s3*s3
	   s6 = c3*s3 + c3*s3
	   c7 = c3*c4 - s3*s4
	   s7 = c4*s3 + s4*c3
	   int8 = int*8
	   j0 = jr*int8 + 1
	   k0 = ji*int8 + 1
	   jlast = j0 + int - 1
	   do j=j0,jlast
	      k = k0 + j - j0
	      tr1 = br1(j)*c1 - bi1(k)*s1
	      ti1 = br1(j)*s1 + bi1(k)*c1
	      tr2 = br2(j)*c2 - bi2(k)*s2
	      ti2 = br2(j)*s2 + bi2(k)*c2
	      tr3 = br3(j)*c3 - bi3(k)*s3
	      ti3 = br3(j)*s3 + bi3(k)*c3
	      tr4 = br4(j)*c4 - bi4(k)*s4
	      ti4 = br4(j)*s4 + bi4(k)*c4
	      tr5 = br5(j)*c5 - bi5(k)*s5
	      ti5 = br5(j)*s5 + bi5(k)*c5
	      tr6 = br6(j)*c6 - bi6(k)*s6
	      ti6 = br6(j)*s6 + bi6(k)*c6
	      tr7 = br7(j)*c7 - bi7(k)*s7
	      ti7 = br7(j)*s7 + bi7(k)*c7
	      t0 =  br0(j) + tr4
	      t1 =  bi0(k) + ti4
	      tr4 = br0(j) - tr4
	      ti4 = bi0(k) - ti4
	      t2 =  tr1 + tr5
	      t3 =  ti1 + ti5
	      tr5 = tr1 - tr5
	      ti5 = ti1 - ti5
	      t4 =  tr2 + tr6
	      t5 =  ti2 + ti6
	      tr6 = tr2 - tr6
	      ti6 = ti2 - ti6
	      t6 =  tr3 + tr7
	      t7 =  ti3 + ti7
	      tr7 = tr3 - tr7
	      ti7 = ti3 - ti7	      
	      tr0 = t0 + t4
	      ti0 = t1 + t5
	      tr2 = t0 - t4
	      ti2 = t1 - t5
	      tr1 = t2 + t6
	      ti1 = t3 + t7
	      tr3 = t2 - t6
	      ti3 = t3 - t7
	      t0 = tr4 - ti6
	      t1 = ti4 + tr6
	      t4 = tr4 + ti6
	      t5 = ti4 - tr6
	      t2 = tr5 - ti7
	      t3 = ti5 + tr7
	      t6 = tr5 + ti7
	      t7 = ti5 - tr7
	      br0(j) = tr0 + tr1
	      bi7(k) = ti0 + ti1
	      bi6(k) = tr0 - tr1
	      br1(j) = ti1 - ti0
	      br2(j) = tr2 - ti3
	      bi5(k) = ti2 + tr3
	      bi4(k) = tr2 + ti3
	      br3(j) = tr3 - ti2
	      pr = p7*(t2-t3)
	      pi = p7*(t2+t3)
	      br4(j) = t0 + pr
	      bi3(k) = t1 + pi
	      bi2(k) = t0 - pr
	      br5(j) = pi - t1
	      pr = -p7*(t6+t7)
	      pi =  p7*(t6-t7)
	      br6(j) = t4 + pr
	      bi1(k) = t5 + pi
	      bi0(k) = t4 - pr
	      br7(j) = pi - t5
	   end do
	   jr = jr + 2
	   ji = ji - 2
	   if (ji-jl .gt. 0) go to 120
	   ji = 2*jr - 1
	   jl = jr
 120    continue
	
	return
	end
c-----------------------------------------------------------------------
c subroutine:  ord1
c-----------------------------------------------------------------------

	subroutine ord1 (m,b)
	dimension b(*)

	k = 4
	kl = 2
	n = 2**m
	do 40 j=4,n,2
	  if (k-j) 20, 20, 10
 10	  t = b(j)
	  b(j) = b(k)
	  b(k) = t
 20	  k = k - 2
	  if (k-kl) 30, 30, 40
 30	  k = 2*j
	  kl = j
 40	continue

	return
	end
c-----------------------------------------------------------------------
c subroutine:  ord2k
c in-place reordering subroutine, for up to 16M points
c-----------------------------------------------------------------------

	subroutine ord2k (m, b)
	real b(*)
	integer l(24),l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,
     &          l13,l14,l15,l16,l17,l18,l19,l20,l21,l22,l23,l24
	equivalence 
     &    (l24,l(1)),  (l23,l(2)),  (l22,l(3)),  (l21,l(4)),
     &    (l20,l(5)),  (l19,l(6)),  (l18,l(7)),  (l17,l(8)), 
     &    (l16,l(9)),  (l15,l(10)), (l14,l(11)), (l13,l(12)), 
     &    (l12,l(13)), (l11,l(14)), (l10,l(15)), (l9,l(16)), 
     &    (l8,l(17)),  (l7,l(18)),  (l6,l(19)),  (l5,l(20)), 
     &    (l4,l(21)),  (l3,l(22)),  (l2,l(23)),  (l1,l(24))

	n = 2**m
	l(1) = n
	do k=2,m
	  l(k) = l(k-1)/2
  	end do
	do k=m,23
	  l(k+1) = 2
  	end do
	ij = 2

	do 40 j1=2,l1,2
	do 40 j2=j1,l2,l1
	do 40 j3=j2,l3,l2
	do 40 j4=j3,l4,l3
	do 40 j5=j4,l5,l4
	do 40 j6=j5,l6,l5
	do 40 j7=j6,l7,l6
	do 40 j8=j7,l8,l7
	do 40 j9=j8,l9,l8
	do 40 j10=j9,l10,l9
	do 40 j11=j10,l11,l10
	do 40 j12=j11,l12,l11
	do 40 j13=j12,l13,l12
	do 40 j14=j13,l14,l13
	do 40 j15=j14,l15,l14
	do 40 j16=j15,l16,l15
	do 40 j17=j16,l17,l16
	do 40 j18=j17,l18,l17
	do 40 j19=j18,l19,l18
	do 40 j20=j19,l20,l19
	do 40 j21=j20,l21,l20
	do 40 j22=j21,l22,l21
	do 40 j23=j22,l23,l22
	do 40 ji=j23,l24,l23
	   if (ij-ji) 30, 40, 40
 30	   t = b(ij-1)
	   b(ij-1) = b(ji-1)
	   b(ji-1) = t
	   t = b(ij)
	   b(ij) = b(ji)
	   b(ji) = t
 40	   ij = ij + 2
	return
	end

c-----------------------------------------------------------------------
c subroutine:  ffsk
c fast fourier synthesis subroutine, radix 8-4-2
c modified by JWB 26/10/92 for 2**22 points
c modified by ulf griesmann 27/12/97 for 2*24 points
c-----------------------------------------------------------------------

	subroutine ffsk(b, nfft)

c This subroutine synthesizes the real vector b(k), where k=1,2,...,n. 
c The initial fourier coefficients are placed in the b array of size n+2.  
c The dc term is in b(1) with b(2) equal to 0.
c The jth harmonic is stored as b(2*j+1) + i b(2*j+2).
c The n/2 harmonic is in b(n+1) with b(n+2) equal to 0.
c The subroutine is called as ffsk(b,n) where n=2**m and b is the n term 
c real array discussed above.

	real b(*)
	include 'iounit.h'

	n = 1
	do i=1,24
	   m = i
	   n = n*2
	   if (n.eq.nfft) go to 20
	end do
	call wrtstr('nfft not a power of 2 < 25 for ffsk')
	stop

20	continue
	b(2) = b(nfft+1)
	con = 1.0/nfft
	do i=1,nfft
	   b(i) = b(i)*con
	end do

c move origin
	do i=3,nfft,4
	   b(i)   = -b(i)
	   b(i+1) = -b(i+1)
	end do
	n8pow = m/3

c reorder the input Fourier coefficients
	call ord2k(m, b)
	call ord1(m, b)

c perform the radix 8 iterations
	if (n8pow .gt. 0) then
	   if (n8pow .gt. 4) then
	      write (wrtbuf,61) nfft,n8pow
	      call wrtstr(wrtbuf)
	   end if
 61	   format (' FFS: performing radix-8 iterations for',
     &             i8,' points (',i2,' iterations)')
	   nn = n
	   do it=1,n8pow
	      int = n/nn
	      call r8synk(int, nn, 
     &           b(1),      b(  int+1),b(2*int+1),b(3*int+1),
     &           b(4*int+1),b(5*int+1),b(6*int+1),b(7*int+1), 
     &           b(1),      b(  int+1),b(2*int+1),b(3*int+1),
     &           b(4*int+1),b(5*int+1),b(6*int+1),b(7*int+1))
	      nn = nn/8
	   end do
	end if

c do a radix 2 or radix 4 iteration if one is required
	if (m-n8pow*3-1) 90, 80, 70
 70	int = n/4
	call r4syn(int, b(1), b(int+1), b(2*int+1), b(3*int+1))
	go to 90
 80	int = n/2
	call fr2tr(int, b(1), b(int+1))

 90	return
	end

c-----------------------------------------------------------------------
c subroutine:  r8synk
c radix 8 synthesis subroutine
c-----------------------------------------------------------------------
	subroutine r8synk(int,nn,br0,br1,br2,br3,br4,br5,br6,br7,
     &                           bi0,bi1,bi2,bi3,bi4,bi5,bi6,bi7)

	parameter(pii = 3.14159265358979,   ! pi
     &            pi8 = 0.39269908169872,   ! pi/8
     &            p7  = 0.70710678118655,   ! 1/sqrt(2)
     &            c22 = 0.92387953251129,   ! cos(pi/8)
     &            s22 = 0.38268343236509,   ! sin(pi/8)
     &            pi2 = 6.28318530717959,   ! 2 * pi
     &		  p7two = 1.41421356237310) ! sqrt(2)

	real br0(*),br1(*),br2(*),br3(*),br4(*),br5(*),br6(*),br7(*), 
     &       bi0(*),bi1(*),bi2(*),bi3(*),bi4(*),bi5(*),bi6(*),bi7(*)

	integer int, l(24),
     &          l1, l2, l3, l4, l5, l6, l7, l8, l9, l10,l11,l12,
     &          l13,l14,l15,l16,l17,l18,l19,l20,l21,l22,l23,l24

	equivalence 
     &    (l24,l(1)),  (l23,l(2)),  (l22,l(3)),  (l21,l(4)),
     &    (l20,l(5)),  (l19,l(6)),  (l18,l(7)),  (l17,l(8)), 
     &    (l16,l(9)),  (l15,l(10)), (l14,l(11)), (l13,l(12)), 
     &    (l12,l(13)), (l11,l(14)), (l10,l(15)), (l9,l(16)), 
     &    (l8,l(17)),  (l7,l(18)),  (l6,l(19)),  (l5,l(20)), 
     &    (l4,l(21)),  (l3,l(22)),  (l2,l(23)),  (l1,l(24))

	l(1) = nn/8
	do 40 k=2,24
	   if (l(k-1)-2) 10, 20, 30
 10	   l(k-1) = 2
 20	   l(k) = 2
	   go to 40
 30	   l(k) = l(k-1)/2
 40	continue

	piovn = pii/float(nn)
	ji = 3
	jl = 2
	jr = 2

	do 120 j1=2,l1,2
	do 120 j2=j1,l2,l1
	do 120 j3=j2,l3,l2
	do 120 j4=j3,l4,l3
	do 120 j5=j4,l5,l4
	do 120 j6=j5,l6,l5
	do 120 j7=j6,l7,l6
	do 120 j8=j7,l8,l7
	do 120 j9=j8,l9,l8
	do 120 j10=j9,l10,l9
	do 120 j11=j10,l11,l10
	do 120 j12=j11,l12,l11
	do 120 j13=j12,l13,l12
	do 120 j14=j13,l14,l13
	do 120 j15=j14,l15,l14
	do 120 j16=j15,l16,l15
	do 120 j17=j16,l17,l16
	do 120 j18=j17,l18,l17
	do 120 j19=j18,l19,l18
	do 120 j20=j19,l20,l19
	do 120 j21=j20,l21,l20
	do 120 j22=j21,l22,l21
	do 120 j23=j22,l23,l22
	do 120 jthet=j23,l24,l23
	   th2 = jthet - 2
	   if (th2) 50, 50, 90
 50	   do k=1,int
	      t0 = br0(k) + br1(k)
	      t1 = br0(k) - br1(k)
	      t2 = br2(k) + br2(k)
	      t3 = br3(k) + br3(k)
	      t4 = br4(k) + br6(k)
	      t6 = br7(k) - br5(k)
	      t5 = br4(k) - br6(k)
	      t7 = br7(k) + br5(k)
	      pr = p7*(t7+t5)
	      pi = p7*(t7-t5)
	      tt0 = t0 + t2
	      tt1 = t1 + t3
	      t2 = t0 - t2
	      t3 = t1 - t3
	      t4 = t4 + t4
	      t5 = pr + pr
	      t6 = t6 + t6
	      t7 = pi + pi
	      br0(k) = tt0 + t4
	      br1(k) = tt1 + t5
	      br2(k) = t2 + t6
	      br3(k) = t3 + t7
	      br4(k) = tt0 - t4
	      br5(k) = tt1 - t5
	      br6(k) = t2 - t6
	      br7(k) = t3 - t7
	   end do

	   if (nn-8) 120, 120, 70
 70	   k0 = int*8 + 1
	   kl = k0 + int - 1
	   do k=k0,kl
	      t1 = bi0(k) + bi6(k)
	      t2 = bi7(k) - bi1(k)
	      t3 = bi0(k) - bi6(k)
	      t4 = bi7(k) + bi1(k)
	      pr = t3*c22 + t4*s22
	      pi = t4*c22 - t3*s22
	      t5 = bi2(k) + bi4(k)
	      t6 = bi5(k) - bi3(k)
	      t7 = bi2(k) - bi4(k)
	      t8 = bi5(k) + bi3(k)
	      rr = t8*c22 - t7*s22
	      ri = -t8*s22 - t7*c22
	      bi0(k) = (t1+t5) + (t1+t5)
	      bi4(k) = (t2+t6) + (t2+t6)
	      bi1(k) = (pr+rr) + (pr+rr)
	      bi5(k) = (pi+ri) + (pi+ri)
	      t5 = t1 - t5
	      t6 = t2 - t6
	      bi2(k) = p7two*(t6+t5)
	      bi6(k) = p7two*(t6-t5)
	      rr = pr - rr
	      ri = pi - ri
	      bi3(k) = p7two*(ri+rr)
	      bi7(k) = p7two*(ri-rr)
	   end do
	   go to 120

 90	   arg = th2*piovn
	   c1 = cos(arg)
	   s1 = -sin(arg)
	   c2 = c1*c1 - s1*s1
	   s2 = c1*s1 + c1*s1
	   c3 = c1*c2 - s1*s2
	   s3 = c2*s1 + s2*c1
	   c4 = c2*c2 - s2*s2
	   s4 = c2*s2 + c2*s2
	   c5 = c2*c3 - s2*s3
	   s5 = c3*s2 + s3*c2
	   c6 = c3*c3 - s3*s3
	   s6 = c3*s3 + c3*s3
	   c7 = c3*c4 - s3*s4
	   s7 = c4*s3 + s4*c3
	   int8 = int*8
	   j0 = jr*int8 + 1
	   k0 = ji*int8 + 1
	   jlast = j0 + int - 1
	   do j=j0,jlast
	      k = k0 + j - j0
	      tr0 = br0(j) + bi6(k)
	      ti0 = bi7(k) - br1(j)
	      tr1 = br0(j) - bi6(k)
	      ti1 = bi7(k) + br1(j)
	      tr2 = br2(j) + bi4(k)
	      ti2 = bi5(k) - br3(j)
	      tr3 = bi5(k) + br3(j)
	      ti3 = bi4(k) - br2(j)
	      tr4 = br4(j) + bi2(k)
	      ti4 = bi3(k) - br5(j)
	      t0  = br4(j) - bi2(k)
	      t1  = bi3(k) + br5(j)
	      tr5 = p7*(t0+t1)
	      ti5 = p7*(t1-t0)
	      tr6 = br6(j) + bi0(k)
	      ti6 = bi1(k) - br7(j)
	      t0  = br6(j) - bi0(k)
	      t1  = bi1(k) + br7(j)
	      tr7 = -p7*(t0-t1)
	      ti7 = -p7*(t1+t0)
	      t0  = tr0 + tr2
	      t1  = ti0 + ti2
	      t2  = tr1 + tr3
	      t3  = ti1 + ti3
	      tr2 = tr0 - tr2
	      ti2 = ti0 - ti2
	      tr3 = tr1 - tr3
	      ti3 = ti1 - ti3
	      t4  = tr4 + tr6
	      t5  = ti4 + ti6
	      t6  = tr5 + tr7
	      t7  = ti5 + ti7
	      ttr6= ti4 - ti6
	      ti6 = tr6 - tr4
	      ttr7= ti5 - ti7
	      ti7 = tr7 - tr5
	      br0(j) = t0 + t4
	      bi0(k) = t1 + t5
	      br1(j) = c1*(t2+t6)    - s1*(t3+t7)
	      bi1(k) = c1*(t3+t7)    + s1*(t2+t6)
	      br2(j) = c2*(tr2+ttr6) - s2*(ti2+ti6)
	      bi2(k) = c2*(ti2+ti6)  + s2*(tr2+ttr6)
	      br3(j) = c3*(tr3+ttr7) - s3*(ti3+ti7)
	      bi3(k) = c3*(ti3+ti7)  + s3*(tr3+ttr7)
	      br4(j) = c4*(t0-t4)    - s4*(t1-t5)
	      bi4(k) = c4*(t1-t5)    + s4*(t0-t4)
	      br5(j) = c5*(t2-t6)    - s5*(t3-t7)
	      bi5(k) = c5*(t3-t7)    + s5*(t2-t6)
	      br6(j) = c6*(tr2-ttr6) - s6*(ti2-ti6)
	      bi6(k) = c6*(ti2-ti6)  + s6*(tr2-ttr6)
	      br7(j) = c7*(tr3-ttr7) - s7*(ti3-ti7)
	      bi7(k) = c7*(ti3-ti7)  + s7*(tr3-ttr7)
	   end do
	   jr = jr + 2
	   ji = ji - 2
	   if (ji-jl) 110, 110, 120
 110	   ji = 2*jr - 1
	   jl = jr
 120    continue

	return
	end

c-----------------------------------------------------------------------
c subroutine:  r4syn
c radix 4 synthesis
c-----------------------------------------------------------------------

	subroutine r4syn(int, b0, b1, b2, b3)
	dimension b0(*), b1(*), b2(*), b3(*)
	do k=1,int
	   t0 = b0(k) + b1(k)
	   t1 = b0(k) - b1(k)
	   t2 = b2(k) + b2(k)
	   t3 = b3(k) + b3(k)
	   b0(k) = t0 + t2
	   b2(k) = t0 - t2
	   b1(k) = t1 + t3
	   b3(k) = t1 - t3
	end do

	return
	end
c-----------------------------------------------------------------------
