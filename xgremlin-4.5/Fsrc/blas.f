**************************************************************************
* A BLAS SUBSET
**************************************************************************

*DECK SAXPY
      subroutine saxpy (n, sa, sx, incx, sy, incy)
C***BEGIN PROLOGUE  SAXPY
C***PURPOSE  Compute a constant times a vector plus a vector.
C***LIBRARY   SLATEC (BLAS)
C***CATEGORY  D1A7
C***TYPE      SINGLE PRECISION (SAXPY-S, DAXPY-D, CAXPY-C)
C***KEYWORDS  BLAS, LINEAR ALGEBRA, TRIAD, VECTOR
C***AUTHOR  Lawson, C. L., (JPL)
C           Hanson, R. J., (SNLA)
C           Kincaid, D. R., (U. of Texas)
C           Krogh, F. T., (JPL)
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       SA  single precision scalar multiplier
C       SX  single precision vector with N elements
C     INCX  storage spacing between elements of SX
C       SY  single precision vector with N elements
C     INCY  storage spacing between elements of SY
C
C     --Output--
C       SY  single precision result (unchanged if N .LE. 0)
C
C     Overwrite single precision SY with single precision SA*SX +SY.
C     For I = 0 to N-1, replace  SY(LY+I*INCY) with SA*SX(LX+I*INCX) +
C       SY(LY+I*INCY),
C     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
C     defined in a similar way using INCY.
C
C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
C                 Krogh, Basic linear algebra subprograms for Fortran
C                 usage, Algorithm No. 539, Transactions on Mathematical
C                 Software 5, 3 (September 1979), pp. 308-323.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   791001  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920310  Corrected definition of LX in DESCRIPTION.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  SAXPY
      real sx(*), sy(*), sa
C***FIRST EXECUTABLE STATEMENT  SAXPY
      if (n.le.0 .or. sa.eq.0.0e0) return
      if (incx .eq. incy) if (incx-1) 5,20,60
C
C     Code for unequal or nonpositive increments.
C
    5 ix = 1
      iy = 1
      if (incx .lt. 0) ix = (-n+1)*incx + 1
      if (incy .lt. 0) iy = (-n+1)*incy + 1
      do 10 i = 1,n
        sy(iy) = sy(iy) + sa*sx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
C
C     Code for both increments equal to 1.
C
C     Clean-up loop so remaining vector length is a multiple of 4.
C
   20 m = mod(n,4)
      if (m .eq. 0) go to 40
      do 30 i = 1,m
        sy(i) = sy(i) + sa*sx(i)
   30 continue
      if (n .lt. 4) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        sy(i) = sy(i) + sa*sx(i)
        sy(i+1) = sy(i+1) + sa*sx(i+1)
        sy(i+2) = sy(i+2) + sa*sx(i+2)
        sy(i+3) = sy(i+3) + sa*sx(i+3)
   50 continue
      return
C
C     Code for equal, positive, non-unit increments.
C
   60 ns = n*incx
      do 70 i = 1,ns,incx
        sy(i) = sa*sx(i) + sy(i)
   70 continue
      return
      end

**************************************************************************

*DECK SDOT
      real function sdot (n, sx, incx, sy, incy)
C***BEGIN PROLOGUE  SDOT
C***PURPOSE  Compute the inner product of two vectors.
C***LIBRARY   SLATEC (BLAS)
C***CATEGORY  D1A4
C***TYPE      SINGLE PRECISION (SDOT-S, DDOT-D, CDOTU-C)
C***KEYWORDS  BLAS, INNER PRODUCT, LINEAR ALGEBRA, VECTOR
C***AUTHOR  Lawson, C. L., (JPL)
C           Hanson, R. J., (SNLA)
C           Kincaid, D. R., (U. of Texas)
C           Krogh, F. T., (JPL)
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       SX  single precision vector with N elements
C     INCX  storage spacing between elements of SX
C       SY  single precision vector with N elements
C     INCY  storage spacing between elements of SY
C
C     --Output--
C     SDOT  single precision dot product (zero if N .LE. 0)
C
C     Returns the dot product of single precision SX and SY.
C     SDOT = sum for I = 0 to N-1 of  SX(LX+I*INCX) * SY(LY+I*INCY),
C     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
C     defined in a similar way using INCY.
C
C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
C                 Krogh, Basic linear algebra subprograms for Fortran
C                 usage, Algorithm No. 539, Transactions on Mathematical
C                 Software 5, 3 (September 1979), pp. 308-323.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   791001  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920310  Corrected definition of LX in DESCRIPTION.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  SDOT
      real sx(*), sy(*)
C***FIRST EXECUTABLE STATEMENT  SDOT
      sdot = 0.0e0
      if (n .le. 0) return
      if (incx .eq. incy) if (incx-1) 5,20,60
C
C     Code for unequal or nonpositive increments.
C
    5 ix = 1
      iy = 1
      if (incx .lt. 0) ix = (-n+1)*incx + 1
      if (incy .lt. 0) iy = (-n+1)*incy + 1
      do 10 i = 1,n
        sdot = sdot + sx(ix)*sy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
C
C     Code for both increments equal to 1.
C
C     Clean-up loop so remaining vector length is a multiple of 5.
C
   20 m = mod(n,5)
      if (m .eq. 0) go to 40
      do 30 i = 1,m
        sdot = sdot + sx(i)*sy(i)
   30 continue
      if (n .lt. 5) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
      sdot = sdot + sx(i)*sy(i) + sx(i+1)*sy(i+1) + sx(i+2)*sy(i+2) +
     1              sx(i+3)*sy(i+3) + sx(i+4)*sy(i+4)
   50 continue
      return
C
C     Code for equal, positive, non-unit increments.
C
   60 ns = n*incx
      do 70 i = 1,ns,incx
        sdot = sdot + sx(i)*sy(i)
   70 continue
      return
      end

**************************************************************************

*DECK SNRM2
      real function snrm2 (n, sx, incx)
C***BEGIN PROLOGUE  SNRM2
C***PURPOSE  Compute the Euclidean length (L2 norm) of a vector.
C***LIBRARY   SLATEC (BLAS)
C***CATEGORY  D1A3B
C***TYPE      SINGLE PRECISION (SNRM2-S, DNRM2-D, SCNRM2-C)
C***KEYWORDS  BLAS, EUCLIDEAN LENGTH, EUCLIDEAN NORM, L2,
C             LINEAR ALGEBRA, UNITARY, VECTOR
C***AUTHOR  Lawson, C. L., (JPL)
C           Hanson, R. J., (SNLA)
C           Kincaid, D. R., (U. of Texas)
C           Krogh, F. T., (JPL)
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       SX  single precision vector with N elements
C     INCX  storage spacing between elements of SX
C
C     --Output--
C    SNRM2  single precision result (zero if N .LE. 0)
C
C     Euclidean norm of the N-vector stored in SX with storage
C     increment INCX .
C     If N .LE. 0, return with result = 0.
C     If N .GE. 1, then INCX must be .GE. 1
C
C     Four Phase Method using two built-in constants that are
C     hopefully applicable to all machines.
C         CUTLO = maximum of  SQRT(U/EPS)  over all known machines.
C         CUTHI = minimum of  SQRT(V)      over all known machines.
C     where
C         EPS = smallest no. such that EPS + 1. .GT. 1.
C         U   = smallest positive no.   (underflow limit)
C         V   = largest  no.            (overflow  limit)
C
C     Brief Outline of Algorithm.
C
C     Phase 1 scans zero components.
C     Move to phase 2 when a component is nonzero and .LE. CUTLO
C     Move to phase 3 when a component is .GT. CUTLO
C     Move to phase 4 when a component is .GE. CUTHI/M
C     where M = N for X() real and M = 2*N for complex.
C
C     Values for CUTLO and CUTHI.
C     From the environmental parameters listed in the IMSL converter
C     document the limiting values are as follows:
C     CUTLO, S.P.   U/EPS = 2**(-102) for  Honeywell.  Close seconds are
C                   Univac and DEC at 2**(-103)
C                   Thus CUTLO = 2**(-51) = 4.44089E-16
C     CUTHI, S.P.   V = 2**127 for Univac, Honeywell, and DEC.
C                   Thus CUTHI = 2**(63.5) = 1.30438E19
C     CUTLO, D.P.   U/EPS = 2**(-67) for Honeywell and DEC.
C                   Thus CUTLO = 2**(-33.5) = 8.23181D-11
C     CUTHI, D.P.   same as S.P.  CUTHI = 1.30438D19
C     DATA CUTLO, CUTHI /8.232D-11,  1.304D19/
C     DATA CUTLO, CUTHI /4.441E-16,  1.304E19/
C
C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
C                 Krogh, Basic linear algebra subprograms for Fortran
C                 usage, Algorithm No. 539, Transactions on Mathematical
C                 Software 5, 3 (September 1979), pp. 308-323.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   791001  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  SNRM2
      integer next
      real sx(*), cutlo, cuthi, hitest, sum, xmax, zero, one
      save cutlo, cuthi, zero, one
      data zero, one /0.0e0, 1.0e0/
C
      data cutlo, cuthi /4.441e-16,  1.304e19/
C***FIRST EXECUTABLE STATEMENT  SNRM2
      if (n .gt. 0) go to 10
         snrm2  = zero
         go to 300
C
   10 assign 30 to next
      sum = zero
      nn = n * incx
C
C                                                 BEGIN MAIN LOOP
C
      i = 1
   20    go to next,(30, 50, 70, 110)
   30 if (abs(sx(i)) .gt. cutlo) go to 85
      assign 50 to next
      xmax = zero
C
C                        PHASE 1.  SUM IS ZERO
C
   50 if (sx(i) .eq. zero) go to 200
      if (abs(sx(i)) .gt. cutlo) go to 85
C
C                                PREPARE FOR PHASE 2.
C
      assign 70 to next
      go to 105
C
C                                PREPARE FOR PHASE 4.
C
  100 i = j
      assign 110 to next
      sum = (sum / sx(i)) / sx(i)
  105 xmax = abs(sx(i))
      go to 115
C
C                   PHASE 2.  SUM IS SMALL.
C                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
C
   70 if (abs(sx(i)) .gt. cutlo) go to 75
C
C                     COMMON CODE FOR PHASES 2 AND 4.
C                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
C
  110 if (abs(sx(i)) .le. xmax) go to 115
         sum = one + sum * (xmax / sx(i))**2
         xmax = abs(sx(i))
         go to 200
C
  115 sum = sum + (sx(i)/xmax)**2
      go to 200
C
C                  PREPARE FOR PHASE 3.
C
   75 sum = (sum * xmax) * xmax
C
C     FOR REAL OR D.P. SET HITEST = CUTHI/N
C     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
C
   85 hitest = cuthi / n
C
C                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
C
      do 95 j = i,nn,incx
      if (abs(sx(j)) .ge. hitest) go to 100
   95    sum = sum + sx(j)**2
      snrm2 = sqrt( sum )
      go to 300
C
  200 continue
      i = i + incx
      if (i .le. nn) go to 20
C
C              END OF MAIN LOOP.
C
C              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
C
      snrm2 = xmax * sqrt(sum)
  300 continue
      return
      end

**************************************************************************

*DECK SROT
      subroutine srot (n, sx, incx, sy, incy, sc, ss)
C***BEGIN PROLOGUE  SROT
C***PURPOSE  Apply a plane Givens rotation.
C***LIBRARY   SLATEC (BLAS)
C***CATEGORY  D1A8
C***TYPE      SINGLE PRECISION (SROT-S, DROT-D, CSROT-C)
C***KEYWORDS  BLAS, GIVENS ROTATION, GIVENS TRANSFORMATION,
C             LINEAR ALGEBRA, PLANE ROTATION, VECTOR
C***AUTHOR  Lawson, C. L., (JPL)
C           Hanson, R. J., (SNLA)
C           Kincaid, D. R., (U. of Texas)
C           Krogh, F. T., (JPL)
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       SX  single precision vector with N elements
C     INCX  storage spacing between elements of SX
C       SY  single precision vector with N elements
C     INCY  storage spacing between elements of SY
C       SC  element of rotation matrix
C       SS  element of rotation matrix
C
C     --Output--
C       SX  rotated vector SX (unchanged if N .LE. 0)
C       SY  rotated vector SY (unchanged if N .LE. 0)
C
C     Multiply the 2 x 2 matrix  ( SC SS) times the 2 x N matrix (SX**T)
C                                (-SS SC)                        (SY**T)
C     where **T indicates transpose.  The elements of SX are in
C     SX(LX+I*INCX), I = 0 to N-1, where LX = 1 if INCX .GE. 0, else
C     LX = 1+(1-N)*INCX, and similarly for SY using LY and INCY.
C
C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
C                 Krogh, Basic linear algebra subprograms for Fortran
C                 usage, Algorithm No. 539, Transactions on Mathematical
C                 Software 5, 3 (September 1979), pp. 308-323.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   791001  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920310  Corrected definition of LX in DESCRIPTION.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  SROT
      real sx, sy, sc, ss, zero, one, w, z
      dimension sx(*), sy(*)
      save zero, one
      data zero, one /0.0e0, 1.0e0/
C***FIRST EXECUTABLE STATEMENT  SROT
      if (n .le. 0 .or. (ss .eq. zero .and. sc .eq. one)) go to 40
      if (.not. (incx .eq. incy .and. incx .gt. 0)) go to 20
C
C          Code for equal and positive increments.
C
           nsteps=incx*n
           do 10 i = 1,nsteps,incx
                w=sx(i)
                z=sy(i)
                sx(i)=sc*w+ss*z
                sy(i)=-ss*w+sc*z
   10           continue
           go to 40
C
C     Code for unequal or nonpositive increments.
C
   20 continue
           kx=1
           ky=1
C
           if (incx .lt. 0) kx = 1-(n-1)*incx
           if (incy .lt. 0) ky = 1-(n-1)*incy
C
           do 30 i = 1,n
                w=sx(kx)
                z=sy(ky)
                sx(kx)=sc*w+ss*z
                sy(ky)=-ss*w+sc*z
                kx=kx+incx
                ky=ky+incy
   30           continue
   40 continue
C
      return
      end

**************************************************************************

*DECK SROTG
      subroutine srotg (sa, sb, sc, ss)
C***BEGIN PROLOGUE  SROTG
C***PURPOSE  Construct a plane Givens rotation.
C***LIBRARY   SLATEC (BLAS)
C***CATEGORY  D1B10
C***TYPE      SINGLE PRECISION (SROTG-S, DROTG-D, CROTG-C)
C***KEYWORDS  BLAS, GIVENS ROTATION, GIVENS TRANSFORMATION,
C             LINEAR ALGEBRA, VECTOR
C***AUTHOR  Lawson, C. L., (JPL)
C           Hanson, R. J., (SNLA)
C           Kincaid, D. R., (U. of Texas)
C           Krogh, F. T., (JPL)
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C       SA  single precision scalar
C       SB  single precision scalar
C
C     --Output--
C       SA  single precision result R
C       SB  single precision result Z
C       SC  single precision result
C       SS  single precision result
C
C     Construct the Givens transformation
C
C         ( SC  SS )
C     G = (        ) ,    SC**2 + SS**2 = 1 ,
C         (-SS  SC )
C
C     which zeros the second entry of the 2-vector  (SA,SB)**T.
C
C     The quantity R = (+/-)SQRT(SA**2 + SB**2) overwrites SA in
C     storage.  The value of SB is overwritten by a value Z which
C     allows SC and SS to be recovered by the following algorithm:
C
C           If Z=1  set  SC=0.0  and  SS=1.0
C           If ABS(Z) .LT. 1  set  SC=SQRT(1-Z**2)  and  SS=Z
C           If ABS(Z) .GT. 1  set  SC=1/Z  and  SS=SQRT(1-SC**2)
C
C     Normally, the subprogram SROT(N,SX,INCX,SY,INCY,SC,SS) will
C     next be called to apply the transformation to a 2 by N matrix.
C
C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
C                 Krogh, Basic linear algebra subprograms for Fortran
C                 usage, Algorithm No. 539, Transactions on Mathematical
C                 Software 5, 3 (September 1979), pp. 308-323.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   791001  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  SROTG
C***FIRST EXECUTABLE STATEMENT  SROTG
      if (abs(sa) .le. abs(sb)) go to 10
C
C *** HERE ABS(SA) .GT. ABS(SB) ***
C
      u = sa + sa
      v = sb / u
C
C     NOTE THAT U AND R HAVE THE SIGN OF SA
C
      r = sqrt(0.25e0 + v**2) * u
C
C     NOTE THAT SC IS POSITIVE
C
      sc = sa / r
      ss = v * (sc + sc)
      sb = ss
      sa = r
      return
C
C *** HERE ABS(SA) .LE. ABS(SB) ***
C
   10 if (sb .eq. 0.0e0) go to 20
      u = sb + sb
      v = sa / u
C
C     NOTE THAT U AND R HAVE THE SIGN OF SB
C     (R IS IMMEDIATELY STORED IN SA)
C
      sa = sqrt(0.25e0 + v**2) * u
C
C     NOTE THAT SS IS POSITIVE
C
      ss = sb / sa
      sc = v * (ss + ss)
      if (sc .eq. 0.0e0) go to 15
      sb = 1.0e0 / sc
      return
   15 sb = 1.0e0
      return
C
C *** HERE SA = SB = 0.0 ***
C
   20 sc = 1.0e0
      ss = 0.0e0
      return
C
      end

**************************************************************************

*DECK SSCAL
      subroutine sscal (n, sa, sx, incx)
C***BEGIN PROLOGUE  SSCAL
C***PURPOSE  Multiply a vector by a constant.
C***LIBRARY   SLATEC (BLAS)
C***CATEGORY  D1A6
C***TYPE      SINGLE PRECISION (SSCAL-S, DSCAL-D, CSCAL-C)
C***KEYWORDS  BLAS, LINEAR ALGEBRA, SCALE, VECTOR
C***AUTHOR  Lawson, C. L., (JPL)
C           Hanson, R. J., (SNLA)
C           Kincaid, D. R., (U. of Texas)
C           Krogh, F. T., (JPL)
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       SA  single precision scale factor
C       SX  single precision vector with N elements
C     INCX  storage spacing between elements of SX
C
C     --Output--
C       SX  single precision result (unchanged if N .LE. 0)
C
C     Replace single precision SX by single precision SA*SX.
C     For I = 0 to N-1, replace SX(IX+I*INCX) with  SA * SX(IX+I*INCX),
C     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX.
C
C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
C                 Krogh, Basic linear algebra subprograms for Fortran
C                 usage, Algorithm No. 539, Transactions on Mathematical
C                 Software 5, 3 (September 1979), pp. 308-323.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   791001  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900821  Modified to correct problem with a negative increment.
C           (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  SSCAL
      real sa, sx(*)
      integer i, incx, ix, m, mp1, n
C***FIRST EXECUTABLE STATEMENT  SSCAL
      if (n .le. 0) return
      if (incx .eq. 1) goto 20
C
C     Code for increment not equal to 1.
C
      ix = 1
      if (incx .lt. 0) ix = (-n+1)*incx + 1
      do 10 i = 1,n
        sx(ix) = sa*sx(ix)
        ix = ix + incx
   10 continue
      return
C
C     Code for increment equal to 1.
C
C     Clean-up loop so remaining vector length is a multiple of 5.
C
   20 m = mod(n,5)
      if (m .eq. 0) goto 40
      do 30 i = 1,m
        sx(i) = sa*sx(i)
   30 continue
      if (n .lt. 5) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        sx(i) = sa*sx(i)
        sx(i+1) = sa*sx(i+1)
        sx(i+2) = sa*sx(i+2)
        sx(i+3) = sa*sx(i+3)
        sx(i+4) = sa*sx(i+4)
   50 continue
      return
      end

**************************************************************************

*DECK SSWAP
      subroutine sswap (n, sx, incx, sy, incy)
C***BEGIN PROLOGUE  SSWAP
C***PURPOSE  Interchange two vectors.
C***LIBRARY   SLATEC (BLAS)
C***CATEGORY  D1A5
C***TYPE      SINGLE PRECISION (SSWAP-S, DSWAP-D, CSWAP-C, ISWAP-I)
C***KEYWORDS  BLAS, INTERCHANGE, LINEAR ALGEBRA, VECTOR
C***AUTHOR  Lawson, C. L., (JPL)
C           Hanson, R. J., (SNLA)
C           Kincaid, D. R., (U. of Texas)
C           Krogh, F. T., (JPL)
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       SX  single precision vector with N elements
C     INCX  storage spacing between elements of SX
C       SY  single precision vector with N elements
C     INCY  storage spacing between elements of SY
C
C     --Output--
C       SX  input vector SY (unchanged if N .LE. 0)
C       SY  input vector SX (unchanged if N .LE. 0)
C
C     Interchange single precision SX and single precision SY.
C     For I = 0 to N-1, interchange  SX(LX+I*INCX) and SY(LY+I*INCY),
C     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
C     defined in a similar way using INCY.
C
C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
C                 Krogh, Basic linear algebra subprograms for Fortran
C                 usage, Algorithm No. 539, Transactions on Mathematical
C                 Software 5, 3 (September 1979), pp. 308-323.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   791001  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920310  Corrected definition of LX in DESCRIPTION.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  SSWAP
      real sx(*), sy(*), stemp1, stemp2, stemp3
C***FIRST EXECUTABLE STATEMENT  SSWAP
      if (n .le. 0) return
      if (incx .eq. incy) if (incx-1) 5,20,60
C
C     Code for unequal or nonpositive increments.
C
    5 ix = 1
      iy = 1
      if (incx .lt. 0) ix = (-n+1)*incx + 1
      if (incy .lt. 0) iy = (-n+1)*incy + 1
      do 10 i = 1,n
        stemp1 = sx(ix)
        sx(ix) = sy(iy)
        sy(iy) = stemp1
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
C
C     Code for both increments equal to 1.
C
C     Clean-up loop so remaining vector length is a multiple of 3.
C
   20 m = mod(n,3)
      if (m .eq. 0) go to 40
      do 30 i = 1,m
        stemp1 = sx(i)
        sx(i) = sy(i)
        sy(i) = stemp1
   30 continue
      if (n .lt. 3) return
   40 mp1 = m + 1
      do 50 i = mp1,n,3
        stemp1 = sx(i)
        stemp2 = sx(i+1)
        stemp3 = sx(i+2)
        sx(i) = sy(i)
        sx(i+1) = sy(i+1)
        sx(i+2) = sy(i+2)
        sy(i) = stemp1
        sy(i+1) = stemp2
        sy(i+2) = stemp3
   50 continue
      return
C
C     Code for equal, positive, non-unit increments.
C
   60 ns = n*incx
      do 70 i = 1,ns,incx
        stemp1 = sx(i)
        sx(i) = sy(i)
        sy(i) = stemp1
   70 continue
      return
      end

*DECK SCOPY
      subroutine scopy (n, sx, incx, sy, incy)
C***BEGIN PROLOGUE  SCOPY
C***PURPOSE  Copy a vector.
C***LIBRARY   SLATEC (BLAS)
C***CATEGORY  D1A5
C***TYPE      SINGLE PRECISION (SCOPY-S, DCOPY-D, CCOPY-C, ICOPY-I)
C***KEYWORDS  BLAS, COPY, LINEAR ALGEBRA, VECTOR
C***AUTHOR  Lawson, C. L., (JPL)
C           Hanson, R. J., (SNLA)
C           Kincaid, D. R., (U. of Texas)
C           Krogh, F. T., (JPL)
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       SX  single precision vector with N elements
C     INCX  storage spacing between elements of SX
C       SY  single precision vector with N elements
C     INCY  storage spacing between elements of SY
C
C     --Output--
C       SY  copy of vector SX (unchanged if N .LE. 0)
C
C     Copy single precision SX to single precision SY.
C     For I = 0 to N-1, copy  SX(LX+I*INCX) to SY(LY+I*INCY),
C     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
C     defined in a similar way using INCY.
C
C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
C                 Krogh, Basic linear algebra subprograms for Fortran
C                 usage, Algorithm No. 539, Transactions on Mathematical
C                 Software 5, 3 (September 1979), pp. 308-323.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   791001  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920310  Corrected definition of LX in DESCRIPTION.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  SCOPY
      real sx(*), sy(*)
C***FIRST EXECUTABLE STATEMENT  SCOPY
      if (n .le. 0) return
      if (incx .eq. incy) if (incx-1) 5,20,60
C
C     Code for unequal or nonpositive increments.
C
    5 ix = 1
      iy = 1
      if (incx .lt. 0) ix = (-n+1)*incx + 1
      if (incy .lt. 0) iy = (-n+1)*incy + 1
      do 10 i = 1,n
        sy(iy) = sx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
C
C     Code for both increments equal to 1.
C
C     Clean-up loop so remaining vector length is a multiple of 7.
C
   20 m = mod(n,7)
      if (m .eq. 0) go to 40
      do 30 i = 1,m
        sy(i) = sx(i)
   30 continue
      if (n .lt. 7) return
   40 mp1 = m + 1
      do 50 i = mp1,n,7
        sy(i) = sx(i)
        sy(i+1) = sx(i+1)
        sy(i+2) = sx(i+2)
        sy(i+3) = sx(i+3)
        sy(i+4) = sx(i+4)
        sy(i+5) = sx(i+5)
        sy(i+6) = sx(i+6)
   50 continue
      return
C
C     Code for equal, positive, non-unit increments.
C
   60 ns = n*incx
      do 70 i = 1,ns,incx
        sy(i) = sx(i)
   70 continue
      return
      end
