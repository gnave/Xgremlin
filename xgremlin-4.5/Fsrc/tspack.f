*
* a subset of the TSPACK library
*
      subroutine tspss (n,x,y,per,unifrm,w,sm,smtol,lwk, wk,
     .                  sigma,ys,yp, nit,ier)
      integer n, lwk, nit, ier
      logical per, unifrm
      real    x(n), y(n), w(n), sm, smtol, wk(lwk), sigma(n),
     .        ys(n), yp(n)
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   03/17/91
C
C   This subroutine computes a set of parameter values which
C define a smoothing tension spline H(x).  The parameters
C consist of knot values YS and derivatives YP computed
C by Subroutine SMCRV, and tension factors SIGMA computed by
C Subroutine SIGS (unless UNIFRM = TRUE).  The Hermite
C interpolatory tension spline H(x) defined by the knot
C values and derivatives has two continuous derivatives and
C satisfies either natural or periodic end conditions.
C
C   The tension spline may be evaluated by Subroutine TSVAL1
C or Functions HVAL (values), HPVAL (first derivatives),
C HPPVAL (second derivatives), and TSINTL (integrals).
C
C On input:
C
C       N = Number of data points.  N .GE. 2 and N .GE. 3 if
C           PER = TRUE.
C
C       X = Array of length N containing a strictly in-
C           creasing sequence of abscissae:  X(I) < X(I+1)
C           for I = 1,...,N-1.
C
C       Y = Array of length N containing data values asso-
C           ciated with the abscissae.  If PER = TRUE, it is
C           assumed that Y(N) = Y(1).
C
C       PER = Logical variable with value TRUE if and only
C             H(x) is to be a periodic function with period
C             X(N)-X(1).  It is assumed without a test that
C             Y(N) = Y(1) in this case.  On output, YP(N) =
C             YP(1) and, more generally, the values and
C             first two derivatives of H at X(1) agree with
C             those at X(N).  If H(x) is one of the compo-
C             nents of a parametric curve, this option may
C             be used to obtained a closed curve.  If PER =
C             FALSE, H satisfies natural end conditions:
C             zero second derivatives at X(1) and X(N).
C
C       UNIFRM = Logical variable with value TRUE if and
C                only if constant (uniform) tension is to be
C                used.  The tension factor must be input in
C                SIGMA(1) in this case and must be in the
C                range 0 to 85.  If SIGMA(1) = 0, H(x) is
C                a cubic spline, and as SIGMA increases,
C                H(x) approaches piecewise linear.  If
C                UNIFRM = FALSE, tension factors are chosen
C                (by SIGS) to preserve local monotonicity
C                and convexity of the data.  This may re-
C                sult in a better fit than the case of
C                uniform tension, but requires an iteration
C                on calls to SMCRV and SIGS.
C
C       W = Array of length N containing positive weights
C           associated with the data values.  The recommend-
C           ed value of W(I) is 1/DY**2, where DY is the
C           standard deviation associated with Y(I).  If
C           nothing is known about the errors in Y, a con-
C           stant (estimated value) should be used for DY.
C           If PER = TRUE, it is assumed that W(N) = W(1).
C
C       SM = Positive parameter specifying an upper bound on
C            Q2(YS), where Q2(YS) is the weighted sum of
C            squares of deviations from the data (differ-
C            ences between YS and Y).  H(x) is linear (and
C            Q2 is minimized) if SM is sufficiently large
C            that the constraint is not active.  It is
C            recommended that SM satisfy N-SQRT(2N) .LE. SM
C            .LE. N+SQRT(2N) and SM = N is reasonable if
C            W(I) = 1/DY**2.
C
C       SMTOL = Parameter in the range (0,1) specifying the
C               relative error allowed in satisfying the
C               constraint:  the constraint is assumed to
C               be satisfied if SM*(1-SMTOL) .LE. Q2 .LE.
C               SM*(1+SMTOL).  A reasonable value for SMTOL
C               is SQRT(2/N).
C
C       LWK = Length of work space WK:
C             LWK .GE. 6N   if PER=FALSE  and  UNIFRM=TRUE
C             LWK .GE. 7N   if PER=FALSE  and  UNIFRM=FALSE
C             LWK .GE. 10N  if PER=TRUE   and  UNIFRM=TRUE
C             LWK .GE. 11N  if PER=TRUE   and  UNIFRM=FALSE
C
C The above parameters are not altered by this routine.
C
C       WK = Array of length at least LWK to be used as
C            temporary work space.
C
C       SIGMA = Array of length .GE. N-1 containing a ten-
C               sion factor (0 to 85) in the first position
C               if UNIFRM = TRUE.
C
C       YS = Array of length .GE. N.
C
C       YP = Array of length .GE. N.
C
C On output:
C
C       WK = Array containing convergence parameters in the
C            first two locations if NIT > 0:
C            WK(1) = Maximum relative change in a component
C                    of YS on the last iteration.
C            WK(2) = Maximum relative change in a component
C                    of SIGMA on the last iteration.
C
C       SIGMA = Array containing tension factors.  SIGMA(I)
C               is associated with interval (X(I),X(I+1))
C               for I = 1,...,N-1.  SIGMA is not altered if
C               N is invalid or -4 < IER < -1, and SIGMA is
C               constant if IER = -1 (and N is valid) or
C               IER = -4.
C
C       YS = Array of length N containing values of H at the
C            abscissae.  YS(N) = YS(1) if PER = TRUE.  YS is
C            not altered if IER < 0.
C
C       YP = Array of length N containing first derivative
C            values of H at the abscissae.  YP(N) = YP(1)
C            if PER = TRUE.  YP is not altered if IER < 0.
C
C       NIT = Number of iterations (calls to SIGS).  NIT = 0
C             if IER < 0 or UNIFRM = TRUE.  If NIT > 0,
C             NIT+1 calls to SMCRV were employed.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered and the
C                     constraint is active:  Q2(YS) is ap-
C                     proximately equal to SM.
C             IER = 1 if no errors were encountered but the
C                     constraint is not active:  YS and YP
C                     are the values and derivatives of the
C                     linear function (constant function if
C                     PERIOD = TRUE) which minimizes Q2, and
C                     Q1 = 0 (refer to SMCRV).
C             IER = -1 if N, W, SM, or SMTOL is outside its
C                      valid range.
C             IER = -2 if LWK is too small.
C             IER = -3 if UNIFRM = TRUE and SIGMA(1) is out-
C                      side its valid range.
C             IER = -4 if the abscissae X are not strictly
C                      increasing.
C
C Modules required by TSPSS:  B2TRI or B2TRIP, SIGS, SMCRV,
C                               SNHCSH, STORE, YPCOEF
C
C Intrinsic functions called by TSPSS:  ABS, AMAX1
C
C***********************************************************
C
      data sbig/85./
C
C Convergence parameters:
C
C   STOL = Absolute tolerance for SIGS
C   MAXIT = Maximum number of SMCRV/SIGS iterations
C   DYSTOL = Bound on the maximum relative change in a
C              component of YS defining convergence of
C              the SMCRV/SIGS iteration when UNIFRM = FALSE
C
      data stol/0./,  maxit/99/,  dystol/.01/
C
C Initialize NIT, and test for invalid input parameters LWK
C   and SIGMA(1).
C
      nit = 0
      nn = n
      nm1 = nn - 1
      if (nn .lt. 2  .or.  (per  .and.  nn .lt. 3)) go to 11
      if (unifrm) then
        if ( lwk .lt. 6*nn  .or.
     .       (per  .and.  lwk .lt. 10*nn) ) go to 12
        sig = sigma(1)
        if (sig .lt. 0.  .or.  sig .gt. sbig) go to 13
      else
        if ( lwk .lt. 7*nn  .or.
     .       (per  .and.  lwk .lt. 11*nn) ) go to 12
        sig = 0.
      endif
C
C Store uniform tension factors, or initialize SIGMA to
C   zeros.
C
      do 1 i = 1,nm1
        sigma(i) = sig
    1   continue
C
C Compute smoothing curve for uniform tension.
C
      call smcrv (nn,x,y,sigma,per,w,sm,smtol,wk, ys,yp,ier)
      if (ier .le. -2) ier = -4
      if (ier .lt. 0  .or.  unifrm) return
C
C   Iterate on calls to SIGS and SMCRV.  The first N-1 WK
C     locations are used to store the function values YS
C     from the previous iteration.
C
C   DYS is the maximum relative change in a component of YS.
C   ICNT is the number of tension factors which were
C        increased by SIGS.
C   DSMAX is the maximum relative change in a component of
C         SIGMA.
C
      do 4 iter = 1,maxit
        dys = 0.
        do 2 i = 2,nm1
          wk(i) = ys(i)
    2     continue
        call sigs (nn,x,y,yp,stol, sigma, dsmax,icnt)
        call smcrv (nn,x,y,sigma,per,w,sm,smtol,wk(nn), ys,
     .              yp,ierr)
        do 3 i = 2,nm1
          e = abs(ys(i)-wk(i))
          if (wk(i) .ne. 0.) e = e/abs(wk(i))
          dys = amax1(dys,e)
    3     continue
        if (icnt .eq. 0  .or.  dys .le. dystol) go to 5
    4   continue
      iter = maxit
C
C No error encountered.
C
    5 wk(1) = dys
      wk(2) = dsmax
      nit = iter
      ier = ierr
      return
C
C Invalid input parameter N, W, SM, or SMTOL.
C
   11 ier = -1
      return
C
C LWK too small.
C
   12 ier = -2
      return
C
C UNIFRM = TRUE and SIGMA(1) outside its valid range.
C
   13 ier = -3
      return
      end

      subroutine sigs (n,x,y,yp,tol, sigma, dsmax,ier)
      integer n, ier
      real    x(n), y(n), yp(n), tol, sigma(n), dsmax
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   05/15/91
C
C   Given a set of abscissae X with associated data values Y
C and derivatives YP, this subroutine determines the small-
C est (nonnegative) tension factors SIGMA such that the Her-
C mite interpolatory tension spline H(x) preserves local
C shape properties of the data.  In an interval (X1,X2) with
C data values Y1,Y2 and derivatives YP1,YP2, the properties
C of the data are
C
C       Monotonicity:  S, YP1, and YP2 are nonnegative or
C                        nonpositive,
C  and
C       Convexity:     YP1 .LE. S .LE. YP2  or  YP1 .GE. S
C                        .GE. YP2,
C
C where S = (Y2-Y1)/(X2-X1).  The corresponding properties
C of H are constant sign of the first and second deriva-
C tives, respectively.  Note that, unless YP1 = S = YP2, in-
C finite tension is required (and H is linear on the inter-
C val) if S = 0 in the case of monotonicity, or if YP1 = S
C or YP2 = S in the case of convexity.
C
C   SIGS may be used in conjunction with Subroutine YPC2
C (or YPC2P) in order to produce a C-2 interpolant which
C preserves the shape properties of the data.  This is
C achieved by calling YPC2 with SIGMA initialized to the
C zero vector, and then alternating calls to SIGS with
C calls to YPC2 until the change in SIGMA is small (refer to
C the parameter descriptions for SIGMA, DSMAX and IER), or
C the maximum relative change in YP is bounded by a toler-
C ance (a reasonable value is .01).  A similar procedure may
C be used to produce a C-2 shape-preserving smoothing curve
C (Subroutine SMCRV).
C
C   Refer to Subroutine SIGBI for a means of selecting mini-
C mum tension factors to satisfy more general constraints.
C
C On input:
C
C       N = Number of data points.  N .GE. 2.
C
C       X = Array of length N containing a strictly in-
C           creasing sequence of abscissae:  X(I) < X(I+1)
C           for I = 1,...,N-1.
C
C       Y = Array of length N containing data values (or
C           function values computed by SMCRV) associated
C           with the abscissae.  H(X(I)) = Y(I) for I =
C           1,...,N.
C
C       YP = Array of length N containing first derivatives
C            of H at the abscissae.  Refer to Subroutines
C            YPC1, YPC1P, YPC2, YPC2P, and SMCRV.
C
C       TOL = Tolerance whose magnitude determines how close
C             each tension factor is to its optimal value
C             when nonzero finite tension is necessary and
C             sufficient to satisfy the constraint:
C             abs(TOL) is an upper bound on the magnitude
C             of the smallest (nonnegative) or largest (non-
C             positive) value of the first or second deriva-
C             tive of H in the interval.  Thus, the con-
C             straint is satisfied, but possibly with more
C             tension than necessary.  TOL should be set to
C             0 for optimal tension.
C
C The above parameters are not altered by this routine.
C
C       SIGMA = Array of length N-1 containing minimum val-
C               ues of the tension factors.  SIGMA(I) is as-
C               sociated with interval (I,I+1) and SIGMA(I)
C               .GE. 0 for I = 1,...,N-1.  SIGMA should be
C               set to the zero vector if minimal tension
C               is desired, and should be unchanged from a
C               previous call in order to ensure convergence
C               of the C-2 iterative procedure.
C
C On output:
C
C       SIGMA = Array containing tension factors for which
C               H(x) preserves the properties of the data,
C               with the restriction that SIGMA(I) .LE. 85
C               for all I (unless the input value is larger).
C               The factors are as small as possible (within
C               the tolerance), but not less than their
C               input values.  If infinite tension is re-
C               quired in interval (X(I),X(I+1)), then
C               SIGMA(I) = 85 (and H is an approximation to
C               the linear interpolant on the interval),
C               and if neither property is satisfied by the
C               data, then SIGMA(I) = 0 (unless the input
C               value is positive), and thus H is cubic in
C               the interval.
C
C       DSMAX = Maximum increase in a component of SIGMA
C               from its input value.  The increase is a
C               relative change if the input value is
C               nonzero, and an absolute change otherwise.
C
C       IER = Error indicator and information flag:
C             IER = I if no errors were encountered and I
C                     components of SIGMA were altered from
C                     their input values for 0 .LE. I .LE.
C                     N-1.
C             IER = -1 if N < 2.  SIGMA is not altered in
C                      this case.
C             IER = -I if X(I) .LE. X(I-1) for some I in the
C                      range 2,...,N.  SIGMA(J-1) is unal-
C                      tered for J = I,...,N in this case.
C
C Modules required by SIGS:  SNHCSH, STORE
C
C Intrinsic functions called by SIGS:  ABS, AMAX1, AMIN1,
C                                        EXP, SIGN, SQRT
C
C***********************************************************
C
      data sbig/85./,  lun/-1/
      nm1 = n - 1
      if (nm1 .lt. 1) go to 9
C
C Compute an absolute tolerance FTOL = abs(TOL) and a
C   relative tolerance RTOL = 100*MACHEPS.
C
      ftol = abs(tol)
      rtol = 1.
    1 rtol = rtol/2.
        if (store(rtol+1.) .gt. 1.) go to 1
      rtol = rtol*200.
C
C Initialize change counter ICNT and maximum change DSM for
C   loop on intervals.
C
      icnt = 0
      dsm = 0.
      do 8 i = 1,nm1
        if (lun .ge. 0) write (lun,100) i
  100   format (//1x,'sigs -- interval',i4)
        ip1 = i + 1
        dx = x(ip1) - x(i)
        if (dx .le. 0.) go to 10
        sigin = sigma(i)
        if (sigin .ge. sbig) go to 8
C
C Compute first and second differences.
C
        s1 = yp(i)
        s2 = yp(ip1)
        s = (y(ip1)-y(i))/dx
        d1 = s - s1
        d2 = s2 - s
        d1d2 = d1*d2
C
C Test for infinite tension required to satisfy either
C   property.
C
        sig = sbig
        if ((d1d2 .eq. 0.  .and.  s1 .ne. s2)  .or.
     .      (s .eq. 0.  .and.  s1*s2 .gt. 0.)) go to 7
C
C Test for SIGMA = 0 sufficient.  The data satisfies convex-
C   ity iff D1D2 .GE. 0, and D1D2 = 0 implies S1 = S = S2.
C
        sig = 0.
        if (d1d2 .lt. 0.) go to 3
        if (d1d2 .eq. 0.) go to 7
        t = amax1(d1/d2,d2/d1)
        if (t .le. 2.) go to 7
        tp1 = t + 1.
C
C Convexity:  Find a zero of F(SIG) = SIG*COSHM(SIG)/
C   SINHM(SIG) - TP1.
C
C   F(0) = 2-T < 0, F(TP1) .GE. 0, the derivative of F
C     vanishes at SIG = 0, and the second derivative of F is
C     .2 at SIG = 0.  A quadratic approximation is used to
C     obtain a starting point for the Newton method.
C
        sig = sqrt(10.*t-20.)
        nit = 0
C
C   Top of loop:
C
    2   if (sig .le. .5) then
          call tssnhcsh (sig, sinhm,coshm,coshmm)
          t1 = coshm/sinhm
          fp = t1 + sig*(sig/sinhm - t1*t1 + 1.)
        else
C
C   Scale SINHM and COSHM by 2*EXP(-SIG) in order to avoid
C     overflow with large SIG.
C
          ems = exp(-sig)
          ssm = 1. - ems*(ems+sig+sig)
          t1 = (1.-ems)*(1.-ems)/ssm
          fp = t1 + sig*(2.*sig*ems/ssm - t1*t1 + 1.)
        endif
C
        f = sig*t1 - tp1
        if (lun .ge. 0) write (lun,110) sig, f, fp
  110   format (5x,'convexity -- sig = ',e15.8,
     .          ', f(sig) = ',e15.8/1x,35x,'fp(sig) = ',
     .          e15.8)
        nit = nit + 1
C
C   Test for convergence.
C
        if (fp .eq. 0.) go to 7
        dsig = -f/fp
        if (abs(dsig) .le. rtol*sig  .or.  (f .ge. 0.  .and.
     .      f .le. ftol)  .or.  abs(f) .le. rtol) go to 7
C
C   Update SIG.
C
        sig = sig + dsig
        go to 2
C
C Convexity cannot be satisfied.  Monotonicity can be satis-
C   fied iff S1*S .GE. 0 and S2*S .GE. 0 since S .NE. 0.
C
    3   if (s1*s .lt. 0.  .or.  s2*s .lt. 0.) go to 7
        t0 = 3.*s - s1 - s2
        d0 = t0*t0 - s1*s2
C
C SIGMA = 0 is sufficient for monotonicity iff S*T0 .GE. 0
C   or D0 .LE. 0.
C
        if (d0 .le. 0.  .or.  s*t0 .ge. 0.) go to 7
C
C Monotonicity:  find a zero of F(SIG) = SIGN(S)*HP(R),
C   where HPP(R) = 0 and HP, HPP denote derivatives of H.
C   F has a unique zero, F(0) < 0, and F approaches abs(S)
C   as SIG increases.
C
C   Initialize parameters for the secant method.  The method
C     uses three points:  (SG0,F0), (SIG,F), and
C     (SNEG,FNEG), where SG0 and SNEG are defined implicitly
C     by DSIG = SIG - SG0 and DMAX = SIG - SNEG.
C
        sgn = sign(1.,s)
        sig = sbig
        fmax = sgn*(sig*s-s1-s2)/(sig-2.)
        if (fmax .le. 0.) go to 7
        stol = rtol*sig
        f = fmax
        f0 = sgn*d0/(3.*(d1-d2))
        fneg = f0
        dsig = sig
        dmax = sig
        d1pd2 = d1 + d2
        nit = 0
C
C   Top of loop:  compute the change in SIG by linear
C     interpolation.
C
    4   dsig = -f*dsig/(f-f0)
        if (lun .ge. 0) write (lun,120) dsig
  120   format (5x,'monotonicity -- dsig = ',e15.8)
        if (abs(dsig) .gt. abs(dmax)) go to 6
C
C   Restrict the step-size such that abs(DSIG) .GE. STOL/2.
C     Note that DSIG and DMAX have opposite signs.
C
        if (abs(dsig) .lt. stol/2.) dsig = -sign(stol/2.,
     .                              dmax)
C
C   Update SIG, F0, and F.
C
        sig = sig + dsig
        f0 = f
        if (sig .le. .5) then
C
C   Use approximations to the hyperbolic functions designed
C     to avoid cancellation error with small SIG.
C
          call tssnhcsh (sig, sinhm,coshm,coshmm)
          c1 = sig*coshm*d2 - sinhm*d1pd2
          c2 = sig*(sinhm+sig)*d2 - coshm*d1pd2
          a = c2 - c1
          e = sig*sinhm - coshmm - coshmm
        else
C
C   Scale SINHM and COSHM by 2*EXP(-SIG) in order to avoid
C     overflow with large SIG.
C
          ems = exp(-sig)
          ems2 = ems + ems
          tm = 1. - ems
          ssinh = tm*(1.+ems)
          ssm = ssinh - sig*ems2
          scm = tm*tm
          c1 = sig*scm*d2 - ssm*d1pd2
          c2 = sig*ssinh*d2 - scm*d1pd2
C
C   R is in (0,1) and well-defined iff HPP(X1)*HPP(X2) < 0.
C
          f = fmax
          if (c1*(sig*scm*d1 - ssm*d1pd2) .ge. 0.) go to 5
          a = ems2*(sig*tm*d2 + (tm-sig)*d1pd2)
          if (a*(c2+c1) .lt. 0.) go to 5
          e = sig*ssinh - scm - scm
        endif
C
        f = (sgn*(e*s2-c2) + sqrt(a*(c2+c1)))/e
C
C   Update number of iterations NIT.
C
    5   nit = nit + 1
        if (lun .ge. 0) write (lun,130) nit, sig, f
  130   format (1x,10x,i2,' -- sig = ',e15.8,', f = ',
     .          e15.8)
C
C   Test for convergence.
C
        stol = rtol*sig
        if ( abs(dmax) .le. stol  .or.  (f .ge. 0.  .and.
     .      f .le. ftol)  .or.  abs(f) .le. rtol ) go to 7
        dmax = dmax + dsig
        if ( f0*f .gt. 0.  .and.  abs(f) .ge. abs(f0) )
     .     go to 6
        if (f0*f .le. 0.) then
C
C   F and F0 have opposite signs.  Update (SNEG,FNEG) to
C     (SG0,F0) so that F and FNEG always have opposite
C     signs.  If SIG is closer to SNEG than SG0 and abs(F) <
C     abs(FNEG), then swap (SNEG,FNEG) with (SG0,F0).
C
          t1 = dmax
          t2 = fneg
          dmax = dsig
          fneg = f0
          if ( abs(dsig) .gt. abs(t1)  .and.
     .         abs(f) .lt. abs(t2)         ) then
C
            dsig = t1
            f0 = t2
          endif
        endif
        go to 4
C
C   Bottom of loop:  F0*F > 0 and the new estimate would
C     be outside of the bracketing interval of length
C     abs(DMAX).  Reset (SG0,F0) to (SNEG,FNEG).
C
    6   dsig = dmax
        f0 = fneg
        go to 4
C
C  Update SIGMA(I), ICNT, and DSM if necessary.
C
    7   sig = amin1(sig,sbig)
        if (sig .gt. sigin) then
          sigma(i) = sig
          icnt = icnt + 1
          dsig = sig-sigin
          if (sigin .gt. 0.) dsig = dsig/sigin
          dsm = amax1(dsm,dsig)
        endif
    8   continue
C
C No errors encountered.
C
      dsmax = dsm
      ier = icnt
      return
C
C N < 2.
C
    9 dsmax = 0.
      ier = -1
      return
C
C X(I+1) .LE. X(I).
C
   10 dsmax = dsm
      ier = -ip1
      return
      end

      subroutine tssnhcsh (x, sinhm,coshm,coshmm)
      real x, sinhm, coshm, coshmm
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/01/90
C
C   This subroutine computes approximations to the modified
C hyperbolic functions defined below with relative error
C bounded by 4.7E-12 for a floating point number system with
C sufficient precision.  For IBM 370 single precision, the
C relative error was found to be bounded by 2.1E-4 for all
C x, and 6.0E-6 for abs(x) .LE. .5.
C
C   Note that the 13-digit constants in the data statements
C below may not be acceptable to all compilers.
C
C On input:
C
C       X = Point at which the functions are to be
C           evaluated.
C
C X is not altered by this routine.
C
C On output:
C
C       SINHM = sinh(X) - X.
C
C       COSHM = cosh(X) - 1.
C
C       COSHMM = cosh(X) - 1 - X*X/2.
C
C Modules required by SNHCSH:  None
C
C Intrinsic functions called by SNHCSH:  ABS, EXP
C
C***********************************************************
C
      data c1/.1666666666659e0/,
     .     c2/.8333333431546e-2/,
     .     c3/.1984107350948e-3/,
     .     c4/.2768286868175e-5/
      ax = abs(x)
      xs = ax*ax
      if (ax .le. .5) then
C
C Approximations for small X:
C
        xc = x*xs
        sinhm = xc*(((c4*xs+c3)*xs+c2)*xs+c1)
        xsd4 = .25*xs
        xsd2 = xsd4 + xsd4
        f = (((c4*xsd4+c3)*xsd4+c2)*xsd4+c1)*xsd4
        coshmm = xsd2*f*(f+2.)
        coshm = coshmm + xsd2
      else
C
C Approximations for large X:
C
        expx = exp(ax)
        sinhm = -(((1./expx+ax)+ax)-expx)/2.
        if (x .lt. 0.) sinhm = -sinhm
        coshm = ((1./expx-2.)+expx)/2.
        coshmm = coshm - xs/2.
      endif
      return
      end

      subroutine smcrv (n,x,y,sigma,period,w,sm,smtol,
     .                  wk, ys,yp,ier)
      logical period
      integer n, ier
      real    x(n), y(n), sigma(n), w(n), sm, smtol,
     .        wk(n,10), ys(n), yp(n)
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   03/17/91
C
C   Given a sequence of abscissae X with associated data
C values Y and tension factors SIGMA, this routine deter-
C mines a set of function values YS and first derivatives YP
C associated with a Hermite interpolatory tension spline
C H(x) which smoothes the data.  H(x) has two continuous
C derivatives for all x and satisfies either natural or per-
C iodic end conditions.  The values and derivatives are
C chosen to minimize a quadratic functional Q1(YS,YP) sub-
C ject to the constraint Q2(YS) .LE. SM for Q2(YS) =
C (Y-YS)**T*W*(Y-YS), where **T denotes transpose and W is a
C diagonal matrix of positive weights.
C
C   Functions HVAL, HPVAL, HPPVAL, and TSINTL may be called
C to compute values, derivatives, and integrals of H.  The
C function values YS must be used as data values in those
C subprograms.
C
C   The smoothing procedure is an extension of the method
C for cubic spline smoothing due to C. Reinsch:  Numer.
C Math., 10 (1967) and 16 (1971).  Q1 is defined as the sum
C of integrals over the intervals (X(I),X(I+1)) of HPP**2 +
C (SIGMA(I)/DX)**2*(HP-S)**2, where DX = X(I+1)-X(I), HP and
C HPP denote first and second derivatives of H, and S =
C (YS(I+1)-YS(I))/DX.  Introducing a smoothing parameter P,
C and assuming the constraint is active, the problem is
C equivalent to minimizing Q(P,YS,YP) = Q1(YS,YP) +
C P*(Q2(YS)-SM).  The secant method is used to find a zero
C of G(P) = 1/SQRT(Q2) - 1/SQRT(SM), where YS and YP satisfy
C the order 2N symmetric positive-definite linear system
C obtained by setting the gradient of Q (treated as a func-
C tion of YS and YP) to zero.
C
C   Note that the interpolation problem corresponding to
C YS = Y, SM = 0, and P infinite is solved by Subroutine
C YPC2 or YPC2P.
C
C On input:
C
C       N = Number of data points.  N .GE. 2 if PERIOD =
C           FALSE, and N .GE. 3 if PERIOD = TRUE.
C
C       X = Array of length N containing a strictly in-
C           creasing sequence of abscissae:  X(I) < X(I+1)
C           for I = 1,...,N-1.
C
C       Y = Array of length N containing data values assoc-
C           iated with the abscissae.  If PERIOD = TRUE, it
C           is assumed that Y(N) = Y(1).
C
C       SIGMA = Array of length N-1 containing tension
C               factors.  SIGMA(I) is associated with inter-
C               val (X(I),X(I+1)) for I = 1,...,N-1.  If
C               SIGMA(I) = 0, H is cubic, and as SIGMA in-
C               creases, H approaches linear in the inter-
C               val.
C
C       PERIOD = Periodic end condition flag:
C                PERIOD = .F. if H is to satisfy natural end
C                             conditions:  zero second der-
C                             ivatives at X(1) and X(N).
C                PERIOD = .T. if H is to satisfy periodic
C                             end conditions:  the values
C                             and first two derivatives at
C                             X(1) agree with those at X(N),
C                             and a period thus has length
C                             X(N)-X(1).
C
C       W = Array of length N containing positive weights
C           associated with the data values.  The recommend-
C           ed value of W(I) is 1/DY**2, where DY is the
C           standard deviation associated with Y(I).  If
C           nothing is known about the errors in Y, a con-
C           stant (estimated value) should be used for DY.
C           If PERIOD = TRUE, it is assumed that W(N) =
C           W(1).
C
C       SM = Positive parameter specifying an upper bound on
C            Q2(YS).  H(x) is linear (and Q2 is minimized)
C            if SM is sufficiently large that the constraint
C            is not active.  It is recommended that SM sat-
C            isfy N-SQRT(2N) .LE. SM .LE. N+SQRT(2N) and
C            SM = N is reasonable if W(I) = 1/DY**2.
C
C       SMTOL = Parameter in the range (0,1) specifying the
C               relative error allowed in satisfying the
C               constraint:  the constraint is assumed to
C               be satisfied if SM*(1-SMTOL) .LE. Q2 .LE.
C               SM*(1+SMTOL).  A reasonable value for SMTOL
C               is SQRT(2/N).
C
C The above parameters are not altered by this routine.
C
C       WK = Work space of length at least 6N if PERIOD =
C            FALSE, and 10N if PERIOD = TRUE.
C
C On output:
C
C       YS = Array of length N containing values of H at the
C            abscissae unless IER < 0.  YS(N) = YS(1) if
C            PERIOD = TRUE.
C
C       YP = Array of length N containing first derivative
C            values of H at the abscissae unless IER < 0.
C            YP(N) = YP(1) if PERIOD = TRUE.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered and the
C                     constraint is active:  Q2(YS) is ap-
C                     proximately equal to SM.
C             IER = 1 if no errors were encountered but the
C                     constraint is not active:  YS and YP
C                     are the values and derivatives of the
C                     linear function (constant function if
C                     PERIOD = TRUE) which minimizes Q2, and
C                     Q1 = 0.
C             IER = -1 if N, W, SM, or SMTOL is outside its
C                      valid range.  YS and YP are unaltered
C                      in this case.
C             IER = -I if X(I) .LE. X(I-1) for some I in the
C                      range 2,...,N.  YS and YP are unal-
C                      tered in this case.
C
C Modules required by SMCRV:  B2TRI or B2TRIP, SNHCSH,
C                               YPCOEF
C
C Intrinsic functions called by SMCRV:  ABS, SQRT
C
C***********************************************************
C
      logical per
      data    lun/-1/
      nn = n
      per = period
C
C Test for errors, and compute the components of the system
C   (normal equations) for the weighted least squares linear
C   fit.
C
      ier = -1
      if (nn .lt. 2  .or.  (nn .lt. 3  .and.  per)  .or.
     .    sm .le. 0.  .or.  smtol .le. 0.  .or.
     .    smtol .ge. 1.) return
      c11 = 0.
      c12 = 0.
      c22 = 0.
      r1 = 0.
      r2 = 0.
      xi = x(1) - 1.
      do 1 i = 1,nn
        wi = w(i)
        if (wi .le. 0.) return
        if (x(i) .le. xi) then
          ier = -i
          return
        endif
        xi = x(i)
        yi = y(i)
        c22 = c22 + wi
        r2 = r2 + wi*yi
        if (.not. per) then
          wixi = wi*xi
          c11 = c11 + wixi*xi
          c12 = c12 + wixi
          r1 = r1 + wixi*yi
        endif
    1   continue
C
C Solve the system for (HP,H0), where HP is the derivative
C   (constant) and H0 = H(0).
C
      if (per) then
        h0 = r2/c22
        hp = 0.
      else
        h0 = (c11*r2-c12*r1)/(c11*c22-c12*c12)
        hp = (r1 - c12*h0)/c11
      endif
C
C Store function values and derivatives, and accumulate
C   Q2 = (Y-YS)**T*W*(Y-YS).
C
      q2 = 0.
      do 2 i = 1,nn
        ys(i) = hp*x(i) + h0
        yp(i) = hp
        q2 = q2 + w(i)*(y(i)-ys(i))**2
    2   continue
C
C Compute bounds on Q2 defined by SMTOL, and test for the
C   constraint satisfied by the linear fit.
C
      q2min = sm*(1. - smtol)
      q2max = sm*(1. + smtol)
      if (q2 .le. q2max) then
C
C   The constraint is satisfied by a linear function.
C
        ier = 1
        if (lun .ge. 0) write (lun,100)
  100   format (///1h ,'smcrv -- the constraint is not ',
     .          'active and the fit is linear.'/)
        return
      endif
C
C Compute the matrix components for the linear system.
C
      ier = 0
      nm1 = nn - 1
      do 3 i = 1,nm1
        dx = x(i+1) - x(i)
        sig = abs(sigma(i))
        call ypcoef (sig,dx, d,sd)
        wk(i,1) = d
        wk(i,2) = sd
    3   continue
C
C Compute G0 = G(0), and print a heading.
C
      s = 1./sqrt(sm)
      g0 = 1./sqrt(q2) - s
      if (lun .ge. 0) write (lun,110) sm, smtol, g0
  110 format (///1h ,3x,'smcrv -- sm = ',e10.4,', smtol = ',
     .        e14.8,', g(0) = ',e15.8///)
C
C G(P) is strictly increasing and concave, and G(0) < 0.
C
C Initialize parameters for the secant method.  The method
C   uses three points:  (P0,G0), (P,G), and (PNEG,GNEG),
C   where P0 and PNEG are defined implicitly by DP = P - P0
C   and DMAX = P - PNEG.
C
      p = 10.*sm
      dp = p
      dmax = 0.
      iter = 0
C
C Top of loop:  compute G and print a message.  For each
C               secant iteration, the following values are
C               printed:  P, G(P), and DP, where DP is the
C               change in P computed by linear interpolation
C               between the current point (P,G) and a previ-
C               ous point.
C
C
    4 if (.not. per) then
        call b2tri (nn,x,y,w,p,wk,wk(1,2),wk(1,3),wk(1,4),
     .              wk(1,5),wk(1,6), ys,yp,ierr)
      else
        call b2trip (nn,x,y,w,p,wk,wk(1,2),wk(1,3),wk(1,4),
     .               wk(1,5),wk(1,6),wk(1,7),wk(1,8),
     .               wk(1,9),wk(1,10), ys,yp,ierr)
      endif
      q2 = 0.
      do 5 i = 1,nn
        q2 = q2 + w(i)*(y(i)-ys(i))**2
    5   continue
      g = 1./sqrt(q2) - s
      iter = iter + 1
      if (lun .ge. 0) then
        p0 = p - dp
        if (lun .ge. 0) write (lun,120) iter, p, g, p0, g0
  120   format (/1x,i2,' -- p = ',e15.8,',  g = ',e15.8/
     .          6x,'p0 = ',e15.8,', g0 = ',e15.8)
      endif
C
C   Test for convergence.
C
      if ( g .eq. g0  .or.  (q2min .le. q2  .and.
     .                       q2 .le. q2max) )      return
      if (dmax .ne. 0.  .or.  g .gt. 0.) go to 6
C
C   Increase P until G(P) > 0.
C
      p = 10.*p
      dp = p
      go to 4
C
C   A bracketing interval [P0,P] has been found.
C
    6 if (g0*g .le. 0.) then
C
C   G0*G < 0.  Update (PNEG,GNEG) to (P0,G0) so that G
C     and GNEG always have opposite signs.
C
        dmax = dp
        gneg = g0
      endif
C
C   Compute the change in P by linear interpolation between
C     (P0,G0) and (P,G).
C
    7 dp = -g*dp/(g-g0)
      if (lun .ge. 0) write (lun,130) dp
  130 format (1x ,5x,'dp = ',e15.8)
      if (abs(dp) .gt. abs(dmax)) then
C
C   G0*G > 0, and the new estimate would be outside of the
C     bracketing interval of length abs(DMAX).  Reset
C     (P0,G0) to (PNEG,GNEG).
C
        dp = dmax
        g0 = gneg
        go to 7
      endif
C
C   Bottom of loop:  update P, DMAX, and G0.
C
      p = p + dp
      dmax = dmax + dp
      g0 = g
      go to 4
      end

      subroutine ypcoef (sigma,dx, d,sd)
      real sigma, dx, d, sd
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/01/90
C
C   This subroutine computes the coefficients of the deriva-
C tives in the symmetric diagonally dominant tridiagonal
C system associated with the C-2 derivative estimation pro-
C cedure for a Hermite interpolatory tension spline.
C
C On input:
C
C       SIGMA = Nonnegative tension factor associated with
C               an interval.
C
C       DX = Positive interval width.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       D = Component of the diagonal term associated with
C           the interval.  D = SIG*(SIG*COSHM(SIG) -
C           SINHM(SIG))/(DX*E), where SIG = SIGMA and E =
C           SIG*SINH(SIG) - 2*COSHM(SIG).
C
C       SD = Subdiagonal (superdiagonal) term.  SD = SIG*
C            SINHM(SIG)/E.
C
C Module required by YPCOEF:  SNHCSH
C
C Intrinsic function called by YPCOEF:  EXP
C
C***********************************************************
C
      sig = sigma
      if (sig .le. 0.) then
C
C SIG = 0:  cubic interpolant.
C
        d = 4./dx
        sd = 2./dx
      elseif (sig .le. .5) then
C
C 0 .LT. SIG .LE. .5:  use approximations designed to avoid
C                      cancellation error in the hyperbolic
C                      functions when SIGMA is small.
C
        call tssnhcsh (sig, sinhm,coshm,coshmm)
        e = (sig*sinhm - coshmm - coshmm)*dx
        d = sig*(sig*coshm-sinhm)/e
        sd = sig*sinhm/e
      else
C
C SIG > .5:  scale SINHM and COSHM by 2*EXP(-SIG) in order
C            to avoid overflow when SIGMA is large.
C
        ems = exp(-sig)
        ssinh = 1. - ems*ems
        ssm = ssinh - 2.*sig*ems
        scm = (1.-ems)*(1.-ems)
        e = (sig*ssinh - scm - scm)*dx
        d = sig*(sig*scm-ssm)/e
        sd = sig*ssm/e
      endif
      return
      end

      subroutine b2tri (n,x,y,w,p,d,sd,t11,t12,t21,t22, ys,
     .                  yp,ier)
      integer n, ier
      real    x(n), y(n), w(n), p, d(n), sd(n), t11(n),
     .        t12(n), t21(n), t22(n), ys(n), yp(n)
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   03/13/91
C
C   This subroutine solves the order 2N symmetric positive-
C definite block tridiagonal linear system associated with
C minimizing the quadratic functional Q(YS,YP) described in
C Subroutine SMCRV.
C
C On input:
C
C       N = Number of data points.  N .GE. 2.
C
C       X,Y,W = Arrays of length N containing abscissae,
C               data values, and positive weights, respect-
C               ively.  The abscissae must be strictly in-
C               creasing.
C
C       P = Positive smoothing parameter defining Q.
C
C       D,SD = Arrays of length N-1 containing positive ma-
C              trix entries.  Letting DX and SIG denote the
C              width and tension factor associated with the
C              interval (X(I),X(I+1)), D(I) = SIG*(SIG*
C              COSHM(SIG) - SINHM(SIG))/(DX*E) and SD(I) =
C              SIG*SINHM(SIG)/(DX*E) where E = SIG*SINH(SIG)
C              - 2*COSHM(SIG).
C
C The above parameters are not altered by this routine.
C
C       T11,T12,T21,T22 = Arrays of length N-1 used as
C                         temporary work space.
C
C On output:
C
C       YS,YP = Arrays of length N containing solution com-
C               ponents:  function and derivative values,
C               respectively, at the abscissae.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if N or P is outside its valid range
C                     on input.
C             Note that no test is made for a nonpositive
C             value of X(I+1)-X(I), W(I), D(I), or SD(I).
C
C Modules required by B2TRI:  None
C
C***********************************************************
C
      nn = n
      nm1 = nn - 1
      pp = p
      ier = 1
      if (nn .lt. 2  .or.  pp .le. 0.) return
C
C The forward elimination step consists of scaling a row by
C   the inverse of its diagonal block and eliminating the
C   subdiagonal block.  The superdiagonal is stored in T and
C   the right hand side in YS,YP.  For J = 11, 12, and 22,
C   SJI and SJIM1 denote the elements in position J of the
C   superdiagonal block in rows I and I-1, respectively.
C   Similarly, DJI denotes an element in the diagonal block
C   of row I.
C
C Initialize for I = 2.
C
      dx = x(2) - x(1)
      dim1 = d(1)
      s22im1 = sd(1)
      s12im1 = (dim1 + s22im1)/dx
      s11im1 = -2.*s12im1/dx
      r1 = pp*w(1)
      d11i = r1 - s11im1
      d12i = s12im1
      d22i = dim1
      den = d11i*d22i - d12i*d12i
      t11(1) = (d22i*s11im1 + d12i*s12im1)/den
      t12(1) = (d22i*s12im1 - d12i*s22im1)/den
      t21(1) = -(d12i*s11im1 + d11i*s12im1)/den
      t22(1) = (d11i*s22im1 - d12i*s12im1)/den
      r1 = r1*y(1)/den
      ys(1) = d22i*r1
      yp(1) = -d12i*r1
C
C I = 2,...,N-1:
C
      do 1 i = 2,nm1
        im1 = i - 1
        dx = x(i+1) - x(i)
        di = d(i)
        s22i = sd(i)
        s12i = (di + s22i)/dx
        s11i = -2.*s12i/dx
        r1 = pp*w(i)
        d11i = r1 - s11im1 - s11i - (s11im1*t11(im1) -
     .         s12im1*t21(im1))
        d12i = s12i - s12im1 - (s11im1*t12(im1) - s12im1*
     .         t22(im1))
        d22i = dim1 + di - (s12im1*t12(im1)+s22im1*t22(im1))
        den = d11i*d22i - d12i*d12i
        t11(i) = (d22i*s11i + d12i*s12i)/den
        t12(i) = (d22i*s12i - d12i*s22i)/den
        t21(i) = -(d12i*s11i + d11i*s12i)/den
        t22(i) = (d11i*s22i - d12i*s12i)/den
        r1 = r1*y(i) - s11im1*ys(im1) + s12im1*yp(im1)
        r2 = -s12im1*ys(im1) - s22im1*yp(im1)
        ys(i) = (d22i*r1 - d12i*r2)/den
        yp(i) = (d11i*r2 - d12i*r1)/den
        dim1 = di
        s22im1 = s22i
        s12im1 = s12i
        s11im1 = s11i
    1   continue
C
C I = N:
C
      r1 = pp*w(nn)
      d11i = r1 - s11im1 - (s11im1*t11(nm1)-s12im1*t21(nm1))
      d12i = -s12im1 - (s11im1*t12(nm1) - s12im1*t22(nm1))
      d22i = dim1 - (s12im1*t12(nm1) + s22im1*t22(nm1))
      den = d11i*d22i - d12i*d12i
      r1 = r1*y(nn) - s11im1*ys(nm1) + s12im1*yp(nm1)
      r2 = -s12im1*ys(nm1) - s22im1*yp(nm1)
      ys(nn) = (d22i*r1 - d12i*r2)/den
      yp(nn) = (d11i*r2 - d12i*r1)/den
C
C Back solve the system.
C
      do 2 i = nm1,1,-1
        ys(i) = ys(i) - (t11(i)*ys(i+1) + t12(i)*yp(i+1))
        yp(i) = yp(i) - (t21(i)*ys(i+1) + t22(i)*yp(i+1))
    2   continue
      ier = 0
      return
      end

      subroutine b2trip (n,x,y,w,p,d,sd,t11,t12,t21,t22,u11,
     .                   u12,u21,u22, ys,yp,ier)
      integer n, ier
      real    x(n), y(n), w(n), p, d(n), sd(n), t11(n),
     .        t12(n), t21(n), t22(n), u11(n), u12(n),
     .        u21(n), u22(n), ys(n), yp(n)
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   03/13/91
C
C   This subroutine solves the order 2(N-1) symmetric posi-
C tive-definite linear system associated with minimizing the
C quadratic functional Q(YS,YP) (described in Subroutine
C SMCRV) with periodic end conditions.  The matrix is block
C tridiagonal except for nonzero blocks in the upper right
C and lower left corners.
C
C On input:
C
C       N = Number of data points.  N .GE. 3.
C
C       X = Array of length N containing a strictly in-
C           creasing sequence of abscissae.
C
C       Y,W = Arrays of length N-1 containing data values
C             and positive weights, respectively, associated
C             with the first N-1 abscissae.
C
C       P = Positive smoothing parameter defining Q.
C
C       D,SD = Arrays of length N-1 containing positive ma-
C              trix elements.  Letting DX and SIG denote the
C              width and tension factor associated with the
C              interval (X(I),X(I+1)), D(I) = SIG*(SIG*
C              COSHM(SIG) - SINHM(SIG))/(DX*E) and SD(I) =
C              SIG*SINHM(SIG)/(DX*E) where E = SIG*SINH(SIG)
C              - 2*COSHM(SIG).
C
C The above parameters are not altered by this routine.
C
C       T11,T12,T21,T22,U11,U12,U21,U22 = Arrays of length
C                                         N-2 used as temp-
C                                         orary work space.
C
C On output:
C
C       YS,YP = Arrays of length N containing solution com-
C               ponents:  function and derivative values,
C               respectively, at the abscissae.  YS(N) =
C               YS(1) and YP(N) = YP(1).
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if N or P is outside its valid range
C                     on input.
C             Note that no test is made for a nonpositive
C             value of X(I+1)-X(I), W(I), D(I), or SD(I).
C
C Modules required by B2TRIP:  None
C
C***********************************************************
C
      nn = n
      nm1 = nn - 1
      nm2 = nn - 2
      nm3 = nn - 3
      pp = p
      ier = 1
      if (nn .lt. 3  .or.  pp .le. 0.) return
C
C The forward elimination step consists of scaling a row by
C   the inverse of its diagonal block and eliminating the
C   subdiagonal block for the first N-2 rows.  The super-
C   diagonal is stored in T, the negative of the last column
C   in U, and the right hand side in YS,YP.  For J = 11, 12,
C   and 22, SJI and SJIM1 denote the elements in position J
C   of the superdiagonal block in rows I and I-1, respect-
C   ively.  Similarly, DJI denotes an element in the diago-
C   nal block of row I.
C
C I = 1:
C
      dx = x(nn) - x(nm1)
      dnm1 = d(nm1)
      s22nm1 = sd(nm1)
      s12nm1 = -(dnm1 + s22nm1)/dx
      s11nm1 = 2.*s12nm1/dx
      dx = x(2) - x(1)
      di = d(1)
      s22i = sd(1)
      s12i = (di + s22i)/dx
      s11i = -2.*s12i/dx
      r1 = pp*w(1)
      d11i = r1 - s11nm1 - s11i
      d12i = s12i + s12nm1
      d22i = dnm1 + di
      den = d11i*d22i - d12i*d12i
      t11(1) = (d22i*s11i + d12i*s12i)/den
      t12(1) = (d22i*s12i - d12i*s22i)/den
      t21(1) = -(d12i*s11i + d11i*s12i)/den
      t22(1) = (d11i*s22i - d12i*s12i)/den
      u11(1) = -(d22i*s11nm1 + d12i*s12nm1)/den
      u12(1) = (d12i*s22nm1 - d22i*s12nm1)/den
      u21(1) = (d12i*s11nm1 + d11i*s12nm1)/den
      u22(1) = (d12i*s12nm1 - d11i*s22nm1)/den
      r1 = r1*y(1)/den
      ys(1) = d22i*r1
      yp(1) = -d12i*r1
      if (nn .eq. 3) go to 2
C
C I = 2,...,N-2:
C
      do 1 i = 2,nm2
        im1 = i - 1
        dim1 = di
        s22im1 = s22i
        s12im1 = s12i
        s11im1 = s11i
        dx = x(i+1) - x(i)
        di = d(i)
        s22i = sd(i)
        s12i = (di + s22i)/dx
        s11i = -2.*s12i/dx
        r1 = pp*w(i)
        d11i = r1 - s11im1 - s11i - (s11im1*t11(im1) -
     .         s12im1*t21(im1))
        d12i = s12i - s12im1 - (s11im1*t12(im1) - s12im1*
     .         t22(im1))
        d22i = dim1 + di - (s12im1*t12(im1)+s22im1*t22(im1))
        den = d11i*d22i - d12i*d12i
        t11(i) = (d22i*s11i + d12i*s12i)/den
        t12(i) = (d22i*s12i - d12i*s22i)/den
        t21(i) = -(d12i*s11i + d11i*s12i)/den
        t22(i) = (d11i*s22i - d12i*s12i)/den
        su11 = s11im1*u11(im1) - s12im1*u21(im1)
        su12 = s11im1*u12(im1) - s12im1*u22(im1)
        su21 = s12im1*u11(im1) + s22im1*u21(im1)
        su22 = s12im1*u12(im1) + s22im1*u22(im1)
        u11(i) = (d12i*su21 - d22i*su11)/den
        u12(i) = (d12i*su22 - d22i*su12)/den
        u21(i) = (d12i*su11 - d11i*su21)/den
        u22(i) = (d12i*su12 - d11i*su22)/den
        r1 = r1*y(i) - s11im1*ys(im1) + s12im1*yp(im1)
        r2 = -s12im1*ys(im1) - s22im1*yp(im1)
        ys(i) = (d22i*r1 - d12i*r2)/den
        yp(i) = (d11i*r2 - d12i*r1)/den
    1   continue
C
C The backward elimination step zeros the first N-3 blocks
C   of the superdiagonal.  For I = N-2,N-3,...,1, T(I) and
C   (YS(I),YP(I)) are overwritten with the negative of the
C   last column and the new right hand side, respectively.
C
    2 t11(nm2) = u11(nm2) - t11(nm2)
      t12(nm2) = u12(nm2) - t12(nm2)
      t21(nm2) = u21(nm2) - t21(nm2)
      t22(nm2) = u22(nm2) - t22(nm2)
      do 3 i = nm3,1,-1
        ip1 = i + 1
        ys(i) = ys(i) - t11(i)*ys(ip1) - t12(i)*yp(ip1)
        yp(i) = yp(i) - t21(i)*ys(ip1) - t22(i)*yp(ip1)
        t11(i) = u11(i) - t11(i)*t11(ip1) - t12(i)*t21(ip1)
        t12(i) = u12(i) - t11(i)*t12(ip1) - t12(i)*t22(ip1)
        t21(i) = u21(i) - t21(i)*t11(ip1) - t22(i)*t21(ip1)
        t22(i) = u22(i) - t21(i)*t12(ip1) - t22(i)*t22(ip1)
    3   continue
C
C Solve the last equation for YS(N-1),YP(N-1).  SJI = SJNM2
C   and DJI = DJNM1.
C
      r1 = pp*w(nm1)
      d11i = r1 - s11i - s11nm1 + s11nm1*t11(1) -
     .       s12nm1*t21(1) + s11i*t11(nm2) - s12i*t21(nm2)
      d12i = -s12nm1 - s12i + s11nm1*t12(1) - s12nm1*t22(1)
     .       + s11i*t12(nm2) - s12i*t22(nm2)
      d22i = di + dnm1 + s12nm1*t12(1) + s22nm1*t22(1) +
     .       s12i*t12(nm2) + s22i*t22(nm2)
      den = d11i*d22i - d12i*d12i
      r1 = r1*y(nm1) - s11nm1*ys(1) + s12nm1*yp(1) -
     .     s11i*ys(nm2) + s12i*yp(nm2)
      r2 = -s12nm1*ys(1) - s22nm1*yp(1) - s12i*ys(nm2) -
     .     s22i*yp(nm2)
      ysnm1 = (d22i*r1 - d12i*r2)/den
      ypnm1 = (d11i*r2 - d12i*r1)/den
      ys(nm1) = ysnm1
      yp(nm1) = ypnm1
C
C Back substitute for the remainder of the solution
C   components.
C
      do 4 i = 1,nm2
        ys(i) = ys(i) + t11(i)*ysnm1 + t12(i)*ypnm1
        yp(i) = yp(i) + t21(i)*ysnm1 + t22(i)*ypnm1
    4   continue
C
C YS(N) = YS(1) and YP(N) = YP(1).
C
      ys(nn) = ys(1)
      yp(nn) = yp(1)
      ier = 0
      return
      end

      real function hval (t,n,x,y,yp,sigma, ier)
      integer n, ier
      real    t, x(n), y(n), yp(n), sigma(n)
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/01/90
C
C   This function evaluates a Hermite interpolatory tension
C spline H at a point T.  Note that a large value of SIGMA
C may cause underflow.  The result is assumed to be zero.
C
C   Given arrays X, Y, YP, and SIGMA of length NN, if T is
C known to lie in the interval (X(I),X(J)) for some I < J,
C a gain in efficiency can be achieved by calling this
C function with N = J+1-I (rather than NN) and the I-th
C components of the arrays (rather than the first) as par-
C ameters.
C
C On input:
C
C       T = Point at which H is to be evaluated.  Extrapo-
C           lation is performed if T < X(1) or T > X(N).
C
C       N = Number of data points.  N .GE. 2.
C
C       X = Array of length N containing the abscissae.
C           These must be in strictly increasing order:
C           X(I) < X(I+1) for I = 1,...,N-1.
C
C       Y = Array of length N containing data values.
C           H(X(I)) = Y(I) for I = 1,...,N.
C
C       YP = Array of length N containing first deriva-
C            tives.  HP(X(I)) = YP(I) for I = 1,...,N, where
C            HP denotes the derivative of H.
C
C       SIGMA = Array of length N-1 containing tension fac-
C               tors whose absolute values determine the
C               balance between cubic and linear in each
C               interval.  SIGMA(I) is associated with int-
C               erval (I,I+1) for I = 1,...,N-1.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       IER = Error indicator:
C             IER = 0  if no errors were encountered and
C                      X(1) .LE. T .LE. X(N).
C             IER = 1  if no errors were encountered and
C                      extrapolation was necessary.
C             IER = -1 if N < 2.
C             IER = -2 if the abscissae are not in strictly
C                      increasing order.  (This error will
C                      not necessarily be detected.)
C
C       HVAL = Function value H(T), or zero if IER < 0.
C
C Modules required by HVAL:  INTRVL, SNHCSH
C
C Intrinsic functions called by HVAL:  ABS, EXP
C
C***********************************************************
C
      data sbig/85./
      if (n .lt. 2) go to 1
C
C Find the index of the left end of an interval containing
C   T.  If T < X(1) or T > X(N), extrapolation is performed
C   using the leftmost or rightmost interval.
C
      if (t .lt. x(1)) then
        i = 1
        ier = 1
      elseif (t .gt. x(n)) then
        i = n-1
        ier = 1
      else
        i = interval (t,n,x)
        ier = 0
      endif
      ip1 = i + 1
C
C Compute interval width DX, local coordinates B1 and B2,
C   and second differences D1 and D2.
C
      dx = x(ip1) - x(i)
      if (dx .le. 0.) go to 2
      u = t - x(i)
      b2 = u/dx
      b1 = 1. - b2
      y1 = y(i)
      s1 = yp(i)
      s = (y(ip1)-y1)/dx
      d1 = s - s1
      d2 = yp(ip1) - s
      sig = abs(sigma(i))
      if (sig .eq. 0.) then
C
C SIG = 0:  H is the Hermite cubic interpolant.
C
        hval = y1 + u*(s1 + b2*(d1 + b1*(d1-d2)))
      elseif (sig .le. .5) then
C
C 0 .LT. SIG .LE. .5:  use approximations designed to avoid
C   cancellation error in the hyperbolic functions.
C
        sb2 = sig*b2
        call tssnhcsh (sig, sm,cm,cmm)
        call tssnhcsh (sb2, sm2,cm2,dummy)
        e = sig*sm - cmm - cmm
        hval = y1 + s1*u + dx*((cm*sm2-sm*cm2)*(d1+d2) +
     .                         sig*(cm*cm2-(sm+sig)*sm2)*d1)
     .                         /(sig*e)
      else
C
C SIG > .5:  use negative exponentials in order to avoid
C   overflow.  Note that EMS = EXP(-SIG).  In the case of
C   extrapolation (negative B1 or B2), H is approximated by
C   a linear function if -SIG*B1 or -SIG*B2 is large.
C
        sb1 = sig*b1
        sb2 = sig - sb1
        if (-sb1 .gt. sbig  .or.  -sb2 .gt. sbig) then
          hval = y1 + s*u
        else
          e1 = exp(-sb1)
          e2 = exp(-sb2)
          ems = e1*e2
          tm = 1. - ems
          ts = tm*tm
          tp = 1. + ems
          e = tm*(sig*tp - tm - tm)
          hval = y1 + s*u + dx*(tm*(tp-e1-e2)*(d1+d2) + sig*
     .                         ((e2+ems*(e1-2.)-b1*ts)*d1 +
     .                          (e1+ems*(e2-2.)-b2*ts)*d2))/
     .                          (sig*e)
        endif
      endif
      return
C
C N is outside its valid range.
C
    1 hval = 0.
      ier = -1
      return
C
C X(I) .GE. X(I+1).
C
    2 hval = 0.
      ier = -2
      return
      end

      function tsintl (a,b,n,x,y,yp,sigma, ier)
      integer n, ier
      real    a, b, x(n), y(n), yp(n), sigma(n)
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/01/90
C
C   This function computes the integral from A to B of a
C Hermite interpolatory tension spline H.
C
C On input:
C
C       A,B = Lower and upper limits of integration, re-
C             spectively.  Note that -TSINTL(B,A,...) =
C             TSINTL(A,B,...).
C
C       N = Number of data points.  N .GE. 2.
C
C       X = Array of length N containing the abscissae.
C           These must be in strictly increasing order:
C           X(I) < X(I+1) for I = 1,...,N-1.
C
C       Y = Array of length N containing data values.
C           H(X(I)) = Y(I) for I = 1,...,N.
C
C       YP = Array of length N containing first deriva-
C            tives.  HP(X(I)) = YP(I) for I = 1,...,N, where
C            HP denotes the derivative of H.
C
C       SIGMA = Array of length N-1 containing tension fac-
C               tors whose absolute values determine the
C               balance between cubic and linear in each
C               interval.  SIGMA(I) is associated with int-
C               erval (I,I+1) for I = 1,...,N-1.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       IER = Error indicator:
C             IER = 0  if no errors were encountered and
C                      X(1) .LE. T .LE. X(N) for T = A and
C                      T = B, or A = B.
C             IER = 1  if no errors were encountered but
C                      extrapolation was necessary:  A or B
C                      not in the interval (X(1),X(N)).
C             IER = -1 IF N < 2.
C             IER = -2 if the abscissae are not in strictly
C                      increasing order.  Only those in or
C                      adjacent to the interval of integra-
C                      tion are tested.
C
C       TSINTL = Integral of H from A to B, or zero if
C                IER < 0.
C
C Modules required by TSINTL:  INTRVL, SNHCSH
C
C Intrinsic functions called by TSINTL:  ABS, AMAX1, AMIN1,
C                                          EXP, MAX0
C
C***********************************************************
C
      data sbig/85./
      if (n .lt. 2) go to 7
C
C Accumulate the integral from XL to XU in SUM.
C
      xl = amin1(a,b)
      xu = amax1(a,b)
      sum = 0.
      ier = 0
      if (xl .eq. xu) go to 6
C
C Find left-end indexes of intervals containing XL and XU.
C   If XL < X(1) or XU > X(N), extrapolation is performed
C   using the leftmost or rightmost interval.
C
      il = interval (xl,n,x)
      iu = interval (xu,n,x)
      if (xl .lt. x(1)  .or.  xu .gt. x(n)) ier = 1
      ilp1 = il + 1
      imin = il
      if (xl .eq. x(il)) go to 2
C
C Compute the integral from XL to X(IL+1).
C
      dx = x(ilp1) - x(il)
      if (dx .le. 0.) go to 8
      u = x(ilp1) - xl
      if (u .eq. 0.) go to 1
      b1 = u/dx
      y2 = y(ilp1)
      s = (y2-y(il))/dx
      s2 = yp(ilp1)
      d1 = s - yp(il)
      d2 = s2 - s
      sig = abs(sigma(il))
      if (sig .eq. 0.) then
C
C   SIG = 0.
C
        sum = sum + u*(y2 - u*(6.*s2 - b1*(4.*d2 +
     .                 (3.*b1-4.)*(d1-d2)))/12.)
      elseif (sig .le. .5) then
C
C   0 .LT. SIG .LE. .5.
C
        sb1 = sig*b1
        call tssnhcsh (sig, sm,cm,cmm)
        call tssnhcsh (sb1, sm1,cm1,cmm1)
        e = sig*sm - cmm - cmm
        sum = sum + u*(y2 - s2*u/2.) + ((cm*cmm1-sm*sm1)*
     .             (d1+d2) + sig*(cm*sm1-(sm+sig)*cmm1)*d2)/
     .             ((sig/dx)**2*e)
      else
C
C   SIG > .5.
C
        sb1 = sig*b1
        sb2 = sig - sb1
        if (-sb1 .gt. sbig  .or.  -sb2 .gt. sbig) then
          sum = sum + u*(y2 - s*u/2.)
        else
          e1 = exp(-sb1)
          e2 = exp(-sb2)
          ems = e1*e2
          tm = 1. - ems
          tp = 1. + ems
          t = sb1*sb1/2. + 1.
          e = tm*(sig*tp - tm - tm)
          sum = sum + u*(y2 - s2*u/2.)+(sig*tm*(tp*t-e1-e2-
     .                tm*sb1)*d2 - (tm*(tm*t-e1+e2-tp*sb1) +
     .                sig*(e1*ems-e2+2.*sb1*ems))*(d1+d2))/
     .                ((sig/dx)**2*e)
        endif
      endif
C
C Test for XL and XU in the same interval.
C
    1 imin = ilp1
      if (il .eq. iu) go to 5
C
C Add in the integral from X(IMIN) to X(J) for J =
C   Max(IL+1,IU).
C
    2 imax = max0(il,iu-1)
      do 3 i = imin,imax
        ip1 = i + 1
        dx = x(ip1) - x(i)
        if (dx .le. 0.) go to 8
        sig = abs(sigma(i))
        if (sig .eq. 0.) then
C
C   SIG = 0.
C
          sum = sum + dx*((y(i)+y(ip1))/2. -
     .                    dx*(yp(ip1)-yp(i))/12.)
        elseif (sig .le. .5) then
C
C   0 .LT. SIG .LE. .5.
C
          call tssnhcsh (sig, sm,cm,cmm)
          e = sig*sm - cmm - cmm
          sum = sum + dx*(y(i)+y(ip1) - dx*e*(yp(ip1)-yp(i))
     .                /(sig*sig*cm))/2.
        else
C
C   SIG > .5.
C
          ems = exp(-sig)
          sum = sum + dx*(y(i)+y(ip1) - dx*(sig*(1.+ems)/
     .                (1.-ems)-2.)*(yp(ip1)-yp(i))/
     .                (sig*sig))/2.
        endif
    3   continue
C
C Add in the integral from X(IU) to XU if IU > IL.
C
      if (il .eq. iu) go to 4
      iup1 = iu + 1
      dx = x(iup1) - x(iu)
      if (dx .le. 0.) go to 8
      u = xu - x(iu)
      if (u .eq. 0.) go to 6
      b2 = u/dx
      y1 = y(iu)
      s = (y(iup1)-y1)/dx
      s1 = yp(iu)
      d1 = s - s1
      d2 = yp(iup1) - s
      sig = abs(sigma(iu))
      if (sig .eq. 0.) then
C
C   SIG = 0.
C
        sum = sum + u*(y1 + u*(6.*s1 + b2*(4.*d1 +
     .                (4.-3.*b2)*(d1-d2)))/12.)
      elseif (sig .le. .5) then
C
C   0 .LT. SIG .LE. .5.
C
        sb2 = sig*b2
        call tssnhcsh (sig, sm,cm,cmm)
        call tssnhcsh (sb2, sm2,cm2,cmm2)
        e = sig*sm - cmm - cmm
        sum = sum + u*(y1 + s1*u/2.) + ((cm*cmm2-sm*sm2)*
     .              (d1+d2) + sig*(cm*sm2-(sm+sig)*cmm2)*d1)
     .              /((sig/dx)**2*e)
      else
C
C   SIG > .5.
C
        sb2 = sig*b2
        sb1 = sig - sb2
        if (-sb1 .gt. sbig  .or.  -sb2 .gt. sbig) then
          sum = sum + u*(y1 + s*u/2.)
        else
          e1 = exp(-sb1)
          e2 = exp(-sb2)
          ems = e1*e2
          tm = 1. - ems
          tp = 1. + ems
          t = sb2*sb2/2. + 1.
          e = tm*(sig*tp - tm - tm)
          sum = sum + u*(y1 + s1*u/2.)+(sig*tm*(tp*t-e1-e2-
     .                tm*sb2)*d1 - (tm*(tm*t-e2+e1-tp*sb2) +
     .                sig*(e2*ems-e1+2.*sb2*ems))*(d1+d2))/
     .                ((sig/dx)**2*e)
        endif
      endif
      go to 6
C
C IL = IU and SUM contains the integral from XL to X(IL+1).
C   Subtract off the integral from XU to X(IL+1).  DX and
C   SIG were computed above.
C
    4 y2 = y(ilp1)
      s = (y2-y(il))/dx
      s2 = yp(ilp1)
      d1 = s - yp(il)
      d2 = s2 - s
C
    5 u = x(ilp1) - xu
      if (u .eq. 0.) go to 6
      b1 = u/dx
      if (sig .eq. 0.) then
C
C   SIG = 0.
C
        sum = sum - u*(y2 - u*(6.*s2 - b1*(4.*d2 +
     .              (3.*b1-4.)*(d1-d2)))/12.)
      elseif (sig .le. .5) then
C
C   0 .LT. SIG .LE. .5.
C
        sb1 = sig*b1
        call tssnhcsh (sig, sm,cm,cmm)
        call tssnhcsh (sb1, sm1,cm1,cmm1)
        e = sig*sm - cmm - cmm
        sum = sum - u*(y2 - s2*u/2.) - ((cm*cmm1-sm*sm1)*
     .              (d1+d2) + sig*(cm*sm1-(sm+sig)*cmm1)*d2)
     .              /((sig/dx)**2*e)
      else
C
C   SIG > .5.
C
        sb1 = sig*b1
        sb2 = sig - sb1
        if (-sb1 .gt. sbig  .or.  -sb2 .gt. sbig) then
          sum = sum - u*(y2 - s*u/2.)
        else
          e1 = exp(-sb1)
          e2 = exp(-sb2)
          ems = e1*e2
          tm = 1. - ems
          tp = 1. + ems
          t = sb1*sb1/2. + 1.
          e = tm*(sig*tp - tm - tm)
          sum = sum - u*(y2 - s2*u/2.)-(sig*tm*(tp*t-e1-e2-
     .                tm*sb1)*d2 - (tm*(tm*t-e1+e2-tp*sb1) +
     .                sig*(e1*ems-e2+2.*sb1*ems))*(d1+d2))/
     .                ((sig/dx)**2*e)
        endif
      endif
C
C No errors were encountered.  Adjust the sign of SUM.
C
    6 if (xl .eq. b) sum = -sum
      tsintl = sum
      return
C
C N < 2.
C
    7 ier = -1
      tsintl = 0.
      return
C
C Abscissae not strictly increasing.
C
    8 ier = -2
      tsintl = 0.
      return
      end

      integer function interval (t,n,x)
      integer n
      real    t, x(n)
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/06/90
C
C   This function returns the index of the left end of an
C interval (defined by an increasing sequence X) which
C contains the value T.  The method consists of first test-
C ing the interval returned by a previous call, if any, and
C then using a binary search if necessary.
C
C On input:
C
C       T = Point to be located.
C
C       N = Length of X.  N .GE. 2.
C
C       X = Array of length N assumed (without a test) to
C           contain a strictly increasing sequence of
C           values.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       INTRVL = Index I defined as follows:
C
C                  I = 1    if  T .LT. X(2) or N .LE. 2,
C                  I = N-1  if  T .GE. X(N-1), and
C                  X(I) .LE. T .LT. X(I+1) otherwise.
C
C Modules required by INTRVL:  None
C
C***********************************************************
C
      save il
      data il/1/
      tt = t
      if (il .ge. 1  .and.  il .lt. n) then
        if (x(il) .le. tt  .and.  tt .lt. x(il+1)) go to 2
      endif
C
C Initialize low and high indexes.
C
      il = 1
      ih = n
C
C Binary search:
C
    1 if (ih .le. il+1) go to 2
        k = (il+ih)/2
        if (tt .lt. x(k)) then
          ih = k
        else
          il = k
        endif
        go to 1
C
C X(IL) .LE. T .LT. X(IL+1)  or  (T .LT. X(1) and IL=1)
C                            or  (T .GE. X(N) and IL=N-1)
C
    2 interval = il
      return
      end

      function store (x)
      real x
C
C***********************************************************
C
C                                                From TSPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/01/90
C
C   This function forces its argument X to be stored in a
C memory location, thus providing a means of determining
C floating point number characteristics (such as the machine
C precision) when it is necessary to avoid computation in
C high precision registers.
C
C On input:
C
C       X = Value to be stored.
C
C X is not altered by this function.
C
C On output:
C
C       STORE = Value of X after it has been stored and
C               possibly truncated or rounded to the single
C               precision word length.
C
C Modules required by STORE:  None
C
C***********************************************************
C
      common/stcom/y
      y = x
      store = y
      return
      end
