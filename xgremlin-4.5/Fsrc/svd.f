      subroutine svdlsq(x,y,sig,ndata,pc,npar,u,v,w,chisq,std,tol,fcn)
*
* Least squares fit based on Singular Value Decomposition. 
*
* Parameters:
*     x,y        data pairs (x,y)
*     sig        uncertainty in y
*     ndata      number of data points
*     pc         output: parameter vector with coefficients
*     npar       number of parameters
*     u          matrix  ndata x npar
*     v          matrix  npar x npar
*     w          vector with singular values
*     chisq      chi-square value of fit
*     std        vector with standard deviations for fitted parameters
*     tol        eliminate singular values smaller than Wmax * tol
*     fcn        fcn(x,afunc,npar) is a function that returns the 
*                values of npar functions at x in array afunc
*
* Ulf Griesmann  07/96
*
      parameter ( nmax=4*1024 )  ! max. number of data points
      parameter ( mmax=12 )      ! max. number of parameters in fit

      integer ndata, npar
      real x(*),y(*),sig(*)
      real u(nmax,*),v(mmax,*)
      real w(*),pc(*),std(*)
      real ee(mmax)
      real chisq, tol, tp

      integer i,j,k,ierr,irem
      real sum,thresh,s
      real afunc(mmax),tmp(nmax)
      character*80 wrtbuf

* assemble the design matrix for the fit
      do i=1,ndata
         call fcn(x(i),afunc,npar)
         tp = 1.0 / sig(i)
         do j=1,npar
            u(i,j) = afunc(j) * tp
         end do
      end do

* compute the singular value decomposition of the design matrix
      call ssvdc(u,nmax,ndata,npar,w,ee,u,nmax,v,mmax,tmp,21,ierr)
      if ( ierr .ne. 0 ) then
         call wrtstr(' error :  singular value decomp failed')
      end if

* calculate the solution from the SVD / edit singular values
      thresh = tol * w(1)
      irem = 0
      do j=1,npar
         s=0.0
         if ( w(j).gt.thresh ) then
            do i=1,ndata
               s = s + u(i,j) * y(i) / sig(i)
            end do
            s = s / w(j)
         else
            irem = irem + 1
         endif
         tmp(j) = s
      end do
      do j=1,npar
         s=0.0
         do k=1,npar
            s = s + v(j,k) * tmp(k)
         end do
         pc(j) = s
      end do
      if ( irem .ne. 0 ) then
         write (wrtbuf,*)
     &   ' number of singular values below threshold :  ',irem
         call wrtstr(wrtbuf)
      end if

* calculate chi**2
      chisq=0.0
      do i=1,ndata
         call fcn(x(i),afunc,npar)
         sum=0.0
         do j=1,npar
            sum = sum + pc(j) * afunc(j)
         end do
         chisq = chisq + ( (y(i)-sum) / sig(i) )**2
      end do
      
* calculate standard deviations
      do i=1,npar
         do j=1,i
            sum = 0.0
            do k=1,npar
               sum = sum + (v(i,k)/w(k)) * (v(j,k)/w(k))
            end do
         end do
         std(i) = sqrt(sum)
      end do
  
      return
      end

* Revision history:
* -----------------
* $Log: svd.f,v $
* Revision 1.3  1996/07/14 00:59:43  ulf
* small changes, fcn not declared external
*
* Revision 1.2  1996/07/09 03:02:48  ulf
* make the calculation of the standard deviation less 'Numerical Recipes' - like
*
* Revision 1.1  1996/07/07 18:52:21  ulf
* Initial revision
*
*

*************************************************************************
*                         SLATEC Subroutines
*************************************************************************

*DECK SSVDC
      subroutine ssvdc (x, ldx, n, p, s, e, u, ldu, v, ldv, work, job,
     +   info)
C***BEGIN PROLOGUE  SSVDC
C***PURPOSE  Perform the singular value decomposition of a rectangular
C            matrix.
C***LIBRARY   SLATEC (LINPACK)
C***CATEGORY  D6
C***TYPE      SINGLE PRECISION (SSVDC-S, DSVDC-D, CSVDC-C)
C***KEYWORDS  LINEAR ALGEBRA, LINPACK, MATRIX,
C             SINGULAR VALUE DECOMPOSITION
C***AUTHOR  Stewart, G. W., (U. of Maryland)
C***DESCRIPTION
C
C     SSVDC is a subroutine to reduce a real NxP matrix X by orthogonal
C     transformations U and V to diagonal form.  The elements S(I) are
C     the singular values of X.  The columns of U are the corresponding
C     left singular vectors, and the columns of V the right singular
C     vectors.
C
C     On Entry
C
C         X         REAL(LDX,P), where LDX .GE. N.
C                   X contains the matrix whose singular value
C                   decomposition is to be computed.  X is
C                   destroyed by SSVDC.
C
C         LDX       INTEGER
C                   LDX is the leading dimension of the array X.
C
C         N         INTEGER
C                   N is the number of rows of the matrix X.
C
C         P         INTEGER
C                   P is the number of columns of the matrix X.
C
C         LDU       INTEGER
C                   LDU is the leading dimension of the array U.
C                   (See below).
C
C         LDV       INTEGER
C                   LDV is the leading dimension of the array V.
C                   (See below).
C
C         WORK      REAL(N)
C                   work is a scratch array.
C
C         JOB       INTEGER
C                   JOB controls the computation of the singular
C                   vectors.  It has the decimal expansion AB
C                   with the following meaning
C
C                        A .EQ. 0  Do not compute the left singular
C                                  vectors.
C                        A .EQ. 1  Return the N left singular vectors
C                                  in U.
C                        A .GE. 2  Return the first MIN(N,P) singular
C                                  vectors in U.
C                        B .EQ. 0  Do not compute the right singular
C                                  vectors.
C                        B .EQ. 1  Return the right singular vectors
C                                  in V.
C
C     On Return
C
C         S         REAL(MM), where MM=MIN(N+1,P).
C                   The first MIN(N,P) entries of S contain the
C                   singular values of X arranged in descending
C                   order of magnitude.
C
C         E         REAL(P).
C                   E ordinarily contains zeros.  However, see the
C                   discussion of INFO for exceptions.
C
C         U         REAL(LDU,K), where LDU .GE. N.  If JOBA .EQ. 1, then
C                                   K .EQ. N.  If JOBA .GE. 2 , then
C                                   K .EQ. MIN(N,P).
C                   U contains the matrix of right singular vectors.
C                   U is not referenced if JOBA .EQ. 0.  If N .LE. P
C                   or if JOBA .EQ. 2, then U may be identified with X
C                   in the subroutine call.
C
C         V         REAL(LDV,P), where LDV .GE. P.
C                   V contains the matrix of right singular vectors.
C                   V is not referenced if JOB .EQ. 0.  If P .LE. N,
C                   then V may be identified with X in the
C                   subroutine call.
C
C         INFO      INTEGER.
C                   the singular values (and their corresponding
C                   singular vectors) S(INFO+1),S(INFO+2),...,S(M)
C                   are correct (here M=MIN(N,P)).  Thus if
C                   INFO .EQ. 0, all the singular values and their
C                   vectors are correct.  In any event, the matrix
C                   B = TRANS(U)*X*V is the bidiagonal matrix
C                   with the elements of S on its diagonal and the
C                   elements of E on its super-diagonal (TRANS(U)
C                   is the transpose of U).  Thus the singular
C                   values of X and B are the same.
C
C***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
C                 Stewart, LINPACK Users' Guide, SIAM, 1979.
C***ROUTINES CALLED  SAXPY, SDOT, SNRM2, SROT, SROTG, SSCAL, SSWAP
C***REVISION HISTORY  (YYMMDD)
C   790319  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  SSVDC
      integer ldx,n,p,ldu,ldv,job,info
      real x(ldx,*),s(*),e(*),u(ldu,*),v(ldv,*),work(*)
C
C
      integer i,iter,j,jobu,k,kase,kk,l,ll,lls,lm1,lp1,ls,lu,m,maxit,
     1        mm,mm1,mp1,nct,nctp1,ncu,nrt,nrtp1
      real sdot,t
      real b,c,cs,el,emm1,f,g,snrm2,scale,shift,sl,sm,sn,smm1,t1,test,
     1     ztest
      logical wantu,wantv
C***FIRST EXECUTABLE STATEMENT  SSVDC
C
C     SET THE MAXIMUM NUMBER OF ITERATIONS.
C
      maxit = 30
C
C     DETERMINE WHAT IS TO BE COMPUTED.
C
      wantu = .false.
      wantv = .false.
      jobu = mod(job,100)/10
      ncu = n
      if (jobu .gt. 1) ncu = min(n,p)
      if (jobu .ne. 0) wantu = .true.
      if (mod(job,10) .ne. 0) wantv = .true.
C
C     REDUCE X TO BIDIAGONAL FORM, STORING THE DIAGONAL ELEMENTS
C     IN S AND THE SUPER-DIAGONAL ELEMENTS IN E.
C
      info = 0
      nct = min(n-1,p)
      nrt = max(0,min(p-2,n))
      lu = max(nct,nrt)
      if (lu .lt. 1) go to 170
      do 160 l = 1, lu
         lp1 = l + 1
         if (l .gt. nct) go to 20
C
C           COMPUTE THE TRANSFORMATION FOR THE L-TH COLUMN AND
C           PLACE THE L-TH DIAGONAL IN S(L).
C
            s(l) = snrm2(n-l+1,x(l,l),1)
            if (s(l) .eq. 0.0e0) go to 10
               if (x(l,l) .ne. 0.0e0) s(l) = sign(s(l),x(l,l))
               call sscal(n-l+1,1.0e0/s(l),x(l,l),1)
               x(l,l) = 1.0e0 + x(l,l)
   10       continue
            s(l) = -s(l)
   20    continue
         if (p .lt. lp1) go to 50
         do 40 j = lp1, p
            if (l .gt. nct) go to 30
            if (s(l) .eq. 0.0e0) go to 30
C
C              APPLY THE TRANSFORMATION.
C
               t = -sdot(n-l+1,x(l,l),1,x(l,j),1)/x(l,l)
               call saxpy(n-l+1,t,x(l,l),1,x(l,j),1)
   30       continue
C
C           PLACE THE L-TH ROW OF X INTO  E FOR THE
C           SUBSEQUENT CALCULATION OF THE ROW TRANSFORMATION.
C
            e(j) = x(l,j)
   40    continue
   50    continue
         if (.not.wantu .or. l .gt. nct) go to 70
C
C           PLACE THE TRANSFORMATION IN U FOR SUBSEQUENT BACK
C           MULTIPLICATION.
C
            do 60 i = l, n
               u(i,l) = x(i,l)
   60       continue
   70    continue
         if (l .gt. nrt) go to 150
C
C           COMPUTE THE L-TH ROW TRANSFORMATION AND PLACE THE
C           L-TH SUPER-DIAGONAL IN E(L).
C
            e(l) = snrm2(p-l,e(lp1),1)
            if (e(l) .eq. 0.0e0) go to 80
               if (e(lp1) .ne. 0.0e0) e(l) = sign(e(l),e(lp1))
               call sscal(p-l,1.0e0/e(l),e(lp1),1)
               e(lp1) = 1.0e0 + e(lp1)
   80       continue
            e(l) = -e(l)
            if (lp1 .gt. n .or. e(l) .eq. 0.0e0) go to 120
C
C              APPLY THE TRANSFORMATION.
C
               do 90 i = lp1, n
                  work(i) = 0.0e0
   90          continue
               do 100 j = lp1, p
                  call saxpy(n-l,e(j),x(lp1,j),1,work(lp1),1)
  100          continue
               do 110 j = lp1, p
                  call saxpy(n-l,-e(j)/e(lp1),work(lp1),1,x(lp1,j),1)
  110          continue
  120       continue
            if (.not.wantv) go to 140
C
C              PLACE THE TRANSFORMATION IN V FOR SUBSEQUENT
C              BACK MULTIPLICATION.
C
               do 130 i = lp1, p
                  v(i,l) = e(i)
  130          continue
  140       continue
  150    continue
  160 continue
  170 continue
C
C     SET UP THE FINAL BIDIAGONAL MATRIX OR ORDER M.
C
      m = min(p,n+1)
      nctp1 = nct + 1
      nrtp1 = nrt + 1
      if (nct .lt. p) s(nctp1) = x(nctp1,nctp1)
      if (n .lt. m) s(m) = 0.0e0
      if (nrtp1 .lt. m) e(nrtp1) = x(nrtp1,m)
      e(m) = 0.0e0
C
C     IF REQUIRED, GENERATE U.
C
      if (.not.wantu) go to 300
         if (ncu .lt. nctp1) go to 200
         do 190 j = nctp1, ncu
            do 180 i = 1, n
               u(i,j) = 0.0e0
  180       continue
            u(j,j) = 1.0e0
  190    continue
  200    continue
         if (nct .lt. 1) go to 290
         do 280 ll = 1, nct
            l = nct - ll + 1
            if (s(l) .eq. 0.0e0) go to 250
               lp1 = l + 1
               if (ncu .lt. lp1) go to 220
               do 210 j = lp1, ncu
                  t = -sdot(n-l+1,u(l,l),1,u(l,j),1)/u(l,l)
                  call saxpy(n-l+1,t,u(l,l),1,u(l,j),1)
  210          continue
  220          continue
               call sscal(n-l+1,-1.0e0,u(l,l),1)
               u(l,l) = 1.0e0 + u(l,l)
               lm1 = l - 1
               if (lm1 .lt. 1) go to 240
               do 230 i = 1, lm1
                  u(i,l) = 0.0e0
  230          continue
  240          continue
            go to 270
  250       continue
               do 260 i = 1, n
                  u(i,l) = 0.0e0
  260          continue
               u(l,l) = 1.0e0
  270       continue
  280    continue
  290    continue
  300 continue
C
C     IF IT IS REQUIRED, GENERATE V.
C
      if (.not.wantv) go to 350
         do 340 ll = 1, p
            l = p - ll + 1
            lp1 = l + 1
            if (l .gt. nrt) go to 320
            if (e(l) .eq. 0.0e0) go to 320
               do 310 j = lp1, p
                  t = -sdot(p-l,v(lp1,l),1,v(lp1,j),1)/v(lp1,l)
                  call saxpy(p-l,t,v(lp1,l),1,v(lp1,j),1)
  310          continue
  320       continue
            do 330 i = 1, p
               v(i,l) = 0.0e0
  330       continue
            v(l,l) = 1.0e0
  340    continue
  350 continue
C
C     MAIN ITERATION LOOP FOR THE SINGULAR VALUES.
C
      mm = m
      iter = 0
  360 continue
C
C        QUIT IF ALL THE SINGULAR VALUES HAVE BEEN FOUND.
C
         if (m .eq. 0) go to 620
C
C        IF TOO MANY ITERATIONS HAVE BEEN PERFORMED, SET
C        FLAG AND RETURN.
C
         if (iter .lt. maxit) go to 370
            info = m
            go to 620
  370    continue
C
C        THIS SECTION OF THE PROGRAM INSPECTS FOR
C        NEGLIGIBLE ELEMENTS IN THE S AND E ARRAYS.  ON
C        COMPLETION THE VARIABLES KASE AND L ARE SET AS FOLLOWS.
C
C           KASE = 1     IF S(M) AND E(L-1) ARE NEGLIGIBLE AND L.LT.M
C           KASE = 2     IF S(L) IS NEGLIGIBLE AND L.LT.M
C           KASE = 3     IF E(L-1) IS NEGLIGIBLE, L.LT.M, AND
C                        S(L), ..., S(M) ARE NOT NEGLIGIBLE (QR STEP).
C           KASE = 4     IF E(M-1) IS NEGLIGIBLE (CONVERGENCE).
C
         do 390 ll = 1, m
            l = m - ll
            if (l .eq. 0) go to 400
            test = abs(s(l)) + abs(s(l+1))
            ztest = test + abs(e(l))
            if (ztest .ne. test) go to 380
               e(l) = 0.0e0
               go to 400
  380       continue
  390    continue
  400    continue
         if (l .ne. m - 1) go to 410
            kase = 4
         go to 480
  410    continue
            lp1 = l + 1
            mp1 = m + 1
            do 430 lls = lp1, mp1
               ls = m - lls + lp1
               if (ls .eq. l) go to 440
               test = 0.0e0
               if (ls .ne. m) test = test + abs(e(ls))
               if (ls .ne. l + 1) test = test + abs(e(ls-1))
               ztest = test + abs(s(ls))
               if (ztest .ne. test) go to 420
                  s(ls) = 0.0e0
                  go to 440
  420          continue
  430       continue
  440       continue
            if (ls .ne. l) go to 450
               kase = 3
            go to 470
  450       continue
            if (ls .ne. m) go to 460
               kase = 1
            go to 470
  460       continue
               kase = 2
               l = ls
  470       continue
  480    continue
         l = l + 1
C
C        PERFORM THE TASK INDICATED BY KASE.
C
         go to (490,520,540,570), kase
C
C        DEFLATE NEGLIGIBLE S(M).
C
  490    continue
            mm1 = m - 1
            f = e(m-1)
            e(m-1) = 0.0e0
            do 510 kk = l, mm1
               k = mm1 - kk + l
               t1 = s(k)
               call srotg(t1,f,cs,sn)
               s(k) = t1
               if (k .eq. l) go to 500
                  f = -sn*e(k-1)
                  e(k-1) = cs*e(k-1)
  500          continue
               if (wantv) call srot(p,v(1,k),1,v(1,m),1,cs,sn)
  510       continue
         go to 610
C
C        SPLIT AT NEGLIGIBLE S(L).
C
  520    continue
            f = e(l-1)
            e(l-1) = 0.0e0
            do 530 k = l, m
               t1 = s(k)
               call srotg(t1,f,cs,sn)
               s(k) = t1
               f = -sn*e(k)
               e(k) = cs*e(k)
               if (wantu) call srot(n,u(1,k),1,u(1,l-1),1,cs,sn)
  530       continue
         go to 610
C
C        PERFORM ONE QR STEP.
C
  540    continue
C
C           CALCULATE THE SHIFT.
C
            scale = max(abs(s(m)),abs(s(m-1)),abs(e(m-1)),abs(s(l)),
     1                    abs(e(l)))
            sm = s(m)/scale
            smm1 = s(m-1)/scale
            emm1 = e(m-1)/scale
            sl = s(l)/scale
            el = e(l)/scale
            b = ((smm1 + sm)*(smm1 - sm) + emm1**2)/2.0e0
            c = (sm*emm1)**2
            shift = 0.0e0
            if (b .eq. 0.0e0 .and. c .eq. 0.0e0) go to 550
               shift = sqrt(b**2+c)
               if (b .lt. 0.0e0) shift = -shift
               shift = c/(b + shift)
  550       continue
            f = (sl + sm)*(sl - sm) - shift
            g = sl*el
C
C           CHASE ZEROS.
C
            mm1 = m - 1
            do 560 k = l, mm1
               call srotg(f,g,cs,sn)
               if (k .ne. l) e(k-1) = f
               f = cs*s(k) + sn*e(k)
               e(k) = cs*e(k) - sn*s(k)
               g = sn*s(k+1)
               s(k+1) = cs*s(k+1)
               if (wantv) call srot(p,v(1,k),1,v(1,k+1),1,cs,sn)
               call srotg(f,g,cs,sn)
               s(k) = f
               f = cs*e(k) + sn*s(k+1)
               s(k+1) = -sn*e(k) + cs*s(k+1)
               g = sn*e(k+1)
               e(k+1) = cs*e(k+1)
               if (wantu .and. k .lt. n)
     1            call srot(n,u(1,k),1,u(1,k+1),1,cs,sn)
  560       continue
            e(m-1) = f
            iter = iter + 1
         go to 610
C
C        CONVERGENCE.
C
  570    continue
C
C           MAKE THE SINGULAR VALUE  POSITIVE.
C
            if (s(l) .ge. 0.0e0) go to 580
               s(l) = -s(l)
               if (wantv) call sscal(p,-1.0e0,v(1,l),1)
  580       continue
C
C           ORDER THE SINGULAR VALUE.
C
  590       if (l .eq. mm) go to 600
               if (s(l) .ge. s(l+1)) go to 600
               t = s(l)
               s(l) = s(l+1)
               s(l+1) = t
               if (wantv .and. l .lt. p)
     1            call sswap(p,v(1,l),1,v(1,l+1),1)
               if (wantu .and. l .lt. n)
     1            call sswap(n,u(1,l),1,u(1,l+1),1)
               l = l + 1
            go to 590
  600       continue
            iter = 0
            m = m - 1
  610    continue
      go to 360
  620 continue
      return
      end

