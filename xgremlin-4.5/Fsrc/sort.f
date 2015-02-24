* ======================================================================
* NIST Guide to Available Math Software.
* Fullsource for module ISORT from package CMLIB.
* Retrieved from CAMSUN on Fri Jun 28 23:23:09 1996.
* ======================================================================
      subroutine issort(x,y,n,kflag)
*
* sorts a integer array IX and carries a real array Y along
*
C***BEGIN PROLOGUE  ISORT
C***DATE WRITTEN   761118   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  N6A2A
C***KEYWORDS  QUICKSORT,SINGLETON QUICKSORT,SORT,SORTING
C***AUTHOR  JONES, R. E., (SNLA)
C           KAHANER, D. K., (NBS)
C           WISNIEWSKI, J. A., (SNLA)
C***PURPOSE  ISORT sorts integer array X and optionally makes the same
C            interchanges in real array Y.  The array X may be
C            sorted in increasing order or decreasing order.  A
C            slightly modified QUICKSORT algorithm is used.
C***DESCRIPTION
C
C     Written by Rondall E Jones
C     Modified by John A. Wisniewski to use the Singleton QUICKSORT
C     algorithm. Date 18 November 1976.
C
C     Further modified by David K. Kahaner
C     NATIONAL BUREAU OF STANDARDS
C     August, 1981
C
C     Abstract
C         ISORT sorts integer array X and optionally makes the same
C         interchanges in integer array Y.  The array X may be sorted in
C         INCREASING order or DECREASING order.  A slightly modified
C         QUICKSORT algorithm is used.
C
C     Reference
C         Singleton,R.C., Algorithm 347, An Efficient Algorithm For
C         Sorting With Minimal Storage, CACM,12(3),1969,185-7.
C
C     Description of Parameters
C         X - integer array of values to be sorted
C         Y - integer array to be (optionally) carried along
C         N - number of values in integer array X to be sorted
C     KFLAG - control parameter
C           = 2 means sort X in INCREASING order and carry Y along.
C           = 1 means sort X in INCREASING order (ignoring Y)
C           =-1 means sort X in DECREASING order (ignoring Y)
C           =-2 means sort X in DECREASING order and carry Y along.
C***REFERENCES  SINGLETON, R. C., ALGORITHM 347, AN EFFICIENT
C                 ALGORITHM FOR SORTING WITH MINIMAL STORAGE, CACM,
C                 VOL. 12, NO. 3, 1969, PP. 185-187.
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  ISORT
      dimension il(21),iu(21)
      integer x(n),t,tt
      real y(n), ty, tty
C***FIRST EXECUTABLE STATEMENT  ISORT
      nn = n
      if (nn.ge.1) go to 10
      kflag = -99
      return
   10 kk = iabs(kflag)
      if ((kk.eq.1).or.(kk.eq.2)) go to 15
      kflag = -99
      return
C
C ALTER ARRAY X TO GET DECREASING ORDER IF NEEDED
C
   15 if (kflag.ge.1) go to 30
      do 20 i=1,nn
   20 x(i) = -x(i)
   30 go to (100,200),kk
C
C SORT X ONLY
C
  100 continue
      m=1
      i=1
      j=nn
      r=.375
  110 if (i .eq. j) go to 155
  115 if (r .gt. .5898437) go to 120
      r=r+3.90625e-2
      go to 125
  120 r=r-.21875
  125 k=i
C                                  SELECT A CENTRAL ELEMENT OF THE
C                                  ARRAY AND SAVE IT IN LOCATION T
      ij = i + ifix (float (j-i) * r)
      t=x(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      if (x(i) .le. t) go to 130
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
  130 l=j
C                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
C                                  T, INTERCHANGE WITH T
      if (x(j) .ge. t) go to 140
      x(ij)=x(j)
      x(j)=t
      t=x(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      if (x(i) .le. t) go to 140
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
      go to 140
  135 tt=x(l)
      x(l)=x(k)
      x(k)=tt
C                                  FIND AN ELEMENT IN THE SECOND HALF OF
C                                  THE ARRAY WHICH IS SMALLER THAN T
  140 l=l-1
      if (x(l) .gt. t) go to 140
C                                  FIND AN ELEMENT IN THE FIRST HALF OF
C                                  THE ARRAY WHICH IS GREATER THAN T
  145 k=k+1
      if (x(k) .lt. t) go to 145
C                                  INTERCHANGE THESE ELEMENTS
      if (k .le. l) go to 135
C                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
C                                  THE ARRAY YET TO BE SORTED
      if (l-i .le. j-k) go to 150
      il(m)=i
      iu(m)=l
      i=k
      m=m+1
      go to 160
  150 il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 160
C                                  BEGIN AGAIN ON ANOTHER PORTION OF
C                                  THE UNSORTED ARRAY
  155 m=m-1
      if (m .eq. 0) go to 300
      i=il(m)
      j=iu(m)
  160 if (j-i .ge. 1) go to 125
      if (i .eq. 1) go to 110
      i=i-1
  165 i=i+1
      if (i .eq. j) go to 155
      t=x(i+1)
      if (x(i) .le. t) go to 165
      k=i
  170 x(k+1)=x(k)
      k=k-1
      if (t .lt. x(k)) go to 170
      x(k+1)=t
      go to 165
C
C SORT X AND CARRY Y ALONG
C
  200 continue
      m=1
      i=1
      j=nn
      r=.375
  210 if (i .eq. j) go to 255
  215 if (r .gt. .5898437) go to 220
      r=r+3.90625e-2
      go to 225
  220 r=r-.21875
  225 k=i
C                                  SELECT A CENTRAL ELEMENT OF THE
C                                  ARRAY AND SAVE IT IN LOCATION T
      ij = i + ifix (float (j-i) *r)
      t=x(ij)
      ty= y(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      if (x(i) .le. t) go to 230
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
       y(ij)= y(i)
       y(i)=ty
      ty= y(ij)
  230 l=j
C                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
C                                  T, INTERCHANGE WITH T
      if (x(j) .ge. t) go to 240
      x(ij)=x(j)
      x(j)=t
      t=x(ij)
       y(ij)= y(j)
       y(j)=ty
      ty= y(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      if (x(i) .le. t) go to 240
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
       y(ij)= y(i)
       y(i)=ty
      ty= y(ij)
      go to 240
  235 tt=x(l)
      x(l)=x(k)
      x(k)=tt
      tty= y(l)
       y(l)= y(k)
       y(k)=tty
C                                  FIND AN ELEMENT IN THE SECOND HALF OF
C                                  THE ARRAY WHICH IS SMALLER THAN T
  240 l=l-1
      if (x(l) .gt. t) go to 240
C                                  FIND AN ELEMENT IN THE FIRST HALF OF
C                                  THE ARRAY WHICH IS GREATER THAN T
  245 k=k+1
      if (x(k) .lt. t) go to 245
C                                  INTERCHANGE THESE ELEMENTS
      if (k .le. l) go to 235
C                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
C                                  THE ARRAY YET TO BE SORTED
      if (l-i .le. j-k) go to 250
      il(m)=i
      iu(m)=l
      i=k
      m=m+1
      go to 260
  250 il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 260
C                                  BEGIN AGAIN ON ANOTHER PORTION OF
C                                  THE UNSORTED ARRAY
  255 m=m-1
      if (m .eq. 0) go to 300
      i=il(m)
      j=iu(m)
  260 if (j-i .ge. 1) go to 225
      if (i .eq. 1) go to 210
      i=i-1
  265 i=i+1
      if (i .eq. j) go to 255
      t=x(i+1)
      ty= y(i+1)
      if (x(i) .le. t) go to 265
      k=i
  270 x(k+1)=x(k)
       y(k+1)= y(k)
      k=k-1
      if (t .lt. x(k)) go to 270
      x(k+1)=t
       y(k+1)=ty
      go to 265
C
C CLEAN UP
C
  300 if (kflag.ge.1) return
      do 310 i=1,nn
  310 x(i) = -x(i)
      return
      end

* ======================================================================
* NIST Guide to Available Math Software.
* Fullsource for module ISORT from package CMLIB.
* Retrieved from CAMSUN on Fri Jun 28 23:23:09 1996.
* ======================================================================
      subroutine idsort(x,y,n,kflag)
*
* sorts a integer array IX and carries a double precision array Y along
*
C***BEGIN PROLOGUE  ISORT
C***DATE WRITTEN   761118   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  N6A2A
C***KEYWORDS  QUICKSORT,SINGLETON QUICKSORT,SORT,SORTING
C***AUTHOR  JONES, R. E., (SNLA)
C           KAHANER, D. K., (NBS)
C           WISNIEWSKI, J. A., (SNLA)
C***PURPOSE  ISORT sorts integer array X and optionally makes the same
C            interchanges in double precision array Y.  The array X may be
C            sorted in increasing order or decreasing order.  A
C            slightly modified QUICKSORT algorithm is used.
C***DESCRIPTION
C
C     Written by Rondall E Jones
C     Modified by John A. Wisniewski to use the Singleton QUICKSORT
C     algorithm. Date 18 November 1976.
C
C     Further modified by David K. Kahaner
C     NATIONAL BUREAU OF STANDARDS
C     August, 1981
C
C     Abstract
C         ISORT sorts integer array X and optionally makes the same
C         interchanges in integer array Y.  The array X may be sorted in
C         INCREASING order or DECREASING order.  A slightly modified
C         QUICKSORT algorithm is used.
C
C     Reference
C         Singleton,R.C., Algorithm 347, An Efficient Algorithm For
C         Sorting With Minimal Storage, CACM,12(3),1969,185-7.
C
C     Description of Parameters
C         X - integer array of values to be sorted
C         Y - integer array to be (optionally) carried along
C         N - number of values in integer array X to be sorted
C     KFLAG - control parameter
C           = 2 means sort X in INCREASING order and carry Y along.
C           = 1 means sort X in INCREASING order (ignoring Y)
C           =-1 means sort X in DECREASING order (ignoring Y)
C           =-2 means sort X in DECREASING order and carry Y along.
C***REFERENCES  SINGLETON, R. C., ALGORITHM 347, AN EFFICIENT
C                 ALGORITHM FOR SORTING WITH MINIMAL STORAGE, CACM,
C                 VOL. 12, NO. 3, 1969, PP. 185-187.
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  ISORT
      dimension il(21),iu(21)
      integer x(n),t,tt
      double precision y(n), ty, tty
C***FIRST EXECUTABLE STATEMENT  ISORT
      nn = n
      if (nn.ge.1) go to 10
      kflag = -99
      return
   10 kk = iabs(kflag)
      if ((kk.eq.1).or.(kk.eq.2)) go to 15
      kflag = -99
      return
C
C ALTER ARRAY X TO GET DECREASING ORDER IF NEEDED
C
   15 if (kflag.ge.1) go to 30
      do 20 i=1,nn
   20 x(i) = -x(i)
   30 go to (100,200),kk
C
C SORT X ONLY
C
  100 continue
      m=1
      i=1
      j=nn
      r=.375
  110 if (i .eq. j) go to 155
  115 if (r .gt. .5898437) go to 120
      r=r+3.90625e-2
      go to 125
  120 r=r-.21875
  125 k=i
C                                  SELECT A CENTRAL ELEMENT OF THE
C                                  ARRAY AND SAVE IT IN LOCATION T
      ij = i + ifix (float (j-i) * r)
      t=x(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      if (x(i) .le. t) go to 130
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
  130 l=j
C                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
C                                  T, INTERCHANGE WITH T
      if (x(j) .ge. t) go to 140
      x(ij)=x(j)
      x(j)=t
      t=x(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      if (x(i) .le. t) go to 140
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
      go to 140
  135 tt=x(l)
      x(l)=x(k)
      x(k)=tt
C                                  FIND AN ELEMENT IN THE SECOND HALF OF
C                                  THE ARRAY WHICH IS SMALLER THAN T
  140 l=l-1
      if (x(l) .gt. t) go to 140
C                                  FIND AN ELEMENT IN THE FIRST HALF OF
C                                  THE ARRAY WHICH IS GREATER THAN T
  145 k=k+1
      if (x(k) .lt. t) go to 145
C                                  INTERCHANGE THESE ELEMENTS
      if (k .le. l) go to 135
C                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
C                                  THE ARRAY YET TO BE SORTED
      if (l-i .le. j-k) go to 150
      il(m)=i
      iu(m)=l
      i=k
      m=m+1
      go to 160
  150 il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 160
C                                  BEGIN AGAIN ON ANOTHER PORTION OF
C                                  THE UNSORTED ARRAY
  155 m=m-1
      if (m .eq. 0) go to 300
      i=il(m)
      j=iu(m)
  160 if (j-i .ge. 1) go to 125
      if (i .eq. 1) go to 110
      i=i-1
  165 i=i+1
      if (i .eq. j) go to 155
      t=x(i+1)
      if (x(i) .le. t) go to 165
      k=i
  170 x(k+1)=x(k)
      k=k-1
      if (t .lt. x(k)) go to 170
      x(k+1)=t
      go to 165
C
C SORT X AND CARRY Y ALONG
C
  200 continue
      m=1
      i=1
      j=nn
      r=.375
  210 if (i .eq. j) go to 255
  215 if (r .gt. .5898437) go to 220
      r=r+3.90625e-2
      go to 225
  220 r=r-.21875
  225 k=i
C                                  SELECT A CENTRAL ELEMENT OF THE
C                                  ARRAY AND SAVE IT IN LOCATION T
      ij = i + ifix (float (j-i) *r)
      t=x(ij)
      ty= y(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      if (x(i) .le. t) go to 230
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
       y(ij)= y(i)
       y(i)=ty
      ty= y(ij)
  230 l=j
C                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
C                                  T, INTERCHANGE WITH T
      if (x(j) .ge. t) go to 240
      x(ij)=x(j)
      x(j)=t
      t=x(ij)
       y(ij)= y(j)
       y(j)=ty
      ty= y(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      if (x(i) .le. t) go to 240
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
       y(ij)= y(i)
       y(i)=ty
      ty= y(ij)
      go to 240
  235 tt=x(l)
      x(l)=x(k)
      x(k)=tt
      tty= y(l)
       y(l)= y(k)
       y(k)=tty
C                                  FIND AN ELEMENT IN THE SECOND HALF OF
C                                  THE ARRAY WHICH IS SMALLER THAN T
  240 l=l-1
      if (x(l) .gt. t) go to 240
C                                  FIND AN ELEMENT IN THE FIRST HALF OF
C                                  THE ARRAY WHICH IS GREATER THAN T
  245 k=k+1
      if (x(k) .lt. t) go to 245
C                                  INTERCHANGE THESE ELEMENTS
      if (k .le. l) go to 235
C                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
C                                  THE ARRAY YET TO BE SORTED
      if (l-i .le. j-k) go to 250
      il(m)=i
      iu(m)=l
      i=k
      m=m+1
      go to 260
  250 il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 260
C                                  BEGIN AGAIN ON ANOTHER PORTION OF
C                                  THE UNSORTED ARRAY
  255 m=m-1
      if (m .eq. 0) go to 300
      i=il(m)
      j=iu(m)
  260 if (j-i .ge. 1) go to 225
      if (i .eq. 1) go to 210
      i=i-1
  265 i=i+1
      if (i .eq. j) go to 255
      t=x(i+1)
      ty= y(i+1)
      if (x(i) .le. t) go to 265
      k=i
  270 x(k+1)=x(k)
       y(k+1)= y(k)
      k=k-1
      if (t .lt. x(k)) go to 270
      x(k+1)=t
       y(k+1)=ty
      go to 265
C
C CLEAN UP
C
  300 if (kflag.ge.1) return
      do 310 i=1,nn
  310 x(i) = -x(i)
      return
      end

* ======================================================================
* NIST Guide to Available Math Software.
* Fullsource for module SSORT from package CMLIB.
* Retrieved from CAMSUN on Fri Jun 28 23:26:09 1996.
* ======================================================================
      subroutine ssort(x,y,n,kflag)
*
* sorts a real array X and sorts another array Y alongside
*
C***BEGIN PROLOGUE  SSORT
C***DATE WRITTEN   761101   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  N6A2B1
C***KEYWORDS  QUICKSORT,SINGLETON QUICKSORT,SORT,SORTING
C***AUTHOR  JONES, R. E., (SNLA)
C           WISNIEWSKI, J. A., (SNLA)
C***PURPOSE  SSORT sorts array X and optionally makes the same
C            interchanges in array Y.  The array X may be sorted in
C            increasing order or decreasing order.  A slightly modified
C            QUICKSORT algorithm is used.
C***DESCRIPTION
C
C     Written by Rondall E. Jones
C     Modified by John A. Wisniewski to use the Singleton quicksort
C     algorithm.  Date 18 November 1976.
C
C     Abstract
C         SSORT sorts array X and optionally makes the same
C         interchanges in array Y.  The array X may be sorted in
C         increasing order or decreasing order.  A slightly modified
C         quicksort algorithm is used.
C
C     Reference
C         Singleton, R. C., Algorithm 347, An Efficient Algorithm for
C         Sorting with Minimal Storage, CACM,12(3),1969,185-7.
C
C     Description of Parameters
C         X - array of values to be sorted   (usually abscissas)
C         Y - array to be (optionally) carried along
C         N - number of values in array X to be sorted
C         KFLAG - control parameter
C             =2  means sort X in increasing order and carry Y along.
C             =1  means sort X in increasing order (ignoring Y)
C             =-1 means sort X in decreasing order (ignoring Y)
C             =-2 means sort X in decreasing order and carry Y along.
C***REFERENCES  SINGLETON,R.C., ALGORITHM 347, AN EFFICIENT ALGORITHM
C                 FOR SORTING WITH MINIMAL STORAGE, CACM,12(3),1969,
C                 185-7.
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  SSORT
      dimension x(n),y(n),il(21),iu(21)
C***FIRST EXECUTABLE STATEMENT  SSORT
      nn = n
      if (nn.ge.1) go to 10
      kflag = -99
      return
   10 kk = iabs(kflag)
      if ((kk.eq.1).or.(kk.eq.2)) go to 15
      kflag = -99
      return
C
C ALTER ARRAY X TO GET DECREASING ORDER IF NEEDED
C
   15 if (kflag.ge.1) go to 30
      do 20 i=1,nn
   20 x(i) = -x(i)
   30 go to (100,200),kk
C
C SORT X ONLY
C
  100 continue
      m=1
      i=1
      j=nn
      r=.375
  110 if (i .eq. j) go to 155
  115 if (r .gt. .5898437) go to 120
      r=r+3.90625e-2
      go to 125
  120 r=r-.21875
  125 k=i
C                                  SELECT A CENTRAL ELEMENT OF THE
C                                  ARRAY AND SAVE IT IN LOCATION T
      ij = i + ifix (float (j-i) * r)
      t=x(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      if (x(i) .le. t) go to 130
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
  130 l=j
C                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
C                                  T, INTERCHANGE WITH T
      if (x(j) .ge. t) go to 140
      x(ij)=x(j)
      x(j)=t
      t=x(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      if (x(i) .le. t) go to 140
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
      go to 140
  135 tt=x(l)
      x(l)=x(k)
      x(k)=tt
C                                  FIND AN ELEMENT IN THE SECOND HALF OF
C                                  THE ARRAY WHICH IS SMALLER THAN T
  140 l=l-1
      if (x(l) .gt. t) go to 140
C                                  FIND AN ELEMENT IN THE FIRST HALF OF
C                                  THE ARRAY WHICH IS GREATER THAN T
  145 k=k+1
      if (x(k) .lt. t) go to 145
C                                  INTERCHANGE THESE ELEMENTS
      if (k .le. l) go to 135
C                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
C                                  THE ARRAY YET TO BE SORTED
      if (l-i .le. j-k) go to 150
      il(m)=i
      iu(m)=l
      i=k
      m=m+1
      go to 160
  150 il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 160
C                                  BEGIN AGAIN ON ANOTHER PORTION OF
C                                  THE UNSORTED ARRAY
  155 m=m-1
      if (m .eq. 0) go to 300
      i=il(m)
      j=iu(m)
  160 if (j-i .ge. 1) go to 125
      if (i .eq. 1) go to 110
      i=i-1
  165 i=i+1
      if (i .eq. j) go to 155
      t=x(i+1)
      if (x(i) .le. t) go to 165
      k=i
  170 x(k+1)=x(k)
      k=k-1
      if (t .lt. x(k)) go to 170
      x(k+1)=t
      go to 165
C
C SORT X AND CARRY Y ALONG
C
  200 continue
      m=1
      i=1
      j=nn
      r=.375
  210 if (i .eq. j) go to 255
  215 if (r .gt. .5898437) go to 220
      r=r+3.90625e-2
      go to 225
  220 r=r-.21875
  225 k=i
C                                  SELECT A CENTRAL ELEMENT OF THE
C                                  ARRAY AND SAVE IT IN LOCATION T
      ij = i + ifix (float (j-i) *r)
      t=x(ij)
      ty= y(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      if (x(i) .le. t) go to 230
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
       y(ij)= y(i)
       y(i)=ty
      ty= y(ij)
  230 l=j
C                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
C                                  T, INTERCHANGE WITH T
      if (x(j) .ge. t) go to 240
      x(ij)=x(j)
      x(j)=t
      t=x(ij)
       y(ij)= y(j)
       y(j)=ty
      ty= y(ij)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      if (x(i) .le. t) go to 240
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
       y(ij)= y(i)
       y(i)=ty
      ty= y(ij)
      go to 240
  235 tt=x(l)
      x(l)=x(k)
      x(k)=tt
      tty= y(l)
       y(l)= y(k)
       y(k)=tty
C                                  FIND AN ELEMENT IN THE SECOND HALF OF
C                                  THE ARRAY WHICH IS SMALLER THAN T
  240 l=l-1
      if (x(l) .gt. t) go to 240
C                                  FIND AN ELEMENT IN THE FIRST HALF OF
C                                  THE ARRAY WHICH IS GREATER THAN T
  245 k=k+1
      if (x(k) .lt. t) go to 245
C                                  INTERCHANGE THESE ELEMENTS
      if (k .le. l) go to 235
C                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
C                                  THE ARRAY YET TO BE SORTED
      if (l-i .le. j-k) go to 250
      il(m)=i
      iu(m)=l
      i=k
      m=m+1
      go to 260
  250 il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 260
C                                  BEGIN AGAIN ON ANOTHER PORTION OF
C                                  THE UNSORTED ARRAY
  255 m=m-1
      if (m .eq. 0) go to 300
      i=il(m)
      j=iu(m)
  260 if (j-i .ge. 1) go to 225
      if (i .eq. 1) go to 210
      i=i-1
  265 i=i+1
      if (i .eq. j) go to 255
      t=x(i+1)
      ty= y(i+1)
      if (x(i) .le. t) go to 265
      k=i
  270 x(k+1)=x(k)
       y(k+1)= y(k)
      k=k-1
      if (t .lt. x(k)) go to 270
      x(k+1)=t
       y(k+1)=ty
      go to 265
C
C CLEAN UP
C
  300 if (kflag.ge.1) return
      do 310 i=1,nn
  310 x(i) = -x(i)
      return
      end
