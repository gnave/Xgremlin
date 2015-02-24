* ======================================================================
* NIST Guide to Available Math Software.
* Fullsource for module UNI from package CMLIB.
* Retrieved from CAMSUN on Wed Jul  3 15:17:21 1996.
* ======================================================================
      real function uni(jd)
C***BEGIN PROLOGUE  UNI
C***DATE WRITTEN   810915
C***REVISION DATE  830805
C***CATEGORY NO.  L6A21
C***KEYWORDS  RANDOM NUMBERS, UNIFORM RANDOM NUMBERS
C***AUTHOR    BLUE, JAMES, SCIENTIFIC COMPUTING DIVISION, NBS
C             KAHANER, DAVID, SCIENTIFIC COMPUTING DIVISION, NBS
C             MARSAGLIA, GEORGE, COMPUTER SCIENCE DEPT., WASH STATE UNIV
C
C***PURPOSE  THIS ROUTINE GENERATES QUASI UNIFORM RANDOM NUMBERS ON [0,1
C             AND CAN BE USED ON ANY COMPUTER WITH WHICH ALLOWS INTEGERS
C             AT LEAST AS LARGE AS 32767.
C***DESCRIPTION
C
C       THIS ROUTINE GENERATES QUASI UNIFORM RANDOM NUMBERS ON THE INTER
C       [0,1).  IT CAN BE USED WITH ANY COMPUTER WHICH ALLOWS
C       INTEGERS AT LEAST AS LARGE AS 32767.
C
C
C   USE
C       FIRST TIME....
C                   Z = UNI(JD)
C                     HERE JD IS ANY  N O N - Z E R O  INTEGER.
C                     THIS CAUSES INITIALIZATION OF THE PROGRAM
C                     AND THE FIRST RANDOM NUMBER TO BE RETURNED AS Z.
C       SUBSEQUENT TIMES...
C                   Z = UNI(0)
C                     CAUSES THE NEXT RANDOM NUMBER TO BE RETURNED AS Z.
C
C
C..................................................................
C   NOTE: USERS WHO WISH TO TRANSPORT THIS PROGRAM FROM ONE COMPUTER
C         TO ANOTHER SHOULD READ THE FOLLOWING INFORMATION.....
C
C   MACHINE DEPENDENCIES...
C      MDIG = A LOWER BOUND ON THE NUMBER OF BINARY DIGITS AVAILABLE
C              FOR REPRESENTING INTEGERS, INCLUDING THE SIGN BIT.
C              THIS VALUE MUST BE AT LEAST 16, BUT MAY BE INCREASED
C              IN LINE WITH REMARK A BELOW.
C
C   REMARKS...
C     A. THIS PROGRAM CAN BE USED IN TWO WAYS:
C        (1) TO OBTAIN REPEATABLE RESULTS ON DIFFERENT COMPUTERS,
C            SET 'MDIG' TO THE SMALLEST OF ITS VALUES ON EACH, OR,
C        (2) TO ALLOW THE LONGEST SEQUENCE OF RANDOM NUMBERS TO BE
C            GENERATED WITHOUT CYCLING (REPEATING) SET 'MDIG' TO THE
C            LARGEST POSSIBLE VALUE.
C     B. THE SEQUENCE OF NUMBERS GENERATED DEPENDS ON THE INITIAL
C          INPUT 'JD' AS WELL AS THE VALUE OF 'MDIG'.
C          IF MDIG=16 ONE SHOULD FIND THAT
C            THE FIRST EVALUATION
C              Z=UNI(305) GIVES Z=.027832881...
C            THE SECOND EVALUATION
C              Z=UNI(0) GIVES   Z=.56102176...
C            THE THIRD EVALUATION
C              Z=UNI(0) GIVES   Z=.41456343...
C            THE THOUSANDTH EVALUATION
C              Z=UNI(0) GIVES   Z=.19797357...
C
C***REFERENCES  MARSAGLIA G., "COMMENTS ON THE PERFECT UNIFORM RANDOM
C                 NUMBER GENERATOR", UNPUBLISHED NOTES, WASH S. U.
C***END PROLOGUE  UNI
      integer m(17)
C
      save i,j,m,m1,m2
C
      data m(1),m(2),m(3),m(4),m(5),m(6),m(7),m(8),m(9),m(10),m(11),
     1     m(12),m(13),m(14),m(15),m(16),m(17)
     2                   / 30788,23052,2053,19346,10646,19427,23975,
     3                     19049,10949,19693,29746,26748,2796,23890,
     4                     29168,31924,16499 /
      data m1,m2,i,j / 32767,256,5,17 /

C***FIRST EXECUTABLE STATEMENT  UNI
      if(jd .eq. 0) go to 3

C  FILL
      mdig=32         ! assume 32 bit integer
      m1= 2**(mdig-2) + (2**(mdig-2)-1)
      m2 = 2**(mdig/2)
      jseed = min0(iabs(jd),m1)
      if( mod(jseed,2).eq.0 ) jseed=jseed-1
      k0 =mod(9069,m2)
      k1 = 9069/m2
      j0 = mod(jseed,m2)
      j1 = jseed/m2
      do 2 i=1,17
        jseed = j0*k0
        j1 = mod(jseed/m2+j0*k1+j1*k0,m2/2)
        j0 = mod(jseed,m2)
    2   m(i) = j0+m2*j1
      i=5
      j=17

C  BEGIN MAIN LOOP HERE
    3 k=m(i)-m(j)
      if(k .lt. 0) k=k+m1
      m(j)=k
      i=i-1
      if(i .eq. 0) i=17
      j=j-1
      if(j .eq. 0) j=17
      uni=float(k)/float(m1)
      return
      end
