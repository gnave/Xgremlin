C***********************************************************************
C*	ROUTINE: Miniz
C*
C*	DESCRIPTION:
C*		Main control routine for non-linear least squares fit data.
C*
C*	CALLS:
C*
C*	CALLED BY:
C*
C*	REVISION HISTORY
C*
C***********************************************************************
C***********************************************************************
C*	SUBROUTINE: MINIZ
C*
C*	DESCRIPTION: main control routine for the non-linear least squares fit
C*		of the A and B splitting factors, center of gravity, intensity of 
C*		strongest component, a shape factor and halfwidth of the components.
C*		MINIZ requires a routine PROFILE to calculate the structure and the
C*		derivative of the structure for each parameter.  STEP calculates
C*		the delta that will be added to each parameter for an iteration.
C*		MINIZ requires that the target goodness of fit TOLERANCE be acheived
C*		during at least 3 successive iterations before returning.
C*
C*  INPUTS:
C*		Fit.Num_Params		- Integer*2
C*			Number of parameters to be fit.
C*		Fit.P()				- Real*8
C*			Parameter values
C*		Fit.Err				- Real*8
C*			a RMS error for the fit
C*		Fit.RMS				- Real*8
C*			a weighted RMS error 
C*		Delta()				- Real*8
C*			the incremental step to be added to each parameter
C*
C*	OUTPUTS:
C*		Fit.P()				- Real*8
C*			new parameter values
C*		Delta()				- Real*8
C*			will quarter the step interval if Fit.Err did not decrease
C*
C*	CALLS:
C*		PROFILE
C*			Calculates the function (structure shape) and the derivatives of
C*			the function relative to each parameter
C*		STEP
C*			Determines the incremental step to be added to each parameter
C*
C*	SPECIAL CONSIDERATIONS: None
C*
C*  REFERENCES:
C*		This routine is adapted from a algorithmn proposed by R. I. Jennrich
C*		and P. F. Sampson in an article in TECHNOMETRICS Vol 10 No 1.
C*
C*	REVISION HISTORY
C*		13-FEB-1993		Original baseline for V6.0
C*		
C*      Modified by Gill Nave for compatability with Xgremlin
C*      November, 1999
C*
C****************************************************************** 
	SUBROUTINE MINIZ(delw)
	IMPLICIT NONE

	INCLUDE 'hfsline.h'
	INCLUDE 'global_constants.h'

        real*8 delw

	INTEGER*2	I,J
	INTEGER*2	Max_iterations,Max_Num_Quarters,Num_Quarterings
	INTEGER*2	Num_Left, Num_Needed, Num_Iterations, ISTAT
	LOGICAL*2	UserDidCancel, didCancel
	REAL*8	Delta, Tolerance, Epsilon, E0, Percentage_Change
	REAL*8	Old_Params(25)

 	character wrtbuf*256                         ! Buffer for writing to Xgremlin window

	COMMON /MINN/ Delta(25),Tolerance,Epsilon
	
	didCancel = isFalse
	Max_iterations   = 50 
	Max_Num_Quarters =  8 
	Num_Quarterings  =  0 
	Num_Iterations   =  0 
	Num_Needed       =  3 
	Num_Left         =  3
	Epsilon   = 0.00001D0 
	Tolerance = 1.0D-10 
	E0        = 1.0D29 

	DO I=1,Fit%NUM_PARAMS 
		Old_Params(I) = Fit%P(I)
	End Do
	OPEN(UNIT=87,FILE='Phits_FitData',STATUS='UNKNOWN', 
     1       access='append')
C     1       access='append', CARRIAGECONTROL='LIST')
C 
C   Calculate Function
C 
103	CALL Profile
	DO WHILE (Num_Quarterings .lt. Max_Num_Quarters 
     1            .and. Fit%Err .gt. E0)
C
C  If RMS did not decrease then try cutting increment in quarter
C 
		Num_Quarterings = Num_Quarterings + 1 
		DO I = 1,Fit%Num_Params
			Delta(I) = Delta(I)/4.0D0 
			Fit%P(I) = Old_Params(I) + Delta(I)
		END DO
		CALL PROFILE 
	END DO 
C
C  Check if user has hit Command-q, set flag, put here purposely just before
C	   because Language Systems will clear event queue on write
C
!(gn)	IF(UserDidCancel()) didCancel = isTrue
C 
C  Write information for this iteration
C 
	WRITE(87,125,err=9020) Num_Iterations,Num_Quarterings,Fit%Err,
     1        Fit%RMs, Fit%Npts, Fit%Lpts, (Fit%P(I),I=1,Fit%Num_Params) 
!	WRITE(6,125) Num_Iterations,Num_Quarterings,Fit%Err, Fit%RMs,        ! Don't display all iterations
!     1		     Fit%Npts,Fit%Lpts, (Fit%P(I),I=1,Fit%Num_Params) 
125	FORMAT (1H0,I2,I2,1X,F20.5,1X,F20.5,2I10/(6X,4(2X,F16.5)))

	Num_Quarterings = 0 
	Num_Iterations = Num_iterations+1 

	CALL STEP
	IF (Num_Iterations .GE. Max_Iterations) GO TO 900 
	IF (ABS(Fit%Err).LT.1E-30) Go TO 800 
C
	Percentage_Change = 1.0 
	IF (E0.NE.1.0D29) Percentage_Change = ((E0-Fit%Err)/Fit%Err) 
	E0 = Fit%Err
	IF(Percentage_Change .LE. 0.0D0 ) then 
		DO I=1,Fit%Num_Params 
			Fit%P(I)=Old_Params(I)
		END DO
		WRITE(87,1010,err=9020) 
!		WRITE(6,1010)
		call wrtstr(' *** RMS value increased after this *** ')
1010	FORMAT(/1X,'***RMS VALUE INCREASED AFTER THIS***') 
		Go To 800
	Endif	
	IF (Percentage_Change .LT. Epsilon) then
		Num_Left = Num_Left - 1
		IF(Num_Left .EQ. 0) Go To 800
	else
		Num_Left = Num_Needed
	endif
C
C  Check if user has hit Command-q
C
	IF(didCancel) go to 800
C 
C  Set new parameter values and iterate
C 
	DO I=1,Fit%Num_Params 
		Old_Params(I)=Fit%P(I) 
		Fit%P(I) = Fit%P(I)+Delta(I)
	END DO
	GO TO 103 
	
800	WRITE(87,8000,err=9020)
!	WRITE(6,8000)
	call wrtstr(' Parameters and Standard deviations:')           ! Write to Xgremlin window
8000	FORMAT(/' ASYMPTOTIC STANDARD DEVIATIONS:')
	DO I = 1,Fit%Num_Params
		Fit%Matrix(I,I) = SQRT(ABS(Fit%Matrix(I,I)*Fit%Err))
	ENDDO
	WRITE(87,8010,err=9020) (Fit%Matrix(I,I),I=1,Fit%Num_Params)
!	WRITE(6,8010) (Fit%Matrix(I,I),I=1,Fit%Num_Params)
8010	FORMAT(6X,4(2X,F16.5))

        call wrtstr(
     1  'Wavenumber    SNR    FWHM Damp   A_odd  B_odd  A_even B_even')
	do i=0,fit%num_params-1, 8
           ! Something for which one's higher
           write(wrtbuf,8020) Fit%P(5+i), Fit%P(6+i), 
     1           Fit%P(8+i)*2000.*delw, Fit%P(7+i),Fit%P(1+i)*1000., 
     1           Fit%P(2+i)*1000., Fit%P(3+i)*1000., Fit%P(4+i)*1000.
           call wrtstr(wrtbuf)
	   write(wrtbuf,8020) Fit%Matrix(5+i,5+i),Fit%Matrix(6+i,6+i),
     1          Fit%Matrix(8+i,8+i)*2000.*delw, Fit%Matrix(7+i,7+i),
     1          Fit%Matrix(1+i,1+i)*1000., Fit%Matrix(2+i,2+i)*1000.,
     1          Fit%Matrix(3+i,3+i)*1000., Fit%Matrix(4+i,4+i)*1000.
           call wrtstr(wrtbuf)
        enddo
8020    format(f10.4,1x,f7.1,1x,f6.1,1x,f4.2,1x,2(2f7.1,1x))

	CLOSE(UNIT=87)
	RETURN
	
900	WRITE(87,9000,err=9020)  Max_Iterations 
!	WRITE(6,9000)  Max_Iterations 
	write(wrtbuf,9010) Max_Iterations                 ! Write to Xgremlin window
	call wrtstr(wrtbuf)                               ! Write to Xgremlin window
9000	FORMAT(/' Process is not converging. Maximum number of ',I3,
     1                          ' iterations has been reached.')
9010	FORMAT(' Process is not converging. Maximum number of ',I3,
     1                         	' iterations has been reached.')
	GO TO 800
 9020	call wrtstr('Error on write - Disk full ?')
	return
	END 

C***********************************************************************
C*	SUBROUTINE: STEP
C*
C*	DESCRIPTION: 
C*		This routine calculates a vector step in the direction that will
C*		minimize the function.  The parameters are used in the order that
C*		will do "the most good".
C*
C*  INPUTS:
C*		Fit.Num_Params		- Integer*2
C*			Number of parameters to be fit.
C*		Fit.P()				- Real*8
C*			Parameter values
C*		Fit.Matrix(,)		- Real*8
C*			correlation matrix
C*		TOLERANCE			- Real*8
C*			the desired level of goodness of fit 
C*
C*	OUTPUTS:
C*		Delta()				- Real*8
C*			the incremental step to be added to each parameter
C*		Fit.Matrix(,)		- Real*8
C*			updated correlation matrix
C*
C*	CALLS: None
C*
C*	CALLED BY:
C*		Miniz
C*
C*	SPECIAL CONSIDERATIONS: None
C*
C*  REFERENCES:
C*		This routine is adapted from a algorithmn proposed by R. I. Jennrich
C*		and P. F. Sampson in an article in TECHNOMETRICS Vol 10 No 1.
C*
C*	REVISION HISTORY
C*		13-FEB-1993		Original baseline for V6.0
C*		
C****************************************************************** 
	SUBROUTINE STEP
	IMPLICIT NONE

	INCLUDE 'global_constants.h'
	INCLUDE 'hfsline.h'

	INTEGER*2	I, J, K
	REAL*8 		V(25), Q, C, YVV, PP
	REAL*8		DAMP, TMP_DAMP
	INTEGER*2	NP1, IP0, IP, IP1, ISTAT
	INTEGER*2	BEEN_STEPPED(25)
	INTEGER*2	HW_INDX, ETA_INDX

	REAL*8		  Delta(25), Tolerance, Epsilon
	COMMON /MINN/ Delta, Tolerance, Epsilon 

	SELECT CASE (FIT%PEAK%TYPE)
		CASE (kHFS3Fit)
			ETA_INDX = 7
			HW_INDX = 8
C		CASE (kHgFit)
C			ETA_INDX = 21
C			HW_INDX = 22
	END SELECT
	NP1 = Fit%Num_Params+1 
	DO I = 1,NP1 
		V(I) = SQRT(FIT%MATRIX(I,I)) 
		DO J = 1,I 
			IF (V(I)*V(J) .NE. 0.0D0) THEN
				Fit%Matrix(I,J) = Fit%Matrix(I,J)/(V(I)*V(J)) 
			ELSE
				Fit%Matrix(I,J) = 0.0D0
			ENDIF
		ENDDO
	ENDDO
	DO I=1,Fit%Num_Params 
		BEEN_STEPPED(I) = 0
	ENDDO
C
C  Determine the step size for each variable.  The steps are determined in the
C	  order of variables that does the "most good".  If a changing variable would
C	  not significantly contribute to the fit then it is ignored during an
C	  iteration
C
104	K = 0 
	DAMP = 0.0D0 
	DO I = 1,Fit%Num_Params
		IF((BEEN_STEPPED(I).EQ.0).AND.
     1             Fit%Matrix(I,I).GT.Tolerance) THEN
			Q = Fit%Matrix(NP1,I)*Fit%Matrix(NP1,I)/Fit%Matrix(I,I) 
			IF (Q .GT. DAMP) THEN 
				K = I 
				DAMP = Q
			ENDIF
		ENDIF
	END DO
	
	IF (K.EQ.0) GO TO 112 
	C=-1. 
106	DO I = 1,K 
		Delta(I) = Fit%Matrix(K,I) 
		Fit%Matrix(K,I) = 0.0D0
	ENDDO
	PP = Delta(K) 
	DO I = K,NP1 
		Delta(I) = Fit%Matrix(I,K) 
		Fit%Matrix(I,K)   = 0.0D0
	ENDDO
	Delta(K) = C 

	BEEN_STEPPED(K) = BEEN_STEPPED(K) + 1
	DO I = 1,NP1 
		IF (PP .EQ. 0.0D0 .OR. Delta(I) .EQ. 0.0D0) THEN
			YVV = 0.0D0
		ELSE
			YVV = Delta(I)/PP
		ENDIF
		DO J = 1,I 
			Fit%Matrix(I,J) = Fit%Matrix(I,J)-Delta(J)*YVV
		END DO
	END DO
	GO TO 104 

C
C   The following code is used to damp the convergence and
C		bound certain parameters
C		The Kielkopf eta (parameter ETA_INDX) must be between 0.0 and 1.0
C		The halfwidth (parameter HW_INDX) must be greater than 0.0
C 
112	DAMP = 1.0D0 
	DO I = 1,Fit%Num_Params
	    IF (BEEN_STEPPED(I) .EQ. 1) THEN
		IF (V(I) .NE. 0.0D0) THEN
		    Delta(I) = Fit%Matrix(NP1,I)*V(NP1)/V(I) 
		    IF (Delta(I) .NE. 0.0D0) THEN 
		        IF(FIT%SWITCH(I)) THEN
		            TMP_DAMP = 0.0D0
		        ELSE
		          TMP_DAMP = DAMP
		          IF(I.EQ.ETA_INDX) 
     1    TMP_DAMP = DMAX1(-FIT%P(I)/Delta(I),(1.0D0-FIT%P(I))/Delta(I)) 
		          IF(I.EQ.HW_INDX.AND.(Fit%P(I)+Delta(I)).LT.0.) 
     1		    	  TMP_DAMP = -Fit%P(I)/Delta(I)
		        ENDIF
		    ELSE
		    	TMP_DAMP = 0.0D0
		    ENDIF
		ELSE
			Delta(I) = 0.0D0 
			TMP_DAMP = 0.0D0 
		ENDIF
		IF (TMP_DAMP .LT. DAMP) THEN
			DAMP = TMP_DAMP 
			K = I 
		ENDIF
	    ENDIF
	ENDDO

	C=1.0 
	IF (DAMP .LE. Tolerance) GO TO 106
	
	DO I=1,Fit%Num_Params 
	    IF (BEEN_STEPPED(I) .EQ. 1) THEN
		    Delta(I) = Delta(I)*DAMP 
		    DO J=1,I 
		       IF ((V(I)*V(J)) .NE. 0.0D0) THEN
		          Fit%Matrix(I,J) = -Fit%Matrix(I,J)/(V(I)*V(J)) 
	    	       ELSE 
			  Fit%Matrix(I,J) = 0.0D0
		       ENDIF
		          Fit%Matrix(J,I) = Fit%Matrix(I,J)
		    ENDDO
		ELSE
			Delta(I) = 0.0D0 
			DO J = 1,Fit%Num_Params 
				Fit%Matrix(I,J) = 0.0D0 
				Fit%Matrix(J,I) = 0.0D0
			ENDDO
		ENDIF
	ENDDO

	RETURN 
	END 

C***********************************************************************
C*	SUBROUTINE: PROFILE 
C*
C*	DESCRIPTION: 
C*		This routine calculates the shape of hyperfile structure given the
C*		parameters: A and B splitting factors for each level, center of gravity,
C*		intensity of strongest component, and a component shape factor and
C*		halfwidth.  PROFILE also calculates the derivatives of the function
C*		with respect to each fit parameter.
C*
C*  INPUTS:
C*		Fit.Num_Params					- Integer*2
C*			Number of parameters to be fit.
C*		Fit.P()							- Real*8
C*			Parameter values
C*		Fit.Num_Structures				- Integer*2
C*			Number of structures to fit simulataneously ( max 3 )
C*		Fit.Refractive_Index 			- Real*8
C*			the refractive index at the center of gravity of structure
C*		Fit.ISOTOPE().Num_of_Components	- Integer*2
C*			Number of components for each structure
C*		Fit.ISOTOPE().Rel_Intensity()	- Real*8
C*			Relative Intensity for each component of each structure (range 0.0 - 1.0)
C*		Fit.ISOTOPE().CF(,)				- Real*8
C*			Coefficient matrix for splitting factors
C*		Fit.Npts						- Real*8
C*			Number of points in data to be fitted
C*		Fit.Beg_Abspt					- Real*8
C*			Beginning absolute point number of data
C*		Fit.Data_Pts()					- Real*8
C*			Data points to be fitted
C*		Fit.Weight()					- Real*8
C*			Weight to be given each data point in fit
C*		IDENT structure
C*			Structure containing information on spectrum data
C*		Spec_Cons						- Real*8
C*			Constant = FreeSpectralRange / Num of points in transform
C*		Spec_Factor						- Real*8
C*			Constant = Num of points measured / Num of points in transform
C*
C*	OUTPUTS:
C*		Fit.Matrix(,)					- Real*8
C*			initial correlation matrix
C*		Fit.ISOTOPE().Comp_PTN()			- Real*8
C*			point number position of each component of each structure
C*		Fit.LPTS						- Integer*2
C*			number of data points above the RMS noise level
C*		Fit.RMS							- Real*8
C*			RMS error of fit versus data points above noise level
C*
C*	CALLS: 
C*		VOIGT
C*			Calculates a voigt profile
C*		DBL_CNVL
C*			Double precision convolution of two functions
C*
C*	SPECIAL CONSIDERATIONS: None
C*
C*  REFERENCES:
C*		The calculation of the voigt profile uses an approximation proposed
C*	J. Keilkopf in JOSA Vol. 63 No. 8 pp987-995. 
C*
C*	REVISION HISTORY
C*		13-FEB-1993		Original baseline for V6.0
C*		
C****************************************************************** 
	SUBROUTINE PROFILE
	IMPLICIT NONE

	INCLUDE 'global_constants.h'
	INCLUDE 'specfile.h'
	INCLUDE 'hfsline.h'

	INTEGER*2	I, J, K, L
	INTEGER*2	NFLG, IP, NLFIT2, KN, NTMP, NP1, NPTS
	INTEGER*2	NCONV, NCNV2, NUM_STRUCT, NUM_COMP
	REAL*8		WAVNUM
	REAL*8		SIGMA, SIGBG, ARG, CS2, ACONS, ACON2
	REAL*8		BCONS, AB, KETA, HWVT, Q, FNUM, FCONS, FNORM
	REAL*8		VGT, VET, VHW, VSG, RPNPR, RPD, FPIC
	REAL*8		COMP_INTENSITY

	INTEGER*2	NLFIT, ONE
	INTEGER*2	NUM_FIT_PARS
	REAL*8		PI, PI2
	PARAMETER (PI=3.14159265359, PI2=6.283185308, NLFIT=49, ONE=1)
	
	REAL*8  FP(25), DRSIG(1000+NLFIT,3), SINC(NLFIT,41,7),
     1   	VT(1000+NLFIT,3), DETA(1000+NLFIT,3), DHWV(1000+NLFIT,3)
	 
C
C  Determine component positions and intensities
C
	NFLG =  1
	NPTS = 20
	NUM_FIT_PARS = 0
	DO I = 1,Fit%Num_Params
		IF(.NOT. Fit%Switch(I)) NUM_FIT_PARS = NUM_FIT_PARS + 1
	ENDDO
	
	SELECT CASE (FIT%PEAK%TYPE)
	
		CASE (kHFS3Fit)
	
			IP   = -8
			DO KN = 1,Fit%Num_Structures
				IP = IP+8
				DO I = 1,Fit%Struct(KN)%Num_Components
					WAVNUM =   Fit%Struct(KN)%CF(1,I)*FIt%P(IP+1) 
     1						 + Fit%Struct(KN)%CF(2,I)*FIt%P(IP+2)
     1						 + Fit%Struct(KN)%CF(3,I)*Fit%P(IP+3) 
     1						 + Fit%Struct(KN)%CF(4,I)*Fit%P(IP+4) 
     1						 + Fit%P(IP+5)
					CALL WVN_TO_PT(WAVNUM,FIT%STRUCT(KN)%PTN(I))
				ENDDO
				NTMP = 20D0*Fit%P(IP+8)
				IF(NTMP.GT.NPTS) NPTS = NTMP
			ENDDO

C		CASE (kHgFit)
C		
C			CALL WVN_TO_PT(Fit.P(1), FIT.ISOTOPE(1).PTN(1))			! Hg 200
C			WAVNUM = Fit.P(1) + Fit.P(2)
C			CALL WVN_TO_PT(WAVNUM, FIT.ISOTOPE(2).PTN(1))			! Hg 202
C			WAVNUM = Fit.P(1) + Fit.P(3)*Fit.P(2)
C			CALL WVN_TO_PT(WAVNUM, FIT.ISOTOPE(3).PTN(1))			! Hg 198
C			DO I = 1,Fit.ISOTOPE(4).Num_Components
C				WAVNUM =   Fit.ISOTOPE(4).CF(1,I)*FIt.P(13) 
C     1						 + Fit.ISOTOPE(4).CF(2,I)*FIt.P(14)
C     1						 + Fit.ISOTOPE(4).CF(3,I)*Fit.P(15) 
C     1						 + Fit.ISOTOPE(4).CF(4,I)*Fit.P(16) 
C     1						 + Fit.P(1) + Fit.P(4)*Fit.P(2)			! Hg 199
C				CALL WVN_TO_PT(WAVNUM,FIT.ISOTOPE(4).PTN(I))
C			ENDDO
C			DO I = 1,Fit.ISOTOPE(5).Num_Components
C				WAVNUM =   Fit.ISOTOPE(5).CF(1,I)*FIt.P(17) 
C     1						 + Fit.ISOTOPE(5).CF(2,I)*FIt.P(18)
C     1						 + Fit.ISOTOPE(5).CF(3,I)*Fit.P(19) 
C     1						 + Fit.ISOTOPE(5).CF(4,I)*Fit.P(20) 
C     1						 + Fit.P(1)	+ Fit.P(5)*Fit.P(2)			! Hg 201
C				CALL WVN_TO_PT(WAVNUM,FIT.ISOTOPE(5).PTN(I))
C			ENDDO
C			WAVNUM = Fit.P(1) + Fit.P(6)*Fit.P(2)
C			CALL WVN_TO_PT(WAVNUM, FIT.ISOTOPE(6).PTN(1))			! Hg 204
C	
C			NTMP = 20D0*Fit.P(22)					! base width of calculation on HW
C			IF(NTMP.GT.NPTS) NPTS = NTMP
C			
	END SELECT
C
C  Calculate Voigt profile for each pattern
C
	NCONV = NPTS+NLFIT-1
	NCNV2 = NCONV/2
	SIGBG = -NCNV2
	FNUM  = 1.0D0
	NP1   = Fit%Num_Params + 1
	SELECT CASE (FIT%PEAK%TYPE)
		CASE (kHFS3Fit)
			IP    = -1
			DO KN=1,Fit%Num_Structures
				IP	 = IP+8
				KETA = Fit%P(IP)
				HWVT = Fit%P(IP+1)
				IF(HWVT .LE. 1.0D-3) HWVT = 1.0D-3
				CALL hfsVOIGT(VT(1,KN), DRSIG(1,KN), DETA(1,KN), DHWV(1,KN),
     1						SIGBG, KETA, HWVT, FNUM, NCONV)
			ENDDO
C		CASE (kHgFit)
C			KETA = Fit%P(21)
C			HWVT = Fit%P(22)
C			IF(HWVT .LE. 1.0D-3) HWVT = 1.0D-3
C			CALL hfsVOIGT(VT(1,1), DRSIG(1,1), DETA(1,1), DHWV(1,1), 
C     1						SIGBG, KETA, HWVT, FNUM, NCONV)
	END SELECT
C
C  Calculate FT Instrumental function
C
	ACON2 = (IDENT%Npts_Measured-IDENT%Npts_Apodized)
     1            /IDENT%Transform_Npts
	ACONS = ACON2/2.0D0
	BCONS = IDENT%Npts_Apodized/IDENT%Transform_Npts/2.0D0
	AB    = ACONS+BCONS/2.0D0
	NLFIT2= NLFIT/2 + 1
	DO J = 1,NLFIT
		SIGMA = DFLOAT(J-NLFIT2)
		ARG   = BCONS*SIGMA+0.25D0
		CS2   = COS(PI2*(AB*SIGMA+0.125D0))
		IF(ABS(ARG) .GT. 1.0D-30) THEN
			SINC(J,41,1) = BCONS*CS2*SIN(PI*ARG)/(PI*ARG)
		ELSE
			SINC(J,41,1) = BCONS*CS2
		ENDIF
		CS2 = COS(PI2*(AB*SIGMA-0.125D0))
		ARG = BCONS*SIGMA-0.25D0
		IF(ABS(ARG) .GT. 1.0D-30) THEN
			SINC(J,41,1) = SINC(J,41,1) + BCONS*CS2*SIN(PI*ARG)/(PI*ARG)
		ELSE
			SINC(J,41,1) = SINC(J,41,1) + BCONS*CS2
		ENDIF
		SIGMA = ACON2*SIGMA
		IF(ABS(SIGMA) .GT. 1.0D-30) THEN
			SINC(J,41,1)=SINC(J,41,1) + ACON2*SIN(PI*SIGMA)/(PI*SIGMA)
		ELSE
			SINC(J,41,1)=SINC(J,41,1) + ACON2
		ENDIF
	ENDDO
	SELECT CASE (FIT%PEAK%TYPE)
		CASE (kHFS3Fit)
			NUM_STRUCT = FIT%NUM_STRUCTURES
C		CASE (kHgFit)
C			NUM_STRUCT = FIT.NUM_ISOTOPES
		CASE DEFAULT
			NUM_STRUCT = 0
	END SELECT
	DO KN = 1,NUM_STRUCT
	    SELECT CASE (FIT%PEAK%TYPE)
    	       CASE (kHFS3Fit)
       	    	NUM_COMP = FIT%STRUCT(KN)%NUM_COMPONENTS
C			CASE (kHgFit)
C				NUM_COMP = FIT%ISOTOPE(KN)%NUM_COMPONENTS
	    	CASE DEFAULT
				NUM_COMP = 0
    	    END SELECT
		DO I = 1,NUM_COMP
		   DO J = 1,NLFIT
		      SELECT CASE (FIT%PEAK%TYPE)
		   	CASE (kHFS3Fit)
			    SIGMA = (DFLOAT(J-NLFIT2) + 
     1                          Fit%STRUCT(KN)%Ptn(I)
     1		   	     		- DINT(Fit%STRUCT(KN)%Ptn(I)))
C		     		CASE (kHgFit)
C						SIGMA = (DFLOAT(J-NLFIT2) + Fit%ISOTOPE(KN)%Ptn(I)
C    1							- DINT(Fit%ISOTOPE(KN)%Ptn(I)))
	     	      END SELECT
		        ARG = BCONS*SIGMA+0.25D0
			CS2 = COS(PI2*(AB*SIGMA+0.125D0))
			IF(ABS(ARG) .GT. 1.0D-30) THEN
		           SINC(J,I,KN) = BCONS*CS2*SIN(PI*ARG)/(PI*ARG)
			ELSE
			   SINC(J,I,KN) = BCONS*CS2
			ENDIF
			ARG = BCONS*SIGMA-0.25D0
			CS2 = COS(PI2*(AB*SIGMA-0.125D0))
			IF(ABS(ARG) .GT. 1.0D-30) THEN
			   SINC(J,I,KN) = SINC(J,I,KN) + 
     1                           BCONS*CS2*SIN(PI*ARG)/(PI*ARG)
			ELSE
			   SINC(J,I,KN) = SINC(J,I,KN) + BCONS*CS2
			ENDIF
			SIGMA = ACON2*SIGMA
			IF(ABS(SIGMA) .GT. 1.0D-30) THEN
			   SINC(J,I,KN) = SINC(J,I,KN)+
     1                             ACON2*SIN(PI*SIGMA)/(PI*SIGMA)
			ELSE
			   SINC(J,I,KN) = SINC(J,I,KN)+ACON2
			ENDIF
		   ENDDO
		ENDDO
	ENDDO
C
C  Find normalization constant
C
	CALL DBL_CNVL(SINC(1,41,1),VT(NCNV2-NLFIT2+2,1),FNORM,ONE,NLFIT)

	FCONS=Fit%Refractive_Index/SPEC_CONS
c previous version reversed spectrum (gn)
C	FCONS=Fit%Refractive_Index/SPEC_CONS*((-1)**(IDENT.Spectrum_Order+1))
	DO I=1,NP1
		DO J=1,I
			Fit%Matrix(I,J)=0.0D0
		ENDDO
	ENDDO
	FIT%LPTS = 0
	Fit%RMS = 0.0D0
C
C  Calculate Components
C
	DO L=1,Fit%Npts
		IF(Fit%Data_Pts(L) .GT. 2.0*ABS(IDENT%RMS_NOISE)) THEN
			Fit%Weight(L) = 1.0
		ELSE
			Fit%Weight(L) = 0.0
		ENDIF
C		IF(Fit.Weight(L) .GE. (1.0D-20)) THEN
			DO I=1,NP1
				FP(I) = 0.0D0
			ENDDO
			Q  = 0.0D0
			IP = -8
			DO KN = 1,NUM_STRUCT 
				SELECT CASE (FIT%PEAK%TYPE)
					CASE (kHFS3Fit)
						NUM_COMP = FIT%STRUCT(KN)%NUM_COMPONENTS
						IP = IP + 8
C					CASE (kHgFit)
C						NUM_COMP = FIT%ISOTOPE(KN)%NUM_COMPONENTS
					CASE DEFAULT
						NUM_COMP = 0
				END SELECT
				DO I = 1, NUM_COMP
					SELECT CASE (FIT%PEAK%TYPE)
						CASE (kHFS3Fit)
c don't reverse spectrum (gn)
C							IF((-1)**IDENT.SPECTRUM_ORDER .EQ. -1) THEN
								K = NCNV2 + L  - NLFIT2 + 1 -
     1								(INT(Fit%STRUCT(KN)%PTN(I) - Fit%BEG_ABSPT))
C							ELSE
C								K = NCNV2 + L  - NLFIT2 + 1 - 
C     1								(INT(Fit.STRUCT(KN).PTN(I) - Fit.END_ABSPT))
C							ENDIF
CC						CASE (kHgFit)
c don't reverse spectrum (gn)
C							IF((-1)**IDENT.SPECTRUM_ORDER .EQ. -1) THEN
CC								K = NCNV2 + L  - NLFIT2 + 1 - 
CC     1								(INT(Fit.ISOTOPE(KN).PTN(I) - Fit.BEG_ABSPT))
C							ELSE
C								K = NCNV2 + L  - NLFIT2 + 1 - 
C     1								(INT(Fit.ISOTOPE(KN).PTN(I) - Fit.END_ABSPT))
C							ENDIF
					END SELECT
					IF(K .GT. 0 .AND. K .LE. NPTS) THEN
					SELECT CASE (FIT%PEAK%TYPE)
				
						CASE (kHFS3Fit)
C
C  Convolute with instrumental function
C
						CALL DBL_CNVL(SINC(1,I,KN),   VT(K,KN),VGT,ONE,NLFIT)
						CALL DBL_CNVL(SINC(1,I,KN), DETA(K,KN),VET,ONE,NLFIT)
						CALL DBL_CNVL(SINC(1,I,KN), DHWV(K,KN),VHW,ONE,NLFIT)
						CALL DBL_CNVL(SINC(1,I,KN),DRSIG(K,KN),VSG,ONE,NLFIT)
						VGT=VGT/FNORM
						VET=VET/FNORM
						VHW=VHW/FNORM
						VSG=VSG/FNORM
C
C  Calculate derivatives
C
						RPNPR = FIT%P(IP+6)*Fit%Struct(KN)%Rel_Intensity(I)
						RPD   = FCONS*RPNPR*VSG
						DO K=1,4
							FP(IP+K) = FP(IP+K)+Fit%Struct(KN)%CF(K,I)*RPD
						ENDDO
						FP(IP+5) = FP(IP+5)+RPD
						FP(IP+6) = FP(IP+6)+VGT*Fit%Struct(KN)%Rel_Intensity(I)
						FP(IP+7) = FP(IP+7)+VET*RPNPR
						FP(IP+8) = FP(IP+8)+VHW*RPNPR
						Q = Q + VGT*RPNPR
					
C					CASE (kHgFit)
C
C  Convolute with instrumental function
C
C						CALL DBL_CNVL(SINC(1,I,KN),   VT(K,1),VGT,ONE,NLFIT)
C						CALL DBL_CNVL(SINC(1,I,KN), DETA(K,1),VET,ONE,NLFIT)
C						CALL DBL_CNVL(SINC(1,I,KN), DHWV(K,1),VHW,ONE,NLFIT)
C						CALL DBL_CNVL(SINC(1,I,KN),DRSIG(K,1),VSG,ONE,NLFIT)
C						VGT=VGT/FNORM
C						VET=VET/FNORM
C						VHW=VHW/FNORM
C						VSG=VSG/FNORM
C
C  Calculate derivatives
C
C						IF (KN .EQ. 1) THEN						! Hg 200
C							RPNPR = FIT.P(7)
C						ELSE
C							COMP_INTENSITY = FIT.ISOTOPE(KN).Rel_Intensity(I)
C     1								/ FIT.ISOTOPE(KN).Sum_Intensities
C							RPNPR = FIT.P(7)*FIT.P(6+KN) * COMP_INTENSITY 
C						ENDIF
C						RPD   = FCONS*VSG
C						FP(1) = FP(1) + RPD * RPNPR
C						IF(KN .EQ. 2) THEN
C							FP(2) = FP(2) + RPD * RPNPR
C						ELSEIF(KN .NE. 1) THEN
C							FP(2) = FP(2) + RPD * RPNPR * FIT.P(KN)
C							FP(KN) = FP(KN) + RPD * RPNPR * FIT.P(2)
C						ENDIF
C						
C						IF(KN .EQ. 1) THEN
C							FP(7) = FP(7) + VGT
C						ELSE
C							FP(7) = FP(7) + VGT * FIT.P(6+KN) * COMP_INTENSITY
C							FP(KN+6) = FP(KN+6) + VGT * FIT.P(7) * COMP_INTENSITY
C						ENDIF
C						IF(KN .EQ. 4) THEN						! Hg 199
C							DO IP = 1,4
C								FP(IP+12) = FP(IP+12) + 
C     1								RPD*Fit.ISOTOPE(4).CF(IP,I)*RPNPR
C							ENDDO
C						ELSEIF(KN .EQ. 5) THEN					! Hg 201
C							DO IP = 1,4
C								FP(IP+16) = FP(IP+16) + 
C     1								RPD*Fit.ISOTOPE(5).CF(IP,I)*RPNPR
C							ENDDO
C						ENDIF
C						FP(21) = FP(21) + VET * RPNPR
C						FP(22) = FP(22) + VHW * RPNPR
C						Q = Q + VGT*RPNPR
				END SELECT
				
					ENDIF
				ENDDO
			ENDDO
			
			FP(NP1) = Fit%Data_Pts(L)-Q
			IF(DABS(Q) .GE. ABS(IDENT%RMS_NOISE)) THEN
				FIT%LPTS = FIT%LPTS+1
				Fit%RMS = Fit%RMS + (FP(NP1)/Q)**2
			ENDIF
			DO I=1,NP1
				FPIC = FP(I)
C				FPIC = FP(I)*Fit%Weight(L)
				DO J=1,I
					Fit%Matrix(I,J) = Fit%Matrix(I,J)+FPIC*FP(J)
				ENDDO
			ENDDO
C		ENDIF
	ENDDO
C	Fit.ERR = Fit.Matrix(NP1,NP1)/DFLOAT(Fit.Npts - Fit.Num_Params)
	Fit%ERR = Fit%Matrix(NP1,NP1)/DFLOAT(Fit%Npts - NUM_FIT_PARS)
	IF(FIT%LPTS .EQ. 0) FIT%LPTS = 1
	Fit%RMS = SQRT(Fit%RMS)/DFLOAT(FIT%LPTS)
	
	RETURN
	END
