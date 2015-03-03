C***********************************************************************
C*	FUNCTION:	A_SPLIT_COEF
C*
C*	DESCRIPTION:
C*		Function to the calculate the A splitting factor coefficient
C*	based on the I, J, and F quantum numbers.  If the J quantum number is
C*	zero then the function returns zero.
C*	
C*  INPUTS:
C*		I_VALUE		- REAL*8
C*			The value of the I quantum number
C*		F_VALUE 	- REAL*8
C*			The value of the F quantum number
C*		J_VALUE		- REAL*8
C*			The value of the J quantum number
C*
C*	OUTPUTS:
C*		A_SPLIT_COEF - REAL*8
C*			The coefficient value of the A splitting factor to be use in
C*		the calculation of the position of the hyperfine splitting components.
C*
C*	SPECIAL CONSIDERATIONS: None
C*
C*	REVISION HISTORY
C*		15-DEC-1991		Original Version
C*
C***********************************************************************
	REAL*8 FUNCTION A_SPLIT_COEF(I_VALUE,F_VALUE,J_VALUE)
	IMPLICIT NONE

	REAL*8
     1	I_VALUE,		!I quantum number
     1	F_VALUE,		!F quantum number
     1	J_VALUE			!J quantum number
C
	IF(J_VALUE .NE. 0.0D0) THEN
		A_SPLIT_COEF =  ( F_VALUE*(F_VALUE+1.0D0)
     1					 -I_VALUE*(I_VALUE+1.0D0)
     1					 -J_VALUE*(J_VALUE+1.0D0) ) / 2.0D0
	ELSE
		A_SPLIT_COEF = 0.0D0
	ENDIF
	
	RETURN
	END

C***********************************************************************
C*	FUNCTION: 	B_SPLIT_COEF
C*
C*	DESCRIPTION:
C*		Function to the calculate the B splitting factor coefficient
C*	based on the I, J, and F quantum numbers.  If the J quantum number is
C*	less than or equal to 0.5 then the function returns zero.
C*	
C*  INPUTS:
C*		I_VALUE		- REAL*8
C*			The value of the I quantum number
C*		F_VALUE 	- REAL*8
C*			The value of the F quantum number
C*		J_VALUE		- REAL*8
C*			The value of the J quantum number
C*
C*	OUTPUTS:
C*		B_SPLIT_COEF - REAL*8
C*			The coefficient value of the B splitting factor to be use in
C*		the calculation of the position of the hyperfine splitting components.
C*
C*	CALLS:
C*		A_SPLIT_COEF - REAL*8 function
C*			Calculates the A splitting factor coefficient which is used in 
C*		calculating the B splitting factor coefficient.
C*
C*	SPECIAL CONSIDERATIONS: None
C*
C*	REVISION HISTORY
C*		15-DEC-1991		Original Version
C*
C***********************************************************************
	REAL*8 FUNCTION B_SPLIT_COEF(I_VALUE,F_VALUE,J_VALUE)
	IMPLICIT NONE

	REAL*8
     1	I_VALUE,		! I quantum number
     1	F_VALUE,		! F quantum number
     1	J_VALUE			! J quantum number
	
	REAL*8
     1	CF1, C2, C3		! intermediate results
	
	REAL*8
     1	A_SPLIT_COEF	! function to calculate A coefficient
C
	IF(J_VALUE .GT. 0.5D0 .AND. I_VALUE .GT. 0.5D0) THEN
		CF1 = A_SPLIT_COEF(I_VALUE,F_VALUE,J_VALUE)
		C2  = I_VALUE*(I_VALUE+1.0D0) * J_VALUE*(J_VALUE+1.0D0)
		C3  = 2.0D0*I_VALUE
		C3  = C3*(C3-1.0D0) * J_VALUE*(2.0D0*J_VALUE-1.0D0)
		B_SPLIT_COEF = (3.0D0*CF1*(CF1+0.5D0) - C2) / C3
	ELSE
		B_SPLIT_COEF = 0.0D0
	ENDIF
	
	RETURN
	END

C***********************************************************************
C*	SUBROUTINE: CALC_HFS_COEFFS
C*
C*	DESCRIPTION:
C*		This routine calculates A and B splitting factor coefficients
C*	and relative intensities of the components of a hyperfine structure
C*	based on the I and J quantum values.
C*	
C*  INPUTS:
C*		I_VALUE		- REAL*8
C*			Value of the I quantum factor
C*		J_VAL_ODD	- REAL*8
C*			Value of the J quantum factor of the even level
C*		J_VAL_EVEN	- REAL*8
C*			Value of the J quantum factor of the odd level
C*		HIGHER_LEVEL - REAL*8
C*			= ODD_HIGHER  =  1.0D0 if odd level is higher than even level
C*			= EVEN_HIGHER = -1.0D0 if even level is higher than odd level
C*
C*	OUTPUTS:
C*		NUM_HFS_COMPONENTS - INTEGER*2
C*			Number of hyperfine structure components
C*		CF			- REAL*8 (4,40)
C*			Array of coefficients to determine position of components
C*			(1,*) - A splitting coefficient of odd level
C*			(2,*) - B splitting coefficient of odd level
C*			(3,*) - A splitting coefficient of even level
C*			(4,*) - B splitting coefficient of even level
C*		CMP_INTENSITY - REAL*8 (40)
C*			Array of intensities of hyperfine structure components
C*
C*	CALLS:
C*		A_SPLIT_COEFF - REAL*8 function
C*			Calculates the A splitting factor coefficients
C*		B_SPLIT_COEFF - REAL*8 function
C*			Calculates the B splitting factor coefficients
C*		CALC_HFS_INTENSITY - REAL*8 function
C*			Calculates the HFS structure component intensities
C*
C*	SPECIAL CONSIDERATIONS: None
C*
C*	REVISION HISTORY
C*		15-DEC-1991		Original Version
C*
C***********************************************************************
	SUBROUTINE CALC_HFS_COEFFS(I_VALUE, J_VAL_ODD, J_VAL_EVEN,
     1							HIGHER_LEVEL, NUM_HFS_COMPONENTS,
     1							CF, CMP_INTENSITY)
	IMPLICIT NONE

	REAL*8
     1	I_VALUE,			! I quantum number
     1	J_VAL_ODD,			! J quantum number of odd level
     1	J_VAL_EVEN,			! J quantum number of even level
     1	HIGHER_LEVEL,		! flag indicating which level is higher
     1	CF(4,40),			! coefficients array
     1	CMP_INTENSITY(40)	! array of relative component intensities
	
	INTEGER*2
     1	NUM_HFS_COMPONENTS	! number of components in hfs.
	
	REAL*8
     1	FMNOD,				! minimum of (odd  level J and I)
     1	FMNEV,				! minimum of (even level J and I)
     1	FMNAX,				! maximum F value
     1	F1_VALUE,			! temporary F value
     1	F2_VALUE			! temporary F value
	
	REAL*8
     1	A_SPLIT_COEF,		! function to calculate A-coefficient
     1	B_SPLIT_COEF,		! function to calculate B-coefficient
     1	CALC_HFS_INTENSITY	! function to calculate relative intensities

	INTEGER*2
     1	N1,N2,IMC,			! temporary variables
     1	I,J					! temporary indices
C
C
C
	FMNOD = ABS(I_VALUE - J_VAL_ODD)
	FMNEV = ABS(I_VALUE - J_VAL_EVEN)
	NUM_HFS_COMPONENTS = 0
	N1    = INT(2.0D0*J_VAL_ODD  + 1.1D0)
	N2    = INT(2.0D0*J_VAL_EVEN + 1.1D0)
	IMC   = INT(2.0D0*I_VALUE    + 1.1D0)
	IF(N1 .GT. IMC) N1 = IMC
	IF(N2 .GT. IMC) N2 = IMC
	FMNAX = FMNEV + DFLOAT(N2-1)
	DO 100 I =  1,N1
		F1_VALUE = FMNOD    + DFLOAT(I-1)
	DO 100 J = -1,1
		F2_VALUE = F1_VALUE + DFLOAT(J)
		IF( ABS(F1_VALUE) .LT. 0.1D0 .AND. 
     1		ABS(F2_VALUE) .LT. 0.1D0)		GO TO 100
		IF( F2_VALUE .LT. FMNEV      .OR. 
     1		F2_VALUE .GT. FMNAX)			GO TO 100
		NUM_HFS_COMPONENTS = NUM_HFS_COMPONENTS + 1
		CMP_INTENSITY(NUM_HFS_COMPONENTS) = 
     1		 CALC_HFS_INTENSITY(I_VALUE, F1_VALUE,  F2_VALUE,
     1								     J_VAL_ODD, J_VAL_EVEN)
		CF(1,NUM_HFS_COMPONENTS) = 
     1		 HIGHER_LEVEL * A_SPLIT_COEF(I_VALUE, F1_VALUE, J_VAL_ODD)
		CF(2,NUM_HFS_COMPONENTS) =
     1		 HIGHER_LEVEL * B_SPLIT_COEF(I_VALUE, F1_VALUE, J_VAL_ODD)
		CF(3,NUM_HFS_COMPONENTS) =
     1		-HIGHER_LEVEL * A_SPLIT_COEF(I_VALUE, F2_VALUE, J_VAL_EVEN)
		CF(4,NUM_HFS_COMPONENTS) =
     1		-HIGHER_LEVEL * B_SPLIT_COEF(I_VALUE, F2_VALUE, J_VAL_EVEN)
100	CONTINUE
	RETURN
	END

C***********************************************************************
C*	FUNCTION:	CALC_HFS_INTENSITY
C*
C*	DESCRIPTION:
C*		Calculates the relative intensities of the hyperfine structure
C*	components defined by the inputted I, F and J quantum numbers.
C*	
C*  INPUTS:
C*		I_VALUE		- REAL*8
C*			Value of the I quantum number
C*		F1_VALUE	- REAL*8
C*			Value of the F quantum number of first level
C*		F2_VALUE	- REAL*8
C*			Value of the F quantum number of other level
C*		J1_VALUE	- REAL*8
C*			Value of the J quantum number of first level
C*		J2_VALUE	- REAL*8
C*			Value of the J quantum number of other level
C*
C*	OUTPUTS:
C*		CALC_HFS_INTENSITY - REAL*8
C*			Relative intensity of the hyperfine structure component
C*
C*	CALLS: None
C*
C*	SPECIAL CONSIDERATIONS: None
C*
C*	REVISION HISTORY
C*		15-DEC-1991		Original Version
C*
C***********************************************************************
	REAL*8 FUNCTION CALC_HFS_INTENSITY( I_VALUE, F1_VALUE, F2_VALUE,
     1											 J1_VALUE, J2_VALUE)
	IMPLICIT NONE
	
	REAL*8
     1	I_VALUE,			! I quantum number
     1	F1_VALUE, F2_VALUE,	! F quantum numbers
     1	J1_VALUE, J2_VALUE	! J quantum numbers
	
	REAL*8
     1	P,Q,R,A,B,C		! dummy functions and arguments for statement ftns
	
	REAL*8
     1	F1,F2,F3,F4,		! temporary variables
     1	FMAX,				! temporary intensity
     1	C1					! temporary coefficient
	
	INTEGER*2
     1	IFLAG,				! control flag - we first determine
							! maximum intensity to scale by
     1	IF1,IF2				! temporary variables
C
C   The following are statement functions to simplify the calculations
C
	P(A,B,C) = (A+B)*(A+B+1.0D0) - C*(C+1.0D0)
	Q(A,B,C) = C*(C+1.0D0) - (A-B)*(A-B+1.0D0)
	R(A,B,C) = A*(A+1.0D0) + B*(B+1.0D0) - C*(C+1.0D0)
C
C   Execution starts here
C
	IFLAG = 0
	F1 = I_VALUE + J1_VALUE
	F2 = I_VALUE + J2_VALUE

15	IF1 = INT(2.0D0*F1 + 0.0001D0)
	IF2 = INT(2.0D0*F2 + 0.0001D0)
	IF(IF1 .EQ. IF2) THEN
		C1 = (2.0D0*F1+1.0D0) / (F1*(F1+1.0D0))
	ELSE
		IF(ABS(IF2-IF1) .NE. 2) GO TO 800
	ENDIF

	IF    (J1_VALUE .EQ. J2_VALUE) THEN

		IF    (IF1 .EQ. IF2) THEN
			CALC_HFS_INTENSITY = C1 * R(F1,J1_VALUE,I_VALUE)**2
		ELSEIF(IF1 .LT. IF2) THEN
			CALC_HFS_INTENSITY = ( P(F2,J1_VALUE,I_VALUE) *
     1							   Q(F1,J1_VALUE,I_VALUE)   ) / F2
		ELSE ! IF1 .GT. IF2
			CALC_HFS_INTENSITY = ( P(F1,J1_VALUE,I_VALUE) * 
     1							   Q(F2,J1_VALUE,I_VALUE)   ) / F1
		ENDIF

	ELSEIF(J1_VALUE .GT. J2_VALUE) THEN

		IF    (IF1 .EQ. IF2) THEN
			CALC_HFS_INTENSITY = C1 * P(F1,J1_VALUE,I_VALUE) *
     1								  Q(F1,J1_VALUE,I_VALUE)
		ELSEIF(IF1 .LT. IF2) THEN
			CALC_HFS_INTENSITY = ( Q(F2,J1_VALUE,I_VALUE) *
     1							   Q(F1,J1_VALUE,I_VALUE)   ) / F2
		ELSE ! IF1 .GT. IF2
			CALC_HFS_INTENSITY = ( P(F1,J1_VALUE,I_VALUE) * 
     1							   P(F2,J1_VALUE,I_VALUE)   ) / F1
		ENDIF

	ELSE	! J1_VALUE .LT. J2_VALUE

		IF    (IF1 .EQ. IF2) THEN
			F4 = F1+1.0D0
			F3 = F1-1D0
			CALC_HFS_INTENSITY = C1 * P(F4,J1_VALUE,I_VALUE) *
     1								  Q(F3,J1_VALUE,I_VALUE)
		ELSEIF(IF1 .LT. IF2) THEN
			F4 = F2+1.0D0
			CALC_HFS_INTENSITY = ( P(F2,J1_VALUE,I_VALUE) *
     1							   P(F4,J1_VALUE,I_VALUE)   ) / F2
		ELSE ! IF1 .GT. IF2
			F4=F2-1
			CALC_HFS_INTENSITY = ( Q(F2,J1_VALUE,I_VALUE) *
     1		Q(F4,J1_VALUE,I_VALUE)   ) / F1
		ENDIF
		
	ENDIF
	
	IF(IFLAG.NE.1) THEN
		IFLAG=1
		FMAX=CALC_HFS_INTENSITY
		F1 = F1_VALUE
		F2 = F2_VALUE
		GO TO 15
	ELSE
		CALC_HFS_INTENSITY = CALC_HFS_INTENSITY/FMAX
		RETURN
	ENDIF
	
800	CALC_HFS_INTENSITY = 0.0D0
	RETURN
	END
