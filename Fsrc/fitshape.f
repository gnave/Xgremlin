C***********************************************************************
C*	SUBROUTINE:	VOIGT
C*
C*	DESCRIPTION:
C*		Calculates a voigt profile along with its parital derivatives
C*	using an approximation worked out by J. Keilkopf. Refer to 
C*	JOSA Vol. 63 No. 8 pp987-995 for details.
C*	
C*  INPUTS:
C*		SIGMA		- REAL*8
C*			Starting vacuum wavenumber
C*		ETA			- REAL*8
C*			Shape factor as defined by Keilkopf
C*		HALFWIDTH	- REAL*8
C*			Halfwidth of voigt profile (in points)
C*		FNUM		- REAL*8
C*
C*		NCONV		- INTEGER*2
C*
C*	OUTPUTS:
C*		VOIGT_PROF	- REAL*8
C*			array of points defining the voigt profile
C*		DERV_SIGMA	- REAL*8
C*			array of points defining the derivative rsp. sigma
C*		DERV_ETA	- REAL*8
C*			array of points defining the derivative rsp. eta
C*		DERV_HWDTH	- REAL*8
C*			array of points defining the derivative rsp. halfwidth
C*
C*	CALLS:
C*
C*	SPECIAL CONSIDERATIONS:
C*
C*	REVISION HISTORY
C*		15-DEC-1991		Original Version
C*
C***********************************************************************
	SUBROUTINE hfsVOIGT(VOIGT_PROF, DERV_SIGMA, DERV_ETA, DERV_HWDTH,
     1	SIGMA_BEG, ETA, HALFWIDTH, FNUM, NCONV)
	IMPLICIT NONE

	REAL*8
     1	VOIGT_PROF(*),
     1	DERV_SIGMA(*),
     1	DERV_ETA(*),
     1	DERV_HWDTH(*),
     1	SIGMA_BEG,
     1	ETA,
     1	HALFWIDTH,
     1	FNUM
	
	INTEGER*2
     1	I,
     1	NCONV
	
	REAL*8
     1	OMF,
     1	ETAN,
     1	SIG,
     1	SIGMA,
     1	ARG,
     1	FLRZ,
     1	GAUS,TMP1,TMP2,ERSIG,GMF,EGMF,DRG,FLRZ2,ERS2,DER
	
	REAL*8
     1	FLN2,
     1	F2LN2
	PARAMETER (FLN2=.693147181D0,F2LN2=1.386294361D0)

	OMF  = 1.0D0 - ETA
	ETAN = ETA*OMF
	DO 100 I=1,NCONV
		SIG   = SIGMA_BEG + DFLOAT(I-1)/FNUM
		SIGMA = SIG/HALFWIDTH
		ARG   = SIGMA*SIGMA
		FLRZ  = 0.0D0
		IF(ARG .LT. 1.0D12) FLRZ = 1.0D0/(1.0D0+ARG)
		GAUS  = 0.0D0
		IF(ARG .LT. 30.0D0) GAUS = EXP(-FLN2*ARG)
		TMP1  = 0.8029D0 - 0.4207D0*ARG
		TMP2  = 1.0D0 + (0.2030D0 + 0.07335D0*ARG)*ARG
		ERSIG = TMP1/TMP2
		GMF   = GAUS - FLRZ
		EGMF  = ERSIG*GMF
		VOIGT_PROF(I) = OMF*GAUS + (FLRZ+OMF*EGMF)*ETA
		DRG   = -F2LN2*SIGMA*GAUS
		FLRZ2 = -2.0D0*SIGMA*FLRZ*FLRZ
		TMP1  = (-1.1674D0+(-0.2356D0+0.0617D0*ARG)*ARG)*SIGMA
		TMP2  = 1.0D0+(0.4060D0+(0.1879D0+(0.0298D0
     1							+0.00538D0*ARG)*ARG)*ARG)*ARG
		ERS2  = TMP1/TMP2
		DER   = OMF*DRG+FLRZ2*ETA+ETAN*ERSIG*(DRG-FLRZ2)+ETAN*GMF*ERS2
		DERV_SIGMA(I)  = -DER/HALFWIDTH
		DERV_ETA(I)    = (((1.0D0-2.0D0*ETA)*ERSIG-1.0D0)*GMF)
		DERV_HWDTH(I)  = -SIG*DER/(HALFWIDTH*HALFWIDTH)
100	CONTINUE
	RETURN
	END

C***********************************************************************
C*	SUBROUTINE:	Ddbl_Cnvl
C*
C*	DESCRIPTION:
C*		Routine to perform a double precision convolution between arrays.
C*	
C*  INPUTS:
C*		CNVL_FTN	- REAL*8 array
C*			Convolution function
C*		DATA_ARRAY	- REAL*8 array
C*			array of data points to be convoluted with convolution function
C*		NPTS_CNVL	- INTEGER*2
C*			number of points in convoluting function
C*		NPTS_DATA	- INTEGER*2
C*			number of points in data array
C*
C*	OUTPUTS:
C*		RESULT		- REAL*8
C*			result
C*
C*	CALLS: None
C*
C*	SPECIAL CONSIDERATIONS: None
C*
C*	REVISION HISTORY
C*		15-DEC-1991		Original Version
C*
C***********************************************************************
	SUBROUTINE Dbl_Cnvl(CNVL_FTN, DATA_ARRAY, RESULT,
     1					  NPTS_DATA, NPTS_CNVL)
	IMPLICIT NONE

	REAL*8
     1	CNVL_FTN(*),	! Convoluting function
     1	DATA_ARRAY(*),	! Data array to be convoluted
     1	RESULT		! Resultant data array
	
	INTEGER*2
     1	NPTS_CNVL,		! Number of points in convoluting function
     1	NPTS_DATA,		! Number of points in original data array
     1	J, K			! temporary indices
C
	DO 100 J = 1,NPTS_DATA
	RESULT = 0.0D0
	DO 100 K = 1,NPTS_CNVL
100	RESULT = RESULT + CNVL_FTN(K)*DATA_ARRAY(J+K-1)
	RETURN
	END

C***********************************************************************
C*	SUBROUTINE:	DCONVOLUTE 
C*
C*	DESCRIPTION:
C*		Routine to perform a double precision convolution between arrays.
C*	
C*  INPUTS:
C*		CNVL_FTN	- REAL*8 array
C*			Convolution function
C*		DATA_ARRAY	- REAL*8 array
C*			array of data points to be convoluted with convolution function
C*		NPTS_CNVL	- INTEGER*2
C*			number of points in convoluting function
C*		NPTS_DATA	- INTEGER*2
C*			number of points in data array
C*		INTERVAL	- INTEGER*2
C*			interval at which to place results
C*
C*	OUTPUTS:
C*		RESULT		- REAL*8 array
C*			result array
C*
C*	CALLS: None
C*
C*	SPECIAL CONSIDERATIONS: None
C*
C*	REVISION HISTORY
C*		15-DEC-1991		Original Version
C*
C***********************************************************************
	SUBROUTINE DCONVOLUTE(CNVL_FTN, DATA_ARRAY, RESULT,
     1					  NPTS_DATA, NPTS_CNVL, INTERVAL)
	IMPLICIT NONE

	REAL*8
     1	CNVL_FTN(*),	! Convoluting function
     1	DATA_ARRAY(*),	! Data array to be convoluted
     1	RESULT(*)		! Resultant data array
	
	INTEGER*2
     1	NPTS_CNVL,		! Number of points in convoluting function
     1	NPTS_DATA,		! Number of points in original data array
     1	INTERVAL,		! Interval at which to place results
						!   (= number of points to be interpolated)
     1	JJ, J, K		! temporary indices
C
	JJ = 1 - INTERVAL
	DO 100 J = 1,NPTS_DATA
	JJ = JJ + INTERVAL
	RESULT(JJ) = 0.0D0
	DO 100 K = 1,NPTS_CNVL
100	RESULT(JJ) = RESULT(JJ) + CNVL_FTN(K)*DATA_ARRAY(J+K-1)
	RETURN
	END

C***********************************************************************
C*	FUNCTION:	SCONVOLUTE 
C*
C*	DESCRIPTION:
C*		Routine to perform a single precision convolution between arrays.
C*	
C*  INPUTS:
C*		CNVL_FTN	- REAL*4 array
C*			Convolution function
C*		DATA_ARRAY	- REAL*4 array
C*			array of data points to be convoluted with convolution function
C*		NPTS_CNVL	- INTEGER*2
C*			number of points in convoluting function
C*		NPTS_DATA	- INTEGER*2
C*			number of points in data array
C*		INTERVAL	- INTEGER*2
C*			interval at which to place results
C*
C*	OUTPUTS:
C*		RESULT		- REAL*4 array
C*			result array
C*
C*	CALLS: None
C*
C*	SPECIAL CONSIDERATIONS: None
C*
C*	REVISION HISTORY
C*		15-DEC-1991		Original Version
C*
C***********************************************************************
	SUBROUTINE SCONVOLUTE(CNVL_FTN, DATA_ARRAY, RESULT,
     1					  NPTS_DATA, NPTS_CNVL, INTERVAL)
	IMPLICIT NONE

	REAL*4
     1	CNVL_FTN(*),	! Convoluting function
     1	DATA_ARRAY(*),	! Data array to be convoluted
     1	RESULT(*)		! Resultant data array
	
	INTEGER*2
     1	NPTS_CNVL,		! Number of points in convoluting function
     1	NPTS_DATA,		! Number of points in original data array
     1	INTERVAL,		! Interval at which to place results
					!   (= number of points to be interpolated)
     1	JJ, J, K		! temporary indices
C
	JJ = 1 - INTERVAL
	DO 100 J = 1,NPTS_DATA
	JJ = JJ + INTERVAL
	RESULT(JJ) = 0.0D0
	DO 100 K = 1,NPTS_CNVL
100	RESULT(JJ) = RESULT(JJ) + CNVL_FTN(K)*DATA_ARRAY(J+K-1)
	RETURN
	END
