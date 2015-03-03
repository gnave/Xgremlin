C***********************************************************************
C*	FUNCTION:	PT_TO_WVN 
C*
C*	DESCRIPTION:
C*		Routine to convert absolute point number to wavenumbers (cm-1)
C*	
C*  INPUTS:
C*		ABSPT	- REAL*8
C*			Absolute point number
C*
C*	OUTPUTS:
C*		WVN		- REAL*8
C*			wavenumber
C*
C*	CALLS:
C*
C*	SPECIAL CONSIDERATIONS:
C*
C*	REVISION HISTORY
C*		15-DEC-1991		Original Version
C*
C***********************************************************************
	SUBROUTINE PT_TO_WVN(ABSPT,WVN)
	IMPLICIT NONE

	INCLUDE 'specfile.h'
	
	REAL*8
     1	ABSPT,		! Absolute point number
     1	WVN			! Wavenumber
	
	REAL*8
     1	VACWVN		! Function to calculate vacuum wavenumber

	WVN = (IDENT%FREE_SPECTRAL_RANGE
     1			/IDENT%TRANSFORM_NPTS)*(ABSPT-1.0D0)

c don't reverse aliases (gn)

C	IF(((-1)**IDENT.SPECTRUM_ORDER) .EQ. -1) THEN
		WVN = WVN + 
     1		(IDENT%SPECTRUM_ORDER-1)*IDENT%FREE_SPECTRAL_RANGE
C	ELSE
C		WVN = IDENT.SPECTRUM_ORDER*IDENT.FREE_SPECTRAL_RANGE
C     1			- WVN
C	ENDIF

	WVN=VACWVN(IDENT%TEMPERATURE,IDENT%PRESSURE,WVN)

	RETURN
	END

C***********************************************************************
C*	SUBROUTINE:	WVN_TO_PT 
C*
C*	DESCRIPTION:
C*		Routine to convert wavenumbers (cm-1) to absolute point numbers.
C*	
C*  INPUTS:
C*		WVN		- REAL*8
C*			Vacuum wavenumber (cm-1)
C*
C*	OUTPUTS:
C*		ABSPT	- REAL*8
C*			Absolute point number
C*
C*	CALLS:
C*		REFNDX	- REAL*8 function
C*			Calculates the refractive index in dry air
C*
C*	SPECIAL CONSIDERATIONS: None
C*
C*	REVISION HISTORY
C*		15-DEC-1991		Original Version
C*
C***********************************************************************
	SUBROUTINE WVN_TO_PT(WVN,ABSPT)
	IMPLICIT NONE
	
	INCLUDE 'specfile.h'

	REAL*8
     1	WVN,		! Vacuum wavenumber
     1	ABSPT		! Absolute point number
	
	REAL*8
     1	AWN			! Air wavenumber
	
	REAL*8
     1	REFNDX		! Function to calculate refractive index
	
	AWN = WVN*REFNDX(IDENT%TEMPERATURE,IDENT%PRESSURE,WVN)
	
c don't reverse aliases (gn)

C	IF(((-1)**IDENT.SPECTRUM_ORDER) .EQ. -1) THEN
		AWN = AWN - 
     1		(IDENT%SPECTRUM_ORDER-1)*IDENT%FREE_SPECTRAL_RANGE
C	ELSE
C		AWN = IDENT.SPECTRUM_ORDER*IDENT.FREE_SPECTRAL_RANGE
C     1			- AWN
C	ENDIF
	
	ABSPT = AWN*IDENT%TRANSFORM_NPTS
     1				/IDENT%FREE_SPECTRAL_RANGE + 1.0D0

	RETURN
	END

C***********************************************************************
C*	FUNCTION:	ORDCR
C*
C*	DESCRIPTION:
C*		Function to correct input for spectral order number.  This routine
C*	handles both wavenumber and point conversions by setting the flag
C*	appropriately
C*	
C*  INPUTS:
C*		VALUE:	REAL*8
C*			The inputted value to be converted.  May be point or wavenumber
C*		NORD:	INTEGER*2
C*			The spectral order of the spectrum.
C*		FREE_SPECTRAL_RANGE:	REAL*8
C*			The free spectral range.
C*		IFLAG:	INTEGER*2
C*			Flag to indicate if VALUE is a point or wavenumber
C*				IFLAG=0:  PTN TO WVN
C*				IFLAG=1:  WVN TO PTN
C*
C*	OUTPUTS:
C*		ORDCR:	REAL*8
C*			The corrected value
C*
C*	SPECIAL CONSIDERATIONS:
C*
C*	REVISION HISTORY
C*		15-DEC-1991		Original Version
C*
C***********************************************************************
	REAL*8 FUNCTION ORDCR(VALUE,NORD,FREE_SPECTRAL_RANGE,IFLAG)
	IMPLICIT NONE
	
	REAL*8
     1	VALUE,
     1	FREE_SPECTRAL_RANGE

	INTEGER*2
     1	NORD,
     1	IFLAG,
     1	NRD
	
C******
C   CORRECT FOR ORDER NUMBER
C
C   IFLAG=0:  PTN TO WVN
C   IFLAG=1:  WVN TO PTN
C******
	NRD=2*(NORD/2)
	IF(IFLAG.EQ.0) THEN
		IF(NRD.EQ.NORD) THEN
			ORDCR = DFLOAT(NORD)*FREE_SPECTRAL_RANGE - VALUE
		ELSE
			ORDCR = VALUE + DFLOAT(NORD-1)*FREE_SPECTRAL_RANGE
		ENDIF
	ELSE
		IF(NRD .EQ. NORD) THEN
			ORDCR = DFLOAT(NORD)*FREE_SPECTRAL_RANGE - VALUE
		ELSE
			ORDCR = VALUE - DFLOAT(NORD-1)*FREE_SPECTRAL_RANGE
		ENDIF
	ENDIF
	RETURN
	END

C***********************************************************************
C*	FUNCTION:	REFNDX 
C*
C*	DESCRIPTION:
C*		Function to compute refractive index of dry air as a function of
C*	temperature (degrees C), pressure (mm Hg), and vacuum wavenumber (cm-1).
C*	
C*  INPUTS:
C*		TEMPERATURE	- REAL*8
C*			Temperature (degrees C)
C*		PRESSURE	- REAL*8
C*			Pressure (mm Hg)
C*		SIGMA		- REAL*8
C*			Vacuum wavenumber (cm-1)
C*
C*	OUTPUTS:
C*		REFNDX		- REAL*8
C*			Refractive index of dry air
C*
C*	CALLS:
C*		DMIN1		- REAL*8
C*			Selects minimum of two double precision variables
C*
C*	SPECIAL CONSIDERATIONS: None
C*
C*	REFERENCE:
C*		INDEX OF STANDARD AIR: PECK AND REEDER,JOSA,V62,958(1972)
C*
C*	REVISION HISTORY
C*		15-DEC-1991		Original Version
C*
C***********************************************************************
	REAL*8 FUNCTION REFNDX(TEMPERATURE,PRESSURE,SIGMA)
	IMPLICIT NONE

	REAL*8
     1	TEMPERATURE,	! Temperature
     1	PRESSURE,		! Pressure
     1	SIGMA			! Vacuum Wavenumber
	
	REAL*8
     1	SIGSQR,			! Sigma squared
     1	STD_INDX_M1,	! Standard index - 1
     1	R				! Pressure/Temperature ratio

	R = PRESSURE*288.18D0 / (760.0D0*(TEMPERATURE+273.18D0))
	SIGSQR = DMIN1(SIGMA*SIGMA,3.7D9)
	STD_INDX_M1 = 8060.51D-8 + 2480990.0D0/(132.274D8-SIGSQR)
     1				+ 17455.7D0/(39.32957D8-SIGSQR)
C
C USE LAW OF BIOT AND MASCART.
C
	REFNDX=1.0D0 + R*STD_INDX_M1
	
	RETURN
	END

C***********************************************************************
C*	FUNCTION:	VACWVN 
C*
C*	DESCRIPTION:
C*		Function to compute vacuum wavenumbers as a function of
C*	wavenumber in dry air at specified temperature (degrees C) and
C*	pressure (mm Hg).
C*	
C*  INPUTS:
C*		TEMPERATURE	- REAL*8
C*			Temperature (degrees C)
C*		PRESSURE	- REAL*8
C*			Pressure (mm Hg)
C*		AIRWVN		- REAL*8
C*			Wavenumber in dry air
C*
C*	OUTPUTS:
C*		VACWVN	- REAL*8
C*			Vacuum wavenumber
C*
C*	CALLS:
C*		REFNDX	- REAL*8 function
C*			Calculates the refractive index
C*
C*	SPECIAL CONSIDERATIONS: None
C*
C*	REVISION HISTORY
C*		15-DEC-1991		Original Version
C*
C***********************************************************************
	REAL*8 FUNCTION VACWVN(TEMPERATURE,PRESSURE,AIRWVN)
	IMPLICIT NONE
	
	REAL*8
     1	TEMPERATURE,	! Temperature
     1	PRESSURE,		! Pressure
     1	AIRWVN			! Wavenumber in air
	
	REAL*8
     1	REFNDX			! Function to calculate refractive index
	
	REAL*8
     1	RINDEX,			! Temporary refractive index
     1	SIGMAV			! Temporary vacuum wavenumber
	
	INTEGER*2	I		! Temporary index
	
	RINDEX = 1.0003D0
	DO 10 I = 1,5
		SIGMAV = AIRWVN/RINDEX
		RINDEX = REFNDX(TEMPERATURE,PRESSURE,SIGMAV)
10	CONTINUE
	VACWVN=AIRWVN/RINDEX
	
	RETURN
	END

