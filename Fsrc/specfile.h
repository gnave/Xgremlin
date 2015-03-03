C***********************************************************************
C*	MODULE: specfile
C*
C*	© Bruce Pulliam, 1993 - All Rights Reserved
C*
C*	DESCRIPTION:
C*		Common data area for data associated with the spectrum data file.
C* 
C*	SUBROUTINE/FUNCTIONS INCLUDED
C*		NONE
C*
C*	REVISION HISTORY
C*		25-MAY-1993		Baseline Version 0.1.0
C*
C***********************************************************************
C                                                                                                                              
C Ident block for Spectrum file and Output file                                                                                
C                                                                                                                              
C Modified for use with gfortran (GN 6Apr09)
C	STRUCTURE /IDENT_STRUC/                                                                                                             
        TYPE IDENT_STRUC
            SEQUENCE
	    CHARACTER*80	TITLE				! Spectrum title
	    REAL*8			BEG_PTN				! 1st point in spectrum
	    REAL*8			END_PTN				! last point 
	    REAL*8			TEMPERATURE			! temperature (¡C)
	    REAL*8			PRESSURE			! pressure (mm Hg)
	    REAL*8			FREE_SPECTRAL_RANGE	! free spectral range (cm-1)
	    REAL*8			NPTS_APODIZED		! number of points apodized
	    REAL*8			NPTS_MEASURED		! number of points measured
	    REAL*8			TRANSFORM_NPTS		! number of points transformed
	    INTEGER*2		BASE_FLAG			! base flag (1 subtract base, 0 don't)
	    INTEGER*2		SPECTRUM_ORDER		! order number
	    INTEGER*2		NUM_INTRPT_PTS		! number of points interpolated
	    INTEGER*2		INVERT				! spectrum inverted
		INTEGER*2		UNIQUE_ID			! unique identifier for spectrum file
		INTEGER*2		BASE_SET			! base calculation flag (see NOISE.TXT)
		CHARACTER*10	SHORT_NAME			! nickname for spectrum for short fields
		REAL*4	 		THRESHOLD			! threshold set by user
		REAL*4	 		BASE				! base (to subtract blackbody radiation)
		REAL*4			RMS_NOISE
		REAL*4			SCALE(2)			! y-scale factor for raw and intrpt
		REAL*4			DEFAULT_SCALE		! default y-scale factor for REVERT cmd
	    REAL*8			BEG_ABSPT(2)		! start/end absolute point num in buffer                                                                                                       
	    REAL*8			END_ABSPT(2)
	    INTEGER*4		BEG_PT(2)			! start/end relative point num in buffer
	    INTEGER*4		END_PT(2)
		INTEGER*4		FIRST_DISPLAY_BLOCK
		INTEGER*4		LAST_DISPLAY_BLOCK
        END TYPE
C	END STRUCTURE
C
C  Spectrum data points
C
C	STRUCTURE /SPEC_STRUC/
        TYPE SPEC_STRUC
		SEQUENCE
		REAL*4	DATA(1280,2)
        END TYPE
C	END STRUCTURE
	
C	RECORD	/IDENT_STRUC/ IDENT
C	RECORD	/SPEC_STRUC/  SPEC
        TYPE(IDENT_STRUC) IDENT
        TYPE(SPEC_STRUC) SPEC
	
	REAL*8	CUR_ABSPT
	REAL*8	SPEC_CONS
	REAL*8	SPEC_FACTOR
	
	COMMON	/IDENT/ IDENT, SPEC, CUR_ABSPT, SPEC_CONS, SPEC_FACTOR
