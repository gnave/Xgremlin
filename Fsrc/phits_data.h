C***********************************************************************
C*	MODULE: phits_data
C*
C*	© Bruce Pulliam, 1993 - All Rights Reserved
C*
C*	DESCRIPTION:
C*		Common data area for miscellaneaous data that is commonly used but 
C*  does not require toolbox definitions.
C* 
C*	SUBROUTINE/FUNCTIONS INCLUDED
C*		NONE
C*
C*	REVISION HISTORY
C*		25-MAY-1993		Baseline Version 0.1.0
C*		08-AUG-1993		added CurFitType, CurActStruct
C*						Baselined for Version 0.4.0
C*		10-OCT-1993		added CurPeakType; deleted Threshold
C*
C***********************************************************************
	INTEGER*2		NPTS_TO_DISPLAY		! no. of points in display
	REAL*8			BEG_WVN(2)			! beg/end wavenumbers being displayed
	REAL*8			END_WVN(2)
	INTEGER*2		MODE(2)				! display mode (1 = linear, 2 = log-linear)
	INTEGER*2		DEFAULT_MODE		! used in REVERT
	REAL*4			HEIGHT				! height (inches)
	REAL*4  		WIDTH				! width (inches)
	REAL*4			Y_SCALE_FACTOR		! data scale factor
	REAL*4  		Y_AXIS_OFFSET		! pos. of y-axis
	LOGICAL*2 		DISPLAY_FIT			! flag for fit
	INTEGER*2 		DISPLAY_STATUS		! what is being disp.
	
	COMMON	/DISPLAY_DATA/ 	BEG_WVN, END_WVN,
     1			HEIGHT, WIDTH, Y_SCALE_FACTOR, Y_AXIS_OFFSET, 
     1                  MODE, DEFAULT_MODE,
     1			DISPLAY_FIT, DISPLAY_STATUS, NPTS_TO_DISPLAY
	
C	STRUCTURE	/DISPLAY_SETS/
        TYPE DISPLAY_SETS
                SEQUENCE
		CHARACTER*10	NAME
		INTEGER*2		TYPE
		INTEGER*2		COLOR
		INTEGER*2		VISIBILITY
        END TYPE
C	END STRUCTURE
	
C	RECORD	/DISPLAY_SETS/ DISPLAY_SET
        TYPE(DISPLAY_SETS) DISPLAY_SET
	INTEGER*2	DISPLAYED_DATA
	INTEGER*2	INTRP, RAW                                                                                                          
	PARAMETER	(RAW = 1, INTRP =2)
	INTEGER*2	DISPLAY_COMMAND
	
	COMMON	/DISPLAY_SET/ DISPLAY_SET, DISPLAYED_DATA, DISPLAY_COMMAND
	
C
C  Control Flags
C
	LOGICAL*2	IsSpectraOpen			! Flag indicating if spectrum file is open
	LOGICAL*2	CURRENT_LINE_EXISTS
	INTEGER*2	CurFitType				! Current fit type (e.g. kHFSfit, kHgFit)
	INTEGER*2	CurPeakType				! Current peak location type
	INTEGER*2	CurActStruct			! Current active structure in kHFS type
	
	COMMON	/CONTROL_FLAGS/	 IsSpectraOpen, CURRENT_LINE_EXISTS, 
     1                    CurFitType, CurPeakType,CurActStruct
