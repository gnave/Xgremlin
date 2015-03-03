C***********************************************************************
C*	MODULE: HFSline
C*
C*	© Bruce Pulliam, 1993 - All Rights Reserved
C*
C*	DESCRIPTION:
C*			Global common data area for output data record.  Each record
C*		is 256 bytes.  By using Unions of Maps the data area can handle multiple
C*		fit types.  Current fit types are:
C*			1 - not used - reserved for Where command
C*			2 - kWtAvg - Weighted average - of all points between two cursor pos.
C*			3 - kCentroid - Centroid - average of all data pts above threshold
C*							on each side of cursor position
C*			4 - kDerv - Derivitive - pos where 1st derivative changes sign
C*			5 - kHFS3FIt - HFS - up to 3 hyperfine structures fitted simultaneously
C*			6 - kHgFIt - special case for Hg - 6 isotopes of which 2 have HFS
C*
C*		Up to 7 lines of type 1 through 4 can be packed into a single output
C*		record; however, they must be kept in numerical order with no lines of
C*		any other types in between.  This means that if a line is measured with a
C*		type other than 1-4 and the position of that line falls between two lines
C*		within the packed output record, then the packed output record must be
C*		broken into two separate records with the measured line record linked
C*		in between.
C*			Module also contains parameters, structures and commons for
C*		the directory of currently visible lines, lines in Output Dialog
C*		list and fit data.
C* 
C*	SUBROUTINE/FUNCTIONS INCLUDED
C*			None
C*
C*	REVISION HISTORY
C*		29-MAY-1993		Combined line_desc and fit_data into one file	
C*						added Unions to handle multiple fit types
C*						Baseline for ver 0.2.0
C*		12-JUN-1993		changed Fit.Beg_Ptn, Fit.End_Ptn to
C*							Fit.Beg_abspt, Fit.End_abspt to avoid confusion
C*              6-APR-2009              port for gfortran (GN)
C*
C***********************************************************************
C	STRUCTURE	/TRANSITION/			! 12 bytes of info on transition
        TYPE TRANSITION
		SEQUENCE
 		INTEGER*2	SpectraID				! Spectrum ID number
		INTEGER*2	Odd_Level				! Odd level number
		INTEGER*2	Even_Level				! Even level number
		INTEGER*2	Odd_JVal				! Odd J value	(*10.0)
		INTEGER*2	Even_JVal				! Even J value	(*10.0)
		INTEGER*2	High					! Higher level (1 = odd, -1 = even)
        END TYPE
C	END STRUCTURE
	
C	STRUCTURE	/PEAK/					! 24 bytes of info for a peak measurement
        TYPE PEAK
		SEQUENCE
		INTEGER*2	TYPE					! fit type (see above module description)
		INTEGER*2	ORDER					! spectrum order
		REAL*8		PTN						! absolute pt # of peak
		REAL*8		INTENSITY				! intensity of peak
		INTEGER*2	Left_Hw					! left of cg extent (units: pts*100)
		INTEGER*2	Right_Hw				! right of cg extent (units: pts*100)
        END TYPE
C	END STRUCTURE
	
C	STRUCTURE	/PARAM1/				! Types 1-4 output - peak meas. only 
        TYPE PARAM1
		SEQUENCE
		INTEGER*2	NO_OF_PEAKS				! number of lines in output record
C		RECORD	/PEAK/ PEAK(7)				! data for lines
                TYPE(PEAK)	PEAK(7)
		INTEGER*2	Fill(3)		! fill to 176 bytes
        END TYPE
C	END STRUCTURE
	
C	STRUCTURE	/PARAM5/				! Type 5 output - 8 params
        TYPE PARAM5
		SEQUENCE
		REAL*8		P(8)					! parameters, cg,inten,A,B,A,B,eta,hw
		REAL*8		AsymDevP(8)				! asymtopic deviations
		INTEGER*2	Fill(21)	! fill to 176 bytes
		INTEGER*2	Num_Structures
		INTEGER*2	NEXT_STRUCTURE			! Link list of structures fitted together
		INTEGER*2	LAST_STRUCTURE
        END TYPE
C	END STRUCTURE

	
C	STRUCTURE	/PARAM6/				! Type 6 output - 22 params
        TYPE PARAM6
		REAL*4		P(22)					! parameters - 6 isotopes/2 having HFS
		REAL*4		AsymDevP(22)			! asymtopic deviations
        END TYPE
C	END STRUCTURE

C	STRUCTURE	/OUTPUT_LINE/			! output line record - 256 bytes
        TYPE OUTPUT_LINE
                SEQUENCE
 
C 		RECORD	/PEAK/				PEAK
C		RECORD	/TRANSITION/		LINE
                TYPE(PEAK) 		PEAK
                TYPE(TRANSITION)	LINE
C  Remove for use with gfortran (GN 6Apr09)
C		UNION
C  			MAP
C				RECORD	/PARAM1/	PARAM1		! Types 1-4
C			END MAP
C			MAP
C 				RECORD	/PARAM5/	PARAM5		! Type 5 - HFS
C			END MAP
C 			MAP
C				RECORD	/PARAM6/	PARAM6		! Type 6 - isotopes
C			END MAP
C		END UNION
C Only preserve HFS part of code (GN 6Apr09)
        TYPE(PARAM5) PARAM5  
		REAL*4		THRESHOLD		! Threshold used in measurement
		REAL*4		BASE			! Baseline used in line measurement
		REAL*4		RMS				! RMS noise
		REAL*4		FIT_RMS			! RMS of fit
		CHARACTER*24 COMMENT		! comment
		INTEGER*2	FORWARD_PTR		! doubly-linked queue of output records
		INTEGER*2	BACKWARD_PTR	!		(ptrs must be converted to I4)
        END TYPE
C	END STRUCTURE

C	RECORD /OUTPUT_LINE/	CURRENT_LINE
        TYPE(OUTPUT_LINE) 	CURRENT_LINE
	
	COMMON	/CURRENT_LINE/	CURRENT_LINE
C
C  Set up structures and commons for directory information
C
C	STRUCTURE	/LINE_INFO/			! in memory directory of lines in current area
	TYPE LINE_INFO
		SEQUENCE
                TYPE(PEAK) 	PEAK
C		RECORD	/PEAK/	PEAK
		INTEGER*2	SpectraID
		INTEGER*2	ODD_LEVEL
		INTEGER*2	EVEN_LEVEL
		INTEGER*2	LINE_PTR			! only nonzero for packed records
		INTEGER*4	REC_PTR				! output rec# of line 
        END TYPE
C	END STRUCTURE

	
C	STRUCTURE	/DIRECTORY/
        TYPE DIRECTORY
		SEQUENCE
		INTEGER*2	NLINES				! number of lines in directory
		TYPE(LINE_INFO) 	LINE(100)
C		RECORD	/LINE_INFO/ LINE(100)	! info on lines in directory
        END TYPE
C	END STRUCTURE
	
C	RECORD	/Directory/ DIR
	TYPE(Directory) 	DIR
	REAL*4		DUMMY_LINE(64)
	
	COMMON	/Directory/ DIR, DUMMY_LINE
C
C  Set up structures and common for Output Dialog List
C
	INTEGER*2	NumVisibleOut
	INTEGER*2	MaxOutInList
	PARAMETER	(NumVisibleOut = 14)
	PARAMETER	(MaxOutInList = 64)
	
C	STRUCTURE	/OUT_LIST/
	TYPE OUT_LIST
		SEQUENCE
		INTEGER*2	NLINES				! num of output lines in output dialog box
		INTEGER*2	OutLineSelected		! line selected in output dialog box
		LOGICAL*2	OutAtBottom			! no more lines below
		LOGICAL*2	OutAtTop			! no more lines above
		TYPE(LINE_INFO)		LINE(2000)
C		RECORD	/LINE_INFO/ LINE(2000)	! info on lines in output dialog box
	END TYPE
C	END STRUCTURE
	
C	RECORD	/OUT_LIST/ OUTPUT_LIST
	TYPE(OUT_LIST) OUTPUT_LIST
	
	COMMON	/OUT_LIST/ OUTPUT_LIST
C
C  Set up structures and commons for fitting information
C
	INTEGER*2 MAX_NUM_PARAMS
	INTEGER*2 MAX_NUM_STRUCT
	INTEGER*2 MAX_NUM_ISOTOPES
	INTEGER*2 MAX_NUM_COMPONENTS
	INTEGER*2 MAX_FIT_PTS
	INTEGER*2 TOTAL_NUM_PARAMS
	PARAMETER (MAX_NUM_PARAMS=22)
	PARAMETER (MAX_NUM_STRUCT=3)
	PARAMETER (MAX_NUM_ISOTOPES=6)
	PARAMETER (MAX_NUM_COMPONENTS=40)
	PARAMETER (MAX_FIT_PTS=1000)
	PARAMETER (TOTAL_NUM_PARAMS=MAX_NUM_PARAMS*MAX_NUM_STRUCT)

C	STRUCTURE	/Hyperfine_Struct/
	TYPE Hyperfine_Struct
		SEQUENCE
		INTEGER*2	ORDER			! Spectrum order
		REAL*8		NUCLEAR_SPIN	! Nuclear spin of spectra
		INTEGER*2	NUM_COMPONENTS	! Number of components in structure
		REAL*8		Sum_Intensities	! sum of all relative intensities
		REAL*8		REL_INTENSITY(MAX_NUM_COMPONENTS)	! of components
		REAL*8		CF(4,MAX_NUM_COMPONENTS)	! split coefficients
		REAL*8		PTN(MAX_NUM_COMPONENTS)		! pt pos of components
	END TYPE
C	END STRUCTURE
	
C	STRUCTURE	/H_FIT/
        TYPE H_FIT
		SEQUENCE
C		RECORD	/PEAK/	PEAK				! peak summary info
                TYPE(PEAK) 	PEAK
		INTEGER*2	NUM_PARAMS				! # parameters being fitted
		REAL*8		P(TOTAL_NUM_PARAMS)		! parameter values
		REAL*8	MATRIX(TOTAL_NUM_PARAMS+1,TOTAL_NUM_PARAMS+1) ! fit matrix
		LOGICAL*2	SWITCH(TOTAL_NUM_PARAMS)	! hold value switches
		REAL*8		REFRACTIVE_INDEX		! refractive_index of spectrum
		REAL*8		BEG_ABSPT			! 1st absolute pt # of fit data
		REAL*8		END_ABSPT			! last absolute pt # of fit data
		REAL*8		RMS				! a weighted RMS error of fit
		REAL*8		ERR				! total RMS error of fit
		INTEGER*2	LPTS				! # pts above spectrum RMS
		INTEGER*2	NPTS				! # primary points in fit
		INTEGER*2	NPTS_INTRPT			! # of interpolated points
		REAL*8		DATA_PTS(MAX_FIT_PTS)		! spectrum data being fitted
		REAL*8		WEIGHT(MAX_FIT_PTS)		! fit weigt for data points

C Modify for use with gfortran (GN 6Apr09)
C		UNION
C		   MAP						! type HFS3Fit (type 5) fit
C		     INTEGER*2	NUM_STRUCTURES			! # hyperfine structures
C		     RECORD  /TRANSITION/  LINE(MAX_NUM_STRUCT)	! level info
C		     RECORD  /Hyperfine_Struct/  STRUCT(MAX_NUM_STRUCT)	! comp. info
C		   END MAP
C		   MAP						! type HgFit (type 6) fit
C		     INTEGER*2	NUM_ISOTOPES	! # isotopes
C		     RECORD  /TRANSITION/    LEVELS		! isotope info
C		     RECORD /Hyperfine_Struct/ ISOTOPE(MAX_NUM_ISOTOPES) ! ISOTOPE COMP. Info
C		   END MAP
C		END UNION
        
        INTEGER*2  NUM_STRUCTURES                  ! # hyperfine structures
        TYPE(TRANSITION) LINE(MAX_NUM_STRUCT)
        TYPE(Hyperfine_Struct) STRUCT(MAX_NUM_STRUCT)

        END TYPE
C	END STRUCTURE
	
C	RECORD	/H_FIT/	Fit, DialogFit
        TYPE(H_FIT)	Fit, DialogFIt
	LOGICAL*2	CurFitExists
	LOGICAL*2	CurFitValid
	
	COMMON	/H_FIT/	Fit, DialogFit, CurFitExists, CurFitValid

