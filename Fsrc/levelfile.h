C***********************************************************************
C*	MODULE: levelfile
C*
C*	© Bruce Pulliam, 1993 - All Rights Reserved
C*
C*	DESCRIPTION:
C*		Common data area for data associated with the level files
C* 
C*	SUBROUTINE/FUNCTIONS INCLUDED
C*		NONE
C*
C*	REVISION HISTORY
C*		25-MAY-1993		Baseline Version 0.1.0
C*
C***********************************************************************
C
C  Structure for energy level information
C
C Modify for use with gfortran (GN 6Apr09)
C	STRUCTURE /LEVEL/
        TYPE LEVEL
                SEQUENCE
		CHARACTER*32	NAME				! level designation
		REAL*4			J_VALUE				! J-value
		INTEGER*2		LEV_ID				! Unique level number for parity
		REAL*8			ENERGY_LEVEL		! Energy level in cm-1
		REAL*4			A_SPLIT				! A splitting factor in cm-1
		REAL*4			B_SPLIT				! B splitting factor in cm-1
        END TYPE
C	END STRUCTURE

	INTEGER*2	MAX_NUM_LEVELS
	INTEGER*2	MAX_LEVEL_NUM
	PARAMETER (MAX_NUM_LEVELS = 2000, MAX_LEVEL_NUM = 1000)
		
C	RECORD	/LEVEL/ ODD_LEVEL(MAX_NUM_LEVELS,2)
C	RECORD	/LEVEL/ EVEN_LEVEL(MAX_NUM_LEVELS,2)
        TYPE(LEVEL)     ODD_LEVEL(MAX_NUM_LEVELS,2)
        TYPE(LEVEL)     EVEN_LEVEL(MAX_NUM_LEVELS,2)

	
	REAL*8		NUCLEAR_SPIN(2)
	INTEGER*2	NUM_ODD_LEVELS(2), NUM_EVEN_LEVELS(2)
	INTEGER*2	ODD_MAP(MAX_LEVEL_NUM,2), EVEN_MAP(MAX_LEVEL_NUM,2)
	INTEGER*2	LevSelParity(2), LevSelected(2)
	INTEGER*2	LastOddID(2), LastEvenID(2)
	
	COMMON	/LEVELS/ NUCLEAR_SPIN, NUM_ODD_LEVELS, NUM_EVEN_LEVELS, 
     1	ODD_LEVEL, EVEN_LEVEL, ODD_MAP, EVEN_MAP, 
     1	LevSelParity, LevSelected, LastOddId, LastEvenID
