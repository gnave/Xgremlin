C***********************************************************************C*	MODULE: fit_draw
C*
C*	© Bruce Pulliam, 1993 - All Rights Reserved
C*
C*	DESCRIPTION:
C*		Common data area for the storage of the sinc instrumental function
C*  the Voigt profile, and miscellaneaous other data associated with drawing
C*  the fit.
C* 
C*	SUBROUTINE/FUNCTIONS INCLUDED
C*		NONE
C*
C*	REVISION HISTORY
C*		25-MAY-1993		Baseline Version 0.1.0
C*       7-JUN-1993		Restored the ability to store separate ftns for the
C*						possible three structures being fit simultaneouly
C*						during a kHFS3fit.
C*						Baseline Version 0.2.0
C*
C***********************************************************************
C
C   Drawing storage
C
	INTEGER*2  NLEN2
	PARAMETER (NLEN2=13)
	REAL*4 
     1	SINC(32*NLEN2+1),
     1	VOIGT(768+32*NLEN2,3),
     1	TOTAL_FIT(768+32*NLEN2),
     1	SINC_PEAK,
     1	CONV_VOIGT(768,3)
	INTEGER*2
     1	LIM1,
     1	LIM2,
     1	LIM3,
     1	STRUCTURE_NO,
     1	NPTS_SINC,
     1	NPTS_SINC_2

	COMMON /DRWDAT/ CONV_VOIGT, VOIGT, TOTAL_FIT, SINC, SINC_PEAK,
     1	LIM1, LIM2, LIM3, STRUCTURE_NO, NPTS_SINC, NPTS_SINC_2

C	REAL*4
C	1	SMLP,
C	1	FX,
