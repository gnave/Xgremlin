C***********************************************************************
C*	MODULE: display_ftns.f
C*
C*	© Bruce Pulliam, 1993 - All Rights Reserved
C*
C*	DESCRIPTION:
C*		Routine to calculate the voigt and instrumental function profiles
C*	for display fits.
C* 
C*	SUBROUTINE/FUNCTIONS INCLUDED
C*		Calc_Display_Fit - calculate the fit to be displayed
C*		Display_Voigt 	 - calculate voigt profile for fit
C*		Display_Inst_Ftn - calculate sinc instrumental function for spectrum
C*
C*	REVISION HISTORY
C*		25-MAY-1993		Baseline Version 0.1.0
C*		 7-JUN-1993		modified for mutiple fit data types
C*						restored ability to calculate three different Voigts
C*						Baseline Version 0.2.0
C*
C***********************************************************************
!!G Toolbox.finc
!!MP Inlines.f
C***********************************************************************
C*	ROUTINE: Calc_Display_Fit
C*
C*	DESCRIPTION:
C*		Routine to calculate a fit to data using the current fit data.
C*  Works with multiple fit types.
C* 
C*	CALLS:
C*		Wvn_To_Ptn	- converts wavenumber [cm-1) to absolute point number
C*
C*	CALLED BY:
C*		Draw_Spectrum
C*
C*	REVISION HISTORY
C*
C***********************************************************************
	SUBROUTINE CALC_DISPLAY_FIT
	IMPLICIT NONE

	INCLUDE 'global_constants.h'
	INCLUDE 'specfile.h'
	INCLUDE 'hfsline.h'
	INCLUDE 'fit_draw.h'

	REAL*8	VWN, WAVNUM
	REAL*4  A1, A2, XFTN, SG, FAR
	INTEGER*2  IP, KN, IFAR
	INTEGER*2  NTMP
	INTEGER*2  I, J, K
	INTEGER*2	NUM_STRUCT, NUM_COMP
	INTEGER*4  INDX1, INDX2, INDX3
C 
C   Calculate component positions and intensities 
C 
	NPTS_SINC = 20*IDENT%NUM_INTRPT_PTS
	SELECT CASE (FIT%PEAK%TYPE)
	
		CASE (kHFS3Fit)
		
			IP=0
			DO KN=1,FIT%NUM_STRUCTURES
				IP = IP+8
				NTMP = 20*FIT%P(IP)*IDENT%NUM_INTRPT_PTS
				IF(NTMP.GT.NPTS_SINC) NPTS_SINC = NTMP
			ENDDO
			IF(NPTS_SINC .GT. 768) NPTS_SINC = 768
			NPTS_SINC_2 = NPTS_SINC/2 + 1 
			DO KN=1,FIT%NUM_STRUCTURES
				IP=8*(KN-1)
				DO I=1,FIT%STRUCT(KN)%NUM_COMPONENTS 
				     VWN = FIT%STRUCT(KN)%CF(1,I)*FIT%P(IP+1) +
     1				       FIT%STRUCT(KN)%CF(2,I)*FIT%P(IP+2) +
     1				       FIT%STRUCT(KN)%CF(3,I)*FIT%P(IP+3) +
     1				       FIT%STRUCT(KN)%CF(4,I)*FIT%P(IP+4) + FIT%P(IP+5)
				     CALL WVN_TO_PT(VWN,FIT%STRUCT(KN)%PTN(I))
				ENDDO
			ENDDO

C		CASE (kHgFit)
C
C			NTMP = 20*FIT%P(22)*IDENT%NUM_INTRPT_PTS
C			IF(NTMP.GT.NPTS_SINC) NPTS_SINC = NTMP
C			IF(NPTS_SINC .GT. 768) NPTS_SINC = 768
C			NPTS_SINC_2 = NPTS_SINC/2 + 1
C			
C			CALL WVN_TO_PT(Fit%P(1), FIT%ISOTOPE(1)%PTN(1))			! Hg 200
C			WAVNUM = Fit%P(1) + Fit%P(2)
C			CALL WVN_TO_PT(WAVNUM, FIT%SOTOPE(2)%PTN(1))			! Hg 202
CC			WAVNUM = Fit%P(1) + Fit%P(3)*Fit%P(2)
C			CALL WVN_TO_PT(WAVNUM, FIT%ISOTOPE(3)%PTN(1))			! Hg 198
CC			DO I = 1,Fit%ISOTOPE(4)%Num_Components
C				WAVNUM =   Fit%ISOTOPE(4)%CF(1,I)*FIt%P(13) 
C     1						 + Fit%ISOTOPE(4)%CF(2,I)*FIt%P(14)
C     1						 + Fit%ISOTOPE(4)%CF(3,I)*Fit%P(15) 
C     1						 + Fit%ISOTOPE(4)%CF(4,I)*Fit%P(16) 
C     1						 + Fit%P(1) + Fit%P(4)*Fit%P(2)			! Hg 199
C				CALL WVN_TO_PT(WAVNUM,FIT%ISOTOPE(4)%PTN(I))
C			ENDDO
C			DO I = 1,Fit%ISOTOPE(5)%Num_Components
C				WAVNUM =   Fit%ISOTOPE(5)%CF(1,I)*FIt%P(17) 
C     1						 + Fit%ISOTOPE(5)%CF(2,I)*FIt%P(18)
C     1						 + Fit%ISOTOPE(5)%CF(3,I)*Fit%P(19) 
C     1						 + Fit%ISOTOPE(5)%CF(4,I)*Fit%P(20) 
C     1						 + Fit%P(1)	+ Fit%P(5)*Fit%P(2)			! Hg 201
C				CALL WVN_TO_PT(WAVNUM,FIT%ISOTOPE(5).PTN(I))
C			ENDDO
C			WAVNUM = Fit%P(1) + Fit%P(6)*Fit%P(2)
C			CALL WVN_TO_PT(WAVNUM, FIT%ISOTOPE(6)%PTN(1))			! Hg 204
C	
	END SELECT
C	
C   Plot fit of data
C
	DO I=1,768
		TOTAL_FIT(I)=0.0
	ENDDO
	SELECT CASE (FIT%PEAK%TYPE)
C		CASE (kCentroid, kWtAvg, kDerv)
C			NUM_STRUCT = 0
		CASE (kHFS3Fit)
			NUM_STRUCT = FIT%NUM_STRUCTURES
			IP = -8
C		CASE (kHgFit)
C			NUM_STRUCT = FIT%NUM_ISOTOPES
	END SELECT
	DO KN = 1,NUM_STRUCT
		SELECT CASE (FIT%PEAK%TYPE)
			CASE (kHFS3Fit)
				NUM_COMP = FIT%STRUCT(KN)%NUM_COMPONENTS
				IP = IP + 8
C			CASE (kHgFit)
C				NUM_COMP = FIT%ISOTOPE(KN)%NUM_COMPONENTS
			CASE DEFAULT
				NUM_COMP = 0
		END SELECT
		DO K=1,NUM_COMP
			SELECT CASE (FIT%PEAK%TYPE)
				CASE (kHFS3Fit)
c don't reverse spectrum (gn)
C					IF((-1)**IDENT%SPECTRUM_ORDER .EQ. -1) THEN
						FAR = (FIT%BEG_ABSPT - FIT%STRUCT(KN)%PTN(K))
     1										*DFLOAT(IDENT%NUM_INTRPT_PTS)
C					ELSE
C						FAR = (FIT%END_ABSPT - FIT%STRUCT(KN)%PTN(K))
C     1										*DFLOAT(IDENT%NUM_INTRPT_PTS)
C					ENDIF
C				CASE (kHgFit)
c don't reverse spectrum (gn)
C					IF((-1)**IDENT%SPECTRUM_ORDER .EQ. -1) THEN
C						FAR = (FIT%BEG_ABSPT - FIT%ISOTOPE(KN)%PTN(K))
C     1										*DFLOAT(IDENT%NUM_INTRPT_PTS)
C					ELSE
C						FAR = (FIT%END_ABSPT - FIT%ISOTOPE(KN)%PTN(K))
C     1										*DFLOAT(IDENT%NUM_INTRPT_PTS)
C					ENDIF
			END SELECT
			SG = 1.0D0 
			IF(FAR .LE. 0.0D0) SG = -1.0D0 
			IFAR = IDINT(FAR + 0.5D0*SG) 
			INDX2 = NPTS_SINC_2 + IFAR - 1 
			INDX1 = INDX2 - 1
			INDX3 = INDX2 + 1
			DO J = 1,FIT%NPTS_INTRPT
				INDX1 = INDX1+1
				INDX2 = INDX2+1 
				INDX3 = INDX3+1
				IF(INDX1 .GT. 0) THEN 
					IF(INDX3 .LE. NPTS_SINC) THEN
						SELECT CASE (FIT%PEAK%TYPE)
							CASE (kHFS3Fit)
								A1 =   0.5*CONV_VOIGT(INDX1,KN) 
     1								 -     CONV_VOIGT(INDX2,KN) 
     1								 + 0.5*CONV_VOIGT(INDX3,KN)
								A2 =  -1.5*CONV_VOIGT(INDX1,KN) 
     1								 + 2.0*CONV_VOIGT(INDX2,KN) 
     1								 - 0.5*CONV_VOIGT(INDX3,KN)
C							CASE (kHgFit)
C								A1 =   0.5*CONV_VOIGT(INDX1,kOne) 
C     1								 -     CONV_VOIGT(INDX2,kOne) 
C     1								 + 0.5*CONV_VOIGT(INDX3,kOne)
C								A2 =  -1.5*CONV_VOIGT(INDX1,kOne) 
C     1								 + 2.0*CONV_VOIGT(INDX2,kOne) 
C     1								 - 0.5*CONV_VOIGT(INDX3,kOne)
						END SELECT
						XFTN = 1.0 + (FAR-DFLOAT(IFAR))
						SELECT CASE (FIT%PEAK%TYPE)
							CASE (kHFS3Fit)
								TOTAL_FIT(J) = TOTAL_FIT(J)
     1							+ (A1*XFTN*XFTN + A2*XFTN + CONV_VOIGT(INDX1,KN))
     1							* FIT%P(IP+6) * FIT%STRUCT(KN)%REL_INTENSITY(K)
C							CASE (kHgFit)
C								IF(KN .EQ. 1) THEN
C									TOTAL_FIT(J) = TOTAL_FIT(J)
C     1									+ (A1*XFTN*XFTN + A2*XFTN 
C     1									+ CONV_VOIGT(INDX1,kOne)) * FIT%P(7)
C								ELSE
C									TOTAL_FIT(J) = TOTAL_FIT(J)
C     1									+ (A1*XFTN*XFTN + A2*XFTN 
C     1									+ CONV_VOIGT(INDX1,kOne))
C     1									* FIT%P(7) * FIT%P(KN+6)
C     1									* FIT%ISOTOPE(KN)%REL_INTENSITY(K)
C     1									/ FIT%ISOTOPE(KN)%SUM_INTENSITIES
C								ENDIF
						END SELECT
					ENDIF
				ENDIF
			ENDDO 
		ENDDO
	ENDDO

	RETURN
	END
	
C***********************************************************************
C*	ROUTINE: Calculate_Comp
C*
C*	DESCRIPTION:
C*		Routine to display all the components of a fit.
C* 
C*	CALLS:
C*		XPLOT_MOVE - Move the draw point to a new point
C*		XPLOT_DRAW - Draw a line to a point
C*
C*	CALLED BY:
C*		Draw_Spectrum
C*
C*	REVISION HISTORY
C*      Modified by Gill Nave for use with Xgremlin
C*      November, 1999.
C*
C***********************************************************************
	SUBROUTINE CALCULATE_COMP(wstart,delw,isave,scratch)
	IMPLICIT NONE
	
	INCLUDE		'global_constants.h'
	INCLUDE		'hfsline.h'
	INCLUDE		'fit_draw.h'
	INCLUDE		'specfile.h'
	INCLUDE		'phits_data.h'
C	INCLUDE		'plot_defs.h'
	include		'color.h'         ! From Xgremlin - plotting colours

	INTEGER*4	STATUS
	INTEGER*2	MODEPL,IFAR, KSEL,I,J,K,INDX2
	INTEGER*2	NUM_STRUCT, NUM_COMP
	REAL*4		FAR,SG,FX
	real*8          wstart,delw
	logical         isave
	integer         scratch
        real*4          X(768+32*NLEN2), Y1(768+32*NLEN2)  ! Now these are arrays to use with pltaux in Xgremlin

	FX=FIT%NPTS_INTRPT-1
	NUM_STRUCT = FIT%NUM_STRUCTURES
	DO KSEL = 1,NUM_STRUCT
		NUM_COMP = FIT%STRUCT(KSEL)%NUM_COMPONENTS
		DO K=1,NUM_COMP
		        ! First clear X and Y1
		        do j=1,768+32*NLEN2
			   x(j) = 0.
			   y1(j) = 0.
			enddo
			MODEPL=3
c don't reverse spectrum (gn)
C				IF((-1)**IDENT%SPECTRUM_ORDER .EQ. -1) THEN
					FAR = (FIT%BEG_ABSPT - FIT%Struct(Ksel)%PTN(K))
     1									*DFLOAT(IDENT%NUM_INTRPT_PTS)
C				ELSE
C					FAR = (FIT%END_ABSPT - FIT%Struct(Ksel)%PTN(K))
C     1									*DFLOAT(IDENT%NUM_INTRPT_PTS)
C				ENDIF
			SG=1.0 
			IF(FAR .LE. 0.0) SG=-1.0 
			IFAR = INT(FAR+0.5*SG) 
			INDX2 = NPTS_SINC_2+IFAR
			J = 1
			DO WHILE (J .LE. FIT%NPTS_INTRPT .AND. INDX2 .LE. NPTS_SINC)
			  IF(INDX2 .GT. 0) THEN
			     Y1(j) = CONV_VOIGT(INDX2,KSEL)*FIT%P(8*KSEL-2)
     1						*FIT%STRUCT(KSEL)%REL_INTENSITY(K)
			     X(j) = (FLOAT(J-1)/float(ident%num_intrpt_pts)+
     1                               fit%beg_abspt-1)*delw + wstart
!					IF(2*(FIT%PEAK%ORDER/2) .EQ. FIT%PEAK%ORDER) X(j) = -X(j) 
!				IF(Y1(j) .LT. -1.0) Y1(j) = -1.0 
!					IF(Y1(j) .GT. HEIGHT) Y1(j) = HEIGHT
				ENDIF
				INDX2=INDX2+1 
				J = J + 1
			ENDDO 
			call pltaux(fit%npts_intrpt, X, Y1, col_blue)
			if (isave) then
			   write(scratch,'(f10.4,1x, e11.3)') (X(i),y1(i),
     1                             i=1,fit%npts_intrpt)  
			endif
		ENDDO
	ENDDO
	RETURN
	END

C***********************************************************************
C*	ROUTINE: Display_Voigt
C*
C*	DESCRIPTION:
C*		Routine to calculate the Voigt function at intervals corresponding to
C*	the current interpolation interval.
C* 
C*	CALLS:
C*		Sconvolute - single precision convolution function
C*
C*	CALLED BY:
C*		Draw_Spectrum
C*
C*	REVISION HISTORY
C*
C***********************************************************************
	SUBROUTINE DISPLAY_VOIGT
	IMPLICIT NONE

	INCLUDE 'global_constants.h'
	INCLUDE 'specfile.h'
	INCLUDE 'hfsline.h'
	INCLUDE 'fit_draw.h'

	INTEGER*2	NCONV,NOFF,IP,NTMP,KN,I,K,NUM_STRUCT
	REAL*4		FETA,HWDTH,GMF,EGMF,OMF,SIGMA,ARG,GAUS,FLRZ
	REAL*4		TMP1,TMP2,ERSIG

	REAL*4		FLN2
	PARAMETER (FLN2=0.6931472)
C 
C   Calculate voigt profile 
C
	NPTS_SINC=20*IDENT%NUM_INTRPT_PTS
	LIM1  = IDENT%NUM_INTRPT_PTS*NLEN2
	LIM2  = 2*LIM1+1 
	SELECT CASE(FIT%PEAK%TYPE)
		CASE (kHFS3Fit)
			HWDTH = FIT%P(8)
			IP = 8
			NUM_STRUCT = FIT%NUM_STRUCTURES
C		CASE (kHgFit)
C			HWDTH = FIT%P(22)
C			IP = 22
C			NUM_STRUCT = 1
	END SELECT
	NTMP = 20*HWDTH*IDENT%NUM_INTRPT_PTS
	IF(NTMP.GT.NPTS_SINC) NPTS_SINC=NTMP
	IF(NPTS_SINC.GT.768) NPTS_SINC=768
	NPTS_SINC_2 = NPTS_SINC/2 + 1 
	NCONV       = NPTS_SINC   + LIM1*2 
	NOFF        = NPTS_SINC_2 + LIM1 
	DO KN = 1,NUM_STRUCT
		FETA = FIT%P(IP*KN-1)
		HWDTH = FIT%P(IP*KN)
		DO I=1,NCONV 
			SIGMA = FLOAT(I-NOFF)/(FLOAT(IDENT%NUM_INTRPT_PTS)*HWDTH) 
			ARG   = SIGMA*SIGMA 
			FLRZ  = 0.0 
			IF(ARG.LT.1E12) FLRZ = 1.0/(1.0+ARG) 
			GAUS  = 0.0 
			IF(ARG.LT.30.)  GAUS = EXP(-FLN2*ARG) 
			TMP1 = 0.8029 - 0.4207*ARG 
			TMP2 = 1.0 + (0.2030 + 0.07335*ARG)*ARG 
			ERSIG= TMP1/TMP2 
			GMF  = GAUS-FLRZ 
			EGMF = ERSIG*GMF 
			OMF  = 1.0 - FETA 
			VOIGT(I,KN) = OMF*GAUS+(FLRZ+OMF*EGMF)*FETA
		ENDDO
	ENDDO
	CALL SCONVOLUTE(SINC(1),VOIGT(NPTS_SINC_2,1),SINC_PEAK,INT2(1),
     1                  LIM2,INT2(1))
C 
C   Convolute with instrumental function 
C
	DO KN = 1,NUM_STRUCT
		CALL SCONVOLUTE(SINC(1),VOIGT(1,KN),CONV_VOIGT(1,KN),NPTS_SINC,
     1                          LIM2,INT2(1)) 
		DO I = 1,NPTS_SINC 
			CONV_VOIGT(I,KN) = CONV_VOIGT(I,KN)/SINC_PEAK
		ENDDO
	END DO

	RETURN
	END


C***********************************************************************
C*	ROUTINE: Display_Inst_Ftn
C*
C*	DESCRIPTION:
C*		Routine to calculate the instrumentl function at the display resolution
C* 
C*	CALLS:
C*		Wvn_To_Ptn	- converts wavenumber [cm-1) to absolute point number
C*
C*	CALLED BY:
C*		ClickLLLamDialog - Load fit from LLLam dialog
C*		ClickFitDialog	 - VIEW fit function
C*		ClickFit3Dialog	 - VIEW fit function
C*		??** Show		 - Show a previously fitted line
C*
C*	REVISION HISTORY
C*
C***********************************************************************
	SUBROUTINE Display_Inst_Ftn
	IMPLICIT NONE

	INCLUDE 'specfile.h'
	INCLUDE 'fit_draw.h'

	INTEGER*4	I
	REAL*4		ACON2, ACONS, BCONS, AB, TOT, SIGMA, ARG, CS2, PI2, PI
	PARAMETER(PI=3.14159265359, PI2=6.28318)
C 
C   CALCULATE FT INSTRUMENTAL FUNCTION 
C 
	LIM1  = IDENT%NUM_INTRPT_PTS*NLEN2
	LIM2  = 2*LIM1+1 
	LIM3  = LIM1+1 
	ACON2 = (IDENT%NPTS_MEASURED-IDENT%NPTS_APODIZED)
     1				/IDENT%TRANSFORM_NPTS
	ACONS = ACON2/2.0
	BCONS = IDENT%NPTS_APODIZED/IDENT%TRANSFORM_NPTS/2.0
	AB    = ACONS+BCONS/2.0
	TOT   = 0.0
	DO I=1,LIM2 
		SIGMA = FLOAT(I-LIM3)/FLOAT(IDENT%NUM_INTRPT_PTS)
		ARG   = BCONS*SIGMA+.25
		CS2   = COS(PI2*(AB*SIGMA+.125))
		IF(ABS(ARG).GT.1D-30) THEN
			SINC(I) = BCONS*SIN(PI*ARG)*CS2/(PI*ARG)
		ELSE
			SINC(I) = BCONS*CS2
		ENDIF
		CS2 = COS(PI2*(AB*SIGMA - 0.125))
		ARG = BCONS*SIGMA - 0.25
		IF(ABS(ARG).GT.1D-30) THEN
			SINC(I) = SINC(I)+BCONS*CS2*SIN(PI*ARG)/(PI*ARG)
		ELSE
			SINC(I) = SINC(I)+BCONS*CS2
		ENDIF
		SIGMA = ACON2*SIGMA
		IF(ABS(SIGMA).GT.1D-30) THEN
			SINC(I) = SINC(I)+ACON2*SIN(PI*SIGMA)/(PI*SIGMA)
		ELSE
			SINC(I) = SINC(I)+ACON2
		ENDIF
		TOT = TOT + SINC(I)
	ENDDO
	DO I=1,LIM2 
		SINC(I) = SINC(I)/TOT 
	ENDDO
	
	RETURN
	END
