C***********************************************************************
C*	MODULE: global_constants
C*
C*	© Bruce Pulliam, 1993 - All Rights Reserved
C*
C*	DESCRIPTION:
C*		Common data area for miscellaneous constants 
C* 
C*	SUBROUTINE/FUNCTIONS INCLUDED
C*		NONE
C*
C*	REVISION HISTORY
C*		25-MAY-1993		Baseline Version 0.1.0
C*              7-JUN-1993		added constants related to the multiple fit types
C*					Baseline Version 0.2.0
C*		12-JUN-1993		added resources for kHFS3Fit dialog box
C*						and menu items
C*		10-OCT-1993		added peak types to attributes menu
C*
C***********************************************************************
C
C  Window Resources
C
	INTEGER*2	Res_W_Data,
     1			Res_W_Init
	PARAMETER (Res_W_Data = 128, Res_W_Init = 129)
C
C  Alert Resources
C
	INTEGER*2	Res_Alrt_Close,
     1			Res_Alrt_OK
	PARAMETER  (Res_Alrt_Close = 701, Res_Alrt_OK = 702)
C
C  Dialog Resources
C
	INTEGER*2	Res_D_Spectrum, 
     1			Res_D_Wvn, 
     1			Res_D_Fit,
     1			Res_D_OpenLev,
     1			Res_D_Pref,
     1			Res_D_OutMan,
     1			Res_D_NucSpin,
     1			Res_D_LevelMan,
     1			Res_D_AddLev,
     1			Res_D_EditLev,
     1			Res_D_OutFile,
     1			Res_D_LLLamParm,
     1			Res_D_LLLamLst,
     1			Res_D_FitControl,
     1			Res_D_Fit3Dialog,
     1			Res_D_PrintSpec,
     1			Res_D_PrintL,
     1			Res_D_PrintFit,
     1			Res_D_PrintOut
	
	PARAMETER  (Res_D_Spectrum   = 128, Res_D_Wvn        = 129,
     1			Res_D_Fit        = 130, Res_D_OpenLev    = 131,
     1			Res_D_Pref       = 132, Res_D_OutMan     = 133, 
     1			Res_D_NucSpin    = 134, Res_D_LevelMan   = 135, 
     1			Res_D_AddLev     = 136, Res_D_EditLev    = 137, 
     1			Res_D_OutFile    = 138, Res_D_LLLamParm  = 139,
     1			Res_D_LLLamLst   = 140, Res_D_FitControl = 141,
     1			Res_D_Fit3Dialog = 142, Res_D_PrintSpec  = 143,
     1			Res_D_PrintL     = 144, Res_D_PrintFit   = 145,
     1			Res_D_PrintOut   = 146)
C
C  Generic Dialogs
C
	INTEGER*2	Res_D_YesNoCancel,
     1			Res_D_OKCancel,
     1			Res_D_WriteStr,
     1			Res_D_GetString
	PARAMETER ( Res_D_YesNoCancel = 501, Res_D_OKCancel  = 502,
     1			Res_D_WriteStr    = 503, Res_D_GetString = 504)
C
C  Control Resources
C
	INTEGER*2	Res_C_Where,
     1			Res_C_GoTo,
     1			Res_C_Interpolate,
     1			Res_C_Auto,
     1			Res_C_Manual,
     1			Res_C_Enlarge,
     1			Res_C_Components,
     1			Res_C_Revert,
     1			Res_C_Update,
     1			Res_C_Start
	PARAMETER(Res_C_Where =128,Res_C_GoTo=129,Res_C_Interpolate=130,
     1		   Res_C_Auto=131,Res_C_Manual=132,Res_C_Enlarge =133,
     1 		   Res_C_Components=134,Res_C_Revert = 135,
     1             Res_C_Update=136,Res_C_Start = 137 )
C	
C  String Resources
C
	INTEGER*2
     1			Res_str_FILE
	PARAMETER ( Res_str_FILE = 128)

	INTEGER*2	k_str_FILE_Spec,
     1			k_str_FILE_Out,
     1			k_str_FILE_Level,
     1			k_str_FILE_LLLam
	PARAMETER(k_str_FILE_Spec=1,k_str_FILE_Out=2,k_str_FILE_Level=3,
     1			k_str_FILE_LLLam = 4)
	
C
C  Menu Resources
C
	INTEGER*2	Res_MB_Data,
     1			Res_MB_Init
	PARAMETER (	Res_MB_Data = 128, Res_MB_Init = 129)

	INTEGER*2	Res_M_Apple,
     1			Res_M_File,
     1			Res_M_Edit,
     1			Res_M_Commands,
     1			Res_M_LogLin,
     1			Res_M_Jump,
     1			Res_M_InitFile,
     1			Res_M_Managers,
     1			Res_M_LevType,
     1			Res_M_LLLamType,
     1			Res_M_Attributes,
     1			Res_M_FitTypes,
     1			Res_M_PeakTypes
	PARAMETER (Res_M_Apple=128,Res_M_File=129, Res_M_Edit=130,
     1	           Res_M_Commands=131,Res_M_LogLin=134,Res_M_Jump=133, 
     1		   Res_M_InitFile=136,Res_M_Managers=132,
     1             Res_M_LevType=135,
     1		   Res_M_LLLamType = 137, Res_M_Attributes = 138, 
     1		   Res_M_FitTypes  = 139, Res_M_PeakTypes = 140)
	
	INTEGER*2	k_mi_FILE_Type,
     1			k_mi_FILE_New,
     1			k_mi_FILE_Open,
     1			k_mi_FILE_Save,
     1			k_mi_FILE_SaveAs,
     1			k_mi_FILE_Close,
     1			k_mi_FILE_PageSetup,
     1			k_mi_FILE_Print,
     1			k_mi_FILE_Preferences,
     1			k_mi_FILE_Quit
	PARAMETER (k_mi_FILE_Type=1,k_mi_FILE_New=3,k_mi_FILE_Open =4, 
     1		      k_mi_FILE_Save=5,k_mi_FILE_SaveAs=6,
     1                k_mi_FILE_Close = 7,
     1		      k_mi_FILE_PageSetup = 9, k_mi_FILE_Print = 10,
     1		      k_mi_FILE_Preferences = 12, k_mi_FILE_Quit = 14)
	
	INTEGER*2	k_mi_EDIT_Undo,
     1			k_mi_EDIT_Cut,
     1			k_mi_EDIT_Copy,
     1			k_mi_EDIT_Paste
	PARAMETER ( k_mi_EDIT_Undo = 1, k_mi_EDIT_Cut   = 2, 
     1			k_mi_EDIT_Copy = 3, k_mi_EDIT_Paste = 4)
	
	INTEGER*2	k_mi_CMDS_GoTo,
     1			k_mi_CMDS_Components,
     1			k_mi_CMDS_Threshold,
     1			k_mi_CMDS_Base,
     1			k_mi_CMDS_Noise
	PARAMETER ( k_mi_CMDS_GoTo  = 1, k_mi_CMDS_Components = 2, 
     1			k_mi_CMDS_Threshold = 3, k_mi_CMDS_Base = 4, 
     1			k_mi_CMDS_Noise = 5)
	
	INTEGER*2 	k_mi_MGRS_Spectrum,
     1			k_mi_MGRS_Output,
     1			k_mi_MGRS_Level,
     1			k_mi_MGRS_LLLAM,
     1			k_mi_MGRS_Fit
	PARAMETER ( k_mi_MGRS_Spectrum = 1, k_mi_MGRS_Output = 2,
     1			k_mi_MGRS_Level    = 3, k_mi_MGRS_LLLAM  = 4,
     1			k_mi_MGRS_Fit      = 5)
	
	INTEGER*2	k_mi_LEVT_Current,
     1			k_mi_LEVT_Alternate
	PARAMETER ( k_mi_LEVT_Current = 1, k_mi_LEVT_Alternate = 2)
	
	INTEGER*2	k_mi_LOGL_Linear,
     1			k_mi_LOGL_LogLin
	PARAMETER ( k_mi_LOGL_Linear = 1, k_mi_LOGL_LogLin = 2)
	
	INTEGER*2	k_mi_JUMP_Block,
     1			k_mi_JUMP_Cursor,
     1			k_mi_JUMP_Fitted,
     1			k_mi_JUMP_Large
	PARAMETER (k_mi_JUMP_Block = 1, k_mi_JUMP_Cursor = 2, 
     1               k_mi_JUMP_Fitted = 3,
     1		     k_mi_JUMP_Large = 4)
	
	INTEGER*2	k_mi_ATTR_FitType,
     1			k_mi_ATTR_PeakType
	PARAMETER ( k_mi_ATTR_FitType = 1, k_mi_ATTR_PeakType = 2)
	
	INTEGER*2	k_mi_FITT_HFS,
     1			k_mi_FITT_HG
	PARAMETER ( k_mi_FITT_HFS = 1, k_mi_FITT_HG = 2)
	
	INTEGER*2	k_mi_PEAK_Where,
     1			k_mi_PEAK_WtAvg,
     1			k_mi_PEAK_Centroid,
     1			k_mi_PEAK_Derivative
	PARAMETER ( k_mi_PEAK_Where = 1,    k_mi_PEAK_WtAvg = 2,
     1			k_mi_PEAK_Centroid = 3, 
     1                  k_mi_PEAK_Derivative = 4)
C
C  Miscellaneous Constants
C
	INTEGER*2	kWhere,kWtAvg,kCentroid,kDerv,kHFS3Fit,kHgFit
	PARAMETER ( kWhere = 1, kWtAvg = 2, kCentroid = 3, kDerv = 4, 
     1                   kHFS3Fit = 5, kHgFIt = 6)
	
	INTEGER*2	kPeriod, kReturn, kEnter, kEscape, kTab
	PARAMETER	(kPeriod = 46, kReturn = 13, kEnter = 3, 
     1			 kEscape = 27, kTab = 9)
	
	INTEGER*2	kCut, kCopy, kPaste
	PARAMETER	(kCut = '78'X, kCopy = '63'X, kPaste = '76'X)
	
	INTEGER*2	kOn, kOff
	PARAMETER	(kOn = 1, kOff = 0)
	
	INTEGER*2	kZero, kOne, kTwo, kThree, kFour, kFive, kSix
	PARAMETER	(kZero=0, kOne=1, kTwo=2, kThree=3, kFour=4,
     1			 kFive = 5, kSix = 6)

	INTEGER*2	kLeft, kRight
	PARAMETER	(kLeft = -1, kRight = 1)

	INTEGER*2	kOK, kYes, kCancel, kNo
	PARAMETER	(kOk = 1, kYes = 1, kCancel = 2, kNo = 3)
	
	LOGICAL*2	isTrue, isFalse
	PARAMETER	(isTrue = .TRUE., isFalse = .FALSE.)
	
	LOGICAL*1	pTrue, pFalse
	PARAMETER	(pTrue = .TRUE., pFalse = .FALSE.)
	
	INTEGER*2	kOdd, kEven
	PARAMETER	(kOdd = 1, kEven = -1)
 



