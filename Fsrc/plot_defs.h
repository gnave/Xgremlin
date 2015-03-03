C***********************************************************************
C*	MODULE: plot_defs
C*
C*	© Bruce Pulliam, 1993 - All Rights Reserved
C*
C*	DESCRIPTION:
C*		Common data area for definitions of constants used by routines
C*  interfacing to the XPlot package.
C* 
C*	SUBROUTINE/FUNCTIONS INCLUDED
C*		NONE
C*
C*	REVISION HISTORY
C*		25-MAY-1993		Baseline Version 0.1.0
C*
C***********************************************************************
	INTEGER*4 XPLOT$M_DEFAULT, XPLOT$M_NEW_ORIGIN, XPLOT$M_NEW_OFFSET     
	PARAMETER 							      
     1		(XPLOT$M_DEFAULT    = 0,                              
     1	     XPLOT$M_NEW_ORIGIN = 1,                                  
     1	     XPLOT$M_NEW_OFFSET = 2)     
	INTEGER*4 XPLOT$M_ALIGN_BEGINNING, XPLOT$M_ALIGN_CENTER,              
     1	  XPLOT$M_ALIGN_END, XPLOT$M_ALIGN_BOTTOM,                    
     1	  XPLOT$M_ALIGN_MIDDLE, XPLOT$M_ALIGN_TOP                     
	PARAMETER 							      
     1		(XPLOT$M_ALIGN_BEGINNING = 0,                         
     1	     XPLOT$M_ALIGN_CENTER    = 1,                             
     1	     XPLOT$M_ALIGN_END       = 2,                             
     1        XPLOT$M_ALIGN_BOTTOM    = 4,                                 
									      
     1	     XPLOT$M_ALIGN_MIDDLE    = 8,                             
     1	     XPLOT$M_ALIGN_TOP       = 16)
	INTEGER*2 XPLOT$M_LINEAR, XPLOT$M_LOGLINEAR                           
	PARAMETER 							      
     1		(XPLOT$M_LINEAR 	= 1,                               
     1	     XPLOT$M_LOGLINEAR	= 2)         
	INTEGER*2 XPLOT$M_DRAW_DATA, XPLOT$M_DRAW_FIT,                        
     1	  XPLOT$M_DRAW_COMPONENTS, XPLOT$M_REDRAW_SCREEN,             
     1	  XPLOT$M_CALC_VOIGT, XPLOT$M_CALC_FIT_SHAPE                  
	PARAMETER 							      
     1		(XPLOT$M_DRAW_DATA       = 1,                         
     1	     XPLOT$M_DRAW_FIT        = 2,                             
     1	     XPLOT$M_DRAW_COMPONENTS = 4,                             
     1	     XPLOT$M_REDRAW_SCREEN   = 8,                             
     1	     XPLOT$M_CALC_VOIGT      = 16,                            
     1	     XPLOT$M_CALC_FIT_SHAPE  = 32)      
	INTEGER*2  XPLOT$M_DATA_DRAWN, XPLOT$M_FIT_DRAWN,                     
     1	   XPLOT$M_COMPONENTS_DRAWN                                   
	PARAMETER 							      
     1		(XPLOT$M_DATA_DRAWN       = 1,                        
     1	     XPLOT$M_FIT_DRAWN        = 2,                            
     1	     XPLOT$M_COMPONENTS_DRAWN = 4)  
