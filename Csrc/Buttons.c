/*
 * Copyright (C) 1994, 1995
 * Ulf Griesmann, Gaithersburg MD 20878, U.S.A.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * (see file COPYING) along with this program; if not, write to the
 * Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 * Buttons in the top button bar
 *
 * $Id: Buttons.c,v 1.8 1996/06/15 02:32:58 ulf Exp $
 */

#include <X11/Intrinsic.h>     /* X toolkit intrinsics definitions */
#include <X11/StringDefs.h>    /* Standard Name-String definitions */

#include <X11/Xaw/Toggle.h>

#include "LiteClue.h"
#include "Buttons.h"
#include "Edit.h"
#include "PlotMode.h"
#include "defaults.h"
#include "FortranWrappers.h"


/*
 * global flag
 */
extern Widget gremlinTop;
extern Widget gremlinPlot;
extern XtAppContext gremlinContext;
extern Widget liteClue;
extern Widget phaseButton;                  /* used in phase correction */
extern Widget optionButton;                 /* button to bring up option menu*/
extern Widget toggleButton;                 /* toggle edit / plot mode */
extern Widget intrButton;                   /* the interrupt Button */
extern Widget leftButton;                   /* buttons to scroll spectrum */
extern Widget rightButton;
extern Widget yfullButton;
extern Widget xshrinkButton;
extern Widget xexpandButton;
extern Widget yshrinkButton;
extern Widget yexpandButton;
extern Widget xcentreButton;
extern Widget ycentreButton;
extern Widget xsectionButton;
extern Widget ysectionButton;
extern Widget redrawButton;
extern Widget undoButton;

static int glob_irq = FALSE;          /* = 1 if interrupt button was pressed */
static int buttons_blocked = FALSE;   /* react to configurable buttons or not */
static int plot_mode = OFF;           /* edit mode at startup */
static Widget loc_itr_wid;    
static Widget loc_tog_wid;

/*-----------------------------------------------------------------*/

void 
InitializeButtons( Widget toggle, Widget interrupt )
{
   /*
    * make local copies of widgets, makes bookkeeping easier.
    */
   loc_tog_wid = toggle;
   loc_itr_wid = interrupt;
}

/*-----------------------------------------------------------------*/

void 
XTogglePlotEdit(Widget w, XtPointer client_data, XtPointer call_data) 
{
   if ( plot_mode == ON )  /* turn off plotting mode */
     {
	plot_mode = OFF;
	FocusToEdit();
     }
   else                    /* turn on plotting mode */
     {
	plot_mode = ON;
	FocusToPlot();
     }
}

/*-----------------------------------------------------------------*/

void 
TogglePlotEdit() 
{
   Arg args[4];
   int i;
   
   i = 0;
   if ( plot_mode == ON ) /* turn off plotting mode */
     { 
	plot_mode = OFF;
	XtSetArg(args[i], XtNstate, False); i++;
	FocusToEdit();
     }
   else                   /* turn on plotting mode */
     {                  
	plot_mode = ON;
	XtSetArg(args[i], XtNstate, True); i++;
	FocusToPlot();
     }
   XtSetValues(loc_tog_wid, args, i);
}

/*-----------------------------------------------------------------*/

void 
InterruptRequested(Widget w, XtPointer client_data, XtPointer call_data)
{   
   glob_irq = TRUE;
}

/*-----------------------------------------------------------------*/

void 
scroll_left( Widget w, XtPointer client_data, XtPointer call_data)
{
   int ch, shift, ctrl, meta;

   ch = ',';   /* fake a key stroke */
   shift = 0;
   ctrl  = 0;
   meta  = 0;
   keycmd_( &ch, &shift, &ctrl, &meta ); 
   if ( TextOutput() == TRUE )
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

void 
scroll_right( Widget w, XtPointer client_data, XtPointer call_data)
{
   int ch, shift, ctrl, meta;

   ch = '.';   /* fake a key stroke */
   shift = 0;
   ctrl  = 0;
   meta  = 0;
   keycmd_( &ch, &shift, &ctrl, &meta ); 
   if ( TextOutput() == TRUE )
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

void 
yfull_world( Widget w, XtPointer client_data, XtPointer call_data)
{
   /* 
    * full zoom out of y scale
    */
   int ch, shift, ctrl, meta;

   ch = '^';   /* fake a key stroke */
   shift = 1;
   ctrl  = 0;
   meta  = 0;
   keycmd_( &ch, &shift, &ctrl, &meta ); 
   if ( TextOutput() == TRUE )
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

void 
xshrink_world( Widget w, XtPointer client_data, XtPointer call_data)
{
   /* 
    * zoom in 
    */
   int ch, shift, ctrl, meta;

   ch = 'x';   /* fake a key stroke */
   shift = 0;
   ctrl  = 0;
   meta  = 0;
   keycmd_( &ch, &shift, &ctrl, &meta ); 
   if ( TextOutput() == TRUE )
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

void 
xexpand_world( Widget w, XtPointer client_data, XtPointer call_data)
{
   /* 
    * zoom out 
    */
   int ch, shift, ctrl, meta;

   ch = 'x';   /* fake a key stroke */
   shift = 0;
   ctrl  = 0;
   meta  = 1;
   keycmd_( &ch, &shift, &ctrl, &meta ); 
   if ( TextOutput() == TRUE )
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

void 
yshrink_world( Widget w, XtPointer client_data, XtPointer call_data)
{
   /* 
    * zoom in 
    */
   int ch, shift, ctrl, meta;

   ch = 'y';   /* fake a key stroke */
   shift = 0;
   ctrl  = 0;
   meta  = 0;
   keycmd_( &ch, &shift, &ctrl, &meta ); 
   if ( TextOutput() == TRUE )
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

void 
yexpand_world( Widget w, XtPointer client_data, XtPointer call_data)
{
   /* 
    * zoom out 
    */
   int ch, shift, ctrl, meta;

   ch = 'y';   /* fake a key stroke */
   shift = 0;
   ctrl  = 0;
   meta  = 1;
   keycmd_( &ch, &shift, &ctrl, &meta ); 
   if ( TextOutput() == TRUE )
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

void 
xcentre_world( Widget w, XtPointer client_data, XtPointer call_data)
{
   /* 
    * center horizontally 
    */
   int ch, shift, ctrl, meta;

   ch = 201;   /* fake a key stroke */
   shift = 0;
   ctrl  = 0;
   meta  = 0;
   keycmd_( &ch, &shift, &ctrl, &meta ); 
   if ( TextOutput() == TRUE )
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

void 
ycentre_world( Widget w, XtPointer client_data, XtPointer call_data)
{
   /* 
    * center vertically 
    */
   int ch, shift, ctrl, meta;

   ch = 202;   /* fake a key stroke */
   shift = 0;
   ctrl  = 0;
   meta  = 0;
   keycmd_( &ch, &shift, &ctrl, &meta ); 
   if ( TextOutput() == TRUE )
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

void 
xsection_world( Widget w, XtPointer client_data, XtPointer call_data)
{
   /* 
    * plot a section 
    */
   int ch, shift, ctrl, meta;

   ch = 203;   /* fake a key stroke */
   shift = 0;
   ctrl  = 0;
   meta  = 0;
   keycmd_( &ch, &shift, &ctrl, &meta ); 
   if ( TextOutput() == TRUE )
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

void 
ysection_world( Widget w, XtPointer client_data, XtPointer call_data)
{
   /* 
    * plot a section 
    */
   int ch, shift, ctrl, meta;

   ch = 204;   /* fake a key stroke */
   shift = 0;
   ctrl  = 0;
   meta  = 0;
   keycmd_( &ch, &shift, &ctrl, &meta ); 
   if ( TextOutput() == TRUE )
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

void 
redraw_world( Widget w, XtPointer client_data, XtPointer call_data)
{
   /* 
    * redraw the whole enchillada 
    */
   int ch, shift, ctrl, meta;

   ch = 'P';   /* fake a key stroke */
   shift = 1;
   ctrl  = 0;
   meta  = 0;
   keycmd_( &ch, &shift, &ctrl, &meta ); 
   if ( TextOutput() == TRUE )
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

void 
undo_world( Widget w, XtPointer client_data, XtPointer call_data)
{
   /* 
    * undo the last plot mode command
    */
   int ch, shift, ctrl, meta;

   ch = 'U';   /* fake a key stroke */
   shift = 1;
   ctrl  = 0;
   meta  = 0;
   keycmd_( &ch, &shift, &ctrl, &meta ); 
   if ( TextOutput() == TRUE )
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

int 
PlotMode() 
{
   return plot_mode;
}

/*-----------------------------------------------------------------*/

int
ButtonsBlocked()
{
   return buttons_blocked;
}

/*-----------------------------------------------------------------*/

void 
itron_()
{   
   buttons_blocked = TRUE;       /* block configurable buttons */
}

/*-----------------------------------------------------------------*/

void 
itroff_()
{
   buttons_blocked = FALSE;
}

/*-----------------------------------------------------------------*/

int 
itrflag_()
{
   int irq_status;

   irq_status = glob_irq;
   glob_irq = FALSE;      /* clear flag if set */
   return( irq_status );
}

/*-----------------------------------------------------------------*/

void
procpend_()
{
   XtInputMask ret_val;
   XEvent ev;

   for(;;)
     {
	ret_val = XtAppPending(gremlinContext) & XtIMXEvent;

	if ( ret_val == (XtInputMask)0 )         /* no X event pending, do nothing */
	  break;

	XtAppNextEvent(gremlinContext, &ev );    /* fetch pending X event */
	
	/*
	 * to maintain the integrity of Xgremlin's data we discard all key events.
	 * This prevents users from typing in new commands which may alter the 
	 * the contents of e.g. the r array while a calculation (e.g. a fft) 
	 * is under way.
	 */
	if ( ev.type != KeyPress && ev.type != KeyRelease )
	  XtDispatchEvent( &ev );	     
     } 
}

/*-----------------------------------------------------------------*/

void
butunb_()
{
   buttons_blocked = FALSE;
}

/*-----------------------------------------------------------------*/

void
bubbles_(int *arg)
{
   if ( *arg == 0 )
     {
        XcgLiteClueSetSensitive(liteClue, optionButton, False);
        XcgLiteClueSetSensitive(liteClue, toggleButton, False);
        XcgLiteClueSetSensitive(liteClue, phaseButton, False);
        XcgLiteClueSetSensitive(liteClue, intrButton, False);
        XcgLiteClueSetSensitive(liteClue, leftButton, False);
        XcgLiteClueSetSensitive(liteClue, rightButton, False);
        XcgLiteClueSetSensitive(liteClue, yfullButton, False);
        XcgLiteClueSetSensitive(liteClue, xshrinkButton, False);
        XcgLiteClueSetSensitive(liteClue, xexpandButton, False);
        XcgLiteClueSetSensitive(liteClue, yshrinkButton, False);
        XcgLiteClueSetSensitive(liteClue, yexpandButton, False);
        XcgLiteClueSetSensitive(liteClue, xcentreButton, False);
        XcgLiteClueSetSensitive(liteClue, ycentreButton, False);
        XcgLiteClueSetSensitive(liteClue, xsectionButton, False);
        XcgLiteClueSetSensitive(liteClue, ysectionButton, False);
        XcgLiteClueSetSensitive(liteClue, undoButton, False);
        XcgLiteClueSetSensitive(liteClue, redrawButton, False);
     }

   if ( *arg == 1 )
     {
        XcgLiteClueSetSensitive(liteClue, optionButton, True);
        XcgLiteClueSetSensitive(liteClue, toggleButton, True);
        XcgLiteClueSetSensitive(liteClue, phaseButton, True);
        XcgLiteClueSetSensitive(liteClue, intrButton, True);
        XcgLiteClueSetSensitive(liteClue, leftButton, True);
        XcgLiteClueSetSensitive(liteClue, rightButton, True);
        XcgLiteClueSetSensitive(liteClue, yfullButton, True);
        XcgLiteClueSetSensitive(liteClue, xshrinkButton, True);
        XcgLiteClueSetSensitive(liteClue, xexpandButton, True);
        XcgLiteClueSetSensitive(liteClue, yshrinkButton, True);
        XcgLiteClueSetSensitive(liteClue, yexpandButton, True);
        XcgLiteClueSetSensitive(liteClue, xcentreButton, True);
        XcgLiteClueSetSensitive(liteClue, ycentreButton, True);
        XcgLiteClueSetSensitive(liteClue, xsectionButton, True);
        XcgLiteClueSetSensitive(liteClue, ysectionButton, True);
        XcgLiteClueSetSensitive(liteClue, undoButton, True);
        XcgLiteClueSetSensitive(liteClue, redrawButton, True);
     }
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * -----------------
 * $Log: Buttons.c,v $
 * Revision 1.8  1996/06/15 02:32:58  ulf
 * added implementation of 'undo_world' callback function (S+u command).
 *
 * Revision 1.7  1996/03/19 14:56:41  ulf
 * do no longer turn on and off 'stop' button; it remains always sensitive now.
 *
 * Revision 1.6  1996/03/03  14:01:56  ulf
 * replace calls to 'gdispatch' by calls to 'keycmd'
 *
 * Revision 1.5  1996/02/06  15:16:41  ulf
 * add a function 'butunb' to unblock configurable buttons
 *
 * Revision 1.4  1996/01/31  15:45:35  ulf
 * added copyleft
 *
 * Revision 1.3  1995/08/19  19:21:35  ulf
 * Flush changes to 'stop' button status to display immediately
 *
 * Revision 1.2  1995/06/14  04:01:15  ulf
 * change variable name   flag ---> irq_status
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */
