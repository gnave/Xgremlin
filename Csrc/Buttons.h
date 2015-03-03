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
 * $Id: Buttons.h,v 1.4 1996/06/15 02:29:43 ulf Exp $
 */

#ifndef _Buttons_h
#define _Buttons_h

/*
 * initializes the module (stores copies of button and interrupt widget)
 */
void InitializeButtons( Widget toggle, Widget interrupt );


/*
 * toggles between editing mode and plotting mode ( callback function ! )
 */
void XTogglePlotEdit(Widget, XtPointer, XtPointer);


/*
 * toggles between editing mode and plotting mode ( called by <tab> key )
 */
void TogglePlotEdit();


/*
 * called when the interrupt button is pressed. Used to interrupt lengthy
 * calculations. Simply sets a flag
 */
void InterruptRequested(Widget, XtPointer, XtPointer);


/*-------------------------------------------------------------
 * Callback functions for plot mode buttons on top of window
 *-------------------------------------------------------------
 */

void 
scroll_left( Widget, XtPointer, XtPointer );

void 
scroll_right( Widget, XtPointer, XtPointer );

void 
yfull_world( Widget, XtPointer, XtPointer );

void 
xshrink_world( Widget, XtPointer, XtPointer );

void 
xexpand_world( Widget, XtPointer, XtPointer );

void 
yshrink_world( Widget, XtPointer, XtPointer );

void 
yexpand_world( Widget, XtPointer, XtPointer );

void 
xcentre_world( Widget, XtPointer, XtPointer );

void 
ycentre_world( Widget, XtPointer, XtPointer );

void 
xsection_world( Widget, XtPointer, XtPointer );

void 
ysection_world( Widget, XtPointer, XtPointer );

void 
redraw_world( Widget, XtPointer, XtPointer );

void
undo_world( Widget, XtPointer, XtPointer );

/* 
 * a function which queries the status of the plotting mode flag
 */
int 
PlotMode();


/*
 * queries if button block flag is set
 */
int
ButtonsBlocked();


/*
 * makes interrupt button sensitive and clears the flag (Fortran callable)
 */
void 
itron_();


/*
 * turns interrupt button insensitive to mouse buttons (Fortran callable)
 */
void 
itroff_();


/*
 * queries status of interrupt flag, clears it if set (Fortran callable)
 */
int 
itrflag_();


/*
 * process any important pending X events (must be called before calls to
 * 'itrflag')
 */
void
procpend_();


/*
 * unblock the configurable buttons
 */
void
butunb_();

/*
 * function to turn on and off bubble help
 */
void
bubbles_(int *arg);

#endif /* _Buttons_h */

/* Revision history:
 * -----------------
 * $Log: Buttons.h,v $
 * Revision 1.4  1996/06/15 02:29:43  ulf
 * added 'undo_world' function for plot mode undo command
 *
 * Revision 1.3  1996/02/06 15:17:00  ulf
 * add function 'butunb' to unblock configurable buttons
 *
 * Revision 1.2  1996/01/31  15:45:44  ulf
 * added copyleft
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */
