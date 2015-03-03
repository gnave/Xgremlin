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
 * $Id: Functions.h,v 1.3 1995/11/09 16:10:38 ulf Exp $
 */

/*
 * The actions bound to the function buttons
 */


#ifndef _Functions_h
#define _Functions_h

#include "F77_types.h"

#define NUM_FUNC_BUTTONS  8
#define LABEL_LEN         128

/*
 * the callback function that is used for all of the buttons
 */
void 
FunctionChoice(Widget, XtPointer, XtPointer);


/*
 * stores the widget of a button in the command table
 */
void
InitFunctions( int nbut, Widget w );


/*
 * sets up the default functions for the user definable buttons
 */
void
SetDefaultFunctions();


/*
 * a Fortran callable function to assign a plotting mode function to a
 * button
 */
void
butplot_( int *numbut, char cmd[], char lbl[], ftnlen lenc, ftnlen lenl );


/*
 * a Fortran callable function to assign an edit mode function to a
 * button
 */
void
butedit_( int *numbut, char cmd[], char lbl[], ftnlen lenc, ftnlen lenl );


#endif /* _Functions_h */

/* Revision history:
 * -----------------
 * $Log: Functions.h,v $
 * Revision 1.3  1995/11/09  16:10:38  ulf
 * new function 'SetDefaultFunctions'
 *
 * Revision 1.2  1995/06/27  04:39:30  ulf
 * change interface of function 'butplot_'.
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */


