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
 * Contains the actions bound to the buttons in the options menu
 */

#ifndef _OptionMenu_h
#define _OptionMenu_h

#include <X11/Intrinsic.h>     /* X toolkit intrinsics definitions */
#include <X11/StringDefs.h>    /* Standard Name-String definitions */
#include <X11/Shell.h>

#include "F77_types.h"

/*
 * callback function for the main Gremlin menu
 */
void 
MenuChoice(Widget, XtPointer, XtPointer);


/*
 * initialize the quit button action
 */
void 
InitializeVersion( Widget );


/*
 * closes the main Xgremlin window, Fortran callable
 */
void
closexgr_();


/*
 * set the printer for screen dumps
 */
void
setprt_( char prname[], ftnlen len );


/*
 * display currently set printer
 */
void
shwprt_();


#endif /* _OptionMenu_h */

/* Revision history:
 * -----------------
 * $Log: OptionMenu.h,v $
 * Revision 1.3  1995/08/23  02:27:36  ulf
 * new function 'shwprt'
 *
 * Revision 1.2  1995/07/07  21:10:38  ulf
 * new function setprt_ to make printer setable from outside.
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */




