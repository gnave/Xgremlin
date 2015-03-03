/*
 * Copyright (C) 1994, 1995, 1996
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
 * Function called when user presses the help button
 */

#ifndef _Help_h
#define _Help_h


/*
 * initialize the help system
 */
void
InitializeHelp();


/*
 * launch the html browser with help text
 */
void
StartHelp();


/*
 * set the name of the HTML viewer (Fortran callable)
 */
void
htmlbrowser_( char *browser, int len );


#endif /* _Help_h */

/* Revision history:
 * -----------------
 * $Log: Help.h,v $
 * Revision 1.6  1996/05/12 03:36:17  ulf
 * Help system completely revised. Xgremlin now uses an external HTML
 * browser to display online documentation.
 *
 * Revision 1.5  1995/09/13 01:46:23  ulf
 * display variable is not passed in InitializeHelp any longer, use global instead.
 *
 * Revision 1.4  1995/08/03  03:36:31  ulf
 * do not pass button widget to function InitializeHelp any more
 *
 * Revision 1.3  1995/08/03  03:25:36  ulf
 * remove  StartHelp  callback function and export  OpenHelpWindow
 *
 */
