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
 * $Id: Phase.h,v 1.1 1995/09/22 23:03:59 ulf Exp $
 */

/*
 * a GUI interface to phase correction
 */


/*
 * initialize phase correction module
 */
void
InitializePhase( Widget button );


/*
 * open the window with the GUI to phase correction
 */
void
OpenPhaseWindow();

/*
 * additional actions
 */

/*
 * used to give the focus to the text input windows
 */
void
Focus( Widget w, XEvent *ev, String *par, Cardinal *npar );

 
/*
 * bound to return key, gives focus back to shell widget
 */
void
Accept( Widget w, XEvent *ev, String *par, Cardinal *npar );


/* Revision history:
 * -----------------
 * $Log: Phase.h,v $
 * Revision 1.1  1995/09/22  23:03:59  ulf
 * Initial revision
 *
 */

