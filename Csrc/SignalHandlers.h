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
 * $Id: SignalHandlers.h,v 1.1 1995/07/09 03:03:05 ulf Exp $
 */

/*
 * signal handlers to catch SIGFPE and SIGSEGV. Floating point exceptions
 * seem to occur mostly as a result of broken header files ( i.e. too many
 * of them ).
 */

#ifndef _SignalHandlers_h
#define _SignalHandlers_h


/*
 * install the signal handlers
 */
void
install_signal_handlers();


#endif /* _SignalHandlers_h */

/* Revision history:
 * -----------------
 * $Log: SignalHandlers.h,v $
 * Revision 1.1  1995/07/09  03:03:05  ulf
 * Initial revision
 *
 */
