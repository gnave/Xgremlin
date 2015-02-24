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
 * miscellaneous functions for XGremlin
 */


#ifndef _Miscellaneous_h
#define _Miscellaneous_h

#include "defaults.h"
#include "List.h"

/*
 * return a string with the fully qualified domain name
 * function returns : 0   FQDN found
 *                   -1   host name not found
 *                   -2   domain name not found
 */
int 
GetFQDN( char[] );


/*
 * return the host name of the machine
 */
int 
GetHN( char[] );


/*
 * get the home directory of the user running the program
 */
void
get_homedir( char dir[] );


/*
 * print a list of command line options
 */
void 
Syntax();


/*
 * print a message and terminate the program
 */
void 
Abort( char[] );


/*
 * remove the last character from a string and return it. Returns \0
 * if the string has zero length.
 */
extern INLINE char 
Chop( char[] );


/*
 * remove all trailing blanks from a string
 */
extern INLINE void
remove_trailing_blanks( char s[] );


/*
 * convert a Fortran string to a C string (C callable)
 */
extern INLINE void 
StrForToC( char[], int );


/*
 * convert a \0 terminated C string to a ' ' padded Fortran string
 * returns length of C string
 */
extern INLINE int 
StrCToFor( char[], int );


/*
 * returns TRUE if x is in the interval [a,b], FALSE otherwise
 */
extern INLINE int
in_interval( int a, int b, int x );


/*
 * returns the names of the files in the current directory in a linked list
 */
void
current_dir_files( tList files );


#endif /* _Miscellaneous_h */

/*-----------------------------------------------------------------*/

/* Revision history:
 * ----------------
 * $Log: Miscellaneous.h,v $
 * Revision 1.8  1996/09/07 04:26:04  ulf
 * added function 'remove_trailing_blanks'
 *
 * Revision 1.7  1995/11/21 15:45:56  ulf
 * inline more functions
 *
 * Revision 1.6  1995/09/10  17:47:46  ulf
 * remove inclusion of f2c.h
 *
 * Revision 1.5  1995/08/24  21:16:43  ulf
 * removed scratch file locking again because it is nonsense
 *
 * Revision 1.4  1995/08/22  23:25:08  ulf
 * return lock status in 'unlock' function
 *
 * Revision 1.3  1995/08/22  23:11:28  ulf
 * combined 'chklock' and 'lock'
 *
 * Revision 1.2  1995/08/22  22:30:39  ulf
 * add file locking functions for the scratch file
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */
