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
 * $Id: Scratch.h,v 1.2 1996/03/09 17:04:11 ulf Exp $
 */

/*
 * An interface to the Xgremlin scratch file directory
 */

#ifndef _Scratch_h
#define _Scratch_h

#include "F77_types.h"

/*
 * open a specified scratch directory. create it if it does not exist
 */
void
opensdir_( char def_name[], ftnlen len );


/*
 * list the scratch directory file ( 'whats saved' )
 */
void
lssdir_();


/*
 * low level function to read the contents of a scratch file
 *
 * nums (in) :  number (key) of the scratch file
 * npts (i/o):  number of points to be read, number of points actually read
 * nwpp (out):  =1: points are real, =2: points are complex
 * wref (out):  wavenumber reference and dispersion
 * delw (out):
 * arr  (out):  the array with the points
 * memo (out):  scratch file id
 */
void
rlowscr_( int *nums, int *npts, int *nwpp, double *wref, double *delw, float *arr,
	  char memo[], ftnlen len);


/*
 * low level function to write a buffer to a scratch file
 *
 * nums (in) :  number (key) of the scratch file
 * npts (in) :  number of points to be saved
 * nwpp (in) :  =1: points are real, =2: points are complex
 * wref (in) :  wavenumber reference and dispersion
 * delw (in) :
 * arr  (in) :  the array with the points
 * memo (in) :  the scratch file id
 */
void
wlowscr_( int *nums, int *npts, int *nwpp, double *wref, double *delw, float *arr,
	  char memo[], ftnlen len);


/*
 * low level function to read 8k points of a scratch file into 'r'
 * (used in oscrread)
 *
 * nums (in) :  number (key) of the scratch file
 * nwpp (out):  =1: points are real, =2: points are complex
 * wref (out):  wavenumber reference and dispersion
 * delw (out):
 * memo (out):  scratch file id
 */
void
read8kr_( int *nums, int *nwpp, double *wref, double *delw,
	  char memo[], ftnlen len);


/*
 * low level function to write 8k points from 'r' to a scratch file
 * (used in oscrwrite)
 *
 * nums (in) :  number (key) of the scratch file
 * nwpp (in) :  =1: points are real, =2: points are complex
 * wref (in) :  wavenumber reference and dispersion
 * delw (in) :
 * memo (in) :  the scratch file id
 */
void
write8kr_( int *nums, int *nwpp, double *wref, double *delw,
	  char memo[], ftnlen len);


/*
 * remove a scratch file with key number 'key'
 */
void
rmscr_( int *key );


#endif /* _Scratch_h */

/* Revision history:
 * -----------------
 * $Log: Scratch.h,v $
 * Revision 1.2  1996/03/09  17:04:11  ulf
 * added subroutines 'write8kr' and 'read8kr' to directly read and
 * write data from/to the 'r' array
 *
 * Revision 1.1  1995/10/25  13:21:13  ulf
 * Initial revision
 *
 */
