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
 * Part of Xgremlin; functions to allocate work arrays
 */

/*
 * allocates the arrays r, tr and the phase array phz which is an
 * alias for tr.
 *
 * npts :  number of points in the array after (re-)allocation
 * ierr :  =0 : no error; =1 error occured
 */
void
rtralloc_( int *npts, int *ierr );


/*
 * allocates arrays for Fourier transforms (ffta, iffta)
 * iffta is simply an alias for ffta.
 *
 * npts :  number of points in the array after (re-)allocation
 */
void
fftalloc_( int *npts, int *ierr );


/*
 * allocates arrays for line list functions (point, amp, wd, dmp, eps[1-5],
 * nit, nhold, ctag and dent
 *
 * npts :  number of points in the array after (re-)allocation
 */
void
linalloc_( int *npts, int *ierr );


/* Revision history:
 * -----------------
 * $Log: arralloc.h,v $
 * Revision 1.2  1996/03/18  16:34:29  ulf
 * added error variables 'ierr' to functions
 *
 * Revision 1.1  1996/02/04  18:14:54  ulf
 * Initial revision
 *
 */



