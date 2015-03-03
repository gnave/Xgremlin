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


#ifndef _ByteOrder_h
#define _ByteOrder_h

/* The function 'gmboc' returns the machine byte order code of the machine
 * on which Xgremlin is running. Data files (interferograms) read by XGremlin
 * carry a data byte order code. If the machine byte order code and the data
 * byte order code don't match data bytes must be swapped. 
 * The only two important byte orders are little endian (e.g. DEC Alpha, Intel)
 * and big endian (e.g. Motorola 680x0, Sparc processors). Other byte orders 
 * are essentially obsolete and are not supported.
 *
 * Note 1:
 * -------
 * currently, the functions in this module are meant to be called from 
 * Fortran. 
 * 
 * Usage:
 *   INTEGER MBOC
 *   CALL GMBOC( MBOC )
 */
void 
gmboc_( int * );


/*
 * changes data byte order in an integer array
 *
 * Usage:
 *    INTEGER IN(100) ! or whatever
 *    CALL CHIBORD( IN, 100 )
 *
 */
void 
chibord_( int *, int * );


/*
 * reverse byte order in an integer*2. This is a potential portability hazard.
 * integer*2 should be illegal!
 *
 * Usage:
 *    INTEGER*2 NUM
 *    CALL SBREV( NUM )
 */
void
sbrev_( short int *inum );   /* short */


/*
 * reverse the byte order in a single 32 bit integer, 
 * a single 32 bit float and a single 64 bit float number.
 *
 * Usage:
 *    DOUBLE PRECISION DNUM
 *    REAL FNUM
 *    INTEGER NUM
 *    CALL IBREV( NUM )
 *    CALL FBREV( FNUM )
 *    CALL DBREV( DNUM )
 */
void
ibrev_( int *inum );         /* integer */

void
fbrev_( float *fnum );       /* float */

void
dbrev_( double *dnum );      /* double */

#endif /* _ByteOrder_h */



