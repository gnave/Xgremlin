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


#ifndef _byteorder_h
#define _byteorder_h


#ifdef __GNUC__
#define INLINE __inline__
#else
#define INLINE
#endif


/*
 * Xgremlin machine byte order codes
 */
#if !defined(LITTLE_ENDIAN) && !defined(BIG_ENDIAN)
#define LITTLE_ENDIAN	0	
#define BIG_ENDIAN	1
#endif


/*
 * return the byte order of the machine the program is running on
 */
extern INLINE int
byte_order();


/*
 * reverse the byte order in a short int (2 bytes)
 */
extern INLINE void
byte_reverse_short( short int *inum ); 


/*
 * reverse the byte order in an int (4 bytes)
 */
extern INLINE void
byte_reverse_int( int *inum );         /* integer */


/*
 * reverse the byte order in a float (4 bytes)
 */
extern INLINE void
byte_reverse_float( float *fnum );       /* float */


/*
 * reverse the byte order in a double (8 bytes)
 */
extern INLINE void
byte_reverse_double( double *dnum );

#endif /* _ByteOrder_h */

/* Revision history:
 * -----------------
 * $Log: byteorder.h,v $
 * Revision 1.7  1996/09/15 03:07:38  ulf
 * define  LITTLE_ENDIAN  and BIG_ENDIAN  instead of  LITTLE and BIG
 *
 * Revision 1.6  1996/08/15 01:25:53  ulf
 * fixed argument type of 'byte_reverse_float'
 *
 * Revision 1.5  1996/03/22 14:21:58  ulf
 * better use of __inline__
 *
 * Revision 1.4  1996/03/03  14:56:33  ulf
 * add 'extern INLINE' to function definitions to enforce
 * inlining from a library file
 *
 * Revision 1.3  1995/11/04  11:49:57  ulf
 * use generic byte reversal procedure only (not using 'long long int' )
 *
 * Revision 1.2  1995/11/04  08:55:31  ulf
 * remove type conversions from byte reversal functions
 *
 * Revision 1.1  1995/09/27  21:26:41  ulf
 * Initial revision
 *
 */


