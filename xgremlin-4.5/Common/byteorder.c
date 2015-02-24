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

#include "byteorder.h"

/*-----------------------------------------------------------------*/

int
byte_order()
{
  /*
   * figure out byte order on the fly
   */
  union {
     int i;
     char c[sizeof(int)];
  } u;

  u.i = 1;
  if ( (int)u.c[ sizeof(int)-1 ] == 1 )
    return BIG_ENDIAN;
  else
    return LITTLE_ENDIAN;
}

/*-----------------------------------------------------------------*/

void
byte_reverse_short( short int *inum )
{
   unsigned short int x;
   
   x = *inum;
   *inum =  ( ( x & 0x00ffU ) <<  8 ) | 
            ( ( x & 0xff00U ) >>  8 );
}

/*-----------------------------------------------------------------*/

void
byte_reverse_int( int *inum )
{
   register unsigned int x;

   x = *inum;
   *inum =  ( ( x & 0x000000ffU ) << 24 ) | 
            ( ( x & 0x0000ff00U ) <<  8 ) | 
            ( ( x & 0x00ff0000U ) >>  8 ) | 
	    ( ( x & 0xff000000U ) >> 24 );
}

/*-----------------------------------------------------------------*/

void
byte_reverse_float( float *fnum )
{
   union {
     unsigned int i;
     float f;
   } x;

   x.f = *fnum;
   x.i = ( ( ( x.i & 0x000000ffU ) << 24 ) | 
           ( ( x.i & 0x0000ff00U ) <<  8 ) | 
           ( ( x.i & 0x00ff0000U ) >>  8 ) | 
	   ( ( x.i & 0xff000000U ) >> 24 ) );
   *fnum = x.f;
}

/*-----------------------------------------------------------------*/

void
byte_reverse_double( double *dnum )
{
   union  {
      double d;     /* this relies on characters being 1 byte wide */
      char c[sizeof(double)];   
   } dc;
   int i;
   char tmp;

   dc.d = *dnum;
   for ( i=0; i<sizeof(double)/2; i++ )
     {
	tmp = dc.c[i];
	dc.c[i] = dc.c[sizeof(double)-1-i];
	dc.c[sizeof(double)-1-i] = tmp;
     }
   *dnum = dc.d;
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * -----------------
 * $Log: byteorder.c,v $
 * Revision 1.6  1996/09/15 03:08:46  ulf
 * use  int  instead of  long int  in function 'byte_order'
 *
 * Revision 1.5  1996/08/15 01:27:59  ulf
 * fixed the argument type of function 'byte_reverse_float'
 *
 * Revision 1.4  1996/03/03 14:59:00  ulf
 * removed 'inline' from function implementations
 *
 * Revision 1.3  1995/11/04  12:23:47  ulf
 * only use the generic byte reversal procedure for 'double' instead of
 * using 'long long ints'.
 *
 * Revision 1.2  1995/11/04  08:55:40  ulf
 * remove type conversions
 *
 * Revision 1.1  1995/09/27  21:26:41  ulf
 * Initial revision
 *
 */


