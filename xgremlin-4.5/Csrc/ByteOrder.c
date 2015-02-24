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

#include "ByteOrder.h"
#include "byteorder.h"

/*-----------------------------------------------------------------*/

void 
gmboc_( int *mboc ) 
{
   *mboc = byte_order();
}

/*-----------------------------------------------------------------*/

void 
chibord_( int *inarr, int *num ) 
{
   int *ip;
   int k;

   ip = inarr;  
   for ( k=0;  k<*num;  k++, ip++ ) 
     byte_reverse_int( ip );
}

/*-----------------------------------------------------------------*/

void
sbrev_( short int *inum )
{
   byte_reverse_short( inum );
}

/*-----------------------------------------------------------------*/

void
ibrev_( int *inum )
{
   byte_reverse_int( inum );
}

/*-----------------------------------------------------------------*/

void
fbrev_( float *fnum )
{
   byte_reverse_float( fnum );
}

/*-----------------------------------------------------------------*/

void
dbrev_( double *dnum )
{
   byte_reverse_double( dnum );
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * -----------------
 * $Log: ByteOrder.c,v $
 * Revision 1.16  1996/08/24 03:18:02  ulf
 * fixed the parameter type of 'byte_reverse_float'
 *
 * Revision 1.15  1996/03/03 15:02:54  ulf
 * functions in 'byteorder.h' are now inlined from a library.
 *
 * Revision 1.14  1995/11/04  11:53:54  ulf
 * use generic byte reversal function for doubles only.
 *
 * Revision 1.13  1995/11/04  09:01:22  ulf
 * accomodate modifications in 'byteorder.[hc]'
 *
 * Revision 1.12  1995/10/31  10:58:14  ulf
 * cosmetic fixed in 'chibord'
 *
 * Revision 1.11  1995/10/01  15:09:21  ulf
 * fixed a typo in 'chibord'
 *
 * Revision 1.10  1995/09/28  00:55:44  ulf
 * base subroutines on generic module 'byteorder'
 *
 * Revision 1.9  1995/09/07  21:07:46  ulf
 * removed 'unsigned' in function gmboc
 *
 * Revision 1.8  1995/09/07  21:06:24  ulf
 * better generic byte reversal function for doubles
 *
 * Revision 1.7  1995/09/07  21:04:00  ulf
 * make sure it also works on machines with sizeof(long) > sizeof(int)
 *
 * Revision 1.6  1995/09/07  21:00:54  ulf
 * byte order is now calculated on the fly and not defined
 *
 * Revision 1.5  1995/08/11  02:02:02  ulf
 * removed function 'sdboc_'. Data and machine byte order are now always
 * compared in the Fortran part of the program.
 *
 * Revision 1.4  1995/06/26  00:02:40  ulf
 * remove unused variables from function 'sbrev_'
 *
 * Revision 1.3  1995/06/22  17:23:10  ulf
 * cosmetic fix: static int  -->  static const int
 *
 * Revision 1.2  1995/06/22  03:39:38  ulf
 * simplify compile time definition machine byte order code
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */


