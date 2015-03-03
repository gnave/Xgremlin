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
#include <X11/Intrinsic.h>     /* X toolkit intrinsics definitions */
#include <X11/StringDefs.h>    /* Standard Name-String definitions */
#include <X11/keysym.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Edit.h"
#include "arralloc.h"

/*
 * include pointers to global data arrays
 */
#include "arrays.h"

static int ctag_num = 0;

/*
 * a table with aligned and unaligned addresses of allocated arrays
 */
typedef struct {
  int  size;       /* the number of bytes actually allocated */
  char *malloc;    /* the address returned by 'malloc' */
  char *aligned;   /* the address aligned to double word boundaries */
} arr_adr;
static arr_adr adr_table[NUM_ARRAYS];

/*
 * local prototype
 *
 * ptr :  pointer to array
 * size:  size of an array entry
 * np  :  number of array entries
 */
static void * 
gmalloc( void *ptr, int size, int np );
 
/*-----------------------------------------------------------------*/

void
rtralloc_( int *npts, int *ierr )
{
   void *p;

   *ierr = 0;
   p = gmalloc( rtr, sizeof(float), 2 * *npts );
   if ( p == NULL )
     {
	*ierr = 1;
	return;
     }

   r = tr = phz = rtr = p; /* r is lower half of rtr */
   tr  += *npts;           /* tr: upper half of rtr */
   phz += *npts/2;         /* phz: upper half of r */
}

/*-----------------------------------------------------------------*/

void
fftalloc_( int *npts, int *ierr )
{
   void *p;

   *ierr = 0;
   p = gmalloc( ffta, sizeof(float), *npts );
   if ( p == NULL )
     {
	*ierr = 1;
	return;
     }
   ffta = p;
}

/*-----------------------------------------------------------------*/

void
linalloc_( int *npts, int *ierr )
{
   void *p;

   *ierr = 0;
   p = gmalloc( point, sizeof(double), *npts );
   if ( p == NULL ) {
	*ierr = 1;
	return;
   }
   point = p;

   p = gmalloc( amp, sizeof(float), *npts );
   if ( p == NULL ) {
	*ierr = 1;
	return;
   }
   amp = p;

   p = gmalloc( wd, sizeof(float), *npts );
   if ( p == NULL ) {
	*ierr = 1;
	return;
   }
   wd = p;

   p = gmalloc( dmp, sizeof(float), *npts );
   if ( p == NULL ) {
	*ierr = 1;
	return;
   }
   dmp = p;

   p = gmalloc( eps1, sizeof(float), *npts );
   if ( p == NULL ) {
	*ierr = 1;
	return;
   }
   eps1 = p;

   p = gmalloc( eps2, sizeof(float), *npts );
   if ( p == NULL ) {
	*ierr = 1;
	return;
   }
   eps2 = p;

   p = gmalloc( eps3, sizeof(float), *npts );
   if ( p == NULL ) {
	*ierr = 1;
	return;
   }
   eps3 = p;

   p = gmalloc( eps4, sizeof(float), *npts );
   if ( p == NULL ) {
	*ierr = 1;
	return;
   }
   eps4 = p;

   p = gmalloc( eps5, sizeof(float), *npts );
   if ( p == NULL ) {
	*ierr = 1;
	return;
   }
   eps5 = p;

   p = gmalloc( nit, sizeof(int), *npts );
   if ( p == NULL ) {
	*ierr = 1;
	return;
   }
   nit = p;

   p = gmalloc( nhold, sizeof(int), *npts );
   if ( p == NULL ) {
	*ierr = 1;
	return;
   }
   nhold = p;

   p = gmalloc( ctag, 4, *npts );
   if ( p == NULL ) {
	*ierr = 1;
	return;
   }
   ctag = p;

   p = gmalloc( dent, 32, *npts );
   if ( p == NULL ) {
        *ierr = 1;
	return;
   }
   dent = p;

   /*
    * fill strings with blanks
    */
   if ( *npts > ctag_num ) 
     {
	memset( ctag +  4*ctag_num, ' ',  4 * (*npts - ctag_num) );
	memset( dent + 32*ctag_num, ' ', 32 * (*npts - ctag_num) );
     }
   ctag_num = *npts;
}

/*-----------------------------------------------------------------*/

static void * 
gmalloc( void *ptr, int size, int np )
{
  register char *adr;
  int req_size;
  int i;

  /*
   * some machines required variables of type 'double' to be 
   * aligned on double word boundaries. Unfortunately, not all
   * 'malloc' implementations return memory aligned on 8 byte
   * boundaries so we do the alignment here manually which requires
   * some extra book-keeping.
   */
  
  /*
   * allocate 8 more bytes to have room for alignment
   */
  req_size = size * np + sizeof(double);

  if ( ptr == NULL )   
    {
      /* 
       * an array is allocated for the first time 
       * first find the next free entry in the address table
       */
      for ( i=0; i<NUM_ARRAYS; i++)
	if ( adr_table[i].malloc == NULL )
	  break;

      adr = malloc( req_size );
    }
   else
     {
       /*
	* an array is made smaller or larger
	* first find the true address of the array in the table
	*/
       for ( i=0; i<NUM_ARRAYS; i++)
	 if ( adr_table[i].aligned == ptr )
	   break;
    
       adr = realloc( adr_table[i].malloc, req_size );
     }

  /*
   * align the address to a double word boundary
   */
  if ( adr != NULL )
    {
      adr_table[i].size = req_size;
      adr_table[i].malloc = adr;
      if ( (int)adr % sizeof(double) != 0 )     /* address is not aligned */
	adr = (char *) (sizeof(double) * ( 1 + (int)adr / sizeof(double) ));
      adr_table[i].aligned = adr;
    }
  else
    {
      fprintf(stderr, "Fatal error :  virtual memory exhausted.\n" );
      WriteStr(" Fatal error :  virtual memory exhausted.");
      return NULL;
    }
  
  return (void *)adr;
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * -----------------
 * $Log: arralloc.c,v $
 * Revision 1.10  1996/04/22 01:59:32  ulf
 * added book-keeping for double word alignment of allocated memory
 * on systems which do not return memory double word aligned.
 *
 * Revision 1.9  1996/03/18 16:45:02  ulf
 * buid in error checking in memory allocation functions
 *
 * Revision 1.8  1996/03/18  14:39:02  ulf
 * fixed assignment of tr and phz (should have learned my pointer arithmetic ...)
 *
 * Revision 1.7  1996/03/12  18:17:57  ulf
 * always return a value in 'gmalloc'
 *
 * Revision 1.6  1996/03/12  18:12:43  ulf
 * fixed a bunch of typos
 *
 * Revision 1.5  1996/03/12  15:44:49  ulf
 * fill strings with blanks after allocation
 *
 * Revision 1.4  1996/03/02  09:29:31  ulf
 * make 'gmalloc' static
 *
 * Revision 1.3  1996/02/21  17:39:12  ulf
 * use include file  arrays.h  for array definition because these are
 * used in various places of the program.
 *
 * Revision 1.2  1996/02/05  16:54:08  ulf
 * fixed a bugger in 'gmalloc'
 *
 * Revision 1.1  1996/02/04  18:14:54  ulf
 * Initial revision
 *
 */



