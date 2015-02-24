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
 * Part of Xgremlin; a function that checks the type of a command line argument
 */

#include <stdio.h>
#include <string.h>
#include "F77_types.h"
#ifdef TEST_ARGTYPE
void StrForToC( char str[], int len );
#else
#include "Miscellaneous.h"
#endif
#include "regexpr.h"

#define IS_A_INTEGER     1
#define IS_A_FLOAT       2
#define IS_A_STRING      3

#define ALPHA_STRLEN     80    /* max length of a string argument */

/*
 * These are the regular expressions which describe integer 
 * and floating point argument types
 */
#define RE_INTEGER    "[-+]?[0-9]+"
#define RE_FLOAT      "[-+]?([0-9]+\\.?|\\.[0-9])[0-9]*([eEdD][-+]?[0-9]+)?"


/*
 * the following variables hold the compiled regular expressions for the
 * argument types.
 */
static struct re_pattern_buffer rec_integer;
static struct re_pattern_buffer rec_float;
static struct re_registers regs;

/*-----------------------------------------------------------------*/

/*
 * check the type of argument 'argmnt' and return its type in 'itype'.
 *
 * itype = 1: integer number
 * itype = 2: floating point number
 * itype = 3: a string (not a number)
 */
void
argtype_( char argmnt[], int *itype, ftnlen len )
{
   char buff[ALPHA_STRLEN];     /* a local argument buffer; slow but safe */
   int lenbuf;
   int res;

   memset(buff, ' ', ALPHA_STRLEN );  /* fill with blanks */
   strncpy( buff, argmnt, len );
   StrForToC( buff, ALPHA_STRLEN );
   lenbuf = strlen(buff);

   if ( (res = re_match(&rec_integer, buff, lenbuf, 0, &regs)) == lenbuf )
     *itype = IS_A_INTEGER;

   else if ( (res = re_match(&rec_float, buff, lenbuf, 0, &regs)) == lenbuf )
     *itype = IS_A_FLOAT;

   else
     *itype = IS_A_STRING;
}

/*-----------------------------------------------------------------*/

/*
 * compile the regular expressions
 */
void
rexcmp_()
{
   rec_integer.buffer = NULL;
   rec_integer.allocated = 0;
   rec_integer.translate = NULL;

   rec_float.buffer = NULL;
   rec_float.allocated = 0;
   rec_float.translate = NULL;

   re_set_syntax( RE_NO_BK_PARENS | RE_NO_BK_VBAR );

   if ( re_compile_pattern( RE_INTEGER, strlen(RE_INTEGER), &rec_integer ) != NULL ||
        re_compile_pattern( RE_FLOAT,   strlen(RE_FLOAT),   &rec_float )   != NULL )
     {
	fprintf(stderr, "Fatal error :  regular expression not compiled.\n");
	exit( -1 );
     }
}

/*-----------------------------------------------------------------*/

#ifdef TEST_ARGTYPE

/*
 * a test program for the argument type matching function
 */

int
main()
{
   char line[80];
   int itype;

   rexcmp_();

   for(;;)
     {
	printf(">>> ");
	gets(line);
	argtype_(line,&itype,80);

	switch( itype )
	  {
	   case 1: 
	     printf("integer\n\n");
	     break;
	   case 2: 
	     printf("float\n\n");
	     break;
	   case 3: 
	     printf("string\n\n");
	     break;
	  }
     }     
}

void
StrForToC( char str[], int len )
{
   int i;

   for (i=len-1; i>=0; i--)
     if (str[i] != ' ')
       break;
   if (i<len-1)
     str[++i] = '\0';
}
 
#endif /* TEST_ARGTYPE */

/*-----------------------------------------------------------------*/

/* Revision history:
 * -----------------
 * $Log: argtype.c,v $
 * Revision 1.2  1996/01/09  03:45:34  ulf
 * preset buffer with blanks before copying over argument string.
 *
 * Revision 1.1  1996/01/08  05:10:25  ulf
 * Initial revision
 *
 */



