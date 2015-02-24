/*
 * Copyright (C) 1992, 1994, 1995
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

#if defined( HAVE_BSD_TIME )
#include <unistd.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include "timer.h"

#if defined( HAVE_BSD_TIME )
#define CLOCKTICKS  _SC_CLK_TCK
#endif

#if defined( HAVE_ANSI_TIME )
#define CLOCKTICKS  CLOCKS_PER_SEC
#endif

double 
seconds_( void )
{
   return time( NULL );
}

void 
timestr_( char str[], ftnlen len )
{
   time_t time_val;
   struct tm *all_times;
   int i,k;

   time_val  = time( NULL );
   all_times = localtime( (time_t*) &time_val );
   strftime( str, len, "%d/%m/%y  %H:%M:%S", all_times );
   
   /*
    * turn it into a Fortran string
    */
   i = 0;
   while ( str[i] != '\0' )    /* search terminating \0 */
     i++;
   for ( k=i; k<len; k++ )
     str[k] = ' ';
}


void 
datestr_( char str[], ftnlen len )
{
   time_t time_val;
   struct tm *all_times;
   int i,k;

   time_val  = time( NULL );
   all_times = localtime( (time_t*) &time_val );
   strftime( str, len, "%b %d,%y", all_times );
   
   /*
    * turn it into a Fortran string
    */
   i = 0;
   while ( str[i] != '\0' )    /* search terminating \0 */
     i++;
   for ( k=i; k<len; k++ )
     str[k] = ' ';
}


void 
getcpu_( int *icpu ) 
{
   *icpu = (int) clock() / CLOCKTICKS;
}
 

void 
getdat_( int *year, int *month, int *day ) 
{
   time_t time_val;
   struct tm *now;

   time_val = time( NULL );
   now      = localtime( (time_t*) &time_val );
   *year  = now->tm_year + 1900;
   *month = now->tm_mon + 1;
   *day   = now->tm_mday;
}


void 
gettim_( int *hour, int *minute, int *second, int *hsec ) 
{
   time_t time_val;
   struct tm *now;

   time_val = time( NULL );
   now      = localtime( (time_t*) &time_val );
   *hour  = now->tm_hour;
   *minute= now->tm_min;
   *second= now->tm_sec;
   *hsec  = 0;  /* gets ignored since not available in ANSI C */     
}

void 
cputime_( char *timestr, ftnlen len )
{
   int cputime_in_sec;
   int hh,mm,ss;
   int i, k;
   int rest;

   cputime_in_sec = (int) clock() / CLOCKTICKS;
   hh = cputime_in_sec / 3600;
   rest = cputime_in_sec - hh * 3600;
   mm = rest / 60;
   ss = rest - mm * 60;
   sprintf(timestr, "%02d:%02d:%02d", hh, mm, ss );
   i = strlen( timestr );
   timestr[i] = ' '; /* overwrite \0 */
   for ( k=i+1; k<len; k++ )
     timestr[k] = ' ';
}



