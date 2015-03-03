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

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include "timer.h"

double 
seconds( void )
{
   time_t t;

   t = time( NULL );
   return (double)t;
}

void 
timestr( char str[], int len )
{
   time_t time_val;
   struct tm *all_times;

   time_val  = time( NULL );
   all_times = localtime( (time_t*) &time_val );
   strftime( str, len, "%H:%M:%S", all_times );
}


void 
datestr( char str[], int len )
{
   time_t time_val;
   struct tm *all_times;

   time_val  = time( NULL );
   all_times = localtime( (time_t*) &time_val );
   strftime( str, len, "%b %d, %Y", all_times );
}

void
getdat( int *year, int *month, int *day )
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
gettim( int *hour, int *minute, int *second, int *hsec )
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


