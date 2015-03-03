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

/*
 * Time functions
 */

#ifdef __cplusplus
  extern "C" {
#endif

double 
seconds(void);
/*
 * Returns the number of seconds since
 * the beginning of 1900 as a double precision number.
 * Use
 *      REAL*8 T
 *      T = SECONDS()
 * in the Fortran program.
 */

void 
timestr( char str[], int len );
/*
 * Returns time string in the format "HH:MM:SS".
 */

void
datestr( char str[], int len );
/*
 * returns the date in the format "MON dd, yyyy"
 */

void 
getdat( int*, int*, int* );
/*
 * returns current year, month, day
 * Use
 *      int IYEAR, MONTH, IDAY
 *      getdat( IYEAR, MONTH, IDAY )
 */

void 
gettim( int*, int*, int*, int* );
/*
 * returns current time
 * Use
 *       int IHOUR, MINUTE, ISECOND, IHSECOND
 *       gettim( IHOUR, MINUTE, ISECOND, IHSECOND )
 */

#ifdef __cplusplus
  }
#endif
