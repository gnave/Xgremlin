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
 * Time functions for Fortran programs.
 */

#include "F77_types.h"

#ifdef __cplusplus
  extern "C" {
#endif

double 
seconds_(void);
/*
 * Returns the number of seconds since
 * the beginning of 1900 as a double precision number.
 * Use
 *      REAL*8 T
 *      T = SECONDS()
 * in the Fortran program.
 */

void 
timestr_( char *, ftnlen );
/*
 * Returns a date and time string in the format "DD/MM/YY  HH:MM:SS".
 * Use
 *      CHARACTER S*20
 *      CALL TIMESTR(S)
 * in the Fortran program
 */

void
datestr_( char *, ftnlen);
/*
 * returns the date in the format "MON dd,yy"
 * Use
 *      CHARACTER S*20
 *      CALL DATESTR(S)
 */

void 
getdat_( int*, int*, int* );
/*
 * returns current year, month, day
 * Use
 *      INTEGER IYEAR, MONTH, IDAY
 *      CALL GETDAT( IYEAR, MONTH, IDAY )
 */

void 
gettim_( int*, int*, int*, int* );
/*
 * returns current time
 * Use
 *       INTEGER IHOUR, MINUTE, ISECOND, IHSECOND
 *       CALL GETTIM( IHOUR, MINUTE, ISECOND, IHSECOND )
 */

void 
getcpu_( int* );
/*
 * returns elapsed CPU time of the running process
 * CALL GETCPU( ICPU )
 */

void 
cputime_( char *timestr, ftnlen len );
/*
 * returns the elapsed CPU time as a string in the form hh:mm:ss
 *
 * CHARACTER TS*10
 * CALL CPUTIME( TS )
 */

#ifdef __cplusplus
  }
#endif
