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


/*
 * a collection of wrappers for C functions to make them callable from 
 * Fortran (currently only f2c is supported).
 */

#ifndef _FortranWrappers_h
#define _FortranWrappers_h

#include "F77_types.h"

/*------------------------------------------------------------
 * Function prototypes for some important Fortran subroutines
 *------------------------------------------------------------*/

/*
 * initializes the Fortran part of Xgremlin
 */
void
gremlininit_();


/*
 * dispatch a key command in plotting mode (calls Fortran)
 * and pass dynamic arrays to Fortran.
 */
void
gdispatch_(int *ch, int *shift, int *ctrl, int *meta,
	   float *r, float *tr, float *phz,
	   double *point, float *amp, float *wd, float *dmp, 
	   float *eps1, float *eps2, float *eps3, float *eps4, float *eps5, 
	   int *nit, int *nhold, 
           char *ctag, char *dent, int ctag_len, int dent_len);


/*
 * call the gremlin command interpreter with a command and
 * pass dynamic arrays to Fortran part of Xgremlin.
 */
void
dispatch_(char *line, float *r, float *tr, float *ffta,
	  float *phz, double *point, float *amp, float *wd,
	  float *dmp, float *eps1, float *eps2, float *eps3, float *eps4,
	  float *eps5, int *nit, int *nhold, char *ctag, char *dent,
	  int line_len, int ctag_len, int dent_len);


/*
 * called to exit Xgremlin, closes all files
 */
void
dexit_();


/*
 * remove a keyboard command lock (Fortran subroutine)
 */
void
unlkbd_();


/*
 * prototype for a Fortran subroutine (lives in 'plot.f')
 */
int
fncpinfo_( char[], int *, int );


/*------------------------------------------------------------
 * generally useful subroutines implemented in C
 *------------------------------------------------------------*/

/*
 * get an environment variable:  length = getenv( variable, value )
 * ( Fortran callable )
 *
 * Usage:
 *     CHARACTER VARIABLE*128, VALUE*128  
 *     CALL GETENV( VARIABLE, VALUE )
 */
int
getenv_( char[], char[], ftnlen, ftnlen );


/*
 * get the PID of the running Xgremlin
 *
 * Usage:
 *    INTEGER PID
 *    CALL GETPID( PID )
 */
void
getpid_( int *pid );


/* 
 * get a users home directory (Fortran callable)
 *
 * Usage:
 *    CHARACTER HD*256
 *    CALL HOMEDIR( HD )
 */
void 
homedir_( char[], ftnlen );


/*
 * return some version information
 */
void
versinfo_( char host[], char osname[], char osversion[], char cversion[],
	   char fversion[],
           ftnlen host_len, ftnlen osname_len, ftnlen osversion_len,
           ftnlen cversion_len, ftnlen fversion_len );

/*
 * return strings with system information
 */
void
sysinfo_( char host[], char osversion[], ftnlen host_len, ftnlen osversion_len );


/*
 * convert a Fortran string to a \0 terminated C string
 * 
 * Usage:
 *     CHARACTER  STR*80
 *     CALL STRFORTOC( STR, 80 )
 */
void 
strfortoc_( char[], ftnlen );


/*
 * create the name of a unique temporary file to save data
 *
 * Usage:
 *     CHARACTER TMPNAM*32
 *     CALL MKTMP( TMPNAM )
 */
void
mktmp_( char[], ftnlen );


/*
 * pop up a dialogue box which prints a message and lets the
 * user write a response.
 *
 * Usage:
 *    CHARACTER  MSG*80, RSP*80
 *    CALL DIALOG( MSG, RSP )
 */
/* void dialog_( char msg[], char rsp[], ftnlen lenmsg, ftnlen lenrsp ); */


/*
 * used to retrieve the version string
 *
 * Usage:
 *    CHARACTER VER*40
 *    CALL GETVER( VER )
 */
void 
getver_( char ver[], ftnlen len );


/*
 * a function to change the current working directory
 *
 * Usage:
 *    CHARACTER STR*40
 *    STR = '~/test'
 *    CALL CHNGDIR( STR )
 */
void
chngdir_( char dir[], ftnlen len );


/*
 * a function to print the current working directory
 *
 * Usage:
 * CALL PWDIR
 */
void
pwdir_();


/*
 * store a new plot size 
 * 
 * Usage:
 *    CHARACTER STR*40
 *    INTEGER ILANDS
 *    STR = '5x7'
 *    ILANDS = 1                    ! landscape mode
 *    CALL PLTSIZ( STR, ILANDS )
 */
void
pltsiz_( char str[], int* ilands, ftnlen len );


/*
 * a C callable function to read the plot size
 */
void
get_plotsize( char str[] );


/*
 * a function to retrieve the orientation
 */
int
get_orientation();


/*
 * create a copy of the file 'filnam' and give it the extension '.ext'
 */
void
mkbackup_( char filnam[], char ext[], int *ierr, ftnlen flen, ftnlen elen );


/*
 * returns 1 if the display is a monochrome display
 * (used to propaget the command line option -m to the Fortran part of Xgremlin
 */
void
dsptype_( int *mono );


/*
 * passes a command string to the command dispatcher from within Fortran
 * (a wrapper for 'dispatch')
 */
void
runcmd_( char cmd[], ftnlen len );


/*
 * passes a plot command to the graphics command dispatcher
 * (a wrapper for  'gdispatch')
 */
void
keycmd_(int *ch, int *shift, int *ctrl, int *meta);


/*
 * copies a block of real numbers from source to destination
 */
void
rmove_(int *npts, float *src, float * dst);


#endif /* _FortranWrappers_h */

/* Revision history:
 * -----------------
 * $Log: FortranWrappers.h,v $
 * Revision 1.21  1996/07/06 03:25:16  ulf
 * change name 'mkftstmp' --> 'mktmp'
 *
 * Revision 1.20  1996/03/14 16:46:38  ulf
 * taken out array rtr from 'dispatch'
 *
 * Revision 1.19  1996/03/13  17:49:10  ulf
 * fixed order of string argument lengths in 'dispatch'
 *
 * Revision 1.18  1996/03/11  17:00:55  ulf
 * array 'iffta' in 'dispatch' not needed any more
 *
 * Revision 1.17  1996/03/09  17:12:03  ulf
 * 'gdispatch' needs phase array also
 *
 * Revision 1.16  1996/03/08  16:48:37  ulf
 * eps? variables are needed in 'gdispatch'
 *
 * Revision 1.15  1996/03/06  17:50:00  ulf
 * fewer parameters in 'gdispatch' (no fft related arrays are needed).
 *
 * Revision 1.14  1996/03/03  13:51:49  ulf
 * new function 'keycmd' which is a simple wrapper to 'gdispatch'. Added
 * array parameters to 'gdispatch'
 *
 * Revision 1.13  1996/03/02  15:44:19  ulf
 * added subroutine 'runcmd'
 *
 * Revision 1.12  1996/01/29  16:44:48  ulf
 * add prototype for function fncpinfo_
 *
 * Revision 1.11  1995/11/11  10:28:39  ulf
 * new function get_orientation
 *
 * Revision 1.10  1995/11/11  10:25:07  ulf
 * 'pltsiz' function allows to alter plot orientation
 *
 * Revision 1.9  1995/10/03  08:13:02  ulf
 * added a prototype for f77 subroutine 'unlkbd'
 *
 * Revision 1.8  1995/09/23  15:41:43  ulf
 * add function 'dsptype' to inquire about the display type
 *
 * Revision 1.7  1995/09/10  18:25:02  ulf
 * new functions 'versinfo' and 'sysinfo'
 *
 * Revision 1.6  1995/08/20  04:26:28  ulf
 * Added prototype for 'dexit' to make it callable from C.
 *
 * Revision 1.5  1995/08/20  02:30:59  ulf
 * rename function  mkrtmp --> mkftstmp
 *
 * Revision 1.4  1995/08/16  02:43:56  ulf
 * add function 'mkrtmp'
 *
 * Revision 1.3  1995/07/21  02:14:14  ulf
 * add 'getpid_' function to get PID in Fortran
 *
 * Revision 1.2  1995/07/04  22:55:50  ulf
 * temporarily take out function dialog_
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */
