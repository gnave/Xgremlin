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

#include <X11/Intrinsic.h>     /* X toolkit intrinsics definitions */
#include <X11/StringDefs.h>    /* Standard Name-String definitions */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/utsname.h>

#include "F77_types.h"
#include "Miscellaneous.h"
#include "Edit.h"
#include "defaults.h"
#include "FortranWrappers.h"
#include "ostype.h"
#include "arrays.h"               /* for the 'runcmd' subroutine */

extern char xgremlin_version[];   /* version String */
extern int monochrome_display;    /* display type, =1 if monochrome */


/* 
 * for backup copies 
 */
#define EXTENSION_LEN  32
#define BUFFER_LENGTH  16*1024   
static char buffer[BUFFER_LENGTH];


/*
 * stores the plot size string
 */
static char loc_plot_size[32];


/*
 * stores the name of the previous directory
 */
static char prev_directory[FILE_NAME_LEN];


/*
 * stores the orientation of the plot on the page (default: landscape)
 */
static int landscape_mode = DEFAULT_ORIENTATION;

/*-----------------------------------------------------------------*/

int 
getenv_( char variable[], char value[], ftnlen varlen, ftnlen vallen)
{
   char *env_str;
   
   StrForToC( variable, (int)varlen );
   env_str = getenv( variable );
   if ( env_str == NULL ) 
      { 
	 memset(value, ' ', varlen);                 /* empty FORTRAN string */
	 return 0;
      }
   else 
      {
	strncpy( value, env_str, (int)vallen );
	StrCToFor( variable, (int)varlen );           /* remove the \0 */
	return( StrCToFor( value, (int)vallen ) );    /* return length of value */
      }
}

/*-----------------------------------------------------------------*/

void
getpid_( int *pid )
{
   *pid = (int)getpid();
}

/*-----------------------------------------------------------------*/

void 
homedir_( char dir[], ftnlen len ) 
{
   char temp_dir[FILE_NAME_LEN];

   get_homedir( temp_dir );              /* dir may be too short */
   strncpy( dir, temp_dir, (int)len );
   StrCToFor( dir, (int)len );
}

/*-----------------------------------------------------------------*/

void
versinfo_( char host[], char osname[], char osversion[], char cversion[],
	   char fversion[],
           ftnlen host_len, ftnlen osname_len, ftnlen osversion_len,
           ftnlen cversion_len, ftnlen fversion_len )
{
   strncpy( host,      SYSNAME,   (int)host_len );
   strncpy( osname,    OSNAME,    (int)osname_len );
   strncpy( osversion, OSVERSION, (int)osversion_len );
   strncpy( cversion,  CCVERSION, (int)cversion_len );
   strncpy( fversion,  FFVERSION, (int)cversion_len );
   StrCToFor( host,      (int)host_len );
   StrCToFor( osname,    (int)osname_len );
   StrCToFor( osversion, (int)osversion_len );
   StrCToFor( cversion,  (int)cversion_len );
   StrCToFor( fversion,  (int)fversion_len );
}
/*-----------------------------------------------------------------*/

void
sysinfo_( char host[], char osversion[], ftnlen host_len, ftnlen osversion_len )
{
   struct utsname info;

   if ( uname( &info ) == -1 )            /* something went wrong */
     return;
   strncpy( host, info.nodename, (int)host_len );
   strncpy( osversion, info.release, (int)osversion_len );
   strcat( osversion, " " );
   strcat( osversion, info.machine );
   StrCToFor( host,      (int)host_len );
   StrCToFor( osversion, (int)osversion_len );   
}

/*-----------------------------------------------------------------*/

void
mktmp_( char str[], ftnlen len )
{
   strcpy(str, "/tmp/xgremlin-tmp-XXXXXX");
   mkstemp(str); 
   StrCToFor( str, len );
}

/*-----------------------------------------------------------------*/

void 
strfortoc_( char str[], ftnlen len ) 
{
   StrForToC(str, (int)len);
}

/*-----------------------------------------------------------------*/

void 
getver_( char ver[], ftnlen len ) 
{
   strncpy( ver, xgremlin_version, (int)len ); 
   StrCToFor( ver, (int)len );
}

/*-----------------------------------------------------------------*/

void
chngdir_( char dir[], ftnlen len )
{
   char dirnam[FILE_NAME_LEN];  /* buffer for directory name */
   char tmpnam[FILE_NAME_LEN];  /* buffer for directory name */
   char msg[] = "Error :  could not change working directory\n";
   char *cp;

   StrForToC( dir, (int)len );
   strcpy( tmpnam, dir );
   
   if ( tmpnam[0] == '\0' )
     return;

   /* 
    * check if name is relative to home directory
    */
   if ( tmpnam[0] == '~' )
     {
	get_homedir( dirnam );            
	cp = tmpnam;              /* point to beginning */
	cp++;                     /* position after ~ */
	if ( *cp != '/' )
	  strcat( dirnam, "/" );
	strcat( dirnam, cp );
     }

   /* 
    * check if return to previous directory is wanted
    */
   else if ( tmpnam[0] == '-' )
     {
	if ( prev_directory[0] == '\0' )
	  return;
	strcpy( dirnam, prev_directory );
     }

   /*
    * no special directory
    */
   else
     strcpy( dirnam, tmpnam );

   /*
    * change directory
    */
   getcwd( prev_directory, FILE_NAME_LEN );
   if ( chdir(dirnam) == -1 )
     WriteStr( msg );
}

/*-----------------------------------------------------------------*/

void
pwdir_()
{
   char dirnam[FILE_NAME_LEN];  /* buffer for directory name */
   char tmpnam[FILE_NAME_LEN];

   if ( getcwd( tmpnam, FILE_NAME_LEN ) == NULL ) {
      strcpy( tmpnam, "Error :  could not read working directory" );
   }
   strcpy( dirnam, " " );       /* one space */
   strcat( dirnam, tmpnam );
   strcat( dirnam, "\n" );
   WriteStr( dirnam );
}

/*-----------------------------------------------------------------*/

void
pltsiz_( char str[], int* ilands, ftnlen len )
{
   StrForToC( str, (int)len );
   strcpy( loc_plot_size, str );
   StrCToFor( str, (int)len );
   if ( *ilands == 0 || *ilands == 1 )
     landscape_mode = *ilands;
}

/*-----------------------------------------------------------------*/

void
get_plotsize( char str[] )
{
   strcpy( str, loc_plot_size );
}

/*-----------------------------------------------------------------*/

int 
get_orientation()
{
   return landscape_mode;
}

/*-----------------------------------------------------------------*/

void
mkbackup_( char filnam[], char ext[], int *ierr, ftnlen flen, ftnlen elen )
{
   FILE *original_file;
   FILE *backup_file;
   char filename[FILE_NAME_LEN];
   char extension[EXTENSION_LEN];
   int nread;
   int nwrite;
   int k;

   
   /*
    * copy file name
    */
   StrForToC(filnam, flen); 
   strcpy(filename, filnam);
   StrCToFor(filnam, flen);
   

   /*
    * and extension
    */
   k=0;
   while (ext[k] != ' ' && ext[k] != '\0' && k < EXTENSION_LEN-1) {
      extension[k] = ext[k];
      k++;
   }
   extension[k] = '\0';
   

   /*
    * open the files
    */
   original_file = fopen( filename, "rb" );
   strcat( filename, extension );
   backup_file = fopen( filename, "wb" );

   /*
    * copy over the contents
    */
   clearerr( original_file );
   clearerr( backup_file );
   do {
      nread  = fread( buffer, sizeof(char), BUFFER_LENGTH, original_file );
      nwrite = fwrite(buffer, sizeof(char), nread,         backup_file );
      if ( nwrite != nread ) {
	 *ierr = 1;
	 break;
      }
   } while ( feof(original_file) == 0 );
   fclose( original_file );
   fclose( backup_file );

   *ierr = 0;
}

/*-----------------------------------------------------------------*/

void
dsptype_( int *mono )
{
   *mono = monochrome_display;
}

/*-----------------------------------------------------------------*/

void
runcmd_( char cmd[], ftnlen len )
{
   dispatch_(cmd, 
	     r, tr, ffta, phz,
             point, amp, wd, dmp, 
	     eps1, eps2, eps3, eps4, eps5, 
	     nit, nhold, ctag, dent, 
	     len, CTAG_LEN, DENT_LEN);
}

/*-----------------------------------------------------------------*/

void
keycmd_(int *ch, int *shift, int *ctrl, int *meta)
{
   gdispatch_(ch, shift, ctrl, meta,
	      r, tr, phz,
	      point, amp, wd, dmp, 
	      eps1, eps2, eps3, eps4, eps5, 
	      nit, nhold, ctag, dent, CTAG_LEN, DENT_LEN);
}

/*-----------------------------------------------------------------*/

void
rmove_(int *npts, float *src, float * dst)
{
   memcpy(dst, src, *npts * sizeof(float) );
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * -----------------
 * $Log: FortranWrappers.c,v $
 * Revision 1.26  1996/09/07 04:51:27  ulf
 * fixed bug in 'getenv_'. An empty value string made of blanks must be returned if an
 * environment variable was not found.
 *
 * Revision 1.25  1996/07/06 03:26:20  ulf
 * change name 'mkftstmp' --> 'mktmp' and use 'mktemp' function to
 * generate a unique temporary file name
 *
 * Revision 1.24  1996/03/14 16:46:22  ulf
 * array rtr does not need to be passed to Fortran part
 *
 * Revision 1.23  1996/03/13  17:48:30  ulf
 * string argument lengths in 'runcmd' were in wrong order
 *
 * Revision 1.22  1996/03/11  17:00:38  ulf
 * array iffta not needed any more
 *
 * Revision 1.21  1996/03/09  17:12:42  ulf
 * 'gdispatch' must also pass the phase array
 *
 * Revision 1.20  1996/03/08  16:48:24  ulf
 * eps? variables are needed in 'gdispatch'
 *
 * Revision 1.19  1996/03/06  17:49:34  ulf
 * reduced number of parameters in 'gdispatch'
 *
 * Revision 1.18  1996/03/03  13:52:07  ulf
 * new function 'keycmd'
 *
 * Revision 1.17  1996/03/02  15:56:14  ulf
 * added subroutine 'rumcmd', a version of 'dispatch' that can be
 * called from within Fortran from places without access to the
 * global arrays.
 *
 * Revision 1.16  1996/02/26  17:43:34  ulf
 * fixed bug in function 'getver'
 *
 * Revision 1.15  1996/02/26  17:42:06  ulf
 * fixed the type of xgremlin_version
 *
 * Revision 1.14  1995/11/11  11:28:36  ulf
 * cosmetic fix
 *
 * Revision 1.13  1995/11/11  10:28:29  ulf
 * new function get_orientation
 *
 * Revision 1.12  1995/11/11  10:24:47  ulf
 * change 'pltsize' command to allow to alter plot orientation
 *
 * Revision 1.11  1995/09/23  16:11:45  ulf
 * removed the attemps at the 'dialog' function
 *
 * Revision 1.10  1995/09/23  15:44:01  ulf
 * new function 'dsptype'
 *
 * Revision 1.9  1995/09/10  18:25:30  ulf
 * add new functions 'versinfo' and 'sysinfo'
 *
 * Revision 1.8  1995/09/06  22:52:28  ulf
 * &xgremlin_version must be cast to char *
 *
 * Revision 1.7  1995/09/06  22:47:30  ulf
 * gremlinVersion --> xgremlin_version
 *
 * Revision 1.6  1995/09/06  01:41:48  ulf
 * gremlinVersion is now defined extern
 *
 * Revision 1.5  1995/08/20  02:32:22  ulf
 * changed name of function  mkrtemp --> mkftstmp
 * and use a more general temp file name
 *
 * Revision 1.4  1995/08/16  02:43:35  ulf
 * add function 'mkrtmp'
 *
 * Revision 1.3  1995/07/21  02:14:34  ulf
 * add 'getpid_' function to obtain PID in Fortran
 *
 * Revision 1.2  1995/07/04  22:56:20  ulf
 * temporarily take out function 'dialog_' because dialog boxes are not
 * working yet.
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */
