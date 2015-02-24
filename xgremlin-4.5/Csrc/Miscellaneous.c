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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

#include "Miscellaneous.h"
#include "defaults.h"

#define HOST_NAME_LEN    64
#define DOMAIN_NAME_LEN  128


char lock_name[FILE_NAME_LEN];   /* name of lock for scratch file */
char home_dir[FILE_NAME_LEN];    /* home directory */


/*-----------------------------------------------------------------*/

/*
 * compare strings to build sorted directory listing
 */
int
sort_name( char s1[], char s2[] );

/*
 * try to obtain file lock for scratch file or check if it is ours
 */
int
scratch_lock();

/*-----------------------------------------------------------------*/

int 
GetFQDN( char fqdn[] ) 
{
   int error;

   char hostName[HOST_NAME_LEN];
   char domainName[DOMAIN_NAME_LEN];

   error = gethostname( hostName, HOST_NAME_LEN );
   if ( error  == -1 )  /* give up if host name was not found */
     return( -1 );
   strcpy( fqdn, hostName );

   error = getdomainname( domainName, DOMAIN_NAME_LEN );
   if ( error != -1 ) 
     {
	strcat( fqdn, "." );
	strcat( fqdn, domainName );
	return( 0 );
     }
   else 
      return( -2 );     /* domain name is unknown */

}

/*-----------------------------------------------------------------*/

int 
GetHN( char host[] ) 
{
   return gethostname( host, HOST_NAME_LEN );
}

/*-----------------------------------------------------------------*/

void
get_homedir( char dir[] )
{
   struct passwd *pw;

   if ( home_dir[0] == '\0' )
     {
	pw = getpwuid( getuid() );  /* get the user database structure */
	if (pw == NULL)
	  {
	     dir[0] = '\0';
	  }
	else
	  {
	     strcpy( dir, pw->pw_dir );
	     strcpy( home_dir, pw->pw_dir );
	  }
     }
   else
     {
	strcpy( dir, home_dir );
     }
}

/*-----------------------------------------------------------------*/

void 
Syntax() 
{
   fprintf( stderr, "\nError :  unrecognizable command line option.\n\n" );
   fprintf( stderr, "Options:\n" );
   fprintf( stderr, "   -display  <[host.domain]:display.screen>\n" );
   fprintf( stderr, "   -geometry <HorizontalxVertical>\n" );
   fprintf( stderr, "   -monochrome\n" );
}

/*-----------------------------------------------------------------*/

void 
Abort( char msg[] ) 
{
   fprintf( stderr, "%s\n", msg );
   exit( 0 );
}

/*-----------------------------------------------------------------*/

INLINE char 
Chop( char s[] ) 
{
   int k;
   
   k = strlen( s );
   if (k>0) 
     {
	s[k-1] = '\0';
	return s[k];
     }
   else
     return '\0';
}

/*-----------------------------------------------------------------*/

INLINE void
remove_trailing_blanks( char s[] )
{
  int len;

  len = strlen(s);
  if ( len == 0 )
    return;

  StrForToC(s, len);
}

/*-----------------------------------------------------------------*/

INLINE void 
StrForToC( char str[], int len ) 
{
   int i;
   
   for (i=len-1; i>=0; i--)
     if (str[i] != ' ')
       break;
   if (i<len-1) 
     str[++i] = '\0';
}

/*-----------------------------------------------------------------*/

INLINE int 
StrCToFor( char str[], int len ) 
{
   int i,k;
   
   i = strlen( str );
   str[i] = ' '; /* overwrite \0 */
   for ( k=i+1; k<len; k++ )
     str[k] = ' ';
   return( i );
}

/*-----------------------------------------------------------------*/

INLINE int
in_interval( int a, int b, int x )
{
   register int min;
   register int max;

   min = a < b ? a : b;
   max = a > b ? a : b;
   if ( x >= min && x <= max )
     return TRUE;
   else
     return FALSE;
}

/*-----------------------------------------------------------------*/

void
current_dir_files( tList files )
{
   DIR *dir_stream;
   struct dirent *dp;
   struct stat file_status;
   char tmp_name[FILE_NAME_LEN];

   /*
    * clear the list of files
    */
   erase_list_entries( files );

   /*
    * open directory stream
    */
   dir_stream = opendir( "./" );
   if ( dir_stream == NULL )
     return;

   /*
    * read the directory, mark directory files with [ ]
    */
   while ( (dp = readdir(dir_stream)) != NULL )
     {
	if ( *dp->d_name == '.' )   /* skip all dot files */
	  continue;

	stat( dp->d_name, &file_status );         /* find out file type */
	if ( S_ISDIR( file_status.st_mode ) )    /* it's a directory */
	  sprintf( tmp_name, "[ %s ]", dp->d_name );
	else
	  strcpy( tmp_name, dp->d_name );

	sorted_list_insert_object( files, tmp_name, FILE_NAME_LEN, SORT_PROC(sort_name));
     }
   (void)closedir( dir_stream );
}

/*-----------------------------------------------------------------*/

int
sort_name( char s1[], char s2[] )
{
   return strcmp( s1, s2 );
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * ----------------
 * $Log: Miscellaneous.c,v $
 * Revision 1.12  1996/09/07 04:27:25  ulf
 * new function 'remove_trailing_blanks'
 *
 * Revision 1.11  1996/04/22 01:47:49  ulf
 * moved functions 'gmalloc' and 'grealloc' to file arralloc.c
 *
 * Revision 1.10  1996/02/04 16:17:13  ulf
 * new functions 'gmalloc' and 'grealloc' which write an error message
 * if memory allocation fails.
 *
 * Revision 1.9  1996/01/24  14:12:59  ulf
 * fixed a typo
 *
 * Revision 1.8  1996/01/24  14:12:05  ulf
 * do not include any dot files in the directory listing
 *
 * Revision 1.7  1995/11/21  15:45:46  ulf
 * inline more functions
 *
 * Revision 1.6  1995/10/20  15:06:08  ulf
 * cosmetic fix in 'current_dir_files'
 *
 * Revision 1.5  1995/09/22  23:32:46  ulf
 * added 'monochrome' option
 *
 * Revision 1.4  1995/08/24  21:17:02  ulf
 * removed scratch file locking again because of brain damage.
 *
 * Revision 1.3  1995/08/22  23:25:35  ulf
 * return status in 'unlock' function.
 *
 * Revision 1.2  1995/08/22  23:12:10  ulf
 * add file locking functions for the scratch file
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */






