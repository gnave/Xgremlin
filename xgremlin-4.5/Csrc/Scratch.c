/*
 * This program is part of Xgremlin
 *
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
 * $Id: Scratch.c,v 1.11 1996/08/08 02:28:15 ulf Exp $
 */

#include <X11/Intrinsic.h>     /* X toolkit intrinsics definitions */
#include <X11/StringDefs.h>    /* Standard Name-String definitions */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

#include "defaults.h"
#include "Scratch.h"
#include "Edit.h"
#include "Miscellaneous.h"

#define SCRATCH_FILE_VERSION  1


/*
 * a pointer to the r-array
 */
extern float *r;


/*
 * scratch directory file entry
 */
typedef struct {
   int version;           /* scratch file version */
   int key;               /* the scratch file number */
   int num_points;        /* number of points. -1: entry unused */
   int nwpp;              /* =1: real data   =2: complex data */
   double wref;           /* start wavenumber / dispersion of saved array */
   double delw;
   char id[MAX_LINE_LEN]; /* scratch file id */
   int unused[20];        /* some space for later */
} scratch_file_descriptor;


/*
 * in-memory scratch file database
 */
static scratch_file_descriptor scratch_db[ MAX_NUM_SCRATCH_FILES ];

/*
 * some important file names
 */
static char scratch_directory[FILE_NAME_LEN];

/*
 * data types
 */
static char num_type[2][5] = { "real","cplx" };

/*------------------ Local Prototypes ----------------------------*/

/*
 * prints out the contents of a scratch file descriptor
 */
static INLINE void
display_record( scratch_file_descriptor *d );


/*
 * build scratch file directory from scratch file headers
 */
static INLINE void
read_scratch_db();

/*-----------------------------------------------------------------*/

void
opensdir_( char dir_name[], ftnlen len )
{
   struct stat file_status;
   char message[MAX_LINE_LEN];

   /* 
    * cobble together full file names
    */
   memset( scratch_directory, ' ', FILE_NAME_LEN );
   strncpy( scratch_directory, dir_name, len );
   StrForToC( scratch_directory, len );
   if ( strlen(scratch_directory) == 0 )        /* use the default directory */
     {
	get_homedir( scratch_directory );
	strcat( scratch_directory, DEFAULT_SCRATCH_DIRECTORY );
	strcpy( dir_name, scratch_directory );  /* return directory */
	StrCToFor( dir_name, len );
     }

   /*
    * if directory file does not exit then make one
    */
   if ( stat( scratch_directory, &file_status ) == 0 ) 
     {
	/*
	 * check if file is a directory
	 */
	if ( !S_ISDIR( file_status.st_mode ) )
	  {
	     sprintf(message, 
		     " Error :  scratch directory  %s  is not a directory\n",
		     scratch_directory);
	     WriteStr( message );
	     return;
	  }
     }
   else
     {
	sprintf(message, 
		" Warning :  creating new scratch file directory  %s\n",
		scratch_directory);
	WriteStr( message );
	mkdir( scratch_directory, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH );
     }

   /*
    * now read the headers of all scratch files
    */
   read_scratch_db();
}

/*-----------------------------------------------------------------*/

void
lssdir_()
{
   int k;

   read_scratch_db();
   WriteStr(" key      nop  type            wref            delw   id\n");
   for ( k=0; k<MAX_NUM_SCRATCH_FILES; k++ )
     if ( scratch_db[k].num_points > 0 )
       display_record( &scratch_db[k] );
}

/*-----------------------------------------------------------------*/

void
rlowscr_( int *nums, int *npts, int *nwpp, double *wref, double *delw, float *arr,
	  char memo[], ftnlen len)
{
   FILE *f;
   char message[MAX_LINE_LEN];
   char scratch_file_name[FILE_NAME_LEN];
   int key;
   int num;
   int npoints;

   key = *nums;

   /*
    * check if file key is o.k.
    */
   if ( key >= MAX_NUM_SCRATCH_FILES )
     {
	sprintf(message, " Error :  scratch file number must be between 0 and %d\n",
		MAX_NUM_SCRATCH_FILES-1);
	WriteStr(message);
	return;
     }
   
   /*
    * open the scratch file
    */
   sprintf(scratch_file_name, "%s/scratch.%d", 
	   scratch_directory, key );
   f = fopen( scratch_file_name, "r" );
   if ( f == NULL )
     {
	sprintf(message,
		" Error :  cannot open scratch file  %s\n", scratch_file_name );
	WriteStr(message);
	/*
	 * correct database
	 */
	scratch_db[key].num_points = -1;
	return;
     }

   /*
    * read header section first, then data
    */
   if ( fread( &scratch_db[key], sizeof(scratch_file_descriptor), 1, f ) != 1 )
     {
	WriteStr(" Error :  cannot read scratch file header.");
	return;
     }

   if ( *npts == -1 )
     {
	npoints =  scratch_db[key].num_points;
	*npts = npoints;
     }
   else
     {
	npoints = *npts;
	if ( npoints > scratch_db[key].num_points )
	  {
	     npoints = scratch_db[key].num_points;
	     sprintf(message,
		     " Warning :  can read only  %d  points from scratch file\n",
		     npoints);
	     WriteStr( message );
	  }
     }
   *nwpp = scratch_db[key].nwpp;
   *wref = scratch_db[key].wref;
   *delw = scratch_db[key].delw;
   strcpy( memo, scratch_db[key].id );
   StrCToFor( memo, len );
   num = fread( arr, sizeof(float), npoints * scratch_db[key].nwpp, f);
   fclose(f);
   if ( num != npoints * scratch_db[key].nwpp )
	WriteStr(" Error :  failure when reading scratch file\n");

}

/*-----------------------------------------------------------------*/

void
wlowscr_( int *nums, int *npts, int *nwpp, double *wref, double *delw, float *arr,
	  char memo[], ftnlen len)
{
   FILE *f;
   char message[MAX_LINE_LEN];
   char scratch_file_name[FILE_NAME_LEN];
   scratch_file_descriptor sfd;
   scratch_file_descriptor sfd_aux;
   int num;

   /*
    * check if file key is o.k.
    */
   if ( *nums >= MAX_NUM_SCRATCH_FILES )
     {
	sprintf(message, " Error :  scratch file number must be between 0 and %d\n",
		MAX_NUM_SCRATCH_FILES-1);
	WriteStr(message);
	return;
     }

   /*
    * assemble a scratch file header
    */
   sfd.version = SCRATCH_FILE_VERSION;
   sfd.key = *nums;  
   sfd.num_points = *npts;
   sfd.nwpp = *nwpp;
   sfd.wref = *wref;
   sfd.delw = *delw;
   strncpy( sfd.id, memo, len );
   StrForToC( sfd.id, len );
   scratch_db[sfd.key] = sfd;

   /*
    * WARNING: at this point we should use file locking to make sure that
    * there can never be two Xgremlins modifying a scratch file
    * simultaneously. 
    */
   sprintf(scratch_file_name, "%s/scratch.%d", 
	   scratch_directory, sfd.key );

   /*
    * if the id string is empty, try to retain old id string
    */
   if ( strlen(sfd.id) == 0 )
     {
       f = fopen(scratch_file_name, "r");
       if ( f != NULL )  /* old scratch file exists */
	 {
	   fread( &sfd_aux, sizeof(scratch_file_descriptor), 1, f);
	   fclose(f);
	   strcpy(sfd.id, sfd_aux.id);
	 }
     }
   f = fopen( scratch_file_name, "w" );
   if ( fwrite( &sfd, sizeof(scratch_file_descriptor), 1, f ) != 1 )
     {
	WriteStr(" Error :  could not write scratch file header.");
	return;
     }
   num = fwrite( arr, sizeof(float), sfd.num_points * sfd.nwpp, f );
   fclose(f);
   if ( num != sfd.num_points * sfd.nwpp ) 
     {
	WriteStr(" Error :  failed to write scratch file\n");
	sfd.num_points = -1;  /* mark record as empty */
     }
}

/*-----------------------------------------------------------------*/

void
read8kr_( int *nums, int *nwpp, double *wref, double *delw,
	  char memo[], ftnlen len)
{
   int npts;

   npts = 8192;
   rlowscr_( nums, &npts, nwpp, wref, delw, r, memo, len);
}

/*-----------------------------------------------------------------*/

void
write8kr_( int *nums, int *nwpp, double *wref, double *delw,
	  char memo[], ftnlen len)
{
   int npts;

   npts = 8192;
   wlowscr_(nums, &npts, nwpp, wref, delw, r, memo, len);
}

/*-----------------------------------------------------------------*/

void
rmscr_( int *key )
{
   char message[MAX_LINE_LEN];
   char scratch_file_name[FILE_NAME_LEN];

   /*
    * check if file key is o.k.
    */
   if ( *key >= MAX_NUM_SCRATCH_FILES )
     {
	sprintf(message, " Error :  scratch file number must be between 0 and %d\n",
		MAX_NUM_SCRATCH_FILES-1);
	WriteStr(message);
	return;
     }

   /*
    * remove it
    */
   sprintf(scratch_file_name, "%s/scratch.%d", 
	   scratch_directory, *key );
   unlink( scratch_file_name );
   scratch_db[*key].num_points = -1;
}

/*-----------------------------------------------------------------*/


static INLINE void
display_record( scratch_file_descriptor *d )
{
   char line[MAX_LINE_LEN];

   sprintf(line," %3d  %7d  %4s  %14.6g  %14.6g   %s\n", 
	   d->key, d->num_points, num_type[(d->nwpp)-1], 
	   d->wref, d->delw, d->id );
   WriteStr( line );
}

/*-----------------------------------------------------------------*/

static INLINE void
read_scratch_db()
{
   /*
    * this function finds all scratch files in the scratch directory
    * and reads the scratch file headers into memory
    */
   DIR *s_dir;
   struct dirent *dp;
   FILE *f;
   char fname[FILE_NAME_LEN];
   scratch_file_descriptor sfd;
   int k;

   /*
    * reset a few scratch file variables
    */
   for ( k=0; k<MAX_NUM_SCRATCH_FILES; k++ )
     scratch_db[k].num_points = -1;

   /*
    * open the scratch directory
    */
   s_dir = opendir( scratch_directory );
   if ( s_dir == NULL )
     return;

   /*
    * look up all scratch files
    */
   while ( (dp = readdir(s_dir)) != NULL )
     {
	if ( strncmp( dp->d_name, "scratch.", 8 ) != 0 )
	  continue;

	sprintf( fname, "%s/%s", scratch_directory, dp->d_name );
	f = fopen( fname, "r" );   /* read the header */
	if ( fread( &sfd, sizeof(scratch_file_descriptor), 1, f ) != 1 )
	  {
	     WriteStr(" Error :  could not read scratch file header");
	     return;
	  }
	fclose(f);

	scratch_db[sfd.key] = sfd;
     }
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * -----------------
 * $Log: Scratch.c,v $
 * Revision 1.11  1996/08/08 02:28:15  ulf
 * Ooops. Now it's working.
 *
 * Revision 1.10  1996/08/08 02:05:55  ulf
 * modified function for writing a scratch file so that it retains the old
 * id string if no now id string is typed in. This is identical to the
 * behaviour of Gremlin.
 *
 * Revision 1.9  1996/03/09 17:05:27  ulf
 * fixed a typo
 *
 * Revision 1.8  1996/03/09  17:05:06  ulf
 * new functions 'write8kr' and 'read8kr' which are used in
 * the subroutines which emulate old scratch file behaviour
 *
 * Revision 1.7  1996/02/26  17:50:09  ulf
 * forgot to write error messages to the text window in a few places
 *
 * Revision 1.6  1996/01/23  14:49:12  ulf
 * fix up some dangling comments which were left over from before
 * code re-organisation
 *
 * Revision 1.5  1996/01/09  04:31:17  ulf
 * fixed a bug in read_scratch_db
 *
 * Revision 1.4  1996/01/08  15:07:12  ulf
 * got rid of the scratch file database, use scratch file headers instead because
 * this is simpler and also faster.
 *
 * Revision 1.3  1995/10/25  14:10:47  ulf
 * fixed a typo
 *
 * Revision 1.2  1995/10/25  13:38:46  ulf
 * Warning message if number of points recalled exceeds number of points
 * in the scratch file.
 *
 * Revision 1.1  1995/10/25  13:21:29  ulf
 * Initial revision
 *
 */
