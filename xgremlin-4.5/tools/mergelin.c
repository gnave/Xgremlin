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
 * program to merge two or more Gremlin linelist (.lin) files
 *
 * $Id: mergelin.c,v 1.16 1996/09/27 04:39:49 ulf Exp $
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#if defined( NEED_UNISTD_H )
#include <unistd.h>
#endif

#include "linelist.h"
#include "List.h"
#include "byteorder.h"
#include <math.h>

/*
 * for getopt
 */
extern int optind;
extern char *optarg;
 
#define HDR_SIZE  320

/*
 * some globals
 */
double threshold = 0.005;     /* merge threshold */
double sigma_mbx;

/*----------------------- local prototypes ------------------------*/

static int
find_line( gremlin_linelist_record *line );

static int
line_comp( gremlin_linelist_record *line1, gremlin_linelist_record *line2 );

/*-----------------------------------------------------------------*/

int
main( int argc, char *argv[] )
{
   FILE *lin_file;
   FILE *out_file;
   char ch;
   int iopt;
   char lin_name[256];
   char out_name[256];
   gremlin_linelist_header hdr;
   gremlin_linelist_record rec;
   tList linelist;
   int num_lines = 0;            /* number of lines in list */
   int num_data;
   int num_added;
   int obj_size;                 
   int ret_val = 0;              /* error counter */
   int endian_ness;              
   int lines_total = 0;
   int i, idx;
   int zero = 0;

   if ( argc == 1 ) /* there is nothing on the command line */
     {
	printf("\n");
	printf("Version:  Feb 6, 96\n");
        printf("Usage  :  mergelin -o <output file> [ -t xxx.x ] <.lin files> \n");
        printf("Example:  mergelin -o k11sh.lin  k11sh_a.lin k11sh_b.lin\n");
	printf("Options:  -o <output file>  this option is required.\n");
        printf("          -t xxx.x  if a line is closer than xxx.x wavenumbers to\n");
        printf("                    a line already in the output list it will not be\n");
        printf("                    added to the output list. default: 0.005 (5mK)\n\n");
        exit( 1 );
     }

   /*
    * store byte order for fast access
    */
   endian_ness = byte_order();

   /*
    * check options with getopt
    */
   while( (iopt = getopt(argc, argv, "o:t:")) != -1)
     {
	ch = (char)iopt;
        switch (ch)
          {
           case 'o':
	     strcpy( out_name, optarg );
             break;

           case 't':
	     sscanf( optarg, "%f", &threshold );
             break;

           case '?':
             printf("\nError :  unknown option on command line\n\n");
             exit( -1 );
             break;
          }
     }
 
   if ( out_name[0] == '\0' )
     {
	printf("\nError :  no output file name; use -o option.\n\n");
	exit( -1 );
     }

   /*
    * create the list of all lists
    */
   create_list( &linelist );


   for ( idx=optind; idx < argc; idx++ )
     {
	/*
	 * put together file name and open file
	 */
	strcpy( lin_name, argv[idx] ); 
	if ( strstr( lin_name, ".lin" ) == NULL )
	  strcat( lin_name, ".lin" );

        if ( (lin_file = fopen( lin_name, "rb" )) != NULL )
	  {
	     printf( "reading file  %s :", lin_name );

	     /*
	      * read header and byte reverse if required
	      */
	     obj_size = fread( &hdr, sizeof(gremlin_linelist_header), 1, lin_file );
	     if ( obj_size != 1 )
	       {
		  printf("  unable to read header, skipped.\n" );
		  fclose( lin_file );
		  ret_val++;
		  continue;
	       }
	     num_lines = hdr.num_lines;
	     if ( endian_ness == BIG_ENDIAN )
	       byte_reverse_int( &num_lines );
	     printf("   %4d lines", num_lines);

	     /*
	      * skip to data, read in records and store them in list
	      */
	     num_added = 0;
             fseek( lin_file, 320L, SEEK_SET );
	     for ( i=0; i<num_lines; i++ )
	       {
                  obj_size = fread( &rec, sizeof(gremlin_linelist_record), 1, lin_file );
                  if ( obj_size != 1 )
                    {
		       printf("  record %d read failed, aborted.\n", i+1 );
                       ret_val++;
		       exit( ret_val );
		    }

		  /*
		   * byte reverse wavenumber for comparisons
		   */
		  if ( endian_ness == BIG_ENDIAN )
		    byte_reverse_double( &rec.sig );

		  /*
		   * now check if line is in the list of lists already
		   * and if not then insert it
		   */
		  sigma_mbx = rec.sig;
		  /* 
		   * the following line relies on short circuit evaluation 
		   */
		  if ( threshold == 0.0 || 
		       search_list(linelist,SEARCH_PROC(find_line))==NULL )
		    {
		       sorted_list_insert_object( linelist, &rec,
						  sizeof(gremlin_linelist_record),
                                                  SORT_PROC(line_comp) );
		       num_added++;
		    }
	       }
	     printf(",   added %4d\n", num_added );
	     fclose( lin_file );
	  }
     }

   /*
    * now create new .lin file and write all lines to disk
    */
   printf("Creating output file  %s :  ", out_name );
   out_file = fopen( out_name, "wb" );
   num_lines = list_entries( linelist );
   num_data = num_lines * sizeof(gremlin_linelist_record) + HDR_SIZE;
   if ( endian_ness == BIG_ENDIAN )
     {
	byte_reverse_int( &num_lines );
	byte_reverse_int( &num_data );
     }

   /*
    * write a new header
    */
   fwrite( &num_lines, sizeof(int), 1, out_file );
   fwrite( &num_data, sizeof(int), 1, out_file );
   for ( i=0; i<78; i++ )
     fwrite( &zero, sizeof(int), 1, out_file );
    
   /*
    * write out the line records
    */
   num_lines = list_entries( linelist );
   printf( "%d lines\n", num_lines );
   list_head( linelist );
   for ( i=0; i<num_lines; i++ )
     {
	get_current_object( linelist, &rec, sizeof(gremlin_linelist_record) );
	if ( endian_ness == BIG_ENDIAN )
	  byte_reverse_double( &rec.sig );	  
	obj_size = fwrite( &rec, sizeof(gremlin_linelist_record), 1, out_file );
	if ( obj_size != 1 )
	  {
	     printf("\nError :  could not write record to output file.\n\n");
	     exit( -1 );
	  }
     }
   fclose( out_file );
   erase_list_entries( linelist );
   delete_list( &linelist );

   return ret_val;
}

/*-----------------------------------------------------------------*/

static int
find_line( gremlin_linelist_record *line )
{
   if ( fabs( sigma_mbx - line->sig ) < threshold )
     return 1;
   else
     return 0;
}

/*-----------------------------------------------------------------*/

static int
line_comp( gremlin_linelist_record *line1, gremlin_linelist_record *line2 )
{
   if ( line1->sig > line2->sig )
     return 1;
   if ( line1->sig < line2->sig )
     return -1;
   return 0;
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * -----------------
 * $Log: mergelin.c,v $
 * Revision 1.16  1996/09/27 04:39:49  ulf
 * change order fo file inclusion
 *
 * Revision 1.15  1996/09/27 04:24:13  ulf
 * replace BIG --> BIG_ENDIAN
 *
 * Revision 1.14  1996/02/06 14:47:55  ulf
 * corrected the program usage message. The -o option has to go before input
 * file names.
 *
 * Revision 1.13  1996/01/13  16:14:04  ulf
 * print out a version date if nothing is on the command line
 *
 * Revision 1.12  1995/11/05  17:34:12  ulf
 * simplify insertion code
 *
 * Revision 1.11  1995/11/04  17:45:34  ulf
 * make it compile clean under AIX
 *
 * Revision 1.10  1995/11/04  15:23:16  ulf
 * modify to use AT&T's getopt instead of the GNU version which causes trouble
 * under AIX.
 *
 * Revision 1.9  1995/10/26  09:17:30  ulf
 * use define HAVE_GETOPT
 *
 * Revision 1.8  1995/10/04  06:42:16  ulf
 * added 'getopt' for SUNOS
 *
 * Revision 1.7  1995/10/03  14:59:45  ulf
 * if threshold == 0 then put all lines into the merged line list
 *
 * Revision 1.6  1995/09/30  16:47:22  ulf
 * remove hardcoding of record size
 *
 * Revision 1.5  1995/09/30  16:17:12  ulf
 * fixed a bug (line_list --> linelist)
 *
 * Revision 1.4  1995/09/30  14:29:50  ulf
 * cosmetic changes to messages.
 *
 * Revision 1.3  1995/09/30  04:08:18  ulf
 * remove list from memory before returning from 'main'
 *
 * Revision 1.2  1995/09/30  04:00:20  ulf
 * fixed a couple of bugs and make it work
 *
 * Revision 1.1  1995/09/29  02:31:22  ulf
 * Initial revision
 *
 */



















