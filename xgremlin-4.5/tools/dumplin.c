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
 * simple program to dump Xgremlin (or Gremlin) linelist files (.lin) in ASCII
 * also checks consistency of header with data and repairs inconsistent headers
 *
 * $Id: dumplin.c,v 1.29 1996/09/27 04:23:36 ulf Exp $
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

/*
 * for getopt
 */
extern int optind;

/*---------------------- local prototypes -------------------------*/

static int
line_comp( gremlin_linelist_record *line1, gremlin_linelist_record *line2 );

/*-----------------------------------------------------------------*/

/*
 * main program
 */
int
main( int argc, char *argv[] )
{
   FILE *lin_file;
   FILE *truncate_file;
   FILE *ascii_file;
   char lin_name[256];
   char truncate_name[256];
   char ascii_name[256];
   int idx;
   int i, k;
   int iopt;
   int obj_nums;
   int bytes_found;
   int num_records;
   int truncate_flag = 0;
   int hdr_flag = 0;
   int std_flag = 0;
   int flag_set;
   int ret_val = 0;
   char tag[8];
   char id[36];
   char *cp;
   int zero = 0;
   int endian_ness;
   char ch;
   gremlin_linelist_header hdr;
   gremlin_linelist_record rec;
   tList linelist;

   if ( argc == 1 )
     {
	printf("\n");
        printf("Version:  Jan 14, 96\n");
        printf("Usage  :  dumplin [-tsh] <.lin files>\n");
        printf("Example:  dumplin k11sh2 neii\n");
	printf("Options:  -t    remove trailing invalid line records\n" );
	printf("          -s    write output to  stdout\n" );
	printf("          -h    print out header information\n\n" );
        exit( 1 );
     }

   endian_ness = byte_order();

   /*
    * check options 
    */
   while( (iopt = getopt(argc, argv, "tsh")) != -1)
     {
	ch = (char)iopt;
	switch (ch)
	  {
	   case 't':
	     truncate_flag = 1;
	     break;
	   case 'h':
	     hdr_flag = 1;
	     break;
	   case 's':
	     std_flag = 1;
	     break;
	   case '?':
	     printf("\nError :  unknown option on command line\n\n");
	     exit( -1 );
	     break;
	  }
     }   
   flag_set = truncate_flag + hdr_flag;

   /*
    * if truncate_flag is set we need a line list
    */
   if ( truncate_flag == 1 )
     create_list( &linelist );

   /*
    * no do all the files on the command line
    */
   for ( idx=optind; idx < argc; idx++ )
     {
	strcpy( lin_name, argv[idx] );  /* put together file name */
	if ( ( cp = strstr( lin_name, ".lin" ) ) != NULL )
	  *cp = '\0';                   /* chop off extension(s) */
	strcat( lin_name, ".lin" );
	printf( "file  %s", lin_name );
        if ( (lin_file = fopen( lin_name, "rb" )) != NULL )
	  {
	     if ( flag_set == 0 )
	       {
		  if ( std_flag == 1 )
		    ascii_file = stdout;
		  else
		    {
		       printf( "\n" );
		       strcpy( ascii_name, lin_name );
		       strcat( ascii_name, ".lst" );
		       ascii_file = fopen( ascii_name, "w" );
		    }
	       }

	     /*
	      * first read the file header
	      */
	     obj_nums = fread( &hdr, sizeof(gremlin_linelist_header), 1, lin_file );
	     if ( obj_nums != 1 )
	       {
		  printf("\nError :  header of file  %s  is unreadable\n\n", lin_name );
		  if ( flag_set == 0 )
		    if ( std_flag == 0 )
		      fclose( ascii_file );
		  fclose( lin_file );
		  ret_val++;
		  continue;
	       }
	     /*
	      * byte reverse the header if necessary
	      */
	     if ( endian_ness == BIG_ENDIAN ) 
	       {
		  byte_reverse_int( &hdr.num_lines );
		  byte_reverse_int( &hdr.data_size );
	       }

	     if ( flag_set == 0 )
	       {
		  fprintf( ascii_file, "   Number of lines    : %d\n", hdr.num_lines );
		  fprintf( ascii_file, "   Data bytes in file : %d\n", hdr.data_size );
	       }
	     else 
	       {
		  if ( hdr_flag == 1 )
		    {
		       printf( "\n" );
		       printf( "Number of lines    : %d\n", hdr.num_lines );
		       printf( "Data bytes in file : %d\n", hdr.data_size );
		       fclose( lin_file );
		       continue;
		    }
	       }

	     /*
	      * skip over first 320 bytes and read all line records
	      */
	     fseek( lin_file, 320L, SEEK_SET );
	     num_records = 0;
	     for ( i=0; i<hdr.num_lines; i++) /* read all line records */
	       {
		  obj_nums = fread( &rec, sizeof(gremlin_linelist_record), 1, lin_file );
		  if ( obj_nums != 1 )
		    {
		       printf("\nError :  could not read all line records\n");
		       exit( -1 );
		    }
		  else
		    {
		       num_records++;
		       if ( truncate_flag == 1 )  /* put the record in a sorted list */
			 {
			    if ( endian_ness == BIG_ENDIAN )
			      byte_reverse_double( &rec.sig );
			    sorted_list_insert_object(linelist, &rec,
						      sizeof(gremlin_linelist_record),
						      SORT_PROC(line_comp));
			 }
		    }

		  /*
		   * write out in text format
		   */
		  if ( flag_set == 0 )
		    {
		       if ( endian_ness == BIG_ENDIAN )
			 {
			    byte_reverse_double( &rec.sig );
			    byte_reverse_float( &rec.xint );
			    byte_reverse_float( &rec.width );
			    byte_reverse_float( &rec.dmping );
			    byte_reverse_short( &rec.itn );
			    byte_reverse_short( &rec.ihold );
			 }
		       strncpy( tag, rec.tags, 4 );
		       tag[4] = '\0';
		       strncpy( id, rec.ident, 32 );
		       for ( k=31; k>=0; k-- )
			 if (id[k] != ' ') 
			   break;
		       id[k+1] = '\0';
		       if ( k == 0 )
			 id[0] = '\0';
		       fprintf( ascii_file, 
    	               " %4d: %13.5lf  %14.3f  %8.3f  %6.2f  %+3d  %+3d  %4s  %s\n",
                       num_records, rec.sig, rec.xint, rec.width, rec.dmping,
                       (int)rec.itn, (int)rec.ihold, tag, id );
		    }
	       }

	     if ( flag_set == 0 )
	       {
		  if ( std_flag == 0 )
		    fclose(ascii_file);
	       }
	     else
	       {
		  bytes_found = 80 * num_records + 320;

		  if ( truncate_flag == 1 )
		    {
		       /*
			* first write the new file header
			*/
		       strcpy( truncate_name, lin_name );
		       strcat( truncate_name, ".trc" );
		       truncate_file = fopen( truncate_name, "w" ); /* open for writing */
		       if ( endian_ness == BIG_ENDIAN ) 
			 {
			    byte_reverse_int( &num_records );
			    byte_reverse_int( &bytes_found );
			 }
		       fwrite( &num_records, sizeof(int), 1, truncate_file );
		       fwrite( &bytes_found, sizeof(int), 1, truncate_file );
		       for ( i=0; i<78; i++ )
			 fwrite( &zero, sizeof(int), 1, truncate_file );
		       
		       /*
			* now write out the sorted line list records
			*/
		       list_head( linelist );
		       for ( i=0; i<list_entries(linelist); i++ )
			 {
			    get_current_object(linelist, &rec, 
					       sizeof(gremlin_linelist_record) );
			    if ( endian_ness == BIG_ENDIAN )
			      byte_reverse_double( &rec.sig );
			    obj_nums = fwrite(&rec, sizeof(gremlin_linelist_record), 1, 
					      truncate_file);
			    if ( obj_nums != 1 )
			      {
				 printf("\nError :  could not write output file\n");
				 exit( -1 );
			      }
			 }
		       fclose( truncate_file );

		       /*
			* write out a summary of what was done
			*/
		       printf("\nLine list file  %s  truncated\n", lin_name);
		       printf("Number of lines      : %d\n", num_records );
		       printf("Data bytes in file   : %d\n", bytes_found );

		       /*
			* clear line list for next file
			*/
		       erase_list_entries( linelist );
		    }
	       }
	     fclose( lin_file );
	  }
     }
   return ret_val;
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
 * $Log: dumplin.c,v $
 * Revision 1.29  1996/09/27 04:23:36  ulf
 * change BIG --> BIG_ENDIAN
 *
 * Revision 1.28  1996/08/30 03:52:03  ulf
 * fixed argument type of 'byte_reverse_float' to match fixed prototype.
 *
 * Revision 1.27  1996/01/17 14:04:09  ulf
 * use a %13.5lf format to write out the wavenumber
 *
 * Revision 1.26  1996/01/14  14:50:34  ulf
 * there was a fundamental flaw in dumplin so far: the program read as many line
 * records from the line list file as it could. dumplin now reads only as many line
 * records as there are according to the file header because trailing line records
 * belong to deleted lines which should not be returned to the line list. There is
 * a new option to remove the invalid line records from the file (-t).
 *
 * Revision 1.25  1996/01/13  16:59:32  ulf
 * improved the -f (fix file) option. If line list records are out of order in
 * the line list file they will be ordered correctly in the fixed line list
 * file. The file header is updated too.
 *
 * Revision 1.24  1995/11/04  17:42:49  ulf
 * make it compile clean under AIX.
 *
 * Revision 1.23  1995/11/04  15:30:22  ulf
 * line number was not printed out correctly, fixed.
 *
 * Revision 1.22  1995/11/04  15:18:59  ulf
 * modify to use AT&T getopt because the GNU version causes trouble under AIX.
 *
 * Revision 1.21  1995/11/04  14:51:15  ulf
 * somewhat better handling of .lin file name extension
 *
 * Revision 1.20  1995/10/31  15:07:59  ulf
 * use the extension .lst for output files instead of .txt
 *
 * Revision 1.19  1995/10/31  10:25:53  ulf
 * allow more digits for the intensity
 *
 * Revision 1.18  1995/10/26  15:12:50  ulf
 * fix info message
 *
 * Revision 1.17  1995/10/26  15:01:04  ulf
 * the new option 's' must be included in 'getopt' too ...
 *
 * Revision 1.16  1995/10/26  14:57:02  ulf
 * new switch  -s  to write line list to std output
 *
 * Revision 1.15  1995/10/26  14:47:09  ulf
 * make fields for itn and ihold a bit wider
 *
 * Revision 1.14  1995/10/26  14:46:16  ulf
 * make sure output file has name *.lin.txt
 *
 * Revision 1.13  1995/10/26  14:37:17  ulf
 * fixed two typos
 *
 * Revision 1.12  1995/10/26  14:16:13  ulf
 * write out id string and properly terminate tag and id.
 *
 * Revision 1.11  1995/10/26  11:11:29  ulf
 * appended one '.lin' too many to create the text file name
 *
 * Revision 1.10  1995/10/26  09:18:33  ulf
 * use define HAVE_GETOPT
 *
 * Revision 1.9  1995/10/04  06:41:34  ulf
 * added 'getopt' for SUNOS
 *
 * Revision 1.8  1995/09/29  03:02:44  ulf
 * forgot most of the byte reversing (before writing out data in ASCII ...
 *
 * Revision 1.7  1995/09/29  02:41:10  ulf
 * one more significant figure in wavenumber
 *
 * Revision 1.6  1995/09/29  02:38:15  ulf
 * cosmetic fix.
 *
 * Revision 1.5  1995/09/29  00:42:23  ulf
 * changed the name of the program to 'dumplin'
 *
 * Revision 1.4  1995/09/28  02:55:12  ulf
 * use 'getopt' to read the command arguments
 *
 * Revision 1.3  1995/09/28  01:05:17  ulf
 * make program runnable on big endian machines
 *
 * Revision 1.2  1995/09/21  03:28:14  ulf
 * add options to dump header only, check the .lin file and fix the .lin file
 *
 * Revision 1.1  1995/09/20  19:21:35  ulf
 * Initial revision
 *
 */



















