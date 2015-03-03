/*
 * Turns all <lf>s in a file into <cr><lf> sequences.
 *
 * Author: Ulf Griesmann, 8/93
 *
 * Revision history:
 * -----------------
 * $Log: unix2dos.c,v $
 * Revision 1.1  1993/09/23  07:39:39  ulf
 * Initial revision
 *
 */

#include <stdlib.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
   FILE *dosfile;
   FILE *unixfile;
   int  idx;
   int  buf;
   char dosname[48];

#ifdef __EMX__
   /* 
    * under OS/2 and DOS wildcard expansion is not provided by the shell.
    */
   _wildcard( &argc, &argv );
#endif

   if ( argc == 1 ) {
      printf("Usage :  unix2dos  file1 [file2 ... fileN]\n");
      printf("         Wildcards in file names will be expanded.\n");
      exit( 1 );
   }

   for ( idx=1; idx < argc; idx++ ) {
      printf( "%s\n", argv[idx] );
      unixfile = fopen( argv[idx], "rb" );
      strcpy( dosname, "u2dXXXXX" );
      mktemp( dosname );
      dosfile = fopen( dosname, "wb" );
      while((buf = getc( unixfile ) ) != EOF ) {
	if (buf != '\n') 
	  putc(buf, dosfile);
	else {
	  putc( '\r', dosfile );
	  putc( '\n', dosfile );
	}
      }
      fclose( dosfile );
      fclose( unixfile );
      remove( argv[idx] );
      rename( dosname, argv[idx] );
   }
   return 0;
}




