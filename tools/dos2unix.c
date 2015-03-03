/*
 * Turns all <cr><lf> sequences in a file into <lf>.
 *
 * Author: Ulf Griesmann, 8/93
 *
 * Revision history:
 * -----------------
 * $Log: dos2unix.c,v $
 * Revision 1.2  1993/10/26  22:51:11  ulf
 * check if dos file exists before converting <cr><lf> combo.
 *
 * Revision 1.1  1993/09/04  15:50:07  ulf
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
   char unixname[48];

#ifdef __EMX__
   /* 
    * under OS/2 and DOS wildcard expansion is not provided by the shell.
    */
   _wildcard( &argc, &argv );
#endif

   if ( argc == 1 ) {
      printf("Usage :  dos2unix  [-f] file1 [file2 ... fileN]\n");
      printf("         Wildcards in file names will be expanded.\n");
      exit( 1 );
   }

   for ( idx=1; idx < argc; idx++ ) {
      printf( "%s\n", argv[idx] );
      if ( dosfile = fopen( argv[idx], "rb" ) ) {
	 strcpy( unixname, "d2uXXXXX" );
	 mktemp( unixname );
	 unixfile = fopen( unixname, "wb" );
	 while((buf = getc( dosfile ) ) != EOF ) {
	    if (buf !=  '\r')
	      putc( buf, unixfile );
	 }
	 fclose( dosfile );
	 fclose( unixfile );
	 remove( argv[idx] );
	 rename( unixname, argv[idx] );
      }
   }
   return 0;
}

