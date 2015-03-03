/*
 * National Institute of Standards and Technology
 * Gaithersburg MD 20899, U.S.A.
 *
 * This program is in the public domain.
 */

/*
 * A program to convert data files from the NIST UV-vis-IR FTS spectrometer
 * in FITS format to old style NSO type 6 header / data file pairs.
 *
 * $Id: fits2nso.c,v 1.4 1996/11/17 23:57:40 ulf Exp $
 */

#include <sys/stat.h>
#include <unistd.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>

#include "List.h"
#include "fits_io.h"

#define FILE_NAME_LEN   256
#define BUFF_LEN        128*1024   /* file copy buffer */
#define IMAX            40         /* for rational approximation */
#define EPS             1.0e-7     /* accuracy for M, K */

/*#define FAT_FS */                /* define this for MS-DOS */

/*
 * for getopt
 */
extern int optind;


/*
 * global flags (I know, that's ugly ...)
 */
static int hdr_flag = 0;
static int rename_flag = 0;
static int mk_flag = 1;

static int npo;                    /* number of points in file */
static int fboffs;                 /* file byte offset */
static int mbx_len;                /* a mailbox */
static char mbx_str[16];
 
static struct stat fs;             /* for file size */

/*--------------------- Prototypes --------------------------------*/

int 
write_nso_header(char fname[], tList header);

/*
 * get the value of a variable from the list
 */
void
retrieve_value(tList h, char var[], int *val);

/*
 * remove a variable from the list
 */
void
delete_var(tList h, char var[]);

/*
 * change the value of a variable in a string
 */
void
edit_value(char line[], int val);

void
val_str( char line[], char num[] );

void
ratapp(float x, float *eps, int *num, int *den);

void
rational(float x, int i, int *n, int *d);

/*-----------------------------------------------------------------*/

static int
findvar(char *v)
{
   if ( strncmp(mbx_str, v, mbx_len) == 0 )
     return 1;
   else
     return 0;
}

/*-----------------------------------------------------------------*/

/*
 * main program
 */
int
main( int argc, char *argv[] )
{
   tList fits_header;
   FILE *ff, *df;
   char file_name[FILE_NAME_LEN];
   char hdr_file[FILE_NAME_LEN];
   char dat_file[FILE_NAME_LEN];
   char ch;
   int numarg;
   int ndone;
   int nump;
   int hbytes;
   int iopt;
   char *cp;
   char buffer[4*BUFF_LEN];

   if ( argc == 1 ) {
      printf("\n");
      printf("Version:  March 2000\n");
      printf("Usage  :  fits2nso [-fhr] <list of FITS files>\n");
      printf("Example:  fits2nso neon*\n");
      printf("Options:\n");
      printf("          -h    create only a header file\n");
      printf("          -r    rename original FITS file to .dat file\n");
      printf("          -f    convert 'fdr' to M, K\n\n");
      exit( 1 );
   }
 
   /*
    * check options
    */
   while( (iopt = getopt(argc, argv, "fhr")) != -1) {
      ch = (char)iopt;
      switch (ch) {
           case 'f':
             mk_flag = 0;
             break;
           case 'h':
             hdr_flag = 1;
             break;
           case 'r':
             rename_flag = 1;
             break;
           case '?':
             printf("\nError :  unknown option on command line\n\n");
             exit( -1 );
             break;
      }
   }

   if ( hdr_flag && rename_flag ) {
      printf("Error :  can use only  -r  OR  -h .\n");
      exit(-1);
   }
 
   /*
    * create FITS header list
    */
   create_list(&fits_header);

   /*
    * deal with all files on the command line in succession
    */
   for (numarg=optind; numarg<argc; numarg++) {
      /* 
       * construct input and output file names
       */
      cp = NULL;
      strcpy(file_name, argv[numarg]);
#if defined(FAT_FS)                                 /* this is for old style MS-DOS */
      if ( (cp=strchr(file_name, '.')) != NULL )  /* truncate extension */
	 *cp = '\0';
#else
      if ( (cp=strstr(file_name, ".fits")) != NULL )
	 *cp = '\0';
#endif
      strcpy(hdr_file, file_name);
      strcpy(dat_file, file_name);
      if ( cp != NULL )
	 strcat(file_name, ".fits");
      strcat(hdr_file, ".hdr");
      strcat(dat_file, ".dat");

      if (stat(file_name, &fs) < 0) {
	 printf("\nError :  could not stat file  %s\n", file_name);
	 continue;
      }
	
      /*
       * clear out old entries from the list
       */
      erase_list_entries(fits_header);


      /*
       * open FITS file and read header, write a NSO type 6 header
       */
      printf("  %s  -->  ", file_name); 
      fflush(stdout);
      if ( (ff = fopen(file_name, "rb")) == NULL ) {
	 printf("\nError :  could not open FITS file  %s\n", file_name);
	 continue;
      }

      if ( (hbytes = fits_read_header_lines(ff, fits_header)) < 0 ) {
	 printf("\nError :  could not read header of  %s\n", file_name);
	 fclose(ff);
	 continue;
      }

      /*
       * some of the file variables are needed later
       */
      retrieve_value(fits_header, "FBOFFS", &fboffs);   /* file byte offset */
      retrieve_value(fits_header, "NPO", &npo);         /* number of data points */

      /*
       * a correct file byte offset is always needed
       */
      if (fboffs == 0)
	 fboffs = hbytes; 

      printf("%s  ",hdr_file);
      fflush(stdout);
      if ( write_nso_header(hdr_file, fits_header) ) {
	 printf("\nError :  could not write NSO header file  %s\n", hdr_file);
	 fclose(ff);
	 continue;
      }

      /*
       * if header creation only we are done. Handle next file
       */
      if ( hdr_flag ) {
	 fclose(ff);
	 printf("\n");
	 continue;
      }


      /*
       * first check if renaming the FITS file is wanted
       */
      if ( rename_flag ) {
 	 if ( rename(file_name, dat_file) ) {
	    printf("\nError :  could not rename file  %s\n", file_name);
	    exit(-1);
	 }
	 fclose(ff);
	 printf("\n");
	 continue;
      }

	
      /*
       * complete conversion
       */
      printf("%s", dat_file);
      fflush(stdout);
      df = fopen(dat_file, "wb");
      if ( df == NULL ) {
	 printf("\nError :  could not open data file  %s\n", dat_file);
	 exit(-1);
      }
      fseek(ff, (long int)fboffs, 0);
      ndone = 0;
      while(ndone < npo) {
	 nump = (npo - ndone > BUFF_LEN) ? BUFF_LEN : npo - ndone;
	 ndone += fread(buffer, sizeof(float), nump, ff);
	 fwrite(buffer, sizeof(float), nump, df);
      }
      fclose(ff);
      fclose(df);
	printf("\n");
   }

   return 0;
}

/*-----------------------------------------------------------------*/

int
write_nso_header(char fname[], tList header)
{
   FILE *hf;
   float eps;
   float fdr;
   int m, k, s;
   char *cp;
   char line[82];
   char numstr[32];
   int i;

   /*
    * open header file
    */
   hf = fopen(fname, "w");
   if ( hf == NULL )
      return -1;

   /*
    * NSO type 6
    */
   fprintf(hf, "/ FTS data file generated from FITS file by  fits2nso\n");
   
   list_head(header);             /* reset list pointer */
   while( !list_at_tail(header) )
     {
        get_current_object(header, line, 81);
	for (i=0; i<8; i++)
	   line[i] = tolower(line[i]);	  
	
	/*
	 * check for a number of special cases
	 */
	if ( strncmp(line, "comment ", 8) == 0 ) {
	   cp = line;
	   cp += 10;
	   fprintf(hf, "/ %s\n", cp);
	}
	else if ( strncmp(line, "bitpix", 6)   == 0 ||
		  strncmp(line, "naxis",  5)   == 0 ||
		  strncmp(line, "naxis1", 6)   == 0 ||
		  strncmp(line, "origin", 6)   == 0 ||
		  strncmp(line, "instrume", 8) == 0 ||
		  strncmp(line, "date", 4)     == 0 ||
		  strncmp(line, "datamin", 7)  == 0 ||
		  strncmp(line, "datamax", 7)  == 0 ||
		  strncmp(line, "ftype", 5)    == 0 ||
		  strncmp(line, "fvar", 4)     == 0 ||
		  strncmp(line, "mode_is", 7)  == 0 ||
		  strncmp(line, "gainbadj", 8) == 0 ||
		  strncmp(line, "iraf", 4)     == 0 ||
		  strncmp(line, "filevers", 8) == 0 ||
		  strncmp(line+2, "cuta", 4)   == 0 ||  /* cutoff parameters */
		  strncmp(line, "wcsdim", 6)   == 0 ||
		  strncmp(line, "ctype1", 6)   == 0 ||
		  strncmp(line, "crval1", 6)   == 0 ||
		  strncmp(line, "cdelt1", 6)   == 0 ||
		  strncmp(line, "cd1_1", 5)    == 0 ||
		  strncmp(line, "ltm1_1", 6)   == 0 ||
		  strncmp(line, "wat0_001", 6) == 0 ||
		  strncmp(line, "        ", 8) == 0 ) {
	   fprintf(hf, "/ %s\n", line);  /* turn these into comments */
	}
	else if ( strncmp(line, "fdr", 3) == 0 ) {
	  if ( mk_flag ) {
	     val_str(line, numstr);
	     sscanf(numstr, "%f", &fdr);
	     eps = EPS;
	     ratapp(fdr, &eps, &m, &k);
	     s = npo / m;
	     fprintf(hf, "nm      = %8d              / M (fringe rate)\n", m);
	     fprintf(hf, "nk      = %8d              / K (sample frequency)\n", k);
	     fprintf(hf, "ns      = %8d              / S (related to scan length)\n", s);
	     fprintf(hf, "sampfreq= 39062.5          / Sampling rate in Hz\n");
	  }
	  else
	    fprintf(hf, "%s\n", line);
	}
	else if ( strncmp(line, "fboffs", 6) == 0 ) {
	   if ( hdr_flag || rename_flag ) {
	       if (fboffs == 0)
		  edit_value(line, fs.st_size - npo*4);
	       fprintf(hf, "%s\n", line);
	   }
	}
	else
	   fprintf(hf, "%s\n", line);
     }

   fprintf(hf, "END\n");

   fclose(hf);
   return 0;
}

/*-----------------------------------------------------------------*/

void
retrieve_value(tList h, char var[], int *val)
{
   char *cp;
   char line[80];
   char numstr[32];

   strcpy(mbx_str, var); 
   mbx_len = strlen(var);
   cp = search_list(h, SEARCH_PROC(findvar));
   if (cp != NULL) {
     strcpy(line, cp);
     val_str(line, numstr);
     sscanf(numstr, "%d", val);
   }
}

/*-----------------------------------------------------------------*/

void
delete_var(tList h, char var[])
{
   char *cp;
   char line[80];
   char numstr[32];

   strcpy(mbx_str, var); 
   mbx_len = strlen(var);
   cp = search_list(h, SEARCH_PROC(findvar));
   if (cp != NULL) {
      list_prev(h);
      remove_current_entry(h);
   }
}

/*-----------------------------------------------------------------*/

void
edit_value(char line[], int val)
{
   char *cp;
   char *ip;
   char numstr[32];

   cp = line;

   /*
    * remove old value
    */
   ip = cp + 10;    /* point right after '=' */
   while (*ip != '/' && *ip != '\0') {
      *ip = ' ';
      ip++;
   }

   /* 
    * paste in new value
    */
   ip = cp + 10;
   sprintf(numstr, "%d", val);
   strncpy(ip, numstr, strlen(numstr));
}

/*-----------------------------------------------------------------*/

void
val_str( char line[], char num[] )
{
   char *cp;
   char *ce;

   for (cp=line+10; cp<line+70; cp++)
     if ( *cp != ' ' )
       break;
   ce = cp;
   while ( *ce != ' ' && *ce != '/' )
     ce++;
   *ce = '\0';
   strcpy(num, cp);
}

/*-----------------------------------------------------------------*/

/*
 *     for a given positive real number x this function calculates a
 *     rational approximation for x such that
 *
 *                 |       num  |
 *                 | x  -  ---  | < eps    .
 *                 |       den  |
 *
 *     'eps' is replaced by the actually achieved precision.
 *
 *     Author    :  Ulf Griesmann    08/92
 *
 *     Reference :  M. C. Gutzwiller, Chaos in Classical and Quantum
 *                  Mechanics : Interdisciplinary Applied Mathematics Vol.1,
 *                  Springer-Verlag, page 129
 */

void
ratapp(float x, float *eps, int *num, int *den)
{
      float epsloc;
      int inum, iden, i;

      for (i=1; i<IMAX; i++) {
	 rational(x, i, &inum, &iden);
	 epsloc = fabs( x - (float)inum/(float)iden) ;
	 if (epsloc <= *eps)
	    break;
      }
      *num = inum;
      *den = iden;
      *eps = epsloc;
}

/*
 *     calculates a rational approximation of x from a continued fraction
 *     approximation up to i-th order. Numerator an denominator are
 *     returned in n and d respectively.
 */
void
rational(float x, int i, int *n, int *d)
{
   int a[IMAX];
   int m, ntmp;
   double rest, xnext, dx;

   /*     
    * calculate the continued fraction approximation
    */
   dx = (double)x;
   rest = dx - floor(dx);
   if (rest == 0.0) {
      *n = (int)floor(dx);
      *d = 1;
      return;
   }

   a[0] = (int)floor(dx);
   for(m=1; m<=i; m++) {
      xnext = 1.0/rest;
      a[m] = (int)floor(xnext);
      rest = xnext - (double)a[m];
   }

   /*
    * put together rational number
    */
   *n = 1;
   *d = a[i];
   for(m=i; m>0; m--) {
      ntmp = *n;
      *n = *d;
      *d = a[m-1] * *d + ntmp;
   }
   ntmp = *n;
   *n = *d;
   *d = ntmp;
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * -----------------
 * $Log: fits2nso.c,v $
 * Revision 1.4  1996/11/17 23:57:40  ulf
 * fixed spelling of nm, nk and ns; added 'sampfreq' variable in
 * output header file.
 *
 * Revision 1.3  1996/11/15 05:10:28  ulf
 * calculate variables M, K and S by default with a rational approximation of the
 * fringe division ratio 'fdr'
 *
 * Revision 1.2  1996/11/11 22:33:23  ulf
 * When FAT_FS is defined during compilation the program handles old 8.3 style
 * file names more gracefully.
 *
 * Revision 1.1  1996/11/10 04:37:28  ulf
 * Initial revision
 *
 */



















