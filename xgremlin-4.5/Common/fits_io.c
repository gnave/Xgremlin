/*
 * National Institute of Standards and Technology
 * Gaithersburg MD 20899, U.S.A.
 *
 * This program is in the public domain.
 */

/*
 * Simple functions for reading and writing FITS files for the
 * NIST FTS
 *
 * $Id: fits_io.c,v 1.4 1996/10/30 04:47:31 ulf Exp $
 */

#include <stdlib.h>
#include <string.h>

#include "fits_io.h"
#include "byteorder.h"

#define KEY_LEN   9
#define VAL_LEN   72

/*-----------------------------------------------------------------*/

static char line[128];          /* a line buffer */
static int mbx;                 /* a mailbox for list operations */
static char *mbx_cp;            /* character pointer mailbox */

/*-----------------------------------------------------------------*/

/*
 * data structure for FITS header records in the internal list
 */
typedef struct {
   char  key[KEY_LEN];          /* 8-character keyword (plus \0)*/
   char  value[VAL_LEN];        /* the value associated with the keyword */
   int   value_len;             /* length of value field */
   char  comment[VAL_LEN];      /* the optional comment string */
   int  comment_len;            /* length of comment */
} header_record;

/*------------------------ Local functions ------------------------*/

/*
 * pad out a string with blank characters up to a length of numc.
 * (currently slow but safe)
 */ 
static void
pad_blanks( char s[], int numc)
{
   int len;
   int k;

   len = strlen(s);
   for (k=len; k<numc; k++)
     s[k] = ' ';
   s[numc] = '\0';
}

/*-----------------------------------------------------------------*/

static void
scan_value_length(header_record *h)
{
   if ( h->comment_len > 0 && h->value_len > mbx )
     mbx = h->value_len;
}

static int
search_key(header_record *h)
{
   if ( strcmp(h->key, mbx_cp) == 0 )
     return 1;
   else
     return 0;
}

/*-----------------------------------------------------------------*/

int
fits_write_header(FILE *f, tList header)
{
   int vlmax;
   int i;
   int numr;
   int clen;
   header_record *h;

   if ( f == NULL )
      return -1;

   /*
    * find the longest value to determine the length of the value field
    */
   mbx = 0;
   scan_list(header, SCAN_PROC(scan_value_length));
   vlmax = mbx;

   /*
    * first write the SIMPLE keyword
    */
   strcpy(line, "SIMPLE  = T");
   pad_blanks(line, 10+vlmax);
   strcat(line, " / FITS standard (NOST 100-1.1)");
   pad_blanks(line, 80);
   fwrite(line, 1, 80, f);
	  
   /*
    * now write out all header records in order
    */   
   list_head(header);
   for (i=0; i<list_entries(header); i++)
     {
        get_current_entry(header, (void *)&h);
        strcpy(line, h->key);
        pad_blanks(line, 8);             /* first 8 bytes are keyword */
        clen = strlen(h->comment);
        if ( strlen(h->value) )          /* value field exists */
          {
             strcat(line, "= ");
             strcat(line, h->value);
             pad_blanks(line, 10+vlmax);
             if ( clen )
               strcat(line, " / ");
          }
        else
          strcat(line, "  ");
        if ( clen )
          strcat(line, h->comment);
        pad_blanks(line, 80);
        fwrite(line, 1, 80, f);          /* write out the header line */
     }
   
   /* 
    * terminate header
    */
   strcpy(line, "END");
   pad_blanks(line, 80);
   fwrite(line, 1, 80, f);

   /*
    * the number of 80-byte records must be a multiple of 36, 
    * pad with a suitable number of blank lines.
    */
   line[0] = '\0';
   pad_blanks(line,80);
   numr = 36 - ( list_entries(header) + 2 ) % 36;
   for (i=0; i<numr; i++)          /*   ^-- for SIMPLE and END lines */
     fwrite(line, 1, 80, f);

   return 0;
}

/*-----------------------------------------------------------------*/

int
fits_read_header(FILE *f, tList header)
{
   char *cp;
   int hb;
   int nb;
   header_record *h;

   if ( f == NULL )
      return -1;

   /*
    * read and skip the SIMPLE keyword
    */
   fread(line, 1, 80, f);
   hb = 80;

   /*
    * now read the remaining header records
    */
   while(1)
     {
        fread(line, 1, 80, f);
	hb += 80;

	/*
	 * extract keyword
	 */
	for ( cp=line+7; cp!=line && *cp==' '; cp-- );
	if ( *cp != ' ' )
	  cp++;
	*cp = '\0';

	if ( strcmp(line, "END") == 0 )      /* found last card image */
	  break;

	if ( line[0] == '\0' || strcmp(line, "COMMENT") == 0 )
	  continue;

	h = (header_record *)malloc( sizeof(header_record) );
	if ( h ==  NULL )
	  return -1;

	strcpy(h->key, line);              /* store the keyword */

	/*
	 * extract value
	 */
	cp = line + 10;     /* first the value type */
	while(*cp != '/')
	  cp++;
	cp--;
	while (*cp == ' ')
	  cp--;

	if ( *cp == '\'' )
	  {
	     cp--;
	     while(*cp == ' ')
	       cp--;
	     cp++;
	     *cp = '\0';
	     cp = line + 10;
	     strcpy(h->value, cp);
	  }
	else
	  {
	     cp++;
	     *cp = '\0';
	     cp = line + 10;
	     while (*cp == ' ')
	       cp++;
	     strcpy(h->value, cp);
	  }
	h->value_len = strlen(cp);
	list_insert(header, h, AFTER);
     }

   /*
    * compute number of bytes in the header
    */
   if (hb % 2880) {
      nb = hb / 2880;
      hb = 2880 * (nb + 1);
   }
   return hb;
}

/*-----------------------------------------------------------------*/

int
fits_read_header_lines(FILE *f, tList header)
{
   int i;
   int hb;
   int nb;

   if ( f == NULL )
      return -1;

   /*
    * read and skip the SIMPLE keyword
    */
   fread(line, 1, 80, f);
   hb = 80;

   /*
    * now read the remaining header records
    */
   while(1)
     {
        fread(line, 1, 80, f);
	hb += 80;
	if ( strncmp(line,"END     ",8) == 0 )    /* reached the end */
	   break;

	for (i=79; i>=0; i--)
	  if ( line[i] != ' ' )
	    break;
	line[++i] = '\0';

	list_insert_object(header, line, 81, AFTER);
     }   

   /*
    * compute number of bytes in the header
    */
   if (hb % 2880) {
      nb = hb / 2880;
      hb = 2880 * (nb + 1);
   }
   return hb;
}

/*-----------------------------------------------------------------*/

int
fits_add_keyword(tList header, char keyword[], 
		 int type, void *value, char comment[])
{
   header_record *h;
   int    *ip;
   float  *fp;
   double *dp;
   char   *cp;

   h = (header_record *)malloc( sizeof(header_record) );
   if ( h ==  NULL )
     return -1;

   strcpy(h->key, keyword);

   switch(type) 
     {     
        case INTEGER:
            ip = (int *)value;
            sprintf(h->value, "%-12d", *ip);
            break;

        case FLOAT:
            fp = (float *)value;
            sprintf(h->value, "%#.8g", *fp);
            break;

        case DOUBLE:
            dp = (double *)value;
            sprintf(h->value, "%#.12g", *dp);
            break;

        case STRING:
            cp = (char *)value;
            sprintf(h->value,"'%s'", cp);
            break;

        default:
            h->value[0] = '\0';
            break;
     }

   h->value_len = strlen(h->value);
   strcpy(h->comment,comment);
   h->comment_len = strlen(comment);
   list_insert(header, h, AFTER);

   return 0;
}

/*-----------------------------------------------------------------*/

int
fits_get_value(tList header, char *keyword, int type, void *value)
{
   header_record *h;
   int    *ip;
   float  *fp;
   double *dp;
   char   *cp;

   mbx_cp = keyword;
   h = search_list(header, SEARCH_PROC(search_key));
   if ( h == NULL )
     return -1;
   
   switch(type)
     {
        case INTEGER:
           ip = (int *)value;
           sscanf(h->value, "%d", ip);
           break;

        case FLOAT:
           fp = (float *)value;
           sscanf(h->value, "%f", fp);
           break;

        case DOUBLE:
           dp = (double *)value;
           sscanf(h->value, "%lf", dp);
           break;

        case STRING:
           cp = (char *)value;
           strcpy(cp, h->value);
           break;
     }

   return 0;
}

/*-----------------------------------------------------------------*/

int
fits_write_data(FILE *f, float *buffer, int num_data)
{
   int numr;
   int i;
   float pad = 0.0;
   float *fp;

   if ( f == NULL )
      return -1;

   /*
    * write data to file and pad with 0.0 if required
    */
   if ( byte_order() == LITTLE_ENDIAN )
     {
        byte_reverse_float( &pad );
        fp = buffer;
        for (i=0; i<num_data; i++)
          {
             byte_reverse_float(fp);
             fp++;
          }
     }

   fwrite(buffer, sizeof(float), num_data, f);
   numr = 2880 - (sizeof(float) * num_data) % 2880;
   for(i=0; i<numr; i++)
     fwrite(&pad, sizeof(float), 1, f);

   return 0;
}

/*-----------------------------------------------------------------*/

int
fits_read_data(FILE *f, float *buffer, int num_data)
{
   int i;
   float *fp;

   if ( f == NULL )
      return -1;

   fread(buffer, sizeof(float), num_data, f);
   if ( byte_order() == LITTLE_ENDIAN )
     {
       fp = buffer;
       for (i=0; i<num_data; i++)
         {
            byte_reverse_float(fp);
            fp++;
         }
     }

   return 0;
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * -----------------
 * $Log: fits_io.c,v $
 * Revision 1.4  1996/10/30 04:47:31  ulf
 * fixed another buf in  fits_read_header_line :  the end of the header
 * was not recognized correctly.
 *
 * Revision 1.3  1996/10/30 04:18:52  ulf
 * fixed a bug in  fits_read_header_line  which trucated the last character.
 *
 * Revision 1.2  1996/10/28 04:10:38  ulf
 * add new function  fits_read_header_lines
 *
 * Revision 1.1  1996/10/18 01:53:07  ulf
 * Initial revision
 *
 */
