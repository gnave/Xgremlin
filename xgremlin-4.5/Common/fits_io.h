/*
 * National Institute of Standards and Technology
 * Gaithersburg MD 20899, U.S.A.
 * 
 * This program is in the public domain.
 */

/*
 * Simple, streamlined functions for reading and writing FITS files
 * for the NIST FTS. To simplify reading and writing of file headers
 * the keyword/value pairs of the header are first stored in 
 * a list and then written to a file in correct order. Reading the
 * header is organized in a similar fashion. The header is read
 * into a list which allows fast, unordered lookup of values.
 */

#ifndef _FITS_IO_H
#define _FITS_IO_H

#include <stdio.h>
#include "List.h"


/*
 * data types that can be stored in a header record
 */
enum data_types { NONE, INTEGER, FLOAT, DOUBLE, STRING };


/*-----------------------------------------------------------------*/

/*
 * Write a standard FITS header from a keyword/value list to the file. 
 * The function assumes that the file pointer is at the beginning of the file.
 * The first record of the FITS file (SIMPLE = T) is added automatically.
 * Returns 0 if successful, -1 if a failure occured.
 */
int
fits_write_header(FILE *f, tList header );


/*
 * Reads the FITS file header from a file into a keyword/value list.
 * Assumes that the list is empty.
 * returns the total number of bytes in the header if successful, 
 * -1 if a failure occured.
 */
int
fits_read_header_(FILE *f, tList header);


/*
 * Reads the FITS file header from an open file and returns the header as
 * a list of strings, NOT as keyword/value pairs. Returns the total number
 * of bytes in the header if successful, -1 otherwise.
 */
int
fits_read_header_lines(FILE *f, tList header);


/*
 * Add a FITS keyword/value pair to the keyword/value list
 * Must NOT be used for the SIMPLE keyword.
 * Returns 0 if all is well, -1 in case of an error.
 */
int
fits_add_keyword(tList header, char keyword[], 
		 int type, void *value, char comment[]);


/*
 * Get the value for a FITS keyword from the header list.
 * Returns -1 if the keyword was not found in the header list,
 * 0 when everything is all right.
 */
int
fits_get_value(tList header, char *keyword, int type, void *value);


/*
 * Write an array of data to a FITS file with padding if necessary.
 * On little endian machines the data are byte reversed before
 * they are written to the file.
 * returns 0 if successful, -1 if a failure occured.
 */
int
fits_write_data(FILE *f, float *buffer, int num_data);


/*
 * Read data from a FITS file to a buffer
 * returns 0 if successful, -1 if a failure occured.
 */
int
fits_read_data(FILE *f, float *buffer, int num_data);


/*-----------------------------------------------------------------*/

#endif /* _FITS_IO_H */

/* Revision history:
 * -----------------
 * $Log: fits_io.h,v $
 * Revision 1.2  1996/10/28 04:10:55  ulf
 * add new function   fits_read_header_lines
 *
 * Revision 1.1  1996/10/18 01:52:45  ulf
 * Initial revision
 *
 */



