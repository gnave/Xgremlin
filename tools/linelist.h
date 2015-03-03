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
 * data structures used for line list files in programs of the Gremlin family
 */

/*
 * data layout in a linelist file:
 *
 * A linelist file starts with a prefix of 320 bytes of which most have no
 * meaning. The first two integers (32 bits) contain the number of lines
 * in the line list file and the number of data bytes in the file (see
 * structure 'gremlin_linelist_header'). The prefix is followed by 
 * one linelist record 'gremlin_linelist_record' for each line in the
 * list. The records must sorted in ascending order with respect to the
 * wavenumber 'sig'. The byte order in line list files is ALWAYS little endian.
 */

typedef struct {
   int num_lines;       /* number of lines */
   int data_size;       /* amount of valid data in .lin file incl. prefix */
} gremlin_linelist_header;


typedef struct {
   double sig;          /* wave number */
   float xint;          /* intensity */
   float width;         /* line width */
   float dmping;        /* damping (for Voigt profiles) */
   short int itn;       /* iterations */
   short int ihold;     /* a flag */
   char tags[4];        /* line label */
   float epstot;
   float epsevn;
   float epsodd;
   float epsran;
   float spare;         /* nothin' yet */
   char ident[32];
} gremlin_linelist_record;
 

