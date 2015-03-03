/*
 * Copyright (C) 1994 - 1997
 * Ulf Griesmann, Gaithersburg MD 20877, U.S.A.
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
 * $Id: Phase.c,v 1.21 1996/07/14 02:33:19 ulf Exp $
 */

/*
 * This program is part of Xgremlin / GUI interface to phase correction
 */

#include <X11/Intrinsic.h>     /* X toolkit intrinsics definitions */
#include <X11/StringDefs.h>    /* Standard Name-String definitions */
#include <X11/Shell.h>

#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>

#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <setjmp.h>

#include "Version.h"
#include "defaults.h"
#include "FortranWrappers.h"
#include "Miscellaneous.h"
#include "Edit.h"
#include "Phase.h"
#include "PlotMode.h"
#include "regexpr.h"

#include "icons/Xgremlin_phase.icon.xbm"
#include "icons/count_up.xbm"
#include "icons/count_down.xbm"

#define NUM_LEN        32
#define OTHER_LEN      256
#define MAX_CMD_LEN    256

#define REAL_OUT       0     /* output types */
#define IMAG_OUT       1
#define CPLX_OUT       2
#define BOTH_OUT       3

#define REAL_EXT       "_r"
#define IMAG_EXT       "_i"
#define CPLX_EXT       "_c"
#define BOTH_EXT       "_*"

/*
 * regular expression that describes a well formed number or blank space
 */
#define RE_NUMBER      " *|( *[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eEdD][-+]?[0-9]+)?)" 
 
/*
 * global variables
 */
extern Display *display;               /* our display */
extern Widget gremlinTop;              /* top level widget */

static Widget phaseShell,              /* phase correction window */
              phaseForm,
              fileLabel,
              fileText,
              symmetryLabel,
              symmetryCommand,
              fittypeLabel,
              fittypeCommand,
              invertLabel,
              invertCommand,
              buttonBox,
              centerLabel,
              centerText,
              centerUpButton,
              centerDownButton,
              spectrumCommand,
              spectrumLabel,
              termsLabel,
              termsText,
              termsUpButton,
              termsDownButton,
              termsToggleButton,
              discriminatorLabel,
              discriminatorText,
              discriminatorLabel2,
              snrLabel,
              snrText,
              snrLabel2,
              otherLabel,
              otherText,
              skipLabel,
              skipText,
              pointsLabel,
              pointsText,
              apodizeLabel,
              apodizeText,
              centerToggleButton,
              limitsLabel,
              leftText,
              rightText,
              outputCommand,
              outputLabel,
              outputFile,
              closeButton,
              fillinButton,
              phaseButton,
              transformButton;

static Widget phaseMenuButton;       /* button in 'Gremlins' menu */
static Widget last_widget;

static Pixmap count_up, count_down;

/*
 * integer flags
 */
static int twosided = TRUE;          /* spectrum is twosided (balanced) */
static int invert = FALSE;           /* invert interferogram */
static int line_spectrum = TRUE;     /* line spectrum or continuum */
static int output_type = CPLX_OUT;   /* complex, real or both output */
static int phase_fit = TRUE;         /* calculate polyn. if no. of terms > 0 */
static int polynomial_fit = TRUE;    /* polynomial or spline fit */
static int center_read = FALSE;      /* transform only central portion of interferogram */

/*
 * strings with values
 */
static char interferogram_str[FILE_NAME_LEN];
static char center_str[NUM_LEN];
static char terms_str[NUM_LEN];
static char discriminator_str[NUM_LEN];
static char snr_str[NUM_LEN];
static char other_str[OTHER_LEN];
static char skip_str[NUM_LEN];
static char points_str[NUM_LEN];
static char apodize_str[NUM_LEN];
static char range_left_str[NUM_LEN];
static char range_right_str[NUM_LEN];
static char output_str[FILE_NAME_LEN];

/*
 * status buffer used to store the stack contents prior to dispatching a command.
 * The status information is used by the signal handlers to return from a
 * broken command.
 */
extern jmp_buf proc_status;

/*
 * for sanity check of input
 */
static struct re_pattern_buffer rec_number;
static struct re_registers regs;

/*------------------  Local prototypes  --------------------------*/ 

static void
SymmetryCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
InvertCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
TypeCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
CenterToggleCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
OutputCB(Widget w, XtPointer client_data, XtPointer call_data);
 
static void
ClosePhaseWindow(Widget w, XtPointer client_data, XtPointer call_data);

static void
PhasecorrectCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
PhaseFitTypeCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
TransformCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
FillinCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
CenterUpCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
CenterDownCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
TermsUpCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
TermsDownCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
TermsToggleCB(Widget w, XtPointer client_data, XtPointer call_data);

/*
 * checks if a character array contains more than ' ' or \0
 */
static int
string_contains_char( char s[], int len );

/*
 * returns the true string length ignoring trailing blanks
 */
static int
truestrlen( char s[] );

/*
 * updates the text in a text widget
 */
static void
update_text( Widget w, char s[], int len );

/*------------------  Callback functions for buttons  ------------------*/ 

static void
SymmetryCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   Arg args[1];

   if ( twosided == TRUE )
     {
	XtSetArg( args[0], XtNlabel, "unbalanced");
	XtSetValues( symmetryLabel, args, 1);
 	twosided = FALSE;
     }
   else
     {
	XtSetArg( args[0], XtNlabel, "twosided  ");
	XtSetValues( symmetryLabel, args, 1);
 	twosided = TRUE;
     }
}

/*-------------------------------------------------------------*/ 

static void
InvertCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   Arg args[1];

   if ( invert == TRUE )
     {
	XtSetArg( args[0], XtNlabel, "no ");
	XtSetValues( invertLabel, args, 1);
 	invert = FALSE;
     }
   else
     {
	XtSetArg( args[0], XtNlabel, "yes");
	XtSetValues( invertLabel, args, 1);
 	invert = TRUE;
     }
}

/*-------------------------------------------------------------*/ 

static void
TypeCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   Arg args[1];

   if ( line_spectrum == TRUE )
     {
	XtSetArg( args[0], XtNlabel, "Continuum spectrum" );
	XtSetValues( spectrumLabel, args, 1);
	XtSetArg( args[0], XtNlabel, "Number of points:" );
	XtSetValues( termsLabel, args, 1);
	XtSetArg( args[0], XtNsensitive, False );
	XtSetValues( discriminatorLabel, args, 1);
	XtSetValues( snrLabel, args, 1);
 	line_spectrum = FALSE;
     }
   else
     {
	XtSetArg( args[0], XtNlabel, "Line spectrum     ");
	XtSetValues( spectrumLabel, args, 1);
	XtSetArg( args[0], XtNlabel, "Number of terms: " );
	XtSetValues( termsLabel, args, 1);
	XtSetArg( args[0], XtNsensitive, True );
	XtSetValues( discriminatorLabel, args, 1);
	XtSetValues( snrLabel, args, 1);	
 	line_spectrum = TRUE;
     }
}

/*-------------------------------------------------------------*/ 

static void
PhaseFitTypeCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   Arg args[1];
   
   if (polynomial_fit == TRUE) {
	XtSetArg(args[0], XtNlabel, "Cubic spline ( smoothed )");
	XtSetValues(fittypeLabel, args, 1);
	XtSetArg(args[0], XtNsensitive, False);
	XtSetValues(termsLabel, args, 1);
	polynomial_fit = FALSE;

	/*
	 * turn off phase fitting
	 */
	XtSetArg(args[0], XtNstate, True);
	XtSetValues(termsToggleButton, args, 1);
	phase_fit = FALSE;
   }
   else {
	XtSetArg(args[0], XtNlabel, "Chebychev polynomial     ");
	XtSetValues(fittypeLabel, args, 1);
	XtSetArg(args[0], XtNsensitive, True);
	XtSetValues(termsLabel, args, 1);
	polynomial_fit = TRUE ;
   }
}

/*-----------------------------------------------------------------*/

static void
CenterToggleCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   if (center_read == TRUE)
      center_read = FALSE;
   else
      center_read = TRUE;
}

/*-----------------------------------------------------------------*/

static void
OutputCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   Arg args[1];
   int len;
   char *cp;

   if ( output_type == BOTH_OUT )    /* REAL --> IMAG --> CPLX --> BOTH --> REAL */
     {
	XtSetArg( args[0], XtNlabel, "real spectrum");
	XtSetValues( outputLabel, args, 1);
 	output_type = REAL_OUT;
     }
   else if ( output_type == REAL_OUT )
     {
	XtSetArg( args[0], XtNlabel, "imag. spectrum");
	XtSetValues( outputLabel, args, 1);
 	output_type = IMAG_OUT;
     }
   else if ( output_type == IMAG_OUT )
     {
	XtSetArg( args[0], XtNlabel, "complex spectrum");
	XtSetValues( outputLabel, args, 1);
 	output_type = CPLX_OUT;
     }
   else if ( output_type == CPLX_OUT )
     {
	XtSetArg( args[0], XtNlabel, "real & complex");
	XtSetValues( outputLabel, args, 1);
 	output_type = BOTH_OUT;
     }

   if ( string_contains_char(output_str, FILE_NAME_LEN) == TRUE )
     {
	/*
	 * now check if the file name has our extension. if yes,
	 * replace it, if not append an appropriate extension
	 */
	len = truestrlen(output_str);
	if ( len > 2 )
	  cp = output_str + len - 2;   /* point to _r, _i, _c or _* */
	else
	  cp = output_str;
	if ( strcmp(cp, REAL_EXT) == 0 || 
	     strcmp(cp, IMAG_EXT) == 0 ||
	     strcmp(cp, CPLX_EXT) == 0 ||
	     strcmp(cp, BOTH_EXT) == 0 ) 
	  {
	     if ( output_type == BOTH_OUT )
	       output_str[len-1] = '*';
	     else if ( output_type == CPLX_OUT )
	       output_str[len-1] = 'c';
	     else if ( output_type == REAL_OUT )
	       output_str[len-1] = 'r';
	     else
	       output_str[len-1] = 'i';
	     XawTextDisplay(outputFile);
	  }
	else
	  {
	     if ( output_type == BOTH_OUT )
	       strcat(output_str,BOTH_EXT);
	     else if ( output_type == CPLX_OUT )
	       strcat(output_str,CPLX_EXT);
	     else if ( output_type == REAL_OUT )
	       strcat(output_str,REAL_EXT);
	     else
	       strcat(output_str,IMAG_EXT);
	     update_text(outputFile, output_str, FILE_NAME_LEN);
	  }
     }
}

/*-------------------------------------------------------------*/ 

static void
CenterUpCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   int num;
   char numstr[NUM_LEN];

   if ( string_contains_char(center_str, NUM_LEN) )
     {
       sscanf( center_str, "%d", &num );
       num++;
     }
   else
     num = 0;
   sprintf( numstr, " %d", num );
   update_text( centerText, numstr, NUM_LEN );
}

/*-------------------------------------------------------------*/ 

static void
CenterDownCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   int num;
   char numstr[NUM_LEN];

   if ( string_contains_char(center_str, NUM_LEN) )
     {
       sscanf( center_str, "%d", &num );
       num--;
       if ( num < 0 )
	 num = 0;
     }
   else
     num = 0;
   sprintf( numstr, " %d", num );
   update_text( centerText, numstr, NUM_LEN );
}

/*-------------------------------------------------------------*/ 

static void
TermsUpCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   int num;
   char numstr[NUM_LEN];

   if (polynomial_fit == FALSE)
      return;
   sscanf( terms_str, "%d", &num );
   num++;
   sprintf( numstr, " %d", num );
   update_text( termsText, numstr, NUM_LEN );
}

/*-------------------------------------------------------------*/ 

static void
TermsDownCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   int num;
   char numstr[NUM_LEN];

   if (polynomial_fit == FALSE)
      return;
   sscanf( terms_str, "%d", &num );
   num--;
   if ( num < 0 )
     num = 0;
   sprintf( numstr, " %d", num );
   update_text( termsText, numstr, NUM_LEN );
}

/*-------------------------------------------------------------*/ 

static void
TermsToggleCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   if ( phase_fit == TRUE )
     phase_fit = FALSE;
   else
     phase_fit = TRUE;
}

/*-----------------------------------------------------------------*/

static void
ClosePhaseWindow(Widget w, XtPointer client_data, XtPointer call_data)
{
   Arg args[1];

   /*
    * make help button sensitive again
    */
   XtSetArg( args[0], XtNsensitive, True);
   XtSetValues( phaseMenuButton, args, 1);

   /*
    * destroy help window
    */
   XtDestroyWidget( phaseShell );
}

/*-----------------------------------------------------------------*/
 
static void
FillinCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   int center_point;
   char fname[FILE_NAME_LEN];
   char numstr[NUM_LEN];

   /*
    * the following function is a Fortran subroutine which returns
    * the name of the file currently attached to 'datain' and
    * the position of the currently set mouse marker.
    */
   fncpinfo_( fname, &center_point, FILE_NAME_LEN );

   StrForToC( fname, FILE_NAME_LEN );
   if ( strlen(fname) > 0 )
     {
	strcpy(interferogram_str, fname);
	update_text( fileText, interferogram_str, FILE_NAME_LEN );
	strcpy(output_str, fname);
	if ( output_type == CPLX_OUT )
	  strcat(output_str, CPLX_EXT);
	else if ( output_type == REAL_OUT )
	  strcat(output_str, REAL_EXT);
	else
	  strcat(output_str, BOTH_EXT);
	update_text( outputFile, output_str, FILE_NAME_LEN);
     }

   if ( center_point > 0 )
     {
	sprintf(numstr, " %d", center_point);
	update_text( centerText, numstr, NUM_LEN );
     }
}

/*-------------------------------------------------------------*/ 

static void
PhasecorrectCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   FILE *phase_batch;
   char file_name[FILE_NAME_LEN];
   char command[MAX_CMD_LEN];            /* command string */

   /*
    * check numeric parameters
    */
   if ( re_match(&rec_number,terms_str,strlen(terms_str),0,&regs) < 0 ||
        re_match(&rec_number,center_str,strlen(center_str),0,&regs) < 0 ) {
      WriteStr("\n Error :  malformed number in phase correction form.");
      return;
   }
   if ( line_spectrum == TRUE ) {
      if (re_match(&rec_number,discriminator_str,strlen(discriminator_str),0,&regs)<0 ||
	  re_match(&rec_number,snr_str,strlen(snr_str),0,&regs) < 0 ) {
	 WriteStr("\n Error :  malformed number in phase correction form.");
	 return;
      }
   }

   /*
    * create a phase correction batch file and feed it to Xgremlin
    */
   sprintf( file_name, "phase-%d.batch", (int)getpid() );
   phase_batch = fopen( file_name, "w" );
   fprintf( phase_batch, "# Phase correction batch file created by Xgremlin %s\n#\n", VERSION );
   fprintf( phase_batch, "close datain\n");
   fprintf( phase_batch, "open rawin %s\n", interferogram_str );
   if ( twosided == TRUE ) 
     fprintf( phase_batch, "twosided\n" );
   else
     fprintf( phase_batch, "onesided\n" );
   fprintf( phase_batch, "ncenter %s\n", center_str );
   if ( line_spectrum == TRUE ) {
      if (polynomial_fit == TRUE)
	 fprintf(phase_batch, "phcorr fit %s %s %s\n", 
		 (phase_fit == TRUE) ? terms_str : "0", 
		 discriminator_str, snr_str);
      else
	 fprintf(phase_batch, "phcorr spline %s %s %s\n", 
		 (phase_fit == TRUE) ? "1" : "0", 
		 discriminator_str, snr_str);
   }
   else
     fprintf( phase_batch, "phcorr smooth %s\n", terms_str );
   if ( invert == TRUE )
     fprintf( phase_batch, "input inverted\n" );
   fprintf( phase_batch, "input center\n" );
   fprintf( phase_batch, "close rawin\n" );
   fprintf( phase_batch, "plot phase\n" );        /* draw phase plot */
   fprintf( phase_batch, "break\n");
   fprintf( phase_batch, "# the end\n" );
   fclose( phase_batch );
  
   /*
    * reset phase zoom button
    */
   PhaseOff();

   /*
    * put together Xgremlin run command and dispatch it
    */
   strcpy( command, "run " );
   strcat( command, file_name );
   StrCToFor( command, MAX_CMD_LEN );

   if ( setjmp(proc_status) == 0 )
     runcmd_( command, MAX_CMD_LEN );
   else
     WriteStr( "\n Warning :  batch file not processed completely\n\n" ); 

   /*
    * remove the batch file
    */
   unlink( file_name );
}

/*-------------------------------------------------------------*/ 

static void
TransformCB(Widget w, XtPointer client_data, XtPointer call_data)
{
   FILE *transform_batch;
   char file_name[FILE_NAME_LEN];
   int ast_pos;
   int nskp, nctr, npts;
   char command[MAX_CMD_LEN];            /* command string */

   /*
    * check numeric parameters
    */
   if (re_match(&rec_number,skip_str,strlen(skip_str),0,&regs) < 0 ||
       re_match(&rec_number,points_str,strlen(points_str),0,&regs) < 0 ||
       re_match(&rec_number,apodize_str,strlen(apodize_str),0,&regs) < 0 ||
       re_match(&rec_number,center_str,strlen(center_str),0,&regs) < 0 ||
       re_match(&rec_number,range_left_str,strlen(range_left_str),0,&regs) < 0 ||
       re_match(&rec_number,range_right_str,strlen(range_right_str),0,&regs) < 0 ) {
      WriteStr("\n Error :  malformed number in phase correction form.");
      return;
   }

   /*
    * create a Fourier transform batch file and feed it to Xgremlin
    */
   sprintf( file_name, "transform-%d.batch", (int)getpid() );
   transform_batch = fopen( file_name, "w" );
   fprintf( transform_batch, "# Transform batch file created by Xgremlin %s\n#\n", VERSION );
   fprintf( transform_batch, "wavelimits\n"); /* clear wavenumber bounds */
   fprintf( transform_batch, "open rawin %s\n", interferogram_str );
   if ( string_contains_char(skip_str,NUM_LEN) == TRUE && center_read == FALSE)
     fprintf( transform_batch, "skip %s\n", skip_str );
   if ( twosided == TRUE ) 
     fprintf( transform_batch, "twosided\n" );
   else
     fprintf( transform_batch, "onesided\n" );
   fprintf( transform_batch, "ncenter %s\n", center_str );
   if ( line_spectrum == TRUE ) {
      if (polynomial_fit == TRUE)
	 fprintf(transform_batch, "phcorr fit %s %s %s\n", 
		 (phase_fit == TRUE) ? terms_str : "0", 
		 discriminator_str, snr_str);
      else
	 fprintf(transform_batch, "phcorr spline %s %s %s\n", 
		 (phase_fit == TRUE) ? "1" : "0", 
		 discriminator_str, snr_str);
   }
   else
     fprintf( transform_batch, "phcorr smooth %s\n", terms_str );
   if ( invert == TRUE )
     fprintf( transform_batch, "input inverted\n" );
   if (center_read == TRUE && string_contains_char(points_str, NUM_LEN) == TRUE) {
      sscanf(points_str, "%d", &npts);      /* number of points to transform */
      sscanf(center_str, "%d", &nctr);      /* index of central point */
      nskp = nctr - npts/2;                 /* number of points to skip */
      fprintf(transform_batch, "skip %d\n", nskp);
      fprintf(transform_batch, "info ntrans %d\n", npts);
      fprintf(transform_batch, "input %s\n", points_str);
   }
   else
     fprintf( transform_batch, "input %s\n", points_str );
   if ( string_contains_char(other_str,OTHER_LEN) == TRUE ) /* additional commands after */
     fprintf( transform_batch, "%s\n", other_str);          /* phase correction          */
   fprintf( transform_batch, "apodize %s\n", apodize_str );
   fprintf( transform_batch, "transform\n" );
   if ( string_contains_char(range_left_str,NUM_LEN) == TRUE && 
        string_contains_char(range_right_str,NUM_LEN) == TRUE )
     fprintf( transform_batch, "wavelimits %s %s\n", range_left_str, range_right_str );

   if ( output_type == BOTH_OUT ) {
      ast_pos = strlen(output_str) - 1;
      output_str[ast_pos] = 'c';
      fprintf( transform_batch, "complex %s\n", output_str );
      output_str[ast_pos] = 'r';
      fprintf( transform_batch, "real %s\n", output_str );
   }
   else if ( output_type == CPLX_OUT )
     fprintf( transform_batch, "complex %s\n", output_str );
   else if ( output_type == REAL_OUT )
     fprintf( transform_batch, "real %s\n", output_str );
   else
     fprintf( transform_batch, "imaginary %s\n", output_str );
 
   fprintf( transform_batch, "close rawin\n" );
   fprintf( transform_batch, "break\n" );
   fprintf( transform_batch, "# the end\n" );
   fclose( transform_batch );

   /*
    * reset phase zoom button
    */
   PhaseOff();

   /*
    * put together Xgremlin run command and dispatch it
    */
   strcpy( command, "run " );
   strcat( command, file_name );
   StrCToFor( command, MAX_CMD_LEN );

   if ( setjmp(proc_status) == 0 )
     runcmd_( command, MAX_CMD_LEN );
   else
     WriteStr( "\n Warning :  batch file not processed completely\n\n" );

   /*
    * remove the batch file
    */
   unlink( file_name );
}

/*-------------------------------------------------------------*/ 

static int
string_contains_char( char s[], int len )
{
   int k;

   for ( k=0; k<len; k++ )
     {
	if ( s[k] != ' ' && s[k] != '\0' )
	  return TRUE;
     }
   return FALSE;
}

/*-------------------------------------------------------------*/ 

static int
truestrlen( char s[] )
{
   int len;
   int k;
   
   len = strlen(s);
   for ( k=len-1; k>=0; k-- )
     if ( s[k] != ' ')
       break;
   if ( k == 0 && s[k] == ' ' )
     s[0] = '\0';
   else
     s[k+1] = '\0';
   return strlen(s);
}

/*-----------------------------------------------------------------*/

static void
update_text( Widget w, char s[], int len )
{
   XawTextBlock line;

   line.firstPos = 0;
   line.length   = strlen(s);
   line.ptr      = s;
   line.format   = FMT8BIT;
   XawTextReplace( w, 0, len, &line );
}

/*-----------------------------------------------------------------*/

void
Focus( Widget w, XEvent *ev, String *par, Cardinal *npar )
{
   Arg args[4];
   int num;

   if ( w == fileText          || w == centerText  || w == termsText ||
        w == discriminatorText || w == snrText     || w == otherText ||
        w == skipText          || w == apodizeText || w == leftText  ||
        w == rightText         || w == outputFile  || w == pointsText )
     {
	if ( line_spectrum == FALSE )
	  if ( w == discriminatorText || w == snrText )
	    return;   /* don't allow input */

	if ( last_widget != NULL )
	  {
	     num = 0;
	     XtSetArg( args[num], XtNdisplayCaret, False ); num++;
	     XtSetValues( last_widget, args, num );
	  }

	/*
	 * check if output file name should be filled in
	 */
	if (last_widget == fileText && 
	    string_contains_char(interferogram_str, FILE_NAME_LEN) == TRUE)
	  {
	     strcpy(output_str, interferogram_str);
	     truestrlen(output_str);
	     if ( output_type == CPLX_OUT )
	       strcat(output_str, CPLX_EXT);
	     else if ( output_type == REAL_OUT )
	       strcat(output_str, REAL_EXT);
	     else
	       strcat(output_str, BOTH_EXT);
	     update_text( outputFile, output_str, FILE_NAME_LEN );
	  }

	num = 0;
	XtSetArg( args[num], XtNdisplayCaret, True ); num++;
	XtSetValues( w, args, num );
	XtSetKeyboardFocus( phaseShell, w );
	last_widget = w;
     }
   else
     XtSetKeyboardFocus( phaseShell, phaseShell );
}
 
/*-------------------------------------------------------------*/ 

void
Accept( Widget w, XEvent *ev, String *par, Cardinal *npar )
{
   Arg args[2];
   int num;

   if ( w == fileText          || w == centerText  || w == termsText ||
        w == discriminatorText || w == snrText     || w == otherText ||
        w == skipText          || w == apodizeText || w == leftText  ||
        w == rightText         || w == outputFile  || w == pointsText )
     {
	num = 0;
	XtSetArg( args[num], XtNdisplayCaret, False ); num++;
	XtSetValues( w, args, num );
	XtSetKeyboardFocus( phaseShell, phaseShell );
	last_widget = w;
     }
}

/*-------------------------------------------------------------*/ 

void
InitializePhase( Widget button )
{
   /*
    * save menu button for later use
    */
   phaseMenuButton = button;

   /*
    * compile regular expression
    */
   rec_number.buffer = NULL;
   rec_number.allocated = 0;
   rec_number.translate = NULL;
   re_set_syntax( RE_NO_BK_PARENS | RE_NO_BK_VBAR );
   if ( re_compile_pattern( RE_NUMBER, strlen(RE_NUMBER), &rec_number ) != NULL )
     {
	fprintf( stderr, "Fatal error :  cannot compile reg. exp. in InitializePhase\n");
	exit( -1 );
     }
}

/*-------------------------------------------------------------*/ 

void
OpenPhaseWindow()
{
   Arg args[1];
   Pixmap icon;

   /*
    * preset all string variables with \0
    * That way everything that is typed into the text widgets
    * will always be correctly terminated.
    */
   memset( interferogram_str, '\0', FILE_NAME_LEN );
   memset( center_str, '\0', NUM_LEN );
   memset( terms_str, '\0', NUM_LEN );
   memset( discriminator_str, '\0', NUM_LEN );
   memset( snr_str, '\0', NUM_LEN );
   memset( other_str, '\0', NUM_LEN );
   memset( skip_str, '\0', NUM_LEN );
   memset( points_str, '\0', NUM_LEN );
   memset( apodize_str, '\0', NUM_LEN );
   memset( range_left_str, '\0', NUM_LEN );
   memset( range_right_str, '\0', NUM_LEN );
   memset( output_str, '\0', FILE_NAME_LEN );

   /*
    * a few default values
    */
   strcpy( terms_str, " 0" );  
   strcpy( discriminator_str, " 0.1" );
   strcpy( snr_str, " 0.001" );

   twosided = TRUE; 
   invert = FALSE;   
   line_spectrum = TRUE; 
   output_type = CPLX_OUT;

   last_widget = NULL;

   /*
    * make menu button insensitive
    */
   XtSetArg( args[0], XtNsensitive, False);
   XtSetValues( phaseMenuButton, args, 1);

   /*
    * create a shell window
    */
   phaseShell = XtVaAppCreateShell(
        NULL,
        "phaseshell",
        topLevelShellWidgetClass,
        display,
        XtNwidth,  PHASE_WINDOW_WIDTH,
        XtNheight, PHASE_WINDOW_HEIGHT,
        NULL);
 
   /*
    * geometry management for widgets 
    */
   phaseForm = XtVaCreateManagedWidget(
        "phaseform",
        formWidgetClass,
        phaseShell,
        NULL );

   /*
    * file name input field label / text input
    */
   fileLabel = XtVaCreateManagedWidget(
        "filelabel",
        labelWidgetClass,
        phaseForm,
        XtNlabel, "Interferogram file:  ",
        NULL );

   fileText = XtVaCreateManagedWidget(
        "filetext",
        asciiTextWidgetClass,
        phaseForm,
        XtNfromHoriz, fileLabel,
        XtNstring, &interferogram_str,
        XtNlength, FILE_NAME_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,				    
        NULL );

   /*
    * symmetry type of interferogram
    */
   symmetryCommand = XtVaCreateManagedWidget(
        "symmetrycommand",
        commandWidgetClass,
        phaseForm,
        XtNfromVert, fileLabel,
        XtNlabel, "Symmetry:",
        NULL );
   XtAddCallback( symmetryCommand, XtNcallback, SymmetryCB, 0 );
   twosided = TRUE;

   symmetryLabel = XtVaCreateManagedWidget(
        "symmetrylabel",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, fileText,
        XtNfromHoriz, symmetryCommand,
        XtNlabel, "twosided  ",
        NULL );

   /*
    * invert interferogram or not
    */
   invertCommand = XtVaCreateManagedWidget(
        "invertcommand",
        commandWidgetClass,
        phaseForm,
        XtNfromVert, fileLabel,
        XtNfromHoriz, symmetryLabel,
        XtNlabel, "Invert interferogram:",
        NULL );
   XtAddCallback( invertCommand, XtNcallback, InvertCB, 0 );
   invert = FALSE;

   invertLabel = XtVaCreateManagedWidget(
        "invertlabel",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, fileText,
        XtNfromHoriz, invertCommand,
        XtNlabel, "no ",
        NULL );

   /*
    * center of interferogram
    */
   centerLabel = XtVaCreateManagedWidget(
        "centerlabel",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, symmetryCommand,
        XtNlabel, "Interferogram center:",
        NULL );

   centerText = XtVaCreateManagedWidget(
        "centertext",
        asciiTextWidgetClass,
        phaseForm,
        XtNfromVert, symmetryCommand,
        XtNfromHoriz, centerLabel,
        XtNstring, &center_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,				    
        NULL );

   centerUpButton = XtVaCreateManagedWidget(
        "centerup",
        commandWidgetClass,
        phaseForm,
        XtNfromVert, symmetryCommand,
        XtNfromHoriz, centerText,
        XtNwidth, 25,
        NULL );
   XtAddCallback(centerUpButton, XtNcallback, CenterUpCB, 0 );

   centerDownButton = XtVaCreateManagedWidget(
        "centerdown",
        commandWidgetClass,
        phaseForm,
        XtNfromVert, symmetryCommand,
        XtNfromHoriz, centerUpButton,
        XtNwidth, 25,
        NULL );
   XtAddCallback(centerDownButton, XtNcallback, CenterDownCB, 0 );


    /*
     * spectrum type  line / continuum
     */
   spectrumCommand = XtVaCreateManagedWidget(
        "spectrumcommand",
        commandWidgetClass,
        phaseForm,
        XtNfromVert, centerLabel,
        XtNlabel, "Spectrum type:",
        NULL );
   XtAddCallback( spectrumCommand, XtNcallback, TypeCB, 0 );
   line_spectrum = TRUE;

   spectrumLabel = XtVaCreateManagedWidget(
        "spectrumlabel",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, centerText,
        XtNfromHoriz, spectrumCommand,
        XtNlabel, "Line spectrum     ",
        NULL );

   /*
    * type of phase fit: polynomial or spline
    */
   fittypeCommand = XtVaCreateManagedWidget(
        "fittypecommand",
        commandWidgetClass,
        phaseForm,
        XtNfromVert, spectrumCommand,
        XtNlabel, "Phase curve  :",
        NULL );
   XtAddCallback( fittypeCommand, XtNcallback, PhaseFitTypeCB, 0 );
   polynomial_fit = TRUE;

   fittypeLabel = XtVaCreateManagedWidget(
        "fittypelabel",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, spectrumCommand,
        XtNfromHoriz, fittypeCommand,
        XtNlabel,"Chebychev polynomial     ",
        NULL );


   /*
    * parameters for phase fit
    */
   termsLabel = XtVaCreateManagedWidget(
        "termslabel",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, fittypeCommand,
        XtNlabel, "Number of terms: ",
        NULL );

   termsText = XtVaCreateManagedWidget(
        "termstext",
        asciiTextWidgetClass,
        phaseForm,
        XtNfromVert, fittypeCommand,
        XtNfromHoriz, termsLabel,
        XtNstring, &terms_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,				    
        NULL );

   termsUpButton = XtVaCreateManagedWidget(
        "termsup",
        commandWidgetClass,
        phaseForm,
        XtNfromVert, fittypeCommand,
        XtNfromHoriz, termsText,
        XtNwidth, 25,
        NULL );
   XtAddCallback(termsUpButton, XtNcallback, TermsUpCB, 0 );

   termsDownButton = XtVaCreateManagedWidget(
        "termsdown",
        commandWidgetClass,
        phaseForm,
        XtNfromVert, fittypeCommand,
        XtNfromHoriz, termsUpButton,
        XtNwidth, 25,
        NULL );
   XtAddCallback(termsDownButton, XtNcallback, TermsDownCB, 0 );

   termsToggleButton = XtVaCreateManagedWidget(
        "termstoggle",
        toggleWidgetClass,
        phaseForm,
        XtNfromVert, fittypeCommand,
        XtNfromHoriz, termsDownButton,
        XtNlabel, "no fit",
        NULL );
   XtAddCallback(termsToggleButton, XtNcallback, TermsToggleCB, 0 );
   phase_fit = TRUE;


   discriminatorLabel = XtVaCreateManagedWidget(
        "discriminatorlabel",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, termsLabel,
        XtNlabel, "Discriminator:   ",
        NULL );

   discriminatorText = XtVaCreateManagedWidget(
        "discriminatortext",
        asciiTextWidgetClass,
        phaseForm,
        XtNfromVert, termsLabel,
        XtNfromHoriz, discriminatorLabel,
        XtNstring, &discriminator_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,				    
        NULL );

   discriminatorLabel2 = XtVaCreateManagedWidget(
        "discriminatorlabel2",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, termsLabel,
        XtNfromHoriz, discriminatorText,
        XtNlabel, " % / 100",
        NULL );


   snrLabel = XtVaCreateManagedWidget(
        "snrlabel",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, discriminatorLabel,
        XtNlabel, "Signal / Noise:  ",
        NULL );

   snrText = XtVaCreateManagedWidget(
        "snrtext",
        asciiTextWidgetClass,
        phaseForm,
        XtNfromVert, discriminatorLabel,
        XtNfromHoriz, snrLabel,
        XtNstring, &snr_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,				    
        NULL );

   snrLabel2 = XtVaCreateManagedWidget(
        "snrlabel2",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, discriminatorLabel,
        XtNfromHoriz, snrText,
        XtNlabel, " % / 100",
        NULL );


   otherLabel = XtVaCreateManagedWidget(
        "otherlabel",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, snrLabel,
        XtNlabel, "Other:  ",
        NULL );

   otherText = XtVaCreateManagedWidget(
        "othertext",
        asciiTextWidgetClass,
        phaseForm,
        XtNfromVert, snrLabel,
        XtNfromHoriz, otherLabel,
        XtNstring, &other_str,
        XtNlength, OTHER_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,				    
        NULL );


   /*
    * skip points when transforming
    */
   skipLabel = XtVaCreateManagedWidget(
        "skiplabel",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, otherLabel,
        XtNlabel, "Skip:   ",
        NULL );

   skipText = XtVaCreateManagedWidget(
        "skiptext",
        asciiTextWidgetClass,
        phaseForm,
        XtNfromVert, otherLabel,
        XtNfromHoriz, skipLabel,
        XtNstring, &skip_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,				    
        NULL );


   /*
    * read only so many points with 'input' command
    */
   pointsLabel = XtVaCreateManagedWidget(
        "pointslabel",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, otherLabel,
        XtNfromHoriz, skipText,
        XtNlabel, "Points: ",
        NULL );

   pointsText = XtVaCreateManagedWidget(
        "pointstext",
        asciiTextWidgetClass,
        phaseForm,
        XtNfromVert, otherLabel,
        XtNfromHoriz, pointsLabel,
        XtNstring, &points_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,				    
        NULL );


   /*
    * apodisation
    */
   apodizeLabel = XtVaCreateManagedWidget(
        "apodizelabel",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, skipLabel,
        XtNlabel, "Apodize:",
        NULL );

   apodizeText = XtVaCreateManagedWidget(
        "apodizetext",
        asciiTextWidgetClass,
        phaseForm,
        XtNfromVert, skipLabel,
        XtNfromHoriz, apodizeLabel,
        XtNstring, &apodize_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,				    
        NULL );

   /*
    * process center only or all
    */
   centerToggleButton = XtVaCreateManagedWidget(
        "centertoggle",
        toggleWidgetClass,
        phaseForm,
        XtNfromVert, pointsLabel,
        XtNfromHoriz, apodizeText,
        XtNlabel, "Central points only",
        NULL );
   XtAddCallback(centerToggleButton, XtNcallback, CenterToggleCB, 0 );
   center_read = FALSE;


   /*
    * wavelimits for transform
    */
   limitsLabel = XtVaCreateManagedWidget(
        "limitslabel",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, apodizeLabel,
        XtNlabel, "Range:  ",
        NULL );

   leftText = XtVaCreateManagedWidget(
        "lefttext",
        asciiTextWidgetClass,
        phaseForm,
        XtNfromVert, apodizeLabel,
        XtNfromHoriz, limitsLabel,
        XtNstring, &range_left_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,				    
        NULL );

   rightText = XtVaCreateManagedWidget(
        "righttext",
        asciiTextWidgetClass,
        phaseForm,
        XtNfromVert, apodizeLabel,
        XtNfromHoriz, leftText,
        XtNstring, &range_right_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,				    
        NULL );


   /*
    * output type
    */
   outputCommand = XtVaCreateManagedWidget(
        "outputcommand",
        commandWidgetClass,
        phaseForm,
        XtNfromVert, limitsLabel,
        XtNlabel, "Output:",
        NULL );
   XtAddCallback( outputCommand, XtNcallback, OutputCB, 0 );
   output_type = CPLX_OUT;

   outputLabel = XtVaCreateManagedWidget(
        "outputlabel",
        labelWidgetClass,
        phaseForm,
        XtNfromVert, limitsLabel,
        XtNfromHoriz, outputCommand,
        XtNlabel, "complex spectrum",
        NULL );

   outputFile = XtVaCreateManagedWidget(
        "outputfile",
        asciiTextWidgetClass,
        phaseForm,
        XtNfromVert, limitsLabel,
        XtNfromHoriz, outputLabel,
        XtNstring, &output_str,
        XtNlength, FILE_NAME_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,				    
        NULL );


   /*
    * holds the close / fill in / phase correct / transform buttons
    */
   buttonBox = XtVaCreateManagedWidget(
        "buttonbox",
        boxWidgetClass,
        phaseForm,
        XtNfromVert, outputLabel,
        XtNorientation, XtorientHorizontal,
        NULL );

   closeButton = XtVaCreateManagedWidget(
        "closebutton",
        commandWidgetClass,
        buttonBox,
        XtNlabel, "Close",
        NULL );
   XtAddCallback( closeButton, XtNcallback, ClosePhaseWindow, 0 );

   fillinButton = XtVaCreateManagedWidget(
        "fillinbutton",
        commandWidgetClass,
        buttonBox,
        XtNlabel, "Fill in",
        NULL );
   XtAddCallback( fillinButton, XtNcallback, FillinCB, 0 );

   phaseButton = XtVaCreateManagedWidget(
        "phasebutton",
        commandWidgetClass,
        buttonBox,
        XtNlabel, "Phase curve",
        NULL );
   XtAddCallback( phaseButton, XtNcallback, PhasecorrectCB, 0 );

   transformButton = XtVaCreateManagedWidget(
        "transformbutton",
        commandWidgetClass,
        buttonBox,
        XtNlabel, "Fourier transform",
        NULL );
   XtAddCallback( transformButton, XtNcallback, TransformCB, 0 );

   /*
    * bring window to life
    */
   XtRealizeWidget( phaseShell );
 
   /*
    * attach the icon and the button bitmaps
    */
   icon = XCreateBitmapFromData(XtDisplay(phaseShell),
                                XtWindow(phaseShell),
                                Xgremlin_phase_bits,
                                Xgremlin_phase_width, Xgremlin_phase_height);
   XtSetArg( args[0], XtNiconPixmap, icon);
   XtSetValues( phaseShell, args, 1);

   count_up = XCreateBitmapFromData(XtDisplay(phaseForm),
				    XtWindow(phaseForm),
				    count_up_bits,
				    count_up_width, count_up_height);
   XtSetArg( args[0], XtNbitmap, count_up);
   XtSetValues( centerUpButton, args, 1);
   XtSetValues( termsUpButton, args, 1);

   count_down = XCreateBitmapFromData(XtDisplay(phaseForm),
				    XtWindow(phaseForm),
				    count_down_bits,
				    count_down_width, count_down_height);
   XtSetArg( args[0], XtNbitmap, count_down);
   XtSetValues( centerDownButton, args, 1);
   XtSetValues( termsDownButton, args, 1);
}

/*-------------------------------------------------------------*/ 

/* Revision history:
 * -----------------
 * $Log: Phase.c,v $
 * Revision 1.21  1996/07/14 02:33:19  ulf
 * fixed a bug
 *
 * Revision 1.20  1996/07/14 02:32:24  ulf
 * run batch files for phase correction and Fourier transforms also in
 * debugging mode instead of only writing out the batch files.
 *
 * Revision 1.19  1996/07/11 04:08:52  ulf
 * cosmetic fix
 *
 * Revision 1.18  1996/06/15 02:57:12  ulf
 * fixed a bug in the CenterUp/Down callbacks which would lead to a nonsense center
 * to be filled in to the text field if one of the buttens was pressed and the field
 * was empty.
 *
 * Revision 1.17  1996/06/02 01:25:19  ulf
 * add an output option to allow creation of both real and complex spectra
 * together after a 'transform' command
 *
 * Revision 1.16  1996/06/01 21:37:33  ulf
 * fixed a bug in memset( output_str ....
 *
 * Revision 1.15  1996/03/03 13:33:02  ulf
 * replace calls to 'dispatch' by calls to 'runcmd' which supports
 * dynamically allocated arrays
 *
 * Revision 1.14  1996/01/30  17:47:32  ulf
 * include a 'ncenter ...' command in the transform batch file. If it is
 * missing the program crashes whenever a FT is repeated.
 *
 * Revision 1.13  1996/01/29  16:46:13  ulf
 * moved definition of  fncpinfo_  to FortranWrappers.h
 *
 * Revision 1.12  1996/01/29  15:15:54  ulf
 * fix a few typos and a bug
 *
 * Revision 1.11  1996/01/29  14:59:25  ulf
 * added a button to toggle phase fitting on and off which is handy when phase
 * correcting many spectra in bulk.
 *
 * Revision 1.10  1995/11/24  16:02:34  ulf
 * display a prompt after phase correcting/fourier transforming
 *
 * Revision 1.9  1995/10/10  11:31:44  ulf
 * New text input field in 'Phase / Transform' menu to specify the number
 * of points to be read with the 'input' command.
 *
 * Revision 1.8  1995/09/25  12:14:14  ulf
 * Fixed a bug in return value of 'string_contains_char'
 *
 * Revision 1.7  1995/09/25  11:46:28  ulf
 * forgot to unlink used batch files.
 *
 * Revision 1.6  1995/09/24  01:54:16  ulf
 * do not write a skip command into the transform batch if no
 * parameter was typed in
 *
 * Revision 1.5  1995/09/23  23:59:51  ulf
 * add default values for discriminator and signal to noise ratio
 *
 * Revision 1.4  1995/09/23  21:17:00  ulf
 * re-arrange some of the labels in the phase correction window
 *
 * Revision 1.3  1995/09/23  04:44:51  ulf
 * more cosmetic fixes to prevent clipping of labels
 *
 * Revision 1.2  1995/09/23  04:39:34  ulf
 * cosmetic fix
 *
 * Revision 1.1  1995/09/22  23:04:17  ulf
 * Initial revision
 *
 */

