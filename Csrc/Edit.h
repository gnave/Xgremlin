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
 * Input / Output in the text pane of XGremlin
 * ( Note that much of the behaviour of the editor pane is determined by
 *   the resource settings )
 */

#ifndef _Edit_h
#define _Edit_h

#include "F77_types.h"

#define MAX_LINE_LEN  256                           /* max length of command line */
#define EDIT_BUFF_LEN 32*1024                       /* length of editor buffer */
#define HIGH_WATER    EDIT_BUFF_LEN - 4*MAX_LINE_LEN
#define OLD_COMMANDS 64                             /* number of old commands */
#define BACKWARD     1
#define FORWARD      2

enum prompt_type { NOINCREMENT, INCREMENT };

/*
 * initialize the editor window
 */
void 
InitializeEdit( Widget, char * );


/*
 * give the keyboard focus to the realized editor widget
 */
void 
FocusToEdit( );


/*
 * is called when the return key is pressed in the editor window. Retrieves
 * the last line that was typed in, passes it to Gremlin and finally calls
 * DisplayPrompt( INCREMENT ).
 */
void 
ExecCommand( Widget, XEvent *, String *, Cardinal * ); 


/*
 * the following function is called when the arrow up key is pressed in
 * editing mode. It recalls a matching old command from the command buffer.
 */
void
RecallCommand( Widget, XEvent *, String *, Cardinal * );


/*
 * function which is called when the TAB key is pressed to switch to
 * plotting mode.
 */
void 
TabAction( Widget, XEvent *, String *, Cardinal * );  


/*
 * this function is called when the backspace key is pressed in the 
 * text window. 
 */
void 
BackSpace( Widget, XEvent *, String *, Cardinal * );  


/*
 * this function is called when the arrow left key is pressed in the 
 * text window. 
 */
void 
ArrowLeft( Widget, XEvent *, String *, Cardinal * );  


/*
 * this function positions the cursor right behind the prompt at the
 * beginning of a line in the text window.
 */
void 
BeginLine( Widget, XEvent *, String *, Cardinal * );  


/*
 * display the next Xgremlin prompt. DisplayPrompt( INCREMENT ) increments
 * the prompt counter, DisplayPrompt( NOINCREMENT ) does not.
 */
void 
DisplayPrompt( int );


/*
 * write a string into the editor pane's buffer and display it.
 */
void 
WriteStr( char[] );


/*
 * this is a Fortran callable version of WriteString ( currently f2c only )
 * 
 * e.g.
 *      CHARACTER S(20)
 *      S='foobar'
 *      CALL WRTSTR( S )
 */
void 
wrtstr_( char[], ftnlen );


/*
 * overwrites the previously written line with a new line
 * ( the first call to 'prevline' after a call to 'wrtstr' does
 * not overwrite the previous line but works just as 'wrtstr' ).
 */
void
prevline_( char line[], ftnlen len );
 

/*
 * enables or disables redisplay in the text area
 */
void 
DisplayStr( int on_or_off);


/*
 * a Fortran callable version of DisplayStr
 *
 * CALL DSPSTR( 0 )
 * CALL DSPSTR( 1 )
 */
void 
dspstr_( int * ioo );


/*
 * returns TRUE if text output has occured during the execution of a command
 * ( i.e. a call to WriteStr ), FALSE otherwise.
 */
int
TextOutput();


/*
 * print a new prompt - Fortran callable
 */
void
newprom_();


/*
 * print a listing of the current directory
 */
void
lsdir_();


#endif /* _Edit_h */

/* Revision history:
 * ----------------
 * $Log: Edit.h,v $
 * Revision 1.5  1995/10/23  14:48:46  ulf
 * clean up #define-s
 *
 * Revision 1.4  1995/08/23  02:32:39  ulf
 * fixed description of 'WriteStr'
 *
 * Revision 1.3  1995/07/12  02:58:57  ulf
 * added function prevline_ for process indicators
 *
 * Revision 1.2  1995/07/04  23:24:57  ulf
 * added funtions BackSpace, ArrowLeft and BeginLine. The new functions are actions
 * bound to the respective keys and make sure that the prompt cannot be removed
 * any more and that the C+a moves the point behind the prompt.
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */

