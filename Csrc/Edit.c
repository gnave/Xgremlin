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

#include <X11/Intrinsic.h>     /* X toolkit intrinsics definitions */
#include <X11/StringDefs.h>    /* Standard Name-String definitions */
#include <X11/keysym.h>
#include <X11/Xaw/AsciiText.h>

#include <stdio.h>
#include <string.h>
#include <setjmp.h>

#include "F77_types.h"
#include "defaults.h"
#include "Buttons.h"
#include "FortranWrappers.h"
#include "Miscellaneous.h"
#include "Edit.h"
#include "List.h"
#include "SignalHandlers.h"

#define COL_SPACE  4    /* space between columns when printing a directory */
#define LINE_LEN   256  /* output line length */

extern Widget gremlinTop;

static Widget editor;                     /* local copy of editor widget */
static int num_prompt = 0;                /* number of input line */
static XawTextPosition current_pos = 0;   /* position of point after new prompt */
static char *buffer;                      /* editor buffer */

static char cmd_mbx[MAX_LINE_LEN];        /* mailbox used in finding old commands */
static int len_mbx;

static XawTextBlock text_line;            /* for inserting new text into widget */
static char nothing[] = "";               /* an empty string */


/*
 * status buffer used to store the stack contents pior to dispatching a command.
 * The status information is used by the signal handlers to return from a 
 * broken command.
 */
extern jmp_buf proc_status;


/*
 * a new prompt is required after some output has occured into the text window
 * after a command was dispatched from a button or plot mode key-command.
 */
static int text_output = FALSE;


/*
 * after an interactive 'run' command a new prompt is not required
 * when the 'break' in the batch file is hit. ( this is gettin' messy ... )
 */
static int nl_typed = FALSE;


/*
 * when a RecallCommand (arrow up or down) is followed by another 
 * RecallCommand the contents of the command line must not be used
 * for command completion. A flag is needed to indicate that a command
 * has been recalled.
 */
static int command_recalled = FALSE;


/* 
 * the stack of old commands is implemented as a dynamically linked list
 */
static tList command_stack;


/*
 * flags TRUE when 'prevline' was called
 */
static int prev_line = FALSE;


/*
 * a list of files for the directory lister
 */
static tList file_list;
static int maxlen_mbx;

static void
find_maxlen( char s[] );

/*-----------------------------------------------------------------*/
/* 
 * Local Prototypes
 *
 * this function is used to empty the editor buffer partially as soon 
 * as it reaches its high water mark. It should not be used outside this
 * module.
 */
static void 
remove_old_lines();

/*
 * this funcion looks up a matching command in the buffer of old commands
 * and replaces the argument with the completed command. The parameter
 * direction sets the search direction (BACKWARD, FORWARD) in the buffer.
 */
static int
complete_command( int direction, char cmd[] );

/*
 * searches the list of old commands for a matching one
 */
static int
search_command( char cmd[] );


/*-----------------------------------------------------------------*/

void 
InitializeEdit( Widget editorWidget, char *editor_buffer )        
{ 
   Arg args[10];
   int num;

   editor = editorWidget;          /* save the widget locally */
   buffer = editor_buffer;         /* store pointer to the editor buffer */
   num = 0;
   XtSetArg(args[num], XtNtype, XawAsciiString);    num++;
   XtSetArg(args[num], XtNdisplayCaret,  True);     num++;
   XtSetArg(args[num], XtNscrollVertical, XawtextScrollAlways);     num++;
   XtSetArg(args[num], XtNwrap, XawtextWrapNever);  num++;  /* looks best */
   XtSetArg(args[num], XtNeditType, XawtextEdit);   num++;
   XtSetValues( editor, args, num );

   /* 
    * create the command stack ( should never fail )
    */
   if ( create_list( &command_stack ) == -1 )
     {
	fprintf( stderr, " Error :  failed to initialize command stack.\n" );
	exit(-1);
     }

   /*
    * create file list
    */
   if ( create_list( &file_list ) == -1 )
     {
	fprintf( stderr, " Error :  failed to initialize file list.\n" );
	exit(-1);
     }

   /*
    * install signal handler for floating point exceptions in commands
    */
   install_signal_handlers();
}

/*-----------------------------------------------------------------*/

void 
FocusToEdit( ) 
{   
   XtSetKeyboardFocus( gremlinTop, editor ); /* focus to editor */
}

/*-----------------------------------------------------------------*/

void 
ExecCommand( Widget w, XEvent *ev, String *par, Cardinal *npar ) 
{
   XawTextPosition new_pos;
   int len;
   char *cp;
   char cmd_line[MAX_LINE_LEN];                        /* command line */

   new_pos = XawTextGetInsertionPoint( editor );       /* after typing a line */
   len = new_pos - current_pos;
   if ( len < 0 )                                      /* avoid a crash */
     {
	XawTextSetInsertionPoint( editor, current_pos );
	return;
     }
   cp = buffer + current_pos;
   current_pos = new_pos;                              /* update point shadow */
   strncpy( cmd_line, cp, len );
   cmd_line[len] = '\0';
   Chop( cmd_line );                                   /* remove \n */

   if ( cmd_line[0] != '\0')                           /* filter out blank lines */
     {                      
	if ( list_entries(command_stack) > OLD_COMMANDS )
	  chop_list_tail( command_stack );             /* discard oldest */

	list_head(command_stack);                      /* insert at beginning */
	list_insert_object(command_stack, cmd_line, MAX_LINE_LEN, BEFORE);

	StrCToFor( cmd_line, MAX_LINE_LEN );           /* prepare Fortran string */
	nl_typed = TRUE;
	if ( setjmp(proc_status) == 0 )                /* for signal handlers */
	  runcmd_( cmd_line, MAX_LINE_LEN );           /* dispatch the command */
	else
	  {
	     WriteStr( "\n Warning :  last command was not completed\n\n" );
	     unlkbd_();                                /* allow further commands */
	  }
	DisplayPrompt( INCREMENT );             
	command_recalled = FALSE;                      /* clear flag */
     }
   else
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

void 
RecallCommand( Widget w, XEvent *ev, String *par, Cardinal *npar ) 
{
   int len;
   char *cp;
   char cmd_line[MAX_LINE_LEN];                      /* command line */
   int direction;
   XKeyEvent *ke;
   KeySym key;
   Modifiers m_out;

   /*
    * figure out what key was pressed and determine direction of search
    */
   ke = (XKeyEvent *)ev;
   XtTranslateKeycode(ke->display, ke->keycode,
                      (Modifiers)ke->state, &m_out, &key);
   
   if( key == XK_Up )
     direction = BACKWARD;
   if( key == XK_Down )
     direction = FORWARD;

   /*
    * as in the ExecCommand function get the partly typed command
    */
   len = XawTextGetInsertionPoint( editor ) - current_pos;     

   /*
    * check if cursor was positioned in the last line
    */
   if ( len < 0 )
     {
	XawTextSetInsertionPoint( editor, current_pos );
	return;
     }

   cp = buffer + current_pos;         /* pointer to the command */
   strncpy( cmd_line, cp, len );
   cmd_line[len] = '\0';

   if ( complete_command( direction, cmd_line ) == TRUE )
     {
	len = strlen(cmd_line);

	/*
	 * replace previous string from command line
	 */
	if ( len != 0 ) 
	  {
	     text_line.firstPos = 0;
	     text_line.length   = len;
	     text_line.ptr      = cmd_line;
	     text_line.format   = FMT8BIT;
	     XawTextReplace( editor, current_pos, 
  			     current_pos+MAX_LINE_LEN, &text_line );
	  }
	XawTextSetInsertionPoint( editor, current_pos );
     }
}

/*-----------------------------------------------------------------*/

void 
TabAction( Widget w, XEvent *ev, String *parm, Cardinal *nparm ) 
{
   TogglePlotEdit();
}

/*-----------------------------------------------------------------*/

void 
BackSpace( Widget w, XEvent *ev, String *parm, Cardinal *nparm )
{
   XawTextPosition pos;

   pos = XawTextGetInsertionPoint( editor );     
   if ( pos > current_pos ) /* delete character to left of cursor */
     {
	pos--;
	text_line.firstPos = 0;
	text_line.length   = 0;
	text_line.ptr      = nothing;
	text_line.format   = FMT8BIT;
	XawTextReplace( editor, pos, pos+1, &text_line );
	XawTextSetInsertionPoint( editor, pos );
     }
}

/*-----------------------------------------------------------------*/

void 
ArrowLeft( Widget w, XEvent *ev, String *parm, Cardinal *nparm )
{
   XawTextPosition pos;

   pos = XawTextGetInsertionPoint( editor );     
   if ( pos > current_pos )  /* move cursor to the left */
     {
	pos--;
	XawTextSetInsertionPoint( editor, pos );
     }
}

/*-----------------------------------------------------------------*/

void 
BeginLine( Widget w, XEvent *ev , String *parm, Cardinal *nparm )
{
   /*
    * Note:
    * 'current_pos' is the position of the insertion point
    * after a new prompt has been printed. 'current_pos' is
    * updated every time a line is completed by typing <ret>
    */
   XawTextSetInsertionPoint( editor, current_pos );
}

/*-----------------------------------------------------------------*/

void 
DisplayPrompt( int inc ) 
{
   char prompt[10];

   if ( inc == INCREMENT ) 
     num_prompt++;
   sprintf( prompt, "[%d]: ", num_prompt );
   WriteStr( prompt );
   text_output = FALSE;
   nl_typed = FALSE;
}
   
/*-----------------------------------------------------------------*/

void 
WriteStr( char str[] ) 
{
   int len;

   /*
    * insert the string at the end of the text in the window
    */
   len  = strlen(str);
   text_line.firstPos = 0;
   text_line.length   = len;
   text_line.ptr      = str;
   text_line.format   = FMT8BIT;
   XawTextReplace( editor, current_pos, current_pos, &text_line );
   current_pos += len;                 /* advance to new pointer position */

   /*
    * discard old text if high water mark was crossed
    */
   if (current_pos >= HIGH_WATER)
     remove_old_lines();

   XawTextSetInsertionPoint( editor, current_pos );
   text_output = TRUE;
}

/*-----------------------------------------------------------------*/

void 
wrtstr_( char str[], ftnlen len ) 
{
   char linbuff[MAX_LINE_LEN];
   int i;
   int l;

   memcpy( linbuff, str, (int)len );   /* put Fortran string in buffer */
   for ( l=(int)len-1; l>=0; l-- )     /* \0 - terminate the string */
     if ( linbuff[l] != ' ' ) 
       break;

   if ( l < MAX_LINE_LEN-1 )           /* emulate Fortran behaviour */
     {
	i = l;
	linbuff[++i] = '\n';              
	linbuff[++i] = '\0';
     }
   else
     {
	linbuff[MAX_LINE_LEN-2] = '\n';
	linbuff[MAX_LINE_LEN-1] = '\0';
     }

   for ( i=0; i<=l; i++ )              /* see comment below */
     if ( linbuff[i] == '\0' )
       linbuff[i] = ' ';

   WriteStr( linbuff );                /* put it in the editor buffer */
   prev_line = FALSE;                  /* end overwrite mode */
}

/*
 * Note:
 * the DOS version of gremlin has a flaw in that no strings are initialized.
 * As a result strings may be padded with \0 instead of ' ' as it should
 * be when they arrive in  'wrtstr'. This may cause core dumps.
 * Especially older .lin files suffer from this flaw.
 */

/*-----------------------------------------------------------------*/

void
prevline_( char line[], ftnlen len )
{
   char linbuff[MAX_LINE_LEN];
   char *cp;
   int prev_pos;
   int linlen;

   memcpy( linbuff, line, (int)len ); 
   StrForToC( linbuff, len );
   
   if ( prev_line == FALSE )
     {
	strcat( linbuff, "\n" );
	WriteStr( linbuff );
	prev_line = TRUE;
     }
   else
     {
	/*
	 * backtrack to the beginning of the previous line
	 */
	prev_pos = current_pos - 2;
	cp = buffer + prev_pos;       /* end of prev line */
	while( *cp != '\n' && cp != buffer )
	  {
	     cp--;
	     prev_pos--;
	  }
	if ( cp != buffer )
	  {
	     cp++;                         /* keep that \n */
	     prev_pos++;
	  }

	/*
	 * replace previous line
	 */
	linlen = strlen( linbuff );
	text_line.firstPos = 0;
	text_line.length   = linlen;
	text_line.ptr      = linbuff;
	text_line.format   = FMT8BIT;
	XawTextReplace( editor, prev_pos, current_pos-1, &text_line );
	current_pos = prev_pos + linlen + 1;
     }
}

/*-----------------------------------------------------------------*/

void 
DisplayStr( int on_or_off ) 
{
   if ( on_or_off == ON )
     XawTextEnableRedisplay( editor );
   if ( on_or_off == OFF )
     XawTextDisableRedisplay( editor );
}

/*-----------------------------------------------------------------*/

void 
dspstr_( int *ioo ) 
{
   if ( *ioo == 0 )
     DisplayStr( OFF );
   if ( *ioo == 1 )
     DisplayStr( ON );
}

/*-----------------------------------------------------------------*/

static void 
remove_old_lines()
{   
   int num;
   int nkeep;
   char *cp;

   DisplayStr( OFF );
   cp = buffer + EDIT_BUFF_LEN/2;        /* middle of buffer */
   while (*cp++ != '\n');                /* seek past next \n */
   nkeep = strlen( cp );
   num = strlen(buffer) - nkeep;         /* number of chars to discard */
   text_line.firstPos = 0;
   text_line.length   = 0;
   text_line.ptr      = nothing;
   text_line.format   = FMT8BIT;
   XawTextReplace( editor, 0, num, &text_line );
   current_pos = nkeep;
   XawTextSetInsertionPoint( editor, current_pos );
   DisplayStr( ON );
}

/*-----------------------------------------------------------------*/

static int
complete_command( int direction, char cmd[] )
{
   int len;
   char *cp;

   /*
    * the command stack is empty directly after starting Xgremlin,
    * trap this case here.
    */
   if ( list_entries(command_stack) == 0 )
     return FALSE;

   len = strlen( cmd );
   if ( len == 0 || command_recalled == TRUE )    
     {
	if ( direction == BACKWARD )
	  {
	     /* next function also increments the list pointer */
	     get_current_object( command_stack, cmd, MAX_LINE_LEN );
	  }
	else
	  {
	     list_prev( command_stack );
	     get_current_object( command_stack, cmd, MAX_LINE_LEN );
	     list_prev( command_stack );
	  }
	command_recalled = TRUE;
	return TRUE;
     }
   else
     {
	/*
	 * scan the list for an old matching command
	 */
	strcpy( cmd_mbx, cmd );     /* put command in mailbox for search_command */
	len_mbx = len;
	if ((cp=(char *)search_list(command_stack,SEARCH_PROC(search_command)))==NULL )
	  return FALSE;
	else
	  {
	     strcpy( cmd, cp );
	     command_recalled = TRUE;
	     return TRUE;
	  }
     }
}

/*-----------------------------------------------------------------*/

static int
search_command( char cmd[] )
{
   if ( strncmp( cmd, cmd_mbx, len_mbx ) == 0 )
     return 1;
   else
     return 0;
}

/*-----------------------------------------------------------------*/

int
TextOutput()
{
   return text_output;
}

/*-----------------------------------------------------------------*/

void
newprom_()
{
   if ( TextOutput() == TRUE && nl_typed == FALSE )
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

void
lsdir_()
{
   int maxlen;
   int len;
   Dimension text_width;
   int char_width;
   int num_col;     /* number of columns in listing */
   int cur_col;
   int ne;
   int k;
   XFontStruct *font;
   char *cp;
   char blanks[LINE_LEN];
   char linbuf[LINE_LEN];
   
   current_dir_files( file_list );
   if ( (ne = list_entries(file_list)) > 0 )
     {
	/*
	 * find out length of longest name in the list
	 */
	maxlen_mbx = 0;
	scan_list( file_list, SCAN_PROC(find_maxlen) );
	maxlen = maxlen_mbx + COL_SPACE; /* add some space between columns */

	/*
	 * find out how many names currently fit on a line
	 */
	XtVaGetValues( editor, XtNwidth, &text_width, 
                               XtNfont,  &font,
		               NULL );
	char_width = font->max_bounds.width;
	num_col = text_width / ( (maxlen + COL_SPACE) * char_width );

	/*
	 * print at least one column
	 */
	if ( num_col <= 0 )
	  num_col = 1;

	/*
	 * print 'em out in columns
	 */
	list_head( file_list );
	cur_col = 1;
	strcpy( linbuf, " " );
	for ( k=0; k<ne; k++ )
	  {
	     get_current_entry( file_list, (void *)&cp );
	     len = strlen(cp);
	     strcat( linbuf, cp );
	     memset(blanks, ' ', maxlen-len );
	     blanks[maxlen-len] = '\0';
	     strcat( linbuf, blanks );
	     if ( cur_col == num_col )
	       {
		  strcat( linbuf, "\n" );
		  WriteStr( linbuf );
		  strcpy( linbuf, " " );
		  cur_col = 1;
	       }
	     else
	       cur_col++;
	  }
	if ( cur_col > 1 )       /* write out last line */
	  {
	     strcat( linbuf, "\n" );
	     WriteStr( linbuf );
	  }
	if ( ne == 1 )
	  sprintf( linbuf, " 1 file\n" );
	else
	  sprintf( linbuf, " %d files\n", ne );
	WriteStr( linbuf );
     }
   else
     {
	WriteStr( " no files\n" );
     }
}

/*-----------------------------------------------------------------*/

static void
find_maxlen( char s[] )
{
   int len;

   len = strlen(s);
   if ( len > maxlen_mbx )
     maxlen_mbx = len;
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * ----------------
 * $Log: Edit.c,v $
 * Revision 1.18  1996/04/28 01:13:06  ulf
 * the loop in 'wrtstr_' which filters out stray \0 characters had
 * to be re-instated because some older .lin files have them in the
 * tag field.
 *
 * Revision 1.17  1996/03/03 14:03:41  ulf
 * use 'runcmd' instead of 'dispatch'
 *
 * Revision 1.16  1996/03/02  16:03:54  ulf
 * added global array parameters to command dispatcher.
 *
 * Revision 1.15  1996/01/19  16:41:20  ulf
 * speed up wrtstr subroutine by removing the loop to catch uninitialized strings.
 *
 * Revision 1.14  1995/10/03  08:16:35  ulf
 * turn on keyboard command input after a signal was caught. This is necessary
 * because control will not reach the end of function 'dispatch' in case of
 * an exception.
 *
 * Revision 1.13  1995/09/23  23:13:18  ulf
 * tell editor widget to allways scroll vertically (hard coded)
 *
 * Revision 1.12  1995/09/21  13:29:36  ulf
 * define variable 'jmp_buf' in 'SignalHandlers.c'
 *
 * Revision 1.11  1995/09/10  01:04:28  ulf
 * fixed the calculation of number of columns.
 *
 * Revision 1.10  1995/09/09  22:12:40  ulf
 * fixed a glitch in the recall command: the text window is not repainted any longer
 * when an old command is recalled --> no flicker.
 *
 * Revision 1.9  1995/07/12  02:59:16  ulf
 * added function prevline_
 *
 * Revision 1.8  1995/07/09  01:47:29  ulf
 * use setjmp/longjmp and an exception handler to catch floating point
 * exceptions in commands. If a floating point exception occurs in a command
 * the program returns to the function 'ExecCommand'.
 *
 * Revision 1.7  1995/07/04  23:28:06  ulf
 * Added functions (actions)  BackSpace, ArrowLeft, BeginLine.
 *
 * Revision 1.6  1995/07/04  04:10:21  ulf
 * remove unused variable 'l'.
 *
 * Revision 1.5  1995/06/26  00:15:18  ulf
 * write out "no files" if the 'ls' command was typed in an empty directory.
 *
 * Revision 1.4  1995/06/25  23:26:17  ulf
 * Variable RecallCommand:direction remains uninitialized now
 *
 * Revision 1.3  1995/06/23  03:06:31  ulf
 * forgot to print out last line in an 'ls' command if the line was not full.
 *
 * Revision 1.2  1995/06/18  01:50:48  ulf
 * make 'lsdir' more efficient.
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */
