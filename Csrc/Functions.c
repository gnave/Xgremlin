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
#include <X11/Xaw/MenuButton.h>

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "defaults.h"
#include "Edit.h"
#include "Buttons.h"
#include "Functions.h"
#include "Miscellaneous.h"
#include "FortranWrappers.h"


typedef struct {
   char type;                           /* 'e' = edit, 'p' = plot, 'n' none defined */
   Widget w;                            /* the button widget */
   union {                              /* edit command */
      struct {
	 char cmd_string[CMD_LEN];      /* command string */
	 int len;
      } edit;
      struct {                          /* plot command */
	 int key;                       /* keystroke */
	 int shft;                      /* modifiers */
	 int ctrl;
	 int meta;
      } plot;
   } u;
} Command;

/*
 * a table with commands stored with the 'button' command
 */
static Command cmd_table[NUM_FUNC_BUTTONS];


/*-----------------------------------------------------------------*/

void
InitFunctions( int nbut, Widget w )
{
   int na;
   Arg args[4];

   cmd_table[nbut].type = 'n';
   cmd_table[nbut].w    = w;
   
   /*
    * set up default look for the button (label 'none' )
    */
   na = 0;
   XtSetArg( args[na], XtNlabel, "none" );      na++;
   XtSetValues( w, args, na );
}

/*-----------------------------------------------------------------*/

void 
FunctionChoice(Widget w, XtPointer button, XtPointer call_data)
{
   int ib;
   char cmd[CMD_LEN];

   if ( ButtonsBlocked() == TRUE )    /* during a lengthy calculation */
     return;

   ib = (int)button;                  /* button is an integer */
   switch( cmd_table[ib].type )    
     {
      /*
       * no command assigned to button, nothing to do
       */
      case 'n': 
	return;
	break;
	
      /*
       * dispatch a plot mode command
       */
      case 'p':
	keycmd_(&cmd_table[ib].u.plot.key,
		&cmd_table[ib].u.plot.shft,
		&cmd_table[ib].u.plot.ctrl,
		&cmd_table[ib].u.plot.meta);
	if ( TextOutput() == TRUE )
	  DisplayPrompt( NOINCREMENT );
	break;

      /*
       * dispatch an edit mode command
       */
      case 'e':
	strcpy( cmd, cmd_table[ib].u.edit.cmd_string );
	StrCToFor( cmd, CMD_LEN );
	runcmd_( cmd, CMD_LEN );
	if ( TextOutput() == TRUE )
	  DisplayPrompt( NOINCREMENT );
	break;
     }
}

/*-----------------------------------------------------------------*/

void
butplot_( int *numbut, char cmd[], char lbl[], ftnlen lenc, ftnlen lenl )
{
   Arg args[2];
   int l, ia, ib;
   int shift, ctrl, meta, ch;
   char label[LABEL_LEN];
   char command[4];
   
   ib = *numbut - 1;                    /* we number buttons from 1..8 */
   if ( ib < 0 || ib > NUM_FUNC_BUTTONS-1 )
     {
	WriteStr( "\n Error :  button does not exist.\n" );
	return;
     }

   /*
    * decode the command
    */
   for ( l=0; l<4; l++ )        /* make a local copy + count characters */
     {
	command[l] = cmd[l];
	if ( cmd[l] == ' ' )
	  {
	     command[l] = '\0'; /* l is length of command */
	     break;
	  }
     }
   shift = ctrl = meta = 0;
   switch( command[0] )
     {
      case 'S':
	shift = 1;
	break;
      case 'C':
	ctrl = 1;
	break;
      case 'M':
	meta = 1;
	break;
      default:                            /* is must be a command without modifier */
	ch = tolower( (int)command[0] );  /* allow case error */
     }

   if ( l > 1 )                           /* command with modifier */
     {
	if ( cmd[1] != '+' ) 
	  {
	     WriteStr( "\n Error :  invalid plot command.\n" );
	     return;
	  }
	ch = tolower( (int)cmd[2] );      /* allow case error */
     }

   cmd_table[ib].type = 'p';
   cmd_table[ib].u.plot.key  = ch;
   cmd_table[ib].u.plot.shft = shift;
   cmd_table[ib].u.plot.ctrl = ctrl;
   cmd_table[ib].u.plot.meta = meta;
   strncpy( label, lbl, lenl );
   StrForToC( label, lenl );
   
   ia = 0;
   XtSetArg( args[ia], XtNlabel, label );      ia++;
   XtSetValues( cmd_table[ib].w, args, ia );
}

/*-----------------------------------------------------------------*/

void
butedit_( int *numbut, char cmd[], char lbl[], ftnlen lenc, ftnlen lenl )
{
   Arg args[2];
   int ia, ib;
   char label[LABEL_LEN];
   
   ib = *numbut - 1;                    /* we number buttons from 1..8 */
   if ( ib < 0 || ib > NUM_FUNC_BUTTONS-1 )
     {
	WriteStr( "\n Error :  button does not exist.\n" );
	return;
     }
   cmd_table[ib].type = 'e';
   strncpy( cmd_table[ib].u.edit.cmd_string, cmd, lenc );
   strncpy( label, lbl, lenl );
   StrForToC( label, lenl );
   
   ia = 0;
   XtSetArg( args[ia], XtNlabel, label );      ia++;
   XtSetValues( cmd_table[ib].w, args, ia );
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * -----------------
 * $Log: Functions.c,v $
 * Revision 1.4  1996/03/03  13:57:58  ulf
 * use new functions 'runcmd' and 'keycmd' for command dispatch
 *
 * Revision 1.3  1995/06/27  04:41:10  ulf
 * Function 'butplot_' now accepts a command string of the form <modifier>+<key>
 * instead of command character and a set of flags.
 *
 * Revision 1.2  1995/06/18  01:49:09  ulf
 * assignment to non-existent button does not crash any more, buttons are numbered
 * from 1 - ... at Xgremlin top level.
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */
