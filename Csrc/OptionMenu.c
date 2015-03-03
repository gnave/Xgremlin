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
#include <X11/Shell.h>

#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/MenuButton.h>

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>

#include "Help.h"
#include "Phase.h"
#include "Hfs.h"
#include "OptionMenu.h"
#include "Miscellaneous.h"
#include "PlotMode.h"
#include "FortranWrappers.h"
#include "Edit.h"
#include "defaults.h"

extern char xgremlin_version[];   /* version string */

extern Widget optionMenu;         /* defined in  xgremlin.c */
extern Widget gremlinTop;         /* defined in  xgremlin.c */

static Widget verShell, 
              verDialog,
              verButton;

/*
 * printer name
 */
static char printer_name[32] = "none";
static char printer_cmd[64];


/*--------------- Local prototypes --------------------------------*/

static void 
VersionPopdown( Widget w, XtPointer client_data, XtPointer call_data);

static void 
PrintGraph();

static void 
Version();

static void 
Quit();

static void
CloseXgremlin();

/*-----------------------------------------------------------------*/

void MenuChoice(Widget w, XtPointer button, XtPointer call_data)
{
   switch ( (int)button ) {    /* button is known to be integer */
    
    case 0:  /* print graph */
      PrintGraph();
      break;

    case 1:  /* launch HTML browser with help text */
      StartHelp();
      break;

    case 2:  /* open phase correction window */
      OpenPhaseWindow();
      break;
      
   case 3:   /* open hfs fitting window */
      OpenHfsWindow();
      break;

    case 4:  /* print version information */
      Version();
      break;

    case 5:  /* quit the program */
      Quit();
      break;
   }
}

/*-----------------------------------------------------------------*/

void 
InitializeVersion( Widget versionButton ) 
{
   Arg args[2];
   int i = 0;

   /* 
    * set up a dialog box which displays the version number 
    */
   verShell = XtVaCreatePopupShell(
          "vershell",
          transientShellWidgetClass,
          gremlinTop,
          NULL); 

   verDialog = XtVaCreateManagedWidget(
          "verdialog", 
          dialogWidgetClass,  
          verShell,             
          NULL);    
   
   verButton = XtVaCreateManagedWidget(
          "verbutton", 
          commandWidgetClass,      
          verDialog,            
          NULL);         
   
   /* 
    * add a second callback to the (top level) version button 
    */
   XtAddCallback(versionButton, XtNcallback, XtCallbackExclusive, verShell); 
   XtAddCallback(verButton, XtNcallback, VersionPopdown, versionButton);

   /* 
    * hard wire label resource for verDialog.label and verButton.label
    */
   XtSetArg(args[i], XtNlabel, xgremlin_version); i++;
   XtSetValues(verDialog, args, i);
   i = 0;
   XtSetArg(args[i], XtNlabel, "      Return to Xgremlin      "); i++;
   XtSetValues(verButton, args, i);
}

/*-----------------------------------------------------------------*/

static void 
VersionPopdown( Widget w, XtPointer client_data, XtPointer call_data) 
{
   XtPopdown( verShell );
   XtSetSensitive( (Widget)client_data, TRUE );
}

/*-----------------------------------------------------------------*/

static void 
PrintGraph()
{
   char home_path[FILE_NAME_LEN];
   char plot_file[FILE_NAME_LEN];
   char plot_size[32];
   char cmd[FILE_NAME_LEN];        /* for printing command */
   char *ep;

   /*
    * close window
    */
   XtPopdown(optionMenu);

   /* 
    * construct full name of plot file include PID to make it unique
    */
   get_homedir( home_path );
   sprintf( plot_file, "%s/xgremlin-%d.ps", home_path, (int)getpid() );
   get_plotsize( plot_size );
   if ( plot_size[0] == ' ' )
     plot_size[0] = '\0';
   screen_dump( plot_file, plot_size, get_orientation() );

  /* 
   * next spawn 'lpr' in background to print the file.
   */
   if ( strcmp(printer_name, "none") != 0 )
     {
	ep = getenv( PRINT_ENV_COMMAND );
	if ( ep != NULL )
	  strcpy(printer_cmd, ep);	
	else
	  strcpy(printer_cmd, DEFAULT_PRINT_COMMAND);
	sprintf( cmd, "%s%s %s &", printer_cmd, printer_name, plot_file );
	system( cmd );
     }
}

/*-----------------------------------------------------------------*/

void
setprt_( char prname[], ftnlen len )
{   
   StrForToC( prname, len );
   strcpy( printer_name, prname );
}

/*-----------------------------------------------------------------*/

void
shwprt_()
{
   char buff[128];

   sprintf( buff, " current printer :  %s\n", printer_name );
   WriteStr( buff );
}

/*-----------------------------------------------------------------*/

/*
 * prepare the popup window with the version information
 */
static void 
Version()
{
   Position x, y;
   Dimension width, height;

   /*
    * get the coordinates of the middle of topLevel widget.
    */
   XtVaGetValues(gremlinTop,
		 XtNwidth, &width,
		 XtNheight, &height,
		 NULL);

   /*
    * translate coordinates in application top-level window
    * into coordinates from root window origin. Put it in upper left corner.
    */
   XtTranslateCoords(gremlinTop,           
                 (Position) width/3,   /* x */
		 (Position) height/3,  /* y */
		  &x, &y);             /* coords on root window */

   /* 
    * move popup shell to this position (it's not visible yet) 
    */
   XtVaSetValues(verShell,
		 XtNx, x,
		 XtNy, y,
		 NULL);
  
  /* 
   * remove the main menu before leaving 
   */
  XtPopdown(optionMenu);
}

/*-----------------------------------------------------------------*/

static void 
Quit()
{
   XtPopdown(optionMenu);
   dexit_();               /* leave through the normal exit */
}

/*-----------------------------------------------------------------*/

static void
CloseXgremlin()
{
   XtDestroyWidget( gremlinTop );
}

/*-----------------------------------------------------------------*/

void
closexgr_()
{
   CloseXgremlin();
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * ----------------
 * $Log: OptionMenu.c,v $
 * Revision 1.17  1996/05/12 01:43:30  ulf
 * changed  OpenHelpWindow() --> StartHelp()
 *
 * Revision 1.16  1996/03/12 18:06:43  ulf
 * fixed a bug in function PrintGraph
 *
 * Revision 1.15  1996/02/26  17:43:59  ulf
 * fixed type of variable 'xgremlin_version'
 *
 * Revision 1.14  1996/02/21  17:55:19  ulf
 * removed the initalization of 'printer_cmd' from initialization of version button
 * to make code clearer.
 *
 * Revision 1.13  1995/11/11  10:41:59  ulf
 * add plot orientation to call to function 'screen_dump'
 *
 * Revision 1.12  1995/10/10  10:40:43  ulf
 * allow print command to be changed with an environment variable
 * XGREMLIN_PRINT_COMMAND.
 *
 * Revision 1.11  1995/10/08  14:52:40  ulf
 * change default printer name to 'none'
 *
 * Revision 1.10  1995/10/08  14:47:47  ulf
 * don't print the Postscript file if printer has the name 'none'
 *
 * Revision 1.9  1995/10/08  14:44:50  ulf
 * print directly via 'lpr' not via the script 'xgremlin_print'
 *
 * Revision 1.8  1995/09/06  22:48:27  ulf
 * gremlinVersion --> xgremlin_version
 *
 * Revision 1.7  1995/09/06  01:42:02  ulf
 * defined 'gremlinVersion' extern
 *
 * Revision 1.6  1995/08/24  21:26:44  ulf
 * cosmetic fix.
 *
 * Revision 1.5  1995/08/23  02:33:46  ulf
 * add function 'shwprt' to display the currently set printer
 *
 * Revision 1.4  1995/08/20  04:29:40  ulf
 * The 'Quit' button now invokes the normal exit procedure 'dexit' in
 * the Fortran part of the program to make sure that all files are
 * flushed and closes correctly (especially the log file).
 *
 * Revision 1.3  1995/08/03  03:27:22  ulf
 * New menu item to open the help window.
 *
 * Revision 1.2  1995/07/07  21:11:15  ulf
 * new function setprt_ which allows to define the printer for screen dumps.
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */


