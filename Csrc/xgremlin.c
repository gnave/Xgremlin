/*
 * Copyright (C) 1994, 1995, 1996
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
 * $Id: xgremlin.c,v 1.21 1997/03/28 03:36:51 ulf Exp $
 */

/*****************************************************************************
 *                                                                           *
 * Unix / X11 version of the Gremlin program for the analysis of FTS spectra *
 *                                                                           *
 *****************************************************************************/


#include <X11/Intrinsic.h>     /* X toolkit intrinsics definitions */
#include <X11/StringDefs.h>    /* Standard Name-String definitions */
#include <X11/Shell.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* we recommend: use 3d Athena Widgets (poor man's Motif) */
#include <X11/Xaw/AsciiText.h> 
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>   
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Toggle.h>
#include <Display.h>
#include <LiteClue.h>

/* local defines */
#include "defaults.h"
#include "OptionMenu.h"       
#include "Help.h"             
#include "Edit.h"
#include "Phase.h"
#include "Hfs.h"
#include "Functions.h"                   /* function buttons */
#include "Buttons.h"
#include "Miscellaneous.h"
#include "FortranWrappers.h"
#include "PlotMode.h"
#include "arralloc.h"

/*
 * built-in icons and other bitmaps
 */
#include "icons/Xgremlin.icon.xbm"       /* default monochrome bitmap icon */
#include "icons/arrow_left.xbm"          /* page button bitmaps */
#include "icons/arrow_right.xbm"
#include "icons/y_fullscale.xbm"
#include "icons/x_expand.xbm"
#include "icons/x_shrink.xbm"
#include "icons/y_expand.xbm"
#include "icons/y_shrink.xbm"
#include "icons/x_centre.xbm"
#include "icons/y_centre.xbm"
#include "icons/x_section.xbm"
#include "icons/y_section.xbm"
#include "icons/redraw.xbm"
#include "icons/undoplot.xbm"

#define NUM_MENU_BUTTONS  6
#define BUFFLEN           64
#define NAMELEN           128


/* 
 * a table of extra actions ( to be configured in applications default file )
 */
static XtActionsRec actions[] = {
   { "ExecCommand",   ExecCommand   },  /* execute a command */
   { "RecallCommand", RecallCommand },  /* recall an old command */
   { "TabAction",     TabAction     },  
   { "BackSpace",     BackSpace     },
   { "ArrowLeft",     ArrowLeft     },
   { "BeginLine",     BeginLine     },  /* go to beginning of line */
   { "Focus",         Focus         },
   { "Accept",        Accept        },
   { "hfsFocus",      hfsFocus      },
   { "hfsAccept",     hfsAccept     }
};


/*
 * global variables for the whole program
 */
Display *display;                     /* the display the program uses */
XtAppContext gremlinContext;           /* the application context */

Widget  gremlinTop;                   /* main widget */
Widget  liteClue;                     /* help for bubble help */
Widget  optionMenu;                   /* for the menu (needed in OptionMenu.c) */
Widget  gremlinPlot;                  /* the plotting area */
Widget  phaseButton;                  /* used in phase correction */
Widget  gremlinPane;                  /* split windows */
Widget  gremlinButtons;               /* box for menu bar */
Widget  gremlinFuncs;                 /* function button bar */
Widget  gremlinEdit;                  /* place of line editor */
Widget  optionButton;                 /* button to bring up option menu*/
Widget  menuBox;
Widget  menuItem[NUM_MENU_BUTTONS];
Widget  funcButton[NUM_FUNC_BUTTONS]; /* function button bar */
Widget  toggleButton;                 /* toggle edit / plot mode */
Widget  intrButton;                   /* the interrupt Button */
Widget  leftButton;                   /* buttons to scroll spectrum */
Widget  rightButton;
Widget  yfullButton;
Widget  xshrinkButton;
Widget  xexpandButton;
Widget  yshrinkButton;
Widget  yexpandButton;
Widget  xcentreButton;
Widget  ycentreButton;
Widget  xsectionButton;
Widget  ysectionButton;
Widget  redrawButton;
Widget  undoButton;


/*
 * the following global variable flags that a monochrome display is being used.
 * It can be set to TRUE with the '-monochrome' command line option.
 * No use is made of the Xt command line option mechanism.
 */
int monochrome_display = FALSE;


/*
 * Xgremlin uses a set of global data arrays (r,tr,ffta...) which are
 * allocated statically in Gremlin (and 1.x versions of Xgremlin). 
 * These arrays are now allocated dynamically and the global pointers
 * to these arrays are defined here.
 */
float *r = NULL;        /* r array */
float *tr = NULL;       /* tr array */
float *rtr = NULL;      /* r and tr combined */
float *phz = NULL;      /* phase array, upper half of rtr */
float *ffta = NULL;     /* ffta array */

/*
 * the following are for internal line lists
 */
double *point = NULL;   /* point array */
float *amp = NULL;      /* line intensity */
float *wd = NULL;       /* line width */
float *dmp = NULL;      /* damping */
float *eps1 = NULL;     /* various parameters */
float *eps2 = NULL;
float *eps3 = NULL;
float *eps4 = NULL;
float *eps5 = NULL;
int *nit = NULL;        /* number of iterations */
int *nhold = NULL;      /* for holding parameters */
char *ctag = NULL;      /* line tags */
char *dent = NULL;      /* line identifiers */

/*
 * sizes of dynamic arrays, only used for initialization
 */
static int r_init_size    = INITIAL_R_SIZE;
static int ffta_init_size = INITIAL_FFTA_SIZE;
static int lin_init_size  = INITIAL_LINELIST_SIZE;
static int ierror;

/*
 * the 'main' function of Xgremlin contains only boilerplate code to create 
 * windows and widgets (buttons, drawing area etc.) and to initialize all 
 * parts of the program. Then the program enters the main X event 
 * processing loop.
 */
int
main( int argc, char *argv[] ) 
{    
   int i;
   char *cp;
   char buffer[BUFFLEN];
   char domainName[NAMELEN];
   char titleName[NAMELEN];
   Pixmap icon, 
          arrow_left, arrow_right, 
          y_fullscale, 
          x_section, y_section,
          x_shrink, x_expand, 
          y_shrink, y_expand, 
          x_centre, y_centre, 
          redraw, undoplot;
   Arg args[2];
   static char *editor_buffer;          /* pointer to editor buffer */


   /* 
    * check for left-overs on the command line 
    */
   if ( argc > 1 ) {
      cp = argv[1];
      if ( *cp == '-' && *(++cp) == 'm' )
	monochrome_display = TRUE;
      else
	Syntax();
   }
   
   /* 
    * get the hostname for the title
    */
   GetHN( domainName );
   sprintf( titleName, "Xgremlin@%s", domainName );

   /* 
    * initialize the Xgremlin. Since the standard function 
    * XtVaAppInitialize does not allow the title of the application
    * to be set Xgremlin has to be initalized 'manually'.
    */
   XtToolkitInitialize();

   /*
    * XtSetLanguageProc( NULL, NULL, NULL );
    */
   gremlinContext = XtCreateApplicationContext();
   XtAppSetFallbackResources( gremlinContext, NULL );  /* should be better */
   display = XtOpenDisplay( gremlinContext, 
                            NULL,                      /* default display name */ 
                            titleName,                 /* window title bar */
                            "Xgremlin",                /* application class */
                            NULL,                      /* no command line options */
                            0,
                            &argc, argv );
   if ( display == NULL ) 
     {
	fprintf( stderr, "Error :  could not open X display.\n" );
	exit( -1 );
     }
   gremlinTop = XtVaAppCreateShell( NULL, "Xgremlin", 
                                  applicationShellWidgetClass,
                                  display, NULL );

   /*
    * create the bubble help widget
    */
   liteClue = XtVaCreatePopupShell("bubblehelp",
				   xcgLiteClueWidgetClass,
				   gremlinTop, NULL);
				   
   /*
    * Paned widget to hold plotting and editing space
    */
   gremlinPane = XtVaCreateManagedWidget(
	   "pane",
           panedWidgetClass,
           gremlinTop,
           NULL);


   /*
    * a box widget to hold the menu bar
    */
   gremlinButtons = XtVaCreateManagedWidget(
          "buttons",
          boxWidgetClass,
          gremlinPane,
          NULL);

   /*
    * this widget provides space for the Xgremlin graphics
    */
   gremlinPlot = XtVaCreateManagedWidget(
          "plot",
          displayWidgetClass,
          gremlinPane,
          XtNwidth, DISPLAY_PIX_WIDTH,
          XtNheight, DISPLAY_PIX_HEIGHT,
	  XtNcolorscale, "grayscale",
          NULL);

   /*
    * create a bar with function buttons
    */
   gremlinFuncs = XtVaCreateManagedWidget(
         "functions",
         boxWidgetClass,
         gremlinPane,
         NULL);

  for (i=0; i<NUM_FUNC_BUTTONS; i++) {
     sprintf(buffer, "funcbutton%d",i);
     funcButton[i] = XtVaCreateManagedWidget(
         (String)buffer,
         commandWidgetClass,
         gremlinFuncs,
         NULL);
     InitFunctions( i, funcButton[i] );
     XtAddCallback(funcButton[i], XtNcallback, FunctionChoice, (XtPointer)i);
  }   

   /*
    * alloc edit buffer and create text widget for the line editor of Xgremlin
    */
   editor_buffer = XtCalloc(EDIT_BUFF_LEN, sizeof(char));
   gremlinEdit = XtVaCreateManagedWidget(
          "edit",
          asciiTextWidgetClass,
          gremlinPane,
          XtNstring, editor_buffer,
          XtNlength, EDIT_BUFF_LEN,
          XtNuseStringInPlace, True,
          NULL);

   /*
    * buttons in the top window
    */
   optionButton = XtVaCreateManagedWidget(     /* menu button */
          "option",
          menuButtonWidgetClass,
          gremlinButtons,
          XtNmenuName, "optionmenu",
          NULL);
   XcgLiteClueAddWidget(liteClue, optionButton, 
          "Printing, documentation, phase correction, Fourier transform, .. , Exit\0", 0, 0);

   optionMenu = XtCreatePopupShell(
          "optionmenu",
          overrideShellWidgetClass,
          optionButton,
          NULL, 0);

   menuBox = XtVaCreateManagedWidget(
          "menubox",
          boxWidgetClass,
          optionMenu,
          NULL);

   /*
    * the buttons in the 'Gremlins' menu
    *
    * 0 : plot window
    * 1 : help window
    * 2 : phase correction
    * 3 : hfs fitting window
    * 4 : version info
    * 5 : quit program
    */
   for (i=0; i<NUM_MENU_BUTTONS; i++) 
     {
	sprintf(buffer, "menubutton%d",i);
	menuItem[i] = XtVaCreateManagedWidget(
             (String)buffer,
	     commandWidgetClass,
	     menuBox,
	     NULL);
	XtAddCallback( menuItem[i], XtNcallback, MenuChoice, (XtPointer)i );
     }

   /*
    * button to toggle ploting / editing mode
    */
   toggleButton = XtVaCreateManagedWidget(
          "toggle",
          toggleWidgetClass,
          gremlinButtons,
          NULL);
   XcgLiteClueAddWidget(liteClue, toggleButton, 
          "Toggle plot and edit modes.\0", 0, 0);
   XtAddCallback(toggleButton, XtNcallback, XTogglePlotEdit, 0); 

   /*
    * this is for the phase button
    */
   phaseButton = XtVaCreateManagedWidget(
          "phase",
          toggleWidgetClass,
          gremlinButtons,
          XtNsensitive, False,     /* not sensitive by default */
          NULL);
   XcgLiteClueAddWidget(liteClue, phaseButton, 
          "Toggle data and phase plotting modes.\0", 0, 0);
   XtAddCallback(phaseButton, XtNcallback, PhaseZoom, 0); 

   /*
    * the interrupt button
    */
   intrButton = XtVaCreateManagedWidget(
          "interrupt",
          commandWidgetClass,
          gremlinButtons,
          NULL);
   XcgLiteClueAddWidget(liteClue, intrButton, 
          "Interrupt Fourier transform, linelist etc.\0", 0, 0);
   XtAddCallback(intrButton, XtNcallback, InterruptRequested, 0);    

   /*  
    * buttons for spectrum scrolling etc.
    * Note: Bitmaps are added just before starting the main loop
    */
   leftButton = XtVaCreateManagedWidget(
          "left_button",
          commandWidgetClass,
          gremlinButtons,
          NULL);
   XcgLiteClueAddWidget(liteClue, leftButton, "Plot data to the left.\0", 0, 0);
   XtAddCallback(leftButton, XtNcallback, scroll_left, 0);    

   rightButton = XtVaCreateManagedWidget(
          "right_button",
          commandWidgetClass,
          gremlinButtons,
          NULL);
   XcgLiteClueAddWidget(liteClue, rightButton, "Plot data to the right.\0", 0, 0);
   XtAddCallback(rightButton, XtNcallback, scroll_right, 0);    

   yfullButton = XtVaCreateManagedWidget(
          "yfull_button",
          commandWidgetClass,
          gremlinButtons,
          NULL);
   XcgLiteClueAddWidget(liteClue, yfullButton, 
          "Scale plot to fill window vertically.\0", 0, 0);
   XtAddCallback(yfullButton, XtNcallback, yfull_world, 0);    

   xshrinkButton = XtVaCreateManagedWidget(
          "xshrink_button",
          commandWidgetClass,
          gremlinButtons,
          NULL);
   XcgLiteClueAddWidget(liteClue, xshrinkButton, "Zoom in (stretch) horizontally.\0", 0, 0);
   XtAddCallback(xshrinkButton, XtNcallback, xshrink_world, 0);    

   xexpandButton = XtVaCreateManagedWidget(
          "xexpand_button",
          commandWidgetClass,
          gremlinButtons,
          NULL);
   XcgLiteClueAddWidget(liteClue, xexpandButton, "Zoom out (compress) horizontally.\0", 0, 0);
   XtAddCallback(xexpandButton, XtNcallback, xexpand_world, 0);    

   yshrinkButton = XtVaCreateManagedWidget(
          "yshrink_button",
          commandWidgetClass,
          gremlinButtons,
          NULL);
   XcgLiteClueAddWidget(liteClue, yshrinkButton, "Zoom in (stretch) vertically.\0", 0, 0);
   XtAddCallback(yshrinkButton, XtNcallback, yshrink_world, 0);    

   yexpandButton = XtVaCreateManagedWidget(
          "yexpand_button",
          commandWidgetClass,
          gremlinButtons,
          NULL);
   XcgLiteClueAddWidget(liteClue, yexpandButton, "Zoom out (compress) vertically.\0", 0, 0);
   XtAddCallback(yexpandButton, XtNcallback, yexpand_world, 0);    

   xcentreButton = XtVaCreateManagedWidget(
          "xcentre_button",
          commandWidgetClass,
          gremlinButtons,
          NULL);
   XcgLiteClueAddWidget(liteClue, xcentreButton, "Center plot at marker horizontally.\0", 0, 0);
   XtAddCallback(xcentreButton, XtNcallback, xcentre_world, 0);    

   ycentreButton = XtVaCreateManagedWidget(
          "ycentre_button",
          commandWidgetClass,
          gremlinButtons,
          NULL);
   XcgLiteClueAddWidget(liteClue, ycentreButton, "Center plot at marker vertically.\0", 0, 0);
   XtAddCallback(ycentreButton, XtNcallback, ycentre_world, 0);    

   xsectionButton = XtVaCreateManagedWidget(
          "xsection_button",
          commandWidgetClass,
          gremlinButtons,
          NULL);
   XcgLiteClueAddWidget(liteClue, xsectionButton, 
          "Plot data between two markers (horizontally).\0", 0, 0);
   XtAddCallback(xsectionButton, XtNcallback, xsection_world, 0);    

   ysectionButton = XtVaCreateManagedWidget(
          "ysection_button",
          commandWidgetClass,
          gremlinButtons,
          NULL);
   XcgLiteClueAddWidget(liteClue, ysectionButton, 
          "Plot data between two markers (vertically).\0", 0, 0);
   XtAddCallback(ysectionButton, XtNcallback, ysection_world, 0);    

   undoButton = XtVaCreateManagedWidget(
          "undo_button",
          commandWidgetClass,
          gremlinButtons,
          NULL);
   XcgLiteClueAddWidget(liteClue, undoButton, "Undo previous plotting operations.\0", 0, 0);
   XtAddCallback(undoButton, XtNcallback, undo_world, 0);    

   redrawButton = XtVaCreateManagedWidget(
          "redraw_button",
          commandWidgetClass,
          gremlinButtons,
          NULL);
   XcgLiteClueAddWidget(liteClue, redrawButton, 
          "Redraw plot, place zero line at marker.\0", 0, 0);
   XtAddCallback(redrawButton, XtNcallback, redraw_world, 0);    


   /*
    * initialize all things
    */
   InitializeHelp();                            /* set up help system */
   InitializePhase(menuItem[2]);                /* phase correction */  
   InitializeHfs(menuItem[3]);                  /* hfs fitting */
   InitializeVersion(menuItem[4]);              /* version button */
   InitializeButtons(toggleButton, intrButton);
   InitializeEdit(gremlinEdit, editor_buffer);
   InitializePlotMode(gremlinPlot);


   /*
    * register any additional actions
    */
   XtAppAddActions( gremlinContext, actions, XtNumber(actions) );
   

   /* 
    * bring windows and widgets to life
    */
   XtRealizeWidget(gremlinTop);
   FocusToEdit();               /* keyboard focus to edit window */
   DisplayPrompt( INCREMENT );
   InitializePlotter();         /* set up drawing area */

   /* 
    * make the default icon known to the program. 
    */
   icon = XCreateBitmapFromData(XtDisplay(gremlinPane),
				XtWindow(gremlinPane), 
				Xgremlin_bits, 
				Xgremlin_width, Xgremlin_height);
   XtSetArg( args[0], XtNiconPixmap, icon);
   XtSetValues( gremlinTop, args, 1);

   /*
    * add the bitmaps to the buttons 
    * ( this can't be done earlier because the widgets have to exist first )
    */
   arrow_left = XCreateBitmapFromData(XtDisplay(gremlinPane),
				      XtWindow(gremlinPane), 
				      arrow_left_bits, 
				      arrow_left_width, 
                                      arrow_left_height);
   XtSetArg( args[0], XtNbitmap, arrow_left);
   XtSetValues( leftButton, args, 1);

   arrow_right = XCreateBitmapFromData(XtDisplay(gremlinPane),
				       XtWindow(gremlinPane), 
				       arrow_right_bits, 
				       arrow_right_width, 
                                       arrow_right_height);
   XtSetArg( args[0], XtNbitmap, arrow_right);
   XtSetValues( rightButton, args, 1);

   y_fullscale = XCreateBitmapFromData(XtDisplay(gremlinPane),
				       XtWindow(gremlinPane), 
				       y_fullscale_bits, 
				       y_fullscale_width, 
                                       y_fullscale_height);
   XtSetArg( args[0], XtNbitmap, y_fullscale);
   XtSetValues( yfullButton, args, 1);

   x_shrink = XCreateBitmapFromData(XtDisplay(gremlinPane),
				       XtWindow(gremlinPane), 
				       x_shrink_bits, 
				       x_shrink_width, 
                                       x_shrink_height);
   XtSetArg( args[0], XtNbitmap, x_shrink);
   XtSetValues( xshrinkButton, args, 1);

   x_expand = XCreateBitmapFromData(XtDisplay(gremlinPane),
				       XtWindow(gremlinPane), 
				       x_expand_bits, 
				       x_expand_width, 
                                       x_expand_height);
   XtSetArg( args[0], XtNbitmap, x_expand);
   XtSetValues( xexpandButton, args, 1);

   y_shrink = XCreateBitmapFromData(XtDisplay(gremlinPane),
				       XtWindow(gremlinPane), 
				       y_shrink_bits, 
				       y_shrink_width, 
                                       y_shrink_height);
   XtSetArg( args[0], XtNbitmap, y_shrink);
   XtSetValues( yshrinkButton, args, 1);

   y_expand = XCreateBitmapFromData(XtDisplay(gremlinPane),
				       XtWindow(gremlinPane), 
				       y_expand_bits, 
				       y_expand_width, 
                                       y_expand_height);
   XtSetArg( args[0], XtNbitmap, y_expand);
   XtSetValues( yexpandButton, args, 1);

   x_centre = XCreateBitmapFromData(XtDisplay(gremlinPane),
				       XtWindow(gremlinPane), 
				       x_centre_bits, 
				       x_centre_width, 
                                       x_centre_height);
   XtSetArg( args[0], XtNbitmap, x_centre);
   XtSetValues( xcentreButton, args, 1);

   y_centre = XCreateBitmapFromData(XtDisplay(gremlinPane),
				       XtWindow(gremlinPane), 
				       y_centre_bits, 
				       y_centre_width, 
                                       y_centre_height);
   XtSetArg( args[0], XtNbitmap, y_centre);
   XtSetValues( ycentreButton, args, 1);

   x_section = XCreateBitmapFromData(XtDisplay(gremlinPane),
				       XtWindow(gremlinPane), 
				       x_section_bits, 
				       x_section_width, 
                                       x_section_height);
   XtSetArg( args[0], XtNbitmap, x_section);
   XtSetValues( xsectionButton, args, 1);

   y_section = XCreateBitmapFromData(XtDisplay(gremlinPane),
				       XtWindow(gremlinPane), 
				       y_section_bits, 
				       y_section_width, 
                                       y_section_height);
   XtSetArg( args[0], XtNbitmap, y_section);
   XtSetValues( ysectionButton, args, 1);

   undoplot = XCreateBitmapFromData(XtDisplay(gremlinPane),
				       XtWindow(gremlinPane), 
				       undoplot_bits, 
				       undoplot_width, 
                                       undoplot_height);
   XtSetArg( args[0], XtNbitmap, undoplot);
   XtSetValues( undoButton, args, 1);

   redraw = XCreateBitmapFromData(XtDisplay(gremlinPane),
				       XtWindow(gremlinPane), 
				       redraw_bits, 
				       redraw_width, 
                                       redraw_height);
   XtSetArg( args[0], XtNbitmap, redraw);
   XtSetValues( redrawButton, args, 1);


   /*
    * allocate dynamic arrays, then
    * initialize the Fortran part of the Xgremlin, process rc files
    */
   rtralloc_( &r_init_size, &ierror );
   fftalloc_( &ffta_init_size, &ierror );
   linalloc_( &lin_init_size, &ierror );

   gremlininit_();


   /* 
    * ... and start the main event processing loop.
    */
   XtAppMainLoop( gremlinContext );

   return 0;  /* make compiler happy */
}

/*
 * the following empty function is needed to resolve the symbol MAIN__()
 * when compiling with f2c or g77
 */
int MAIN__()
{
  return 0;
}

/* Revision history:
 * ----------------
 * $Log: xgremlin.c,v $
 * Revision 1.21  1997/03/28 03:36:51  ulf
 * improved the help texts for the bubble help.
 *
 * Revision 1.20  1997/03/27 03:11:09  ulf
 * add bubble help for buttons on top of plotting window.
 *
 * Revision 1.18  1996/06/16 22:13:04  ulf
 * changed the position of the undo-button and created a better symbol for it
 *
 * Revision 1.17  1996/06/15 03:32:58  ulf
 * add new plot mode undo button 'U'
 *
 * Revision 1.16  1996/05/12 02:37:53  ulf
 * modify function InitializeHelp() for new help system
 *
 * Revision 1.15  1996/04/06 07:07:34  ulf
 * add function MAIN__ to resolve symbol from f2c library
 *
 * Revision 1.14  1996/03/19 15:11:35  ulf
 * add error parameter to array allocation functions ...alloc.
 *
 * Revision 1.13  1996/03/19  14:54:34  ulf
 * make 'stop' button sensitive from the beginning because turning sensitivity
 * off and on does not work too well during a Fourier transform (not enough
 * display updates).
 *
 * Revision 1.12  1996/03/15  16:50:01  ulf
 * changes made for dynamic array allocation
 *
 * Revision 1.11  1995/11/13  14:58:45  ulf
 * Language is set after toolkit initialization
 *
 * Revision 1.10  1995/11/13  14:57:46  ulf
 * commented out internationalization because it fails on a machine where the X server
 * is not installed correctly.
 *
 * Revision 1.9  1995/11/09  15:50:38  ulf
 * add registering of default language procedure
 *
 * Revision 1.8  1995/09/23  16:23:09  ulf
 * add support for monochrome displays
 *
 * Revision 1.7  1995/09/23  02:41:49  ulf
 * new 'Phase/Transform' button
 *
 * Revision 1.6  1995/08/19  19:26:01  ulf
 * make changes required for phase plotting
 *
 * Revision 1.5  1995/07/04  23:36:54  ulf
 * Comment out or remove  dialogue box  stuff for the time being.
 * Dialogue boxes have low priority and will be rarely needed.
 *
 * Revision 1.4  1995/07/04  04:00:02  ulf
 * rename  PLOTTER_PIX_*  to DISPLAY_PIX_*  to avoid confusion with the
 * constants referring to the plotter area.
 *
 * Revision 1.3  1995/07/01  17:35:50  ulf
 * set XtNwidth and XtNheight when plot widget is created.
 *
 * Revision 1.2  1995/06/24  19:34:02  ulf
 * make helpButton widget a global variable.
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */



