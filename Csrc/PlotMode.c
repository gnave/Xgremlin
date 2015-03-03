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
 * $Id: PlotMode.c,v 1.69 1996/09/15 03:22:31 ulf Exp $
 */

#include <X11/Intrinsic.h>     /* X toolkit intrinsics definitions */
#include <X11/StringDefs.h>    /* Standard Name-String definitions */
#include <X11/keysym.h>

#include <X11/Xaw/Toggle.h>

#include "Display.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <ctype.h>

#include "PlotMode.h"
#include "Buttons.h"
#include "defaults.h"
#include "Edit.h"
#include "Miscellaneous.h"
#include "FortranWrappers.h"
#include "List.h"


/*------------------------ definitions, types etc. -----------------------*/

#define SPACE         3
#define LABEL_SPACE   2
#define LBL_LINE_LEN  6          /* length of line to indicate label position */
#define RADIUS        2          /* radius of circles in phase plot */
#define TIC_STICK_OUT 3          /* large tic marks stick out below plotter pane a bit */  

enum units { NM, HZ, GHZ };      /* secondary units used by program */
enum { PRIMARY=1, SECONDARY };   /* type of units in Xgremlin */

extern Display *display;         /* the display structure */
extern Widget gremlinTop;        /* the main widget */
extern Widget phaseButton;       /* the phase zoom toggle button */


typedef struct {
   int x;                        /* x coordinate of Point */
   int y;                        /* y coordinate of Point */
} PixelPoint;


typedef struct {
   float x;                      /* x coordinate of Point */
   float y;                      /* y coordinate of Point */
} WorldPoint;


typedef struct {
   float min; /* global min/max of data */
   float max;
   float l;   /* left */
   float r;   /* right */
   float b;   /* bottom */
   float t;   /* top */
   float pb;  /* phase bottom */
   float pt;  /* phase top */
   int defined;
} CoordinateSystem;


typedef struct {  /* the marker coordinates */
   int xmin;
   int xmax;
   int ymin;
   int ymax;
   int visible;
} Marker;


typedef struct {
   WorldPoint w;       /* store both world and pixel coords. */
   PixelPoint p;
   Marker cross;
   display_cmd horz;   /* display commands used for mouse markers */
   display_cmd vert;
} MousePosition;


typedef struct {    /* to store the point range */
   int reference;   /* where the points are in the data _file_ */
   int begin;       /* point range of the _buffer_ that is visible */
   int end;
} Range;


typedef struct {     /* color table entry for overplotting color table */
   char *name;
   int  color;
} palette_color;


typedef struct {
   int point;              /* point in the buffer of the line */
   double wavnum;          /* x position of the marker in world coordinates */
   float intens;           /* line intensity */
   float width;            /* line width */
   int number;             /* number of the line in the list (1,2,3,...) */
   int active;             /* flag if line is active or not */
   int color;              /* color it must be drawn with */
   int pixpos;             /* the pixel position of the line marker in plotter window */
   char *id_str;           /* the address(!) of the line id string */
   int id_len;             /* the lenght of the (Fortran) id string */
   display_cmd dcmd;       /* display command used for drawing the line */
   display_cmd lcmd;       /* display command used for drawing the label */
} LineMarker;


typedef struct {           /* a plot frame for the plot mode undo stack */
   int left;
   int right;
   float top;
   float bottom;
} PlotFrame;


typedef struct {
   int alias;
   double fsr;
   double sigma_laser;
   float velocity;
} SamplingParameters;

/*-----------------------------------------------------------------*
 *               Prototypes of local funtions                      *
 *-----------------------------------------------------------------*/

/*
 * callback function for a pressed key in the plotting widget
 */
static void 
KeyHandler( Widget w, XtPointer client_data, XtPointer call_data );


/*
 * callback function for mouse button events
 */
static void 
ButtonHandler( Widget w, XtPointer client_data, XtPointer call_data );


/*
 * callback function for mouse movement events
 */
static void 
MotionHandler( Widget w, XtPointer client_data, XtPointer call_data );


/*
 * callback function for resize events
 */
static void
ResizeHandler( Widget w, XtPointer client_data, XtPointer call_data );


/*
 * draw a cross on the screen a current mouse position
 */
static void
draw_cross_marker( MousePosition *mouse, int color );


/*
 * remove a cross marker from the plotter
 */
static void
remove_cross_marker( MousePosition *mouse );


/*
 * remove all markers from a specified button
 */
static void
remove_all_markers( int button );


/*
 * calculates the position string from the pixel position
 */
static INLINE void
position_string( PixelPoint *pp, char ps[] );


/*
 * prints a string in the information line below the plot
 */
static INLINE void
line_info( char *is, int color, int printable );


/* 
 * transform from world to pixel coordinates in the plotter (!)  window
 */
static INLINE PixelPoint
world_to_pixel( WorldPoint wp );

static INLINE int             /* y coordinate only */
yworld_to_ypixel( float y_wp );

static INLINE WorldPoint
pixel_to_world( PixelPoint pp );

static INLINE int
pixel_to_point( int pixel );

static INLINE int
phase_yworld_to_ypixel( float y_wp );

static INLINE WorldPoint
phase_pixel_to_world( PixelPoint pp );

/*
 * calculate the air wavelength (for standard air) in nm from vacuum wave number
 * ( for formula see: B Edlen, The refractive index of air 1966, Metrologia 2, 71-80 )
 * above 50000/cm the vacuum wavelength is returned.
 */
static float
kayser_to_nm( float sigma );

/*
 * transform for primary unit to secondary units Hz and GHz
 */
static float
kayser_to_hz( float sigma );

static float
kayser_to_thz( float sigma );


/*
 * clip lines to plotter window. returns TRUE if (part of) the line
 * is within the plotter window.
 */
static INLINE void
clip_line( PixelPoint *p1, PixelPoint *p2, PixelPoint *pa, PixelPoint *pb );

static INLINE void
clip_point( PixelPoint *A, PixelPoint *B, PixelPoint *C, PixelPoint *pc );


/*
 * the following functions are needed for line lists and line markers
 */
static int 
line_comp( LineMarker *line1, LineMarker *line2 );

static void
renumber_lines( LineMarker *line );

static void
show_line( LineMarker *line );

static void
show_label( LineMarker *line );

static void
activate_line( LineMarker *line );

static void
deactivate_line( LineMarker *line );

static int
fuzzy_search_linenumber( LineMarker *line );  /* with allowance for poor aim */

static int 
search_line_pixel( LineMarker *line );        /* for line info update */

static int
search_lnum( LineMarker *line );              /* search for a numbered line */

static int
line_active( LineMarker *line );

static void
clear_labels();

/*---------------------- global variables ------------------------*/

static Widget loc_plot_widget;                    /* keep a local copy */
static int display_pix_width, display_pix_height; /* plotter window size */
static int plotter_pix_width, plotter_pix_height; /* size of drawing area */
static int plotter_pix_left, plotter_pix_right;
static int plotter_pix_top, plotter_pix_bottom;
static int plotter_line1, plotter_line2;          /* text line positions */
static int plotter_pix_shift;

static int plotter_color, frame_color, text_color, bar_color, pink_color,
           green_color, red_color, blue_color, cur_color, button1_color,
           button2_color, black_color, grey_color, marker_color, active_color,
           aux_color, phase_color, bad_color, fit_color, std_color,
           real_color, imag_color;

static int small_font;
static int large_font;
static display_cmd save_cmd;
static display_cmd last_cmd;               /* most recently called display command */
static display_cmd line_info_cmd = NULL;   /* last spectrum line status line */
static int label_pix_width, label_pix_height;
static int label_small_pix_height;
static int clen, cslen;                    /* pixels per character */

static CoordinateSystem world;
static float dx_world, dy_world, dyp_world;
static WorldPoint world_cp;                /* current world cursor position */
static WorldPoint phase_world_cp;          /* same for phase plot world */
static PixelPoint pixel_cp;                /* current pixel cursor position */
static PixelPoint phase_pixel_cp;          /* same for phase plot pixels */
static MousePosition mouse_click[NUM_BUTTONS][MOUSE_CLICKS];
static MousePosition stored_click[NUM_BUTTONS][MOUSE_CLICKS];
static int mc_num[NUM_BUTTONS];            /* number of stored mouse clicks */
static int sto_num[NUM_BUTTONS];           /* clicks saved with stom command */
static int last_marker_button = BUTTON_1;  /* for mouse marker correction */
static Range pt_range;                     /* store the point range */
static int wavenumber_scale;               /* what scale to use */

static tList line_list;                    /* (sorted) list for line markers */
static int markers_visible = TRUE;         /* flag TRUE if markers visible */
static int phase_mode = FALSE;             /* flags phase plot mode (toggle active) */
static int phase_zoom = FALSE;             /* zoom data or phase plot */

static tList undo_stack;                   /* list for the plot mode undo stack */

/*
 * support for secondary units
 */
static SamplingParameters sp;              /* for Hz secondary unit */
static int secondary_unit = 0;             /* secondary unit, nm by default */
char *unit_name[] = { "nm", "Hz", "THz" }; /* names of secondary units */
float (*kayser_to_secondary[])(float) =    /* array of transformation functions */
      { kayser_to_nm,                      /* wavenumbers --> nm */
        kayser_to_hz,                      /* wavenumbers --> (acoustic) Hz */
        kayser_to_thz };                   /* wavenumbers --> THz */

/*
 * global mail boxes to communicate data to list search and scan functions
 */
static int line_number_mbx;                /* used for renumbering lines */
static int last_numstr_mbx;                /* used for plotting line numbers */
static int nbin_mbx;                       /* contains binning used in plotting */
static int active_mbx;                     /* used for setting lines active */


/*
 * Palette of colors used in overplotting (selection is arbitrary)
 */
static palette_color palette[] = {
	 {"red2",        0},  /* lighter colours */ 
         {"orange2",     0}, 
	 {"DeepSkyBlue2",0}, 
         {"green2",      0}, 
	 {"magenta2",    0}, 
	 {"sienna2",     0}, 
	 {"pink2",       0}, 
         {"blue2",       0},   
	 {"red4",        0},  /* darker colours */    
	 {"orange4",     0}, 
	 {"DeepSkyBlue4",0},
         {"green4",      0}, 
	 {"magenta4",    0}, 
	 {"sienna4",     0}, 
	 {"pink4",       0}, 
         {"blue4",       0},   
         { NULL,         0} }; /* end of table */
static int num_colors;                     /* # of colors in the table */
static int color_idx;                      /* index into color table */


/*
 * The following array stores a flag for every horizontal pixel in the
 * plotter frame. If a line is marked in that pixel the flag has a
 * value != 0 (0 otherwise). This is required for the continuous line
 * info printout. The array is allocated during initialization and
 * re-allocated whenever the window is resized. The flag is set by
 * function 'show_line' every time a line is marked on the screen.
 */
static char *pixel_flags;

/*-----------------------------------------------------------------*/

void 
InitializePlotMode( Widget plotW )
{
   loc_plot_widget = plotW;
   XtAddCallback( loc_plot_widget, XtNkcallback, KeyHandler,    NULL); 
   XtAddCallback( loc_plot_widget, XtNbcallback, ButtonHandler, NULL); 
   XtAddCallback( loc_plot_widget, XtNmcallback, MotionHandler, NULL); 
   XtAddCallback( loc_plot_widget, XtNrcallback, ResizeHandler, NULL); 
}

/*-----------------------------------------------------------------*/

void 
InitializePlotter()
{
   /*
    * we now hard code the initial size of the display widget because 
    * XtVaGetValues does not seem to work under SunOS with the Display widget
    */
   display_pix_width = DISPLAY_PIX_WIDTH;
   display_pix_height= DISPLAY_PIX_HEIGHT;

   /*
    * fonts and sizes
    */
   small_font = display_load_font( loc_plot_widget, SMALL_PLOTTER_FONT );
   large_font = display_load_font( loc_plot_widget, LARGE_PLOTTER_FONT );
   label_pix_width  = display_font_width( loc_plot_widget, large_font, "01234567890" );
   label_pix_height = display_font_height( loc_plot_widget, large_font );
   label_small_pix_height = display_font_height( loc_plot_widget, small_font );
   clen  = display_font_width( loc_plot_widget, large_font, "X" );
   cslen = display_font_width( loc_plot_widget, small_font, "X" );

   /* 
    * the plotter is the central part of the graphics window
    * calculate its size.
    */
   plotter_pix_shift  = display_pix_width  * X_SHIFT / 100.0;
   plotter_pix_width  = display_pix_width  * PLOTTER_WIDTH / 100.0;
   plotter_pix_height = display_pix_height -  2 * SPACE * label_pix_height;
   plotter_pix_left   = (display_pix_width - plotter_pix_width)/2 + plotter_pix_shift;
   plotter_pix_right  = plotter_pix_left + plotter_pix_width; 

   /*
    * the central plotting are must leave room for about 2 lines (+1 line slack)
    * of text above and below it
    */
   plotter_pix_top    = display_pix_height - plotter_pix_height - 
                        SPACE * label_pix_height;
   plotter_pix_bottom = display_pix_height - SPACE * label_pix_height;
   plotter_line1      = plotter_pix_top / 3 + 4;
   plotter_line2      = plotter_line1 * 2;

   /*
    * mouse and colors
    */
   world.defined      = FALSE;
   mc_num[0] = mc_num[1] = mc_num[2] = 0;
   plotter_color      = display_set_color(loc_plot_widget, PLOTTER_COLOR );
   frame_color        = display_set_color(loc_plot_widget, FRAME_COLOR );  
   text_color         = display_set_color(loc_plot_widget, TEXT_COLOR );  
   red_color          = display_set_color(loc_plot_widget, RED_COLOR );  
   pink_color         = display_set_color(loc_plot_widget, PINK_COLOR );  
   green_color        = display_set_color(loc_plot_widget, GREEN_COLOR );  
   blue_color         = display_set_color(loc_plot_widget, BLUE_COLOR );  
   grey_color         = display_set_color(loc_plot_widget, GREY_COLOR );  
   black_color        = display_set_color(loc_plot_widget, "black" );  
   bar_color          = display_set_color(loc_plot_widget, BAR_COLOR );  
   marker_color       = display_set_color(loc_plot_widget, MARKER_COLOR );  
   active_color       = display_set_color(loc_plot_widget, ACTIVE_COLOR );  
   button1_color      = display_set_color(loc_plot_widget, RED_COLOR );  
   button2_color      = display_set_color(loc_plot_widget, BLUE_COLOR );  
   aux_color          = display_set_color(loc_plot_widget, AUX_MARKER_COLOR );  
   phase_color        = display_set_color(loc_plot_widget, PHASE_COLOR );  
   bad_color          = display_set_color(loc_plot_widget, BAD_COLOR );  
   fit_color          = display_set_color(loc_plot_widget, PHASE_FIT_COLOR );  
   std_color          = display_set_color(loc_plot_widget, PHASE_STD_COLOR );  
   real_color         = black_color;      /* black */
   imag_color         = button2_color;    /* blue3 */
   cur_color          = black_color;  
   display_set_linestyle( loc_plot_widget, 1, 0 );

   /*
    * set up the color table for overplotting
    */
   num_colors = 0;                            /* first count the colors */
   while ( palette[num_colors].name != NULL )
     {
	palette[num_colors].color = 
	  display_set_color(loc_plot_widget, palette[num_colors].name );
	num_colors++;
     }
   
   /* 
    * draw plotter frame
    */
   display_clear( loc_plot_widget );
   display_fill_rectangle( loc_plot_widget,plotter_pix_left,plotter_pix_top,
                           plotter_pix_width,plotter_pix_height,plotter_color);
   last_cmd = 
   display_draw_rectangle( loc_plot_widget,plotter_pix_left,plotter_pix_top,
                           plotter_pix_width,plotter_pix_height,text_color);   
   /*
    * create the line list and undo stack
    */
   create_list( &line_list );
   create_list( &undo_stack );

   /*
    * create the array of line pixel flags and clear it
    */
   pixel_flags = (char *)malloc( plotter_pix_width * sizeof(char) );
   if (pixel_flags == NULL )
     {
	fprintf(stderr, "Error :  failed to allocate pixel flags\n");
	exit(-1);
     }
   memset( pixel_flags, '\0', plotter_pix_width );
}

/*-----------------------------------------------------------------*/

void
NewPlotter()
{
   /*
    * draw plotter frame, background and center marker
    */
   display_fill_rectangle(loc_plot_widget,plotter_pix_left,plotter_pix_top,
                          plotter_pix_width,plotter_pix_height,plotter_color);
   display_draw_rectangle(loc_plot_widget,plotter_pix_left,plotter_pix_top,
                          plotter_pix_width,plotter_pix_height,text_color);   
   last_cmd = display_draw_line_color(loc_plot_widget,
				      plotter_pix_left + plotter_pix_width/2 + 1,
				      plotter_pix_top + C_MARKER_LEN,
				      plotter_pix_left + plotter_pix_width/2 + 1,
				      plotter_pix_top, grey_color);
   line_info_cmd = NULL;
   memset( pixel_flags, '\0', plotter_pix_width );  /* clear pixel flags */   
}

/*-----------------------------------------------------------------*/

void 
FocusToPlot()
{
   XtSetKeyboardFocus( gremlinTop, loc_plot_widget );
}

/*-----------------------------------------------------------------*/

void
PhaseZoom( Widget w , XtPointer phase_button_widget, XtPointer call_data )
{
   Arg args[2];
   int i;

   i = 0;
   if ( phase_zoom == TRUE ) 
     {
	phase_zoom = FALSE;
        XtSetArg(args[i], XtNstate, False); i++;
     }
   else
     {
	phase_zoom = TRUE;
        XtSetArg(args[i], XtNstate, True); i++;
     }
   XtSetValues( phaseButton, args, i );
}
 
/*-----------------------------------------------------------------*/

void
PhaseOff()
{
   Arg args[2];
   int i;

   i = 0;
   if ( phase_zoom == TRUE ) 
     {
	phase_zoom = FALSE;
        XtSetArg(args[i], XtNstate, False); i++;
	XtSetValues( phaseButton, args, i );
     }
}

/*-----------------------------------------------------------------*/

static void
KeyHandler( Widget w, XtPointer client_data, XtPointer call_data )
{
   XKeyEvent *ke;  /* call_data is pointer to event structure */
   KeySym key;     /* needed for hardware independent code */
   Modifiers modify, m_out;
   int shift, ctrl, meta, ch;

   ke = (XKeyEvent *)call_data;
   modify = (Modifiers)ke->state;
   XtTranslateKeycode(ke->display, ke->keycode,
		      (Modifiers)ke->state, &m_out, &key);

   /* 
    * tabs toggle back to edit mode
    */
   if (key == XK_Tab)  
     {  
	TogglePlotEdit();
	return;
     }
   if (key == XK_less)
     key = (int)'<';
   if (key == XK_equal)
     key = (int)'=';
   if (key == XK_greater)
     key = (int)'>';
   if (key == XK_asciicircum)
     key = (int)'^';

   /*
    * check modifiers
    */
   shift = (modify & ShiftMask)   >> 0;  /* these are either 0 or 1 */
   ctrl  = (modify & ControlMask) >> 2;
   meta  = (modify & Mod1Mask)    >> 3;

   /* 
    * dispatch all other keystrokes to Fortran part. The tricky bit is  
    * that this has to be hardware independent but still reasonably fast.
    * Also, the X key symbols must be made digestible for Fortran.
    * Make use of the fact that in X11R5/6 the key symbols map to ASCII.
    * Try this ...
    */
   ch = (int)key;
   if ( ch > FKBASE )
      {
	switch( (int)key ) 
	  {
	     case (int)XK_F1:  ch = LOC_F1;  break;   /* map function keys to     */
	     case (int)XK_F2:  ch = LOC_F2;  break;   /* ascii. There does not    */
	     case (int)XK_F3:  ch = LOC_F3;  break;   /* seem to be a better way. */
	     case (int)XK_F4:  ch = LOC_F4;  break;   
	     case (int)XK_F5:  ch = LOC_F5;  break;   
	     case (int)XK_F6:  ch = LOC_F6;  break;   
	     case (int)XK_F7:  ch = LOC_F7;  break;   
	     case (int)XK_F8:  ch = LOC_F8;  break;   
	     case (int)XK_F9:  ch = LOC_F9;  break; 
	     case (int)XK_F10: ch = LOC_F10; break; 
	     case (int)XK_F11: ch = LOC_F11; break; 
	     case (int)XK_F12: ch = LOC_F12; break; 

	     case (int)XK_Up:    ch = LOC_UP;     break; 
	     case (int)XK_Down:  ch = LOC_DOWN;   break; 
	     case (int)XK_Left:  ch = LOC_LEFT;   break; 
	     case (int)XK_Right: ch = LOC_RIGHT;  break; 
	     case (int)XK_Home:  ch = LOC_HOME;   break; 
	     case (int)XK_End:   ch = LOC_END;    break; 
	     case (int)XK_Prior: ch = LOC_PGUP;   break; 
	     case (int)XK_Next:  ch = LOC_PGDOWN; break; 
	  }
      }

   keycmd_(&ch, &shift, &ctrl, &meta);  /* dispatch a graphics command */
   if ( TextOutput() == TRUE )
     DisplayPrompt( NOINCREMENT );
}

/*-----------------------------------------------------------------*/

static void
ButtonHandler( Widget w, XtPointer client_data, XtPointer call_data )
{
   XButtonEvent *be;
   PixelPoint pp;
   char pstr[80];
   char label[80];
   int k;
   
   be = (XButtonEvent *)call_data;
   pp.x = be->x;
   pp.y = be->y;
   switch( be->button )
     {
      /*
       * store the mouse position for subsequent commands 
       * and print the position of the last marker
       */
      case Button1 :
	if ( mc_num[BUTTON_1] < MOUSE_CLICKS-1 )
	  {
	     k = mc_num[BUTTON_1];
	     mouse_click[BUTTON_1][k].p.x = pp.x;
	     mouse_click[BUTTON_1][k].p.y = pp.y;
	     mouse_click[BUTTON_1][k].w = pixel_to_world( mouse_click[BUTTON_1][k].p );
	     draw_cross_marker( &mouse_click[BUTTON_1][k], button1_color );
	     mc_num[BUTTON_1]++;
	     
	     sprintf( label, "(B%d, M%d) ", 1, mc_num[BUTTON_1] );
	     position_string( &pp, pstr );
	     strcat( label, pstr );
	     line_info( label, button1_color, TRUE );
	     last_marker_button = BUTTON_1;
	  }
	break;

      case Button2 :
	/* 
	 * draw a blue marker and print the position of the last marker
         * until after the next redraw.
	 */
	if ( mc_num[BUTTON_2] < MOUSE_CLICKS-1 )
	  {
	     k = mc_num[BUTTON_2];
	     mouse_click[BUTTON_2][k].p.x = pp.x;
	     mouse_click[BUTTON_2][k].p.y = pp.y;
	     mouse_click[BUTTON_2][k].w = pixel_to_world( mouse_click[BUTTON_2][k].p );
	     draw_cross_marker( &mouse_click[BUTTON_2][k], button2_color );
	     mc_num[BUTTON_2]++;
	     
	     /*
	      * to be replaced by code to tell user the position of the
	      * line s/he clicked on
	      */
	     sprintf( label, "(B%d, M%d) ", 2, mc_num[BUTTON_2] );
	     position_string( &pp, pstr );
	     strcat( label, pstr );
	     line_info( label, blue_color, TRUE );
	     last_marker_button = BUTTON_2;
	  }
	break;

      case Button3 :
	/*
	 * remove the last mouse click markers
	 */
	if ( mc_num[last_marker_button] == 0 )
	  last_marker_button = last_marker_button == BUTTON_1 ? BUTTON_2 : BUTTON_1;
	if ( mc_num[last_marker_button] > 0 )
	  {
	     mc_num[last_marker_button]--;
	     remove_cross_marker( 
                   &mouse_click[last_marker_button][mc_num[last_marker_button]] );
	  }
	break;
     }
}

/*-----------------------------------------------------------------*/

static void
MotionHandler( Widget w, XtPointer client_data, XtPointer call_data )
{
   XMotionEvent *me;
   static char sticky_label[96];  /* preserve contents */
   int xpos;                      /* position of text */
   int lpos;                      /* line position in plotter window */
   int info_x;                    /* position of the line info string */
   int info_y;
   PixelPoint pp;
   LineMarker *found_line;
   char info[80];
   char id[34];

   /*
    * get the mouse event
    */
   me = (XMotionEvent *)call_data;

   /*
    * old text must be removed first
    */
   xpos = plotter_pix_width * MOUSE_TXT / 100.0; 
   display_string( loc_plot_widget, sticky_label,
                   xpos, plotter_line1 + label_small_pix_height / 2,
                   small_font, frame_color );

   /*
    * now draw the new text
    */
   pp.x = me->x;
   pp.y = me->y;
   position_string( &pp, sticky_label );
   display_string(loc_plot_widget, sticky_label,
		  xpos, plotter_line1 + label_small_pix_height / 2, 
		  small_font, text_color );

   /*
    * now check if line info must be printed at the bottom of the plot
    */
   lpos = pp.x - plotter_pix_left;
   if (pixel_flags[lpos] != 0  && markers_visible == TRUE &&
       in_interval(plotter_pix_left, plotter_pix_right, pp.x) == TRUE &&
       in_interval(plotter_pix_top, plotter_pix_bottom, pp.y) == TRUE )
     {
	/*
	 * now find the marker record of the line at that pixel
	 */
	line_number_mbx = lpos;
	found_line = (LineMarker *)
	  search_list( line_list, SEARCH_PROC( search_line_pixel ) );
	if ( found_line != NULL )
	  {
	     /*
	      * first remove old string
	      */
	     info_x = plotter_pix_left;
	     info_y = display_pix_height - 5;
	     display_string(loc_plot_widget, info,
			    info_x, info_y, 
			    small_font, frame_color );
	     /*
	      * then create and print the new line info string
	      */
	     if ( found_line->id_len != 0 )
	       {
		  strncpy( id, found_line->id_str, found_line->id_len );
		  StrForToC( id, found_line->id_len );
		  sprintf(info, "Line: %d  x: %.9g  W: %.5g  I: %.4g  ( %s )",
			  found_line->number, found_line->wavnum, 
			  found_line->width, found_line->intens, id );
	       }
	     else
	       {
		  sprintf(info, "Line: %d  x: %.9g  W: %.5g  I: %.4g  ( no id )",
			  found_line->number, found_line->wavnum, 
			  found_line->width, found_line->intens );
	       }
	     line_info( info, text_color, FALSE );
	  }
     }
}

/*-----------------------------------------------------------------*/

static void
ResizeHandler( Widget w, XtPointer client_data, XtPointer call_data )
{
   DisplayResizeCBData rdata;   
   int ch, shift, ctrl, meta;

   /*
    * store new window sizes
    */
   rdata = (DisplayResizeCBData)call_data;
   display_pix_width  = rdata->new_width;
   display_pix_height = rdata->new_height;
   plotter_pix_shift  = display_pix_width  * X_SHIFT / 100.0;
   plotter_pix_width  = display_pix_width  * PLOTTER_WIDTH / 100.0;
   plotter_pix_height = display_pix_height -  2 * SPACE * label_pix_height;
   plotter_pix_left   = (display_pix_width - plotter_pix_width)/2 + plotter_pix_shift;
   plotter_pix_right  = plotter_pix_left + plotter_pix_width; 
   plotter_pix_top    = display_pix_height - plotter_pix_height - 
                        SPACE * label_pix_height;
   plotter_pix_bottom = display_pix_height - SPACE * label_pix_height;
   plotter_line1      = plotter_pix_top / 3;
   plotter_line2      = plotter_line1 * 2;
   mc_num[0] = mc_num[1] = mc_num[2] = 0;

   /*
    * re-allocate pixel flags
    */
   pixel_flags = realloc(pixel_flags, plotter_pix_width * sizeof(char));
   if (pixel_flags == NULL )
     {
	fprintf(stderr, "Error :  failed to allocate pixel flags\n");
	exit(-1);
     }
   memset( pixel_flags, '\0', plotter_pix_width );

   /*
    * re-plot data
    */
   ch = (int)'P';   /* fake a key stroke */
   shift = 1;
   ctrl  = 0;
   meta  = 0;
   keycmd_( &ch, &shift, &ctrl, &meta ); 
}

/*-----------------------------------------------------------------*/

static void
draw_cross_marker( MousePosition *mouse, int color )
{
   int xmin, xmax, ymin, ymax;

   if ( world.defined == TRUE &&
        in_interval( plotter_pix_left, plotter_pix_right, mouse->p.x ) == TRUE &&
        in_interval( plotter_pix_top, plotter_pix_bottom, mouse->p.y ) == TRUE )
     {
	xmin = mouse->p.x - X_MARKER_LEN / 2;
	xmax = xmin + X_MARKER_LEN;
	ymin = mouse->p.y + Y_MARKER_LEN / 2;
	ymax = ymin - Y_MARKER_LEN;

	if ( xmin < plotter_pix_left )
	  xmin = plotter_pix_left;
	if ( xmax > plotter_pix_right )
	  xmax = plotter_pix_right;
	if ( ymin > plotter_pix_bottom )  /* y coord run from top to bottom */
	  ymin = plotter_pix_bottom;
	if ( ymax < plotter_pix_top )
	  ymax = plotter_pix_top;

	mouse->cross.xmin = xmin;         /* store size of clipped marker */
	mouse->cross.xmax = xmax;
	mouse->cross.ymin = ymin;
	mouse->cross.ymax = ymax;

	mouse->horz = 
	  display_draw_line_color(loc_plot_widget, 
				  xmin, mouse->p.y, xmax, mouse->p.y,
				  color );
	mouse->vert = 
	  display_draw_line_color(loc_plot_widget,
				  mouse->p.x, ymin, mouse->p.x, ymax, 
				  color );
	mouse->cross.visible = TRUE;
	display_set_color_value( loc_plot_widget, cur_color );
     }   
}

/*-----------------------------------------------------------------*/

static void
remove_cross_marker( MousePosition *mouse )
{
   /*
    * remove the mouse marker from display command list of widget
    */
   display_erase( loc_plot_widget, mouse->horz );
   display_erase( loc_plot_widget, mouse->vert );

   /*
    * overplot marker with background color
    */
   set_objects_printable( FALSE );   /* don't register in display list */
   display_draw_line_color(loc_plot_widget, 
			   mouse->cross.xmin, mouse->p.y, 
			   mouse->cross.xmax, mouse->p.y,
			   plotter_color );
   last_cmd =
     display_draw_line_color(loc_plot_widget,
			     mouse->p.x, mouse->cross.ymin, 
			     mouse->p.x, mouse->cross.ymax, 
			     plotter_color );
   set_objects_printable( TRUE );
   mouse->cross.visible = FALSE;
}

/*-----------------------------------------------------------------*/

static void
remove_all_markers( int button )
{
   int i;

   for ( i=0; i<mc_num[button]; i++ )
     if ( mouse_click[button][i].cross.visible == TRUE )
       remove_cross_marker( &mouse_click[button][i] );
   mc_num[button] = 0;
}

/*-----------------------------------------------------------------*/

static INLINE void
position_string( PixelPoint *pp, char ps[] )
{
   WorldPoint wp;
   int npt;            /* number of data point under cursor */
   float air_wl;       /* air wave length in secondary units */

   if ( in_interval( plotter_pix_left, plotter_pix_right, pp->x ) == TRUE &&
        in_interval( plotter_pix_top, plotter_pix_bottom, pp->y ) == TRUE &&
	world.defined == TRUE )
     {
	if ( phase_zoom == FALSE ) 	  
	  wp = pixel_to_world( *pp );
	else
	  wp = phase_pixel_to_world( *pp );
	npt = pixel_to_point( pp->x ) + pt_range.reference; /* absolute point */
	if ( wavenumber_scale == TRUE )
	  {
	     air_wl = (*kayser_to_secondary[secondary_unit])( wp.x );
	     sprintf( ps, "x: %.8g/cm ( %.8g %s ) y: %.4g  pt: %d           ", 
      		      wp.x, air_wl, unit_name[secondary_unit], wp.y, npt  );
	  }
	else
	  {
	     sprintf( ps, "x: %.8g nm   y: %.4g  pt: %d           ", 
		      wp.x, wp.y, npt  );
	  }
     }
   else
     {
	memset( ps, ' ', 52 );
	ps[52] = '\0';
     }
}

/*-----------------------------------------------------------------*/

static INLINE void
line_info( char *is, int color, int printable )
{
   int xleft;
   int yleft;
   int pixlen;
   int max_len;

   pixlen = display_font_width( loc_plot_widget, small_font, is );
   xleft = ( display_pix_width - pixlen ) / 2;
   if ( xleft < plotter_pix_left )
     xleft = plotter_pix_left;
   yleft = plotter_pix_bottom + 5 * label_pix_height / 2 ; /* that's about right */
   
   /*
    * erase any old text to prevent displaylist from filling up and draw new text
    */
   if ( line_info_cmd != NULL )
     {
	display_erase( loc_plot_widget, line_info_cmd );	
	set_objects_printable( FALSE );
	/* 
	 * make sure not to wipe out the mode indicator 
	 */
	display_fill_rectangle(loc_plot_widget, 
			       plotter_pix_left,  yleft - label_small_pix_height,
			       plotter_pix_width - 4*cslen, label_small_pix_height+2, 
			       frame_color);
	set_objects_printable( TRUE );
     }

   /*
    * make sure string fits underneath plotter 
    */
   max_len = ( plotter_pix_right - plotter_pix_left )/cslen - 4;
   if ( strlen(is) > max_len )
     is[max_len] = '\0';
   if ( printable == FALSE )
     set_objects_printable( FALSE );
   last_cmd = line_info_cmd = display_draw_text(loc_plot_widget, is, 
						xleft, yleft,
						small_font, color );
   if ( printable == FALSE )
     set_objects_printable( TRUE );
}

/*-----------------------------------------------------------------*/

/*-----------------------------------------------
 * coordinate transformations
 *-----------------------------------------------*/

/*
 * Note: 
 * Xgremlin knows of two coordinate systems, world and pixels.
 * The world coordinate system is defined by the minima and maxima of the
 * data that are being plotted in x and y direction. Obviously, the world
 * changes from plot to plot. The pixel coordinate system is the range of
 * pixels available for plotting in x and y direction. Pixel coordinates only
 * change when the plotter window is re-sized. The following functions are
 * transformations from the world coordinates into pixel coordinates and vice
 * versa.
 */

static INLINE PixelPoint
world_to_pixel( WorldPoint wp )
{
   PixelPoint pp;

   pp.x = (int)((wp.x - world.l)*plotter_pix_width/dx_world +0.5) + plotter_pix_left;
   pp.y = plotter_pix_height - 
          (int)((wp.y - world.b)*plotter_pix_height/dy_world +0.5) + plotter_pix_top;
   return pp;
}

/*-----------------------------------------------------------------*/

static INLINE int
yworld_to_ypixel( float y_wp )
{
   return plotter_pix_height - 
          (int)((y_wp - world.b)*plotter_pix_height/dy_world +0.5) + plotter_pix_top;
}

/*-----------------------------------------------------------------*/

static INLINE WorldPoint
pixel_to_world( PixelPoint pp )
{
   WorldPoint wp;

   wp.x = dx_world * (float)( pp.x - plotter_pix_left ) /
          (float)plotter_pix_width + world.l;
   wp.y = -dy_world * (float)( pp.y - plotter_pix_height - plotter_pix_top ) /
          (float)plotter_pix_height + world.b;
   return wp;
}

/*--------------------- same for phase plots ----------------------------*/

static INLINE PixelPoint
phase_world_to_pixel( WorldPoint wp )
{
   PixelPoint pp;

   pp.x = (int)((wp.x - world.l)*plotter_pix_width/dx_world +0.5) + plotter_pix_left;
   pp.y = plotter_pix_height - 
          (int)((wp.y - world.pb)*plotter_pix_height/dyp_world +0.5) + plotter_pix_top;
   return pp;
}

/*-----------------------------------------------------------------*/

static INLINE float
phase_ypixel_to_yworld( int p )
{
   float w;

   w = -dyp_world * (float)( p - plotter_pix_height - plotter_pix_top ) /
          (float)plotter_pix_height + world.pb;
   return w;
}

/*-----------------------------------------------------------------*/

static INLINE int
phase_yworld_to_ypixel( float wy )
{
   int py;

   py = plotter_pix_height - 
          (int)((wy - world.pb)*plotter_pix_height/dyp_world +0.5) + plotter_pix_top;
   return py;
}

/*-----------------------------------------------------------------*/

static INLINE WorldPoint
phase_pixel_to_world( PixelPoint pp )
{
   WorldPoint wp;

   wp.x = dx_world * (float)( pp.x - plotter_pix_left ) /
          (float)plotter_pix_width + world.l;
   wp.y = -dyp_world * (float)( pp.y - plotter_pix_height - plotter_pix_top ) /
          (float)plotter_pix_height + world.pb;
   return wp;
}

/*-----------------------------------------------------------------*/

/*
 * this function relates pixels to data point number. This is used for 
 * the internal plotter line list but should probably be moved elsewhere.
 */
static INLINE int
pixel_to_point( int pixel )
{
 return  (int)(
	 (float)( pixel - plotter_pix_left ) *
	 (float)( pt_range.end - pt_range.begin ) / 
	 (float)( plotter_pix_width ) + 0.5) + pt_range.begin;
}

/*------------------------------------------------------------------
 * Transformations from primary to secondary units
 *------------------------------------------------------------------*/

/*-----------------------------------------------------------------*/

static float
kayser_to_nm( float sigma )
{
   float sigma2;

   /* 
    * trap zero wavenumbers because the first alias starts at 0/cm.
    */
   if  ( sigma != 0.0 )        
     {                       
	if ( sigma > 50000.0 ) /* vacuum */
	  {
	     return 1.0e7 / sigma;
	  }
	else
	  {
	     sigma2 = sigma*sigma;
	     return 1.0e7 / ( sigma * ( 1.0 + 8343.05e-8 + 15999.0 / 
			              ( 3.89e9-sigma2 ) + 2406294.0 / 
			              ( 130.0e8-sigma2 ) ) );
	  }
     }
   else
     return 0.0;
}

/*-----------------------------------------------------------------*/

static float
kayser_to_hz( float sigma )
{
   return sp.velocity * sigma / sp.sigma_laser;
}

/*-----------------------------------------------------------------*/

static float
kayser_to_thz( float sigma )
{
   return C_VAC * sigma / 1.0e12;
}

/*------------------------------------------------------------------
 * Clipping functions 
 *------------------------------------------------------------------*/

static INLINE void
clip_line( PixelPoint *p1, PixelPoint *p2, PixelPoint *pa, PixelPoint *pb )
{
   int in1;
   int in2;

   /*
    * check if end points are on plotter window
    */
   in1 = in_interval( plotter_pix_bottom, plotter_pix_top, p1->y );
   in2 = in_interval( plotter_pix_bottom, plotter_pix_top, p2->y );

   *pa = *p1;
   *pb = *p2;

   if ( in1 == FALSE )           /* first point or both outside */
     clip_point( p1, p2, p1, pa );   
     
   if ( in2 == FALSE )           /* second point or both outside */
     clip_point( p1, p2, p2, pb );
}

/*-----------------------------------------------------------------*/

/*
 * C is the point of A or B which is outside the y range
 */
static INLINE void
clip_point( PixelPoint *A, PixelPoint *B, PixelPoint *C, PixelPoint *pc )
{
   register float cot_a;

   if ( C->y < plotter_pix_top )     
     {
	/*
	 * first trap a trivial case
	 */
	if ( A->x == B->x || A->y == B->y )
	  {
	     pc->x = C->x;
	     pc->y = plotter_pix_top;
	  }
	else
	  {
	     /* 
	      * now the general case, first calculate slope of line 
	      */
	     cot_a = (float)(B->x - A->x) / (float)(A->y - B->y);
	     pc->x = (int)( 0.5 + (float)(A->y - plotter_pix_top) * cot_a) + A->x;
	     pc->y = plotter_pix_top;
	  }
     }
   
   if ( C->y > plotter_pix_bottom )
     {
	if ( A->x == B->x || A->y == B->y )
	  {
	     pc->x = C->x;
	     pc->y = plotter_pix_bottom;
	  }
	else
	  {
	     cot_a = (float)(B->x - A->x) / (float)(A->y - B->y);
	     pc->x = (int)( 0.5 + (float)(A->y - plotter_pix_bottom) * cot_a) + A->x;
	     pc->y = plotter_pix_bottom;
	  }
     }
}

/*-----------------------------------------------
 * additional functions to speed up drawing
 *-----------------------------------------------*/

void
inhibit_()
{
   display_inhibit( loc_plot_widget );
   save_cmd = last_cmd;        /* save last display command */    
}

/*-----------------------------------------------------------------*/

void
update_()
{
   display_permit( loc_plot_widget );
   display_draw_from( loc_plot_widget, save_cmd ); 
}  

/*-----------------------------------------------------------------*/

void
polybeg_()
{
   display_begin_polygon( loc_plot_widget );
}

/*-----------------------------------------------------------------*/

void
polyadd_( int *x, int *y )
{
   world_cp.x = *x;
   world_cp.y = *y;
   pixel_cp = world_to_pixel( world_cp );
   display_polygon_add(loc_plot_widget,pixel_cp.x,pixel_cp.y);
}

/*-----------------------------------------------------------------*/

void
polyend_()
{
   display_end_polygon( loc_plot_widget );
}

/*-----------------------------------------------------------------*/

/*-----------------------------------------------
 * Miscellanea
 *-----------------------------------------------*/

void
ssmpar_(int *alias, double *fsr, double *sigma_l, float *velocity)
{
   sp.alias = *alias;
   sp.fsr = *fsr;
   sp.sigma_laser = *sigma_l;
   sp.velocity = *velocity;
}


void
screen_dump( char filename[], char size_string[], int landscape )
{
   float width, height;
   char *p, *str_beg;
   char buff[32];
   char datname[FILE_NAME_LEN];
   int ncpt;
   int xpos;
   int xlen;
   int fwid;
   display_cmd name_cmd;

   /*
    * before writing the postscript file plot the name of the
    * currently open data file on the plot for identification
    */
   fncpinfo_( datname, &ncpt, FILE_NAME_LEN );
   StrForToC( datname, FILE_NAME_LEN );
   if ( strlen(datname) > 0 )
     {
	xlen = display_font_width( loc_plot_widget, small_font, datname );
	xpos = plotter_pix_right - xlen;
	name_cmd = display_draw_text(loc_plot_widget, datname,
				     xpos, plotter_line1 + label_small_pix_height / 2,
				     small_font, text_color);
     }

   if (size_string[0] == '\0') 
     {
	width  = HARDCOPY_WIDTH;
	height = HARDCOPY_HEIGHT;
     }
   else
     {
	strcpy( buff, size_string );             /* make a local copy */
	if ( strlen(buff) <= 1 )                 /* sanity check */
	  {
	     width  = HARDCOPY_WIDTH;
	     height = HARDCOPY_HEIGHT;
	     return;
	  }
	p = str_beg = buff;
	while ( *p != 'x' && *p != 'X' && *p != '\0' ) 
	  p++;
	if ( *p == '\0' )            /* 'x' is missing, bail out */
	  {
	     width  = HARDCOPY_WIDTH;
	     height = HARDCOPY_HEIGHT;
	     return;
	  }
	*p = '\0';
	width = atof( str_beg );     /* first part of string up to 'x' */
	str_beg = ++p; 
	height = atof( str_beg );    /* everything after 'x' */
     }
   display_postscript( loc_plot_widget, filename, width, height, landscape );

   /*
    * remove the id string if one was printed
    */
   if ( strlen(datname) > 0 )
     {
	display_erase(loc_plot_widget, name_cmd);
	fwid = display_font_width( loc_plot_widget, small_font, datname );
	set_objects_printable( FALSE );         /* don't include in PS dumps */
	display_fill_rectangle(loc_plot_widget, 
			       xpos, plotter_line1,
			       fwid, label_small_pix_height+2, frame_color);
	set_objects_printable( TRUE );     
     }
}

/*-----------------------------------------------------------------*/

void
stom_( int *ib )
{
   int mouse_button;
   int k;

   mouse_button = *ib;
   sto_num[mouse_button] = mc_num[mouse_button];
   for ( k=0; k<sto_num[mouse_button]; k++ )
     stored_click[mouse_button][k] = mouse_click[mouse_button][k];
}

/*-----------------------------------------------------------------*/

void
recm_( int *ib )
{
   int mouse_button;
   int k;

   mouse_button = *ib;
   mc_num[mouse_button] = sto_num[mouse_button];
   for ( k=0; k<mc_num[mouse_button]; k++ )
     {
        mouse_click[mouse_button][k] = stored_click[mouse_button][k];

	/*
	 * pixel coordinate system may have changed --> adjust
	 */	
	mouse_click[mouse_button][k].p =
	  world_to_pixel( stored_click[mouse_button][k].w );
        draw_cross_marker( &mouse_click[mouse_button][k],
                           mouse_button == 0 ? button1_color : button2_color );
     }
}

/*-----------------------------------------------------------------*/

void
plotfont_(int *font_idx, char *font_string, ftnlen len)
{
   int tmp_font;
   int ch, shift, ctrl, meta;
   char font_name[256];

   strncpy(font_name, font_string, len);
   StrForToC(font_name, len);

   /*
    * try to find and load font
    */
   tmp_font = display_load_font( loc_plot_widget, font_name );
   if ( tmp_font == -1 )   /* font was not found */
     return;

   /*
    * small font
    */
   if ( *font_idx == 1 )
     {
	small_font = tmp_font;
	label_small_pix_height = display_font_height( loc_plot_widget, small_font );
	cslen = display_font_width( loc_plot_widget, small_font, "X" );
     }

   /*
    * large font
    */
   if ( *font_idx == 2 )
     {
        large_font = tmp_font;
	label_pix_width  = display_font_width( loc_plot_widget, large_font, "01234567890" );
	label_pix_height = display_font_height( loc_plot_widget, large_font );
	clen  = display_font_width( loc_plot_widget, large_font, "X" );
      }

   /*
    * re-define plotter variables
    */
   plotter_pix_height = display_pix_height -  2 * SPACE * label_pix_height;
   plotter_pix_top    = display_pix_height - plotter_pix_height -
                        SPACE * label_pix_height;
   plotter_pix_bottom = display_pix_height - SPACE * label_pix_height;
   plotter_line1      = plotter_pix_top / 3 + 4;
   plotter_line2      = plotter_line1 * 2;

   /*
    * re-plot data
    */
   ch = (int)'P';   /* fake a key stroke */
   shift = 1;
   ctrl  = 0;
   meta  = 0;
   keycmd_( &ch, &shift, &ctrl, &meta ); 
}

/*-----------------------------------------------------------------*/
 
/*----------------------------------------------------
 * Fortran callable graphics functions
 *----------------------------------------------------*/

void
getpwid_( int *iwid ) 
{
   *iwid = plotter_pix_width;
}

/*-----------------------------------------------------------------*/

void
getphei_( int *ihei )
{
   *ihei = plotter_pix_height;
}

/*-----------------------------------------------------------------*/

void
clrplot_()
{
   last_numstr_mbx = 0;
   display_clear( loc_plot_widget );
   NewPlotter();
}
 
/*-----------------------------------------------------------------*/

void
svcsys_(float *left,float *right,float *bottom,float *top,float *g_min,float *g_max)
{
   /*
    * save the four corners of the world
    */
   world.min = *g_min;
   world.max = *g_max;
   world.l = *left;
   world.r = *right;
   world.b = *bottom;
   world.t = *top;
   world.defined = TRUE;
   
   /*
    * determine size of world coordinate window
    */
   dx_world = world.r - world.l;
   dy_world = world.t - world.b;

   /*
    * dx_world is never 0 except when some XXXXX tries to zoom in too far
    * trap this case here.
    */
   if ( dx_world == 0.0 )
     {
	dx_world = 1.0;
	world.r = world.l + dx_world;
     }

   /*
    * dy_world is never negative except when someone types a plot command
    * before there is anything reasonable in the r array. The following kludge
    * traps this case.
    */
   if ( dy_world <= 0.0 )
     {
	world.b =  0.0;
	world.t =  1.0;
	dy_world = 1.0;
     }
}

/*-----------------------------------------------------------------*/

void
sabsref_(int *npref)
{
   pt_range.reference = *npref;
}

/*-----------------------------------------------------------------*/

void
svpsys_( float *bot, float *top )
{
   /*
    * top and bottom af phase world, left and right as in normal plot
    */
   world.pb = *bot;
   world.pt = *top;

   dyp_world = world.pt - world.pb;

   /*
    * in case someone zooms in too far
    */
   if ( dyp_world <= 0.0 )
     {
	world.pb  = -0.1; 
	world.pt  =  0.1;
	dyp_world =  0.2;
     }
}

/*-----------------------------------------------------------------*/

void
gvmove_( float *x, float *y )
{
   world_cp.x = *x;
   world_cp.y = *y;
   pixel_cp = world_to_pixel( world_cp );
   display_moveto( loc_plot_widget, pixel_cp.x, pixel_cp.y );
}

/*-----------------------------------------------------------------*/

void
gvline_( float *xnew, float *ynew )
{
   /*
    * does not clip !
    */
   world_cp.x = *xnew;
   world_cp.y = *ynew;
   pixel_cp = world_to_pixel( world_cp );
   last_cmd = display_draw_line( loc_plot_widget, pixel_cp.x, pixel_cp.y );
}

/*-----------------------------------------------------------------*/

void
pgvmove_( float *x, float *y )
{
   phase_world_cp.x = *x;
   phase_world_cp.y = *y;
   phase_pixel_cp = phase_world_to_pixel( phase_world_cp );
   display_moveto( loc_plot_widget, phase_pixel_cp.x, phase_pixel_cp.y );
}

/*-----------------------------------------------------------------*/

void
pgvline_( float *xnew, float *ynew )
{
   /*
    * does not clip !
    */
   phase_world_cp.x = *xnew;
   phase_world_cp.y = *ynew;
   phase_pixel_cp = phase_world_to_pixel( phase_world_cp );
   last_cmd = display_draw_line( loc_plot_widget, phase_pixel_cp.x, phase_pixel_cp.y );
}

/*-----------------------------------------------------------------*/

void
pgvcvect_( float *xa, float *ya, float *xb, float *yb)
{
   WorldPoint wp;
   PixelPoint pia, pib;   /* in */
   PixelPoint poa, pob;   /* out */

   /*
    * translate into pixel coordinates
    */
   wp.x = *xa;
   wp.y = *ya;
   pia = phase_world_to_pixel( wp );
   wp.x = *xb;
   wp.y = *yb;
   pib = phase_world_to_pixel( wp );

   /*
    * clip to pixel coordinates
    */
   clip_line( &pia, &pib, &poa, &pob );
   last_cmd = display_draw_line_simple( loc_plot_widget,
				        poa.x, poa.y, pob.x, pob.y);
}

/*-----------------------------------------------------------------*/


void
drwphp_( int *igood, float *w, float *ph )
{
   WorldPoint wp;
   PixelPoint pp;
   int point_color;

   /*
    * transform to pixel coordinates
    */
   wp.x = *w;
   wp.y = *ph;
   pp = phase_world_to_pixel( wp );

   /*
    * set colour according to goodness of point
    */
   if ( *igood == 0 )
     point_color = bad_color;
   if ( *igood == 1 )
     point_color = phase_color;

   /*
    * draw phase point
    */
   last_cmd = display_fill_circle( loc_plot_widget,
				   pp.x, pp.y,
				   RADIUS, point_color );
}

/*-----------------------------------------------------------------*/

void
gvcvect_( float *xa, float *ya, float *xb, float *yb)
{
   WorldPoint wp;
   PixelPoint pia, pib;   /* in */
   PixelPoint poa, pob;   /* out */

   /*
    * translate into pixel coordinates
    */
   wp.x = *xa;
   wp.y = *ya;
   pia = world_to_pixel( wp );
   wp.x = *xb;
   wp.y = *yb;
   pib = world_to_pixel( wp );

   /*
    * clip to pixel coordinates
    */
   clip_line( &pia, &pib, &poa, &pob );
   last_cmd = display_draw_line_simple( loc_plot_widget,
				        poa.x, poa.y, pob.x, pob.y);
}

/*-----------------------------------------------------------------*/

void
gpmove_( int *ix, float *y )
{
   pixel_cp.x = plotter_pix_left + *ix;
   pixel_cp.y = yworld_to_ypixel( *y );
   display_moveto( loc_plot_widget, pixel_cp.x, pixel_cp.y );
}

/*-----------------------------------------------------------------*/

void
gpline_( int *ixnew, float *ynew )
{
   /*
    * does not clip !
    */
   pixel_cp.x = plotter_pix_left + *ixnew;
   pixel_cp.y = yworld_to_ypixel( *ynew);
   last_cmd = display_draw_line( loc_plot_widget, pixel_cp.x, pixel_cp.y );
}

/*-----------------------------------------------------------------*/

void
gpbin_( int *ibin, float *a, float *b )
{
   PixelPoint p1, p2;  /* before clipping */
   PixelPoint pa, pb;  /* after clipping */

   /* 
    * transform to pixels 
    */
   p1.x = plotter_pix_left + *ibin;
   p1.y = yworld_to_ypixel( *a );
   p2.x = plotter_pix_left + *ibin;
   p2.y = yworld_to_ypixel( *b );
   if ( p1.y == p2.y )
     p2.y++;            /* plot at least one pixel */

   /*
    * and clip to plotter
    */
   clip_line( &p1, &p2, &pa, &pb );
   last_cmd = display_draw_line_simple( loc_plot_widget, pa.x, pa.y, pb.x, pb.y );
}

/*-----------------------------------------------------------------*/

void
pgpbin_( int *ibin, float *a, float *b )
{
   PixelPoint p1, p2;  /* before clipping */
   PixelPoint pa, pb;  /* after clipping */

   /*
    * transform to pixels
    */
   p1.x = plotter_pix_left + *ibin;
   p1.y = phase_yworld_to_ypixel( *a );
   p2.x = plotter_pix_left + *ibin;
   p2.y = phase_yworld_to_ypixel( *b );
   if ( p1.y == p2.y )
     p2.y++;            /* plot at least one pixel */

   /*
    * and clip to plotter
    */
   clip_line( &p1, &p2, &pa, &pb );
   last_cmd = display_draw_line_simple( loc_plot_widget, pa.x, pa.y, pb.x, pb.y );
}

/*-----------------------------------------------------------------*/

void
setcolor_( char color_name[], ftnlen len )
{
   char col_str[16];                 /* local copy of color string */
   int k;

   for ( k=0; k<16; k++ )            /* turn into C string safely */
     col_str[k] = ' ';
   for ( k=0; ( k<len )&&( k<16 ); k++ )
     col_str[k] = color_name[k];
   StrForToC( col_str, 16 );        
   cur_color = display_set_color(loc_plot_widget, col_str );  
}

/*-----------------------------------------------------------------*/

void
dashsty_( int *flag )
{
   int thickness;
   int pattern;

   /*
    * retrieve default line thickness
    */
   display_get_linestyle( loc_plot_widget, &thickness, &pattern );
   if ( *flag <= 1 )
     pattern = *flag;      /* solid lines or dotted lines */
   display_set_linestyle( loc_plot_widget, thickness, pattern );
}

/*-----------------------------------------------------------------*/

/*
 * Algorithm for axis labelling:
 * -----------------------------
 *
 * 1. Determine the number of labels that can be accomodated along the axis
 * 2. Calculate the spacing between the labels in world coordinates
 * 3. round the spacing to the nearest pretty number on a log scale. Nice spacings
 *    are e.g.  1,2,5,10,20,50,100,200,500,...... you get the idea.
 */

void
xaxis_( float *left, float *right )
{
   int num_labels;
   int large_tic_pix;
   int small_tic_pix;
   int label_len;
   int k;
   int ix;
   int iy;
   float dx;
   float large_dist;
   float small_dist;
   float xpos;
   float sxpos;
   float ordmag;    /* order of magnitude of the number */
   WorldPoint wp;
   PixelPoint pp;
   char numstr[64];
   float multiplier[] = { 1.0, 2.0, 5.0, 10.0 };

   /*
    * determine how much room there is for labels (approximately)
    */
   num_labels = ( plotter_pix_width + label_pix_width/2 ) / label_pix_width;

   /*
    * calculate distance between labels in world coordinates
    */
   dx = fabs(dx_world / num_labels);

   /*
    * find pretty upper bound of that distance
    */
   ordmag = pow( 10.0, floor(log10(dx)) ); 
   for ( k=1; k<=4; k++ ) 
     {
	large_dist = ordmag * multiplier[k];
	if (large_dist >= dx )
	  break;
     }
   small_dist = large_dist / 10.0;    /* distance between small tics */

   large_tic_pix = plotter_pix_height * LARGE_X_TIC_MARK / 100.0;
   small_tic_pix = plotter_pix_height * SMALL_X_TIC_MARK / 100.0;

   /*
    * plot axis tic marks and labels
    */
   if ( *left < *right )
     /*********************
      * increasing x - axis
      *********************/
     {
       /*
	* calculate position of first large tic mark
	*/
       xpos = large_dist * ceil(*left / large_dist);
       if ( xpos - large_dist == *left ) 
	 xpos = *left;

       /*
	* plot small tic marks from the first large one to the left border
	*/
       sxpos = xpos - small_dist;
       k = 1;
       while ( sxpos >= *left ) 
	 {
	   wp.x = sxpos;
	   wp.y = world.b;
	   pp = world_to_pixel( wp );
	   if ( k == 5 )
	     {
	       display_moveto( loc_plot_widget, pp.x, pp.y+TIC_STICK_OUT );
	       display_draw_line( loc_plot_widget, pp.x, pp.y-large_tic_pix );
	     }
	   else
	     {
	       display_moveto( loc_plot_widget, pp.x, pp.y );
	       display_draw_line( loc_plot_widget, pp.x, pp.y-small_tic_pix );
	     }
	   sxpos -= small_dist;
	   k++;
	 }

       /* 
	* plot the large tic marks interspersed with small ones
	*/
       do
	 {
	   wp.x = xpos;
	   wp.y = world.b;
	   pp = world_to_pixel( wp );
	   display_moveto( loc_plot_widget, pp.x, pp.y+TIC_STICK_OUT );
	   display_draw_line( loc_plot_widget, pp.x, pp.y-large_tic_pix );
	   sprintf( numstr, "%-g", xpos );
	   remove_trailing_blanks(numstr);
	   label_len = display_font_width( loc_plot_widget, large_font, numstr );
	   display_draw_text( loc_plot_widget, numstr, 
			      pp.x - label_len/2, pp.y+label_pix_height+2, 
			      large_font, text_color );
	   sxpos = xpos + small_dist;
	   xpos += large_dist;            /* position of next large label */
	   
	   /*
	    * plot small tic marks
	    */
	   k = 1;   /* count 'em */
	   while ( sxpos < xpos && sxpos < *right )
	     {
	       wp.x = sxpos;
	       wp.y = world.b;
	       pp = world_to_pixel( wp );
	       if ( k == 5 )
		 {
		   display_moveto( loc_plot_widget, pp.x, pp.y+TIC_STICK_OUT );
		   display_draw_line( loc_plot_widget, pp.x, pp.y-large_tic_pix );
		 }
	       else
		 {
		   display_moveto( loc_plot_widget, pp.x, pp.y );
		   display_draw_line( loc_plot_widget, pp.x, pp.y-small_tic_pix );
		 }
	       sxpos += small_dist;
	       k++;
	     }
	   
	 } while( xpos <= *right ); 
     }
   else
     /*********************
      * decreasing x - axis
      *********************/
     {
       /*
	* calculate position of first large tic mark
	*/
       xpos = large_dist * floor(*left / large_dist);
       if ( xpos + large_dist == *left ) 
	 xpos = *left;

       /*
	* plot small tic marks from the first large one to the left border
	*/
       sxpos = xpos + small_dist;
       k = 1;
       while ( sxpos <= *left ) 
	 {
	   wp.x = sxpos;
	   wp.y = world.b;
	   pp = world_to_pixel( wp );
	   if ( k == 5 )
	     {
	       display_moveto( loc_plot_widget, pp.x, pp.y+TIC_STICK_OUT );
	       display_draw_line( loc_plot_widget, pp.x, pp.y-large_tic_pix );
	     }
	   else
	     {
	       display_moveto( loc_plot_widget, pp.x, pp.y );
	       display_draw_line( loc_plot_widget, pp.x, pp.y-small_tic_pix );
	     }
	   sxpos += small_dist;
	   k++;
	 }

       /* 
	* plot the large tic marks interspersed with small ones
	*/
       do
	 {
	   wp.x = xpos;
	   wp.y = world.b;
	   pp = world_to_pixel( wp );
	   display_moveto( loc_plot_widget, pp.x, pp.y+TIC_STICK_OUT );
	   display_draw_line( loc_plot_widget, pp.x, pp.y-large_tic_pix );
	   sprintf( numstr, "%-g", xpos );
	   remove_trailing_blanks(numstr);
	   label_len = display_font_width( loc_plot_widget, large_font, numstr );
	   display_draw_text( loc_plot_widget, numstr, 
			      pp.x - label_len/2, pp.y+label_pix_height+2, 
			      large_font, text_color );
	   sxpos = xpos - small_dist;
	   xpos -= large_dist;            /* position of next large label */
	   
	   /*
	    * plot small tic marks
	    */
	   k = 1;   /* count 'em */
	   while ( sxpos > xpos && sxpos > *right )
	     {
	       wp.x = sxpos;
	       wp.y = world.b;
	       pp = world_to_pixel( wp );
	       if ( k == 5 )
		 {
		   display_moveto( loc_plot_widget, pp.x, pp.y+TIC_STICK_OUT );
		   display_draw_line( loc_plot_widget, pp.x, pp.y-large_tic_pix );
		 }
	       else
		 {
		   display_moveto( loc_plot_widget, pp.x, pp.y );
		   display_draw_line( loc_plot_widget, pp.x, pp.y-small_tic_pix );
		 }
	       sxpos -= small_dist;
	       k++;
	     }
	   
	 } while( xpos >= *right ); 
     }

   /*
    * finally add a label for the unit
    */
   if ( wavenumber_scale == TRUE ) 
     strcpy( numstr, "cm" );
   else
     strcpy( numstr, "nm" );

   label_len = display_font_width(loc_plot_widget,large_font,numstr);
   ix = plotter_pix_right -  label_len / 2;
   iy = plotter_pix_bottom + 5 * label_pix_height / 2;  /* same as info_line */

   /*
    * the label is plotted below the lower right corner of the plotting area
    */
   last_cmd = display_draw_text(loc_plot_widget, numstr, 
				ix, iy, 
				large_font, text_color );     

   /*
    * add the "-1" for wavenumbers
    */
   if ( wavenumber_scale == TRUE )
     {
	ix += label_len;  /* position for -1 */
	iy -= 5;
	last_cmd = display_draw_text(loc_plot_widget, "-1", 
				     ix, iy, 
				     small_font, text_color );     
     }
}

/*-----------------------------------------------------------------*/

/*
 * the y axis is different in that a multiplier is displayed if
 * data are outside 10^+-3
 */
void
yaxis_( float *bottom, float *top )
{
   int num_labels;
   int large_tic_pix;
   int small_tic_pix;
   int label_len;
   int k;
   int squash;
   float dy;
   float large_dist;
   float small_dist;
   float ypos;
   float sypos;
   float ordmag;
   float magnitude;
   float squash_factor;
   float sup;
   int py;
   char numstr[64];
   float multiplier[] = { 1.0, 2.0, 5.0, 10.0 };

   /*
    * determine how much room there is for labels (approximately)
    * leave enough space between labels
    */
   num_labels = plotter_pix_height / ( 2.5 * label_pix_height );

   /*
    * calculate distance between labels in world coordinates
    */
   dy = dy_world / num_labels;

   /*
    * find pretty upper bound of that distance
    */
   ordmag = pow( 10.0, floor(log10(dy)) ); 
   for ( k=1; k<=4; k++ ) 
     {
	large_dist = ordmag * multiplier[k];
	if (large_dist >= dy )
	  break;
     }
   small_dist = large_dist / 10.0;    /* distance between small tics */

   /*
    * on the y axis an exponential factor is plotted if the magnitude of
    * the scale exceeds 3 orders of magnitude.
    */
   sup = fabs( *top ) > fabs( *bottom ) ? fabs( *top ) : fabs( *bottom );
   magnitude = floor( log10(sup) );
   if ( fabs(magnitude) > 3.0  )
     {
	squash_factor = pow( 10.0, magnitude );
	squash = ON;
     }
   else
     {
	squash_factor = 1.0;
	squash = OFF;
     }

   /*
    * calculate position of first large tic mark
    */
   ypos = large_dist * ceil(*bottom / large_dist);
   if ( ypos - large_dist == *bottom ) 
     ypos -= large_dist;

   large_tic_pix = plotter_pix_width * LARGE_Y_TIC_MARK / 100.0;
   small_tic_pix = plotter_pix_width * SMALL_Y_TIC_MARK / 100.0;

   /*
    * plot small tic marks from the first large one to the bottom border
    */
   sypos = ypos - small_dist;
   k = 1;
   while ( sypos >= *bottom ) 
     {
	py = yworld_to_ypixel( sypos );
	display_moveto( loc_plot_widget, plotter_pix_left , py );
	if ( k == 5 )
	  display_draw_line( loc_plot_widget, plotter_pix_left+large_tic_pix, py );
	else
	  display_draw_line( loc_plot_widget, plotter_pix_left+small_tic_pix, py );
	sypos -= small_dist;
	k++;
     }

   /* 
    * plot the large tic marks interspersed with small ones
    */
   do
     {
	py = yworld_to_ypixel( ypos );
	display_moveto( loc_plot_widget, plotter_pix_left, py );
	display_draw_line( loc_plot_widget, plotter_pix_left + large_tic_pix, py );
	sprintf( numstr, "%g", ypos / squash_factor );
	label_len = display_font_width( loc_plot_widget, large_font, numstr );
	last_cmd = 
	  display_draw_text(loc_plot_widget, numstr, 
			    plotter_pix_left - label_len - 2, 
			    py + (label_pix_height+1)/2, 
			    large_font, text_color );
	sypos = ypos + small_dist;
	ypos  = ypos + large_dist;   /* position of next large label */

	/*
	 * plot small tic marks, the middle one is larger
         */
	k = 1;   
	while ( sypos < ypos && sypos < *top )
	  {
	     py = yworld_to_ypixel( sypos );
	     display_moveto( loc_plot_widget, plotter_pix_left, py );
	     if ( k == 5 )
	       last_cmd = 
		 display_draw_line( loc_plot_widget, plotter_pix_left+large_tic_pix, py );
	     else
	       last_cmd = 
		 display_draw_line( loc_plot_widget, plotter_pix_left+small_tic_pix, py );
	     sypos += small_dist;
	     k++;
	  }

     } while( ypos <= *top ); 

   /* 
    * write exp factor above y scale if numbers are squashed
    */
   if ( squash == ON )
     {
	strcpy( numstr, "x10" );

	/* 
         * place it 4 chars left from plotter window 
	 */
	last_cmd = 
	  display_draw_text(loc_plot_widget, numstr, 
			    plotter_pix_left - clen*6, 
			    plotter_pix_top - label_pix_height/2, 
			    large_font, text_color );

	/* 
	 * finally the exponent 
	 */
	sprintf( numstr, "%d", (int)log10(squash_factor) );
	last_cmd = 
	  display_draw_text(loc_plot_widget, numstr, 
			    plotter_pix_left - 3*clen, plotter_pix_top - label_pix_height, 
			    small_font, text_color );
     }
}

/*-----------------------------------------------------------------*/

void
phaxis_( float *bottom, float *top )
{
   int num_labels;
   int large_tic_pix;
   int small_tic_pix;
   int label_len;
   int k;
   float dyp;
   float large_dist;
   float small_dist;
   float ypos;
   float sypos;
   float ordmag;
   int py;
   char numstr[64];
   float multiplier[] = { 1.0, 2.0, 5.0, 10.0 };

   /*
    * determine how much room there is for labels (approximately)
    * leave enough space between labels
    */
   num_labels = plotter_pix_height / ( 2.3 * label_pix_height );

   /*
    * calculate distance between labels in world coordinates
    */
   dyp = dyp_world / num_labels;

   /*
    * find pretty upper bound of that distance
    */
   ordmag = pow( 10.0, floor(log10(dyp)) ); 
   for ( k=1; k<=4; k++ ) 
     {
	large_dist = ordmag * multiplier[k];
	if (large_dist >= dyp )
	  break;
     }
   small_dist = large_dist / 10.0;    /* distance between small tics */

   /*
    * calculate position of first large tic mark
    */
   ypos = large_dist * ceil(*bottom / large_dist);
   if ( ypos - large_dist == *bottom ) 
     ypos -= large_dist;

   large_tic_pix = plotter_pix_width * LARGE_Y_TIC_MARK / 100.0;
   small_tic_pix = plotter_pix_width * SMALL_Y_TIC_MARK / 100.0;

   /*
    * plotting of phase data in PHASE_COLOR (default: red3)
    */
   display_set_color_value( loc_plot_widget, phase_color );

   /*
    * plot small tic marks from the first large one to the bottom border
    */
   sypos = ypos - small_dist;
   k = 1;
   while ( sypos >= *bottom ) 
     {
	py = phase_yworld_to_ypixel( sypos );
	if ( k == 5 )
	  display_moveto( loc_plot_widget, plotter_pix_right - large_tic_pix , py );
	else
	  display_moveto( loc_plot_widget, plotter_pix_right - small_tic_pix , py );
	display_draw_line( loc_plot_widget, plotter_pix_right, py );
	sypos -= small_dist;
	k++;
     }

   /* 
    * plot the large tic marks interspersed with small ones
    */
   do
     {
	py = phase_yworld_to_ypixel( ypos );
	display_moveto( loc_plot_widget, plotter_pix_right - large_tic_pix, py );
	display_draw_line( loc_plot_widget, plotter_pix_right, py );
	sprintf( numstr, "%+.1f", ypos );
	label_len = display_font_width( loc_plot_widget, small_font, numstr );
	display_draw_text(loc_plot_widget, numstr, 
			  plotter_pix_right+2, 
			  py + (label_small_pix_height+1)/2, 
			  large_font, phase_color );
	sypos = ypos + small_dist;
	ypos  = ypos + large_dist;   /* position of next large label */

	/*
	 * plot small tic marks, the middle one is larger
         */
	k = 1;   
	while ( sypos < ypos && sypos < *top )
	  {
	     py = phase_yworld_to_ypixel( sypos );
	     if ( k == 5 )
	       display_moveto( loc_plot_widget, plotter_pix_right - large_tic_pix , py );
	     else
	       display_moveto( loc_plot_widget, plotter_pix_right - small_tic_pix , py );
	     last_cmd = 
	       display_draw_line( loc_plot_widget, plotter_pix_right, py );
	     sypos += small_dist;
	     k++;
	  }

     } while( ypos <= *top ); 
}

/*-----------------------------------------------------------------*/

void
chkzoom_( int *iz )
{
   *iz = ( phase_zoom == TRUE ) ? 1 : 0;
}

/*-----------------------------------------------------------------*/

void
bmeter_( int *nopp, int *nbeg, int *nend, float *wf, float *wl )
{
   int xbeg;     /* begin of bar */
   int ybeg;
   int xend;     /* end of bar */
   int mbeg;     /* begin of marker */
   int mend;     /* end of marker */
   int mwid;     /* width of bar marker */
   int bwid;     /* width and height of bar */
   int bhei;
   float xp;     /* number of pixels */
   char str[64];
   
   xbeg = plotter_pix_left;
   ybeg = plotter_pix_top / 3;
   xend = plotter_pix_width * METER_END / 100.0;
   bwid = xend - xbeg + 1;
   bhei = plotter_pix_top / 4;
   xp   = (float)bwid;
   mbeg = xbeg + (int)( xp * (float)*nbeg / (float)*nopp + 0.5 );
   mend = xbeg + (int)( xp * (float)*nend / (float)*nopp + 0.5 );
   mwid = mend - mbeg + 1;

   /*
    * plot bar meter
    */
   set_objects_printable( FALSE );  /* the following will not appear on screen dumps */

   display_fill_rectangle( loc_plot_widget, xbeg, ybeg,
                           bwid, bhei, grey_color );
   display_fill_rectangle( loc_plot_widget, mbeg, ybeg,
                           mwid, bhei, bar_color );
   display_draw_rectangle( loc_plot_widget, xbeg, ybeg,
                           bwid, bhei, black_color );

   /*
    * plot the range of data in r array in primary units
    */
   ybeg = label_small_pix_height;
   if ( wavenumber_scale == TRUE )
     sprintf( str, "%.6g/cm - %.6g/cm", *wf, *wl );
   else
     sprintf( str, "%.6g nm - %.6g nm", *wf, *wl );

   last_cmd = 
     display_draw_text( loc_plot_widget, str, xbeg, ybeg,
		       small_font, text_color );

   set_objects_printable( TRUE );

   pt_range.begin = *nbeg;
   pt_range.end   = *nend;
}

/*-----------------------------------------------------------------*/

void
shpix_( int *pppix )
{
   char sticky_label[64];
   int xpos;
   
   xpos = plotter_pix_width * PIXEL_TXT / 100.0;
   if ( *pppix == 0 )
     strcpy( sticky_label, "                             " );
   else
     sprintf( sticky_label, "%d points/pixel         ", *pppix );
   last_cmd = 
     display_draw_text(loc_plot_widget, sticky_label,
		       xpos, plotter_line1 + label_small_pix_height / 2, 
		       small_font, text_color );
}

/*-----------------------------------------------------------------*/

void
dflush_()
{
   display_flush( loc_plot_widget );
}

/*-----------------------------------------------------------------*/

void
dmouse_( int *ibut, int *iclick, float *ax, float *ay, int *ix )
{
   float x,y;
   WorldPoint wp;
   PixelPoint pp;

   if ( mc_num[*ibut] == 0 )
     return;

   x = mouse_click[*ibut][*iclick].w.x;
   y = mouse_click[*ibut][*iclick].w.y;
   if ( phase_zoom == FALSE )
     {
	*ax = x;
	*ay = y;
     }
   else
     {
	wp.x = x;
	wp.y = y;
	pp = world_to_pixel( wp );
	wp = phase_pixel_to_world( pp );
	*ax = wp.x;
	*ay = wp.y;
     }
   *ix = pixel_to_point( mouse_click[*ibut][*iclick].p.x );
}

/*-----------------------------------------------------------------*/

void
pdmouse_( int *ibut, int *iclick, float *pax, float *pay )
{
   WorldPoint wp;
   PixelPoint pp;

   if ( mc_num[*ibut] == 0 )
     return;

   *pax = mouse_click[*ibut][*iclick].w.x;
   wp.x = *pax;
   wp.y = mouse_click[*ibut][*iclick].w.y;
   pp = world_to_pixel( wp );
   *pay = phase_ypixel_to_yworld( pp.y );
}

/*-----------------------------------------------------------------*/

void
dclick_( int *ibut, int *num )
{
   *num = mc_num[*ibut];
}

/*-----------------------------------------------------------------*/

void
mclear_( int *ibut )
{
   remove_all_markers( *ibut );
}

/*-----------------------------------------------------------------*/

void
smclear_( int *ibut, int *imarker )
{
   remove_cross_marker( &mouse_click[*ibut][*imarker] );
}

/*-----------------------------------------------------------------*/

void
setunit_( int *iunit )
{
   if ( *iunit == -1 )
     wavenumber_scale = TRUE;
   if ( *iunit == 1 )
     wavenumber_scale = FALSE;
}

/*-----------------------------------------------------------------*/

void
setcol_( char obj[], char col[], ftnlen olen, ftnlen clen )
{
   Arg args[1];
   int n;

   char loc_obj[CMD_LEN];
   char loc_col[CMD_LEN];
   int new_color;

   strncpy( loc_obj, obj, olen );
   strncpy( loc_col, col, clen );
   StrForToC( loc_obj, olen );
   StrForToC( loc_col, clen );

   new_color = display_set_color(loc_plot_widget, loc_col );
   if ( new_color == 0 ) 
     {
	WriteStr( " Error :  unknown color (see rgb.txt file for correct name).\n" );
	return;
     }

   if ( strcmp( loc_obj, "plotter-window" ) == 0 )
     plotter_color = new_color;

   else if ( strcmp( loc_obj, "plotter-frame" ) == 0 )
     {
	frame_color = new_color;
	n = 0;
	XtSetArg( args[0], XtNbackground, frame_color ); n++;
	XtSetValues( loc_plot_widget, args, n );
     }

   else if ( strcmp( loc_obj, "text" ) == 0 )
     text_color = new_color;

   else if ( strcmp( loc_obj, "line-marker" ) == 0 )
     marker_color = new_color;

   else if ( strcmp( loc_obj, "active-marker" ) == 0 )
     active_color = new_color;

   else if ( strcmp( loc_obj, "button-1" ) == 0 )
     button1_color = new_color;

   else if ( strcmp( loc_obj, "button-2" ) == 0 )
     button2_color = new_color;

   else if ( strcmp( loc_obj, "aux-marker" ) == 0 )
     aux_color = new_color;

   else if ( strcmp( loc_obj, "buffer-meter" ) == 0 )
     bar_color = new_color;

   else if ( strcmp( loc_obj, "phase" ) == 0 )
     phase_color = new_color;

   else if ( strcmp( loc_obj, "phase-bad" ) == 0 )
     bad_color = new_color;

   else if ( strcmp( loc_obj, "phase-fit" ) == 0 )
     fit_color = new_color;

   else if ( strcmp( loc_obj, "phase-std" ) == 0 )
     std_color = new_color;

   else if ( strcmp( loc_obj, "real-data" ) == 0 )
     real_color = new_color;

   else if ( strcmp( loc_obj, "imag-data" ) == 0 )
     imag_color = new_color;

   else
     WriteStr( " Error :  unknown object in color command.\n" );
}

/*-----------------------------------------------------------------*/

void
firstcol_()
{
   color_idx = 0;
}

/*-----------------------------------------------------------------*/

void
nextcol_()
{
   cur_color = palette[color_idx].color;
   display_set_color_value( loc_plot_widget, cur_color );
   color_idx++;
   if ( color_idx == num_colors )
     color_idx = 0;
}

/*
 * Fortran interfaces to change colors quickly
 */

/*-----------------------------------------------------------------*/

void
defcol_( int *icol )
{
   switch( *icol )
     {
      case BLACK: 
	cur_color = black_color;
	break;
      case REAL:
	cur_color = real_color;
	break;
      case IMAG:
	cur_color = imag_color;
	break;
      case GREY:
	cur_color = grey_color;
	break;
      case RED:
	cur_color = red_color;
	break;
      case PINK:
	cur_color = pink_color;
	break;
      case PHASE:
	cur_color = phase_color;
	break;
      case FIT:
	cur_color = fit_color;
	break;
      case SDEV:
	cur_color = std_color;
	break;
      case GREEN:
	cur_color = green_color;
	break;
      case BLUE:
	cur_color = blue_color;
	break;
     }
   display_set_color_value( loc_plot_widget, cur_color );
}

/*-----------------------------------------------------------------*/

void
shmode_( char m[], ftnlen mlen )
{
   int mind_x;    /* position of mode indicator */
   int mind_y;
   char tag[12];  /* lower left corner tag for plotting mode */

   if ( m[0] != 'n' )
     {
	mind_x = 5;
	mind_y = display_pix_height - 5;

	switch( m[0] )
	  {
	   case 'r':
	     strcpy( tag, "real" );
	     break;
	   case 'i':
	     strcpy( tag, "imag" );
	     break;
	   case 'c':
	     strcpy( tag, "complex" );
	     break;
	  }
	last_cmd = 
	  display_draw_text(loc_plot_widget,tag,mind_x,mind_y,small_font,black_color);
     }
}

/*-----------------------------------------------------------------*/

void
auxmark_( float *wx )
{
   WorldPoint wp;
   PixelPoint pp;

   wp.x = *wx;
   wp.y = 0.0;
   pp = world_to_pixel( wp );
   display_set_color_value( loc_plot_widget, aux_color );
   last_cmd = display_draw_line_simple( loc_plot_widget,  
					pp.x, plotter_pix_top+1, 
					pp.x, plotter_pix_bottom );
   display_set_color_value( loc_plot_widget, cur_color );
}

/*-----------------------------------------------------------------*/

void
drawlbl_( float *x, float *y, float *xlbl, int *unit )
{
   PixelPoint pp;
   WorldPoint wp;
   int llen;
   int lheight;
   int ptop;
   int pbot;
   char numstr[32];
   
   wp.x = *x;
   wp.y = *y;
   pp = world_to_pixel( wp );
   
   /*
    * plot the label
    */
   lheight = label_small_pix_height - 2;   /* true height of characters */
   display_set_color_value( loc_plot_widget, black_color );
   cur_color = black_color;

   if ( *unit == PRIMARY )
     sprintf( numstr, "%.8g", *xlbl );

   if ( *unit == SECONDARY )
     sprintf( numstr, "%.8g%s", 
	      (*kayser_to_secondary[secondary_unit])( *xlbl ),
              unit_name[secondary_unit] );

   llen = display_font_width( loc_plot_widget, small_font, numstr );
   display_draw_text( loc_plot_widget, numstr,
		      pp.x - llen/2, pp.y + lheight/2, 
		      small_font, text_color );

   /*
    * draw small lines indicating exact position
    */
   pbot = pp.y + lheight/2 + 1;
   ptop = pp.y - lheight/2;
   last_cmd = display_draw_line_simple( loc_plot_widget,
                                        pp.x, pbot,
				        pp.x, pbot + LBL_LINE_LEN );
   last_cmd = display_draw_line_simple( loc_plot_widget,
                                        pp.x, ptop, 
				        pp.x, ptop - LBL_LINE_LEN );
}

/*-----------------------------------------------------------------*/

void
phaseplt_( int *istat )
{
   Arg args[3];
   int i;

   i = 0;
   if ( *istat == 0 )
     {
	phase_mode = FALSE;
	XtSetArg(args[i], XtNsensitive, False); i++;
	XtSetArg(args[i], XtNstate, False);     i++;
     }
   if ( *istat == 1 )
     {
	phase_mode = TRUE;
	XtSetArg(args[i], XtNsensitive, True); i++;
     }
   XtSetValues( phaseButton, args, i );
}

/*-----------------------------------------------------------------*/

void
nxtunit_()
{
   secondary_unit++;
   secondary_unit %= GHZ+1;
}

/*-----------------------------------------------------------------*/

/*-----------------------------------------------------------
 * line list related stuff
 *-----------------------------------------------------------*/

/*
 * Note:
 * This module keeps a list of all marked lines independently of the
 * Gremlin linelist code because this allows to have persistent line
 * markers.
 */

void
insmarker_(int *vis, double *wnum, int *npoint, float *amp, float *wid, 
	   char *id, ftnlen len)
{
   LineMarker line_marker;             

   /* 
    * this function does 2 things:
    * 1) it displays a new line on the plot and
    * 2) it adds the line to the line list
    */

   /*
    * assemble line marker structure
    * (NOTE: the pixel position of the line is calculated and stored
    *  whenever the line is plotted.)
    */
   markers_visible = TRUE;                /* we want to see it later */
   line_marker.point       = *npoint;
   line_marker.wavnum      = *wnum;
   line_marker.intens      = *amp;
   line_marker.width       = *wid;
   line_marker.active      = FALSE;
   line_marker.color       = aux_color;   /* use a different colour for fresh line */
   line_marker.id_str      = id;
   line_marker.id_len      = len;
   line_marker.dcmd        = NULL;
   line_marker.lcmd        = NULL;

   /*
    * draw the new line
    */
   if ( *vis == 1)
     show_line( &line_marker );

   /*
    * and insert it in the line list
    */
   line_marker.color = marker_color;      /* give it the usual colour now */
   sorted_list_insert_object(line_list, &line_marker, sizeof(line_marker), 
			      SORT_PROC( line_comp ));
}

/*-----------------------------------------------------------------*/

void
updone_(int *num, double *point, float *wnum, float *amp, float *wid)
{
  LineMarker *lm;

  /*
   * find the line number 'num'
   */
  line_number_mbx = *num;
  lm = (LineMarker *)search_list(line_list, SEARCH_PROC(search_lnum));

  /*
   * and update
   */
  lm->point  = (int)(*point+0.5);
  lm->wavnum = (double)*wnum;
  lm->intens = *amp;
  lm->width  = *wid;

  /*
   * NOTE: the id string is already updated in the FORTRAN part
   */
}

/*-----------------------------------------------------------------*/

void
updlin_( double *point, float *wnum, float *amp, float *wid )
{
   int i;
   LineMarker *lm;

   list_head(line_list);
   for ( i=0; i<list_entries(line_list); i++ )
     {
        get_current_entry(line_list, (void **)&lm);
        lm->point  = (int)(point[i] + 0.5);
        lm->wavnum = (double)wnum[i];
        lm->intens = amp[i];
        lm->width  = wid[i];
     }
}

/*-----------------------------------------------------------------*/
 
void
shwmarker_( float *wnum )
{
   LineMarker line_marker;             

   /*
    * assemble line marker structure
    */
   line_marker.point       = 0;
   line_marker.wavnum      = *wnum;
   line_marker.active      = FALSE;
   line_marker.color       = grey_color;   /* this is a zombie line, mark it grey */
   line_marker.id_len      = 0;
   line_marker.dcmd        = NULL;
   line_marker.lcmd        = NULL;

   /*
    * draw the new line
    */
   show_line( &line_marker );
}

/*-----------------------------------------------------------------*/

void
dlabels_()
{
   line_number_mbx = 0;
   scan_list( line_list, SCAN_PROC( renumber_lines ) );

   clear_labels();
   last_numstr_mbx = 0;
   scan_list( line_list, SCAN_PROC( show_label ));
}

/*-----------------------------------------------------------------*/

void
marklines_()
{
   /*
    * this function marks all lines on the screen
    */
   if ( markers_visible == TRUE && list_entries( line_list ) > 0 )
     {
	clear_labels();
	last_numstr_mbx = 0;
	scan_list( line_list, SCAN_PROC( show_line ) );
	scan_list( line_list, SCAN_PROC( show_label) );
     }
}

/*-----------------------------------------------------------------*/

void
remline_( int *ipoint, int *nbin, int *nline )
{
   LineMarker *found_line;

   line_number_mbx = *ipoint;
   nbin_mbx = *nbin;
   found_line = (LineMarker *)
                search_list( line_list, SEARCH_PROC( fuzzy_search_linenumber ) );
   if ( found_line != NULL )
     {
	*nline = found_line->number;
	list_prev( line_list );    /* because search_list increments list pointer */
	remove_current_entry( line_list );
	line_number_mbx = 0;       /* renumber all lines */
	scan_list( line_list, SCAN_PROC( renumber_lines ) );
     }
   else
     {
	*nline = 0;
	return;
     }
}

/*-----------------------------------------------------------------*/

void
linevis_( int *iflag )
{
   if ( *iflag == 0 || *iflag == 1 )
     markers_visible = *iflag;
}

/*-----------------------------------------------------------------*/

void
lineact_( int *nline, int *active )
{
   line_number_mbx = *nline;
   active_mbx = *active;
   search_list( line_list, SEARCH_PROC( line_active ) );
}

/*-----------------------------------------------------------------*/

void
numline_( int *ipoint, int *nbin, int *nline )
{
   LineMarker *found_line;

   line_number_mbx = *ipoint;
   nbin_mbx = *nbin;
   found_line = (LineMarker *)
                search_list( line_list, SEARCH_PROC( fuzzy_search_linenumber ) );
   if ( found_line != NULL )
     *nline = found_line->number;
   else
     *nline = 0;
}

/*-----------------------------------------------------------------*/

void
renumlin_()
{
   line_number_mbx = 0;
   scan_list( line_list, SCAN_PROC( renumber_lines ) );
}

/*-----------------------------------------------------------------*/

void
allactive_( int *iact )
{
   scan_list( line_list, 
              *iact == 1 ? SCAN_PROC(activate_line) 
                         : SCAN_PROC(deactivate_line) );
}

/*-----------------------------------------------------------------*/

void
lineinfo_( char s[], ftnlen len )
{
   StrForToC( s, (int)len );
   line_info( s, black_color, TRUE );
}

/*-----------------------------------------------------------------*/

void
linecnt_()
{   
   int c_x;      /* position of counter */
   int c_y;
   char tag[8];  /* lower left corner tag */

   c_x = 5;
   c_y = display_pix_height - 5;

   sprintf( tag, "%d", list_entries(line_list) );
   set_objects_printable( FALSE );         /* don't include in PS dumps */
   last_cmd = 
     display_fill_rectangle(loc_plot_widget, c_x, c_y - label_small_pix_height,
			    6*cslen, label_small_pix_height+2, frame_color);
   last_cmd = 
     display_draw_text(loc_plot_widget,tag,c_x,c_y,small_font,black_color);
   set_objects_printable( TRUE );     
}

/*-----------------------------------------------------------------*/

void
llerase_()
{
   erase_list_entries( line_list );
}

/*-----------------------------------------------------------------*/

static int 
line_comp( LineMarker *line1, LineMarker *line2 )
{
   if ( line1->wavnum > line2->wavnum )
     return 1;
   if ( line1->wavnum < line2->wavnum )
     return -1;
   return 0;
}

/*-----------------------------------------------------------------*/

static void
renumber_lines( LineMarker *line )
{
   line_number_mbx++;
   line->number = line_number_mbx;
}

/*-----------------------------------------------------------------*/

static void
show_line( LineMarker *line )
{
   WorldPoint wp;
   PixelPoint pp;
   int lpos;

   if ( markers_visible == TRUE )  /* otherwise do nothing at all */
     {
	if ( (float)line->wavnum > world.l && (float)line->wavnum < world.r )
	  {
	     wp.x = (float)line->wavnum;
	     wp.y = 0.0;
	     pp = world_to_pixel( wp );
	     display_set_color_value( loc_plot_widget, line->color );

	     /*
	      * draw line and save the display command used for drawing it
	      * in the line marker structure
	      */
	     last_cmd = line->dcmd = 
	       display_draw_line_simple( loc_plot_widget,  
					 pp.x, plotter_pix_top+1, 
					 pp.x, plotter_pix_bottom );
	     display_set_color_value( loc_plot_widget, cur_color );
	     lpos = pp.x - plotter_pix_left;
	     pixel_flags[lpos] = 'L';  /* must be != 0 */
	     line->pixpos = lpos;      /* store pixel position in marker record */
	  }
	else
	  {
	     line->pixpos = -1;        /* remove any old line positions */
	  }
     }
}

/*-----------------------------------------------------------------*/

static void
show_label( LineMarker *line )
{
   char numstr[16];
   int numstr_len;
   int xbeg;        /* where the string starts */
   WorldPoint wp;
   PixelPoint pp;

   /*
    * displays the line number atop the plotter window
    * markers do not overlap. If a marker overlaps with the one previously 
    * drawn it will be skipped.
    * it is assumed that all labels are re-drawn.
    */
   if ( markers_visible == TRUE ) 
     {
	if ( (float)line->wavnum > world.l && (float)line->wavnum < world.r )
	  {
	     sprintf( numstr, "%d", line->number );
	     numstr_len = display_font_width( loc_plot_widget, small_font, numstr );
	     wp.x = (float)line->wavnum;
	     wp.y = 0.0;
	     pp = world_to_pixel( wp );
	     xbeg = pp.x - numstr_len / 2;
	     if ( xbeg > last_numstr_mbx )         
	       {
		  last_cmd = line->lcmd = 
		    display_draw_text( loc_plot_widget, numstr,
				      xbeg, plotter_pix_top - LABEL_SPACE,
				      small_font, text_color );
		  last_numstr_mbx = xbeg + numstr_len + cslen;
	       }
	  }
     }
}

/*-----------------------------------------------------------------*/

static void
activate_line( LineMarker *line )
{
   line->active = TRUE;
   line->color  = active_color;
}

/*-----------------------------------------------------------------*/

static void
deactivate_line( LineMarker *line )
{
   line->active = FALSE;
   line->color  = marker_color;
}

/*-----------------------------------------------------------------*/

static int
fuzzy_search_linenumber( LineMarker *line )
{
   int half_range;
   float ratio;

   ratio = (float)(pt_range.end - pt_range.begin) / (float)plotter_pix_width;
   if ( ratio < POINT_PIXEL_RATIO_LIMIT )  /* few points --> require better aim */
     half_range = 1;
   else
     half_range = ( 1 + MOUSE_TOTTER_ALLOWANCE ) * nbin_mbx;

   if ( in_interval(line->point-half_range, line->point+half_range, line_number_mbx) )
     return 1;
   else
     return 0;
}

/*-----------------------------------------------------------------*/

static int 
search_line_pixel( LineMarker *line )
{
   if ( line->pixpos == line_number_mbx )
     return 1;
   else
     return 0;
}

/*-----------------------------------------------------------------*/

static int
search_lnum( LineMarker *line )
{
  if ( line->number == line_number_mbx )
    return 1;
  else
    return 0;
}

/*-----------------------------------------------------------------*/

static int
line_active( LineMarker *line )
{
   if ( line->number == line_number_mbx )
     {
	if ( active_mbx == TRUE )
	  activate_line( line );
	else
	  deactivate_line( line );
	return 1;
     }
   else
     return 0;
}

/*-----------------------------------------------------------------*/

/*
 * clears all labels
 */
static void
clear_labels()
{
   int xpos;
   int xwid;

   xpos = plotter_pix_left - clen;
   xwid = display_pix_width - xpos - 1;
   set_objects_printable( FALSE );         /* don't include in PS dumps */
   last_cmd =
     display_fill_rectangle(loc_plot_widget, 
			    xpos, plotter_pix_top -label_small_pix_height -LABEL_SPACE+1,
			    xwid, label_small_pix_height, 
			    frame_color );
   set_objects_printable( TRUE );
}

/*-----------------------------------------------------------------*/

/* ---------------------------------------------------------------
 * Functions for the plot mode undo stack
 * ---------------------------------------------------------------
 */

void
clrfrm_()
{
  erase_list_entries(undo_stack);
}

/*-----------------------------------------------------------------*/

void
stofrm_(int *nleft, int *nright, float *wtop, float *wbottom)
{
  PlotFrame p;

  p.left = *nleft;
  p.right = *nright;
  p.top = *wtop;
  p.bottom = *wbottom;

  if ( list_entries(undo_stack) == PLOT_UNDO_FRAMES )
    chop_list_tail(undo_stack);

  list_head(undo_stack);
  list_insert_object(undo_stack, &p, sizeof(PlotFrame), BEFORE);
}

/*-----------------------------------------------------------------*/

void
recfrm_(int *nleft, int *nright, float *wtop, float *wbottom, int *ndone)
{
  PlotFrame p;

  if ( list_entries(undo_stack) < 2 )
    {
      *ndone = 0;
      return;
    }

  /*
   * first discard the most recently stored frame because it is the
   * frame we are currently looking at.
   */
  list_head(undo_stack);
  remove_current_entry(undo_stack);

  /*
   * then retrieve the next to last frame
   */
  list_head(undo_stack);
  get_current_object(undo_stack, &p, sizeof(PlotFrame));

  /*
   * and remove it from the stack because it will 
   * otherwise be stored twice once the plot is displayed
   */
  list_head(undo_stack);
  remove_current_entry(undo_stack);

  *nleft = p.left;
  *nright = p.right;
  *wtop = p.top;
  *wbottom = p.bottom;
  *ndone = 1;
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * -----------------
 * $Log: PlotMode.c,v $
 * Revision 1.69  1996/09/15 03:22:31  ulf
 * better handling of key strokes and special keys.
 *
 * Revision 1.68  1996/09/07 04:27:43  ulf
 * better formatting of x axis labels.
 *
 * Revision 1.67  1996/08/23 03:51:55  ulf
 * added plotting of reversed x-axes with descending labels.
 *
 * Revision 1.66  1996/08/22 03:55:57  ulf
 * make red, not blue the first overplotting color because blue stands
 * for 'complex data' in Xgremlin.
 *
 * Revision 1.65  1996/07/09 02:56:59  ulf
 * new function 'updone_' to update line data for a single line marker
 *
 * Revision 1.64  1996/07/09 01:17:47  ulf
 * new parameter 'vis' in function 'insmarker'
 *
 * Revision 1.63  1996/06/15 20:00:26  ulf
 * add the functions for the plot mode undo functionality
 *
 * Revision 1.62  1996/03/23 11:25:58  ulf
 * fixed function 'updlin'
 *
 * Revision 1.61  1996/03/23  10:16:11  ulf
 * add function 'sabsref' for setting absolute reference point
 *
 * Revision 1.60  1996/03/18  15:49:52  ulf
 * remove the file name from the plotting widget after printing
 *
 * Revision 1.59  1996/03/13  16:11:03  ulf
 * added a cast to 2nd argument of get_current_entry to avoid warning
 *
 * Revision 1.58  1996/03/13  15:56:25  ulf
 * new function 'updlin'
 *
 * Revision 1.57  1996/03/12  17:20:20  ulf
 * made variable 'info' in function MotionHandler a normal local variable and
 * made it longer.
 *
 * Revision 1.56  1996/03/03  13:55:44  ulf
 * replaced calls to 'gdispatch' by calls to new wrapper function 'keycmd'
 *
 * Revision 1.55  1996/02/14  14:04:42  ulf
 * just a few cosmetic modifications
 *
 * Revision 1.54  1996/02/05  16:39:32  ulf
 * better positioning of file name on top of plot during screen dump and better
 * positioning of vertical axis labels.
 *
 * Revision 1.53  1996/02/03  07:12:40  ulf
 * parameters of 'in_interval' were in wrong order in 'MotionHandler'. Fixed.
 *
 * Revision 1.52  1996/02/02  18:12:52  ulf
 * fixed a stupid blunder: did not keep the return value of 'realloc'.
 *
 * Revision 1.51  1996/02/01  15:38:44  ulf
 * Erase the file name in 'screen_dump' from the widget display list.
 * Do not update line info if the cursor has left the plotter pane at top or bottom
 *
 * Revision 1.50  1996/01/29  17:04:24  ulf
 * write out the name of the file connected to 'datain' at the top of
 * the plot to make it easier to identify the plot.
 *
 * Revision 1.49  1996/01/29  15:46:52  ulf
 * fixed the format for the phase axis labels  +.2g --> +.1f
 *
 * Revision 1.48  1996/01/17  14:44:01  ulf
 * changed type of wavnumber position of a line to double
 *
 * Revision 1.47  1996/01/12  09:30:58  ulf
 * added function 'pgpbin' for binned plotting of continuous
 * phase functions
 *
 * Revision 1.46  1996/01/08  09:29:17  ulf
 * plot out line width when moving mouse cursor on a line marker
 *
 * Revision 1.45  1995/11/29  16:00:28  ulf
 * the old line positions were not removed from the plotter line list even of
 * lines which were not currently marked on the screen which resulted in
 * displaying the ids of wrong lines. fixed.
 *
 * Revision 1.44  1995/11/26  13:31:07  ulf
 * make 'Phase' button pop out again if phase plotting is turned off.
 *
 * Revision 1.43  1995/11/23  09:41:44  ulf
 * re-arranged conditional statement in motion handler to make it faster
 * in average.
 *
 * Revision 1.42  1995/11/22  19:01:50  ulf
 * added function to plot out number of lines in the internal line list.
 * modified motion event handler to print out line information whenever
 * the graphics cursor is moved on top of a line.
 *
 * Revision 1.41  1995/11/16  04:05:13  ulf
 * added new function 'linecnt' to display the line count
 * (number of lines in the internal buffer and the plotter list )
 *
 * Revision 1.40  1995/11/12  14:31:29  ulf
 * make sure that the returned display command from the last display_ .. function
 * is saved in variable 'last_cmd' so 'inhibit' and 'update' can work
 *
 * Revision 1.39  1995/11/11  10:40:43  ulf
 * add plot orientation in function 'screen_dump'
 *
 * Revision 1.38  1995/11/11  10:04:02  ulf
 * fixed the 'display_draw_from' function of the Display widget and therefore
 * the function 'update_'. This causes fewer complete redraws during plotting
 * and a general speedup.
 *
 * Revision 1.37  1995/11/09  17:25:07  ulf
 * make large tic marks on the x axis stick out below the axis line by a few pixels
 * because it becomes difficult to discern the tic marks in a plot with many densely
 * packed lines.
 *
 * Revision 1.36  1995/10/17  13:11:07  ulf
 * removed a Newfoundland - dog - sized bug from function 'remlines_'.
 * The lines were not renumbered from 1 after a line was removed from
 * the plotter line list.
 *
 * Revision 1.35  1995/10/17  11:51:10  ulf
 * fixed removing mouse markers with the right mouse button. No extra click is required
 * to switch from blue to red markers.
 *
 * Revision 1.34  1995/10/14  15:29:22  ulf
 * new colour (blue) added to function 'defcol_'
 *
 * Revision 1.33  1995/10/14  15:24:32  ulf
 * new colour in function 'defcol_'
 *
 * Revision 1.32  1995/10/11  13:53:44  ulf
 * cosmetic fix
 *
 * Revision 1.31  1995/10/11  13:51:48  ulf
 * the 'recm_' function to recall markers now places the markers in world
 * coordinates because the pixel coordinates may have changed.
 *
 * Revision 1.30  1995/10/10  15:11:33  ulf
 * new colours 'real_color' and 'imag_color' which can be modified via
 * the 'color' command.
 *
 * Revision 1.29  1995/10/06  02:35:36  ulf
 * fixed the bug that would not allow the colour of the plotter frame to be changed
 * with the 'color' command ( in function 'setcol_' ).
 *
 * Revision 1.28  1995/10/03  13:22:22  ulf
 * added function 'shwmarker_' to mark a moved line
 *
 * Revision 1.27  1995/09/26  19:46:59  ulf
 * finally got the line label distances right. Honest ....
 *
 * Revision 1.26  1995/09/25  23:17:31  ulf
 * distances between labelled marked lines were too large
 *
 * Revision 1.25  1995/09/23  16:23:54  ulf
 * add some basic support for monochrome displays
 *
 * Revision 1.24  1995/09/23  14:46:35  ulf
 * prevent clipping of mode indicator by line info in function 'line_info'
 *
 * Revision 1.23  1995/09/20  18:02:58  ulf
 * At long last: fixed problem of overlapping line labels.
 *
 * Revision 1.22  1995/09/18  17:18:20  ulf
 * add 2nd parameter to function 'lineact_'
 *
 * Revision 1.21  1995/09/17  01:38:44  ulf
 * make sure range display in upper right corner of plotting window
 * does not overlap with line numbers any more.
 *
 * Revision 1.20  1995/09/13  20:48:12  ulf
 * use mode indicators  real / imag / complex  instead of  R / I / C
 *
 * Revision 1.19  1995/09/07  20:37:34  ulf
 * no space in labels between number and unit
 *
 * Revision 1.18  1995/08/24  20:07:41  ulf
 * replaced all set<color> functions by 'defcol' for setting the
 * default drawing color
 *
 * Revision 1.17  1995/08/24  01:06:58  ulf
 * remove display commands for drawing cross markers from the display
 * list in function 'remove_cross_marker' when removing a marker
 *
 * Revision 1.16  1995/08/23  02:04:11  ulf
 * fixed buglet in 'xaxis' where the first large tic mark was plotted small.
 *
 * Revision 1.15  1995/08/21  23:57:52  ulf
 * re-set drawing color after drawing a cross marker
 *
 * Revision 1.14  1995/08/19  19:24:25  ulf
 * added all functions required for phase plotting
 *
 * Revision 1.13  1995/08/06  20:40:46  ulf
 * fix some bugs in secondary unit plotting
 *
 * Revision 1.12  1995/08/06  20:34:41  ulf
 * add code for line labelling, full support for all secondary units
 * start adding phase plotting and phase correction
 *
 * Revision 1.11  1995/07/19  13:32:13  ulf
 * fixed yet another goof in line clipping: horizontal lines completely outside
 * the plotter frame are now caught. Before the calculation of 'cot_a'
 * would cause a SIGFPE.
 *
 * Revision 1.10  1995/07/19  01:01:55  ulf
 * add clipping to function 'gpbin_' and fix function 'clip_point' to
 * clip correctly when both end points of a line are outside the plotter frame.
 *
 * Revision 1.9  1995/07/12  20:22:02  ulf
 * changed the calculation of the number of x labels so that (on average) the
 * density of x labels is somewhat higher than it was ( num_labels is rounded to
 * the *nearest* integer now).
 *
 * Revision 1.8  1995/07/04  04:15:35  ulf
 * remove calls to double buffering functions because they were eliminated from
 * the Display widget to cut down memory consumption.
 *
 * Revision 1.7  1995/07/04  03:59:09  ulf
 * hard code the initial size of the Display widget. This is a procedural workaround
 * to make the program run on Sun Sparcstations with X11R6. The function call to
 * XtVaGetValues returned garbage.
 *
 * Revision 1.6  1995/07/01  17:36:31  ulf
 * make setcolor_ safe to call from Fortran.
 *
 * Revision 1.5  1995/06/28  20:47:51  ulf
 * some minor cosmetic changes to improve readability
 *
 * Revision 1.4  1995/06/19  02:59:27  ulf
 * make colour of the buffer-meter configurable (and the color of the
 * markers of mouse button2 ).
 *
 * Revision 1.3  1995/06/18  01:46:21  ulf
 * new colour option, mainly bug fixes
 *
 * Revision 1.2  1995/06/16  04:19:16  ulf
 * change  search_linenumber --> fuzzy_search_linenumber
 * the new function is less strict when matching a line marker and
 * a cursor marker making it easier for users to toggle and delete lines
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */


