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
 * Variable default settings
 */

#ifndef _defaults_h
#define _defaults_h


/*
 * physical constants
 * E R Cohen and B N Taylor 1973, J. Phys. Chem. Ref. Data 2, 663
 */
#define C_VAC 2.99792458e10  /* cm/sec        : speed of light in vacuum */
#define H     6.626176e-34   /* Joules/Hz     : Planck's constant */
#define K     1.380662e-23   /* Joules/Kelvin : Boltzmann's constant */
#define E     1.6021892e-19  /* Coulombs      : charge of electron */
#define C_1   3.741832e-16   /* w m**2        : (2 pi h c**2) */
#define C_2   1.438786       /* cm Kelvin     : (hc/k) */


/*
 * if we have to use an inferior C compiler we must get rid of 'inline'
 */
#ifdef __GNUC__
#define INLINE  __inline__           /* retain 'inline' keyword */
#else
#define INLINE                       /* replace INLINE by nothing */
#endif


/*
 * the size of the plotter window in the display widget
 */
#define DISPLAY_PIX_WIDTH  700       /* startup size of plotter pane in pixels */
#define DISPLAY_PIX_HEIGHT 320
#define PLOTTER_WIDTH  85.0          /* in % of the display widget height in pixels */ 
#define X_SHIFT        2.0           /* shift plotter area to the right */


/*
 * colors for drawing
 */
#define PLOTTER_COLOR    "white"           /* color of the plotter */
#define FRAME_COLOR      "thistle"         /* frame around plotter */
#define TEXT_COLOR       "black"           /* for text and such (default) */
#define MARKER_COLOR     "goldenrod1"      /* for line markers */
#define ACTIVE_COLOR     "PaleVioletRed2"  /* active lines */
#define BLUE_COLOR       "blue3"
#define RED_COLOR        "red3"
#define PINK_COLOR       "pink1"
#define GREY_COLOR       "LightGrey"
#define GREEN_COLOR      "green3"
#define BAR_COLOR        "goldenrod1"      /* for the bar meter */
#define AUX_MARKER_COLOR "green3"          /* for auxiliary lines */
#define PHASE_COLOR      "red3"            /* for phase plots */
#define BAD_COLOR        "bisque2"         /* for bad phase points */
#define PHASE_FIT_COLOR  "blue3"           /* for phase polynomial */
#define PHASE_STD_COLOR  "SkyBlue1"        /* for phase confidence interval */   

/*
 * length of the cross marker lines in pixels
 */
#define X_MARKER_LEN  13
#define Y_MARKER_LEN  20
#define C_MARKER_LEN  32    /* center marker */


/*
 * horizontal locations of text in the plotter widget (in % of widget width)
 */
#define METER_END   30.0    /* extent of the buffer-meter */
#define PIXEL_TXT   35.0    /* position of binning information */
#define MOUSE_TXT   55.0    /* location of mouse position information */


/*
 * names of the X fonts to be used in plotter widget
 */
#define SMALL_PLOTTER_FONT  "6x13"
#define LARGE_PLOTTER_FONT  "9x15"


/*
 * number of plot frames to be stored for plot mode undo function
 */
#define PLOT_UNDO_FRAMES   16


/*
 * number of mouse clicks that can be stored and a few mouse related constants
 */
#define MOUSE_CLICKS  32
#define BUTTON_1      0
#define BUTTON_2      1
#define BUTTON_3      2
#define NUM_BUTTONS   3    /* number of mouse buttons */


/*
 * The following number is used when removing marked lines from an internal
 * line list. Usually a line is marked by moving the X cursor on it and by
 * dropping a marker on it. The 'MOUSE_TOTTER_ALLOWANCE' defines by how many
 * pixels the line may be missed and still be considered as marked.
 */
#define MOUSE_TOTTER_ALLOWANCE  2      /* must be >= 0 */


/*
 * defines at which point to pixel ratio the mouse totter allowance is 
 * automatically reduced to 0. This is necessary to be able to mark, delete etc. 
 * that are very closely spaced as it may happen if e.g. 'findlines' picks
 * up a noise spike.
 */
#define POINT_PIXEL_RATIO_LIMIT  0.3


/*
 * length of tic marks used in the plotter widget in % of plotter size
 */
#define LARGE_X_TIC_MARK  3.0
#define SMALL_X_TIC_MARK  1.4
#define LARGE_Y_TIC_MARK  1.5
#define SMALL_Y_TIC_MARK  0.8


/*
 * the default size of the PostScript hardcopy
 */
#define HARDCOPY_WIDTH   8.5  /* inches */
#define HARDCOPY_HEIGHT  6.0


/* 
 * startup width and height of the help window 
 */
#define HELP_WINDOW_WIDTH  600
#define HELP_WINDOW_HEIGHT 500


/*
 * startup size of the phase correction window
 */
#define PHASE_WINDOW_WIDTH  400
#define PHASE_WINDOW_HEIGHT 345


/*
 * initial size of the configuration file editor window
 */
#define CONFIG_WINDOW_WIDTH  400
#define CONFIG_WINDOW_HEIGHT 320


/* 
 * default path to the HTML help files and the default browser
 */
#define HELP_DEF_PATH               "/usr/local/xgremlin/html"
#define INITIAL_HTML_FILE           "xgremlin.html"
#define HELP_DEF_BROWSER            "firefox"


/* 
 * environment variable with the path to the help file 
 * (overrides HELP_DEF_PATH)
 */
#define HELP_ENV_PATH  "XGREMLIN_HELP_PATH"


/*
 * command/environment variable to be used for printing a Postscript file
 */
#define PRINT_ENV_COMMAND     "XGREMLIN_PRINT_COMMAND"
#define DEFAULT_PRINT_COMMAND "lpr -P"
#define DEFAULT_ORIENTATION   1    /* 1 = landscape, 0 = portrait */


/*
 * max permitted length of a file name (most Unices will do that)
 */
#define FILE_NAME_LEN  256  


/*
 * scratch directory and directory file name (default)
 * (this will be appended to the home directory)
 */
#define DEFAULT_SCRATCH_DIRECTORY  "/xgremlin-scratch"
#define SCRATCH_DIRECTORY_FILE     "/scratch.dir"
#define MAX_NUM_SCRATCH_FILES      100


/*
 * max length of a command on the command line
 */
#define CMD_LEN  512


/*
 * the initial dimensions of the work arrays  (in data points)
 */
#define INITIAL_R_SIZE          131074    /* 2^17 + 2 */
#define INITIAL_FFTA_SIZE       262144
#define INITIAL_LINELIST_SIZE   100


/*
 * sizes of tag and id strings in Xgremlin
 */
#define CTAG_LEN  4
#define DENT_LEN  32


/*
 * some useful constants
 */
#define ON   1
#define OFF  0


#define TRUE  1
#define FALSE 0

/*
 * portable key codes for some function keys and special keys
 */
#define FKBASE      128
#define LOC_F1      FKBASE+1
#define LOC_F2      FKBASE+2
#define LOC_F3      FKBASE+3
#define LOC_F4      FKBASE+4
#define LOC_F5      FKBASE+5
#define LOC_F6      FKBASE+6
#define LOC_F7      FKBASE+7
#define LOC_F8      FKBASE+8
#define LOC_F9      FKBASE+9
#define LOC_F10     FKBASE+10
#define LOC_F11     FKBASE+11
#define LOC_F12     FKBASE+12

#define SKBASE      200        /* now this is truly horrible ... */
#define LOC_UP      SKBASE+1
#define LOC_DOWN    SKBASE+2
#define LOC_LEFT    SKBASE+3
#define LOC_RIGHT   SKBASE+4
#define LOC_HOME    SKBASE+5
#define LOC_END     SKBASE+6
#define LOC_PGUP    SKBASE+7
#define LOC_PGDOWN  SKBASE+8

#endif /* _defaults_h */
