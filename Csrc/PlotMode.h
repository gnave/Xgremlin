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
 * $Id: PlotMode.h,v 1.28 1996/07/09 02:50:40 ulf Exp $
 */

#ifndef _PlotMode_h
#define _PlotMode_h

#include "F77_types.h"

/*
 * default drawing colours
 */
enum default_colours { BLACK=1, REAL, IMAG, GREY, RED, PINK, PHASE, 
                       FIT, SDEV, GREEN, BLUE };
/*
 * BLACK = 1
 * REAL  = 2
 * IMAG  = 3
 * GREY  = 4
 * RED   = 5
 * PINK  = 6
 * PHASE = 7
 * FIT   = 8
 * SDEV  = 9
 * GREEN = 10
 * BLUE  = 11
 */


/*
 * initializes the screen plotting module
 */
void 
InitializePlotMode( Widget plotW );


/*
 * sets up the drawing area
 */
void InitializePlotter();


/*
 * clear the plotter and redraw plotting area
 */
void
NewPlotter();


/*
 * give the keyboard focus to the plotter widget
 */
void FocusToPlot();


/*
 * callback function for the Phase button, toggles data-zoom and phase-zoom
 * modes
 */
void
PhaseZoom( Widget w , XtPointer phase_button_widget, XtPointer call_data );


/*
 * turn off phase plotting/zooming
 */
void
PhaseOff();
 

/*-----------------------------------------------
 * additional functions to speed up drawing
 *-----------------------------------------------*/

/*
 * refer actual plotting until the actual update command is given
 */
void
inhibit_();


/*
 * update graph since the last inhibit command
 */
void
update_();


/*
 * start buffering polygon data, don't plot yet
 */
void
polybeg_();


/*
 * add another point to the polygon buffer
 */
void
polyadd_( int *x, int *y );


/*
 * plot the polygon from buffered data
 */
void
polyend_();


/*-----------------------------------------------
 * Miscellanea
 *-----------------------------------------------*/

/*
 * Function to transfer sampling parameters (alias, fsr, scan velocity)
 * to the plotting part of the program. This is needed to calculate
 * the acoustic frequency corresponding to a wavenumber.
 */
void
ssmpar_(int *alias, double *fsr, double *sigma_l, float *velocity);


/*
 * write a postscript file of the current plot. The size_string
 * is the size of the picture in the format 'WxH' as a character
 * array where W is the width and H the height (e.g. 12x7).
 * if  landscape == 1  the plot will be in landscape mode.
 */
void
screen_dump( char filename[], char size_string[], int landscape );


/*
 * store currently set markers
 * the argument to the function is the number of the mouse button
 * 0: red markers;  1: blue markers
 */
void
stom_( int *ib );


/*
 * recall and plot previously stored markers
 * 0: red markers;  1: blue markers
 */
void
recm_( int *ib );


/*
 * set the fonts for the plotter window
 */
void
plotfont_(int *font_idx, char *font_string, ftnlen len);
 
/*----------------------------------------------------
 * Fortran callable graphics functions
 *----------------------------------------------------*/

/*
 * return the width of the plotter window in pixels
 */
void
getpwid_( int *iwid );


/*
 * return the height of the plotter window in pixels
 */
void
getphei_( int *ihei );


/*
 * clear the whole plotting area
 */
void
clrplot_();
 

/*
 * set the world coordinate system for plotting
 */
void
svcsys_(float *left,float *right,float *bottom,float *top,float *g_min,float *g_max);

/*
 * store the absolute data file reference point
 */
void
sabsref_(int *npref);


/*
 * set the world coordinate system for phase plotting
 */
void
svpsys_( float *bot, float *top );


/*
 * move the graphics cursor to a point in world coordinates
 */
void
gvmove_( float *x, float *y );


/*
 * draw a line from current position to a new position in world coordinates
 */
void
gvline_( float *xnew, float *ynew );


/*
 * move the graphics cursor to a point in PHASE world coordinates
 */
void
pgvmove_( float *x, float *y );


/*
 * draw a line from current position to a new position in PHASE world coordinates
 */
void
pgvline_( float *xnew, float *ynew );


/*
 * draw a vector from point a to point b with clipping to the frame
 * in PHASE coorinates
 */
void
pgvcvect_( float *xa, float *ya, float *xb, float *yb);


/*
 * draw a phase point in the phase coordinate system
 * ipgood = 0 : bad phase point; ipgood = 1 : good phase point
 * w  : wave number
 * ph : phase value
 */
void
drwphp_( int *igood, float *w, float *ph );


/*
 * draw a vector from (xa,ya) --> (xb,yb) with clipping to the plotter window
 */
void
gvcvect_( float *xa, float *ya, float *xb, float *yb);


/*
 * hybrid version of gvmove 
 * where the first coordinate is in pixels the second in world units
 */
void
gpmove_( int *ix, float *y );


/*
 * hybrid version of gvline
 * where the first coordinate is in pixels the second in world units
 */
void
gpline_( int *ixnew, float *ynew );
 

/*
 * plots a vertical line ( for binning mode )
 */
void
gpbin_( int *ibin, float *a, float *b );


/*
 * plots a vertical line in the phase plot ( for binning mode )
 */
void
pgpbin_( int *ibin, float *a, float *b );


/*
 * set the drawing color for lines and such
 */
void
setcolor_( char color_name[], ftnlen len );


/*
 * turn on / off the dashed line style for line drawing commands
 */
void
dashsty_( int *flag );


/*
 * plot a x axis
 */
void
xaxis_( float *left, float *right );


/*
 * plot a y axis
 */
void
yaxis_( float *bottom, float *top );


/*
 * plot the phase axis on the right side of the plot
 */
void
phaxis_( float *bottom, float *top );


/*
 * check the status of the phase zoom button
 */
void
chkzoom_( int *iz );


/*
 * a graphical display of the part of the buffer that is currently
 * displayed in the plotter window.
 * nopp : number of points in r array
 * nbeg : begin of plotted range
 * nend : end of plotted range
 * wf   : first wavenumber in r array
 * wl   : last wavenumber in r array
 */
void
bmeter_( int *nopp, int *nbeg, int *nend, float *wf, float *wl );


/*
 * show how many points per pixel are displayed with binning
 */
void
shpix_( int *pppix );


/*
 * flush pending requests to the X server
 */
void
dflush_();


/*
 * returns the data associated with a mouse click (in world coordinates)
 * ibut   : the button
 * iclick : which click in the list
 * ax,ay  : world coordinates of the point
 * ix     : pixel coordinate in x direction of the click
 */
void
dmouse_( int *ibut, int *iclick, float *ax, float *ay, int *ix );


/*
 * returns mouse data in PHASE world coordinates
 * ibut     : the button
 * iclick   : which click in the list
 * pax,pay  : phase world coordinates of the point
 */
void
pdmouse_( int *ibut, int *iclick, float *pax, float *pay );


/*
 * return the number of clicks that were stored for one button
 */
void
dclick_( int *ibut, int *num );


/*
 * clear all mouse position markers for a specified button
 */
void
mclear_( int *ibut );


/*
 * clear a single specified mouse click marker 
 */
void
smclear_( int *ibut, int *imarker );


/*
 * set the unit that is being used in xgremlin (wavenumbers / wavelengths)
 * 
 * Interpretation:
 * iunit = -1 : wavenumber
 * iunit = +1 : wavelength
 */
void
setunit_( int *iunit );


/*
 * set (most of the) colors used in the plotting area.
 * the colour of the object 'obj' is set to 'col'.
 */
void
setcol_( char obj[], char col[], ftnlen olen, ftnlen clen );


/*
 * set the color table index to the first color. Used for overplotting.
 */
void
firstcol_();


/*
 * Makes uses the current color in the color table for drawing and
 * increments the color index. Used in overplotting.
 */
void
nextcol_();


/*
 * set the default drawing colour
 */
void
defcol_( int *icol );

/*-----------------------------------------------------------------*/

/*
 * is passed the current drawing mode ( 'n' = normal, 'r' = real part only,
 * 'i' = imag. part only, 'c' = complex and plots a mode indicator
 * in the lower, left corner of the plotting window
 */
void
shmode_( char m[], ftnlen mlen );


/*
 * displays an auxiliary line marker but does not enter the line into the 
 * line list. this function can be used to mark a position after e.g. finding 
 * the centre of gravity of a line. 'wx' is the position of the marker in 
 * world coordinates.
 */
void
auxmark_( float *wx );


/*
 * draw a wavelength label at the world-position (x,y)
 * xlbl is the wavenumber to be plotted in the label. If 'unit' == 1
 * the primary unit (wavenumber) will be used but if 'unit' == 2 the 
 * currently selected secondary unit will be used
 */
void
drawlbl_( float *x, float *y, float *xlbl, int *unit );


/*
 * turn on phase plotting: istat == 0 turn off, istat == 1 turn on
 */
void
phaseplt_( int *istat );


/*
 * cycle to the next secondary unit ( nm, Hz, GHz )
 */
void
nxtunit_();


/*-----------------------------------------------------------
 * line list related stuff
 *-----------------------------------------------------------*/

/*
 * the following mechanism is used for marking lines:
 * 1) information about a line to be marked is stored with a call to 
 *    setline_ . This does not automatically display the line but the line
 *    is flagged 'visible'. 
 *
 * 2) mrklines actually displays all lines that are flagged visible
 *    ( new ones and old ones)
 *
 * 3) a line can be flagged 'active' with a call to lineactv. These lines
 *    are subsequently displayed in a different color.
 *
 * 4) a call to linevis can be used to flag all lines visible or invisible
 */

/*
 * insert data of a line in the marker list, marks it visible and displays it
 * vis    : =1 : plot a new line marker; =0 : no new line marker
 * wnum   : wavenumber of line
 * npoint : data point of line
 * amp    : line intensity
 * wid    : line width
 * id     : the id string of the line
 */
void
insmarker_(int *vis, double *wnum, int *npoint, float *amp, float *wid, 
           char *id, ftnlen len);


/*
 * update parameters of a single line in the plotter line list
 * num    : number of line
 * point  : line point position
 * wnum   : wave number
 * amp    : line intensity
 * wid    : line width
 */
void
updone_(int *num, double *point, float *wnum, float *amp, float *wid);

 
/*
 * update some parameters in the entire plotter line list
 * point  : array with line point positions
 * wnum   : array with wave numbers
 * amp    : line intensities
 * wid    : line widths
 */
void
updlin_(double *point, float *wnum, float *amp, float *wid);
 

/*
 * just display a line marker but do not insert it into the marker list
 * this is used for the plot mode command 'v'. the marker is plotted
 * in grey (marks the old position of a moved marker)
 * wnum : position of line in world coordinates
 */
void
shwmarker_( float *wnum );


/*
 * draws new labels atop the plot
 */
void
dlabels_();


/*
 * marks those lines in the current plotter window which are not flagged 'visible'
 */
void
marklines_();


/*
 * finds the line at data point 'ipoint' in the current plotter list
 * and removes it, returns the number of the removed line in 'nline'
 * ipoint : number of data point
 * nbin   : binning used in plotting
 * nline  : the number of the line removed
 */
void
remline_( int *ipoint, int *nbin, int *nline );


/*
 * flags all lines in the current list visible
 */
void
linevis_( int *iflag );


/*
 * flags the specified line in the current list 'active'
 * active = 1 : flag line active
 * active = 0 : flag line inactive
 */
void
lineact_( int *nline, int *active);


/*
 * finds out which line corresponds to a given point
 * ipoint : number of data point
 * nbin   : binning used in plotting
 * nline  : the number of the line clicked on
 */
void
numline_( int *ipoint, int *nbin, int *nline );


/*
 * renumbers all lines in the plotter line list
 */
void
renumlin_();


/*
 * flags all lines in the line list active
 */
void
allactive_( int *iact );


/*
 * displays the line info string in the plotter window
 */
void
lineinfo_( char s[], ftnlen len );


/*
 * displays the total number of lines in the internal 
 * line list in the lower left corner of the plot
 */
void
linecnt_();


/*
 * empties the complete plotter line list
 */
void
llerase_();


/* ---------------------------------------------------------------
 * Functions for the plot mode undo stack
 * ---------------------------------------------------------------
 */

/*
 * clears the undo stack of all stored frames
 */
void
clrfrm_();

/*
 * stores a frame in the undo stack
 */
void
stofrm_(int *nleft, int *nright, float *wtop, float *wbottom);

/*
 * recalls a frame from the undo stack
 */
void
recfrm_(int *nleft, int *nright, float *wtop, float *wbottom, int *ndone);


#endif /* _PlotMode_h */

/*-----------------------------------------------------------------*/

/* Revision history:
 * -----------------
 * $Log: PlotMode.h,v $
 * Revision 1.28  1996/07/09 02:50:40  ulf
 * added a new function 'updone_'
 * to update the parameters of a single line
 *
 * Revision 1.27  1996/07/09 01:15:49  ulf
 * new parameter 'vis' for function 'insmarker'
 *
 * Revision 1.26  1996/06/15 20:07:08  ulf
 * added functions for plot mode undo
 *
 * Revision 1.25  1996/03/23 11:27:13  ulf
 * fixed function 'updlin'
 *
 * Revision 1.24  1996/03/23  10:15:51  ulf
 * add function 'sabsref' to set an absolute reference point
 *
 * Revision 1.23  1996/03/13  15:55:07  ulf
 * new function 'updlin'
 *
 * Revision 1.22  1996/01/17  14:38:25  ulf
 * change wavenumber argument wnum to type double
 *
 * Revision 1.21  1996/01/12  09:29:23  ulf
 * add function 'pgpbin' for binned plotting in the phase coordinate
 * system
 *
 * Revision 1.20  1996/01/08  09:26:41  ulf
 * store line width with 'insmarker' subroutine
 *
 * Revision 1.19  1995/11/21  17:28:55  ulf
 * add intensity parameter to function 'insmarker_'
 *
 * Revision 1.18  1995/11/16  04:01:09  ulf
 * new function 'linecnt' to display total number of lines in the plot
 *
 * Revision 1.17  1995/11/11  10:39:38  ulf
 * plot orientation in 'screen_dump'
 *
 * Revision 1.16  1995/10/14  15:28:17  ulf
 * jet another colour (blue) added to default_colors
 *
 * Revision 1.15  1995/10/14  15:24:52  ulf
 * new default colour (green)
 *
 * Revision 1.14  1995/10/10  15:00:14  ulf
 * re-arrange colors for plotting to make selection faster (in average)
 *
 * Revision 1.13  1995/10/10  14:53:16  ulf
 * new color number for real plotting
 *
 * Revision 1.12  1995/10/03  13:21:59  ulf
 * added Fortran callable function 'shwmarker' to mark a moved line
 *
 * Revision 1.11  1995/09/23  15:34:59  ulf
 * new function 'dashsty' to change the line style - used for monochrome displays
 *
 * Revision 1.10  1995/09/18  17:18:51  ulf
 * add 2nd parameter to function 'lineact_'
 *
 * Revision 1.9  1995/08/24  20:06:51  ulf
 * removed all those set<color> functions
 *
 * Revision 1.8  1995/08/19  19:23:52  ulf
 * added all functions required for phase plotting
 *
 * Revision 1.7  1995/08/06  20:33:54  ulf
 * add support for all secondary units, line labelling and phase plotting
 *
 * Revision 1.6  1995/07/23  02:30:29  ulf
 * modify function 'svcsys_' to pass global minimum and maximum to plotting module
 *
 * Revision 1.5  1995/07/21  02:48:34  ulf
 * new function 'ptlinlog' to activate lin-log plotting mode
 *
 * Revision 1.4  1995/07/04  04:12:04  ulf
 * removed functions for double buffering. They were not used by Xgremlin
 * and eliminated from the Display widget to reduce memory consumption.
 *
 * Revision 1.3  1995/07/01  16:34:15  ulf
 * changed color --> color_name in 'setcolor_'
 *
 * Revision 1.2  1995/06/28  20:48:39  ulf
 * no change
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */






