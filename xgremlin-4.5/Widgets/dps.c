/*
 * dps.c -- Display widget to postscript.
 *
 * Note that there is a Lisp version in postscript.lisp that needs
 * to be updated if you change or make any bugfixes here.
 * 
 * (c) Copyright 1989-1992 Sundar Narasimhan
 *     Artificial Intelligence Laboratory
 *     Massachusetts Institute of Technology, Cambridge, MA 02139
 *     (see ../README for details)
 */

/*
 * modified for Xgremlin, Ulf Griesmann 5/95
 * retains only the ability to dump the Display widget to PostScript files
 */

#include <stdio.h>
#include <sys/param.h>
#include <X11/Xlib.h>
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/cursorfont.h>

#include "DisplayP.h"

#define PTS_PER_INCH         72
#define PAGE_WIDTH_IN_PTS    603
#define PAGE_HEIGHT_IN_PTS   783


/*
 * the Xgremlin version string
 */
extern char xgremlin_version[];


/*
 * the current drawing color
 */
static int current_color = -1;


static void
display_def_fonts(w, fp)
Widget w;
FILE *fp;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    int i;

    /* do the default font */
    fprintf(fp, "/font-d /%s findfont [0.18 inch 0 0 0.18 inch 0 0] makefont def\n",
	    dpy->dp_psfamily );
    fprintf(fp, "/set-font-d { font-d setfont } def\n");

    /* now do the others */
    for (i=0; i<dpy->dp_num_fonts; i++) {
       fprintf(fp,"/font-%d /%s findfont [0.18 inch 0 0 0.18 inch 0 0] makefont def\n",
	       i, dpy->dp_psfamily);
       fprintf(fp, "/set-font-%d { font-%d setfont } def\n", i, i);
    }
}

static int
display_header(fp, x, y, width, height)
FILE *fp;
double x, y, width, height;
{
    char *time_to_string();
    if (fp == NULL) return -1;

    fprintf(fp, "%%!PS-ADOBE-2.0\n");
    fprintf(fp, "%%%%Creator: %s\n", xgremlin_version );
    fprintf(fp, "%%%%Created on: %s\n", time_to_string(0));
    fprintf(fp, "%%%%BoundingBox: %f %f %f %f\n", x, y, x+width, y+height);
    fprintf(fp, "%%%%EndComments\n");

    fprintf(fp, "/inch { %d mul } def\n", PTS_PER_INCH);
    fprintf(fp, "/pathlength {\n");
    fprintf(fp, "flattenpath\n");
    fprintf(fp, "/dist 0 def\n");
    fprintf(fp, "{ /yfirst exch def /xfirst exch def\n");
    fprintf(fp, "/ymoveto yfirst def /xmoveto xfirst def }\n");
    fprintf(fp, "{ /ynext exch def /xnext exch def\n");
    fprintf(fp, "/dist dist ynext yfirst sub dup mul\n");
    fprintf(fp, "xnext xfirst sub dup mul add sqrt add def\n");
    fprintf(fp, "/yfirst ynext def /xfirst xnext def }\n");
    fprintf(fp, "{}\n");
    fprintf(fp, "{/ynext ymoveto def /xnext xmoveto def\n");
    fprintf(fp, "/dist dist ynext yfirst sub dup mul\n");
    fprintf(fp, "xnext xfirst sub dup mul add sqrt add def\n");
    fprintf(fp, "/yfirst ynext def /xfirst xnext def}\n");
    fprintf(fp, "pathforall\n");
    fprintf(fp, "dist\n");
    fprintf(fp, "} def\n");
    fprintf(fp, "\n");
    fprintf(fp, "/centerdash {\n");
    fprintf(fp, "/pattern exch def\n");
    fprintf(fp, "/pathlen pathlength def\n");
    fprintf(fp, "/patternlength 0 def\n");
    fprintf(fp, "pattern {\n");
    fprintf(fp, "patternlength add /patternlength exch def\n");
    fprintf(fp, "} forall\n");
    fprintf(fp, "pattern length 2 mod 0 ne\n");
    fprintf(fp, "{ /patternlength patternlength 2 mul def } if\n");
    fprintf(fp, "/first pattern 0 get def\n");
    fprintf(fp, "/last patternlength first sub def\n");
    fprintf(fp, "/n pathlen last sub patternlength div def\n");
    fprintf(fp, "/endpart pathlen patternlength n mul sub\n");
    fprintf(fp, "last sub 2 div def\n");
    fprintf(fp, "/offset first endpart sub def\n");
    fprintf(fp, "pattern offset setdash\n");
    fprintf(fp, "} def\n");
    fprintf(fp, "\n");
    fprintf(fp, "/p1 [ .5 3 ] def\n");
    fprintf(fp, "/p2 [ 8 5 .5 5 ] def\n");
    fprintf(fp, "/p3 [ 8 5 ] def\n");
    fprintf(fp, "/p4 [ 8 5 .5 3 .5 5 ] def\n");
    fprintf(fp, "/p5 [ 8 5 .5 3 .5 3 .5 5 ] def\n");
    fprintf(fp, "/pats [ p1 p1 p2 p3 p4 p5 ] def\n");
    fprintf(fp, "\n");
    fprintf(fp, "/drawline {\n");
    fprintf(fp, "/y2 exch def\n");
    fprintf(fp, "/x2 exch def\n");
    fprintf(fp, "/y1 exch def\n");
    fprintf(fp, "/x1 exch def\n");
    fprintf(fp, "/thickness exch def\n");
    fprintf(fp, "/pattern exch def\n");
    fprintf(fp, "gsave\n");
    fprintf(fp, "newpath\n");
    fprintf(fp, "x1 y1 moveto\n");
    fprintf(fp, "x2 y2 lineto\n");
    fprintf(fp, "closepath\n");
    fprintf(fp, "gsave\n");
    fprintf(fp, "initmatrix\n");
    fprintf(fp, "pattern 0 ne { pats pattern get centerdash } if \n");
    fprintf(fp, "thickness setlinewidth\n");
    fprintf(fp, "stroke\n");
    fprintf(fp, "grestore\n");
    fprintf(fp, "grestore\n");
    fprintf(fp, "} def\n");
    fprintf(fp, "/rect {\n");
    fprintf(fp, "/height exch def\n");
    fprintf(fp, "/width exch def\n");
    fprintf(fp, "/y exch def\n");
    fprintf(fp, "/x exch def\n");
    fprintf(fp, "/thickness exch def\n");
    fprintf(fp, "/pattern exch def\n");
    fprintf(fp, "/filled exch def\n");
    fprintf(fp, "gsave\n");
    fprintf(fp, "newpath\n");
    fprintf(fp, "x y moveto\n");
    fprintf(fp, "width 0 rlineto\n");
    fprintf(fp, "0 0 height sub rlineto\n");
    fprintf(fp, "0 width sub 0 rlineto\n");
    fprintf(fp, "0 height rlineto\n");
    fprintf(fp, "closepath\n");
    fprintf(fp, "gsave\n");
    fprintf(fp, "initmatrix\n");
    fprintf(fp, "pattern 0 ne { pats pattern get centerdash } if\n");
    fprintf(fp, "thickness setlinewidth\n");
    fprintf(fp, "filled 1 eq { fill } { stroke } ifelse\n");
    fprintf(fp, "grestore\n");
    fprintf(fp, "grestore\n");
    fprintf(fp, "} def\n");
    fprintf(fp, "\n");
    fprintf(fp, "/ourarc {\n");
    fprintf(fp, "/angle2 exch def\n");
    fprintf(fp, "/angle1 exch def\n");
    fprintf(fp, "/yradius exch def\n");
    fprintf(fp, "/xradius exch def\n");
    fprintf(fp, "/y exch def\n");
    fprintf(fp, "/x exch def\n");
    fprintf(fp, "/thickness exch def\n");
    fprintf(fp, "/pattern exch def\n");
    fprintf(fp, "/filled exch def\n");
    fprintf(fp, "gsave\n");
    fprintf(fp, "newpath\n");
    fprintf(fp, "x y translate\nxradius yradius scale\n");
    fprintf(fp, "0 0 1 angle1 angle2 arc\n");
    fprintf(fp, "closepath\n");
    fprintf(fp, "gsave\n");
    fprintf(fp, "initmatrix\n");
    fprintf(fp, "pattern 0 ne { pats pattern get centerdash } if\n");
    fprintf(fp, "thickness setlinewidth\n");
    fprintf(fp, "filled 1 eq { fill } { stroke } ifelse\n");
    fprintf(fp, "grestore\n");
    fprintf(fp, "grestore\n");
    fprintf(fp, "} def\n");

    fprintf(fp, "/magic {1 add 180 mul cos 1 eta add mul exch 2 add 180 mul cos 1 eta sub mul add 2 div} bind def %% (C) bkph 1989\n"); 
    
    /* This is to set up the screen angle and frequency and screen function */
    fprintf(fp, "/freq 42 def  %% 35 42 53 71\n");
    fprintf(fp, "/angle 45 def /eta 0.08 def %% ellipticity\n");
    fprintf(fp, "freq angle {magic} setscreen\n");

    /* 
     * Frequencies are given by dpi/(sqrt{2} * n), where dpi =  300, 
     * for n integer. Number of distinct halftones possible is (2 n*n + 1) 
     */

    return 0;
}


/*
 * Top-level interface to this converter.
 * Note that width and height are controlleable. 
 * I suspect that my postscript code is not too fast.
 * There must be a way of doing gsave initmatrix etc. in a better way.
 * (One way would be to convert everything to ps units -- some day).
 */
int
display_postscript(Widget w, char *fname, 
		   double width_in_inches, double height_in_inches, 
		   int landscape)
{
    FILE *fp;
    double width_in_pts;  
    double height_in_pts;
    double x_translation;
    double y_translation;
    double rotation;    

    if ( landscape == 0 )
      {
	 /*
	  * center portrait picture on the page (horizontally only)
	  */
	 rotation = 0.0;
	 height_in_pts = width_in_inches * PTS_PER_INCH;
	 width_in_pts = height_in_inches * PTS_PER_INCH;
	 x_translation = ( PAGE_WIDTH_IN_PTS - width_in_pts ) /2;
	 y_translation = ( PAGE_HEIGHT_IN_PTS - height_in_pts );
      }
    else
      {
	 /*
	  * center landscape picture on page
	  */
	 rotation = 90.0;
	 width_in_pts = width_in_inches * PTS_PER_INCH;
	 height_in_pts = height_in_inches * PTS_PER_INCH;
	 x_translation = ( PAGE_WIDTH_IN_PTS  + height_in_pts )/2;
	 y_translation = ( PAGE_HEIGHT_IN_PTS - width_in_pts ) /2;
      }

    if ((fp = fopen(fname, "w")) == NULL) {
	fprintf(stderr, "DisplayWidget: couldn't open %s\n", fname);
	return -1;
    }

    /* first do the header */
    display_header(fp, x_translation, y_translation, width_in_pts,
		   height_in_pts);

    /* define the fonts */
    display_def_fonts(w, fp);

    /* now do the commands */
    display_pscommands(w, fp, x_translation, y_translation, width_in_pts,
		       height_in_pts, rotation);

    fprintf(fp, "showpage\n");
    fclose(fp);

    return 0;
}

static void
display_ps_color(w, fp, color)
Widget w;
FILE *fp;
int color;
{
    double r, g, b;

    display_getRGB(w, color, &r, &g, &b);
    fprintf(fp, "%g %g %g setrgbcolor\n", r, g, b);
}

static void
display_ps_text(w, d, fp, rot)
Widget w;
display_cmd d;
FILE *fp;
double rot;
{
    double x, y;
    int width = w->core.width;
    int height = w->core.height;
    int fn;
    XPoint *f;
    XTextItem *item;

    f = &(d->data.t.point);
    item = &(d->data.t.item);

    x = (double) (f->x)/(double) width;
    y = (double) (height - f->y) / (double) height;

    fprintf(fp, "%f %f moveto\n", x, y);
    if ( d->data.t.color != current_color )
      {
	 current_color = d->data.t.color;
	 display_ps_color( w, fp, current_color );
      }
    fn = display_get_font_idx(w, item->font);
    if (fn < 0)
      fprintf(fp, "gsave initmatrix %g rotate set-font-d (%s) show grestore\n", 
	      rot, item->chars);
    else
      fprintf(fp, "gsave initmatrix %g rotate set-font-%d (%s) show grestore\n",
	      rot, fn, item->chars);
}

static void
display_ps_vtext(w, d, fp)
Widget w;
display_cmd d;
FILE *fp;
{
    double x, y;
    int width = w->core.width;
    int height = w->core.height;
    int offset, fn;
    int xc, yc;
    XPoint *f;
    char c, *str;
    XTextItem *item;

    f = &(d->data.vt.point);
    item = &(d->data.vt.item);

    fprintf(fp, "gsave\n");
    fn = display_get_font_idx(w, item->font);
    if (fn < 0)
      fprintf(fp, "set-font-d\n");
    else
      fprintf(fp, "set-font-%d\n", fn);

    if ( d->data.vt.color != current_color )
      {
	 current_color = d->data.vt.color;
	 display_ps_color( w, fp, current_color );
      }

    str = d->data.vt.string;
    offset = d->data.vt.offset;
    xc = f->x;
    yc = f->y;
    x = (double) (xc)/(double) width;

    while ((c = *str++)) {
	y = (double) (height - yc) / (double) height;
	fprintf(fp, "%f %f moveto ", x, y);
	fprintf(fp, "gsave initmatrix (%c) show grestore \n", c);
	yc += offset;
    }
    fprintf(fp, "grestore\n");
}

static void
display_ps_line(w, d, fp)
Widget w;
display_cmd d;
FILE *fp;
{
    XPoint *f, *s;
    DisplayWidget dpywidget = (DisplayWidget) w;

    double x1, y1, x2, y2, thk;
    int i, width = dpywidget->core.width;
    int height = dpywidget->core.height;
    
    if ( d->data.l.color != current_color )
      {
	 current_color = d->data.l.color;
	 display_ps_color( w, fp, current_color );
      }

    for (i=0; i<(d->data.l.no_points-1); i++) {
	f = &(d->data.l.points[i]);
	s = &(d->data.l.points[i+1]);
	x1 = (double) (f->x)/ (double) width;
	y1 = (double) (height - f->y)/ (double) height;
	x2 = (double) (s->x)/ (double) width;
	y2 = (double) (height - s->y)/ (double) height;
	thk = 0.25 + ((double) d->data.l.thickness * 0.25);
	fprintf(fp, "%d %f %f %f %f %f drawline\n",
		d->data.l.pattern, thk, x1, y1, x2, y2);
    }
}

static void
display_ps_polygon(w, d, fp)
Widget w;
display_cmd d;
FILE *fp;
{
    XPoint *f;
    DisplayWidget dpywidget = (DisplayWidget) w;

    double x1, y1, thk;
    int i, width = dpywidget->core.width;
    int height = dpywidget->core.height;
   
    /*  First define the path */
    fprintf(fp, "gsave\n newpath\n");

    for (i=0; i<(d->data.py.no_points); i++) {
	f = &(d->data.py.points[i]);
	x1 = (double) (f->x)/ (double) width;
	y1 = (double) (height - f->y)/ (double) height;

	if (i == 0) 
	  fprintf(fp, "\t%f %f moveto\n", x1, y1);
	else 
	  fprintf(fp, "\t%f %f lineto\n", x1, y1);
    }

    fprintf(fp, "closepath\n");
   
    /* Select the pattern */
    fprintf(fp, "gsave\ninitmatrix\n");
    fprintf(fp, "%d 0 ne { pats %d get centerdash } if \n",
	    d->data.py.pattern, d->data.py.pattern);
   
    /* Set line thickness */
    thk = 0.25 + ((double) d->data.py.thickness * 0.25);
    fprintf(fp, "%f setlinewidth\n", thk);

    /* Handle color */
    display_ps_color(w, fp, d->data.py.color);

    if (d->data.py.flags & 0x02) 
      fprintf(fp, "fill\n");
    else 
      fprintf(fp, "stroke\n");
    fprintf(fp, "grestore\n");
    fprintf(fp, "grestore\n");
}

static void
display_ps_rect(w, d, fp)
Widget w;
display_cmd d;
FILE *fp;
{
    DisplayWidget dpywidget = (DisplayWidget) w;

    double x, y, width, height;
    int corewidth = dpywidget->core.width;
    int coreheight = dpywidget->core.height;

    x = (double) d->data.r.x/ (double) corewidth;
    y = (double) (coreheight - d->data.r.y)/(double) coreheight;
    width = (double) d->data.r.width / (double) corewidth;
    height = (double) d->data.r.height / (double) coreheight;

    if ( d->data.r.color != current_color )
      {
	 current_color = d->data.r.color;
	 display_ps_color( w, fp, current_color );
      }

    fprintf(fp, "%d %d %d %f %f %f %f rect\n",
 	    d->data.r.filled, d->data.r.pattern, d->data.r.thickness,
	    x, y, width, height);
}

static void
display_ps_arc(w, d, fp)
Widget w;
display_cmd d;
FILE *fp;
{
    DisplayWidget dpywidget = (DisplayWidget) w;

    double x, y, width, height;
    int corewidth = dpywidget->core.width;
    int coreheight = dpywidget->core.height;

    x = (double) (d->data.a.x)/ (double) corewidth;
    y = (double) (coreheight - d->data.a.y)/(double) coreheight;
    width = (double) d->data.a.width / (double) corewidth;
    height = (double) d->data.a.height / (double) coreheight;

    if ( d->data.a.color != current_color )
      {
	 current_color = d->data.a.color;
	 display_ps_color( w, fp, current_color );
      }

    fprintf(fp, "%d %d %d %f %f %f %f %f %f ourarc\n",
	    d->data.a.filled, d->data.a.pattern, d->data.a.thickness,
	    x + (width/2), y-(height/2), width/2, 
	    height/2, 
	    (double) d->data.a.angle1/64,
	    (double) d->data.a.angle2/64);
}

void
display_pscommands(w, fp, xorigin, yorigin, width, height, rotation)
Widget w;
FILE *fp;
double xorigin, yorigin, width, height, rotation;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    display_cmd d;
    
    fprintf(fp, "gsave\n");
    fprintf(fp, "%f %f translate\n", xorigin, yorigin);
    fprintf(fp, "%f rotate\n", rotation);
    fprintf(fp, "%f %f scale\n", width, height);

    /* draw a nice box */
    fprintf(fp, "0 0 0.0 0.0 1.0 0.0 drawline\n");
    fprintf(fp, "0 0 1.0 0.0 1.0 1.0 drawline\n");
    fprintf(fp, "0 0 1.0 1.0 0.0 1.0 drawline\n");
    fprintf(fp, "0 0 0.0 0.0 0.0 1.0 drawline\n");
    
    /* define the clipping path */
    fprintf(fp, "newpath\n0 0 moveto\n");
    fprintf(fp, "1 0 lineto\n1 1 lineto\n0 1 lineto\nclosepath\nclip\n");

    for (d = dpy->dp_buffer; d != NULL; d = d->next) 
      {
	 if ( d->printable == 0 ) /* skip it */
	   continue;

	 switch(d->type) 
	   {
	    case display_line:
	      display_ps_line(w, d, fp);
	      break;
	    case display_text:
	      display_ps_text(w, d, fp, rotation);
	      break;
	    case display_vtext:
	      display_ps_vtext(w, d, fp);
	      break;
	    case display_point:
	      break;
	    case display_rectangle:
	    case display_filled_rectangle:
	      display_ps_rect(w, d, fp);
	      break;
	    case display_arc:
	      display_ps_arc(w, d, fp);
	      break;
	    case display_polygon:
	      display_ps_polygon(w, d, fp);
	      break;
	    default:
	      break;
	   }
      }
    fprintf(fp, "grestore\n");
}

