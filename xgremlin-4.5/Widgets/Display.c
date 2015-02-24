
/*
 * Display Widget
 * 
 * (c) Copyright 1988-90 Sundar Narasimhan,
 *     MIT Artificial Intelligence Laboratory,
 *     Cambridge, MA 02139 USA.
 *     (see ../README for details).
 */

/*
 * modified for the Xgremlin program, Ulf Griesmann 5/95
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/cursorfont.h>

#include <stdlib.h>

#include "DisplayP.h"

#define GRAY_LEVELS	8

/*
 * local prototypes
 */
static void display_make_clearpix( Widget, int, int, int );
static void display_arc_cmd( Widget, display_cmd, int );
static void display_free_cmds( Widget );
static void display_polygon_cmd( Widget, display_cmd, int );
static void display_resetgc( Widget );
int file_execute(char *, void *, int, char, char * );

/*
 * graphics cursor shape and mask
 */
#include "cursor_shape.xbm"
#include "cursor_mask.xbm"

static char display_translations[] = 
  "<Key>:                  display_key_callback()     \n\
   <MotionNotify>:         display_mouse_callback()   \n\
   <ButtonPress>:          display_button_callback()  \n\
   <ButtonRelease>:        display_button_rcallback()  ";

static XtActionsRec display_actions[] = {
    { "display_key_callback", (XtActionProc)display_key_callback },
    { "display_mouse_callback", (XtActionProc)display_mouse_callback },
    { "display_button_callback", (XtActionProc)display_button_callback },
    { "display_button_rcallback", (XtActionProc)display_button_rcallback },
};

static XtResource resources[] = {
    { XtNfont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
	XtOffset(DisplayWidget, display.dp_default_font), 
	XtRString, "9x15" },
    { XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
	XtOffset(DisplayWidget, display.dp_foreground),
	XtRString, XtDefaultForeground },
    { XtNbackground, XtCBackground, XtRPixel, sizeof(Pixel),
	XtOffset(DisplayWidget, display.dp_background),
	XtRString, XtDefaultBackground },
    { XtNkcallback, XtCCallback, XtRCallback, sizeof(XtCallbackProc),
	XtOffset(DisplayWidget, display.dp_kcallback),
	XtRCallback, (caddr_t) NULL },
    { XtNmcallback, XtCCallback, XtRCallback, sizeof(XtCallbackProc),
	XtOffset(DisplayWidget, display.dp_mcallback),
	XtRCallback, (caddr_t) NULL },
    { XtNbcallback, XtCCallback, XtRCallback, sizeof(XtCallbackProc),
	XtOffset(DisplayWidget, display.dp_bcallback),
	XtRCallback, (caddr_t) NULL },
    { XtNbrcallback, XtCCallback, XtRCallback, sizeof(XtCallbackProc),
	XtOffset(DisplayWidget, display.dp_brcallback),
	XtRCallback, (caddr_t) NULL },
    { XtNrcallback, XtCCallback, XtRCallback, sizeof(XtCallbackProc),
	XtOffset(DisplayWidget, display.dp_rcallback),
	XtRCallback, (caddr_t) NULL },
    { XtNpsfamily, XtCString, XtRString, sizeof(String),
	XtOffset(DisplayWidget, display.dp_psfamily),
	XtRString, "Times-Roman"},
    { XtNcolorscale, XtCColorscale, XtRColorscale, sizeof(int),
	XtOffset(DisplayWidget, display.dp_colorscale),	
	XtRString, "grayscale" }
};

#define MAX_PATS                5

#define POS_STR_LEN 128          
static char string_buffer[POS_STR_LEN];  /* static buffer for display_string */

static char patterns[5][10] = {
{4, 3},				/* dotted */
{8, 8, 8, 2},			/* dotdash */
{8, 4},				/* dashed */
{4, 4, 4, 1, 4, 1},		/* dotdotdash */
{4, 1, 4, 1, 4, 1, 4, 4}};	/* dash3dot */

static int pattern_size[] = {
  2, 4, 2, 6, 8 };

#define COLORSCALE_GRAY        1
#define COLORSCALE_HSB         2
#define COLORSCALE_USER        3

static int max_num_requests;    /* maximum number of requests to server */
static int max_num_points;

static int g_printable = 1;     /* flag objects printable globally */

/* 
 * converter type 
 */
#define STR_TO_COL ((*XtConverter)(XrmValue *, Cardinal *, XrmValue *, XrmValue *))

static char *
disp_alloc(char *orig)
{
    char *tmp = XtCalloc (strlen(orig)+1, sizeof(char));
    strcpy(tmp, orig);
    return (tmp);
}

static void
display_class_initialize()
{
   /* 
    * not needed 
    */
   /* 
    * XtAddConverter(XtRString, XtRColorscale, StringToColorscale, NULL, 0); 
    */
}

static void
/*ARGSUSED*/
display_initialize(request, new)
Widget request, new;
{
    DisplayWidget dpy = (DisplayWidget) new;
    int i;

    if(dpy->core.width == 0)
      dpy->core.width = 300;
    if(dpy->core.height == 0)
      dpy->core.height = 300;

    dpy->display.dp_buffer = dpy->display.dp_current = NULL;
    dpy->display.dp_curx = dpy->display.dp_cury = 0;
    dpy->display.dp_moved = TRUE;
    dpy->display.dp_fonts = NULL;
    dpy->display.dp_num_fonts = 0;
    dpy->display.dp_thickness = 0;
    dpy->display.dp_pattern = 0;
    dpy->display.dp_color = dpy->display.dp_foreground;
    dpy->display.dp_head = dpy->display.dp_tail = NULL;
    dpy->display.dp_no_cmds  = 0;
    dpy->display.dp_pix	 = (Pixmap)NULL;
    dpy->display.dp_bpl	 = 0;
    dpy->display.dp_colormap = (Colormap)NULL;
    dpy->display.dp_inhibit = FALSE;
    for ( i = XtNumber(dpy->display.dp_colors); i--;
	 dpy->display.dp_colors[i] = 0);

}

static void
display_realize(Widget w, Mask *mask, XSetWindowAttributes *attributes)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
	      
    Pixmap cur_shape, cur_mask;
    XColor shape_color, mask_color, exact_color;
    Cursor cursor;

    *mask |= CWBitGravity;
    attributes->bit_gravity = NorthWestGravity;
    XtCreateWindow(w, InputOutput, (Visual *) CopyFromParent,
		   *mask, attributes);

    dpy->dp_clearpix = 0;

    if(XtIsRealized(w)) 
      {
	 XGCValues values;

	 values.foreground = dpy->dp_foreground;
	 values.background = dpy->dp_background; 
	 values.font       = dpy->dp_default_font->fid;
	 
	 dpy->dp_GC = XCreateGC(XtDisplay(w),
				XtWindow(w),
				(XtGCMask) GCForeground|GCBackground|GCFont,
				&values);
	 dpy->dp_pix = XCreatePixmap(XtDisplay(w), 
				     RootWindowOfScreen(XtScreen(w)),
				     dpywidget->core.width,
				     dpywidget->core.height,
				     dpywidget->core.depth);

	 if (dpy->dp_pix == 0 ) 
	   {
	      fprintf(stderr, "display_realize: pixmap creation failure!\n");
	      exit(0);
	   } 
	 dpy->dp_bpl = (dpywidget->core.width + 7) >> 3;
	 dpy->dp_image = 0;
	 dpy->dp_width = dpywidget->core.width;
	 dpy->dp_height = dpywidget->core.height;
	 
	/* 
	 * Initialize color map (we now use the default colormap
	 */
	 dpy->dp_colormap = XDefaultColormap( XtDisplay(w), DefaultScreen(XtDisplay(w)) );
	 dpy->dp_visual   = XDefaultVisual( XtDisplay(w), DefaultScreen(XtDisplay(w)) );

	 dpy->dp_output = (unsigned char *) 
	   calloc(dpywidget->core.width * 
		  dpywidget->core.height, sizeof(unsigned char));
	      
	 dpy->dp_data = (unsigned char *) 
	   calloc(dpywidget->core.width * 
		  dpywidget->core.height, sizeof(unsigned char));

	 switch (dpy->dp_colorscale) {
	  case COLORSCALE_HSB:
	    display_hsbscale(w);
	    break;
	  case COLORSCALE_USER:
	    display_userscale(w);
	    break;
	  default:
	  case COLORSCALE_GRAY:
	    display_grayscale(w);
	    break;
	    
	    /* 
	     * Now that we've allocated a colormap 
	     */
	    if (dpy->dp_background != 0) 
	      {
		 display_make_clearpix(w, 
				       dpywidget->core.width, 
				       dpywidget->core.height, 
				       dpywidget->core.depth);
	      }
	 }

	 /*
          * create a better graphics cursor
	  */
	 cur_shape = XCreatePixmapFromBitmapData(
                         w->core.screen->display, XtWindow(w),
		         cursor_shape_bits, 
			 cursor_shape_width, cursor_shape_height,
                         1, 0, 1 );

	 cur_mask  = XCreatePixmapFromBitmapData(
	                 w->core.screen->display, XtWindow(w),
                         cursor_mask_bits, 
                         cursor_mask_width, cursor_mask_height,
                         1, 0, 1 );

	 XLookupColor(w->core.screen->display, dpy->dp_colormap,
		     "VioletRed3", &exact_color, &shape_color );

	 XLookupColor(w->core.screen->display, dpy->dp_colormap,
		     "white", &exact_color, &mask_color );

	 cursor = XCreatePixmapCursor(w->core.screen->display,
                      cur_shape, cur_mask, 
                      &shape_color, &mask_color,
                      cursor_shape_x_hot, cursor_shape_y_hot );

	 XDefineCursor(w->core.screen->display, XtWindow(w), cursor );

    }
    display_free_cmds(w);

    /*
     * data needed for XDrawLines calls
     */
    max_num_requests = XMaxRequestSize( XtDisplay(w) );
    max_num_points = ( max_num_requests - 3 ) / 2; /* Xlib Ref Man. II, p 226 */
}

static void
display_make_clearpix(Widget w, int width, int height, int depth)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    unsigned char *data;
    XImage *ximage;

    /* This means we CAN'T use xor to clear a pixmap */
    dpy->dp_clearpix = XCreatePixmap(XtDisplay(w), 
				     RootWindowOfScreen(XtScreen(w)),
				     width,
				     height,
				     depth);
    data = (unsigned char *) malloc(sizeof(unsigned char) * height * width);
    if ( data == NULL) {
	fprintf(stderr, "DisplayWidget: malloc failed\n");
	exit(-1);
    }
    memset( data,(unsigned char) dpy->dp_background, width * height );
    ximage = XCreateImage(XtDisplay(w), 
			  dpy->dp_visual == NULL ? 
			  DefaultVisual(XtDisplay(w), 
					DefaultScreen(XtDisplay(w))) :
			  dpy->dp_visual,
			  8,
			  ZPixmap,
			  0,
			  data,
			  width, height, 8, 0);
		
    if (XPutImage(XtDisplay(w), dpy->dp_clearpix,
		  dpy->dp_GC,
		  ximage, 0, 0, 0, 0, 
		  ximage->width, ximage->height) < 0) {
	fprintf(stderr, "DisplayWidget: XPutImage failed\n");
	exit(1);
    }
    /*		XDestroyImage(ximage); */
}

/*ARGSUSED*/
static void
display_destroy(w)
Widget w;
{
    /* Not yet */
}

/*ARGSUSED*/
static void
display_resize(w)
Widget w;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    struct _DisplayResizeCBDataRec cbd;
    unsigned char *ptr;
    int nh = dpywidget->core.height;
    int nw = dpywidget->core.width;
    int nd = dpywidget->core.depth;
    int ow = dpy->dp_width;
    int oh = dpy->dp_height;

    if (ow == nw && oh == nh)
      return;

    if (dpy->dp_pix)
      XFreePixmap(XtDisplay(w), dpy->dp_pix);

    dpy->dp_pix = XCreatePixmap(XtDisplay(w), 
				RootWindowOfScreen(XtScreen(w)),
				nw, nh, nd);
    if(dpy->dp_pix == 0) {
	fprintf(stderr, "display_realize: pixmap creation failure!\n");
	exit(0);
    }  

    if (dpy->dp_clearpix) {
	XFreePixmap(XtDisplay(w), dpy->dp_clearpix);
	display_make_clearpix(w, nw, nh, nd);
    }

    dpy->dp_bpl = (nw + 7) >> 3;
    
    if (dpy->dp_output)
      free((char *) dpy->dp_output);

    if (dpy->dp_colormap == 0) 
      {
	 dpy->dp_output = (unsigned char *) 
	   calloc(dpy->dp_bpl * nh, sizeof(unsigned char));	
      } 
    else 
      {
	 if (dpy->dp_image)
	   XDestroyImage(dpy->dp_image); 
	 dpy->dp_output = (unsigned char *) 
	   calloc(nw * nh, sizeof(unsigned char));
      }

    ptr = (unsigned char *)malloc(nw*nh);
    if (ptr == NULL) {
	fprintf(stderr, "display_resize: ran out of memory!\n");
	return;
    }

    free((char *)dpy->dp_data);
    dpy->dp_data = ptr;
    dpy->dp_width = nw;
    dpy->dp_height = nh;

    cbd.new_width = dpywidget->core.width;
    cbd.new_height = dpywidget->core.height;
    XtCallCallbacks(w, XtNrcallback, (XtPointer) &cbd);
}

void
display_clear_out_buffer(w)
Widget w;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);

    if (dpy->dp_colormap == 0)
      memset(dpy->dp_output, '\0', dpy->dp_bpl * dpy->dp_height);
    else 
      memset(dpy->dp_output, '\0', dpy->dp_width * dpy->dp_height);
    return;
}

static void
display_dispatch(w, d, op)
Widget w;
display_cmd d;
int op;
{
    switch(d->type) {
      case display_line:
	display_line_cmd(w, d, op);
	break;
      case display_text:
	display_text_cmd(w, d, op);
	break;
      case display_vtext:
	display_vtext_cmd(w, d, op);
	break;
      case display_point:
	display_point_cmd(w, d, op);
	break;
      case display_rectangle:
      case display_filled_rectangle:
	display_rect_cmd(w, d, op);
	break;
      case display_arc:
	display_arc_cmd(w, d, op);
	break;
      case display_polygon:
	display_polygon_cmd(w, d, op);
	break;
      case display_style:
      case display_NULL:
	break;
    }
}

/*ARGSUSED*/
static void
display_expose(Widget w, XEvent *event, Region region)
{
   XExposeEvent *ee;

   ee = (XExposeEvent *)event;
   if ( ee->count == 0 )
     display_redraw(w);
}

void
display_redraw(Widget w)
{
   DisplayWidget dpywidget = (DisplayWidget) w;
   DisplayPart *dpy = &(dpywidget->display);
   display_cmd d;

   for(d = dpy->dp_buffer; d != NULL; d = d->next) 
     display_dispatch(w, d, GXcopy);
}

/*
 * This function draws all display_cmds downstream from the given
 * display cmd. This is what makes display_inhibit really useful.
 */
void
display_draw_from(Widget w, display_cmd d)
{
   display_cmd d_loc;

   d_loc = d->next;
   while (d_loc != NULL) 
     {
	display_dispatch(w, d_loc, GXcopy);
	d_loc = d_loc->next;
     }
}

/*
 * the following function was obviously 'forgotten' by the 
 * author of the display widget.
 */

/*ARGSUSED*/
static void
display_setvalues(current, request, new)
Widget current, request, new;
{
   /* currently does nothing */
}

/* Call back routines */
/*ARGSUSED*/
void display_key_callback(w, event, params, no)
Widget w;
XKeyEvent *event;
String *params;
unsigned long *no;
{
    char cbuf[4];
    int  i, no_chars;

    no_chars = XLookupString(event, cbuf, 4, NULL, NULL);

    for (i=0; i< no_chars; i++) {
	XtCallCallbacks(w, XtNkcallback, event);
    }
}

void display_button_callback(w, e, params, no)
Widget w;
XEvent *e;
String *params;
unsigned long *no;
{
    XButtonEvent *event = (XButtonEvent *)e;
    XtCallCallbacks(w, XtNbcallback, event);
}


void display_button_rcallback(w, e, params, no)
Widget w;
XEvent *e;
String *params;
unsigned long *no;
{
    XButtonEvent *event = (XButtonEvent *)e;
    XtCallCallbacks(w, XtNbrcallback, event);
}

void display_mouse_callback(w, e, params, no)
Widget w;
XEvent *e;
String *params;
unsigned long *no;
{
    XMotionEvent *event = (XMotionEvent *)e;
    XtCallCallbacks(w, XtNmcallback, event);
}

#define XtCalloc calloc 
#define XtRealloc realloc
#define XtFree    free

display_cmd
display_new_cmd()
{
    display_cmd ret;
    if((ret = (display_cmd) XtCalloc(sizeof(struct _display_cmd), 1)) == NULL) {
	fprintf(stderr, "dpy_new: Ran out of memory!\n");
	return NULL;
    }
    return(ret);
}

int
display_add_cmd(Widget w, display_cmd d)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);

    if(dpy->dp_buffer == NULL) {
	dpy->dp_buffer = dpy->dp_current = d;
    } else {
	if (dpy->dp_current->type == display_polygon &&
	    (dpy->dp_current->data.py.flags & 0x1))
	  display_end_polygon(w);

	dpy->dp_current->next = d;
	dpy->dp_current = d;
    }
    return 0;
}

static void
display_free_cmd_single(display_cmd d)
{
    switch (d->type) {
      case display_line:
	free((char *)d->data.l.points);
	break;
      case display_polygon:
	free((char *)d->data.py.points);
	break;
      case display_text:
	free((char *)d->data.t.item.chars);
	break;
      case display_vtext:
	free((char *)d->data.vt.string);
	break;
      case display_point:
      case display_style:
      case display_rectangle:
      case display_filled_rectangle:
      case display_arc:
      case display_NULL:
	break;
    }
    free((char *)d);
}

static void
display_free_cmds(w)
Widget w;
{
    XGCValues values;
    unsigned mask = 0;

    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    display_cmd d, tofree;

    d = dpy->dp_buffer;
    while(d != NULL) 
      {
	 tofree = d;
	 d = d->next;
	 display_free_cmd_single(tofree);
      }
    dpy->dp_buffer = dpy->dp_current = NULL;
    XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, False);

    /* Now go about clearing the Pixmap associated with the window */
    if (dpy->dp_clearpix != 0) 
      {
	 values.function = GXcopy;
	 mask |= GCFunction;
	 XChangeGC(XtDisplay(w), dpy->dp_GC, mask, &values);
	 if (dpy->dp_pix != 0)
	   XCopyArea(XtDisplay(w), dpy->dp_clearpix, dpy->dp_pix, dpy->dp_GC, 
		     0, 0, 
		     dpywidget->core.width, dpywidget->core.height,
		     0, 0);	    	    
      }
    else 
      {
	 values.function = GXxor;
	 mask |= GCFunction;
	 XChangeGC(XtDisplay(w), dpy->dp_GC, mask, &values);
	 
	 if (dpy->dp_pix != 0) 
	   XCopyArea(XtDisplay(w),  dpy->dp_pix, dpy->dp_pix, dpy->dp_GC, 0, 0, 
		     dpywidget->core.width, dpywidget->core.height,
		     0, 0);	    
	
	 values.function = GXcopy;
	 XChangeGC(XtDisplay(w), dpy->dp_GC, mask, &values);    
      }
}

void
display_clear(Widget w)
{
    display_free_cmds(w);
}

/* User calleable routines */
display_cmd
display_draw_line2(Widget w, int xfrom, int yfrom, int xto, int yto, 
		   int thickness, int pattern, int color)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);

    static int xlast, ylast;
    int othickness, opattern;
    display_cmd d;

    othickness = dpy->dp_thickness;
    opattern = dpy->dp_pattern;

    if(xfrom != xlast || yfrom != ylast) 
      display_moveto(w, xfrom, yfrom);

    display_set_linestyle(w, thickness, pattern); 
    display_set_color_value(w, color);
    d = display_draw_line(w, xto, yto);
    xlast = xto; 
    ylast = yto;
    return d;
}


display_cmd
display_draw_line_simple(Widget w, int xfrom, int yfrom, int xto, int yto)
{
    display_cmd d;
    static int xlast, ylast;
    static Widget lastw;
    if(xfrom != xlast || yfrom != ylast || w != lastw) 
      display_moveto(w, xfrom, yfrom);
    d = display_draw_line(w, xto, yto);
    xlast = xto; 
    ylast = yto;
    lastw = w;
    return d;
}


display_cmd
display_draw_line_color(Widget w, int xfrom, int yfrom, int xto, int yto, int color)
{
    display_cmd d;
    static int xlast, ylast;
    static Widget lastw;
    if(xfrom != xlast || yfrom != ylast || w != lastw) 
      display_moveto(w, xfrom, yfrom);
    display_set_color_value( w, color );
    d = display_draw_line(w, xto, yto);
    xlast = xto; 
    ylast = yto;
    lastw = w;
    return d;
}

void
display_touch(Widget w)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    dpy->dp_moved = TRUE;
}

void
display_moveto(Widget w, int x, int y)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);

    if(dpy->dp_curx == x && dpy->dp_cury == y) 
      return;
    dpy->dp_curx = x;
    dpy->dp_cury = y;
    dpy->dp_moved = TRUE;
}

static Pixel
disp_color_value(Widget w, char *color)
{
   XrmValue in, out;
   in.addr = color;
   in.size = strlen(color) + 1;
   XtConvert(w, XtRString, &in, XtRPixel, &out);
   if (!out.addr)
     return -1;
   return( *(Pixel *)out.addr);
}

Pixel
display_set_color(Widget w, char *name)
{
   int p;

   DisplayWidget dpywidget = (DisplayWidget) w;
   DisplayPart *dpy = &(dpywidget->display);
   p = disp_color_value(w, name); 
   if(p >= 0)
     dpy->dp_color = (Pixel)p;
   return( (Pixel)dpy->dp_color);
}

Pixel
display_get_color(Widget w)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    return((Pixel) dpy->dp_color);
}

void
display_set_color_value(Widget w, int val)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    /* This should be val != foreground_pixel ? */
    if(val >= 0)
      dpy->dp_color = val;
}

int
display_set_color_RGB(Widget w, int r, int g, int b)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    Colormap cmap = dpy->dp_colormap;
    XColor color;

    if (cmap == 0) 
      return -1;
    color.red = 65535 * (float) r/256.0;
    color.green = 65535 * (float) g/256.0;
    color.blue = 65535 * (float) b/256.0;
    color.pixel = 0;
    if (!XAllocColor(XtDisplay(w), cmap, &color)) 
      dpy->dp_color = XBlackPixelOfScreen(XtScreen(w));
    else
      dpy->dp_color = color.pixel;    
    return 0;
}

void
display_set_linestyle(Widget w, int thickness, int pattern)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);

    if(thickness == dpy->dp_thickness &&
       pattern == dpy->dp_pattern) 
      return;

    dpy->dp_thickness = thickness;
    if(pattern >= 0 && pattern < MAX_PATS) 
      dpy->dp_pattern = pattern;
    dpy->dp_moved = TRUE;
}

void
display_get_linestyle(Widget w, int *thickness, int *pattern)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);

    if(thickness)
      *thickness = dpy->dp_thickness;
    if(pattern)
      *pattern = dpy->dp_pattern;
}

/*
 * The next two functions are NOT affected by the value
 * of dp_inhibit.
 */
void
display_erase_xorline(Widget w, display_cmd d)
{
    display_line_cmd(w, d, GXxor);
    free((char *)d->data.l.points);
    free((char *)d);
    display_resetgc(w);
}

display_cmd
display_draw_xorline(Widget w, int xfrom, int yfrom, int xto, int yto, int color )
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    XPoint *pt;
    int idx;

    display_cmd d = display_new_cmd();

    d->printable = g_printable;
    d->type = display_line;
    d->type = display_line;
    d->data.l.no_size = 2;
    d->data.l.thickness = dpy->dp_thickness;
    d->data.l.pattern = dpy->dp_pattern;
    d->data.l.color = color;
    d->data.l.points = (XPoint *)XtCalloc(sizeof(XPoint), 
					  d->data.l.no_size);
    if (d->data.l.points == NULL)
      return NULL;

    idx = d->data.l.no_points = 0;
    pt = (XPoint *) &(d->data.l.points[idx]);
    pt->x = xfrom;
    pt->y = yfrom;
    idx++;
    pt = (XPoint *) &(d->data.l.points[idx]);
    pt->x = xto;
    pt->y = yto;
    d->data.l.no_points = idx + 1;
    display_set_color_value( w, color );
    display_line_cmd(w, d, GXxor);
    display_resetgc(w);
    return (d);
}

display_cmd
display_draw_line(w, xto, yto)
Widget w;
int xto, yto;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);

    display_cmd d;
    XPoint *pt;
    int idx;

    d = dpy->dp_current;
    if((d != NULL) && d->type == display_line && (dpy->dp_moved == FALSE)) {
	if((d->data.l.no_points + 1) >= d->data.l.no_size) {
	    pt = (XPoint *) XtRealloc(d->data.l.points, 
				      (sizeof(XPoint) *
				       d->data.l.no_size * 2));
	    if (pt == NULL)
	      return NULL;

	    d->data.l.no_size = d->data.l.no_size * 2;
	    d->data.l.points = pt;
	}
	idx = d->data.l.no_points;
    } else {
	d = display_new_cmd();
	d->printable = g_printable;
	d->type = display_line;
	d->data.l.no_size = 5;
	d->data.l.thickness = dpy->dp_thickness;
	d->data.l.pattern = dpy->dp_pattern;
	d->data.l.color = dpy->dp_color;
	d->data.l.points = (XPoint *)XtCalloc(sizeof(XPoint), 
					      d->data.l.no_size);
	if (d->data.l.points == NULL)
	  return NULL;

	idx = d->data.l.no_points = 0;
	display_add_cmd(w, d);

	pt = (XPoint *) &(d->data.l.points[idx]);
	pt->x = dpy->dp_curx;
	pt->y = dpy->dp_cury;

	idx++;
    }
    pt = (XPoint *) &(d->data.l.points[idx]);
    pt->x = xto;
    pt->y = yto;
    d->data.l.no_points = idx + 1;
    if (dpy->dp_inhibit != True)
      display_line_cmd(w, d, GXcopy);
    dpy->dp_moved = FALSE;
    dpy->dp_curx = xto;
    dpy->dp_cury = yto;
    return(d);
}

void
display_line_cmd(Widget w, display_cmd d, int op)
{
   XGCValues values;
   int  thk, pat;
   unsigned mask = 0;
   XPoint *xp;
   int nop_done;
   int num_points;
   DisplayWidget dpywidget = (DisplayWidget) w;
   DisplayPart *dpy = &(dpywidget->display);
   
   if((thk = d->data.l.thickness) >= 0) {
      values.line_width = thk;
      mask |= GCLineWidth;
   }
   if((pat = d->data.l.pattern) == 0) 
     values.line_style = LineSolid;
   else {
      values.line_style = LineOnOffDash;
      XSetDashes(XtDisplay(w), dpy->dp_GC, 0,
		 patterns[pat-1], pattern_size[pat-1]);
   }
   values.foreground = d->data.l.color;
   mask |= GCForeground; 
   mask |= GCLineStyle;
    
   if(op != GXcopy) 
     {
	values.function = op;
	mask |= GCFunction;
     }
      
   XChangeGC(XtDisplay(w), dpy->dp_GC, mask, &values);
   
   /*
    * The calls to XDrawlines have to be batched to ensure that
    * the maximum number of requests for the server is not exceeded
    */
   nop_done = 0;            /* number of lines plotted so far */
   xp = d->data.l.points;   /* data of the lines */
   num_points = max_num_points;
   while ( nop_done < d->data.l.no_points )
     {
	if ( num_points + nop_done > d->data.l.no_points )
	  num_points = d->data.l.no_points - nop_done;
	XDrawLines( XtDisplay(w), XtWindow(w), dpy->dp_GC,
		   xp+nop_done, num_points, CoordModeOrigin );
	nop_done += num_points;
     }
}

Font
display_get_font(Widget w, int font)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);

    if(font < 0 || (font >= dpy->dp_num_fonts)) 
      return(dpy->dp_default_font->fid);
    return((dpy->dp_fonts[font])->fid);
}

/* This function performs the inverse mapping. */
int
display_get_font_idx(Widget w, Font font)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    int i;

    if (font == dpy->dp_default_font->fid)
      return -1;
    for (i=0; i<dpy->dp_num_fonts; i++) {
	if (font == (dpy->dp_fonts[i])->fid) 
	  return( i );
    }
    return -1;
}

int
display_font_height(Widget w, int font)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    XFontStruct *fptr;

    if(font < 0 || (font >= dpy->dp_num_fonts)) 
      fptr = dpy->dp_default_font;
    else
      fptr = dpy->dp_fonts[font];
    return(fptr->max_bounds.ascent + fptr->max_bounds.descent);
}

int
display_font_width(Widget w, int font, char *str)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    XFontStruct *fptr;
    XCharStruct c_max, c_min;

    if(font < 0 || (font >= dpy->dp_num_fonts)) 
      fptr = dpy->dp_default_font;
    else
      fptr = dpy->dp_fonts[font];
    
    c_max = fptr->max_bounds;
    c_min = fptr->min_bounds;
    return (strlen(str) * ((int) c_max.rbearing - c_min.lbearing));
}

int
display_load_font(Widget w, char *name)
{
    XFontStruct *f;
    
    if((f = XLoadQueryFont(XtDisplay(w), name)) != NULL) 
      return(display_add_font(w, f));
    else
      return -1;
}

int 
display_add_font(Widget w, XFontStruct *f)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    int i;

    /* check and see if we already have this font loaded first */
    for (i=0; i<dpy->dp_num_fonts; i++) {
	if (f == dpy->dp_fonts[i])
	  return i;
    }
    if(dpy->dp_num_fonts == 0) {
	dpy->dp_fonts = (XFontStruct **) 
	  XtCalloc(sizeof(XFontStruct *), 1);
    } else {
	dpy->dp_fonts = (XFontStruct **)
	  XtRealloc(dpy->dp_fonts, 
		    sizeof(XFontStruct *) * (dpy->dp_num_fonts+1));
    }
    dpy->dp_fonts[dpy->dp_num_fonts] = f;
    return((int) dpy->dp_num_fonts++);
}

/*
 * The following two functions are not affected by dp_inhibit
 */
void
display_erase_xortext(Widget w, display_cmd d)
{
    display_text_cmd(w, d, GXxor);
    free((char *)d->data.t.item.chars);
    free((char *)d);
    display_resetgc(w);
}

display_cmd
display_draw_xortext(Widget w, char *str, int x, int y, int font, int color)
{
    display_cmd d;

    d = display_new_cmd();
    d->printable = g_printable;
    d->type = display_text;
    d->data.t.point.x = x;
    d->data.t.point.y = y;
    d->data.t.color = color;
    d->data.t.item.font = display_get_font(w, font);
    d->data.t.item.delta = 0;
    d->data.t.item.chars = (char *)disp_alloc(str);
    d->data.t.item.nchars = strlen(str);
    display_text_cmd(w, d, GXxor);
    display_resetgc(w);
    return d;
}

display_cmd
display_draw_text(Widget w, char *str, int x, int y, int font, int color)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    display_cmd d;

    d = display_new_cmd();
    d->printable = g_printable;
    d->type = display_text;
    d->data.t.point.x = x;
    d->data.t.point.y = y;
    d->data.t.color = color;
    d->data.t.item.font = display_get_font(w, font);
    d->data.t.item.delta = 0;
    d->data.t.item.chars = (char *)disp_alloc(str);
    d->data.t.item.nchars = strlen(str);
    display_add_cmd(w, d);    
    if (dpy->dp_inhibit != True)
      display_text_cmd(w, d, GXcopy);
    return d;
}

/*
 * added for the continuous display of the cursor position
 */
void
display_string(Widget w, char *str, int x, int y, int font, int color)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    struct _display_cmd d;          /* static because we don't want to store it */

    strcpy( string_buffer, str );   /* use static buffer */
    d.type = display_text;
    d.data.t.point.x = x;
    d.data.t.point.y = y;
    d.data.t.color = color;
    d.data.t.item.font = display_get_font(w, font);
    d.data.t.item.delta = 0;
    d.data.t.item.chars = (char *)&string_buffer;
    d.data.t.item.nchars = strlen( string_buffer );
    if (dpy->dp_inhibit != True)
      display_text_cmd(w, &d, GXcopy);
}

static display_cmd
display_draw_rectangle_aux(w, x, y, width, height, color, filled)
Widget w;
int x, y, width, height, color, filled;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    display_cmd d = display_new_cmd();
    if (d == NULL)
      return NULL;
    
    if (filled)
      d->type = display_filled_rectangle;
    else
      d->type = display_rectangle;

    d->printable = g_printable;
    d->data.r.x = x; d->data.r.y = y;
    d->data.r.width = width; d->data.r.height = height;
    d->data.r.color = color;
    d->data.r.filled = filled;
    d->data.r.thickness = dpy->dp_thickness;
    d->data.r.pattern = dpy->dp_pattern;
    display_add_cmd(w, d);    
    if (dpy->dp_inhibit != True)
      display_rect_cmd(w, d, GXcopy);
    return (d);
}

display_cmd
display_draw_rectangle(w, x, y, width, height, color)
Widget w;
int x, y, width, height, color;
{
    return(display_draw_rectangle_aux(w, x, y, width, height, color, 0));
}

display_cmd
display_fill_rectangle(w, x, y, width, height, color)
Widget w;
int x, y, width, height, color;
{
    return(display_draw_rectangle_aux(w, x, y, width, height, color, 1));
}

void
display_rect_cmd(w, d, op)
Widget w;
display_cmd d;
int op;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    Drawable drawable = XtWindow(w);
    int thk, pat;
    XGCValues values;
    unsigned mask = 0;

    mask |= GCForeground;
    values.foreground = d->data.r.color; 

    if(op != GXcopy) {
	mask |= GCFunction;
	values.function = op;
    }

    if ((thk = d->data.r.thickness) >= 0) {
	values.line_width = thk;
	mask |= GCLineWidth;
    }
    
    if((pat = d->data.r.pattern) == 0) 
      values.line_style = LineSolid;
    else {
	values.line_style = LineOnOffDash;
	XSetDashes(XtDisplay(w), dpy->dp_GC, 0,
		   patterns[pat-1], pattern_size[pat-1]);
    }
    mask |= GCLineStyle;

    XChangeGC(XtDisplay(w), dpy->dp_GC, mask, &values);

    if (d->data.r.filled)
      XFillRectangle(XtDisplay(w), drawable, dpy->dp_GC,
		     d->data.r.x, d->data.r.y, d->data.r.width, 
		     d->data.r.height);      
    else
      XDrawRectangle(XtDisplay(w), drawable, dpy->dp_GC,
		     d->data.r.x, d->data.r.y, d->data.r.width, 
		     d->data.r.height);
}

/* Begin arc, circle support */
static display_cmd
display_arc_aux(w, x, y, width, height, angle1, angle2, filled, color)
Widget w;
int x, y, width, height, angle1, angle2, filled;
int color;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    display_cmd d = display_new_cmd();
    if (d == NULL)
      return NULL;

    d->type = display_arc;
    d->printable = g_printable;
    d->data.a.x = x; d->data.a.y = y;
    d->data.a.width = width; d->data.a.height = height;
    d->data.a.color = color;
    d->data.a.filled = filled;
    d->data.a.thickness = dpy->dp_thickness;
    d->data.a.pattern = dpy->dp_pattern;
    d->data.a.angle1 = angle1;
    d->data.a.angle2 = angle2;
    display_add_cmd(w, d);    
    if (dpy->dp_inhibit != True)
      display_arc_cmd(w, d, GXcopy);
    return (d);
}

static void
display_arc_cmd(w, d, op)
Widget w;
display_cmd d;
int op;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    Drawable drawable = XtWindow(w);
    int thk, pat;
    XGCValues values;
    unsigned mask = 0;

    mask |= GCForeground;
    values.foreground = d->data.a.color; 

    if(op != GXcopy) {
	mask |= GCFunction;
	values.function = op;
    }

    if ((thk = d->data.a.thickness) >= 0) {
	values.line_width = thk;
	mask |= GCLineWidth;
    }
    
    if((pat = d->data.a.pattern) == 0) 
      values.line_style = LineSolid;
    else {
	values.line_style = LineOnOffDash;
	XSetDashes(XtDisplay(w), dpy->dp_GC, 0,
		   patterns[pat-1], pattern_size[pat-1]);
    }
    mask |= GCLineStyle;

    XChangeGC(XtDisplay(w), dpy->dp_GC, mask, &values);

    if (d->data.a.filled)
      XFillArc(XtDisplay(w), drawable, dpy->dp_GC,
	       d->data.a.x, d->data.a.y, 
	       (unsigned short) d->data.a.width, 
	       (unsigned short) d->data.a.height, 
	       d->data.a.angle1, d->data.a.angle2);
    else
      XDrawArc(XtDisplay(w), drawable, dpy->dp_GC,
	       d->data.a.x, d->data.a.y, 
	       (unsigned int) d->data.a.width, 
	       (unsigned int) d->data.a.height, 
	       d->data.a.angle1, d->data.a.angle2);
}

/* user visible functions */
display_cmd
display_draw_arc(w, x, y, width, height, angle1, angle2, color)
Widget w;
int x, y, width, height, angle1, angle2;
int color;
{
    return(display_arc_aux(w, x, y, width, height, angle1, angle2, 0, color));
}

display_cmd
display_fill_arc(w, x, y, width, height, angle1, angle2, color)
Widget w;
int x, y, width, height, angle1, angle2;
int color;
{
    return(display_arc_aux(w, x, y, width, height, angle1, angle2, 1, color));
}

display_cmd
display_draw_circle(w, x, y, radius, color)
Widget w;
int x, y, radius;
int color;
{
    return (display_draw_arc(w, x - radius, y - radius, 
			     2 * radius, 2 * radius, 0, 64 * 360, color));
}

display_cmd
display_draw_ellipse(w, x, y, xradius, yradius, color)
Widget w;
int x, y, xradius, yradius;
int color;
{
    return (display_draw_arc(w, x - xradius, y - yradius, 
			     2 * xradius, 2 * yradius, 0, 64 * 360, color));
}

display_cmd
display_fill_ellipse(w, x, y, xradius, yradius, color)
Widget w;
int x, y, xradius, yradius;
int color;
{
    return (display_fill_arc(w, x - xradius, y - yradius, 
			     2 * xradius, 2 * yradius, 0, 64 * 360, color));
}

display_cmd
display_fill_circle(w, x, y, radius, color)
Widget w;
int x, y, radius;
int color;
{
    return (display_fill_arc(w, x - radius, y - radius, 
			     2 * radius, 2 * radius, 0, 64 * 360, color));
}
/* End arc, circle support */

void
display_text_cmd(w, d, op)
Widget w;
display_cmd d;
int op;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    XGCValues values;
    unsigned mask = 0;

    mask |= GCForeground;
    values.foreground = d->data.t.color;
    if(op != GXcopy) {
	mask |= GCFunction;
	values.function = op;
    }

    XChangeGC(XtDisplay(w), dpy->dp_GC, mask, &values);
    XDrawText(XtDisplay(w), XtWindow(w), dpy->dp_GC,
	      d->data.t.point.x, d->data.t.point.y, 
	      &(d->data.t.item), 1);
}

display_cmd
display_draw_vtext(w, str, x, y, font, color, offset)
Widget w;
char *str;
int x, y, font, color;
int offset;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    display_cmd d;
    int i;

    d = display_new_cmd();
    d->printable = g_printable;
    d->type = display_vtext;
    d->data.vt.point.x = x;
    d->data.vt.point.y = y;
    d->data.vt.color = color;
    d->data.vt.item.font = display_get_font(w, font);
    d->data.vt.item.delta = 0;
    d->data.vt.item.chars = NULL;
    d->data.vt.item.nchars = 1;
    d->data.vt.string = (char *)disp_alloc(str);
    i = d->data.vt.len = strlen(str);
    d->data.vt.offset = offset + display_font_height(w, font);
    display_add_cmd(w, d);    
    if (dpy->dp_inhibit != True)
      display_vtext_cmd(w, d, GXcopy);
    return d;
}

void
display_vtext_cmd(w, d, op)
Widget w;
display_cmd d;
int op;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    XGCValues values;
    Drawable drawable = XtWindow(w);
    unsigned mask = 0;
    char *str = d->data.vt.string;
    char c;
    int  x = d->data.vt.point.x;
    int  y = d->data.vt.point.y;
    int  offset = d->data.vt.offset;

    mask |= GCForeground;
    values.foreground = d->data.vt.color;

    if(op != GXcopy) {
	mask |= GCFunction;
	values.function = op;
    }

    XChangeGC(XtDisplay(w), dpy->dp_GC, mask, &values);
    
    while( (c = *str++) ) {
	d->data.vt.item.chars = (char *)&c;
	XDrawText(XtDisplay(w), drawable, dpy->dp_GC,
		  x, y, &(d->data.vt.item), 1);
	y += offset;
    }
}

static void
display_resetgc(w)
Widget w;
{
    XGCValues values;
    unsigned mask = 0;
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    mask |= GCFunction;
    values.function = GXcopy;
    XChangeGC(XtDisplay(w), dpy->dp_GC, mask, &values);
}

display_cmd
display_draw_point(w, x, y, color)
Widget w;
int x, y, color;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    display_cmd d;

    d = display_new_cmd();
    d->printable = g_printable;
    d->type = display_point;
    d->data.p.point.x = x;
    d->data.p.point.y = y;
    d->data.p.color = color;
    display_add_cmd(w, d);
    if (dpy->dp_inhibit != True)
      display_point_cmd(w, d, GXcopy);
    return d;
}

void    
display_point_cmd(w, d, op)
Widget w;
display_cmd d;
int op;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    XGCValues values;
    Drawable drawable = XtWindow(w);
    unsigned mask = 0;
    int  x = d->data.p.point.x;
    int  y = d->data.p.point.y;

    mask |= GCForeground;
    values.foreground = d->data.p.color;
    if (op != GXcopy) {
	mask |= GCFunction;
	values.function = op;
    }

    XChangeGC(XtDisplay(w), dpy->dp_GC, mask, &values);
    XDrawPoint(XtDisplay(w), drawable, dpy->dp_GC, x, y);
}

/*
 * the next two functions are intended mainly for speed.
 * If display is inhibited a lot of the functions won't draw,
 * but will just add to the display list.
 */
void
display_inhibit(Widget w)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    dpy->dp_inhibit = True;
}

void
display_permit(Widget w)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    dpy->dp_inhibit = False;
}


unsigned char *
display_get_bufferptr(w, height, width)
Widget w;
int *height, *width;
{
    DisplayWidget dpywidget = (DisplayWidget)w;
    DisplayPart *dpy = &(dpywidget->display);
    if (height)
      *height = dpywidget->core.height;
    
    if (width)
      *width = dpywidget->core.width;

    return ((unsigned char *) dpy->dp_data);
}

unsigned char *
display_get_out_bufferptr(w, height, width, bpl)
Widget w;
int *height, *width;
int *bpl;
{
    DisplayWidget dpywidget = (DisplayWidget)w;
    DisplayPart *dpy = &(dpywidget->display);
    if (height)
      *height = dpywidget->core.height;
    
    if (width)
      *width = dpywidget->core.width;

    if (bpl)
      *bpl = dpy->dp_bpl;

    return ((unsigned char *) dpy->dp_output);
}

void
display_set_buffer(w, x, y, value) 
Widget w;
int x, y, value;
{
    DisplayWidget dpywidget = (DisplayWidget)w;
    DisplayPart *dpy = &(dpywidget->display);
/*    dpy->dp_data[((dpywidget->core.height - y -1) * dpywidget->core.width) + x] = (unsigned char) value; */
    if (value >= DISPLAY_MAX_COLORS)
      value = DISPLAY_MAX_COLORS;
    if (value < 0)
      value = 0;
    dpy->dp_data[((dpywidget->core.width * y) + x)] = (unsigned char) value;
}

void
display_erase_all(Widget w)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    display_cmd d, tofree;
    
    d = dpy->dp_buffer;
    while (d != NULL) {
	/* erase it */
	display_dispatch(w, d, GXxor);
	/* now free the storage */
	tofree = d;
	d = d->next;
	display_free_cmd_single(tofree);	
    }
    dpy->dp_buffer = dpy->dp_current = NULL;
}

int
display_erase(Widget w, display_cmd dcmd)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    display_cmd d, prev;

    for(d = prev = dpy->dp_buffer; d != NULL; prev = d, d = d->next) 
      if(d == dcmd) {
	  if(d == dpy->dp_buffer)
	    dpy->dp_buffer = d->next;
	  if(dpy->dp_buffer == NULL)
	    dpy->dp_current = NULL;
	  if(d == dpy->dp_current)
	    dpy->dp_current = prev;
	  if(prev != d) 
	    prev->next = d->next;
	  display_dispatch(w, d, GXxor);
	  display_resetgc(w);
	  XtFree(d);
	  return 0;
      }
    return -1;
}

Colormap
display_get_colormap(w)
Widget w;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    return(dpy->dp_colormap);
}

/* Color functionality */
int
display_grayscale(Widget w)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    Colormap cmap = dpy->dp_colormap;
    XColor colors[GRAY_LEVELS];
    int i, n=GRAY_LEVELS;

    if (cmap == 0) 
      return -1;

    for(i=0;i<n;i++) {
	colors[i].red=65536.0*(float)i/n;
	colors[i].green=65536.0*(float)i/n;
	colors[i].blue=65536.0*(float)i/n;
	colors[i].flags = DoRed|DoGreen|DoBlue;
	colors[i].pixel = 0;
	if(!XAllocColor(XtDisplay(w), cmap, &colors[i])) {
	    fprintf(stderr, "couldn't allocate color %d\n",i);
	    return -1;
	}
	dpy->dp_colors[i] = colors[i].pixel;
    }
    i=0;

    return 0;
}

static int
read_color(char *line, Widget w, int lineno)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    Colormap cmap = dpy->dp_colormap;
    XColor colors[GRAY_LEVELS];
    int i;
    int r, g, b;

    if (sscanf(line, "%d%*[ \t]%d%*[ \t]%d%*[ \t]%d", 
	   &i, &r, &g, &b) != 4) {
	fprintf(stderr, "Malformed line %d\n", lineno);
	return -1;
    }
    
    colors[i].red=r;
    colors[i].green=g;
    colors[i].blue=b;
    colors[i].flags = DoRed|DoGreen|DoBlue;
    colors[i].pixel = 0;
    if(!XAllocColor(XtDisplay(w), cmap, &colors[i])) {
	fprintf(stderr, "couldn't allocate color %d\n",i);
	return -1;
    }
    dpy->dp_colors[i] = colors[i].pixel;

    return 0;
}

int
display_userscale(Widget w)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    Colormap cmap = dpy->dp_colormap;
    char *fname = NULL, *getenv();

    if (cmap == 0) 
      return -1;

    if ((fname = getenv("COLORMAP")) == NULL) {
	fname = ".colors";
    }
    if (file_execute(fname, read_color, 1, '#', (char *)w) < 0)
      return -1;
    
    return 0;
}

int
display_getRGB(Widget w, long pixel, double *r, double *g, double *b)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    Colormap cmap = dpy->dp_colormap;
    XColor color;

    if (cmap == 0)  {
	/* return black */
	*r = *g = *b = 0.0;
	return -1;
    }

    color.pixel = pixel;
    XQueryColors(XtDisplay(w), cmap, &color, 1);
    *r = (double) color.red/65535;
    *g = (double) color.green/65535;
    *b = (double) color.blue/65535;
    return 1;
}

int
display_hsbscale(Widget w)
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    Colormap cmap = dpy->dp_colormap;
    XColor colors[GRAY_LEVELS];
    int i, n=GRAY_LEVELS;
    double h, r, g, b;

    if (cmap == 0) 
      return -1;

    for(i=0;i<n;i++) {
	h = (360.0 * (double) i/n);
	hsb_to_rgb((double) h, 0.5, 1.0, &r, &g, &b);
	colors[i].red=(int)(65536.0*r);
	colors[i].green=(int)(65536.0*g);
	colors[i].blue=(int) (65536.0*b);
	colors[i].flags = DoRed|DoGreen|DoBlue;
	colors[i].pixel = 0;
	if(!XAllocColor(XtDisplay(w), cmap, &colors[i])) {
	    fprintf(stderr, "couldn't allocate color %d\n",i);
	    exit(1);
	}
	dpy->dp_colors[i] = colors[i].pixel;
    }
    i=0;

    return 0;
}


DisplayClassRec displayClassRec = {
  {
/* core_class fields */	
#define superclass		(&coreClassRec)
    /* superclass	  	*/	(WidgetClass) superclass,
    /* class_name	  	*/	"display",
    /* widget_size	  	*/	sizeof(DisplayRec),
    /* class_initialize   	*/	display_class_initialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited       	*/	FALSE,
    /* initialize	  	*/	display_initialize,
    /* initialize_hook		*/	NULL,
    /* realize		  	*/	display_realize,
    /* actions		  	*/	display_actions,
    /* num_actions	  	*/	XtNumber(display_actions),
    /* resources	  	*/	resources,
    /* num_resources	  	*/	XtNumber(resources),
    /* xrm_class	  	*/	NULLQUARK,
    /* compress_motion	  	*/	TRUE,
    /* compress_exposure  	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest	  	*/	FALSE,
    /* destroy		  	*/	display_destroy,
    /* resize		  	*/	display_resize,
    /* expose		  	*/	display_expose,
    /* set_values	  	*/	(XtSetValuesFunc)display_setvalues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus	 	*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private   	*/	NULL,
    /* tm_table		   	*/	display_translations,
    /* query_geometry		*/	NULL,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
/*  {
      XtInheritChangeSensitive
  },
*/
  {
      0
  }
};

WidgetClass displayWidgetClass = (WidgetClass)&displayClassRec;

/* Color function -- This should be in X */
void
hsb_to_rgb(h,s,v,r,g,b)
double h,s,v;
double *r, *g, *b;
{
    double f,m,n,k,floor();
    int i;

    if (s == 0.0) {
	*r = v;
	*g = v;
	*b = v;
    } else {
	h = h / 60.0;
	i = (int)floor(h);
	f = h - i;
	m = v * (1.0 - s);
	n = v * (1.0 - (s * f));
	k = v * (1.0 - (s * (1.0 - f)));
	switch (i) {
	  case 0:
	    *r = v; *g = k; *b = m;
	    break;
	  case 1:
	    *r = n; *g = v; *b = m;
	    break;
	  case 2:
	    *r = m; *g = v; *b = k;
	    break;
	  case 3:
	    *r = m; *g = n; *b = v;
	    break;
	  case 4:
	    *r = k; *g = m; *b = v;
	    break;
	  case 5:
	    *r = v; *g = m; *b = n;
	    break;
	  default:
	    fprintf(stderr,"hsb_to_rgb: shouldn't be here. %f %d\n", h, i);
	    break;
	}
    }
}

/* Polygon support */
display_cmd
display_begin_fill_polygon(w)
Widget w;
{
    display_cmd d = display_begin_polygon(w);
    d->data.py.flags |= 0x02;
    return d;
}

display_cmd
display_begin_polygon(w)
Widget w;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    display_cmd d;

    d = dpy->dp_current;
    d = display_new_cmd();
    d->printable = g_printable;
    d->type = display_polygon;
    d->data.py.no_size = 5;
    d->data.py.thickness = dpy->dp_thickness;
    d->data.py.pattern = dpy->dp_pattern;
    d->data.py.color = dpy->dp_color;
    d->data.py.no_points = 0;
    d->data.py.points = (XPoint *)XtCalloc(sizeof(XPoint), 
					   d->data.py.no_size);

    if (d->data.py.points == NULL)
      return NULL;

    display_add_cmd(w, d);
    d->data.py.flags |= 0x1;
    return (d);
}

void
display_polygon_add(w, xto, yto)
Widget w;
int xto, yto;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);

    display_cmd d;
    XPoint *pt;
    int idx;

    d = dpy->dp_current;
    if ((d != NULL) && d->type != display_polygon) {
	fprintf(stderr, "Display: warning no begin_polygon before add\n");
	display_begin_polygon(w);
    }

    /* reset d because begin_polygon may have changed this */
    d = dpy->dp_current;

    /* if we have exceeded allocated storage bumpit up */
    if((d->data.py.no_points + 1) >= d->data.py.no_size) {
	pt = (XPoint *) XtRealloc(d->data.l.points, 
				  (sizeof(XPoint) *
				   d->data.py.no_size * 2));
	if (pt == NULL)
	  return;
	d->data.py.no_size = d->data.py.no_size * 2;
	d->data.py.points = pt;
    }
    idx = d->data.py.no_points; 

    pt = (XPoint *) &(d->data.py.points[idx]);
    pt->x = xto;
    pt->y = yto;
    d->data.py.no_points = idx + 1;
    return;
}

void
display_end_polygon(w)
Widget w;
{
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    int x, y, lastx, lasty;
    display_cmd d;
    d = dpy->dp_current;
    if ((d != NULL) && d->type != display_polygon) {
	fprintf(stderr, "Display: dangling end_polygon\n");
	return;
    }

    if ((d != NULL) && d->type == display_polygon) {
	if (d->data.py.no_points > 1) {
	    XPoint *f, *l;
	    f = (XPoint *) &(d->data.py.points[0]);
	    x = f->x; y = f->y;

	    l = (XPoint *) &(d->data.py.points[d->data.py.no_points-1]);
	    lastx = l->x; lasty = l->y;
	    if (x != lastx || y != lasty) {
		display_polygon_add(w, x, y);
	    }
	}
	d->data.py.flags &= ~0x1;

	if (dpy->dp_inhibit != True)
	  display_polygon_cmd(w, d, GXcopy);
    }
    return;
}

void
display_flush(Widget w)
{
   XFlush( XtDisplay(w) );
}

static void
display_polygon_cmd(w, d, op)
Widget w;
display_cmd d;
int op;
{
    XGCValues values;
    int  thk, pat;
    unsigned mask = 0;
    int nop_done;
    int num_points;
    XPoint *xp;
    DisplayWidget dpywidget = (DisplayWidget) w;
    DisplayPart *dpy = &(dpywidget->display);
    Drawable drawable = XtWindow(w);
    
    if((thk = d->data.py.thickness) >= 0) {
	values.line_width = thk;
	mask |= GCLineWidth;
    }
    if((pat = d->data.py.pattern) == 0) 
      values.line_style = LineSolid;
    else {
	values.line_style = LineOnOffDash;
	XSetDashes(XtDisplay(w), dpy->dp_GC, 0,
		   patterns[pat-1], pattern_size[pat-1]);
    }
    values.foreground = d->data.py.color;
    mask |= GCForeground; 
    mask |= GCLineStyle;
    
    if(op != GXcopy) {
	values.function = op;
	mask |= GCFunction;
    }
      
    XChangeGC(XtDisplay(w), dpy->dp_GC, mask, &values);

    /*
     * the calls to XDrawLines and XFillPolygon have to be batched
     * to ensure that the max number of requests of the server is
     * not exceeded.
     */
    nop_done = 0;        
    num_points = max_num_points;
    if (d->data.py.flags & 0x02)
      {
	 /* XFillPolygon(XtDisplay(w), drawable, dpy->dp_GC,
	  *	   d->data.py.points, d->data.py.no_points,
	  *	   Nonconvex, CoordModeOrigin);
	  */
	 xp = d->data.py.points;
	 while ( nop_done < d->data.py.no_points )
	   {
	      if ( num_points + nop_done > d->data.py.no_points )
		num_points = d->data.py.no_points - nop_done;
	      XFillPolygon( XtDisplay(w), drawable, dpy->dp_GC,
			    xp+nop_done, num_points,
			    Nonconvex, CoordModeOrigin );
	      nop_done += num_points;
	   }
      }
    else 
      {
	 /* XDrawLines(XtDisplay(w), drawable, dpy->dp_GC,
	  *	 d->data.l.points, d->data.l.no_points,
	  *	 CoordModeOrigin);
	  */
	 xp = d->data.l.points;
	 while ( nop_done < d->data.l.no_points )
	   {
	      if ( num_points + nop_done > d->data.l.no_points )
		num_points = d->data.l.no_points - nop_done;
	      XDrawLines( XtDisplay(w), drawable, dpy->dp_GC,
			    xp+nop_done, num_points,
			    CoordModeOrigin );
	      nop_done += num_points;
	   }
      }
}


void
set_objects_printable( int iflag )
{
   if ( iflag != 0 && iflag != 1 )
     return;
   g_printable = iflag;
}
