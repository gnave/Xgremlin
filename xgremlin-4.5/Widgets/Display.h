/*
 * Display.h -- The root of my widget hierarchy for graphics.
 * 
 * (c) Copyright Sundar Narasimhan 1989-1992 MIT A.I. Laboratory
 *     See ../README for details.
 */
#ifndef _XtDisplay_h
#define _XtDisplay_h

extern WidgetClass displayWidgetClass;

#define XtNkcallback 	"kcallback"	 
#define XtNmcallback 	"mcallback"	 
#define XtNbcallback 	"bcallback"	 
#define XtNbrcallback 	"brcallback"	 
#define XtNrcallback 	"rcallback"	 
#define XtNpsfamily     "psfamily"
#define XtNcolorscale   "colorscale"

#define XtRColorscale   "colorscale"
#define XtCColorscale   "colorscale"

enum display_cmd_type {
    display_text, display_vtext, display_point, display_line, display_style, 
    display_rectangle, display_filled_rectangle, display_arc,
    display_polygon, display_NULL
};

typedef struct _display_cmd *display_cmd;
struct _display_cmd {
    enum display_cmd_type type;
    int printable;             /* =1: dump command to PS; =0 don't dump */
    union {
	struct {
	    XPoint point;
	    int color;
	} p;
	struct {
	    int thickness;
	    int pattern;
	    XPoint *points;
	    int no_points;
	    int no_size;
	    int color;
	} l;
	struct {
	    int thickness;
	    int pattern;
	    XPoint *points;
	    int no_points;
	    int no_size;
	    int color;
	    unsigned char flags;
	} py;
	struct {
	    int x, y, width, height;
	    int thickness;
	    int pattern;
	    int filled;
	    int color;
	} r;
	struct {
	    XPoint point;	    
	    XTextItem item;
	    int color;
	} t;
	struct {
	    XPoint point;
	    XTextItem item;
	    int  offset;
	    char *string;
	    int  len;
	    int color;
	} vt;
	struct {
	    int x, y, width, height;
	    int angle1, angle2;
	    int thickness;
	    int pattern;
	    int filled;
	    int color;
	} a; 
    } data;
    display_cmd next;
};

struct _DisplayResizeCBDataRec {
    int new_width;
    int new_height;
    int doit;
};
typedef struct _DisplayResizeCBDataRec *DisplayResizeCBData;
typedef struct _DisplayClassRec *DisplayWidgetClass;
typedef struct _DisplayRec *DisplayWidget;

/* Forward Declarations for certain routines */
#include "Display_p.h"
#include "dps_p.h"

/* Macro for speed */
#define DISPLAY_MAX_COLORS      127

#endif



