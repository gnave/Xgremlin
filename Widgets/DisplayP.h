/*
 * DisplayP.h -- Private header file for the Display Widget.
 * 
 * (c) Copyright Sundar Narasimhan 1989-1992 MIT A.I. Laboratory
 *     See ../README for details.
 */
#ifndef _XDisplayP
#define _XDisplayP

#include <Display.h>

/* Class Declarations */
typedef struct {
    int foo; 
} DisplayClassPart;

typedef struct _DisplayClassRec {
    CoreClassPart core_class;
    DisplayClassPart display_class;
}DisplayClassRec;

extern DisplayClassRec displayClassRec;

typedef struct _display_snapshot *display_snapshot;

struct _display_snapshot {
    Pixmap pix;
    display_snapshot next;
};

/* Now do the widget definitions */
typedef struct {
    GC  dp_GC;
    XFontStruct *dp_default_font;
    XFontStruct **dp_fonts;
    int dp_num_fonts;
    Pixel dp_foreground;
    Pixel dp_background;
    Pixel dp_color;
    display_cmd dp_buffer;
    display_cmd dp_current;
    int dp_no_cmds;
    int dp_moved;
    int dp_curx;
    int dp_cury;
    int dp_thickness;
    int dp_pattern;
    display_snapshot dp_head;
    display_snapshot dp_tail;
    Pixmap dp_pix;
    Pixmap dp_clearpix;
    int dp_bpl;
    /* */
    unsigned char *dp_data;
    unsigned char *dp_output;
    XtCallbackList dp_kcallback; /* For keyboard events */
    XtCallbackList dp_bcallback; /* For button events */
    XtCallbackList dp_brcallback; /* For button release events */
    XtCallbackList dp_mcallback; /* For motion events */
    XtCallbackList dp_rcallback; /* For resize events */
    Colormap dp_colormap;	 /* Color map for this widget */
    Visual   *dp_visual;	 /* temporary  */
    XImage   *dp_image;		 /* mainly for speed */
    int dp_colors[256];		 /* map 8bits to X color values */
    char *dp_psfamily;		 /* font family to use for postscript dumps */
    int dp_colorscale;		 /* type of color ramp used */
    int dp_inhibit;		 /* have we prohibited display */
    int dp_width;
    int dp_height;
} DisplayPart;

typedef struct _DisplayRec {
    CorePart core;
    DisplayPart display;
} DisplayRec;

void display_key_callback();
void display_button_callback();
void display_button_rcallback();
void display_mouse_callback();

#endif
