#if defined(__STDC__) || defined(__cplusplus)
#define P_(s) s
#else
#define P_(s) ()
#endif

/* Display.c */
void display_clear_out_buffer P_((Widget w));
void display_redraw P_((Widget w));
void display_draw_from P_((Widget w, display_cmd d));
void display_key_callback P_((Widget w, XKeyEvent *event, String *params, unsigned long *no));
void display_button_callback P_((Widget w, XEvent *e, String *params, unsigned long *no));
void display_button_rcallback P_((Widget w, XEvent *e, String *params, unsigned long *no));
void display_mouse_callback P_((Widget w, XEvent *e, String *params, unsigned long *no));
display_cmd display_new_cmd P_((void));
int display_add_cmd P_((Widget w, display_cmd d));
void display_clear P_((Widget w));
display_cmd display_draw_line2 P_((Widget w, int xfrom, int yfrom, int xto, int yto, int thickness, int pattern, int color));
display_cmd display_draw_line_simple P_((Widget w, int xfrom, int yfrom, int xto, int yto));
display_cmd display_draw_line_color P_((Widget w, int xfrom, int yfrom, int xto, int yto, int color));
void display_touch P_((Widget w));
void display_moveto P_((Widget w, int x, int y));
Pixel display_set_color P_((Widget w, char *name));
Pixel display_get_color P_((Widget w));
void display_set_color_value P_((Widget w, int val));
int display_set_color_RGB P_((Widget w, int r, int g, int b));
void display_set_linestyle P_((Widget w, int thickness, int pattern));
void display_get_linestyle P_((Widget w, int *thickness, int *pattern));
void display_erase_xorline P_((Widget w, display_cmd d));
display_cmd display_draw_xorline P_((Widget w, int xfrom, int yfrom, int xto, int yto, int color));
display_cmd display_draw_line P_((Widget w, int xto, int yto));
void display_line_cmd P_((Widget w, display_cmd d, int op));
Font display_get_font P_((Widget w, int font));
int display_get_font_idx P_((Widget w, Font font));
int display_font_height P_((Widget w, int font));
int display_font_width P_((Widget w, int font, char *str));
int display_load_font P_((Widget w, char *name));
int display_add_font P_((Widget w, XFontStruct *f));
void display_erase_xortext P_((Widget w, display_cmd d));
display_cmd display_draw_xortext P_((Widget w, char *str, int x, int y, int font, int color));
display_cmd display_draw_text P_((Widget w, char *str, int x, int y, int font, int color));
void display_string P_((Widget w, char *str, int x, int y, int font, int color));
display_cmd display_draw_rectangle P_((Widget w, int x, int y, int width, int height, int color));
display_cmd display_fill_rectangle P_((Widget w, int x, int y, int width, int height, int color));
void display_rect_cmd P_((Widget w, display_cmd d, int op));
display_cmd display_draw_arc P_((Widget w, int x, int y, int width, int height, int angle1, int angle2, int color));
display_cmd display_fill_arc P_((Widget w, int x, int y, int width, int height, int angle1, int angle2, int color));
display_cmd display_draw_circle P_((Widget w, int x, int y, int radius, int color));
display_cmd display_draw_ellipse P_((Widget w, int x, int y, int xradius, int yradius, int color));
display_cmd display_fill_ellipse P_((Widget w, int x, int y, int xradius, int yradius, int color));
display_cmd display_fill_circle P_((Widget w, int x, int y, int radius, int color));
void display_text_cmd P_((Widget w, display_cmd d, int op));
display_cmd display_draw_vtext P_((Widget w, char *str, int x, int y, int font, int color, int offset));
void display_vtext_cmd P_((Widget w, display_cmd d, int op));
display_cmd display_draw_point P_((Widget w, int x, int y, int color));
void display_point_cmd P_((Widget w, display_cmd d, int op));
void display_inhibit P_((Widget w));
void display_permit P_((Widget w));
unsigned char *display_get_bufferptr P_((Widget w, int *height, int *width));
unsigned char *display_get_out_bufferptr P_((Widget w, int *height, int *width, int *bpl));
void display_set_buffer P_((Widget w, int x, int y, int value));
int display_swapbuffers P_((Widget w));
void display_erase_all P_((Widget w));
int display_erase P_((Widget w, display_cmd dcmd));
int display_dither P_((Widget w));
Colormap display_get_colormap P_((Widget w));
int display_grayscale P_((Widget w));
int display_userscale P_((Widget w));
int display_getRGB P_((Widget w, long pixel, double *r, double *g, double *b));
int display_hsbscale P_((Widget w));
void hsb_to_rgb P_((double h, double s, double v, double *r, double *g, double *b));
display_cmd display_begin_fill_polygon P_((Widget w));
display_cmd display_begin_polygon P_((Widget w));
void display_polygon_add P_((Widget w, int xto, int yto));
void display_end_polygon P_((Widget w));
void display_flush P_((Widget w));

/*
 * if objects are printable, all further drawing commands will be marked
 * 'printable' in the display list and will be dumped to PS if the whole list
 * is dumped. objects not marked 'printable' will not be included in a
 * PS dump. ( the default is printable==1 ).
 */
void set_objects_printable P_(( int )); 

#undef P_
