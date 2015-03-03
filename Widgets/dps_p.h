#if defined(__STDC__) || defined(__cplusplus)
#define P_(s) s
#else
#define P_(s) ()
#endif

#include <stdio.h>

/* dps.c */
int display_postscript P_((Widget w, char *fname, double width_in_inches, double height_in_inches, int landscape));

void display_pscommands P_((Widget w, FILE *fp, double xorigin, double yorigin, double width, double height, double rotation));

#undef P_
