/*
 * Gillian Nave, Gaithersburg MD 20878, U.S.A.
 */
 
/*
 * $Id: Hfs.h,v 1.1 1995/09/22 23:03:59 ulf Exp $
 */

/*
 * a GUI interface to hyperfine structure fitting
 */


/*
 * initialize hfs fitting module
 */
void
InitializeHfs( Widget button );


/*
 * open the window with the GUI to phase correction
 */
void
OpenHfsWindow();

/*
 * additional actions
 */

/*
 * used to give the focus to the text input windows
 */
void
hfsFocus( Widget w, XEvent *ev, String *par, Cardinal *npar );

 
/*
 * bound to return key, gives focus back to shell widget
 */
void
hfsAccept( Widget w, XEvent *ev, String *par, Cardinal *npar );


/* Revision history:
 * -----------------
 * $Log: Phase.h,v $
 * Revision 1.1  1995/09/22  23:03:59  ulf
 * Initial revision
 *
 */

