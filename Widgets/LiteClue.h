/* 
LiteClue.h - Public definitions for LiteClue widget
	See LiteClue documentation

Copyright 1996 COMPUTER GENERATION, INC.,

The software is provided "as is", without warranty of any kind, express
or implied, including but not limited to the warranties of
merchantability, fitness for a particular purpose and noninfringement.
In no event shall Computer Generation, inc. nor the author be liable for
any claim, damages or other liability, whether in an action of contract,
tort or otherwise, arising from, out of or in connection with the
software or the use or other dealings in the software.

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation.

Author:
Gary Aviv 
Computer Generation, Inc.,
gary@compgen.com
*/
/* Revision History:
$Log: LiteClue.h,v $
Revision 1.2  1996/10/20 13:39:25  gary
Version 1.2 freeze

Revision 1.1  1996/10/19 16:08:04  gary
Initial


$log
Version 1.3 freeze
$log
*/

#ifndef _DEF_LiteClue_h
#define _DEF_LiteClue_h
#include <X11/StringDefs.h>

/*
 * New resource names
 */

#define XgcNcancelWaitPeriod	 "cancelWaitPeriod"
#define XgcNwaitPeriod	 "waitPeriod"
/*
 * New resource classes
 */
#define XgcCCancelWaitPeriod	"cancelWaitPeriod"
#define XgcCWaitPeriod	"WaitPeriod"

extern WidgetClass xcgLiteClueWidgetClass; 
typedef struct _LiteClueClassRec *XcgLiteClueWidgetClass;
typedef struct _LiteClueRec      *XcgLiteClueWidget;
void XcgLiteClueAddWidget(Widget w, Widget watch,  char * text, int size, int option );
void XcgLiteClueDeleteWidget(Widget w, Widget watch);
void XcgLiteClueSetSensitive(Widget w, Widget watch, Boolean sensitive);
Boolean XcgLiteClueGetSensitive(Widget w, Widget watch);
#endif
