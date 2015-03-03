/*
 * Copyright (C) 1994, 1995, 1996
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

#include <X11/Intrinsic.h>     /* X toolkit intrinsics definitions */
#include <X11/StringDefs.h>    /* Standard Name-String definitions */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "defaults.h"
#include "Miscellaneous.h"
#include "Edit.h"
#include "Help.h"

static char html_viewer[FILE_NAME_LEN];
static char html_text[FILE_NAME_LEN];

/*-----------------------------------------------------------------*/

void
InitializeHelp()
{
   strcpy(html_viewer, HELP_DEF_BROWSER);
   strcpy(html_text, INITIAL_HTML_FILE);
}

/*-----------------------------------------------------------------*/

void
StartHelp()
{
   char cmd[FILE_NAME_LEN];
   char *ep;

   ep = getenv(HELP_ENV_PATH);
   if ( ep == NULL ) 
     sprintf(cmd, "%s %s/%s &", html_viewer, HELP_DEF_PATH, html_text);
   else
     sprintf(cmd, "%s %s/%s &", html_viewer, ep, html_text);

   system(cmd);
}

/*-----------------------------------------------------------------*/

void
htmlbrowser_( char *browser, int len )
{
   char msg[80];
   char tmp[80];

   strncpy(tmp, browser, len);
   StrForToC(tmp, len);
   if ( strlen(tmp) == 0 )
     {
       sprintf(msg, " Current HTML browser :  %s\n", html_viewer);
       WriteStr(msg);
     }
   else
     strcpy(html_viewer, tmp);
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * ----------------
 * $Log: Help.c,v $
 * Revision 1.13  1996/05/12 03:37:49  ulf
 * completely changed help system. Xgremlin now uses an external HTML browser
 * to display online documentation.
 *
 * Revision 1.12  1996/03/13  17:02:19  ulf
 * cosmetic fix (put two newlines on top of info text
 *
 * Revision 1.11  1996/03/03  15:21:17  ulf
 * new help startup text with warranty disclaimer and mention
 * of the GNU public license
 *
 * Revision 1.10  1995/10/25  11:49:02  ulf
 * check that help file and help index file have really been opened (this
 * gives an error message now instead of a core dump).
 *
 * Revision 1.9  1995/10/10  14:31:02  ulf
 * added 'Back' and 'Forward' buttons the help system to go to previously
 * visited help pages.
 *
 * Revision 1.8  1995/09/13  01:45:34  ulf
 * use external 'display' variable
 *
 * Revision 1.7  1995/09/12  02:32:18  ulf
 * moved help file indexing out into a seperate program to shave some time
 * off the startup time.
 *
 * Revision 1.6  1995/08/19  19:26:48  ulf
 * move help button to 'Gremlins' menu to make room for the 'Phase' button
 *
 * Revision 1.5  1995/08/03  03:37:10  ulf
 * do not save help button widget locally and remove passing of help widget
 * button in function  InitializeHelp
 *
 * Revision 1.4  1995/08/03  03:25:58  ulf
 * remove StartHelp  callback function and export  OpenHelpWindow.
 *
 * Revision 1.3  1995/07/31  23:16:02  ulf
 * changed  port -->  version
 *
 * Revision 1.2  1995/06/24  19:32:38  ulf
 * Extra button to close help window.
 *
 * Revision 1.1  1995/06/14  03:35:23  ulf
 * Initial revision
 *
 */
