/*
 * Gillian Nave, Gaithersburg MD 20877, U.S.A.
 */
 
/*
 * This program is part of Xgremlin / GUI interface to hyperfine structure fitting.
 */

/*
 * Bug fix 12th Feb,2013 - changed dimension of linefile_str and ascfile_str to FILE_NAME_LEN
 */

#include <X11/Intrinsic.h>     /* X toolkit intrinsics definitions */
#include <X11/StringDefs.h>    /* Standard Name-String definitions */
#include <X11/Shell.h>

#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>

#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <setjmp.h>

#include "Version.h"
#include "defaults.h"
#include "FortranWrappers.h"
#include "Miscellaneous.h"
#include "Edit.h"
#include "Hfs.h"
#include "PlotMode.h"
#include "regexpr.h"

#include "icons/Xgremlin_hfs.icon.xbm"
#include "icons/count_up.xbm"
#include "icons/count_down.xbm"

#define NUM_LEN        32
#define OTHER_LEN      256
#define MAX_CMD_LEN    256
#define MAX_NUM_STRUCT 3

#define HFS_WINDOW_WIDTH  500
#define HFS_WINDOW_HEIGHT 450

/*
 * regular expression that describes a well formed number or blank space
 */

#define RE_NUMBER    " *|( *[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eEdD][-+]?[0-9]+)?)" 

/*
 * global variables
 */

extern Display *display;               /* our display */
extern Widget gremlinTop;              /* top level widget */

static Widget hfsShell,              /* hfs correction window */
              hfsForm,

              levfileLabel,
              levfileText,
              levfileButton,

              readlinefileLabel,
              readlinefileText,
              readlinefileButton,

              numStructLabel,
              numStructText,
              numStructUpButton,
              numStructDownButton,
  
              thisStructLabel,
              thisStructText,
              thisStructUpButton,
              thisStructDownButton,

              thislineLabel,
              thislineText,
              thislineButton,

              lowlevLabel,
              lowlevText,
              upplevLabel,
              upplevText,
              lowJLabel,
              lowJText,
              uppJLabel,
              uppJText,
  
              lowALabel,
              lowAText,
              lowAToggleButton,
              lowBLabel,
              lowBText,
              lowBToggleButton,
              uppALabel,
              uppAText,
              uppAToggleButton,
              uppBLabel,
              uppBText,
              uppBToggleButton,

              waveLabel,
              waveText,
              waveToggleButton,
              getinfoButton,
              deletelineButton,

              intenLabel,
              intenText,
              intenToggleButton,
              dampLabel,
              dampText,
              dampToggleButton,
              fwhmLabel,
              fwhmText,
              fwhmToggleButton,
              
              commentLabel,
              commentText,
              updateButton,

              linefileLabel,
              linefileText,
              linefileButton,
              ascfileLabel,
              ascfileText,
              ascfileButton,

              buttonBox,
              fitButton,
              plotButton,
              compButton,
              closeButton;

static Widget hfsMenuButton;       /* button in 'Gremlins' menu */
static Widget hfs_last_widget;

static Pixmap count_up, count_down;


/*
 * integer flags
 */
static int alhold = FALSE;          /* release lower A */
static int blhold = FALSE;          /* release lower B */
static int auhold = FALSE;          /* release upper A */
static int buhold = FALSE;          /* release upper B */
static int wavehold = FALSE;        /* release wavenumber */
static int intenhold = FALSE;       /* release intensity */
static int damphold = FALSE;        /* release damping */
static int fwhmhold = FALSE;        /* release width */
static int alllines[MAX_NUM_STRUCT];    /* line numbers in internal list */

/*
 * strings with values
 */
static char levfile_str[FILE_NAME_LEN];
static char readlinefile_str[FILE_NAME_LEN];
static char thisline_str[NUM_LEN];
static char numstruct_str[NUM_LEN];
static char thisstruct_str[NUM_LEN];
static char lowlev_str[NUM_LEN];
static char upplev_str[NUM_LEN];
static char lowj_str[NUM_LEN];
static char uppj_str[NUM_LEN];
static char lowA_str[NUM_LEN];
static char lowB_str[NUM_LEN];
static char uppA_str[NUM_LEN];
static char uppB_str[NUM_LEN];
static char wave_str[NUM_LEN];
static char inten_str[NUM_LEN];
static char damp_str[NUM_LEN];
static char fwhm_str[NUM_LEN];
static char comment_str[OTHER_LEN];
static char linefile_str[FILE_NAME_LEN];
static char ascfile_str[FILE_NAME_LEN];

/*
 * status buffer used to store the stack contents prior to dispatching a command
 * The status information is used by the signal handlers to return from a
 * broken command.
 */
extern jmp_buf proc_status;

/*
 * for sanity check of input
 */
static struct re_pattern_buffer rec_number;
static struct re_registers regs;

/*------------------  Local prototypes  --------------------------*/ 

static void
readCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
readlineCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
numStructUpCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
numStructDownCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
thisStructUpCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
thisStructDownCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
lowAToggleCB(Widget W, XtPointer client_data, XtPointer call_data);

static void
uppAToggleCB(Widget W, XtPointer client_data, XtPointer call_data);

static void
lowBToggleCB(Widget W, XtPointer client_data, XtPointer call_data);

static void
uppBToggleCB(Widget W, XtPointer client_data, XtPointer call_data);

static void
waveToggleCB(Widget W, XtPointer client_data, XtPointer call_data);

static void
getinfoCB(Widget W, XtPointer client_data, XtPointer call_data);

static void
deleteCB(Widget W, XtPointer client_data, XtPointer call_data);

static void
intenToggleCB(Widget W, XtPointer client_data, XtPointer call_data);

static void
dampToggleCB(Widget W, XtPointer client_data, XtPointer call_data);

static void
fwhmToggleCB(Widget W, XtPointer client_data, XtPointer call_data);

static void
updateCB(Widget W, XtPointer client_data, XtPointer call_data);

static void
saveLinfileCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
saveAscfileCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
fitCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
plotCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
compCB(Widget w, XtPointer client_data, XtPointer call_data);

static void
CloseHfsWindow(Widget w, XtPointer client_data, XtPointer call_data);

/*
 * checks if a character array contains more than ' ' or \0
 */
static int
string_contains_char( char s[], int len );

/*
 * returns the true string length ignoring trailing blanks
 */
static int
truestrlen( char s[] );

/*
 * updates the text in a text widget
 */
static void
hfs_update_text( Widget w, char s[], int len );

/*
 * fills in all the info for a line, given its number
 */
static void
getinfo ( int thisline);
/*
 * sends all the info on the form to the linelist
 */

void
sendinfo( int thisline);

/*------------------  Callback functions for buttons  ------------------*/ 

static void
readCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  char command[MAX_CMD_LEN] ;
  
  strcpy (command, "hfs open hfslev ");
  strcat (command, levfile_str);
  StrCToFor( command, MAX_CMD_LEN);

  if ( setjmp(proc_status) == 0 )
    runcmd_( command, MAX_CMD_LEN );
  else
    WriteStr( "\n Warning :  Can't read level file\n\n" ); 

}


/*-------------------------------------------------------------*/ 

static void
readlineCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  char command[MAX_CMD_LEN] ;
  
  strcpy (command, "hfs open hfsline ");
  strcat (command, readlinefile_str);
  StrCToFor( command, MAX_CMD_LEN);
  if ( setjmp(proc_status) == 0 )
    runcmd_( command, MAX_CMD_LEN );
  else
    WriteStr( "\n Warning :  Can't open line file\n\n" ); 

  strcpy (command, "hfs get");
  StrCToFor( command, MAX_CMD_LEN);
  if ( setjmp(proc_status) == 0 )
    runcmd_( command, MAX_CMD_LEN );
  else
    WriteStr( "\n Warning :  Can't read line file\n\n" ); 

  strcpy (command, "hfs close hfsline ");
  StrCToFor( command, MAX_CMD_LEN);
  if ( setjmp(proc_status) == 0 )
    runcmd_( command, MAX_CMD_LEN );
  else
    WriteStr( "\n Warning :  Can't close line file\n\n" ); 
}

/*-------------------------------------------------------------*/ 

static void
numStructUpCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  int num;
  char numstr[NUM_LEN];

  sscanf( numstruct_str, "%d", &num);
  num++;
  if (num > MAX_NUM_STRUCT)
    num = MAX_NUM_STRUCT;
  sprintf(numstr, "%d", num);
  hfs_update_text( numStructText, numstr, NUM_LEN);
}

/*-------------------------------------------------------------*/ 

static void
numStructDownCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  int num;
  char numstr[NUM_LEN];

  sscanf( numstruct_str, "%d", &num);
  num--;
  if ( num < 1 )
    num = 1;
  sprintf(numstr, "%d", num);
  hfs_update_text( numStructText, numstr, NUM_LEN);
}
/*-------------------------------------------------------------*/ 

static void
thisStructUpCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  int num, numstruct, thisline;
  char numstr[NUM_LEN];

  sscanf( thisstruct_str, "%d", &num);
  sscanf( numstruct_str, "%d", &numstruct);
  num++;
  if (num > numstruct)
    num = numstruct;
  sprintf( numstr, "%d", num);
  hfs_update_text( thisStructText, numstr, NUM_LEN);
  thisline = alllines[num-1];
  getinfo( thisline);
}
/*-------------------------------------------------------------*/ 

static void
thisStructDownCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  int num, thisline;
  char numstr[NUM_LEN];

  sscanf( thisstruct_str, "%d", &num);
  num--;
  if ( num < 1 )
    num = 1;
  sprintf( numstr, "%d", num);
  hfs_update_text( thisStructText, numstr, NUM_LEN);
  thisline = alllines[num-1];
  getinfo( thisline);
}
/*-------------------------------------------------------------*/ 

static void
lowAToggleCB(Widget W, XtPointer client_data, XtPointer call_data)
{
  if (alhold == FALSE)
    alhold = TRUE;
  else
    alhold = FALSE;
}
/*-------------------------------------------------------------*/ 

static void
uppAToggleCB(Widget W, XtPointer client_data, XtPointer call_data)
{
  if (auhold == FALSE)
    auhold = TRUE;
  else
    auhold = FALSE;
}
/*-------------------------------------------------------------*/ 

static void
lowBToggleCB(Widget W, XtPointer client_data, XtPointer call_data)
{
  if (blhold == FALSE)
    blhold = TRUE;
  else
    blhold = FALSE;
}
/*-------------------------------------------------------------*/ 

static void
uppBToggleCB(Widget W, XtPointer client_data, XtPointer call_data)
{
  if (buhold == FALSE)
    buhold = TRUE;
  else
    buhold = FALSE;
}
/*-------------------------------------------------------------*/ 

static void
waveToggleCB(Widget W, XtPointer client_data, XtPointer call_data)
{
  if ( wavehold == FALSE)
    wavehold = TRUE;
  else
    wavehold = FALSE;
}
/*-------------------------------------------------------------*/ 

static void
getinfoCB(Widget W, XtPointer client_data, XtPointer call_data)
{

  char command[MAX_CMD_LEN];
  int thisline;

  /* Fill in rest of data */
  sscanf( thisline_str, "%d", &thisline);
  getinfo (thisline);

  /* Replot centred on line */

  strcpy (command, "goto ");
  strcat (command, wave_str);
  StrCToFor( command, MAX_CMD_LEN);

  if ( setjmp(proc_status) == 0 )
    runcmd_( command, MAX_CMD_LEN );
  else
    WriteStr( "\n Warning :  Can't go to line \n\n" ); 

}

/*-------------------------------------------------------------*/ 

static void
deleteCB(Widget W, XtPointer client_data, XtPointer call_data)
{
  char command[MAX_CMD_LEN] ;
  int thisline;
  
  /* First insert Line into linelist */
  strcpy (command, "hfs delete  ");
  strcat (command, thisline_str );
  StrCToFor( command, MAX_CMD_LEN);

  if ( setjmp(proc_status) == 0 )
    runcmd_( command, MAX_CMD_LEN );
  else
    WriteStr( "\n Warning :  Can't insert line \n\n" ); 

  sscanf( thisline_str, "%d", &thisline);
  thisline++;
  sprintf( thisline_str, "%d", thisline);

  /* Fill in rest of data */
  getinfo ( thisline );

  /* Replot centred on line */

  strcpy (command, "goto ");
  strcat (command, wave_str);
  StrCToFor( command, MAX_CMD_LEN);

  if ( setjmp(proc_status) == 0 )
    runcmd_( command, MAX_CMD_LEN );
  else
    WriteStr( "\n Warning :  Can't go to line \n\n" ); 
  }

/*-------------------------------------------------------------*/ 

static void
insertCB(Widget W, XtPointer client_data, XtPointer call_data)
{
  char command[MAX_CMD_LEN] ;
  double waveno ;
  int thisline;
  
  /* First insert Line into linelist */
  strcpy (command, "hfs insert  ");
  strcat (command, wave_str);
  strcat (command, " ");
  strcat (command, inten_str);
  
  StrCToFor( command, MAX_CMD_LEN);

  if ( setjmp(proc_status) == 0 )
    runcmd_( command, MAX_CMD_LEN );
  else
    WriteStr( "\n Warning :  Can't insert line \n\n" ); 

  /* Replot centred on line */

  strcpy (command, "goto ");
  strcat (command, wave_str);
  StrCToFor( command, MAX_CMD_LEN);

  if ( setjmp(proc_status) == 0 )
    runcmd_( command, MAX_CMD_LEN );
  else
    WriteStr( "\n Warning :  Can't go to line \n\n" ); 
  
  /* Find out where it has been inserted */
  
  sscanf( wave_str, "%lf", &waveno);
  findline_( &thisline, &waveno );
  
  /* Fill in rest of data */
    getinfo ( thisline );
}

 /*-------------------------------------------------------------*/ 

static void
intenToggleCB(Widget W, XtPointer client_data, XtPointer call_data)
{
  if (intenhold == FALSE)
    intenhold = TRUE;
  else
    intenhold = FALSE;
}
/*-------------------------------------------------------------*/ 

static void
dampToggleCB(Widget W, XtPointer client_data, XtPointer call_data)
{
  if (damphold == FALSE)
    damphold = TRUE;
  else
    damphold = FALSE;
}
/*-------------------------------------------------------------*/ 

static void
fwhmToggleCB(Widget W, XtPointer client_data, XtPointer call_data)
{
  if (fwhmhold == FALSE)
    fwhmhold = TRUE;
  else
    fwhmhold = FALSE;

}

/*-------------------------------------------------------------*/ 

static void
updateCB(Widget W, XtPointer client_data, XtPointer call_data)
{
    int thisline;

  /* Fill in rest of data */
  sscanf( thisline_str, "%d", &thisline);
  sendinfo (thisline);
}


/*-------------------------------------------------------------*/ 

static void
saveLinfileCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  char command[MAX_CMD_LEN] ;
  
  strcpy (command, "hfs open hfsline ");
  strcat (command, linefile_str);
  strcat (command, ";hfs save;hfs close hfsline ");
  strcat (command, linefile_str);
  StrCToFor( command, MAX_CMD_LEN);

  if ( setjmp(proc_status) == 0 )
    runcmd_( command, MAX_CMD_LEN );
  else
    WriteStr( "\n Warning :  Can't save line file\n\n" ); 
}
/*-------------------------------------------------------------*/ 

static void
saveAscfileCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  char command[MAX_CMD_LEN] ;
  
  strcpy (command, "hfs write ");
  strcat (command, ascfile_str);
  StrCToFor( command, MAX_CMD_LEN);

  if ( setjmp(proc_status) == 0 )
    runcmd_( command, MAX_CMD_LEN );
  else
    WriteStr( "\n Warning :  Can't write ASCII file\n\n" ); 
}
/*-------------------------------------------------------------*/ 

static void
fitCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  int thisline, num_struct, i, this_struct;
  char command[MAX_CMD_LEN], dummy[NUM_LEN];

  strcpy (command, "hfs fit ");

  /* Loop over all the structures */

  sscanf(numstruct_str, "%d", &num_struct);
  sscanf(thisstruct_str, "%d", &this_struct);
  for ( i=0;i<= num_struct-1;i++) {
    thisline = alllines[i];
    if (i==this_struct-1) sendinfo(alllines[i]);
    sprintf(dummy," %d ", thisline);
    strcat (command, dummy);
    strcat (command, " ");
  }
  StrCToFor( command, MAX_CMD_LEN);

  if ( setjmp(proc_status) == 0 )
    runcmd_( command, MAX_CMD_LEN );
  else
    WriteStr( "\n Warning :  Can't fit lines\n\n" ); 

  getinfo(alllines[this_struct-1]);   /* update form */
}
/*-------------------------------------------------------------*/ 

static void
plotCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  int thisline, num_struct, i, this_struct;
  char command[MAX_CMD_LEN], dummy[NUM_LEN];

  strcpy (command, "hfs plot ");

  /* Loop over all the structures */

  sscanf(numstruct_str, "%d", &num_struct);
  sscanf(thisstruct_str, "%d", &this_struct);
  for ( i=0;i<= num_struct-1;i++) {
    thisline = alllines[i];
    if (i==this_struct-1) sendinfo(alllines[i]);
    sprintf(dummy," %d ", thisline);
    strcat (command, dummy);
    strcat (command, " ");
  }
  StrCToFor( command, MAX_CMD_LEN);

  if ( setjmp(proc_status) == 0 )
    runcmd_( command, MAX_CMD_LEN );
  else
    WriteStr( "\n Warning :  Can't plot lines\n\n" ); 
}
/*-------------------------------------------------------------*/ 

static void
compCB(Widget w, XtPointer client_data, XtPointer call_data)
{
  int thisline, num_struct, i, this_struct;
  char command[MAX_CMD_LEN], dummy[NUM_LEN];

  strcpy (command, "hfs plot ");

  /* Loop over all the structures */

  sscanf(numstruct_str, "%d", &num_struct);
  sscanf(thisstruct_str, "%d", &this_struct);
  for ( i=0;i<= num_struct-1;i++) {
    thisline = alllines[i];
    if (i==this_struct-1) sendinfo(alllines[i]);
    sprintf(dummy," %d ", thisline);
    strcat (command, dummy);
    strcat (command, " ");
  }
  strcat (command ," comp ");
  StrCToFor( command, MAX_CMD_LEN);

  if ( setjmp(proc_status) == 0 )
    runcmd_( command, MAX_CMD_LEN );
  else
    WriteStr( "\n Warning :  Can't plot lines\n\n" ); 
}
/*-------------------------------------------------------------*/ 

static void
CloseHfsWindow(Widget w, XtPointer client_data, XtPointer call_data)
{
   Arg args[1];

   /*
    * make help button sensitive again
    */
   XtSetArg( args[0], XtNsensitive, True);
   XtSetValues( hfsMenuButton, args, 1);

   /*
    * destroy help window
    */
   XtDestroyWidget( hfsShell );
}

/*-------------------------------------------------------------*/ 

static int
string_contains_char( char s[], int len )
{
   int k;

   for ( k=0; k<len; k++ )
     {
        if ( s[k] != ' ' && s[k] != '\0' )
          return TRUE;
     }
   return FALSE;
}

/*-------------------------------------------------------------*/ 

static int
truestrlen( char s[] )
{
   int len;
   int k;
   
   len = strlen(s);
   for ( k=len-1; k>=0; k-- )
     if ( s[k] != ' ')
       break;
   if ( k == 0 && s[k] == ' ' )
     s[0] = '\0';
   else
     s[k+1] = '\0';
   return strlen(s);
}

/*-----------------------------------------------------------------*/

static void
hfs_update_text( Widget w, char s[], int len )
{
   XawTextBlock line;

   line.firstPos = 0;
   line.length   = strlen(s);
   line.ptr      = s;
   line.format   = FMT8BIT;
   XawTextReplace( w, 0, len, &line );
}

/*-----------------------------------------------------------------*/

static void
getinfo ( int thisline)
{
  double lowj, uppj, lowA, uppA, lowB, uppB, wave, snr, damp, fwhm;
  char lowlev[NUM_LEN], upplev[NUM_LEN], comment[OTHER_LEN];
/*
 *  Change to normal int (GN, March 2015)
 *  long int alh, blh, auh, buh, waveh, intenh, damph, fwhmh; /* held params */ 
   int alh, blh, auh, buh, waveh, intenh, damph, fwhmh; /* held params */
  int thisstruct;

  Arg args[1];

  strcpy (lowlev,"        ");
  strcpy (upplev,"        ");
  strcpy (comment,"        ");
  StrCToFor (lowlev, strlen(lowlev));   /* Convert strings to Fortran style */
  StrCToFor (upplev, strlen(upplev));
  StrCToFor (comment, strlen(comment));

  getinfo_( &thisline, &lowj, &uppj, &lowA, &uppA, &lowB, &uppB, &wave, &snr, &damp, &fwhm, &alh, &blh, &auh, &buh, &waveh, &intenh, &damph, &fwhmh, lowlev, upplev, comment, NUM_LEN, NUM_LEN, OTHER_LEN);

  /*
   * where does it go ?
   */

  sscanf(thisstruct_str, "%d", &thisstruct);
  alllines[ thisstruct-1 ] = thisline;

  sprintf(lowj_str, "%g", lowj);
  sprintf(uppj_str, "%g", uppj);
  sprintf(lowA_str, "%g", lowA);
  sprintf(uppA_str, "%g", uppA);
  sprintf(lowB_str, "%g", lowB);
  sprintf(uppB_str, "%g", uppB);
  sprintf(wave_str, "%10.4f", wave);
  sprintf(inten_str, "%g", snr);
  sprintf(damp_str, "%g", damp);
  sprintf(fwhm_str, "%g", fwhm);
  sprintf(thisline_str, "%d", thisline);

  /* set the hold/release buttons */

  if (alh == 1) {
    XtSetArg(args[0], XtNstate, True);
    XtSetValues( lowAToggleButton, args, 1);
    alhold = TRUE;
  }
  else {
    XtSetArg(args[0], XtNstate, False);
    XtSetValues( lowAToggleButton, args, 1);
    alhold = FALSE;
  }

  if (blh == 1) {
    XtSetArg(args[0], XtNstate, True);
    XtSetValues( lowBToggleButton, args, 1);
    blhold = TRUE;
  }
  else {
    XtSetArg(args[0], XtNstate, False);
    XtSetValues( lowBToggleButton, args, 1);
    blhold = FALSE;
  }

  if (auh == 1) {
    XtSetArg(args[0], XtNstate, True);
    XtSetValues( uppAToggleButton, args, 1);
    auhold = TRUE;
  }
  else {
    XtSetArg(args[0], XtNstate, False);
    XtSetValues( uppAToggleButton, args, 1);
    auhold = FALSE;
  }
  if (buh == 1) {
    XtSetArg(args[0], XtNstate, True);
    XtSetValues( uppBToggleButton, args, 1);
    buhold = TRUE;
  }
  else {
    XtSetArg(args[0], XtNstate, False);
    XtSetValues( uppBToggleButton, args, 1);
    buhold = FALSE;
  }

  if (waveh == 1) {
    XtSetArg(args[0], XtNstate, True);
    XtSetValues( waveToggleButton, args, 1);
    wavehold = TRUE;
  }
  else {
    XtSetArg(args[0], XtNstate, False);
    XtSetValues( waveToggleButton, args, 1);
    wavehold = FALSE;
  }

  if (intenh == 1) {
    XtSetArg(args[0], XtNstate, True);
    XtSetValues( intenToggleButton, args, 1);
    intenhold = TRUE;
  }
  else {
    XtSetArg(args[0], XtNstate, False);
    XtSetValues( intenToggleButton, args, 1);
    intenhold = FALSE;
  }

  if (damph == 1) {
    XtSetArg(args[0], XtNstate, True);
    XtSetValues( dampToggleButton, args, 1);
    damphold = TRUE;
  }
  else {
    XtSetArg(args[0], XtNstate, False);
    XtSetValues( dampToggleButton, args, 1);
    damphold = FALSE;
  }

  if (fwhmh == 1) {
    XtSetArg(args[0], XtNstate, True);
    XtSetValues( fwhmToggleButton, args, 1);
    fwhmhold = TRUE;
  }
  else {
    XtSetArg(args[0], XtNstate, False);
    XtSetValues( fwhmToggleButton, args, 1);
    fwhmhold = FALSE;
  }

  StrForToC( lowlev, NUM_LEN);      /* Convert strings back to C style */
  StrForToC( upplev, NUM_LEN);
  StrForToC( comment, OTHER_LEN);
  strcpy (lowlev_str, lowlev);
  strcpy (upplev_str, upplev);
  strcpy (comment_str, comment);

  /* and update the text */
  hfs_update_text( thislineText, thisline_str, NUM_LEN);
  hfs_update_text( lowlevText, lowlev_str, NUM_LEN );
  hfs_update_text( upplevText, upplev_str, NUM_LEN );
  hfs_update_text( lowJText, lowj_str, NUM_LEN );
  hfs_update_text( uppJText, uppj_str, NUM_LEN );
  hfs_update_text( lowAText, lowA_str, NUM_LEN );
  hfs_update_text( uppAText, uppA_str, NUM_LEN );
  hfs_update_text( lowBText, lowB_str, NUM_LEN );
  hfs_update_text( uppBText, uppB_str, NUM_LEN );
  hfs_update_text( waveText, wave_str, NUM_LEN );
  hfs_update_text( intenText, inten_str, NUM_LEN );
  hfs_update_text( dampText, damp_str, NUM_LEN );
  hfs_update_text( fwhmText, fwhm_str, NUM_LEN );
  hfs_update_text( commentText, comment_str, OTHER_LEN);
}

/*-----------------------------------------------------------------*/
void
sendinfo( int thisline)
{
   FILE *hfs_batch;
   double lowj,uppj;
   char file_name[FILE_NAME_LEN];
   char command[MAX_CMD_LEN];            /* command string */
   double dummy;               /* ensures everything has a decimal point */

  /* 
   * Check numeric parameters
   */
   /*  Why doesn't this work ?
   if ( re_match(&rec_number,lowj_str,strlen(lowj_str),0,&regs) < 0 ||
        re_match(&rec_number,uppj_str,strlen(uppj_str),0,&regs) < 0 ||
        re_match(&rec_number,lowA_str,strlen(lowA_str),0,&regs) < 0 ||
        re_match(&rec_number,uppA_str,strlen(uppA_str),0,&regs) < 0 ||
        re_match(&rec_number,lowB_str,strlen(lowB_str),0,&regs) < 0 ||
        re_match(&rec_number,uppB_str,strlen(uppB_str),0,&regs) < 0 || 
        re_match(&rec_number,wave_str,strlen(wave_str),0,&regs) < 0 ||
        re_match(&rec_number,inten_str,strlen(inten_str),0,&regs) < 0 ||
        re_match(&rec_number,damp_str,strlen(damp_str),0,&regs) < 0 ||
        re_match(&rec_number,fwhm_str,strlen(fwhm_str),0,&regs) < 0 
	) {
      WriteStr("\n Error :  malformed number in HFS fitting form.");
      return;
   }
   */

   /*
    * create a phase correction batch file and feed it to Xgremlin
    */

   sprintf( file_name, "hfs-%d.batch", (int)getpid() );
   hfs_batch = fopen( file_name, "w" );

   fprintf( hfs_batch, "# HFS fitting batch file created by Xgremlin %s\n#\n", VERSION );

   sscanf( wave_str, "%lf", &dummy);
   fprintf( hfs_batch, "hfs set %d waveno %f\n", thisline, dummy);
   sscanf( inten_str, "%lf", &dummy);
   fprintf( hfs_batch, "hfs set %d snr %f\n",  thisline, dummy);
   sscanf( damp_str, "%lf", &dummy);
   fprintf( hfs_batch, "hfs set %d damp %f\n",  thisline, dummy );
   sscanf( fwhm_str, "%lf", &dummy);
   if(dummy == 0.0) dummy = 30.;   /* ensures something is always seen */
   fprintf( hfs_batch, "hfs set %d fwhm %f\n",  thisline, dummy);

   fprintf( hfs_batch, "hfs set %d el %s\n",  thisline, lowlev_str);
   fprintf( hfs_batch, "hfs set %d eu %s\n",  thisline, upplev_str);

   sscanf( lowA_str, "%lf", &dummy);
   fprintf( hfs_batch, "hfs set %d al %f\n",  thisline, dummy);
   sscanf( lowB_str, "%lf", &dummy);
   fprintf( hfs_batch, "hfs set %d bl %f\n",  thisline, dummy);
   sscanf( uppA_str, "%lf", &dummy);
   fprintf( hfs_batch, "hfs set %d au %f\n",  thisline, dummy);
   sscanf( uppB_str, "%lf", &dummy);
   fprintf( hfs_batch, "hfs set %d bu %f\n",  thisline, dummy);
   fprintf( hfs_batch, "hfs set %d comment '%s' \n", thisline, comment_str);
   
   if ( alhold) fprintf (hfs_batch, "hfs hold %d al\n", thisline );
   else         fprintf (hfs_batch, "hfs release %d al\n", thisline );
   if ( auhold) fprintf (hfs_batch, "hfs hold %d au\n", thisline );
   else         fprintf (hfs_batch, "hfs release %d au\n", thisline );
   if ( blhold) fprintf (hfs_batch, "hfs hold %d bl\n", thisline );
   else         fprintf (hfs_batch, "hfs release %d bl\n", thisline );
   if ( buhold) fprintf (hfs_batch, "hfs hold %d bu\n", thisline );
   else         fprintf (hfs_batch, "hfs release %d bu\n", thisline );

   if ( wavehold) fprintf (hfs_batch, "hfs hold %d waveno\n", thisline );
   else         fprintf (hfs_batch, "hfs release %d waveno\n", thisline );
   if ( intenhold) fprintf (hfs_batch, "hfs hold %d snr\n", thisline );
   else         fprintf (hfs_batch, "hfs release %d snr\n", thisline );
   if ( damphold) fprintf (hfs_batch, "hfs hold %d damp\n", thisline );
   else         fprintf (hfs_batch, "hfs release %d damp\n", thisline );
   if ( fwhmhold) fprintf (hfs_batch, "hfs hold %d fwhm\n", thisline );
   else         fprintf (hfs_batch, "hfs release %d fwhm\n", thisline );

   fprintf (hfs_batch, "keyboard\n");

   fclose( hfs_batch );

   /*
    * put together Xgremlin run command and dispatch it
    */

   strcpy( command, "run " );
   strcat( command, file_name );
   StrCToFor( command, MAX_CMD_LEN );

   if ( setjmp(proc_status) == 0 )
     runcmd_( command, MAX_CMD_LEN );
   else
     WriteStr( "\n Warning :  batch file not processed completely\n\n" ); 

   /*
    * remove the batch file
    */
   unlink( file_name );

   setj_(&thisline,&lowj,&uppj);     /* Get the J values set by en. levels */
   sprintf(lowj_str, "%g", lowj);
   sprintf(uppj_str, "%g", uppj);
   hfs_update_text( lowJText, lowj_str, NUM_LEN );
   hfs_update_text( uppJText, uppj_str, NUM_LEN );

}

/*-----------------------------------------------------------------*/

void
hfsFocus( Widget w, XEvent *ev, String *par, Cardinal *npar )
{
   Arg args[4];
   int num;

   if ( w == levfileText   || w == numStructText  || w == thisStructText ||
        w == lowlevText    || w == upplevText     || w == lowJText       ||
        w == readlinefileText    || w == thislineText ||
        w == uppJText      || w == lowAText       || w == lowBText       ||
        w == uppAText      || w == uppBText       || w == waveText       ||
        w == intenText     || w == dampText       || w == fwhmText       ||
        w == commentText   || w == linefileText   || w == ascfileText )

     {
        if ( hfs_last_widget != NULL )
          {
             num = 0;
             XtSetArg( args[num], XtNdisplayCaret, False ); num++;
             XtSetValues( hfs_last_widget, args, num );
          }

        num = 0;
        XtSetArg( args[num], XtNdisplayCaret, True ); num++;
        XtSetValues( w, args, num );
        XtSetKeyboardFocus( hfsShell, w );
        hfs_last_widget = w;
     }
   else
     XtSetKeyboardFocus( hfsShell, hfsShell );
}
 
/*-------------------------------------------------------------*/ 

void
hfsAccept( Widget w, XEvent *ev, String *par, Cardinal *npar )
{
   Arg args[2];
   int num;

   if ( w == levfileText   || w == numStructText  || w == thisStructText ||
        w == lowlevText    || w == upplevText     || w == lowJText       ||
        w == readlinefileText    || w == thislineText ||
        w == uppJText      || w == lowAText       || w == lowBText       ||
        w == uppAText      || w == uppBText       || w == waveText       ||
        w == intenText     || w == dampText       || w == fwhmText       ||
        w == commentText   || w == linefileText   || w == ascfileText )
     {
        num = 0;
        XtSetArg( args[num], XtNdisplayCaret, False ); num++;
        XtSetValues( w, args, num );
        XtSetKeyboardFocus( hfsShell, hfsShell );
        hfs_last_widget = w;
     }
}

/*-------------------------------------------------------------*/ 


void
InitializeHfs( Widget button )
{
   /*
    * save menu button for later use
    */
   hfsMenuButton = button;

   /*
    * compile regular expression
    */
   rec_number.buffer = NULL;
   rec_number.allocated = 0;
   rec_number.translate = NULL;
   re_set_syntax( RE_NO_BK_PARENS | RE_NO_BK_VBAR );
   if ( re_compile_pattern( RE_NUMBER, strlen(RE_NUMBER), &rec_number ) != NULL ) 
     {
       fprintf( stderr, "Fatal error :  cannot compile reg. exp. in InitializeHfs\n");

       exit( -1 );
   
     } 
}

/*-------------------------------------------------------------*/ 

void
OpenHfsWindow()
{
   Arg args[1];
   Pixmap icon;

   /*
    * preset all string variables with \0
    * That way everything that is typed into the text widgets
    * will always be correctly terminated.
    */
   memset( levfile_str, '\0', FILE_NAME_LEN );
   memset( readlinefile_str, '\0', FILE_NAME_LEN );
   memset( thisline_str, '\0', NUM_LEN);
   memset( numstruct_str, '\0', NUM_LEN );
   memset( thisstruct_str, '\0', NUM_LEN );
   memset( lowlev_str, '\0', NUM_LEN );
   memset( upplev_str, '\0', NUM_LEN );
   memset( lowj_str, '\0', NUM_LEN );
   memset( uppj_str, '\0', NUM_LEN );
   memset( lowA_str, '\0', NUM_LEN );
   memset( lowB_str, '\0', NUM_LEN );
   memset( uppA_str, '\0', NUM_LEN );
   memset( uppB_str, '\0', NUM_LEN );
   memset( wave_str, '\0', NUM_LEN );
   memset( inten_str, '\0', NUM_LEN );
   memset( damp_str, '\0', NUM_LEN );
   memset( fwhm_str, '\0', NUM_LEN );
   memset( comment_str, '\0', OTHER_LEN);
   memset( linefile_str, '\0', FILE_NAME_LEN );
   memset( ascfile_str, '\0', FILE_NAME_LEN );

   /*
    * a few default values
    */

   alhold = FALSE;     
   blhold = FALSE;     
   auhold = FALSE;  
   buhold = FALSE;     
   wavehold = FALSE;     
   intenhold = FALSE;    
   damphold = FALSE;     
   fwhmhold = FALSE;    
   strcpy( numstruct_str , "1");
   strcpy( thisstruct_str , "1");

   hfs_last_widget = NULL;

   /*
    * make menu button insensitive
    */

   XtSetArg( args[0], XtNsensitive, False);
   XtSetValues( hfsMenuButton, args, 1);

   /*
    * create a shell window
    */
   hfsShell = XtVaAppCreateShell(
        NULL,
        "hfsshell",
        topLevelShellWidgetClass,
        display,
        XtNwidth,  HFS_WINDOW_WIDTH,
        XtNheight, HFS_WINDOW_HEIGHT,
        NULL);
 
   /*
    * geometry management for widgets 
    */
   hfsForm = XtVaCreateManagedWidget(
        "hfsform",
        formWidgetClass,
        hfsShell,
        NULL );

   /*
    * file name input field label / text input
    */

   levfileLabel = XtVaCreateManagedWidget(
        "levfilelabel",
        labelWidgetClass,
        hfsForm,
        XtNlabel, "Level file:  ",
        NULL );

   levfileText = XtVaCreateManagedWidget(
        "levfiletext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, levfileLabel,
        XtNstring, &levfile_str,
        XtNlength, FILE_NAME_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   levfileButton =  XtVaCreateManagedWidget(
	"levfilebutton",
        commandWidgetClass,
        hfsForm,
        XtNfromHoriz, levfileText,
        XtNlabel, "read",
        NULL );
   XtAddCallback(levfileButton, XtNcallback, readCB, 0 );

   /* 
    * line file (for reading)
    */

   readlinefileLabel = XtVaCreateManagedWidget(
        "readlinefilelabel",
        labelWidgetClass,
        hfsForm,
	XtNfromVert, levfileText,
        XtNlabel, "Line file:  ",
        NULL );

   readlinefileText = XtVaCreateManagedWidget(
        "readlinefiletext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, levfileLabel,
	XtNfromVert, levfileText,
        XtNstring, &readlinefile_str,
        XtNlength, FILE_NAME_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   readlinefileButton =  XtVaCreateManagedWidget(
	"readlinefilebutton",
        commandWidgetClass,
        hfsForm,
        XtNfromHoriz, levfileText,
	XtNfromVert, levfileText,
        XtNlabel, "read",
        NULL );
   XtAddCallback(readlinefileButton, XtNcallback, readlineCB, 0 );

   /* 
    * No. of hfs structures and current hfs structure 
    */

   numStructLabel = XtVaCreateManagedWidget(
        "numstructlabel",
        labelWidgetClass,
        hfsForm,
	XtNfromVert, readlinefileText,
        XtNlabel, "No. structures: ",
        NULL );

   numStructText = XtVaCreateManagedWidget(
        "numstructtext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, numStructLabel,
	XtNfromVert,readlinefileText,
        XtNstring, &numstruct_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );
   
   numStructUpButton =  XtVaCreateManagedWidget(
	"numstructupbutton",
        commandWidgetClass,
        hfsForm,
        XtNfromHoriz, numStructText,
	XtNfromVert, readlinefileText,
	XtNwidth, 25,
        NULL );
   XtAddCallback(numStructUpButton, XtNcallback, numStructUpCB, 0 );

   numStructDownButton =  XtVaCreateManagedWidget(
	"numstructdownbutton",
        commandWidgetClass,
        hfsForm,
        XtNfromHoriz, numStructUpButton,
	XtNfromVert, readlinefileText,
	XtNwidth, 25,
        NULL );
   XtAddCallback(numStructDownButton, XtNcallback, numStructDownCB, 0 );
   
   thisStructLabel = XtVaCreateManagedWidget(
        "thisstructlabel",
        labelWidgetClass,
        hfsForm,
	XtNfromHoriz, numStructDownButton,
	XtNfromVert, readlinefileText,
        XtNlabel, "Current structure : ",
        NULL );

   thisStructText = XtVaCreateManagedWidget(
        "thisstructtext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, thisStructLabel,
	XtNfromVert, readlinefileText,
        XtNstring, &thisstruct_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );
   
   thisStructUpButton =  XtVaCreateManagedWidget(
	"thisstructupbutton",
        commandWidgetClass,
        hfsForm,
        XtNfromHoriz, thisStructText,
	XtNfromVert, readlinefileText,
	XtNwidth, 25,
        NULL );
   XtAddCallback(thisStructUpButton, XtNcallback, thisStructUpCB, 0 );

   thisStructDownButton =  XtVaCreateManagedWidget(
	"thisstructdownbutton",
        commandWidgetClass,
        hfsForm,
        XtNfromHoriz, thisStructUpButton,
	XtNfromVert, readlinefileText,
	XtNwidth, 25,
        NULL );
   XtAddCallback(thisStructDownButton, XtNcallback, thisStructDownCB, 0 );

   /*
    * Current line number
    */

   thislineLabel = XtVaCreateManagedWidget(
        "thislinelabel",
        labelWidgetClass,
        hfsForm,
	XtNfromVert, numStructText,
        XtNlabel, "Current line number: ",
        NULL );

   thislineText = XtVaCreateManagedWidget(
        "thislinetext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, numStructDownButton,
	XtNfromVert,numStructText,
        XtNstring, &thisline_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   thislineButton =  XtVaCreateManagedWidget(
	"thislinebutton",
        commandWidgetClass,
        hfsForm,
        XtNfromHoriz, thislineText,
	XtNfromVert, numStructText,
        XtNlabel, "get info",
        NULL );
   XtAddCallback(thislineButton, XtNcallback, getinfoCB, 0 );

   deletelineButton =  XtVaCreateManagedWidget(
	"deletelinebutton",
        commandWidgetClass,
        hfsForm,
        XtNfromHoriz, thislineButton,
	XtNfromVert, numStructText,
        XtNlabel, "delete",
        NULL );
   XtAddCallback(deletelineButton, XtNcallback, deleteCB, 0 ); 
      
   /* 
    * Lower/Upper levels
    */

   lowlevLabel = XtVaCreateManagedWidget(
        "lowlevlabel",
        labelWidgetClass,
        hfsForm,
	XtNfromVert, thislineText,
        XtNlabel, "Lower level:  ",
        NULL );

   lowlevText = XtVaCreateManagedWidget(
        "lowlevtext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, lowlevLabel,
	XtNfromVert, thislineText,
        XtNstring, &lowlev_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   upplevLabel = XtVaCreateManagedWidget(
        "upplevlabel",
        labelWidgetClass,
        hfsForm,
	XtNfromVert, thislineText,
	XtNfromHoriz, lowlevText,
        XtNlabel, "Upper level: ",
        NULL );

   upplevText = XtVaCreateManagedWidget(
        "upplevtext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, upplevLabel,
	XtNfromVert, thislineText,
        XtNstring, &upplev_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   /*
    * Lower/Upper J values
    */

   lowJLabel = XtVaCreateManagedWidget(
        "lowjlabel",
        labelWidgetClass,
        hfsForm,
	XtNfromVert, lowlevText,
        XtNlabel, "Lower J: ",
        NULL );

   lowJText = XtVaCreateManagedWidget(
        "lowjtext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, lowlevLabel,
	XtNfromVert, lowlevText,
        XtNstring, &lowj_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,
        NULL );

   uppJLabel = XtVaCreateManagedWidget(
        "uppjlabel",
        labelWidgetClass,
        hfsForm,
	XtNfromHoriz, lowlevText,
	XtNfromVert, lowlevText,
        XtNlabel, "Upper J: ",
        NULL );

   uppJText = XtVaCreateManagedWidget(
        "uppjtext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, upplevLabel,
	XtNfromVert, lowlevText,
        XtNstring, &uppj_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   /*
    * Lower/Upper A value
    */

   lowALabel = XtVaCreateManagedWidget(
        "lowalabel",
        labelWidgetClass,
        hfsForm,
	XtNfromVert, lowJText,
        XtNlabel, "Lower A: ",
        NULL );

   lowAText = XtVaCreateManagedWidget(
        "lowatext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, lowlevLabel,
	XtNfromVert, lowJText,
        XtNstring, &lowA_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   lowAToggleButton =  XtVaCreateManagedWidget(
	"lowatogglebutton",
        toggleWidgetClass,
        hfsForm,
        XtNfromHoriz, lowAText,
	XtNfromVert, lowJText,
        XtNlabel, "hold",
        NULL );
   XtAddCallback(lowAToggleButton, XtNcallback, lowAToggleCB, 0 );

   uppALabel = XtVaCreateManagedWidget(
        "uppalabel",
        labelWidgetClass,
        hfsForm,
	XtNfromHoriz, lowlevText,
	XtNfromVert, lowJText,
        XtNlabel, "Upper A: ",
        NULL );

   uppAText = XtVaCreateManagedWidget(
        "uppatext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, upplevLabel,
	XtNfromVert, lowJText,
        XtNstring, &uppA_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   uppAToggleButton =  XtVaCreateManagedWidget(
	"lowatogglebutton",
        toggleWidgetClass,
        hfsForm,
        XtNfromHoriz, uppAText,
	XtNfromVert, lowJText,
        XtNlabel, "hold",
        NULL );
   XtAddCallback(uppAToggleButton, XtNcallback, uppAToggleCB, 0 );

   /*
    * Lower/Upper B value
    */

   lowBLabel = XtVaCreateManagedWidget(
        "lowblabel",
        labelWidgetClass,
        hfsForm,
	XtNfromVert, lowAText,
        XtNlabel, "Lower B: ",
        NULL );

   lowBText = XtVaCreateManagedWidget(
        "lowbtext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, lowlevLabel,
	XtNfromVert, lowAText,
        XtNstring, &lowB_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   lowBToggleButton =  XtVaCreateManagedWidget(
	"lowbtogglebutton",
        toggleWidgetClass,
        hfsForm,
        XtNfromHoriz, lowBText,
	XtNfromVert, lowAText,
        XtNlabel, "hold",
        NULL );
   XtAddCallback(lowBToggleButton, XtNcallback, lowBToggleCB, 0 );

   uppBLabel = XtVaCreateManagedWidget(
        "uppblabel",
        labelWidgetClass,
        hfsForm,
	XtNfromHoriz, lowlevText,
	XtNfromVert, lowAText,
        XtNlabel, "Upper B: ",
        NULL );

   uppBText = XtVaCreateManagedWidget(
        "uppbtext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, upplevLabel,
	XtNfromVert, lowAText,
        XtNstring, &uppB_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   uppBToggleButton =  XtVaCreateManagedWidget(
	"lowbtogglebutton",
        toggleWidgetClass,
        hfsForm,
        XtNfromHoriz, uppBText,
	XtNfromVert, lowAText,
        XtNlabel, "hold",
        NULL );
   XtAddCallback(uppBToggleButton, XtNcallback, uppBToggleCB, 0 );
 
   /* 
    *  Wavenumber
    */

   waveLabel = XtVaCreateManagedWidget(
        "wavelabel",
        labelWidgetClass,
        hfsForm,
	XtNfromVert, lowBText,
        XtNlabel, "Wavenumber: ",
        NULL );

   waveText = XtVaCreateManagedWidget(
        "wavetext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, lowlevLabel,
	XtNfromVert, lowBText,
        XtNstring, &wave_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   waveToggleButton =  XtVaCreateManagedWidget(
	"wavetoggle",
        toggleWidgetClass,
        hfsForm,
        XtNfromHoriz, waveText,
	XtNfromVert, lowBText,
        XtNlabel, "hold",
        NULL );
   XtAddCallback(waveToggleButton, XtNcallback, waveToggleCB, 0 );
 
   getinfoButton = XtVaCreateManagedWidget(
        "insert",
        commandWidgetClass,
        hfsForm,
        XtNfromVert, lowBText,
        XtNfromHoriz, waveToggleButton,
        XtNwidth, 80,
        NULL );
   XtAddCallback(getinfoButton, XtNcallback, insertCB, 0 );

   /* 
    *  Intensity
    */

   intenLabel = XtVaCreateManagedWidget(
        "intenlabel",
        labelWidgetClass,
        hfsForm,
	XtNfromVert, waveText,
        XtNlabel, "Intensity: ",
        NULL );

   intenText = XtVaCreateManagedWidget(
        "intentext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, lowlevLabel,
	XtNfromVert, waveText,
        XtNstring, &inten_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   intenToggleButton =  XtVaCreateManagedWidget(
	"intentoggle",
        toggleWidgetClass,
        hfsForm,
        XtNfromHoriz, intenText,
	XtNfromVert, waveText,
        XtNlabel, "hold",
        NULL );
   XtAddCallback(intenToggleButton, XtNcallback, intenToggleCB, 0 );
 
   /* 
    *  Damping
    */

   dampLabel = XtVaCreateManagedWidget(
        "damplabel",
        labelWidgetClass,
        hfsForm,
	XtNfromVert, intenText,
        XtNlabel, "Damping: ",
        NULL );

   dampText = XtVaCreateManagedWidget(
        "damptext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, lowlevLabel,
	XtNfromVert, intenText,
        XtNstring, &damp_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   dampToggleButton =  XtVaCreateManagedWidget(
	"damptoggle",
        toggleWidgetClass,
        hfsForm,
        XtNfromHoriz, dampText,
	XtNfromVert, intenText,
        XtNlabel, "hold",
        NULL );
   XtAddCallback(dampToggleButton, XtNcallback, dampToggleCB, 0 );
 
   /* 
    *  Width
    */

   fwhmLabel = XtVaCreateManagedWidget(
        "fwhmlabel",
        labelWidgetClass,
        hfsForm,
	XtNfromVert, dampText,
        XtNlabel, "Width: ",
        NULL );

   fwhmText = XtVaCreateManagedWidget(
        "fwhmtext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, lowlevLabel,
	XtNfromVert, dampText,
        XtNstring, &fwhm_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   fwhmToggleButton =  XtVaCreateManagedWidget(
	"fwhmtoggle",
        toggleWidgetClass,
        hfsForm,
        XtNfromHoriz, fwhmText,
	XtNfromVert, dampText,
        XtNlabel, "hold",
        NULL );
   XtAddCallback(fwhmToggleButton, XtNcallback, fwhmToggleCB, 0 );
 
   /* 
    *  Comment
    */

   commentLabel = XtVaCreateManagedWidget(
        "commentlabel",
        labelWidgetClass,
        hfsForm,
	XtNfromVert, fwhmText,
        XtNlabel, "Comment: ",
        NULL );

   commentText = XtVaCreateManagedWidget(
        "commenttext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, lowlevLabel,
	XtNfromVert, fwhmText,
        XtNstring, &comment_str,
        XtNlength, NUM_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );


   /*
    * Filename for binary line file
    */

   linefileLabel = XtVaCreateManagedWidget(
        "linefilelabel",
        labelWidgetClass,
        hfsForm,
	XtNfromVert, commentText,
        XtNlabel, "Line file:  ",
        NULL );

   linefileText = XtVaCreateManagedWidget(
        "linefiletext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, lowlevLabel,
	XtNfromVert, commentText,
        XtNstring, &linefile_str,
        XtNlength, FILE_NAME_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   linefileButton =  XtVaCreateManagedWidget(
	"linefilebutton",
        commandWidgetClass,
        hfsForm,
        XtNfromHoriz, linefileText,
	XtNfromVert, commentText,
        XtNlabel, "save",
        NULL );
   XtAddCallback(linefileButton, XtNcallback, saveLinfileCB, 0 );

   /*
    * Filename for ASCII line file
    */

   ascfileLabel = XtVaCreateManagedWidget(
        "ascfilelabel",
        labelWidgetClass,
        hfsForm,
	XtNfromVert, linefileText,
        XtNlabel, "ASCII file:  ",
        NULL );

   ascfileText = XtVaCreateManagedWidget(
        "ascfiletext",
        asciiTextWidgetClass,
        hfsForm,
        XtNfromHoriz, lowlevLabel,
	XtNfromVert, linefileText,
        XtNstring, &ascfile_str,
        XtNlength, FILE_NAME_LEN,
        XtNuseStringInPlace, True,
        XtNdisplayCaret, False,
        XtNtype, XawAsciiString,
        XtNeditType, XawtextEdit,                                   
        NULL );

   ascfileButton =  XtVaCreateManagedWidget(
	"ascfilebutton",
        commandWidgetClass,
        hfsForm,
        XtNfromHoriz, ascfileText,
	XtNfromVert, linefileText,
        XtNlabel, "save",
        NULL );
   XtAddCallback(ascfileButton, XtNcallback, saveAscfileCB, 0 );

   /*
    * holds the fit / plot / components / close buttons
    */

   buttonBox = XtVaCreateManagedWidget(
        "hfsbuttonbox",
        boxWidgetClass,
        hfsForm,
        XtNfromVert, ascfileText,
        XtNorientation, XtorientHorizontal,
        NULL );

   updateButton =  XtVaCreateManagedWidget(
	"updatebutton",
        commandWidgetClass,
        buttonBox,
        XtNlabel, "update",
        NULL );
   XtAddCallback(updateButton, XtNcallback, updateCB, 0 );

   fitButton = XtVaCreateManagedWidget(
        "fitbutton",
        commandWidgetClass,
        buttonBox,
        XtNlabel, "Fit",
        NULL );
   XtAddCallback( fitButton, XtNcallback, fitCB, 0 );

   plotButton = XtVaCreateManagedWidget(
        "plotbutton",
        commandWidgetClass,
        buttonBox,
        XtNlabel, "Plot",
        NULL );
   XtAddCallback( plotButton, XtNcallback, plotCB, 0 );

   compButton = XtVaCreateManagedWidget(
        "compbutton",
        commandWidgetClass,
        buttonBox,
        XtNlabel, "Components",
        NULL );
   XtAddCallback( compButton, XtNcallback, compCB, 0 );

   closeButton = XtVaCreateManagedWidget(
        "hfsclosebutton",
        commandWidgetClass,
        buttonBox,
        XtNlabel, "Close",
        NULL );
   XtAddCallback( closeButton, XtNcallback, CloseHfsWindow, 0 );

   /************************ 
    *** Finishing it off ***
    ************************/

   /*
    * bring window to life
    */
   XtRealizeWidget( hfsShell );
 
   /*
    * attach the icon and the button bitmaps
    */
   icon = XCreateBitmapFromData(XtDisplay(hfsShell),
                                XtWindow(hfsShell),
                                Xgremlin_hfs_bits,
                                Xgremlin_hfs_width, Xgremlin_hfs_height);
   XtSetArg( args[0], XtNiconPixmap, icon);
   XtSetValues( hfsShell, args, 1);

   count_up = XCreateBitmapFromData(XtDisplay(hfsForm),
                                    XtWindow(hfsForm),
                                    count_up_bits,
                                    count_up_width, count_up_height);
   XtSetArg( args[0], XtNbitmap, count_up);
   XtSetValues( numStructUpButton, args, 1);
   XtSetValues( thisStructUpButton, args, 1);

   count_down = XCreateBitmapFromData(XtDisplay(hfsForm),
                                    XtWindow(hfsForm),
                                    count_down_bits,
                                    count_down_width, count_down_height);
   XtSetArg( args[0], XtNbitmap, count_down);
   XtSetValues( numStructDownButton, args, 1);
   XtSetValues( thisStructDownButton, args, 1);
}

/*-------------------------------------------------------------*/ 
