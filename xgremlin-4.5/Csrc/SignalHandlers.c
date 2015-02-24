/*
 * This program is part of Xgremlin
 *
 * Copyright (C) 1994, 1995
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

/*
 * $Id: SignalHandlers.c,v 1.3 1995/09/21 13:50:26 ulf Exp $
 */

#if !defined( __TEST__ )
#include <X11/Intrinsic.h>     /* X toolkit intrinsics definitions */
#endif

#include <signal.h>
#include <setjmp.h>
#include "SignalHandlers.h"

#if defined( __TEST__ )
#include <stdio.h>
#else
#include "Edit.h"
#endif

jmp_buf proc_status;    /* a global variable */


/*--------------------------------------------------------------*/

void
sigfpe_handler()
{
#if defined( __TEST__ )
   printf( " Signal :  caught SIGFPE\n" );
#else
   WriteStr( " Fatal error :  floating point exception ( SIGFPE )\n" );
#endif
   
   /*
    * re-register signal handler to wait for next exception
    */
#if defined( HAVE_ANSI_SIGNALS )
   signal( SIGFPE, sigfpe_handler );
#endif

   /*
    * return to command line editor instead of carrying on with
    * the interrupted command
    */
   longjmp( proc_status, 1 );
}

/*--------------------------------------------------------------*/

void
sigsegv_handler()
{
#if defined( __TEST__ )
   printf( " Signal :  caught SIGSEGV\n" );
#else
   WriteStr(" Fatal error :  segmentation violation ( SIGSEGV )\n");
   WriteStr(" occured probably due to a bug in Xgremlin.\n");
   WriteStr(" This may have left Xgremlin mortally wounded -\n");
   WriteStr(" try to exit the program safely NOW.\n");
#endif
   
   /*
    * re-register signal handler to wait for next exception
    */
#if defined( HAVE_ANSI_SIGNALS )
   signal( SIGSEGV, sigsegv_handler );
#endif

   /*
    * return to command line editor instead of carrying on with
    * the interrupted command
    */
   longjmp( proc_status, 1 );
}

/*--------------------------------------------------------------*/

void
install_signal_handlers()
{
/*
 * we want core dumps when debugging the program
 */
#if !defined( DEBUG )
   signal( SIGFPE,  sigfpe_handler);
   signal( SIGSEGV, sigsegv_handler );
#endif
}

/*--------------------------------------------------------------*/

#if defined( __TEST__ )

int
main()
{
   float a, b, c;
   int i;
   char *cp = NULL;

   a = 1.0;
   b = 0.0;
   install_signal_handlers();

   /*
    * test setjmp/longjmp
    */
   if ( setjmp(proc_status) == 0 )
     c = a/b;
   else
     printf( " Returned from handler with longjump\n" );

   if ( setjmp(proc_status) == 0 )
     *cp = 'blamm';
   else
     printf( " Returned from handler with longjump\n" );

   /*
    * test re-arming signal handler (should cause an infinite loop)
    */
   c = a/b;
}

#endif /* _DEBUG */

/* Revision history:
 * -----------------
 * $Log: SignalHandlers.c,v $
 * Revision 1.3  1995/09/21  13:50:26  ulf
 * better error message for segmentation violations - tell people what to do.
 *
 * Revision 1.2  1995/09/21  13:40:24  ulf
 * add a handler for segmentation violations to give users a chance to exit
 * program cleanly.
 *
 * Revision 1.1  1995/07/09  03:03:33  ulf
 * Initial revision
 *
 */
