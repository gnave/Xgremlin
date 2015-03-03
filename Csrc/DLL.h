/*
 * Copyright (C) 1997
 * Ulf Griesmann, Gaithersburg MD 20877, U.S.A.
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
 * $Id$
 */

/*
 * functions to load shared libraries dynamically to extend Xgremlin
 * without re-compilation.
 */

#ifndef _DLL_H
#define _DLL_H

/*
 * initialize dynamic loader module
 */
void
initialize_dll();


/*
 * change the location of the library files. Xgremlin always searches this
 * directory first (~/xgremlin-dll by default) and then the directory
 * /usr/local/xgremlin/dll (or XGREMLIN_CONF_PATH/dll).
 */
void
dll_path_(char path[]);


/*
 * load a dynamic library
 * variable err is set to 1 if an error occurs, 0 otherwise.
 */
void
load_dll_(char name[], int *err);


/*
 * check if a command has been defined in a dynamic library.
 * returns 1 if the command is defined via a dynamic library, 0 otherwise.
 */
void
dll_cmd_(int *found);


/*
 * call a previously loaded dynamic library function
 */
void
call_dll_(float *r, float *tr, );


#endif /* _DLL_H */

/* Revision history:
 * -----------------
 * $Log$
 */












