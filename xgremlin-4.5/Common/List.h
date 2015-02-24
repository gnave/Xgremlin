/*
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

/******************************************************************************
 *                                                                            *
 *         A collection of functions to manage doubly linked lists            *
 *                                                                            *
 ******************************************************************************/


#ifndef _List_h
#define _List_h

#ifdef __GNUC__
#define INLINE  __inline__
#else
#define INLINE
#endif
 

/*
 * ANSI C compiler happyfiers
 */
#define SORT_PROC (int(*)( void *, void* ))       /* cast to sort function */
#define SCAN_PROC (void(*)( void * ))             /* cast to scan funtion  */
#define SEARCH_PROC (int(*)( void* ))             /* cast to search function */


/*
 * location of a new list entry with respect to current entry
 */
enum { BEFORE, AFTER };


/*
 * fake a list type
 */
typedef char * tList;     


/*
 * Create a List. Returns -1 if not successful, 0 otherwise
 */
extern INLINE int 
create_list( tList *list );


/*
 * Create a List and return it. Return NULL if not successful.
 */
extern INLINE tList
new_list();


/*
 * erase all entries in the list, leaving an empty list
 */
extern INLINE int
erase_list_entries( tList list );


/* 
 * Delete a list from memory (List must be empty)
 */
extern INLINE int 
delete_list( tList *list );


/*
 * Return number of elements in the list (list must exist already)
 */
extern INLINE int 
list_entries( tList list );


/*
 * advance the list pointer to the next entry if not at end of list
 * wraps round to first element at the end of the list
 */
extern INLINE void
list_next( tList list );


/*
 * advance the list pointer to the previous entry if not at beginning of list
 * wraps round to last element at the end of the list
 */
extern INLINE void
list_prev( tList list );


/*
 * Reset the list pointer to the beginning of the list
 */
extern INLINE void 
list_head( tList list );


/*
 * set the list pointer to the last entry in the list
 */
extern INLINE void
list_tail( tList list);


/*
 * returns 1 if the list pointer is set to the first element in the list, 0 otherwise
 */
extern INLINE int
list_at_head( tList list );


/*
 * returns 1 if the list pointer is set to the last element in the list, 0 otherwise
 */
extern INLINE int
list_at_tail( tList list);


/*
 * stores the current list pointer
 */
extern INLINE void
list_store_current(tList list);


/*
 * resets the current list pointer to the previously stored value
 * (This should only be used if the list has not been changed since
 * the last call to 'list_store_current'.
 */
extern INLINE void
list_recall_current(tList list);


/*
 * Add a new entry to the list at the current list pointer
 * The new entry is inserted either BEFORE or AFTER the current pointer.
 * The BEFORE option enables the creation of stacks (=FIFO lists).
 */
extern INLINE int 
list_insert( tList list, void *new_entry, int location );


/*
 * insert the object (usually a structure) into the list at the current pointer
 * The list keeps a copy of the object, the caller is not required to 
 * allocate memory for the list entry. This is essentially a 
 * convenience function;
 */
extern INLINE int
list_insert_object( tList list, void *new_object, int obj_size, int location );


/*
 * Add a new entry to a SORTED list. The procedure 'comp-proc' passed
 * to the function takes two pointers to list entry data as its arguments. 
 * comp_proc has to return 0 if both arguments are equal; comp_proc < 0
 * if the first argument is smaller than the second; comp_proc > 0 if
 * the first argument is larger than the second.
 */
extern INLINE int 
sorted_list_insert(tList list, void *new_entry, int (*comp_proc)(void *, void *));


/*
 * a convenience version of the previous function
 */
extern INLINE int 
sorted_list_insert_object(tList list, void *new_object, int obj_size, int (*comp_proc)(void *, void *));


/*
 * Return a pointer to the current, increment list pointer
 * At the end of the list the list pointer wraps round to the beginning.
 */
extern INLINE void *
get_current_entry( tList list, void **ptr_to_entry );


/*
 * retrieve the current object, increment the list pointer.
 * This too is a convenience function.
 */
extern INLINE void *
get_current_object( tList list, void *object, int obj_size );


/*
 * removes the current list entry. Current pointer is set to prev entry.
 * DOES ALSO DEALLOCATE DATA
 */
extern INLINE void
remove_current_entry( tList list );


/*
 * remove the tail entry of the list, discard content
 */
extern INLINE void
chop_list_tail( tList list );


/*
 * Apply a function to all members of the list.
 * Each list entry is passed to the function 'scan_proc'.
 */
extern INLINE void
scan_list( tList list,  void (*scan_proc)(void *) );


/*
 * Search a list, apply a function to every list element until this
 * function returns 1. Returns a pointer to the corresponding list entry, 
 * NULL otherwise. search_proc MUST return either 0 or 1.
 * The list pointer is positioned behind the found entry !
 */
extern INLINE void *
search_list( tList list, int (*search_proc)(void *) );


/*
 * a convenience version of the previous function
 */
extern INLINE void *
search_list_object(tList list, void *object, int obj_size, int (*search_proc)(void *) );


#endif /* _List_h */

/* Revision history:
 * -----------------
 * $Log: List.h,v $
 * Revision 1.4  1996/03/22  14:20:03  ulf
 * modified use of __inline__ to match what is done elsewhere in the program
 *
 * Revision 1.3  1996/03/03  14:51:28  ulf
 * tell the compiler to always inline the functions in List.h
 * with 'extern inline'
 *
 * Revision 1.2  1995/10/10  12:34:21  ulf
 * add functions 'list_at_head' and 'list_at_tail' to check if
 * list pointer is at beginning or end of list
 *
 * Revision 1.1  1995/06/14  03:28:52  ulf
 * Initial revision
 *
 */




