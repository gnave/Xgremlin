/*
 * these are the pointer to global arrays used in various places of the
 * Xgremlin program
 */

#ifndef _ARRAYS_H
#define _ARRAYS_H

#define NUM_ARRAYS  18  /* number of adjustable arrays */    

extern float *r;        /* r array */
extern float *tr;       /* tr array */
extern float *rtr;      /* r and tr combined */

extern float *ffta;     /* ffta array */
extern float *phz;      /* phase array, upper half of rtr */

extern double *point;   /* point array */
extern float *amp;      /* line intensity */
extern float *wd;       /* line width */
extern float *dmp;      /* damping */
extern float *eps1;     /* various parameters */
extern float *eps2;
extern float *eps3;
extern float *eps4;
extern float *eps5;
extern int *nit;        /* number of iterations */
extern int *nhold;      /* for parameter hold mask */
extern char *ctag;      /* line tags */
extern char *dent;      /* line identifiers */
 
#endif /* _ARRAYS_H */
