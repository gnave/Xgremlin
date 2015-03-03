/*
 * Copyright (C) 1997
 * National Institute of Standards and Technology, 
 * Gaithersburg MD 20899, U.S.A.
 *
 * Program is in the public domain.
 */

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "nso6.h"
#include "timer.h"

#define AUXSTR_LEN  64
#define EPS         1.0e-8   /* accuracy of rational approx. of fdr */
#define IMAX        40

/*------------------------ Local prototypes ----------------------*/

/*
 * construct a well formatted header line and write it to the file
 */
static void
hdr_line(FILE* f, char var[], char val[], char comment[]);


/*
 * construct a well formatted header line with a FORTRAN readable
 * string value.
 */
static void
hdr_strg(FILE* f, char var[], char str[], int len, char comment[]);


/*
 * construct a well formatted and FORTRAN readable comment line.
 */
static void
hdr_comm(FILE* f, char var[], char comm[]);


/*
 * pad out a string with blanks to the specified number of characters
 */
static void
pad_blanks(char* s, int idx);

/*
 * functions for rational approximation of real numbers
 */
void
ratapp(float x, float *eps, int *num, int *den);

void
rational(float x, int i, int *n, int *d);

/*-----------------------------------------------------------------*/

void
write_nso6_header(fts_scan_info *hdr, fts_parameters *fp, int np)
{
   FILE* f;
   char file_name[FILE_NAME_LEN];     /* for file names */
   int ntrans;                        /* transform length */
   char auxstr[AUXSTR_LEN];           /* auxiliary string */
   float eps;                         /* accuracy of fdr = M/K */
   int M, K, S;                       /* sampling variables */

   /*
    * first write header file
    */
   strcpy(file_name, hdr->filename);
   strcat(file_name, ".hdr");
   f = fopen(file_name, "w");

   fprintf(f,  "/         FILE PARAMETERS\n");
   hdr_line(f, "inftype", "6", "Info block type");
   hdr_comm(f, "id", "NSO type 6 FTS data file");
   datestr(auxstr, AUXSTR_LEN);
   hdr_strg(f, "day", auxstr, 16, "Acquisition date (Month dd, yyyy)");
   hdr_line(f, "serialno", "0", "Serial number n/a");
   hdr_line(f, "wstart", "1.0", "First wavenumber / point in file");
   sprintf(auxstr, "%f", (double)np);
   hdr_line(f, "wstop", auxstr, "Last wavenumber / point in file");
   sprintf(auxstr, "%d", np);
   hdr_line(f, "npo", auxstr, "Number of points in file");
   hdr_strg(f, "data_is", "Real", 20, "Data type (Real or Complex)");
   hdr_line(f, "delw", "1.000", "Dispersion (1/cm / point)");
   sprintf(auxstr, "%8.4f", hdr->resol);
   hdr_line(f, "resolutn", auxstr, "Spectral resolution (1/cm)");
   hdr_line(f, "fboffs", "0", "File byte offset");
   hdr_line(f, "bocode", "0", "Data bye order: 0=little endian; 1=big endian");
   hdr_strg(f, "xaxis_is", "Index", 20, "Wavenumber, Wavelength, Index, Other");

   fprintf(f,  "/         FILE REDUCTION PARAMETERS\n");
   hdr_line(f, "wavcorr",  "0.000000E+00", "Fractional wavenumber correction");
   hdr_line(f, "rdsclfct", "1.000000E+00", "Read scale factor");
   hdr_line(f, "noiselev", "0.000000E+00", "Assumed noise level");

   fprintf(f,  "/         OBSERVING PARAMETERS\n");
   hdr_strg(f, "ftsmode", "Lab", 20, "Type of observation");
   hdr_strg(f, "user", hdr->observer, 20, "Observer");
   sprintf(auxstr, "%d", hdr->nscan);
   hdr_line(f, "nscan", auxstr, "Number of co-added scans");
   hdr_strg(f, "timestr", hdr->start_time, 20, "Starting time (hh:mm:ss)");
   sprintf(auxstr, "%7.2f", hdr->etime);
   hdr_line(f, "eltim", auxstr, "Elapsed time for all scans (seconds)");
   if (hdr->spectype == 0)
      hdr_strg(f, "spectype", "Absorption", 20, "Emission or Absorption");
   else
      hdr_strg(f, "spectype", "Emission", 20, "Emission or Absorption");
   strcpy(auxstr, hdr->source);
   strcat(auxstr, " & ");
   strcat(auxstr, hdr->inp_filter);
   hdr_strg(f, "source", auxstr, 20, "Source & Input filter");
   hdr_strg(f, "sample", hdr->conditions, 20, "Source conditions or Sample");
   hdr_strg(f, "filter1", hdr->filter_a, 20, "Optical filter A");
   hdr_strg(f, "filter2", hdr->filter_b, 20, "Optical filter B");
   switch(hdr->mode_is) {
      case 1 : strcpy(auxstr, "A only");
               break;
      case 2 : strcpy(auxstr, "A - B");
               break;
      case 3 : strcpy(auxstr, "A-B/A+B");
               break;
      case 4 : strcpy(auxstr, "A, B separate");
               break;
   }
   hdr_strg(f, "input_is", auxstr, 20, "A only; A-B; A-B/A+B; A,B separate");
   sprintf(auxstr, "%4.2f", hdr->aperture);
   hdr_line(f, "apin", auxstr, "Aperture diameter (mm)");
   hdr_line(f, "apmax", auxstr, "Max. aperture (mm)");
   hdr_strg(f, "spect", hdr->instrument, 20, "Spectrometer");
   hdr_strg(f, "bmsplt", hdr->beamsplitter, 20, "Beamsplitter");
   hdr_strg(f, "dtectrA", hdr->detector_a, 20, "Detector A");
   sprintf(auxstr, "%5.3e", hdr->gain_a);
   hdr_strg(f, "gainA", auxstr, 20, "Channel A gain");
   hdr_strg(f, "dtectrB", hdr->detector_b, 20, "Detector B");
   sprintf(auxstr, "%5.3e", hdr->gain_b);
   hdr_strg(f, "gainB", auxstr, 20, "Channel B gain");

   fprintf(f,  "/         SCAN PARAMETERS\n");
   sprintf(auxstr, "%d", hdr->alias);
   hdr_line(f, "alias", auxstr, "Alias");
   sprintf(auxstr, "%14.10f", hdr->fdr);
   hdr_line(f,"fdr", auxstr, "Fringe division ratio");   /* currently a comment */
   eps = EPS;
   ratapp(hdr->fdr, &eps, &M, &K);
   S = np / M;
   sprintf(auxstr, "%d", M);
   hdr_line(f, "nM", auxstr, "M  ( fringe division ratio ~= M/K )");
   sprintf(auxstr, "%d", K);
   hdr_line(f,"nK", auxstr, "K");
   sprintf(auxstr, "%d", S);
   hdr_line(f, "nS", auxstr, "S");
   hdr_line(f, "fzeeman", "1.5000E5", "Zeeman frequency (needed by Gremlin)");
   sprintf(auxstr, "%f", (double)hdr->velocity);
   hdr_line(f, "drivevel", auxstr, "Carriage velocity in fringes / sec");
   sprintf(auxstr, "%10.4f", fp->sigma_laser);
   hdr_line(f, "refwavno", auxstr, "Laser wavenumber (1/cm)");
   sprintf(auxstr, "%e", SAMP_CLOCK);
   hdr_line(f, "fclock", auxstr, "Sampling clock frequency (Hz)");
   sprintf(auxstr, "%f", SAMP_FREQ);
   hdr_line(f, "sampfreq", auxstr, "Sampling frequency (Hz)");
   sprintf(auxstr, "%f", hdr->decim);
   hdr_line(f, "decim", auxstr, "Decimation factor");
   sprintf(auxstr, "%d", fp->kernel_length);
   hdr_line(f, "kernlen", auxstr, "Digital filter kernel length");
   sprintf(auxstr, "%f", fp->fractional_bw);
   hdr_line(f, "fracbw", auxstr, "Digital filter fract. bandwidth");
   sprintf(auxstr, "%7.3f", hdr->opd);
   hdr_line(f, "ctrav", auxstr, "Carriage max. OPD (cm)");
   sprintf(auxstr, "%9.5f", (float)hdr->velocity/fp->sigma_laser );
   hdr_line(f, "cvel", auxstr, "Carriage velocity (cm/sec)");
   hdr_strg(f, "scan_is", "Forward",20, "Scan direction");
   hdr_line(f, "ratsig", "0.00", "Total signal (A+B) (volts)");
   hdr_line(f, "ratrms", "0.00", "Total RMS (A+B) (volts)");
   if (hdr->mode_is == 3)
      hdr_strg(f, "ratio_is", "On", 20, "Ratio - On or Off");
   else
      hdr_strg(f, "ratio_is", "Off", 20, "Ratio - On or Off");
   sprintf(auxstr, "%4.2f", hdr->modeff);
   hdr_line(f, "modeff", auxstr, "Modulation efficiency (1 = 100%%)");
   hdr_line(f, "srverr", "0.00", "FTS servo RMS error (millifringes)");
   sprintf(auxstr, "%f", hdr->pspect);
   hdr_line(f, "pspect", auxstr, "FTS tank pressure (Torr)");
   hdr_line(f, "tspect", "0.00", "FTS tank temperature (deg C)");
   hdr_line(f, "hspect", "0.00", "FTS tank H2O pressure (Torr)");
   sprintf(auxstr, "%8.2f", hdr->focal_len);
   hdr_line(f, "flin", auxstr, "Input focal length (mm)");
   sprintf(auxstr, "%8.2f", fp->flen_out);
   hdr_line(f, "flout", auxstr, "Output focal length (mm)");
   hdr_strg(f, "acqvers", ACQVERS, 20, "Acquisition software version");

   fprintf(f,  "/         TRANSFORM PARAMETERS\n");
   sprintf(auxstr, "%d", np);
   hdr_line(f, "nint", auxstr, "Number of pts in interferogram");
   hdr_line(f,"inskip", "0", "Number of pts skipped on input");
   sprintf(auxstr, "%d", np/2);
   hdr_line(f, "ncenter", auxstr, "Point number of central fringe");
   hdr_line(f, "nused", "0", "Number of points used in transform");
   for (ntrans=2; ntrans<np; ntrans*=2);
   sprintf(auxstr, "%d", ntrans);
   hdr_line(f, "ntrans", auxstr, "Transform size (2**N)");
   sprintf(auxstr, "%f", hdr->fsr * (double)(hdr->alias - 1) );
   hdr_line(f, "aliaslo", auxstr, "Alias low wavenumber (1/cm)");
   sprintf(auxstr, "%f", hdr->fsr * (double)hdr->alias );
   hdr_line(f, "aliashi", auxstr, "Alias high wavenumber (1/cm)");
   sprintf(auxstr, "%f", hdr->fsr);
   hdr_line(f, "freespec", auxstr, "Free spectral range (1/cm)");
   sprintf(auxstr, "%f", hdr->fsr * (double)(hdr->alias - 1) );
   hdr_line(f, "outlo", auxstr, "Saved lowest wavenumber (1/cm)");
   sprintf(auxstr, "%f", hdr->fsr * (double)hdr->alias );
   hdr_line(f, "outhi", auxstr, "Saved highest wavenumber (1/cm)");
   sprintf(auxstr, "%f", hdr->fsr * (double)(hdr->alias - 1) );
   hdr_line(f, "bandlo", auxstr, "Useful lower data bound");
   sprintf(auxstr, "%f", hdr->fsr * (double)hdr->alias );
   hdr_line(f, "bandhi", auxstr, "Useful upper data bound");
   hdr_strg(f, "aircorr", "No", 20, "Air correction flag - Yes or No");
   hdr_line(f, "modeapod", "0", "Apodization function");
   hdr_line(f, "apod1", "0.000000E+00", "Apodization variable #1");
   hdr_line(f, "modephzc", "0", "Phase correction mode");
   hdr_line(f, "pvar1", "0.00000000E+00", "Phase correction variable #1");
   hdr_strg(f, "archname", "n/a", 20, "Archive file name");
   hdr_line(f, "origtype", "6", "Original info block type");
   hdr_line(f, "uvar0", "0.00000000E+00", "User variable #0");
   hdr_line(f, "uvar1", "0.00000000E+00", "User variable #1");
   hdr_line(f, "uvar2", "0.00000000E+00", "User variable #2");
   hdr_line(f, "uvar3", "0.00000000E+00", "User variable #3");
   hdr_line(f, "uvar4", "0.00000000E+00", "User variable #4");
   hdr_line(f, "uvar5", "0.00000000E+00", "User variable #5");
   hdr_line(f, "uvar6", "0.00000000E+00", "User variable #6");
   hdr_line(f, "uvar7", "0.00000000E+00", "User variable #7");
   hdr_line(f, "uvar8", "0.00000000E+00", "User variable #8");
   hdr_line(f, "uvar9", "0.00000000E+00", "User variable #9");
   hdr_comm(f, "comment0", hdr->comment0 );
   hdr_comm(f, "comment1", hdr->comment1 );
   
   fprintf(f,"END\n");
   fclose(f);
}

/*-----------------------------------------------------------------*/

static void
hdr_line(FILE* f, char var[], char val[], char comment[])
{
   char line[256];

   strcpy(line, var);
   pad_blanks(line, 8);
   strcat(line,"= ");
   strcat(line, val);
   if (*comment != '\0') {
      pad_blanks(line, 32);  /* put comment in column 32 */
      strcat(line, "/ ");
      strcat(line, comment);
   }
   fprintf(f, "%s\n", line);
}

/*-----------------------------------------------------------------*/

static void
hdr_strg(FILE* f, char var[], char str[], int len, char comment[])
{
   char line[256];

   strcpy(line, var);
   pad_blanks(line, 8);
   strcat(line,"= '");
   strcat(line, str);
   pad_blanks(line, len + 12 - 1); /* strings start at column 11 */
   strcat(line, "'");
   if (*comment != '\0') {
      strcat(line, "/ ");
      strcat(line, comment);
   }
   fprintf(f, "%s\n", line);
}

/*-----------------------------------------------------------------*/

static void
hdr_comm(FILE* f, char var[], char comm[])
{
   char line[256];

   strcpy(line, var);
   pad_blanks(line, 8);
   strcat(line,"= '");
   strcat(line, comm);
   pad_blanks(line, 79);
   strcat(line, "'");
   fprintf(f, "%s\n", line);
}

/*-----------------------------------------------------------------*/

static void
pad_blanks(char* s, int idx)
{
   int len;
   int i;

   len = strlen(s);
   if ( len > idx )   /* leave it alone */
      return;
   for(i=len; i<idx; i++) {
      s[i] = ' ';
   }
   s[idx] = '\0';
}

/*-----------------------------------------------------------------*/

/*
 *     for a given positive real number x this function calculates a
 *     rational approximation for x such that
 *
 *                 |       num  |
 *                 | x  -  ---  | < eps    .
 *                 |       den  |
 *
 *     'eps' is replaced by the actually achieved precision.
 *
 *     Author    :  Ulf Griesmann    08/92
 *
 *     Reference :  M. C. Gutzwiller, Chaos in Classical and Quantum
 *                  Mechanics : Interdisciplinary Applied Mathematics Vol.1,
 *                  Springer-Verlag, page 129
 */

void
ratapp(float x, float *eps, int *num, int *den)
{
      float epsloc;
      int inum, iden, i;

      for (i=1; i<IMAX; i++)
	{
	   rational(x, i, &inum, &iden);
           epsloc = fabs( x - (float)inum/(float)iden) ;
	   if (epsloc <= *eps)
	     break;
	}
      *num = inum;
      *den = iden;
      *eps = epsloc;
}

/*
 *     calculates a rational approximation of x from a continued fraction
 *     approximation up to i-th order. Numerator an denominator are
 *     returned in n and d respectively.
 */
void
rational(float x, int i, int *n, int *d)
{
   int a[IMAX];
   int m, ntmp;
   double rest, xnext, dx;

   /*     
    * calculate the continued fraction approximation
    */
   dx = (double)x;
   rest = dx - floor(dx);
   if (rest == 0.0)
     {
        *n = (int)floor(dx);
	*d = 1;
	return;
     }

   a[0] = (int)floor(dx);
   for(m=1; m<=i; m++)
     {
         xnext = 1.0/rest;
         a[m] = (int)floor(xnext);
         rest = xnext - (double)a[m];
     }

   /*
    * put together rational number
    */
   *n = 1;
   *d = a[i];
   for(m=i; m>0; m--)
     {
        ntmp = *n;
        *n = *d;
        *d = a[m-1] * *d + ntmp;
     }
   ntmp = *n;
   *n = *d;
   *d = ntmp;
}

/*-----------------------------------------------------------------*/

/* Revision history:
 * -----------------
 * $Log$
 */
