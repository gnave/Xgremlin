/*
 * Copyright (C) 1997
 * National Institute of Standards and Technology, 
 * Gaithersburg MD 20899, U.S.A.
 */

/*
 * Function for writing NSO type 6 header/data pairs
 */

#ifndef _NSO6_H
#define _NSO6_H

#define STR_LEN       64
#define FILE_NAME_LEN 256

#define SAMP_CLOCK    40000000.0
#define SAMP_FREQ     39062.5
#define ACQVERS       "Unknown 0"

/*
 * parameters pertaining to the FT spectrometer
 */
typedef struct {
   char fts_name[64];     /* a string identifying the instrument (max 60 chars) */
   double sigma_laser;    /* the laser wavenumber of the reference laser */
   float flen_end;        /* focal length of instrument at end input */
   float flen_side;       /* focal length of instrument at side input */
   float flen_out;        /* output focal length */
   int coadd_delay;       /* a delay between coadds */
   int adc_bits;          /* the resolution of the ADC converter in bits */
   int daq_block_size;    /* number of points for which visibility is calculated */
   int f_type;            /* filter type; 0:Gaussian, 1:Lanczos */
   int f_adaptive;        /* 0: static filter; 1: velocity adaptive filter */
   int vel_num;           /* number of velocities for adaptive filter */
   float vel_range;       /* velocity range in % covered by adaptive filter */
   int kernel_length;     /* digital filter kernel length */
   float fractional_bw;   /* fractional filter bandwidth */
   float theta;           /* FWHM of window function as fraction of kernel length */
   int poly_order;        /* order of the fringe time interpolation polynomial */
   int sample_delay;      /* lag in clock ticks between samples and reference signals */
   int return_vel;        /* FTS return velocity */
   int max_data;          /* the maximum number of data points */
   int d_type;            /* data file type; 0:NSO6, 1:FITS */
} fts_parameters;         /* in addition to the ADC group delay */


/*
 * parameters pertaining to a particular scan
 */
typedef struct {
  /*
   * basic parameters
   */
   int spectype;                      /* type of spectrum 1=emission, 0=absorption*/
   int mode_is;                       /* A only, A-B, A-B/A+B, A,B separate */
   char beamsplitter[STR_LEN] ;       /* type of beamsplitter */
   char observer[STR_LEN];            /* who dunnit */
   char source[STR_LEN];              /* description of light source */
   char instrument[STR_LEN];          /* FTS instrument */
   char conditions[STR_LEN];          /* conditions of experiment */
   char input[STR_LEN];               /* side input or end input */
   char inp_filter[STR_LEN];          /* filter used on input */
   float focal_len;                   /* input focal length */
   float pspect;                      /* tank pressure in Pa */

   /*
    * sampling parameters
    */
   int alias;                         /* alias order */
   double fsr;                        /* free spectral range */
   float resol;                       /* resolution in cm^-1 */
   double sigma_low;                  /* lowest sigma of alias */
   double sigma_high;                 /* highest sigma of alias */
   double fdr;                        /* fringe division ratio */
   double decim;                      /* sample decimation */
   int nfringes;                      /* total number of fringes */
   int nsamp;                         /* total number of samples */
   float opd;                         /* max. optical path difference */
   int nscan;                         /* number of co-adds */
   int velocity;                      /* carriage velocity in fringes/sec */
   float aperture;                    /* max. aperture diameter */
   char detector[STR_LEN];            /* detector type on output */
   char filename[FILE_NAME_LEN];      /* output file name for channel */
   char detector_a[STR_LEN];
   char filter_a[STR_LEN];
   float gain_a;
   char detector_b[STR_LEN];
   char filter_b[STR_LEN];
   float gain_b;
   float modeff;                       /* modulation efficiency */
   char start_time[24];                /* starting time of scan */
   float etime;                        /* elapsed time for measurement */
   char comment0[80];
   char comment1[80];
} fts_scan_info;


/*
 * create a NSO type 6 data file header
 */
void
write_nso6_header(fts_scan_info *hdr, fts_parameters *fp, int np);


#endif /* _NSO6_H */

/* Revision history:
 * -----------------
 * $Log$
 */

