/*
 * a hack to convert BOMEM FTS data files to .dat files to go with
 * a NSO type 6 style header file.
 *
 * Perpetrator :  Ulf Griesmann
 */

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "nso6.h"

#define BUFFER_LEN   64*1024     /* number of integers in buffer */
#define SIGMA_LASER  15798.0025  /* wave number of HeNe laser */

/*
 * some guessed entries of the BOMEM data file header structure
 */
typedef struct {
   int unknown_1;
   int num_samples;     /* number of points in interferogram */
   double alias_low;    /* lowest wavenumber in alias */
   double alias_high;   /* highest wave number in alias */
   int alias;           /* alias number of scan */
   int num_scans;       /* number of scans + 1 (?) */
   char unknown_2[4];
   char resol[9];       /* resolution as character string */
} bomem_header;


int
main(int argc, char *argv[]) 
{
   FILE *in_file;
   FILE *out_file;
   char in_name[256];
   char out_name[256];
   char hdr_name[256];
   int buffer[BUFFER_LEN];
   char *cp;
   bomem_header hdr;
   fts_parameters fts;
   fts_scan_info scan;
   int ntot = 0;
   int nr;
   int npts;
   int k;
   int *ip;
   float *fp;
   int nread;
   float resolution;
   int fringes;

   if ( argc == 1 ) {
      printf("Error :  file name missing on command line.\n");
      exit(-1);
   }

   strcpy(in_name, argv[1]);
   strcpy(out_name, in_name);
   cp = index(out_name, '.');
   if (cp)
     *cp = '\0';
   strcpy(hdr_name, out_name);
   strcat(out_name, ".dat");

   /*
    * first read the BOMEM header and write a header file
    */
   in_file = fopen(in_name, "r");
   if (in_file == NULL) {
      printf("Error :  input file not found.\n");
      exit(-1);
   }
   fread(&hdr, sizeof(bomem_header), 1, in_file);
   fseek(in_file, 544, SEEK_SET);          /* skip header */   

   strcpy(fts.fts_name, "BOMEM");
   fts.sigma_laser = SIGMA_LASER;
   fts.flen_end = 1.0;
   fts.flen_side = 1.0;
   fts.flen_out = 1.0;
   fts.coadd_delay = 0;
   fts.adc_bits = 16;
   fts.daq_block_size = 0;
   fts.f_type = 0;
   fts.f_adaptive = 0;
   fts.vel_num = 1;
   fts.vel_range = 0.0;
   fts.kernel_length = 1;
   fts.fractional_bw = 100.0;
   fts.theta = 100.0;
   fts.poly_order = 1;
   fts.sample_delay = 0;
   fts.return_vel = 0;
   fts.max_data = 0;
   fts.d_type = 0;

   scan.spectype = 1;
   scan.mode_is = 0;
   strcpy(scan.beamsplitter, "Fused Silica");
   strcpy(scan.observer, "Unknown");
   strcpy(scan.source, "Unknown");
   strcpy(scan.instrument, "Unknown");
   strcpy(scan.conditions, "Unknown");
   strcpy(scan.input, "Unknown");
   strcpy(scan.inp_filter, "Unknown");
   scan.focal_len = 1.0;
   scan.pspect = 10000.0;
   scan.alias = hdr.alias;
   scan.fsr = hdr.alias_high - hdr.alias_low;
   sscanf(hdr.resol, "%f", &resolution);
   scan.resol = resolution;
   scan.sigma_low = hdr.alias_low;
   scan.sigma_high = hdr.alias_high;
   fringes = SIGMA_LASER / resolution;
   scan.fdr = rint((double)hdr.num_samples / (double)fringes);
   scan.decim = 1.0;
   scan.nfringes = fringes;
   scan.nsamp = hdr.num_samples;
   scan.opd = 1.0/resolution;
   scan.nscan = hdr.num_scans;
   scan.velocity = 1;
   scan.aperture = 1.0;
   strcpy(scan.detector, "Unknown");
   strcpy(scan.filename, hdr_name);
   strcpy(scan.detector_a, "Unknown");
   strcpy(scan.filter_a, "Unknown");
   scan.gain_a = 1.0;
   strcpy(scan.detector_b, "Unknown");
   strcpy(scan.filter_b, "Unknown");
   scan.gain_b = 1.0;
   scan.modeff = 1.0;
   strcpy(scan.start_time, "00:00:0.0");
   scan.etime = 1.0;
   strcpy(scan.comment0,"no comment");
   strcpy(scan.comment1,"no comment");

   write_nso6_header(&scan, &fts, hdr.num_samples);

   /*
    * now write the corresponding data file
    */
   out_file = fopen(out_name, "w");
   while(1) {
      if (hdr.num_samples - ntot < BUFFER_LEN)
	nread = npts - ntot;
      else
	nread = BUFFER_LEN;
      nr = fread(buffer, sizeof(int), nread, in_file);
      if (nr == 0 )
	break;
      ntot += nr;
      ip = (int *)buffer;
      fp = (float *)ip;
      for (k=0; k<nr; k++, ip++, fp++)
	*fp = (float)*ip;
      fwrite(buffer, sizeof(float), nr, out_file);
   }
   
   fclose(in_file);
   fclose(out_file);

   printf("    Number of data points :  %d\n", hdr.num_samples);

   return 0;
} 

/* Revision history:
 * -----------------
 * $Log$
 */
