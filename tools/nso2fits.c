/* 
 * Program to convert NSO type 6 data files to FITS format
 *
 * Gillian Nave, May, 2001
 *
 * This program is in the public domain
 */

#include <stdio.h>
#include <fcntl.h>


main(argc,argv)
     int argc;
     char *argv[];

{
  float c; 
  double wstart,delw, atof();
  FILE *fits, *fhdr, *fdat;
  char *y, z[4];
  int i,j,naxis1,nlin,zeropad;
  char header[32], data_file[32], fits_file[32], keyword[32],value[256],spect[32],day[32];
  c = 0;

  /* Open header file, data file and output file */

  strcpy(header, argv[1]);
  strcpy(data_file,argv[1]);
  strcpy(fits_file, argv[1]);
  strcat (header,".hdr");
  strcat (data_file,".dat");
  strcat (fits_file,".fits");

  if((fhdr = fopen(header, "r")) == NULL){
    printf("Can't open file %s\n", argv[1]);
    exit(-1);
  }

  if((fdat = fopen(data_file, "r")) == NULL){
    printf("Can't open file %s\n", argv[1]);
    exit(-1);
  }

  if((fits = fopen(fits_file, "w")) == NULL){
    printf("Can't open file %s\n", argv[1]);
    exit(-1);
  }

  /* Write out required keywords for fits file */
  while(1 ){
    fgets( value,sizeof(value),fhdr);
    sscanf(value,"%s",keyword); 
/*    printf("string = %s ;keyword = %s; value = \n", value, keyword,value+10); */
    if(strcmp(keyword,"npo")==0.){
      naxis1 = atoi(value+10);
      }
    if(strcmp(keyword,"wstart")==0.){
      wstart = atof(value+10);
      }
    if(strcmp(keyword,"delw")==0.){
      delw = atof(value+10);
      }
    if(strcmp(keyword,"spect")==0.){
      strncpy(spect, value+10,22);
      }
    if(strcmp(keyword,"day")==0.){
      strncpy(day, value+10,22);
      }
    if(strcmp(keyword,"END")==0)
      break;
  }

  rewind(fhdr);

  printf("Number of points in data file = %d\n", naxis1);
  fprintf(fits,"SIMPLE  = T                                                                     ");
  fprintf(fits,"BITPIX  = -32                                                                   ");
  fprintf(fits,"NAXIS   = 1                                                                     ");
  fprintf(fits,"NAXIS1  =  %10.10d                                                           ",naxis1);
  fprintf(fits,"ORIGIN  = %20.20s                                                  ", spect);
  fprintf(fits,"DATE    = %20.20s                                                  ", day);
  fprintf(fits,"CTYPE1  = 'LINEAR'                                                              ");
  fprintf(fits,"CRVAL1  = %15.12f                                                       ", delw);
  fprintf(fits,"CDELT1  = %12.6f                                                          ", wstart);
  fprintf(fits,"WAT0_001= 'system=linear  '                                                     ");
  fprintf(fits,"WAT1_001= 'wtype=linear units=millikaysers label=wavenumber '                   ");
  nlin=11;

  /* Copy header information, convert keywords to upper case */

  while( fgets( value,sizeof(value),fhdr)){
    sscanf(value,"%s",keyword); 
    for(i=0;i<strlen(keyword);i++)
      value[i]=toupper(keyword[i]);
    if(strcmp(keyword,"BOCODE")==0)
      fprintf(fits,"BOCODE  =         0             / 0: little endian, 1: big endian \n");
    else if(strlen(value)!=81){                 /* add one on for the new line char */
      if(strlen(value)>81)
          fprintf(fits,"%80.80s", value);
      else{
          zeropad = 81-strlen(value);
          for(i=0;i<strlen(value)-1;i++)
             fprintf(fits,"%1.1s",value+i);
          for(i=0;i<zeropad;i++)
            fprintf(fits," ");
         }
    }
    else
      fprintf(fits,"%80.80s", value);
    nlin++;
  }
  if(nlin - (nlin/36)*36){
    i = (nlin/36+1)*36 -nlin ;
    for (j=0;j<i;j++)
      fprintf(fits,"                                                                                ");
  }

  nlin=0;
  /* Read words, swap bytes and write out */
  while(fread((char *)&c, 4, 1, fdat) == 1){
    y = (char *)&c;
    z[0] = *(y+3);
    z[1] = *(y+2);
    z[2] = *(y+1);
    z[3] = *(y);

    if (fwrite((char *)z, 4, 1, fits) != 1){
      printf("File write error\n");
      exit(-1);
    }
    nlin++;

  }
  
  if(nlin - (nlin/720)*720) {
    i = (nlin/720+1)*720 - nlin;
    for (j=0;j<0;j++)
      fprintf(fits,"    ");
  }


  close(fits_file);
}
