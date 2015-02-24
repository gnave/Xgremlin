*
* Used in reading header files
*
      integer nmax
      parameter(nmax = 150)  ! max. number of keyword/value pairs

      character hkey(nmax)*32, hvalue(nmax)*128
      integer idxmax

      common /nso_hdr/ hkey, hvalue, idxmax
      
