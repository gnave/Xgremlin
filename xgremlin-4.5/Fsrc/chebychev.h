*
* common block for singular value decomposition (include after 'integrate.h')
*
      integer ncoef
      parameter (ncoef = 16)

      real bma, bpa

      real uu(mxpts,ncoef), vv(ncoef,ncoef), ww(ncoef)
      real chcoef(ncoef), std(ncoef)

      common /svddata/ uu, vv, ww
      common /svddata/ chcoef, std
      common /border/ bma, bpa

*----------------------------------------------------------------------
* ncoef : max number of Chebychev coefficients
* chisq : quality of fit
* u,v,w : auxiliary arrays for SVD
* chcoef: array of Chebychev coefficients
* bma   : b minus a; for transformation [a,b] --> [-1,1]
* bpa   : b plus a
