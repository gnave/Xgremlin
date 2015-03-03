* -*-Fortran-*-
* common data for integrate.f module
*
      integer mbufsiz, ispline, igauss, ivoigt
      integer maxfit,maxpar,mxpts
      parameter (maxfit=10,maxpar=4*maxfit)
      parameter (mbufsiz = 64, mxpts = 4*1024) 
      parameter (lenwrk=mxpts*(maxpar+5)+maxpar)   ! < ffta_size <= 256k
      parameter (inone=1, ispline=2, igauss=3, ivoigt=4, ilorentz=5)
      real sqrtpi2, sqrtln2
      parameter(sqrtpi2=0.88622692545275794d0)   ! sqrt(pi) / 2
      parameter(sqrtln2=0.83255461115769769d0)   ! sqrt( ln(2) )

      logical lhold
      integer nbuf
      real wbuf(mbufsiz)               ! for saving several mouse markers
      real ybuf(mbufsiz)
      integer pbuf(mbufsiz)            ! points

      integer ptype, icbeg, icend
      integer npar
      real smooth, rvar

      real xint(mxpts)                 ! for spline fits
      real yint(mxpts)
      double precision dxint(mxpts/2)  ! for MINPACK fits
      double precision dyint(mxpts/2)
      real sigma(mxpts)
      real weigh(mxpts)
      real ys(mxpts)
      double precision dys(mxpts/2)
      real yp(mxpts)
      real yaux(mxpts)
      real wrkspc(lenwrk)

      integer ihold(maxpar)
      integer mapx(maxpar)
      integer msortx(maxpar)
      
      common /intdata/ wbuf, ybuf, pbuf, nbuf, mapx, ihold, msortx
      common /intdata/ ptype, icbeg, icend, smooth, rvar
      common /intdata/ xint, yint, sigma, weigh, ys, yp
      common /intdata/ yaux, npar, lhold
      common /intdata/ wrkspc

      equivalence (xint(1),dxint(1)), (yint(1),dyint(1)), (ys(1),dys(1))

*-----------------------------------------------------------------------------
* lhold     : T if parameters are modified from the command line
* ifree     : array that maps out the free parameters
* ifrees    : for storing ifree temporarily
* npar      : number of parameters in a nonlinear fit
* mbufsiz   : marker buffer size
* wbuf      : wavenumber positions of markers
* ybuf      : intensities (y-positions) of markers
* pbuf      : point at which marker is placed
* nbuf      : number of currently buffered points
* ptype     : type of profile ( one of the i.... above )
* icbeg     : first point of r saved into tr before background subtraction
* icend     : end of range saved into tr array
* smooth    : smoothing factor for spline fit
* rvar      : estimated noise of data (variance)
* xint      : buffer for spline fit
* yint      :  -"-
* sigma     : spline knots
* weigh     : array with weights
* yp        : array with 1st derivatives for spline fit
* yaux      : auxiliary array for auxiliary plots
* xpar      : parameter array for nonlinear fitting
* sxpar     : parameter array for saving fit parameters
* wrkspc    : work space for spline fit
*
* NOTE:
* The workspace for the fitting procedures is equivalenced to the
* ffta array which is not used when fitting data.

