c \grammy\src\outparms.inc

c Version  1.01  09.10.92  replace ampfl with imagfl
c Version  1.00  18.04.91 

	common/outparms/wlstart,wlstop,lenout
	double precision wlstart,wlstop
	integer*4 lenout

c output flags
	common/outflg/phcorfl,rclphz,imagfl,realfl,cmplex,wvflag,acorrf
	logical       phcorfl,rclphz,imagfl,realfl,cmplex,wvflag,acorrf


c stats parameters
	common/statpar/amean, bmean, rmsa, rmsb,
     *		 absamax,amax,absbmax,bmax
	double precision amean, bmean, rmsa, rmsb
	real absamax,amax,absbmax,bmax

c aircorr parameters
      common/airvar/acorrp,acorrt,gcorr,acorrh 
c      dimension avar(4)                                                 
c      equivalence (avar(1),acorrp)                                      
