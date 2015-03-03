*
* statement functions used in Xgremlin for calculating wavenumbers from 
* data points and vice versa. The inline function wnum is identical to
* the subroutine 'ptow'.
*
* calculate wave numbers from points
      wnum(n) = wref + (n - pref) * delw

* calculate points from wave numbers
      nump(w) = 0.5d0 + pref + ( w - wref ) / delw

*
* the following variables must be declared before this file is included:
*
*       real w
*       double precision wnum
*       integer n, nump
*
* the include file 'infmtn.h' must be included too.
*
