      double precision function dvoigt( wlen, wcen, gwid, lwid )
*
* calculates the Voigt function at one wavelength
* wlen : wavelength
* wcen : centre of the Voigt function (line centre)
* gwid : Gaussian width
* lwid : Lorentzian width (collisional width)
*
* Reference:
* F. Schreier, J. Quant. Spectrosc. Radiat. Transf. Vol 48, 743 (1992)
*
* Author: Ulf Griesmann
*
      double precision srln2, srpi
      parameter (srln2 = 0.83255461115769769d0,
     &           srpi  = 1.77245385091d0 )
      double precision wlen, wcen, gwid, lwid
      double precision  r, w
      double complex v, cerrf

      external cerrf

      w = srln2 * ( wlen - wcen ) / gwid
      r = srln2 * lwid / gwid
      v = cerrf( w, r )

      dvoigt = srln2 * dble( v ) / ( srpi * gwid )

      return
      end

      double complex function cerrf(ce1,ce2)
c+++
c
c   Complex Error Function for calculation of the Voigt Function.
c   All relevant values are given in the call
c
c   References:
c     A.K.Hui, B.H.Armstrong, A.A.Wray
c     Rapid Computations of The Voigt And Complex Error Functions
c     J. Quant. Spectrosc. Radiat. Transfer. Vol. 19. (1978) 509-516
c
c   Input values:
c      CE1 : distance to center in units of the Gaussian linewidth
c      CE2 : half of ratio of Lorenzian to Gaussian linewidth
c
c   Program uses rational polynom of sixth degree with coefficients
c   A1-A7, B1-B7.
c
c TAKE CARE when using CERRF in spectroscopy :                        
c      The function is NOT normalised to unity. The integral          
c      INT(re(CERRF),-inf,+inf) = SQRT(pi)                            
c      im(CERRF) goes to 0 if z goes to +-infinity.                   
c
c---

      implicit double precision (a-h,o-z)
      parameter(a1=122.607931777104326d0,b1=122.607931773875350d0, 
     1          a2=214.382388694706425d0,b2=352.730625110963558d0,
     2          a3=181.928533092181549d0,b3=457.334478783897737d0,
     3          a4= 93.155580458138441d0,b4=348.703917719495792d0,
     4          a5= 30.180142196210589d0,b5=170.354001821091472d0,
     5          a6=  5.912626209773153d0,b6= 53.992906912940207d0,
     6          a7=  0.564189583562615d0,b7= 10.479857114260399d0 )
      double complex co, c1, c2
      co = dcmplx(ce2,-ce1)
      c1 = (((((a7*co+a6)*co+a5)*co+a4)*co+a3)*co+a2)*co+a1
      c2 = ((((((co+b7)*co+b6)*co+b5)*co+b4)*co+b3)*co+b2)*co+b1
      cerrf = c1 / c2

      return
      end

C********************************************************************
      subroutine vecvoigt(x,y,nx,fac,prb)
C********************************************************************
C     Calculates the Voigt-Function times a user-definied factor
C     fac with a relative accuracy better than 2*10-6.
C
C     If this subroutine is called several times with similar
C     values y, the numerically expensive coefficents a1..t8
C     are only calculated once. The coefficients are only recalculated
C     if the relative change in y is grater than the internal parameter
C     rel, which is set to 1e-4.
C--------------------------------------------------------------------
C     x(*)   (in)   : Distance from line center in units of Doppler
C                   : halfwidths times sqrt(ln 2)
C     y      (in)   : Ratio of the Doppler halfwidth to the Lorentz
C                   : halfwidth times sqrt(ln2)
C     nx     (in)   : Length of vector x and prb
C     fac    (in)   : A user defined factor
C     prb(*) (out)  : The Voigt-function times the user defined factor
C--------------------------------------------------------------------
C     author: Dr. Martin Kuntz,
C             Institut fuer Meteorologie und Klimaforschung,
C             Forschungszentrum Karlsruhe,
C             Postfach 3640,
C             76021 Karlsruhe, Germany
C             kuntz@imk.fzk.de
C
C     ref:    A new implementation of the Humlicek algorithm for the
C             calculation of the Voigt profile function, J. Quant.
C             Spectrosc. Radiat. Transfer, 57, 819-824, 1997
C********************************************************************
      parameter (rel = 1e-4)
      real    x(*), prb(*)
      integer lauf(1:4,4), stack(20,4), stackp, bmin, bmax, bfun
      save a8,b8,c8,d8,e8,f8,g8,h8,o8,p8,q8,r8,s8,t8,a7,b7,c7,d7,e7,f7,
     .     g7,h7,o7,p7,q7,r7,s7,t7,a6,b6,c6,d6,e6,a5,b5,c5,d5,e5,a4,b4,
     .     c4,d4,a3,b3,c3,d3,a2,a1,b1,yps4,yps3,yps2,yps1
      data yps1,yps2,yps3,yps4 /-1.,-1.,-1.,-1./

      y2 = y*y

      if (y.ge.15..or.x(1).ge.15..or.x(nx).le.-15.) then
	lauf(1,1) = 1
	lauf(1,2) = nx
	lauf(1,3) = nx
	lauf(1,4) = 0
	goto 7
      endif

      do 1 i2 = 1,4
	do 1 i1 = 1,4
	  lauf(i1,i2) = mod(i2,2)*(nx+1)
 1    continue

      stackp = 1
      stack(stackp,1) = 1
      stack(stackp,2) = nx
      stack(stackp,3) = bfun(y,x(1))
      stack(stackp,4) = bfun(y,x(nx))

 2    imin = stack(stackp,1)
      imax = stack(stackp,2)
      bmin = stack(stackp,3)
      bmax = stack(stackp,4)
      if (bmin.eq.bmax) then
	if (x(imax).lt.0.) then
	  lauf(bmin,1) = min(imin,lauf(bmin,1))
	  lauf(bmax,2) = max(imax,lauf(bmax,2))
	  stackp = stackp-1
	  goto 3
	elseif (x(imin).ge.0.) then
	  lauf(bmin,3) = min(imin,lauf(bmin,3))
	  lauf(bmax,4) = max(imax,lauf(bmax,4))
	  stackp = stackp-1
	  goto 3
	endif
      endif
      imitte = (imax+imin)/2
      stack(stackp,1) = imitte+1
      stack(stackp,2) = imax
      stack(stackp,3) = bfun(y,x(imitte+1))
      stack(stackp,4) = bmax
      stackp = stackp+1
      stack(stackp,1) = imin
      stack(stackp,2) = imitte
      stack(stackp,3) = bmin
      stack(stackp,4) = bfun(y,x(imitte))
 3    if (stackp.gt.0) goto 2

C---- Region 4
C--------------------------------------------------------------------
      if (lauf(4,2).ge.lauf(4,1).or.lauf(4,4).ge.lauf(4,3)) then
	if (abs((y-yps4)/yps4).gt.rel) then
	  yps4 = y
	  a7 = y*(1.16028e9+y2*(-9.86604e8+y2*(4.56662e8+y2*
     .     (-1.53575e8+y2*(4.08168e7+y2*(- 9.69463e6+y2*(1.6841e6+y2*
     .     (-320772.+y2*(40649.2+y2*(-5860.68+y2*(571.687+y2*(-72.9359
     .     +y2*(2.35944-y2*0.56419)))))))))))))
	  b7 = y*(-5.60505e8+y2*(-9.85386e8+y2*(8.06985e8+y2*
     .     (-2.91876e8+y2*(8.64829e7+y2*(-7.72359e6+y2*(3.59915e6+y2*
     .     (-234417.+y2*(45251.3+y2*(-2269.19+y2*(-234.143+y2*
     .     (23.0312-y2*7.33447))))))))))))
	  c7 = y*(-6.51523e8+y2*(2.47157e8+y2*(2.94262e8+y2*
     .     (-2.04467e8+y2*(2.29302e7+y2*(-2.3818e7+y2*(576054.+y2*
     .     (98079.1+y2*(-25338.3+y2*(1097.77+y2*
     .     (97.6203-y2*44.0068)))))))))))
	  d7 = y*(-2.63894e8+y2*(2.70167e8+y2*(-9.96224e7+y2*
     .     (-4.15013e7+y2*(3.83112e7+y2*(2.2404e6+y2*(-303569.+y2*
     .     (-66431.2+y2*(8381.97+y2*(228.563-y2*161.358))))))))))
	  e7 = y*(-6.31771e7+y2*(1.40677e8+y2*(5.56965e6+y2*
     .     (2.46201e7+y2*(468142.+y2*(-1.003e6+y2*(-66212.1+y2*
     .     (23507.6+y2*(296.38-y2*403.396)))))))))
	  f7 = y*(-1.69846e7+y2*(4.07382e6+y2*(-3.32896e7+y2*
     .     (-1.93114e6+y2*(-934717.+y2*(8820.94+y2*(37544.8+y2*
     .     (125.591-y2*726.113))))))))
	  g7 = y*(-1.23165e6+y2*(7.52883e6+y2*(-900010.+y2*(-186682.+
     .     y2*(79902.5+y2*(37371.9+y2*(-260.198-y2*968.15)))))))
	  h7 = y*(-610622.+y2*(86407.6+y2*(153468.+y2*(72520.9+y2*
     .     (23137.1+y2*(-571.645-y2*968.15))))))
	  o7 = y*(-23586.5+y2*(49883.8+y2*(26538.5+y2*(8073.15+y2*
     .     (-575.164-y2*726.113)))))
	  p7 = y*(-8009.1+y2*(2198.86+y2*(953.655+y2*
     .     (-352.467-y2*403.396))))
	  q7 = y*(-622.056+y2*(-271.202+y2*(-134.792-y2*161.358)))
	  r7 = y*(-77.0535+y2*(-29.7896-y2*44.0068))
	  s7 = y*(-2.92264-y2*7.33447)
	  t7 = y*(-0.56419)
	  a8 = 1.02827e9+y2*(-1.5599e9+y2*(1.17022e9+y2*(-5.79099e8+y2*
     .     (2.11107e8+y2*(-6.11148e7+y2*(1.44647e7+y2*(-2.85721e6+y2*
     .     (483737.+y2*(-70946.1+y2*(9504.65+y2*(-955.194+y2*(126.532
     .     +y2*(-3.68288+y2)))))))))))))
	  b8 = 1.5599e9+y2*(-2.28855e9+y2*(1.66421e9+y2*(-7.53828e8+y2*
     .     (2.89676e8+y2*(-7.01358e7+y2*(1.39465e7+y2*(-2.84954e6+y2*
     .     (498334.+y2*(-55600.+y2*(3058.26+y2*(533.254+y2*
     .     (-40.5117+y2*14.))))))))))))
	  c8 = 1.17022e9+y2*(-1.66421e9+y2*(1.06002e9+y2*(-6.60078e8+y2*
     .     (6.33496e7+y2*(-4.60396e7+y2*(1.4841e7+y2*(-1.06352e6+y2*
     .     (-217801.+y2*(48153.3+y2*(-1500.17+y2*
     .     (-198.876+y2*91)))))))))))
	  d8 = 5.79099e8+y2*(-7.53828e8+y2*(6.60078e8+y2*(5.40367e7+y2*
     .     (1.99846e8+y2*(-6.87656e6+y2*(-6.89002e6+y2*(280428.+y2*
     .     (161461.+y2*(-16493.7+y2*(-567.164+y2*364))))))))))
	  e8 = 2.11107e8+y2*(-2.89676e8+y2*(6.33496e7+y2*(-1.99846e8+y2*
     .     (-5.01017e7+y2*(-5.25722e6+y2*(1.9547e6+y2*(240373.+y2*
     .     (-55582.+y2*(-1012.79+y2*1001)))))))))
	  f8 = 6.11148e7+y2*(-7.01358e7+y2*(4.60396e7+y2*(-6.87656e6+y2*
     .     (5.25722e6+y2*(3.04316e6+y2*(123052.+y2*(-106663.+y2*
     .     (-1093.82+y2*2002))))))))
	  g8 = 1.44647e7+y2*(-1.39465e7+y2*(1.4841e7+y2*
     .     (6.89002e6+y2*(1.9547e6+y2*(-123052.+y2*(-131337.+y2*
     .     (-486.14+y2*3003)))))))
	  h8 = 2.85721e6+y2*(-2.84954e6+y2*(1.06352e6+y2*(280428.+y2*
     .     (-240373.+y2*(-106663.+y2*(486.14+y2*3432))))))
	  o8 = 483737.+y2*(-498334.+y2*(-217801.+y2*(-161461.+y2*
     .     (-55582.+y2*(1093.82+y2*3003)))))
	  p8 = 70946.1+y2*(-55600.+y2*(-48153.3+y2*(-16493.7+y2*
     .     (1012.79+y2*2002))))
	  q8 = 9504.65+y2*(-3058.26+y2*(-1500.17+y2*(567.164+y2*1001.)))
	  r8 = 955.194+y2*(533.254+y2*(198.876+y2*364))
	  s8 = 126.532+y2*(40.5117+y2*91.)
	  t8 = 3.68288+y2*14.
	endif
	ym2 = y*2
	do 4 i2 = 1,3,2
	  do 4 i1 = lauf(4,i2),lauf(4,i2+1)
	    x2 = x(i1)*x(i1)
	    prb(i1) = fac*(exp(y2-x2)*cos(x(i1)*ym2) -
     .         (a7+x2*(b7+x2*(c7+x2*(d7+x2*(e7+x2*(f7+x2*(g7+x2*(h7+x2*
     .         (o7+x2*(p7+x2*(q7+x2*(r7+x2*(s7+x2*t7))))))))))))) /
     .         (a8+x2*(b8+x2*(c8+x2*(d8+x2*(e8+x2*(f8+x2*(g8+x2*(h8+x2*
     .         (o8+x2*(p8+x2*(q8+x2*(r8+x2*(s8+x2*(t8+x2)))))))))))))))
 4      continue
      endif

C---- Region 3
C--------------------------------------------------------------------
      if (lauf(3,2).ge.lauf(3,1).or.lauf(3,4).ge.lauf(3,3)) then
	if (abs((y-yps3)/yps3).gt.rel) then
	  yps3 = y
	  a5 = (272.102+y*(973.778+y*(1629.76+y*(1678.33+y*(1174.8+y*
     .     (581.746+y*(204.501+y*(49.5213+y*(7.55895+y*0.564224)))))))))
	  b5 = (-60.5644+y*(-2.34403+y*(220.843+y*(336.364+y*(247.198
     .     +y*(100.705+y*(22.6778+y*2.25689)))))))
	  c5 = (4.58029+y*(18.546+y*(42.5683+y*(52.8454+y*(22.6798+y*
     .     3.38534)))))
	  d5 = (-0.128922+y*(1.66203+y*(7.56186+y*2.25689)))
	  e5 = (0.000971457+y*0.564224)
	  a6 = 272.102+y*(1280.83+y*(2802.87+y*(3764.97+y*(3447.63+y*
     .     (2256.98+y*(1074.41+y*(369.199+y*(88.2674+
     .     y*(13.3988+y)))))))))
	  b6 = 211.678+y*(902.306+y*(1758.34+y*(2037.31+y*(1549.68+y*
     .     (793.427+y*(266.299+y*(53.5952+y*5.)))))))
	  c6 = 78.866+y*(308.186+y*(497.302+y*(479.258+y*(269.292+y*
     .     (80.3928+y*10.)))))
	  d6 = 22.0353+y*(55.0293+y*(92.7568+y*(53.5952+y*10.)))
	  e6 = 1.49645+y*(13.3988+y*5.)
	endif
	do 5 i2 = 1,3,2
	  do 5 i1 = lauf(3,i2),lauf(3,i2+1)
	    x2 = x(i1)*x(i1)
	    prb(i1) = fac*(a5+x2*(b5+x2*(c5+x2*(d5+x2*e5))))/
     .                (a6+x2*(b6+x2*(c6+x2*(d6+x2*(e6+x2)))))
 5      continue
      endif

C---- Region 2
C--------------------------------------------------------------------
      if (lauf(2,2).ge.lauf(2,1).or.lauf(2,4).ge.lauf(2,3)) then
	if (abs((y-yps2)/yps2).gt.rel) then
	  yps2 = y
	  a3 = y*(1.05786+y2*(4.65456+y2*(3.10304+y2*0.56419)))
	  b3 = y*(2.962+y2*(0.56419+y2*1.69257))
	  c3 = y*(1.69257*y2-2.53885)
	  d3 = y*(0.56419)
	  a4 = 0.5625+y2*(4.5+y2*(10.5+y2*(6.+y2)))
	  b4 = -4.5+y2*(9.+y2*(6.+y2*4.))
	  c4 = 10.5+y2*(-6.+y2*6.)
	  d4 = -6.+y2*4.
	endif
	do 6 i2 = 1,3,2
	  do 6 i1 = lauf(2,i2),lauf(2,i2+1)
	    x2 = x(i1)*x(i1)
	    prb(i1) = fac*(a3+x2*(b3+x2*(c3+x2*d3)))/
     .                (a4+x2*(b4+x2*(c4+x2*(d4+x2))))
 6      continue
      endif

C---- Region 1
C--------------------------------------------------------------------
 7    if (lauf(1,2).ge.lauf(1,1).or.lauf(1,4).ge.lauf(1,3)) then
	if (abs((y-yps1)/yps1).gt.rel) then
	  yps1 = y
	  a1 = 0.5641896*y
	  b1 = 0.5+y2
	  a2 = 4*y2
	endif
	c1 = fac*a1
	do 8 i2 = 1,3,2
	  do 8 i1 = lauf(1,i2),lauf(1,i2+1)
	    x2 = x(i1)*x(i1)
	    b2 = b1-x2
	    prb(i1) = c1*(b1+x2)/(b2*b2+a2*x2)
 8      continue
      endif
      end

C--------------------------------------------------------------------
      integer function bfun(y,x)
C--------------------------------------------------------------------
      s = abs(x)+y
      if (s.ge.15) then
	bfun = 1
      elseif (s.ge.5.5) then
	bfun = 2
      elseif (y.ge.(0.195*abs(x))-0.176) then
	bfun = 3
      else
	bfun = 4
      endif
      end
