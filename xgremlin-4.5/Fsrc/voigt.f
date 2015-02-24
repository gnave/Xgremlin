c \grammy\src\voigt.for   Ver. 2.2

c  Revision 2.21 90/02/17   use nrtr+2, not 4  

	subroutine voigt

	include 'datetc.h'
	include 'linparms.h'

	common/vstrt/c,ca,cb,cc,cd,ce,ulim,vf,lastv
	real c,ca,cb,cc,cd,ce,ulim,vf(50,26)
	real u, ut, zm, g
	integer n

c ** bt in xparb, u in xparc, return vf in xpara

	u = xparc
	if (lastv .eq. 2) go to 40
5	xpara = 1.0 / (1.0 + u * u)
	return
  
40	if (u .eq. 0.) go to 5
	if (u .lt. 0.) u = -u
	if (u .ge. ulim) go to 60
	if (u .ge. 4.7) go to 50
	ut = 10.0 * u
	n = ut
	zm = ut - n
	xpara = vt(n+1) + zm * (vt(n+2) - vt(n+1) + (zm - 1.)
     &		* ((zm + 1.) * dm(n+2) + (2. - zm) * dm(n+1)) / 6.0)
	return
  
50	g = 1.0 / (u * u + c)
	xpara = g * (ca + g *(cb + g * (cc + g *(cd + g * ce))))
	return
  
60	xpara = ca / (u * u + c)
	return
	end

c ---------------------------------------------------------------
	
	subroutine vstart

	include 'datetc.h'
	include 'linparms.h'

	common/vstrt/c,ca,cb,cc,cd,ce,ulim,vf,lastv
	real c,ca,cb,cc,cd,ce,ulim,vf(50,26)
	real xd,tb,b,a,pt,aa
	integer*4 ib, i

c ** initialize - compute constants for series, differences for interpolation

	lastv = 1
	if (xparb .lt. 1.0) xparb = 1.0
	if (xparb .ge. 25.99999) go to 35
	lastv = 2
	if (xparb .eq. xlast) go to 35
	xlast = xparb
	ib = xparb
	xd = xparb - ib
	do 20 i = 1, 50
20	    vt(i) = vf(i,ib) + xd * (vf(i,ib+1) - vf(i,ib))
	ds(1) = 2.0 * (vt(2) - vt(1))
	do 25 i= 2, 49
25	    ds(i) = vt(i+1) - 2.0 * vt(i) + vt(i-1)
	tb = 0.1809017
	dm(1) = ds(1) - tb * 2.0 * (ds(2) - ds(1))
	do 30 i= 2, 48
30	    dm(i) = ds(i) - tb * (ds(i+1) - 2.0 * ds(i) + ds(i-1))
	b = (xparb - 1.0) * 0.04
	a = bgg(2*ib-1) + xd * (bgg(2*ib+1) - bgg(2*ib-1))
	pt = p(ib) + xd * (p(ib+1) - p(ib))
	aa = a * a
	c = 2.0 * a + b * b
	ca = b * pt * 0.6366198
	cb = 8.0 * a * ca
	cc = cb * (13.0 * a - c)
	cd = ca * 32.0 * aa * (58.0 * a - 9.0 * c)
	ce = ca * 64.0 * aa * (655.0 * aa - 150.0 * a * c + 3.0 * c * c)
	ulim = (1.0e6 * cb)**0.250
	if (ulim .lt. 4.7) ulim = 4.7
35	return
  
	end

c ---------------------------------------------------------------

	subroutine voit(r,tr)
*
* computes a Voigt profile by calculating the product of a
* Gaussian and a Lorentzian and Fourier transforming
*
	include 'datetc.h'
	include 'infmtn.h'

	real r(*), tr(*)

	integer i
	double precision temp, pi

	nwpp = 1
	if (ifx .eq. 0) inum(1) = nrtr + 2
	nop = min(inum(1), (nrtr + 2))
	if (ifl .ne. 1) go to 20
	iparb = 1 + nop / 2
	if (ifx .eq. 1) go to 10
	iparb = inum(2)
 10	xpara = 0.5 * xnum(1)
c					generate witch of width 2*xpara
c					centered on iparb
	do 15 i=1,nop
	temp = float(i-iparb)/xpara
 15	r(i) = 1.0/(1.0 + temp*temp)
	return
  
 20	call strtrn
	pi = 3.1415926535898
	temp = (2.0d+0 * pi) / np
	xpara = temp * 0.5*xnum(1)
	xparb = 0.5 * temp * 0.6005614*xnum(2)
	xparb = xparb * xparb
  
	do i = 1, nppt, 2
	   temp = 0.5 * (i - 1)
	   temp = -temp * (xpara + temp * xparb)
c						test for floating underflow
	   if (temp .lt. -84.0) go to 29
	   tr(i) = exp(temp)
	   go to 30
 29	   tr(i)   = 0.0
 30	   tr(i+1) = 0.0
	end do
  
	call rext(r,tr)
	call invtrn(r)
	xpara = 1.0 / r(nh+1)
c						multiply r(i) * xpara
	do 100 i=1,nop
100	r(i) = xpara*r(i)
	return
  
	end
c ---------------------------------------------------------------
	BLOCK DATA VTAB

	common/vstrt/c,ca,cb,cc,cd,ce,ulim,vf,lastv
	real c,ca,cb,cc,cd,ce,ulim,vf(50,26)

	data  vf( 1, 1),  vf( 2, 1),  vf( 3, 1),  vf( 4, 1),  vf( 5, 1)
     &	/ 1.000000, 0.993092, 0.972655, 0.939523, 0.895025	/
	data  vf( 6, 1),  vf( 7, 1),  vf( 8, 1),  vf( 9, 1),  vf(10, 1)
     &	/ 0.840896, 0.779165, 0.712025, 0.641713, 0.570382	/
	data  vf(11, 1),  vf(12, 1),  vf(13, 1),  vf(14, 1),  vf(15, 1)
     &	/ 0.500000, 0.432269, 0.368567, 0.309927, 0.257028	/
	data  vf(16, 1),  vf(17, 1),  vf(18, 1),  vf(19, 1),  vf(20, 1)
     &	/ 0.210224, 0.169576, 0.134904, 0.105843, 0.081900	/
	data  vf(21, 1),  vf(22, 1),  vf(23, 1),  vf(24, 1),  vf(25, 1)
     &	/ 0.062500, 0.047039, 0.034915, 0.025559, 0.018453	/
	data  vf(26, 1),  vf(27, 1),  vf(28, 1),  vf(29, 1),  vf(30, 1)
     &	/ 0.013139, 0.009227, 0.006390, 0.004364, 0.002940	/
	data  vf(31, 1),  vf(32, 1),  vf(33, 1),  vf(34, 1),  vf(35, 1)
     &	/ 0.001953, 0.001280, 0.000827, 0.000527, 0.000331	/
	data  vf(36, 1),  vf(37, 1),  vf(38, 1),  vf(39, 1),  vf(40, 1)
     &	/ 0.000205, 0.000126, 0.000076, 0.000045, 0.000026	/
	data  vf(41, 1),  vf(42, 1),  vf(43, 1),  vf(44, 1),  vf(45, 1)
     &	/ 0.000015, 0.000009, 0.000005, 0.000003, 0.000001	/
	data  vf(46, 1),  vf(47, 1),  vf(48, 1),  vf(49, 1),  vf(50, 1)
     &	/ 0.000001, 0.      , 0.      , 0.      , 0.      	/

	data  vf( 1, 2),  vf( 2, 2),  vf( 3, 2),  vf( 4, 2),  vf( 5, 2)
     &	/ 1.000000, 0.993057, 0.972521, 0.939244, 0.894586	/
	data  vf( 6, 2),  vf( 7, 2),  vf( 8, 2),  vf( 9, 2),  vf(10, 2)
     &	/ 0.840314, 0.778491, 0.711346, 0.641137, 0.570032	/
	data  vf(11, 2),  vf(12, 2),  vf(13, 2),  vf(14, 2),  vf(15, 2)
     &	/ 0.500000, 0.432733, 0.369590, 0.311573, 0.259330	/
	data  vf(16, 2),  vf(17, 2),  vf(18, 2),  vf(19, 2),  vf(20, 2)
     &	/ 0.213177, 0.173144, 0.139023, 0.110429, 0.086852	/
	data  vf(21, 2),  vf(22, 2),  vf(23, 2),  vf(24, 2),  vf(25, 2)
     &	/ 0.067714, 0.052408, 0.040342, 0.030955, 0.023744	/
	data  vf(26, 2),  vf(27, 2),  vf(28, 2),  vf(29, 2),  vf(30, 2)
     &	/ 0.018267, 0.014146, 0.011072, 0.008791, 0.007104	/
	data  vf(31, 2),  vf(32, 2),  vf(33, 2),  vf(34, 2),  vf(35, 2)
     &	/ 0.005855, 0.004928, 0.004232, 0.003705, 0.003297	/
	data  vf(36, 2),  vf(37, 2),  vf(38, 2),  vf(39, 2),  vf(40, 2)
     &	/ 0.002975, 0.002715, 0.002501, 0.002321, 0.002165	/
	data  vf(41, 2),  vf(42, 2),  vf(43, 2),  vf(44, 2),  vf(45, 2)
     &	/ 0.002029, 0.001909, 0.001801, 0.001703, 0.001614	/
	data  vf(46, 2),  vf(47, 2),  vf(48, 2),  vf(49, 2),  vf(50, 2)
     &	/ 0.001533, 0.001458, 0.001388, 0.001324, 0.001265	/

	data  vf( 1, 3),  vf( 2, 3),  vf( 3, 3),  vf( 4, 3),  vf( 5, 3)
     &	/ 1.000000, 0.993020, 0.972379, 0.938951, 0.894124	/
	data  vf( 6, 3),  vf( 7, 3),  vf( 8, 3),  vf( 9, 3),  vf(10, 3)
     &	/ 0.839702, 0.777785, 0.710635, 0.640535, 0.569666	/
	data  vf(11, 3),  vf(12, 3),  vf(13, 3),  vf(14, 3),  vf(15, 3)
     &	/ 0.500000, 0.433215, 0.370649, 0.313271, 0.261696	/
	data  vf(16, 3),  vf(17, 3),  vf(18, 3),  vf(19, 3),  vf(20, 3)
     &	/ 0.216204, 0.176790, 0.143221, 0.115087, 0.091868	/
	data  vf(21, 3),  vf(22, 3),  vf(23, 3),  vf(24, 3),  vf(25, 3)
     &	/ 0.072979, 0.057817, 0.045796, 0.036368, 0.029043	/
	data  vf(26, 3),  vf(27, 3),  vf(28, 3),  vf(29, 3),  vf(30, 3)
     &	/ 0.023395, 0.019062, 0.015748, 0.013211, 0.011263	/
	data  vf(31, 3),  vf(32, 3),  vf(33, 3),  vf(34, 3),  vf(35, 3)
     &	/ 0.009756, 0.008578, 0.007645, 0.006893, 0.006276	/
	data  vf(36, 3),  vf(37, 3),  vf(38, 3),  vf(39, 3),  vf(40, 3)
     &	/ 0.005762, 0.005326, 0.004950, 0.004622, 0.004331	/
	data  vf(41, 3),  vf(42, 3),  vf(43, 3),  vf(44, 3),  vf(45, 3)
     &	/ 0.004072, 0.003838, 0.003626, 0.003433, 0.003257	/
	data  vf(46, 3),  vf(47, 3),  vf(48, 3),  vf(49, 3),  vf(50, 3)
     &	/ 0.003094, 0.002944, 0.002806, 0.002677, 0.002558	/

	data  vf( 1, 4),  vf( 2, 4),  vf( 3, 4),  vf( 4, 4),  vf( 5, 4)
     &	/ 1.000000, 0.992981, 0.972230, 0.938642, 0.893637	/
	data  vf( 6, 4),  vf( 7, 4),  vf( 8, 4),  vf( 9, 4),  vf(10, 4)
     &	/ 0.839058, 0.777044, 0.709890, 0.639905, 0.569285	/
	data  vf(11, 4),  vf(12, 4),  vf(13, 4),  vf(14, 4),  vf(15, 4)
     &	/ 0.500000, 0.433716, 0.371744, 0.315023, 0.264129	/
	data  vf(16, 4),  vf(17, 4),  vf(18, 4),  vf(19, 4),  vf(20, 4)
     &	/ 0.219307, 0.180516, 0.147496, 0.119816, 0.096945	/
	data  vf(21, 4),  vf(22, 4),  vf(23, 4),  vf(24, 4),  vf(25, 4)
     &	/ 0.078294, 0.063263, 0.051274, 0.041794, 0.034347	/
	data  vf(26, 4),  vf(27, 4),  vf(28, 4),  vf(29, 4),  vf(30, 4)
     &	/ 0.028522, 0.023973, 0.020417, 0.017626, 0.015419	/
	data  vf(31, 4),  vf(32, 4),  vf(33, 4),  vf(34, 4),  vf(35, 4)
     &	/ 0.013657, 0.012232, 0.011064, 0.010092, 0.009272	/
	data  vf(36, 4),  vf(37, 4),  vf(38, 4),  vf(39, 4),  vf(40, 4)
     &	/ 0.008569, 0.007959, 0.007424, 0.006949, 0.006525	/
	data  vf(41, 4),  vf(42, 4),  vf(43, 4),  vf(44, 4),  vf(45, 4)
     &	/ 0.006143, 0.005797, 0.005482, 0.005194, 0.004930	/
	data  vf(46, 4),  vf(47, 4),  vf(48, 4),  vf(49, 4),  vf(50, 4)
     &	/ 0.004686, 0.004461, 0.004252, 0.004059, 0.003879	/

	data  vf( 1, 5),  vf( 2, 5),  vf( 3, 5),  vf( 4, 5),  vf( 5, 5)
     &	/ 1.000000, 0.992940, 0.972072, 0.938316, 0.893124	/
	data  vf( 6, 5),  vf( 7, 5),  vf( 8, 5),  vf( 9, 5),  vf(10, 5)
     &	/ 0.838380, 0.776264, 0.709107, 0.639246, 0.568887	/
	data  vf(11, 5),  vf(12, 5),  vf(13, 5),  vf(14, 5),  vf(15, 5)
     &	/ 0.500000, 0.434236, 0.372878, 0.316832, 0.266633	/
	data  vf(16, 5),  vf(17, 5),  vf(18, 5),  vf(19, 5),  vf(20, 5)
     &	/ 0.222488, 0.184324, 0.151849, 0.124617, 0.102084	/
	data  vf(21, 5),  vf(22, 5),  vf(23, 5),  vf(24, 5),  vf(25, 5)
     &	/ 0.083658, 0.068744, 0.056776, 0.047233, 0.039654	/
	data  vf(26, 5),  vf(27, 5),  vf(28, 5),  vf(29, 5),  vf(30, 5)
     &	/ 0.033646, 0.028878, 0.025080, 0.022035, 0.019571	/
	data  vf(31, 5),  vf(32, 5),  vf(33, 5),  vf(34, 5),  vf(35, 5)
     &	/ 0.017557, 0.015890, 0.014491, 0.013304, 0.012283	/
	data  vf(36, 5),  vf(37, 5),  vf(38, 5),  vf(39, 5),  vf(40, 5)
     &	/ 0.011395, 0.010614, 0.009922, 0.009304, 0.008747	/
	data  vf(41, 5),  vf(42, 5),  vf(43, 5),  vf(44, 5),  vf(45, 5)
     &	/ 0.008244, 0.007786, 0.007368, 0.006985, 0.006633	/
	data  vf(46, 5),  vf(47, 5),  vf(48, 5),  vf(49, 5),  vf(50, 5)
     &	/ 0.006308, 0.006007, 0.005728, 0.005470, 0.005228	/

	data  vf( 1, 6),  vf( 2, 6),  vf( 3, 6),  vf( 4, 6),  vf( 5, 6)
     &	/ 1.000000, 0.992896, 0.971905, 0.937970, 0.892581	/
	data  vf( 6, 6),  vf( 7, 6),  vf( 8, 6),  vf( 9, 6),  vf(10, 6)
     &	/ 0.837664, 0.775442, 0.708285, 0.638554, 0.568470	/
	data  vf(11, 6),  vf(12, 6),  vf(13, 6),  vf(14, 6),  vf(15, 6)
     &	/ 0.500000, 0.434776, 0.374053, 0.318699, 0.269209	/
	data  vf(16, 6),  vf(17, 6),  vf(18, 6),  vf(19, 6),  vf(20, 6)
     &	/ 0.225750, 0.188214, 0.156282, 0.129489, 0.107281	/
	data  vf(21, 6),  vf(22, 6),  vf(23, 6),  vf(24, 6),  vf(25, 6)
     &	/ 0.089067, 0.074258, 0.062298, 0.052680, 0.044963	/
	data  vf(26, 6),  vf(27, 6),  vf(28, 6),  vf(29, 6),  vf(30, 6)
     &	/ 0.038767, 0.033777, 0.029736, 0.026438, 0.023721	/
	data  vf(31, 6),  vf(32, 6),  vf(33, 6),  vf(34, 6),  vf(35, 6)
     &	/ 0.021458, 0.019552, 0.017928, 0.016529, 0.015311	/
	data  vf(36, 6),  vf(37, 6),  vf(38, 6),  vf(39, 6),  vf(40, 6)
     &	/ 0.014241, 0.013292, 0.012445, 0.011685, 0.010998	/
	data  vf(41, 6),  vf(42, 6),  vf(43, 6),  vf(44, 6),  vf(45, 6)
     &	/ 0.010374, 0.009805, 0.009285, 0.008807, 0.008366	/
	data  vf(46, 6),  vf(47, 6),  vf(48, 6),  vf(49, 6),  vf(50, 6)
     &	/ 0.007960, 0.007584, 0.007234, 0.006909, 0.006606	/

	data  vf( 1, 7),  vf( 2, 7),  vf( 3, 7),  vf( 4, 7),  vf( 5, 7)
     &	/ 1.000000, 0.992849, 0.971727, 0.937603, 0.892006	/
	data  vf( 6, 7),  vf( 7, 7),  vf( 8, 7),  vf( 9, 7),  vf(10, 7)
     &	/ 0.836906, 0.774574, 0.707418, 0.637827, 0.568034	/
	data  vf(11, 7),  vf(12, 7),  vf(13, 7),  vf(14, 7),  vf(15, 7)
     &	/ 0.500000, 0.435338, 0.375272, 0.320628, 0.271861	/
	data  vf(16, 7),  vf(17, 7),  vf(18, 7),  vf(19, 7),  vf(20, 7)
     &	/ 0.229095, 0.192188, 0.160794, 0.134431, 0.112537	/
	data  vf(21, 7),  vf(22, 7),  vf(23, 7),  vf(24, 7),  vf(25, 7)
     &	/ 0.094521, 0.079802, 0.067837, 0.058135, 0.050271	/
	data  vf(26, 7),  vf(27, 7),  vf(28, 7),  vf(29, 7),  vf(30, 7)
     &	/ 0.043882, 0.038668, 0.034384, 0.030835, 0.027868	/
	data  vf(31, 7),  vf(32, 7),  vf(33, 7),  vf(34, 7),  vf(35, 7)
     &	/ 0.025360, 0.023220, 0.021375, 0.019768, 0.018357	/
	data  vf(36, 7),  vf(37, 7),  vf(38, 7),  vf(39, 7),  vf(40, 7)
     &	/ 0.017107, 0.015994, 0.014995, 0.014094, 0.013277	/
	data  vf(41, 7),  vf(42, 7),  vf(43, 7),  vf(44, 7),  vf(45, 7)
     &	/ 0.012533, 0.011854, 0.011232, 0.010659, 0.010131	/
	data  vf(46, 7),  vf(47, 7),  vf(48, 7),  vf(49, 7),  vf(50, 7)
     &	/ 0.009642, 0.009190, 0.008769, 0.008378, 0.008013	/

	data  vf( 1, 8),  vf( 2, 8),  vf( 3, 8),  vf( 4, 8),  vf( 5, 8)
     &	/ 1.000000, 0.992799, 0.971538, 0.937214, 0.891395	/
	data  vf( 6, 8),  vf( 7, 8),  vf( 8, 8),  vf( 9, 8),  vf(10, 8)
     &	/ 0.836103, 0.773657, 0.706504, 0.637063, 0.567577	/
	data  vf(11, 8),  vf(12, 8),  vf(13, 8),  vf(14, 8),  vf(15, 8)
     &	/ 0.500000, 0.435924, 0.376536, 0.322622, 0.274590	/
	data  vf(16, 8),  vf(17, 8),  vf(18, 8),  vf(19, 8),  vf(20, 8)
     &	/ 0.232525, 0.196247, 0.165386, 0.139442, 0.117848	/
	data  vf(21, 8),  vf(22, 8),  vf(23, 8),  vf(24, 8),  vf(25, 8)
     &	/ 0.100015, 0.085372, 0.073390, 0.063594, 0.055576	/
	data  vf(26, 8),  vf(27, 8),  vf(28, 8),  vf(29, 8),  vf(30, 8)
     &	/ 0.048990, 0.043550, 0.039024, 0.035227, 0.032012	/
	data  vf(31, 8),  vf(32, 8),  vf(33, 8),  vf(34, 8),  vf(35, 8)
     &	/ 0.029265, 0.026895, 0.024832, 0.023022, 0.021421	/
	data  vf(36, 8),  vf(37, 8),  vf(38, 8),  vf(39, 8),  vf(40, 8)
     &	/ 0.019996, 0.018719, 0.017570, 0.016530, 0.015585	/
	data  vf(41, 8),  vf(42, 8),  vf(43, 8),  vf(44, 8),  vf(45, 8)
     &	/ 0.014723, 0.013934, 0.013209, 0.012542, 0.011926	/
	data  vf(46, 8),  vf(47, 8),  vf(48, 8),  vf(49, 8),  vf(50, 8)
     &	/ 0.011355, 0.010826, 0.010335, 0.009876, 0.009449	/

	data  vf( 1, 9),  vf( 2, 9),  vf( 3, 9),  vf( 4, 9),  vf( 5, 9)
     &	/ 1.000000, 0.992746, 0.971337, 0.936798, 0.890745	/
	data  vf( 6, 9),  vf( 7, 9),  vf( 8, 9),  vf( 9, 9),  vf(10, 9)
     &	/ 0.835251, 0.772684, 0.705538, 0.636258, 0.567096	/
	data  vf(11, 9),  vf(12, 9),  vf(13, 9),  vf(14, 9),  vf(15, 9)
     &	/ 0.500000, 0.436534, 0.377849, 0.324684, 0.277401	/
	data  vf(16, 9),  vf(17, 9),  vf(18, 9),  vf(19, 9),  vf(20, 9)
     &	/ 0.236042, 0.200392, 0.170056, 0.144520, 0.123211	/
	data  vf(21, 9),  vf(22, 9),  vf(23, 9),  vf(24, 9),  vf(25, 9)
     &	/ 0.105547, 0.090966, 0.078954, 0.069055, 0.060877	/
	data  vf(26, 9),  vf(27, 9),  vf(28, 9),  vf(29, 9),  vf(30, 9)
     &	/ 0.054090, 0.048423, 0.043656, 0.039613, 0.036156	/
	data  vf(31, 9),  vf(32, 9),  vf(33, 9),  vf(34, 9),  vf(35, 9)
     &	/ 0.033172, 0.030577, 0.028301, 0.026291, 0.024504	/
	data  vf(36, 9),  vf(37, 9),  vf(38, 9),  vf(39, 9),  vf(40, 9)
     &	/ 0.022906, 0.021470, 0.020172, 0.018994, 0.017922	/
	data  vf(41, 9),  vf(42, 9),  vf(43, 9),  vf(44, 9),  vf(45, 9)
     &	/ 0.016942, 0.016043, 0.015217, 0.014455, 0.013751	/
	data  vf(46, 9),  vf(47, 9),  vf(48, 9),  vf(49, 9),  vf(50, 9)
     &	/ 0.013099, 0.012493, 0.011929, 0.011404, 0.010913	/

	data  vf( 1,10),  vf( 2,10),  vf( 3,10),  vf( 4,10),  vf( 5,10)
     &	/ 1.000000, 0.992689, 0.971121, 0.936354, 0.890051	/
	data  vf( 6,10),  vf( 7,10),  vf( 8,10),  vf( 9,10),  vf(10,10)
     &	/ 0.834343, 0.771652, 0.704515, 0.635408, 0.566591	/
	data  vf(11,10),  vf(12,10),  vf(13,10),  vf(14,10),  vf(15,10)
     &	/ 0.500000, 0.437171, 0.379212, 0.326816, 0.280295	/
	data  vf(16,10),  vf(17,10),  vf(18,10),  vf(19,10),  vf(20,10)
     &	/ 0.239647, 0.204624, 0.174805, 0.149663, 0.128625	/
	data  vf(21,10),  vf(22,10),  vf(23,10),  vf(24,10),  vf(25,10)
     &	/ 0.111114, 0.096580, 0.084527, 0.074515, 0.066170	/
	data  vf(26,10),  vf(27,10),  vf(28,10),  vf(29,10),  vf(30,10)
     &	/ 0.059180, 0.053286, 0.048280, 0.043995, 0.040298	/
	data  vf(31,10),  vf(32,10),  vf(33,10),  vf(34,10),  vf(35,10)
     &	/ 0.037083, 0.034267, 0.031782, 0.029576, 0.027607	/
	data  vf(36,10),  vf(37,10),  vf(38,10),  vf(39,10),  vf(40,10)
     &	/ 0.025839, 0.024244, 0.022800, 0.021487, 0.020288	/
	data  vf(41,10),  vf(42,10),  vf(43,10),  vf(44,10),  vf(45,10)
     &	/ 0.019191, 0.018184, 0.017256, 0.016400, 0.015607	/
	data  vf(46,10),  vf(47,10),  vf(48,10),  vf(49,10),  vf(50,10)
     &	/ 0.014873, 0.014190, 0.013554, 0.012961, 0.012407	/

	data  vf( 1,11),  vf( 2,11),  vf( 3,11),  vf( 4,11),  vf( 5,11)
     &	/ 1.000000, 0.992628, 0.970889, 0.935878, 0.889309	/
	data  vf( 6,11),  vf( 7,11),  vf( 8,11),  vf( 9,11),  vf(10,11)
     &	/ 0.833373, 0.770552, 0.703430, 0.634509, 0.566059	/
	data  vf(11,11),  vf(12,11),  vf(13,11),  vf(14,11),  vf(15,11)
     &	/ 0.500000, 0.437836, 0.380630, 0.329022, 0.283276	/
	data  vf(16,11),  vf(17,11),  vf(18,11),  vf(19,11),  vf(20,11)
     &	/ 0.243344, 0.208943, 0.179630, 0.154869, 0.134086	/
	data  vf(21,11),  vf(22,11),  vf(23,11),  vf(24,11),  vf(25,11)
     &	/ 0.116711, 0.102210, 0.090103, 0.079971, 0.071455	/
	data  vf(26,11),  vf(27,11),  vf(28,11),  vf(29,11),  vf(30,11)
     &	/ 0.064259, 0.058138, 0.052896, 0.048372, 0.044440	/
	data  vf(31,11),  vf(32,11),  vf(33,11),  vf(34,11),  vf(35,11)
     &	/ 0.040998, 0.037965, 0.035276, 0.032879, 0.030730	/
	data  vf(36,11),  vf(37,11),  vf(38,11),  vf(39,11),  vf(40,11)
     &	/ 0.028794, 0.027044, 0.025455, 0.024007, 0.022684	/
	data  vf(41,11),  vf(42,11),  vf(43,11),  vf(44,11),  vf(45,11)
     &	/ 0.021470, 0.020354, 0.019325, 0.018375, 0.017494	/
	data  vf(46,11),  vf(47,11),  vf(48,11),  vf(49,11),  vf(50,11)
     &	/ 0.016677, 0.015917, 0.015208, 0.014547, 0.013929	/

	data  vf( 1,12),  vf( 2,12),  vf( 3,12),  vf( 4,12),  vf( 5,12)
     &	/ 1.000000, 0.992562, 0.970640, 0.935365, 0.888512	/
	data  vf( 6,12),  vf( 7,12),  vf( 8,12),  vf( 9,12),  vf(10,12)
     &	/ 0.832335, 0.769378, 0.702275, 0.633556, 0.565497	/
	data  vf(11,12),  vf(12,12),  vf(13,12),  vf(14,12),  vf(15,12)
     &	/ 0.500000, 0.438531, 0.382105, 0.331306, 0.286345	/
	data  vf(16,12),  vf(17,12),  vf(18,12),  vf(19,12),  vf(20,12)
     &	/ 0.247131, 0.213348, 0.184531, 0.160135, 0.139589	/
	data  vf(21,12),  vf(22,12),  vf(23,12),  vf(24,12),  vf(25,12)
     &	/ 0.122334, 0.107852, 0.095681, 0.085420, 0.076729	/
	data  vf(26,12),  vf(27,12),  vf(28,12),  vf(29,12),  vf(30,12)
     &	/ 0.069326, 0.062980, 0.057503, 0.052745, 0.048582	/
	data  vf(31,12),  vf(32,12),  vf(33,12),  vf(34,12),  vf(35,12)
     &	/ 0.044918, 0.041673, 0.038784, 0.036198, 0.033873	/
	data  vf(36,12),  vf(37,12),  vf(38,12),  vf(39,12),  vf(40,12)
     &	/ 0.031773, 0.029869, 0.028137, 0.026556, 0.025108	/
	data  vf(41,12),  vf(42,12),  vf(43,12),  vf(44,12),  vf(45,12)
     &	/ 0.023779, 0.022555, 0.021425, 0.020380, 0.019411	/
	data  vf(46,12),  vf(47,12),  vf(48,12),  vf(49,12),  vf(50,12)
     &	/ 0.018511, 0.017674, 0.016893, 0.016163, 0.015480	/

	data  vf( 1,13),  vf( 2,13),  vf( 3,13),  vf( 4,13),  vf( 5,13)
     &	/ 1.000000, 0.992491, 0.970370, 0.934813, 0.887654	/
	data  vf( 6,13),  vf( 7,13),  vf( 8,13),  vf( 9,13),  vf(10,13)
     &	/ 0.831220, 0.768122, 0.701043, 0.632545, 0.564904	/
	data  vf(11,13),  vf(12,13),  vf(13,13),  vf(14,13),  vf(15,13)
     &	/ 0.500000, 0.439258, 0.383639, 0.333669, 0.289506	/
	data  vf(16,13),  vf(17,13),  vf(18,13),  vf(19,13),  vf(20,13)
     &	/ 0.251011, 0.217838, 0.189503, 0.165455, 0.145129	/
	data  vf(21,13),  vf(22,13),  vf(23,13),  vf(24,13),  vf(25,13)
     &	/ 0.127978, 0.113502, 0.101256, 0.090859, 0.081990	/
	data  vf(26,13),  vf(27,13),  vf(28,13),  vf(29,13),  vf(30,13)
     &	/ 0.074379, 0.067810, 0.062102, 0.057113, 0.052725	/
	data  vf(31,13),  vf(32,13),  vf(33,13),  vf(34,13),  vf(35,13)
     &	/ 0.048843, 0.045391, 0.042306, 0.039535, 0.037036	/
	data  vf(36,13),  vf(37,13),  vf(38,13),  vf(39,13),  vf(40,13)
     &	/ 0.034775, 0.032720, 0.030846, 0.029133, 0.027562	/
	data  vf(41,13),  vf(42,13),  vf(43,13),  vf(44,13),  vf(45,13)
     &	/ 0.026118, 0.024786, 0.023556, 0.022417, 0.021359	/
	data  vf(46,13),  vf(47,13),  vf(48,13),  vf(49,13),  vf(50,13)
     &	/ 0.020376, 0.019461, 0.018607, 0.017808, 0.017061	/

	data  vf( 1,14),  vf( 2,14),  vf( 3,14),  vf( 4,14),  vf( 5,14)
     &	/ 1.000000, 0.992414, 0.970077, 0.934213, 0.886727	/
	data  vf( 6,14),  vf( 7,14),  vf( 8,14),  vf( 9,14),  vf(10,14)
     &	/ 0.830019, 0.766773, 0.699726, 0.631468, 0.564275	/
	data  vf(11,14),  vf(12,14),  vf(13,14),  vf(14,14),  vf(15,14)
     &	/ 0.500000, 0.440020, 0.385237, 0.336116, 0.292759	/
	data  vf(16,14),  vf(17,14),  vf(18,14),  vf(19,14),  vf(20,14)
     &	/ 0.254984, 0.222411, 0.194543, 0.170827, 0.150702	/
	data  vf(21,14),  vf(22,14),  vf(23,14),  vf(24,14),  vf(25,14)
     &	/ 0.133639, 0.119154, 0.106824, 0.096286, 0.087235	/
	data  vf(26,14),  vf(27,14),  vf(28,14),  vf(29,14),  vf(30,14)
     &	/ 0.079418, 0.072627, 0.066693, 0.061478, 0.056869	/
	data  vf(31,14),  vf(32,14),  vf(33,14),  vf(34,14),  vf(35,14)
     &	/ 0.052774, 0.049119, 0.045841, 0.042889, 0.040221	/
	data  vf(36,14),  vf(37,14),  vf(38,14),  vf(39,14),  vf(40,14)
     &	/ 0.037799, 0.035595, 0.033582, 0.031738, 0.030045	/
	data  vf(41,14),  vf(42,14),  vf(43,14),  vf(44,14),  vf(45,14)
     &	/ 0.028486, 0.027047, 0.025716, 0.024483, 0.023337	/
	data  vf(46,14),  vf(47,14),  vf(48,14),  vf(49,14),  vf(50,14)
     &	/ 0.022272, 0.021278, 0.020350, 0.019482, 0.018670	/

	data  vf( 1,15),  vf( 2,15),  vf( 3,15),  vf( 4,15),  vf( 5,15)
     &	/ 1.000000, 0.992329, 0.969758, 0.933561, 0.885719	/
	data  vf( 6,15),  vf( 7,15),  vf( 8,15),  vf( 9,15),  vf(10,15)
     &	/ 0.828719, 0.765319, 0.698314, 0.630320, 0.563608	/
	data  vf(11,15),  vf(12,15),  vf(13,15),  vf(14,15),  vf(15,15)
     &	/ 0.500000, 0.440819, 0.386901, 0.338650, 0.296107	/
	data  vf(16,15),  vf(17,15),  vf(18,15),  vf(19,15),  vf(20,15)
     &	/ 0.259048, 0.227065, 0.199648, 0.176243, 0.156302	/
	data  vf(21,15),  vf(22,15),  vf(23,15),  vf(24,15),  vf(25,15)
     &	/ 0.139310, 0.124804, 0.112382, 0.101698, 0.092464	/
	data  vf(26,15),  vf(27,15),  vf(28,15),  vf(29,15),  vf(30,15)
     &	/ 0.084441, 0.077432, 0.071276, 0.065839, 0.061014	/
	data  vf(31,15),  vf(32,15),  vf(33,15),  vf(34,15),  vf(35,15)
     &	/ 0.056712, 0.052858, 0.049391, 0.046262, 0.043426	/
	data  vf(36,15),  vf(37,15),  vf(38,15),  vf(39,15),  vf(40,15)
     &	/ 0.040847, 0.038495, 0.036344, 0.034371, 0.032556	/
	data  vf(41,15),  vf(42,15),  vf(43,15),  vf(44,15),  vf(45,15)
     &	/ 0.030883, 0.029338, 0.027907, 0.026580, 0.025346	/
	data  vf(46,15),  vf(47,15),  vf(48,15),  vf(49,15),  vf(50,15)
     &	/ 0.024197, 0.023125, 0.022123, 0.021186, 0.020307	/

	data  vf( 1,16),  vf( 2,16),  vf( 3,16),  vf( 4,16),  vf( 5,16)
     &	/ 1.000000, 0.992236, 0.969407, 0.932848, 0.884621	/
	data  vf( 6,16),  vf( 7,16),  vf( 8,16),  vf( 9,16),  vf(10,16)
     &	/ 0.827306, 0.763746, 0.696794, 0.629091, 0.562899	/
	data  vf(11,16),  vf(12,16),  vf(13,16),  vf(14,16),  vf(15,16)
     &	/ 0.500000, 0.441656, 0.388636, 0.341271, 0.299549	/
	data  vf(16,16),  vf(17,16),  vf(18,16),  vf(19,16),  vf(20,16)
     &	/ 0.263202, 0.231796, 0.204811, 0.181698, 0.161921	/
	data  vf(21,16),  vf(22,16),  vf(23,16),  vf(24,16),  vf(25,16)
     &	/ 0.144984, 0.130446, 0.117924, 0.107091, 0.097674	/
	data  vf(26,16),  vf(27,16),  vf(28,16),  vf(29,16),  vf(30,16)
     &	/ 0.089448, 0.082224, 0.075849, 0.070196, 0.065160	/
	data  vf(31,16),  vf(32,16),  vf(33,16),  vf(34,16),  vf(35,16)
     &	/ 0.060654, 0.056606, 0.052956, 0.049651, 0.046651	/
	data  vf(36,16),  vf(37,16),  vf(38,16),  vf(39,16),  vf(40,16)
     &	/ 0.043917, 0.041420, 0.039132, 0.037030, 0.035095	/
	data  vf(41,16),  vf(42,16),  vf(43,16),  vf(44,16),  vf(45,16)
     &	/ 0.033310, 0.031658, 0.030127, 0.028706, 0.027383	/
	data  vf(46,16),  vf(47,16),  vf(48,16),  vf(49,16),  vf(50,16)
     &	/ 0.026151, 0.025001, 0.023925, 0.022918, 0.021973	/

	data  vf( 1,17),  vf( 2,17),  vf( 3,17),  vf( 4,17),  vf( 5,17)
     &	/ 1.000000, 0.992134, 0.969021, 0.932062, 0.883416	/
	data  vf( 6,17),  vf( 7,17),  vf( 8,17),  vf( 9,17),  vf(10,17)
     &	/ 0.825764, 0.762039, 0.695153, 0.627773, 0.562144	/
	data  vf(11,17),  vf(12,17),  vf(13,17),  vf(14,17),  vf(15,17)
     &	/ 0.500000, 0.442535, 0.390443, 0.343984, 0.303086	/
	data  vf(16,17),  vf(17,17),  vf(18,17),  vf(19,17),  vf(20,17)
     &	/ 0.267442, 0.236597, 0.210024, 0.187182, 0.167552	/
	data  vf(21,17),  vf(22,17),  vf(23,17),  vf(24,17),  vf(25,17)
     &	/ 0.150656, 0.136075, 0.123446, 0.112461, 0.102863	/
	data  vf(26,17),  vf(27,17),  vf(28,17),  vf(29,17),  vf(30,17)
     &	/ 0.094435, 0.087001, 0.080413, 0.074549, 0.069307	/
	data  vf(31,17),  vf(32,17),  vf(33,17),  vf(34,17),  vf(35,17)
     &	/ 0.064603, 0.060365, 0.056533, 0.053058, 0.049895	/
	data  vf(36,17),  vf(37,17),  vf(38,17),  vf(39,17),  vf(40,17)
     &	/ 0.047009, 0.044368, 0.041945, 0.039717, 0.037662	/
	data  vf(41,17),  vf(42,17),  vf(43,17),  vf(44,17),  vf(45,17)
     &	/ 0.035764, 0.034006, 0.032376, 0.030861, 0.029450	/
	data  vf(46,17),  vf(47,17),  vf(48,17),  vf(49,17),  vf(50,17)
     &	/ 0.028134, 0.026905, 0.025756, 0.024678, 0.023667	/

	data  vf( 1,18),  vf( 2,18),  vf( 3,18),  vf( 4,18),  vf( 5,18)
     &	/ 1.000000, 0.992020, 0.968592, 0.931193, 0.882088	/
	data  vf( 6,18),  vf( 7,18),  vf( 8,18),  vf( 9,18),  vf(10,18)
     &	/ 0.824072, 0.760176, 0.693375, 0.626355, 0.561338	/
	data  vf(11,18),  vf(12,18),  vf(13,18),  vf(14,18),  vf(15,18)
     &	/ 0.500000, 0.443459, 0.392325, 0.346787, 0.306715	/
	data  vf(16,18),  vf(17,18),  vf(18,18),  vf(19,18),  vf(20,18)
     &	/ 0.271764, 0.241461, 0.215280, 0.192688, 0.173186	/
	data  vf(21,18),  vf(22,18),  vf(23,18),  vf(24,18),  vf(25,18)
     &	/ 0.156317, 0.141684, 0.128944, 0.117807, 0.108027	/
	data  vf(26,18),  vf(27,18),  vf(28,18),  vf(29,18),  vf(30,18)
     &	/ 0.099403, 0.091763, 0.084967, 0.078897, 0.073454	/
	data  vf(31,18),  vf(32,18),  vf(33,18),  vf(34,18),  vf(35,18)
     &	/ 0.068556, 0.064132, 0.060123, 0.056480, 0.053158	/
	data  vf(36,18),  vf(37,18),  vf(38,18),  vf(39,18),  vf(40,18)
     &	/ 0.050122, 0.047340, 0.044783, 0.042428, 0.040255	/
	data  vf(41,18),  vf(42,18),  vf(43,18),  vf(44,18),  vf(45,18)
     &	/ 0.038245, 0.036383, 0.034653, 0.033044, 0.031545	/
	data  vf(46,18),  vf(47,18),  vf(48,18),  vf(49,18),  vf(50,18)
     &	/ 0.030146, 0.028838, 0.027614, 0.026466, 0.025389	/

	data  vf( 1,19),  vf( 2,19),  vf( 3,19),  vf( 4,19),  vf( 5,19)
     &	/ 1.000000, 0.991892, 0.968111, 0.930223, 0.880612	/
	data  vf( 6,19),  vf( 7,19),  vf( 8,19),  vf( 9,19),  vf(10,19)
     &	/ 0.822204, 0.758133, 0.691441, 0.624826, 0.560476	/
	data  vf(11,19),  vf(12,19),  vf(13,19),  vf(14,19),  vf(15,19)
     &	/ 0.500000, 0.444429, 0.394285, 0.349681, 0.310433	/
	data  vf(16,19),  vf(17,19),  vf(18,19),  vf(19,19),  vf(20,19)
     &	/ 0.276161, 0.246380, 0.220567, 0.198205, 0.178813	/
	data  vf(21,19),  vf(22,19),  vf(23,19),  vf(24,19),  vf(25,19)
     &	/ 0.161959, 0.147267, 0.134413, 0.123123, 0.113165	/
	data  vf(26,19),  vf(27,19),  vf(28,19),  vf(29,19),  vf(30,19)
     &	/ 0.104347, 0.096507, 0.089509, 0.083238, 0.077600	/
	data  vf(31,19),  vf(32,19),  vf(33,19),  vf(34,19),  vf(35,19)
     &	/ 0.072512, 0.067907, 0.063725, 0.059917, 0.056439	/
	data  vf(36,19),  vf(37,19),  vf(38,19),  vf(39,19),  vf(40,19)
     &	/ 0.053255, 0.050332, 0.047643, 0.045164, 0.042874	/
	data  vf(41,19),  vf(42,19),  vf(43,19),  vf(44,19),  vf(45,19)
     &	/ 0.040753, 0.038785, 0.036957, 0.035255, 0.033668	/
	data  vf(46,19),  vf(47,19),  vf(48,19),  vf(49,19),  vf(50,19)
     &	/ 0.032185, 0.030799, 0.029500, 0.028281, 0.027137	/

	data  vf( 1,20),  vf( 2,20),  vf( 3,20),  vf( 4,20),  vf( 5,20)
     &	/ 1.000000, 0.991747, 0.967568, 0.929131, 0.878961	/
	data  vf( 6,20),  vf( 7,20),  vf( 8,20),  vf( 9,20),  vf(10,20)
     &	/ 0.820127, 0.755881, 0.689327, 0.623170, 0.559553	/
	data  vf(11,20),  vf(12,20),  vf(13,20),  vf(14,20),  vf(15,20)
     &	/ 0.500000, 0.445447, 0.396322, 0.352663, 0.314232	/
	data  vf(16,20),  vf(17,20),  vf(18,20),  vf(19,20),  vf(20,20)
     &	/ 0.280622, 0.251340, 0.225872, 0.203719, 0.184423	/
	data  vf(21,20),  vf(22,20),  vf(23,20),  vf(24,20),  vf(25,20)
     &	/ 0.167573, 0.152816, 0.139846, 0.128405, 0.118273	/
	data  vf(26,20),  vf(27,20),  vf(28,20),  vf(29,20),  vf(30,20)
     &	/ 0.109267, 0.101232, 0.094037, 0.087571, 0.081742	/
	data  vf(31,20),  vf(32,20),  vf(33,20),  vf(34,20),  vf(35,20)
     &	/ 0.076470, 0.071687, 0.067336, 0.063366, 0.059735	/
	data  vf(36,20),  vf(37,20),  vf(38,20),  vf(39,20),  vf(40,20)
     &	/ 0.056405, 0.053345, 0.050525, 0.047923, 0.045516	/
	data  vf(41,20),  vf(42,20),  vf(43,20),  vf(44,20),  vf(45,20)
     &	/ 0.043285, 0.041213, 0.039287, 0.037492, 0.035816	/
	data  vf(46,20),  vf(47,20),  vf(48,20),  vf(49,20),  vf(50,20)
     &	/ 0.034251, 0.032785, 0.031412, 0.030123, 0.028911	/

	data  vf( 1,21),  vf( 2,21),  vf( 3,21),  vf( 4,21),  vf( 5,21)
     &	/ 1.000000, 0.991580, 0.966948, 0.927890, 0.877097	/
	data  vf( 6,21),  vf( 7,21),  vf( 8,21),  vf( 9,21),  vf(10,21)
     &	/ 0.817802, 0.753383, 0.687007, 0.621374, 0.558562	/
	data  vf(11,21),  vf(12,21),  vf(13,21),  vf(14,21),  vf(15,21)
     &	/ 0.500000, 0.446514, 0.398436, 0.355727, 0.318103	/
	data  vf(16,21),  vf(17,21),  vf(18,21),  vf(19,21),  vf(20,21)
     &	/ 0.285133, 0.256327, 0.231181, 0.209218, 0.190003	/
	data  vf(21,21),  vf(22,21),  vf(23,21),  vf(24,21),  vf(25,21)
     &	/ 0.173149, 0.158323, 0.145237, 0.133647, 0.123346	/
	data  vf(26,21),  vf(27,21),  vf(28,21),  vf(29,21),  vf(30,21)
     &	/ 0.114157, 0.105933, 0.098547, 0.091892, 0.085878	/
	data  vf(31,21),  vf(32,21),  vf(33,21),  vf(34,21),  vf(35,21)
     &	/ 0.080426, 0.075471, 0.070953, 0.066825, 0.063043	/
	data  vf(36,21),  vf(37,21),  vf(38,21),  vf(39,21),  vf(40,21)
     &	/ 0.059570, 0.056374, 0.053426, 0.050702, 0.048179	/
	data  vf(41,21),  vf(42,21),  vf(43,21),  vf(44,21),  vf(45,21)
     &	/ 0.045839, 0.043665, 0.041640, 0.039752, 0.037989	/
	data  vf(46,21),  vf(47,21),  vf(48,21),  vf(49,21),  vf(50,21)
     &	/ 0.036341, 0.034797, 0.033348, 0.031988, 0.030709	/

	data  vf( 1,22),  vf( 2,22),  vf( 3,22),  vf( 4,22),  vf( 5,22)
     &	/ 1.000000, 0.991387, 0.966231, 0.926463, 0.874971	/
	data  vf( 6,22),  vf( 7,22),  vf( 8,22),  vf( 9,22),  vf(10,22)
     &	/ 0.815176, 0.750594, 0.684450, 0.619419, 0.557499	/
	data  vf(11,22),  vf(12,22),  vf(13,22),  vf(14,22),  vf(15,22)
     &	/ 0.500000, 0.447631, 0.400622, 0.358864, 0.322030	/
	data  vf(16,22),  vf(17,22),  vf(18,22),  vf(19,22),  vf(20,22)
     &	/ 0.289679, 0.261322, 0.236476, 0.214686, 0.195540	/
	data  vf(21,22),  vf(22,22),  vf(23,22),  vf(24,22),  vf(25,22)
     &	/ 0.178676, 0.163779, 0.150579, 0.138844, 0.128378	/
	data  vf(26,22),  vf(27,22),  vf(28,22),  vf(29,22),  vf(30,22)
     &	/ 0.119013, 0.110606, 0.103035, 0.096198, 0.090004	/
	data  vf(31,22),  vf(32,22),  vf(33,22),  vf(34,22),  vf(35,22)
     &	/ 0.084377, 0.079253, 0.074574, 0.070291, 0.066361	/
	data  vf(36,22),  vf(37,22),  vf(38,22),  vf(39,22),  vf(40,22)
     &	/ 0.062748, 0.059418, 0.056343, 0.053499, 0.050862	/
	data  vf(41,22),  vf(42,22),  vf(43,22),  vf(44,22),  vf(45,22)
     &	/ 0.048414, 0.046136, 0.044015, 0.042035, 0.040185	/
	data  vf(46,22),  vf(47,22),  vf(48,22),  vf(49,22),  vf(50,22)
     &	/ 0.038453, 0.036830, 0.035308, 0.033877, 0.032531	/

	data  vf( 1,23),  vf( 2,23),  vf( 3,23),  vf( 4,23),  vf( 5,23)
     &	/ 1.000000, 0.991159, 0.965387, 0.924798, 0.872515	/
	data  vf( 6,23),  vf( 7,23),  vf( 8,23),  vf( 9,23),  vf(10,23)
     &	/ 0.812182, 0.747459, 0.681619, 0.617290, 0.556358	/
	data  vf(11,23),  vf(12,23),  vf(13,23),  vf(14,23),  vf(15,23)
     &	/ 0.500000, 0.448795, 0.402872, 0.362058, 0.325996	/
	data  vf(16,23),  vf(17,23),  vf(18,23),  vf(19,23),  vf(20,23)
     &	/ 0.294236, 0.266304, 0.241736, 0.220103, 0.201018	/
	data  vf(21,23),  vf(22,23),  vf(23,23),  vf(24,23),  vf(25,23)
     &	/ 0.184139, 0.169172, 0.155861, 0.143987, 0.133363	/
	data  vf(26,23),  vf(27,23),  vf(28,23),  vf(29,23),  vf(30,23)
     &	/ 0.123828, 0.115245, 0.107496, 0.100481, 0.094114	/
	data  vf(31,23),  vf(32,23),  vf(33,23),  vf(34,23),  vf(35,23)
     &	/ 0.088318, 0.083030, 0.078193, 0.073758, 0.069684	/
	data  vf(36,23),  vf(37,23),  vf(38,23),  vf(39,23),  vf(40,23)
     &	/ 0.065933, 0.062472, 0.059272, 0.056309, 0.053560	/
	data  vf(41,23),  vf(42,23),  vf(43,23),  vf(44,23),  vf(45,23)
     &	/ 0.051004, 0.048625, 0.046407, 0.044336, 0.042399	/
	data  vf(46,23),  vf(47,23),  vf(48,23),  vf(49,23),  vf(50,23)
     &	/ 0.040585, 0.038884, 0.037287, 0.035786, 0.034372	/

	data  vf( 1,24),  vf( 2,24),  vf( 3,24),  vf( 4,24),  vf( 5,24)
     &	/ 1.000000, 0.990884, 0.964375, 0.922821, 0.869640	/
	data  vf( 6,24),  vf( 7,24),  vf( 8,24),  vf( 9,24),  vf(10,24)
     &	/ 0.808733, 0.743913, 0.678477, 0.614972, 0.555139	/
	data  vf(11,24),  vf(12,24),  vf(13,24),  vf(14,24),  vf(15,24)
     &	/ 0.500000, 0.450000, 0.405173, 0.365290, 0.329973	/
	data  vf(16,24),  vf(17,24),  vf(18,24),  vf(19,24),  vf(20,24)
     &	/ 0.298778, 0.271246, 0.246939, 0.225451, 0.206420	/
	data  vf(21,24),  vf(22,24),  vf(23,24),  vf(24,24),  vf(25,24)
     &	/ 0.189526, 0.174491, 0.161074, 0.149066, 0.138291	/
	data  vf(26,24),  vf(27,24),  vf(28,24),  vf(29,24),  vf(30,24)
     &	/ 0.128593, 0.119841, 0.111922, 0.104737, 0.098201	/
	data  vf(31,24),  vf(32,24),  vf(33,24),  vf(34,24),  vf(35,24)
     &	/ 0.092241, 0.086794, 0.081804, 0.077222, 0.073006	/
	data  vf(36,24),  vf(37,24),  vf(38,24),  vf(39,24),  vf(40,24)
     &	/ 0.069120, 0.065530, 0.062208, 0.059128, 0.056268	/
	data  vf(41,24),  vf(42,24),  vf(43,24),  vf(44,24),  vf(45,24)
     &	/ 0.053607, 0.051128, 0.048814, 0.046652, 0.044629	/
	data  vf(46,24),  vf(47,24),  vf(48,24),  vf(49,24),  vf(50,24)
     &	/ 0.042734, 0.040955, 0.039283, 0.037711, 0.036231	/

	data  vf( 1,25),  vf( 2,25),  vf( 3,25),  vf( 4,25),  vf( 5,25)
     &	/ 1.000000, 0.990542, 0.963129, 0.920423, 0.866219	/
	data  vf( 6,25),  vf( 7,25),  vf( 8,25),  vf( 9,25),  vf(10,25)
     &	/ 0.804718, 0.739882, 0.674991, 0.612457, 0.553845	/
	data  vf(11,25),  vf(12,25),  vf(13,25),  vf(14,25),  vf(15,25)
     &	/ 0.500000, 0.451236, 0.407503, 0.368530, 0.333931	/
	data  vf(16,25),  vf(17,25),  vf(18,25),  vf(19,25),  vf(20,25)
     &	/ 0.303274, 0.276121, 0.252058, 0.230707, 0.211727	/
	data  vf(21,25),  vf(22,25),  vf(23,25),  vf(24,25),  vf(25,25)
     &	/ 0.194819, 0.179720, 0.166202, 0.154069, 0.143150	/
	data  vf(26,25),  vf(27,25),  vf(28,25),  vf(29,25),  vf(30,25)
     &	/ 0.133298, 0.124386, 0.116303, 0.108954, 0.102256	/
	data  vf(31,25),  vf(32,25),  vf(33,25),  vf(34,25),  vf(35,25)
     &	/ 0.096139, 0.090537, 0.085398, 0.080674, 0.076321	/
	data  vf(36,25),  vf(37,25),  vf(38,25),  vf(39,25),  vf(40,25)
     &	/ 0.072303, 0.068587, 0.065144, 0.061950, 0.058980	/
	data  vf(41,25),  vf(42,25),  vf(43,25),  vf(44,25),  vf(45,25)
     &	/ 0.056216, 0.053638, 0.051230, 0.048979, 0.046870	/
	data  vf(46,25),  vf(47,25),  vf(48,25),  vf(49,25),  vf(50,25)
     &	/ 0.044893, 0.043037, 0.041293, 0.039651, 0.038103	/

	data  vf( 1,26),  vf( 2,26),  vf( 3,26),  vf( 4,26),  vf( 5,26)
     &	/ 1.000000, 0.990099, 0.961539, 0.917431, 0.862069	/
	data  vf( 6,26),  vf( 7,26),  vf( 8,26),  vf( 9,26),  vf(10,26)
     &	/ 0.800000, 0.735295, 0.671141, 0.609756, 0.552486	/
	data  vf(11,26),  vf(12,26),  vf(13,26),  vf(14,26),  vf(15,26)
     &	/ 0.500000, 0.452489, 0.409836, 0.371747, 0.337838	/
	data  vf(16,26),  vf(17,26),  vf(18,26),  vf(19,26),  vf(20,26)
     &	/ 0.307692, 0.280899, 0.257070, 0.235849, 0.216920	/
	data  vf(21,26),  vf(22,26),  vf(23,26),  vf(24,26),  vf(25,26)
     &	/ 0.200000, 0.184843, 0.171233, 0.158983, 0.147929	/
	data  vf(26,26),  vf(27,26),  vf(28,26),  vf(29,26),  vf(30,26)
     &	/ 0.137931, 0.128866, 0.120627, 0.113122, 0.106270	/
	data  vf(31,26),  vf(32,26),  vf(33,26),  vf(34,26),  vf(35,26)
     &	/ 0.100000, 0.094251, 0.088968, 0.084104, 0.079618	/
	data  vf(36,26),  vf(37,26),  vf(38,26),  vf(39,26),  vf(40,26)
     &	/ 0.075472, 0.071633, 0.068074, 0.064767, 0.061691	/
	data  vf(41,26),  vf(42,26),  vf(43,26),  vf(44,26),  vf(45,26)
     &	/ 0.058824, 0.056148, 0.053648, 0.051309, 0.049116	/
	data  vf(46,26),  vf(47,26),  vf(48,26),  vf(49,26),  vf(50,26)
     &	/ 0.047059, 0.045127, 0.043309, 0.041598, 0.039984	/
  
	end
