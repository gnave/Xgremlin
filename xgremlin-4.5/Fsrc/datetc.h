
* modified for Xgremlin

	double precision xpara, xparb, xparc, xnum(20) 
	real absmx, avg, cgr, secm, scal, rbar

	integer ipara, iparb, iparc, inum(20)
	integer ifl, ifx, nstrng, nalpha(6)
	integer me, np, nppt, nh, nddum1,nddum2,nddum3
	integer nact, nc, kl
	integer nrtr, n2rtr, nfftamax, nphzmax
	logical keybd, cmdpnd

	common /dcparm/ xpara, xparb, xparc, xnum 
	common /dcparm/ absmx, avg, cgr, secm, scal, rbar
	common /dcparm/ ipara, iparb, iparc, inum
	common /dcparm/ ifl, ifx, nstrng, nalpha
	common /dcparm/ me, np, nppt, nh, nddum1,nddum2,nddum3
	common /dcparm/ nact, nc, kl
	common /dcparm/ nrtr, n2rtr, nfftamax, nphzmax
	common /dcparm/ keybd, cmdpnd
 
	character kin*128, lp*128, alpha*80
	character cdat*14, ctim*12
 
	common /datacr/ kin, lp, alpha(6)
	common /datacr/ cdat, ctim

	integer ffta_size, rtr_size, r_size, tr_size, phz_size
	integer linbuf_size

	common /ardata/ ffta_size, rtr_size, r_size, tr_size, phz_size
	common /ardata/ linbuf_size

*-----------------------------------------------------------------------
* cmdpnd : a logical that signals a pending command after a ';'
* ivuv   : =1: calculate vacuum wavelength in  sigma <--> wavelength conversion 
