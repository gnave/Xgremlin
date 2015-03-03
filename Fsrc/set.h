c \grammy\src\set.inc

c Revision 2.03 03/20/93  Updated to Decomp level
c Revision 2.02 02/05/91  nshold, cstag added
 
	double precision xreg(10)
	real rdfctr
	real pltylo, pltyhi
	real lsqtol, lsqref, svdtol, wdth, damp
	real sdum1,sdum2
	integer ireg(10)
	integer npltlo, nplthi
	integer rdref, rdlen
	integer lsqcnt, lsqlo, lsqhi, insflg
	integer mnumode
	integer nshold, si2dum, rdscale
	character*4 cstag
	logical verbos,rdnorm,batch,warn,lecho,lglitch

	common /set/  xreg
	common /set/  rdfctr
	common /set/  pltylo, pltyhi
	common /set/  lsqtol, lsqref, svdtol, wdth, damp
	common /set/  sdum1,sdum2
	common /set/  ireg
	common /set/  npltlo, nplthi
	common /set/  rdref, rdlen
	common /set/  lsqcnt, lsqlo, lsqhi, insflg
	common /set/  mnumode
	common /set/  nshold, si2dum, rdscale
	common /set/  cstag
	common /set/  verbos,rdnorm,batch,warn,lecho,lglitch

