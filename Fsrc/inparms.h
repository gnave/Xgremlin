c \grammy\src\inparms.inc   Ver 1.0

c Revision 1.06 22.10.93  phz moved to overlap r; savphz occupies its space
c Revision 1.05 10.12.92  some order changed to put logicals at end
c Revision 1.04 14.10.92  add symflg to inpar
c Revision 1.03 05.09.92  start with doubles for alignment
c Revision 1.02 14.08.91  hsign deleted
c Revision 1.01 11.05.91  Phz dimensioned to 32768
c Version  1.00 18.04.91

c input parameters and flags

	common/inpar/
     &          xwstart,xwstop,xcentr,dindum,
     &		inskip,ixtend,incntr,nseq,iprnt,invflg,
     &		imax,imin,curblk,blksiz,
     &		noptex,mpwr,nwanted,ngot,
     &		rmax,rmin,rsum,hsgndum,xindum,
     &          apodfl,onesidfl,nomore,symflg

	double precision xwstart, xwstop, xcentr, dindum
	integer inskip,ixtend,incntr,nseq,iprnt,invflg,
     &		imax,imin,curblk,blksiz,
     &		noptex,mpwr,nwanted,ngot
	real rmax,rmin,rsum,hsgndum,xindum
	logical apodfl,onesidfl,nomore,symflg

c apodizing parameters
	common/apodz/aparam(10),iparam(10),npi,npr
	real aparam
	integer iparam,npi,npr

c deglitching parameters
	common/baaad/nexmpt,iexmpt,irng1,irng2,xsigma,deglch
	integer iexmpt(10)                                                
	logical deglch                                                    

c lores
	common/lowres/stalow(10), istlow(10), nlores 
	double precision stalow
	integer istlow,nlores

c phase
	real svphz(4097),svphz2(4),phparms(10)
	integer iphparms(10),nphi,nphr,nphz,nphdum,nphap
	logical phapdz                                                    

	common/phzz/svphz,svphz2,phparms
	common/phzz/iphparms,nphi,nphr,nphz,nphdum,nphap
	common/phzz/phapdz      
                                                                       
	common/fxregn/nfregn,mfregn(10,4)                                 

	common/stregn/ nregn,mregn(10,8)                                  

c   phz will contain the phase after cfproc is called                  
c   nphz is currently set to 8192
c   npts is the number of points actually in work at any one time       
c  iprnt sets the level of verbosity
c  inskip = (integer) number of points to skip at start of interferogram
c  ixtend = (integer) factor by which interferogram is to be extended with 0's
c  xcentr = (real) position of center of interferogram
c  apodfl=.true. if apodization is wanted; .false. otherwise
c  onesidfl = .true. if input is onesided; else .false.
c  hsign (real) = 1.0 if alias order has wrong parity on tape; else 0.0
c  nseq = 0 for inputonly
c       = 1 during input
c       = 2 during transform
c  incntr = -1 means use imax found during input
c         = 0 means use ncenter from header
c         = 1 means use ncenter from 'ncenter' command
