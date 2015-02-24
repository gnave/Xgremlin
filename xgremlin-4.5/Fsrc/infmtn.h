c \grammy\src\infmtn.inc   Ver 1.0

c Version  1.01 28.08.91  From IC Decomp

c *********************************************************************
c DO NOT CHANGE THIS COMMON FILE WITHOUT CORRECTING ALL USES OF THESE
c COMMON VARIABLES ELSEWHERE, PARTICULARLY IN scrread, scrwrite
c infread, infwrt, dinfo, whats.  IDENTIFY AND DATE ALL CHANGES
c *********************************************************************

c real*8:
        double precision wref,delw,wstart,wstop,wavcorr,aps,corr,pref,
     &	   freespec,fdivr,fdr,
     &	   uvar0,uvar1,uvar2,uvar3,uvar4,uvar5,uvar6,uvar7,uvar8,uvar9,
     &	   tmpd0,tmpd1,tmpd2,tmpd3,tmpd4,tmpd5,tmpd6,tmpd7,tmpd8,tmpd9

        common /infr8/ wref,delw,wstart,wstop,wavcorr,aps,corr,pref,
     &	   freespec,fdivr,fdr,
     &	   uvar0,uvar1,uvar2,uvar3,uvar4,uvar5,uvar6,uvar7,uvar8,uvar9,
     &	   tmpd0,tmpd1,tmpd2,tmpd3,tmpd4,tmpd5,tmpd6,tmpd7,tmpd8,tmpd9


c real*4:
        real  rdsclfct, noiselev,fclock,motorvel,fracbw,epsilon,
     &	   endgauss,fracmask,wdlim,tmpr5,tmpr6,tmpr7,tmpr8,tmpr9

        common /infr4/ rdsclfct, noiselev,fclock,motorvel,fracbw,
     &	   epsilon,endgauss,fracmask,wdlim,tmpr5,tmpr6,tmpr7,tmpr8,
     &     tmpr9

c integer:
        integer 
     &  nrec, nroll, npo, nop, npt, nwpp, neoa, margin, nrefp,
     &  nwos,iratio,icrflg,inftype,fboffs,bocode,mbocode,kernlen,
     &	tmpi1,tmpi2,tmpi3,tmpi4,tmpi5,tmpi6,tmpi7,tmpi8,tmpi9,ipid

        common /infi4/ 
     &  nrec,nroll,npo,nop,npt,nwpp,neoa,nwos,margin,nrefp,
     &  iratio, icrflg, inftype, fboffs, bocode, mbocode, kernlen,
     &	tmpi1,tmpi2,tmpi3,tmpi4,tmpi5,tmpi6,tmpi7,tmpi8,tmpi9,ipid

c character:
        character day*16, id*80, tmpch*160
        common /infch/ day, id, tmpch

* logical:
	logical lvacuum
	common /inflog/ lvacuum

        double precision disp
        integer nwppt
        common /infsv/ disp, nwppt
