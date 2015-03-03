c \grammy\src\altinf.inc   Ver 1.0

c Revision 1.02  08/03/93  Modified for type 6 header conversion
c Note: eltim is in extinf in Decomp*****************
c Revision 1.01  31/08/91  Modified for type 5 header conversion
c Version  1.00  17.04.91  Merge with IC Decomp version

        double precision strtim,stptim,refwavno,fzeeman
        double precision aliaslo,aliashi,eltim
        double precision pvar0,pvar1,pvar2,pvar3,pvar4
        double precision pvar5,pvar6,pvar7,pvar8,pvar9
        double precision pvar10,pvar11,pvar12,pvar13
        double precision pvar14,pvar15,pvar16,pvar17
 
        common /altr8/ strtim,stptim,refwavno,fzeeman,
     &     aliaslo,aliashi,eltim,
     &     pvar0,pvar1,pvar2,pvar3,pvar4,pvar5,pvar6,pvar7,
     &     pvar8,pvar9,pvar10,pvar11,pvar12,pvar13,pvar14,
     &     pvar15,pvar16,pvar17

 
        real  tppt, modeff, srverr, a4dum1, ratsig, ratrms
        real  apin,apmax, pspect, tspect, hspect
        real  apod1,apod2,apod3,apod4,apod5,decim
        real  resolutn,sampfreq, sumsqr,cvel,ctrav,drivevel
        real  outlo,outhi,glitchlo,glitchhi,gltchdev
        real  sten, stol, smof, swid
 
        common /altr4/ tppt, modeff, srverr, a4dum1, ratsig, ratrms
        common /altr4/ apin,apmax, pspect, tspect, hspect
        common /altr4/ apod1,apod2,apod3,apod4,apod5,decim
        common /altr4/ resolutn,sampfreq,sumsqr,cvel,ctrav,drivevel
        common /altr4/ outlo,outhi,glitchlo,glitchhi,gltchdev
        common /altr4/ sten, stol, smof, swid

 
        integer    ncenter, nserie, nsides, ninput, notput
        integer    nint,nused,ntrans,modeapod
        integer    nscan, serialno, scandir
        integer    alias, nm, nk, ns, s_delay
 
        common /alti4/ ncenter, nserie, nsides, ninput, notput
        common /alti4/ nint,nused,ntrans,modeapod
        common /alti4/ nscan, serialno, scandir
        common /alti4/ alias, nm, nk, ns, s_delay


        character*128 filter1, filter2, bmsplt
        character*128 filter_i,filter_a,filter_b
        character*128 sample_i,sample_a,sample_b
        character*128 dtectrA, gainA, dtectrB, gainB
        character*128 AmBfilt, ApBfilt, spect, user, sample
        character*128 source, origin, observ, creator

        common /altch/ filter1, filter2, bmsplt,
     *                 filter_i,filter_a,filter_b,
     *                 sample_i,sample_a,sample_b,
     *		       dtectrA, gainA, dtectrB, gainB,
     *		       AmBfilt, ApBfilt, spect, user, sample, 
     *                 source, origin, observ, creator




