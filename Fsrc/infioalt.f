c \decomp\src\infioalt.for  Ver. 1.0

c  Revision 2.40 93/09/28 oldinfrd handling of old IC improved
c  Revision 2.39 93/09/03 fitinfrd handles timestr for ihr < 10
c  Revision 2.38 93/05/18 fitinfrd updated to latest CDROM
c  Revision 2.37 93/02/24 old infread, fitinfrd, sinfrd moved here 
c  Revision 2.36 93/02/08 fitinfrd added
c  Revision 2.35 92/09/08 Illegal sqrt trapped
c  Revision 2.34 91/08/30 Header file changed
c  Revision 2.33 91/08/28 Fixed logic in type0/4 decision, type 0 defaults
c  Revision 2.32 91/08/22 Split from dataio to avoid compiler bug
c  Revision 2.31 91/08/21 Type 5 modified to handle NSO parameters
c  Revision 2.30 91/05/26 Infread handles NSO headers
c  Revision 2.29 91/05/26 Infwrt copied from grammy; always inftype=5
c  Revision 2.24 90/09/25 infwrt format corrected; also infread (its)

        subroutine b5infwrt(lu)

	include 'infmtn.h'
	include 'altinf.h'

	double precision tmptim

        ith = strtim/3600.
        tmptim = strtim - 3600.0d0*ith
        itm = tmptim/60.
        tmptim = tmptim - 60.0d0*itm
        its = tmptim
        eltim = stptim - strtim
        if (eltim .lt. 0.0) eltim = eltim + 86400.

	fdivr = float(nm)/float(nk)
	if (refwavno .eq. 0.0) refwavno = 15798.0025d0
	freespec = 0.5d0*refwavno*fdivr
	aliaslo = (alias - 1)*freespec
	aliashi = aliaslo + freespec

	bocode = mbocode   ! save the machine byte order code
	inftype = 5
	nsides = 0
	ninput = 0
	notput = 0

        rewind lu
        write(lu,1001) id(1:75),inftype,serialno,day,ith,itm,its,
     *         eltim,nscan,wstart,wstop,npo
1001    format(a75/
     *          i10,12x,'Info block type'/
     *          i10,12x,'Scan serial number'/
     *          5x, a10,7x,'Date  (mm/dd/yy)'/
     *          i7,':',i2,':',i2,9x,'Starting time'/
     *          f14.3,8x,'Elapsed time(seconds)'/
     *          i10,12x,'Number of co-added scans'/
     *          f17.6,5x,'First wavenumber in file (wstart) cm-1'/
     *          f17.6,5x,'Last  wavenumber in file (wstop) cm-1'/
     *          i10,12x,'Number of words in file',t59,' 10')

        write(lu,1002) nwpp,delw,resolutn,freespec,apin,apmax,user,
     *         source,sample,spect
1002    format(
     *         i10,12x,'Nwpp: 1=real, 2=complex file'/
     *         f20.13,2x,'Dispersion(cm-1/point)'/
     *         f16.9,6x,'Spectral resolution(cm-1)'/
     *         f16.5,6x,'Free Spectral Range(cm-1)'/
     *         f12.1,10x,'Aperture diameter(mm)'/
     *         f12.1,10x,'Maximum aperture(mm)'/
     *         a20,2x,'User'/
     *         a20,2x,'Source'/
     *         a20,2x,'Sample'/
     *         a20,2x,'Spectrometer',t59,' 20')

        write(lu,1003) filter1,filter2,bmsplt,dtectrA,gainA,
     *         dtectrB,gainB,AmBfilt,ApBfilt,ratsig
1003    format(
     *         a20,2x,'Optical filter #1'/
     *         a20,2x,'Optical filter #2'/
     *         a20,2x,'Beamsplitter'/
     *         a20,2x,'Detector A'/
     *         a20,2x,'Detector A gain'/
     *         a20,2x,'Detector B'/
     *         a20,2x,'Detector B gain'/
     *         a20,2x,'Anti-aliasing filter (A-B)'/
     *         a20,2x,'Anti-aliasing filter (A+B)'/
     *         f14.3,8x,'Total signal (A+B) - Volts',t59,' 30')

        idecim = decim
        write(lu,1004) ratrms,sampfreq,idecim,neoa,nwos,ninput,notput,
     *         nsides,iratio,nm
1004    format(
     *         f14.3,8x,'Total RMS    (A+B) - Volts'/
     *         f13.2,9x,'Sampling frequency (Hz)'/
     *         i10,12x,'Decimation factor'/
     *         i10,12x,'Neoa: 1=emission, -1=absorption'/
     *         i10,12x,'Nwos: -1=wavenumber, 1=wavelength '/
     *         i10,12x,'Number of inputs'/
     *         i10,12x,'Number of outputs'/
     *         i10,12x,'One/two sided'/
     *         i10,12x,'Ratio(1), no ratio(0)'/
     *         i10,12x,'Divisions/fringe',t55,'(m)',t59,' 40')

        write(lu,1005) nk,alias,aliaslo,aliashi,refwavno,fzeeman,ns,
     *         ctrav,cvel,modeff
1005    format(
     *         i10,12x,'Divisions/sample',t55,'(k)'/
     *         i10,12x,'Alias'/
     *         f17.6,5x,'Alias low  wavenumber (cm-1)'/
     *         f17.6,5x,'Alias high wavenumber (cm-1)'/
     *         f16.5,6x,'Laser wavenumber(cm-1)'/
     *         f12.1,10x,'Laser Zeeman frequency (Hz)'/
     *         i10,12x,'Carriage travel parameter',t55,'(s)'/
     *         f14.3,8x,'Carriage travel (cm)'/
     *         f14.3,8x,'Carriage velocity (cm/sec)'/
     *         f13.2,9x,'Modulation efficiency',t59,' 50')

        write(lu,1006) srverr,pspect,tspect,scandir,glitchlo,glitchhi,
     *		gltchdev,nserie,nint,ncenter
1006    format(
     *         f15.4,7x,'Sampling position error(Laser fringes)'/
     *         f13.2,9x,'Spectrometer pressure(Torr)'/
     *         f12.1,10x,'Spectrometer temperature(C)'/
     *         i10,12x,'Scan direction'/
     *         f13.3,9x,'Glitch window low'/
     *         f13.3,9x,'Glitch window high'/
     *         f13.3,9x,'Glitch window deviation (%f.s.)'/
     *         i12,12x,'Serial number of control program'/
     *         i10,12x,'Number of points in interferogram'/
     *         i10,12x,'Point number of central fringe',t59,' 60')

	write (lu,1007) nused,ntrans,outlo,outhi,modeapod,apod1,
     *		apod2,apod3,apod4,apod5
1007	format(
     *         i10,12x,'Number of samples used in transform'/
     *         i10,12x,'Transform size'/
     *         f17.6,5x,'Output low  wavenumber (cm-1)'/
     *         f17.6,5x,'Output high wavenumber (cm-1)'/
     *         i10,12x, 'Apodization function'/
     *         f15.4,7x, 'Apodization variable #1'/1p,
     *         e16.6,6x, 'Apodization variable #2'/
     *         e16.6,6x, 'Apodization variable #3'/
     *         e16.6,6x, 'Apodization variable #4'/
     *         e16.6,6x, 'Apodization variable #5',t59,' 70')

	write (lu,1008) pvar0,pvar1,pvar2,pvar3,pvar4,pvar5,pvar6,
     *		pvar7,pvar8,pvar9
1008	format(1p,
     *            e18.8,4x, 'Phase corr variable #0'/
     *            e18.8,4x, 'Phase corr variable #1'/
     *            e18.8,4x, 'Phase corr variable #2'/
     *            e18.8,4x, 'Phase corr variable #3'/
     *            e18.8,4x, 'Phase corr variable #4'/
     *            e18.8,4x, 'Phase corr variable #5'/
     *            e18.8,4x, 'Phase corr variable #6'/
     *            e18.8,4x, 'Phase corr variable #7'/
     *            e18.8,4x, 'Phase corr variable #8'/
     *            e18.8,4x, 'Phase corr variable #9',t59,' 80')

	write (lu,1009) fboffs,bocode,wavcorr,rdsclfct,noiselev,
     *                  uvar9, uvar8, uvar7, uvar6, uvar5,
     *                  uvar4, uvar3, uvar2, uvar1, uvar0
1009	format(1p,
     *            i10,12x, 'File byte offset'/
     *            i10,12x, 'Byte order code'/
     *            e20.10,2x, 'Fractional wavenumber correction'/
     *            e16.6,6x, 'Read scale factor'/
     *            e16.6,6x, 'Assumed noise level'/
     *            e18.8,4x, 'User variable #9'/
     *            e18.8,4x, 'User variable #8'/
     *            e18.8,4x, 'User variable #7'/
     *            e18.8,4x, 'User variable #6'/
     *            e18.8,4x, 'User variable #5' ,t59,' 90'/
     *            e18.8,4x, 'User variable #4'/
     *            e18.8,4x, 'User variable #3'/
     *            e18.8,4x, 'User variable #2'/
     *            e18.8,4x, 'User variable #1'/
     *            e18.8,4x, 'User variable #0'/)
        return
	end

c ----------------------------------------------------------------

        subroutine oldinfrd(lu)

	include 'infmtn.h'
	include 'altinf.h'
	include 'solinf.h'
	include 'extinf.h'
	include 'iounit.h'
        include 'set.h'

c  get parameters from the data header block

c attempt to read the header as an old (type 0) or type 4 IC header
c if it fails here, then it is type 5 or newer, or NSO type 3

	rewind lu
	read(lu,11,err=500) serialno,day,ith,itm,its,fdivr,
     *		resolutn,alias,apin,apmax,freespec,wstart

c the BN edit descriptor ignores blanks, it takes all the non
c blank characters in the field and right-justifies them: 
c example:           ' 23 4 ' = '   234'
11	format(BN,26x,i8/26x,a15/26x,i2,1x,i2,1x,i2/26x,f14.0/
     *     26x,f14.8/26x,i8/26x,f9.3/26x,f9.3/26x,f12.6/26x,f12.6)

	ierr = 10
	read(lu,21,err=9001) wstop,cvel,ctrav,nint,
     *          eltim,sampfreq,delw,nscan,neoa,notput
21	format(BN,26x,f12.6/26x,f11.5/26x,f11.5/26x,i8/
     *          26x,f11.5/26x,f11.5/26x,f17.11/26x,i8/26x,i8/26x,i8)

	ierr = 20
	read(lu,31,err=9001) nsides,iratio,scandir,glitchlo,glitchhi,
     *          gltchdev,user,sample,source,
     *          bmsplt,filter1,dtectr,gaina,id,
     *          ncenter,nwpp,npo,ntrans
31	format(BN,26x,i8/26x,i8/26x,i8/26x,f4.0/26x,f4.0/
     *          26x,f4.0/26x,a20/26x,a20/26x,a20/
     *          26x,a20/26x,a20/26x,a20/26x,a20/26x,a50/
     *          26x,i8/26x,i8/26x,i8/26x,i8)

	spect = 'IC FTS'
	sampfreq = 1000.0*sampfreq
	refwavno = 15798.0025
	fzeeman = 15500000.
	filter2 = ' '
	ambfilt = ' '
	apbfilt = ' '
	ninput = 1
	nm = 360360	! all factors up through  16
	nk = 360360./fdivr
c if it gets to here and then hits EOF it is an old old IC header.
	ierr = 38
	read (lu,41,end=112,err=9001) id(41:75),idecim,
     *			pvar9, pvar8, pvar7, pvar6, pvar5,
     *			pvar4, pvar3, pvar2, pvar1, pvar0
41	format(BN,26x,a35/26x,i8/
     *      26x,e16.6/26x,e16.6/26x,e16.6/
     *      26x,e16.6/26x,e16.6/26x,e16.6/
     *      26x,e16.6/26x,e16.6/26x,e16.6/26x,e16.6)
        decim = idecim

c if it gets to here and then fails it is an old IC header.
	ierr = 50
	read (lu,51,end=114,err=9001) wavcorr,apod1, rdsclfct,noiselev,
     *                  fboffs,bocode,inftype,
     *			uvar9, uvar8, uvar7, uvar6, uvar5,
     *			uvar4, uvar3, uvar2, uvar1, uvar0
51	format(BN,26x,e16.6/26x,f16.0/26x,e16.6/26x,e16.6/
     *      26x,i8/26x,i8/26x,i8/
     *      26x,e16.6/26x,e16.6/26x,e16.6/
     *      26x,e16.6/26x,e16.6/26x,e16.6/
     *      26x,e16.6/26x,e16.6/26x,e16.6/26x,e16.6)

c if this read worked then the header is an IC type 4 header.
        inftype = 4
        call wrtstr(' header is IC type 4')
        go to 1900
c else the header is a type 0 (old IC) header.
112     decim = 0.0
        pvar9 = 0.0
        pvar8 = 0.0
        pvar7 = 0.0
        pvar6 = 0.0
        pvar5 = 0.0
        pvar4 = 0.0
        pvar3 = 0.0
        pvar2 = 0.0
        pvar1 = 0.0
        pvar0 = 0.0
 114    inftype = 0
        call wrtstr(' header is old IC type')
        wavcorr = 0.0
        apod1 = 0
        rdsclfct = 1.0
        noiselev = 0.0
        srverr = 0.0
        fboffs = 0
        uvar9 = 0.0
        uvar8 = 0.0
        uvar7 = 0.0
        uvar6 = 0.0
        uvar5 = 0.0
        uvar4 = 0.0
        uvar3 = 0.0
        uvar2 = 0.0
        uvar1 = 0.0
        uvar0 = 0.0
        go to 1900

c Header is type 5 or later, or NSO type 3
c        5            Info block type     for type 5 or later;
c    04/26/91         date  (mm/dd/yy)    for NSO type 3
500	continue
	rewind lu

        read(lu,2001,err=1500) id(1:75),inftype,serialno,day,
     *         ith,itm,its,eltim,nscan,wstart,wstop,npo
2001    format(BN,a75/ i12/ i12/ 5x, a10/ i7,1x,i2,1x,i2/
     *               f14.3/ i12/ f17.6/ f17.6/ i12)

	ierr = 10
        read(lu,2002,err=9001) nwpp,delw,resolutn,freespec,apin,apmax,
     *         user,source,sample,spect
2002    format(BN,i12/ f20.13/ f16.9/ f16.5/ f12.1/ f12.1/
     *               a20/ a20/ a20/ a20)

	ierr = 20
        read(lu,2003,err=9001) filter1,filter2,bmsplt,dtectra,gaina,
     *         dtectrb,gainB,AmBfilt,apbfilt,ratsig
2003    format(BN,a20/ a20/ a20/ a20/ a20/ a20/ a20/ a20/ a20/ f14.3)

	ierr = 30
        read(lu,2004,err=9001) ratrms,sampfreq,idecim,neoa,nwos,ninput,
     *         notput,nsides,iratio,nm
2004    format(BN, f14.3/ f13.2/ i12/ i12/ i12/ i12/ i12/ i12/i12/i12)
        decim = idecim

	ierr = 40
        read(lu,2005,err=9001) nk,alias,aliaslo,aliashi,refwavno,
     *         fzeeman,ns,ctrav,cvel,modeff
2005	format(BN, i12/ i12/ f17.6/ f17.6/ f16.5/ f16.5/ i12/
     *             f14.3/ f14.3/ f13.2)  

	ierr = 50
        read(lu,2006,err=9001) srverr,pspect,tspect,scandir,
     *		glitchlo,glitchhi,gltchdev,nserie,nint,ncenter
2006    format(BN, f15.4/ f13.2/ f12.1/ i12/ f13.2/ f13.2/ f13.2/ 
     *             i12/ i12/ i12)


	ierr = 60
	read (lu,2007,err=9001) nused,ntrans,outlo,outhi,modeapod,
     *		apod1,apod2,apod3,apod4,apod5
2007	format(BN, i12/	i12/ f17.6/ f17.6/ i12/ f15.4/ f15.4/ f15.4/ 
     *              f15.4/ f15.4)

	ierr = 70
	read (lu,2008,err=9001) pvar0, pvar1, pvar2,pvar3,pvar4,pvar5,
     *		pvar6,pvar7,pvar8,pvar9
2008	format(BN, e21.6/ e21.6/ e21.6/ e21.6/ e21.6/ e21.6/ e21.6/
     *             e21.6/ e21.6/ e21.6)

	ierr = 80
	read (lu,2009,err=9001)fboffs,bocode,wavcorr,rdsclfct,noiselev,
     *			uvar9, uvar8, uvar7, uvar6, uvar5,
     *			uvar4, uvar3, uvar2, uvar1, uvar0
2009	format(BN, i12/ i12/ e21.9/ e21.6/ e21.6/ e21.6/ e21.6/ e21.6/
     *             e21.6/ e21.6/ e21.6/ e21.6/ e21.6/ e21.6/ e21.6)

        write (wrtbuf,'(a,i2)') ' header is type',inftype
        call wrtstr(wrtbuf)
	fdivr = float(nm)/float(nk)
	freespec = 0.5d0*refwavno*fdivr
	hspect = 0.0
        go to 1900

c last try - NSO type 2 or 3
1500	continue
	rewind lu
	read(lu,1511,err=9000) id,day,serialno,ith,itm,its,eltim,nscan,
     *		inftype,npo,delw,resolutn,wstart,wstop,neoa,nwpp,user
1511	format(BN,a80/5x,a10/i13/i7,1x,i2,1x,i2/f17.3/i13/
     *            i13/i13/f20.12/f17.9/f20.6/f20.6/i13/i13/a20)
	ierr = 15

	read(lu,1513,err=9001) 
     *		source,sample,dtectr,gaina,ratsig,ratrms,iratio,ninput,
     *          notput,detrms,spect,filter1,filter2,bmsplt,alias
1513	format(BN,a20/a20/a20/a20/f17.3/f17.3/i13/i13/
     *          i13/f15.1/a20/a20/a20/a20/i13)
	ierr = 30

	read(lu,1515,err=9001) nm,nk,ns,modeff,srverr,hetpm1,hetpm2,
     *          refwavno,fzeeman,apin,wdin,flin,htout,wdout,flout
1515	format(BN,i13/i13/i13/f16.2/f18.4/i13/i13/f19.5/f19.5/
     *            f15.1/f15.1/f15.1/f15.1/f15.1/f15.1)

	ierr = 45
	read(lu,1517,err=9001) pspect,tspect,hspect,patm,tatm,hatm,
     *          julday,ntos,hastrt,hastop,dcmean,zenstrt,zenstop,amstrt
1517	format(BN,f16.2/f15.1/f16.2/f15.1/f15.1/f16.2/
     *         i13/i13/f17.3/f17.3/f20.6/f20.6/f20.6/f17.3)

	ierr = 59
	read(lu,1521,err=9001) 
     *          amstop,vcstrt,vcstop,sclim,dimage,soffst,eoffst,solmu,
     *          nint,ncenter,iapod,ntrans,npt,nwos,nserie
1521	format(BN,f17.3/e20.8/e20.8/f17.3/f15.1/f15.1/f15.1/f18.4/
     *          i13/i13/i13/i13/i13/i13/i13)
        apod1 = iapod

	ierr = 74
	if (inftype .eq. 3) then
		read (lu,1551,err=9001) 
     *			wavcorr,rdsclfct,noiselev,fboffs,bocode,
     *			uvar9, uvar8, uvar7, uvar6, uvar5,
     *			uvar4, uvar3, uvar2, uvar1, uvar0
1551		format(BN,e20.3/e20.3/e20.3/i13/i13/e20.3/e20.3/e20.3/
     *                    e20.3/e20.3/e20.3/e20.3/e20.3/e20.3/e20.3)
	end if
        write (wrtbuf,'(a,i2)') ' header is NSO type',inftype
        call wrtstr(wrtbuf)
	fdivr = float(nm)/float(nk)
	freespec = 0.5d0*refwavno*fdivr
	sampfreq = fzeeman/nk
	decim = 1.0
	aliaslo = freespec*(alias - 1)
	aliashi = aliaslo + freespec
	cvel =  fzeeman/(nm*2.0*refwavno)
	ctrav = 0.5*nk*ns/refwavno
	scandir = 1
	nmax = nint-ncenter
	if (ncenter.gt.nmax) nmax=ncenter
	if (nmax*alias .gt. 0.0) then
           apmax = 2667.0*sqrt(8.0/(nmax*alias))
	else
           apmax = 0.0
	endif
	ambfilt = 'unknown'
	apbfilt = 'unknown'

1900    continue
    
* set data scaling factor
        if ( rdsclfct .eq. 0.0 ) rdsclfct = 1.0  ! take care of broken headers
        if ( rdscale .gt. 0 ) then
           if ( rdscale .eq. 2 ) then
              rdfctr = rdsclfct
           end if
        else
           rdfctr = 1.0
        end if
        if ( rdnorm ) then
           call wrtstr(' Data will be normalized to 1.0')
        else
           if ( rdscale .gt. 0 ) then
              write(wrtbuf,'(a,g14.6)')
     &              ' Data scaling factor          : ', rdfctr
              call wrtstr(wrtbuf)
           else
              call wrtstr(' Data scaling is turned off')
           end if
        end if

* write out wavenumber correction factor        
        if ( wavcorr .ne. 0.d0 ) then
           write(wrtbuf,'(a,e18.16)') 
     &           ' Wavenumber correction factor : ', wavcorr
           call wrtstr(wrtbuf)
        else
           call wrtstr(' No wavenumber correction.')
        end if
                  
	if (nwpp .eq. 1) data_is = 'Real'
	if (nwpp .eq. 2) data_is = 'Complex'
	if (nwos .eq. 1) xaxis_is = 'Wavelength'
	if (nwos .eq.-1) xaxis_is = 'Wavenumber'
	if (neoa .eq. 1) spectype = 'Emission'
	if (neoa .eq.-1) spectype = 'Absorption'
	if (ninput .eq. 1) input_is = 'A only'
	if (ninput .eq. 2) input_is = 'Both A&B'
	if (iratio .eq. 0) ratio_is= 'Off'
	if (iratio .eq. 1) ratio_is= 'On'
	if (icrflg .eq. 0) aircorr = 'No'
	if (icrflg .eq. 1) aircorr = 'Yes'
	if (scandir .eq. 1) scan_is = 'Forward'
	srverr = 1000.0*srverr
	wref = wstart
	npt = 1
	strtim = its + 60.*(itm + 60*ith)
	stptim  = strtim + eltim
c save delw, nwpp for future reads
	disp = delw
	nwppt = nwpp

	return

9000	call wrtstr(' Hdr file is not a valid type - abort')
	inftype = -1
	return
9001	write (wrtbuf,'(a,i3)') 
     &  ' Error :  read failed due to honky header file at line ',
     &  ierr
        call wrtstr(wrtbuf)
	inftype = -1
	return
	end

c ----------------------------------------------------------------
c read FITS CDROM info block

	subroutine fitinfrd(nb)

	include 'datetc.h'
	include 'infmtn.h'
	include 'altinf.h'
	include 'extinf.h'
	include 'iounit.h'

	integer nbstart
	integer nerr
        character*14400 chbuf
        character line*80

	NAMELIST /hdalist/ 
     *	    acqvers, aircorr, alias, aliashi, aliaslo,
     *	    amstop, amstrt, apang, apin, apmax,
     *	    apod1, apod2, apod3, apod4, apod5, apsep, archname,
     *	    bandhi, bandlo, bmsplt, bocode, 
     *      comment0, comment1, comment2, comment3, comment4,
     *      comment5, comment6, comment7, comment8, comment9,
     *	    ctrav, cvel, data_is, day, dblpass, dcmean, 
     *	    decim, decobs, decreq, delw, dimage, dtectrA, dtectrB,
     *	    eltim, eoffst, fboffs, filter1, filter2,
     *	    flin, flout, freespec, ftsmode, fzeeman
	NAMELIST /hdblist/ 
     *	    gainA, gainAmB, gainApB, gainB, gainBadj, 
     *	    glitchhi, glitchlo, gltchdev, 
     *	    hastop, hastrt, hatm, 
     *	    hetpm1, hetpm2, hfcutAmB, hfcutApB, hspect,
     *	    id, inftype, input_is, inskip, 
     *	    julday, lfcutAmB, lfcutApB, modeapod, modeff, modephzc,
     *	    ncenter, nint, nK, nM, noiselev, npo, 
     *	    nS, nscan, ntrans, nused,
     *	    origtype, outhi, outlo, 
     *	    patm, predisp, pspect, pvar0, pvar1, pvar2, pvar3, pvar4, 
     *	    pvar5, pvar6, pvar7, pvar8, pvar9
	NAMELIST /hdclist/ 
     *	    raobs, rareq, ratio_is, ratrms, ratsig, rdsclfct,
     *	    refwavno, resolutn,
     *	    sampfreq, sample, scan_is, sclim, serialno,
     *	    sethi1, sethi2, setlo1, setlo2, setval1, setval2,
     *	    soffst, solmu, source, spect, spectype, srverr, 
     *	    ststop, ststrt, 
     *	    tatm, telescop, timestr, tspect, user, utstop, utstrt, 
     *	    uvar0, uvar1, uvar2, uvar3, uvar4,
     *	    uvar5, uvar6, uvar7, uvar8, uvar9,
     *	    vcstop, vcstrt, wavcorr, wstart, wstop,
     *	    xaxis_is, zenstop, zenstrt

c get FITS header, re-format, write out as 3 namelist files temporarily
        call mktmp(tmpnam)
        open(18,file=tmpnam)
	rewind(18)
	write (18,*) '&hdalist'

        call mktmp(tmpnam)
	open(19,file=tmpnam)
	rewind(19)
	write (19,*) '&hdblist'

        call mktmp(tmpnam)
	open(20,file=tmpnam)
	rewind(20)
	write (20,*) '&hdclist'

c get FITS header, assumed no more than 5 blocks
	nbstart = 0
	call readata(datain,nbstart,chbuf,14400,nerr)
        call wrtstr(' reading header')

	do i=1,180
	    ia = 80*(i-1) + 1
	    line = chbuf(ia:ia+79)
	    if (line(1:3) .eq. 'END') go to 1000
c skip FITS variables
	    if (line(1:6) .eq. 'SIMPLE' .or.
     *		line(1:6) .eq. 'BITPIX' .or.
     *		line(1:5) .eq. 'NAXIS' .or.
     *		line(1:6) .eq. 'NAXIS1' .or.
     *		line(1:6) .eq. 'ORIGIN' .or.
     *		line(1:4) .eq. 'DATE' .or.
     *		line(1:8) .eq. 'IRAFNAME' .or.
     *		line(1:8) .eq. 'IRAF-MAX' .or.
     *		line(1:8) .eq. 'IRAF-MIN' .or.
     *		line(1:8) .eq. 'IRAF-BPX' .or.
     *		line(1:8) .eq. 'IRAFTYPE' .or.
     *		line(1:6) .eq. 'WCSDIM' .or.
     *		line(1:6) .eq. 'CTYPE1' .or.
     *		line(1:6) .eq. 'CRVAL1' .or.
     *		line(1:6) .eq. 'CDELT1' .or.
     *		line(1:5) .eq. 'CD1_1' .or.
     *		line(1:6) .eq. 'LTM1_1' .or.
     *		line(1:8) .eq. 'WAT0_001' .or.
     *		line(1:8) .eq. 'WAT1_001' .or.
     *		line(1:7) .eq. 'HISTORY' .or.
     *		line(1:3) .eq. '   ') go to 900

	    il = 78
	    n = index(line,"'") 
	    if (n .gt. 0) then
	        il = index(line(n+1:80),"'") + n
	    else
	        n = index(line,'/') - 1
	        if (n .gt. 0) il = n
	    endif
c sort alphabet into 3 files to avoid overloading the compiler
	    if (ichar(line(1:1)) .lt. 65) then
		go to 900
	    elseif (ichar(line(1:1)) .lt. 71) then
      		write (18,'(a)') line(1:il)
	    elseif (ichar(line(1:1)) .lt. 82) then
      		write (19,'(a)') line(1:il)
	    elseif (ichar(line(1:1)) .lt. 91) then
      		write (20,'(a)') line(1:il)
	    endif
900	    continue
	enddo
c NOTE**: ia+79 was the last character read
1000	n = (ia + 79 + 2879)/2880
	nb = 2880*n
	write (wrtbuf,'(1x,i4,a)') n,' FITS blocks in header'
        call wrtstr(wrtbuf)

	write (18,*) '&END'
	write (19,*) '&END'
	write (20,*) '&END'
	rewind (18)
	read (18,hdalist,err=2990)
	rewind (19)
	read (19,hdblist,err=2991)
	rewind (20)
	read (20,hdclist,err=2992)

c all done - fix up some variables and quit
c	sigmax = (0.5d0*refwavno*nm)/nk
c	ntrans = sigmax/delw
c	if (decim .eq. 0.0) decim = 1.0
c	if (alias .eq. 0) alias = 1
	fdivr = float(nm)/float(nk)

	if (timestr(2:2) .eq. ':') read (timestr,11) ith,itm,tmptim
11	format (i1,1x,i2,1x,f5.2)
	if (timestr(3:3) .eq. ':') read (timestr,13) ith,itm,tmptim
13	format (i2,1x,i2,1x,f5.2)
	strtim = tmptim + 60.0d0*(itm + 60.0d0*ith)
	stptim = strtim + eltim

	nmax = nint-ncenter
	if (ncenter .gt. nmax) nmax=ncenter
	if (nmax*alias .gt. 0.0) then
	    apmax = 2667.0*sqrt(8.0/(nmax*alias))
	else
	    apmax = 0.0
	endif

c	if (nwpp .eq. 3) nwpp = 1

	close(18,status= 'delete',iostat=i)
	close(19,status= 'delete',iostat=i)
	close(20,status= 'delete',iostat=i)
	return

2990	call wrtstr(' error in keywords a-f')
	return
2991	call wrtstr(' error in keywords g-p')
	return
2992	call wrtstr(' error in keywords r-z')
	return
        end
