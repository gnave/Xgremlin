c \gremlin\src\prnio.for   Ver. 1.0

c  Revision 1.04 30.10.93  prnt,prplt dropped
c  Revision 1.03 26.05.93  Infprnt: npt dropped, alias added
c  Revision 1.02 16.05.93  Wavelimits print corrected
c  Revision 1.01 04.06.91  Total P.d. print corrected
c  Version  1.00 04.04.91

c ----------------------------------------------------------

	subroutine infprn

	include 'datetc.h'
	include 'infmtn.h'
	include 'altinf.h'
	include 'iounit.h'

	integer nmax
	double precision alto,altf
	character*10 labels(3)
	character*3 char3
	data labels(1), labels(2)   / 'absorption', 'not given '/
	data labels(3)  / 'emission  '/

c print information block

	write (wrtbuf, 2001) day,id
	call wrtout(output,wrtbuf)
 2001	format(' FTS information block ',a10,a69)
	write (wrtbuf, 2003) serialno,npo
	call wrtout(output,wrtbuf)
	write (wrtbuf, 2004) source
	call wrtout(output,wrtbuf)
 2003	format(' Scan no.',i4,', ',i7,' points.')
 2004   format(' Source: ',a20)
	write (wrtbuf,2005) filter1
	call wrtout(output,wrtbuf)
	write (wrtbuf,2006) labels(neoa+2)(1:10) 
	call wrtout(output,wrtbuf)
 2005	format(' Optical filter  : ',a20)
 2006	format(' Type of spectrum: ',a10) 
	call disper
	dsp = 1000.0*xpara
	xparb = wstart
	call stow
	alto = xparb
	xparb = wstop
	call stow
	altf = xparb
	write (wrtbuf,2007) dsp
	call wrtout(output,wrtbuf)
	write (wrtbuf,2008) wstart,wstop, alto, altf
	call wrtout(output,wrtbuf)
 2007	format(' Dispersion ',f12.6,' mK/point') 
 2008	format(' File range ',
     &         f14.4,' to ',f14.4,' K',8x,f13.5,' to ',f13.5,' nm')
	itho=strtim/3600.
	tso = strtim - 3600.0d0*itho
	itmo = tso/60.
	tso = tso - 60.*itmo
	write (wrtbuf,2017) nscan,itho,itmo,tso
	call wrtout(output,wrtbuf)
2017	format(' Scan parameters:',10x,i4,' scans, starting at',
     &         2i3,f5.1)
	nmax = npo - ncenter
	if (ncenter.gt.nmax) nmax = ncenter
	res = 1000.0*resolutn
	write (wrtbuf,2023) sampfreq
	call wrtout(output,wrtbuf)
	write (wrtbuf,2024) aliashi,alias
	call wrtout(output,wrtbuf)
	write (wrtbuf,2025) res
	call wrtout(output,wrtbuf)
	write (wrtbuf,2026) nmax
	call wrtout(output,wrtbuf)
 2023	format(' Sample rate =',f6.0,' Hz.')
 2024	format(' Alias high =',f10.3,' cm^-1;  Alias ',i1)
 2025	format(' Resolution =',f6.2,' mk')
 2026	format(' Max. resolving power =',i8)
	write (wrtbuf,2027) nint,ncenter
	call wrtout(output,wrtbuf)
 2027	format (1x,i8,' points recorded. central fringe at',i8)
	write (wrtbuf,2033) bmsplt
	call wrtout(output,wrtbuf)
 2033	format(' Beamsplitter: ',a10)
	write (wrtbuf,2041) apin, apmax
	call wrtout(output,wrtbuf)
 2041	format(' input diam =',f5.2,' mm. Max =',f5.2,' mm.')
	char3 = '** '
	if (icrflg .eq. 0) char3 = 'no '
	write (wrtbuf,2051)
	call wrtout(output,wrtbuf)
	write (wrtbuf,2052) pspect
	call wrtout(output,wrtbuf)
	write (wrtbuf,2053) tspect
	call wrtout(output,wrtbuf)
	write (wrtbuf,2054) hspect
	call wrtout(output,wrtbuf)
	write (wrtbuf,2055) char3
	call wrtout(output,wrtbuf)
 2051	format(' Instrument:')
 2052	format(' Pressure =',f6.1,' torr')
 2053	format(' Temperature =',f5.1,' C')
 2054	format(' H2O pressure =',f5.1,' torr')
 2055	format(' ',a3,' index correction applied')
	char3 = 'on '
	if (iratio .eq. 0) char3 = 'off'
	write (wrtbuf,2056) dtectrA, dtectrB, char3
	call wrtout(output,wrtbuf)
 2056	format(' Detector A: ',a20,'  Detector B: ',a20,'    Ratio ',a3)
	return
	end

c ----------------------------------------------------------------

	subroutine infout

	include 'inparms.h'
	include 'infmtn.h'
	include 'iounit.h'
	include 'altinf.h'

	character*10 labels(8)
	character*10 lable(5)
	character*10 itemp
	common/leftout/sigma0,sigmax
	double precision sigma0,sigmax

	data labels /'absorption', 'not given ', '  emission',
     *             'length    ', 'number    ', 
     *             'a         ', 'k         ', 'a         '/

	data lable/'No        ','Part      ', 'Full      ',
     *            'flagged   ', 'removed   '/

c	data dpr /57.2957795/


	call page(2)

	write(logfil,2001) 
2001  format(30x,'FTS Information Block')

	 write (logfil, 2003) serialno,source
2003	format(' Scan no.',i4,', ',a20)

	write(logfil,2005)labels(neoa+2)
2005  format(' Type of spectrum: ',a10)

	write(logfil,2013) filter1, filter2
2013  format(' Optical filters: ',a,a)

	itho = strtim/3600.
	tso = strtim - 3600. * itho
	itmo = tso / 60.
	tso = tso - 60. * itmo
	itso = tso
	tso = tso - itso
	jtso = tso*10
	ithf = stptim / 3600.
	tsf = stptim - 3600.*ithf
	itmf = tsf / 60.
	tsf = tsf - 60. * itmf
	itsf = tsf
	tsf = tsf - itsf
	jtsf = tsf*10
	tppt = eltim/npo
	temp = 1000.0 * tppt

	write(logfil,2017)
     * nscan,itho,itmo,itso,jtso,ithf,itmf,itsf,jtsf,eltim,temp
2017  format(' Scan parameters:',
     *       4x,i4,' scans, from ',i3,2i3.2,'.',i1,
     *       ' to',i3,2i3.2,'.',i1,
     *        6x,' Integration time ',f7.1,' sec,',f6.2,
     *          ' msec/point.')

	nmax = npo - ncenter
	if (ncenter .gt. nmax + inskip) nmax = ncenter - inskip
	res = (1000.0*freespec)/nmax

	write(logfil,2023)sampfreq,sigma0,sigmax,alias,
     *			2.0*ctrav,res,nmax*alias,resolutn
2023	format(10x,'Sample rate =',f6.0,' hz.',
     *       10x,'Alias range ',f10.3,' to ',f10.3,' cm-1',
     *	     10x,'Alias order =',i3,
     *	     10x,'Total path difference = ',f6.2,' cm',
     *	     10x,'Resolution =',f7.2,' mK, max. resolving power =',i8,
     *	     10x,'Theoretical resolution element (1/2*path ',
     *           'difference) =',f11.6,' cm-1')

	write(logfil,2025) outlo, outhi, npo, ncenter, spect,bmsplt
2025  format(10x,'Wavelimits: ',f10.2,',',f10.2,
     *          10x,i7,' points recorded. Central fringe at',i8,
     *          ' Spectrometer: ',a10,
     *          10x,'Beamsplitter: ',a10)

	write(logfil,2041) apin,apmax,refwavno
2041  format(10x,'Input aperture =',f5.2,' mm.;',
     *        5x,'Max. aperture =',f5.2,' mm.',
     *       10x,'Assumed laser wavenumber =',f11.4,' cm-1')

	write(logfil,2051) pspect,tspect
2051  format(10x,'Spectrometer pressure =',f6.1,
     *       ' torr, temperature =',f5.1,'C')

	write(logfil,2085) nserie
2085  format(10x,'Acquisition Program Version No.',i3)

	  itemp = 'on '
	  if (iratio .eq. 0) itemp = 'off'
	write(logfil,2055) dtectra,dtectrb,itemp,gaina,gainb
2055  format(' Detectors: ',20x,a,3x,a,
     *       10x,'Ratio ',a3,6x,'Gains =',a20,3x,a20)

	write(logfil,2096) lable(iprnt+1), inskip
2096  format(' Input options:',
     *          10x,a4,' printout.',
     *          10x,'Skip count = ',i6,' points.')

	 if (imax .ne. 0) write(logfil,2100) imax
2100  format(10x,'Central fringe to be found at point',i8)

	ir1 = 1
	if (irng1 .lt. nwanted) ir1 = irng1
	ir2 = nwanted
	if (irng2 .lt. nwanted) ir2 = irng2
	itemp = lable(4)
	if (deglch) itemp = lable(5)

	 write(logfil,2102) xsigma,itemp,ir1,ir2
2102  format(10x,'Absolute data values greater than',
     *       f4.0,'*rms to be ',a7,
     *        15x,' between points',i8,' and',i8)

	if (nexmpt .ne. 0) write(logfil,2108) (iexmpt(i),i=1,nexmpt)
2108          format(10x,'Exempted points are: ',i8,(40x,i8))

	return

	end

c ----------------------------------------------------------------
