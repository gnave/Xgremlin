
c  Revision 2.43 93/19/08 bocode saved before infread for type 0 headers
c  Revision 2.42 93/08/20 If NOAA FTS, do not set sampfreq
c  Revision 2.41 93/05/30 Fix handling of nwpp, data_is
c  Revision 2.40 93/05/28 Hdredit added
c  Revision 2.39 93/05/25 Derived parameters calculated here
c  Revision 2.38 93/05/16 Telescope parameters not written for ftsmode=Lab
c  Revision 2.37 93/02/23 fitinfrd, sinfrd moved to infioalt.for; b6 added
c  Revision 2.34 91/08/31 Header file changed
c  Revision 2.33 91/08/28 Fixed logic in type0/4 decision, type 0 defaults
c  Revision 2.32 91/08/22 Split from dataio to avoid compiler bug
c  Revision 2.31 91/08/21 Type 5 modified to handle NSO parameters
c  Revision 2.30 91/05/26 Infread handles NSO headers
c  Revision 2.29 91/05/26 Infwrt copied from grammy; always inftype=5


        subroutine infwrt(lu)

	call b6infwrt(lu)

        return
	end

c ----------------------------------------------------------------

	subroutine infread(lu)

	include 'infmtn.h'
        include 'altinf.h'
	include 'extinf.h'

	maxintgr = 2000000000 

c  initialize all common variables
	if (bocode .ne. maxintgr) ibsav = bocode
	call b6init
c  get parameters from the data header block
c  first attempt to read the header as a new (type 6) header
	call b6infrd(lu)
	if (inftype .eq. 6) go to 100
	ierr = inftype

c read failed - 
c attempt to read the header as an old (type 0) or type 4 IC header
c or NSO type 5, or NSO type 3
	call oldinfrd(lu)
	if (inftype .ge. 0) go to 100

* nothing works - give up
	return

 100    continue
	if (bocode .eq. maxintgr) bocode = ibsav
        fdr = fdivr

* store sampling parameters for plotting
        if (drivevel .eq. 0.0) then
           drivevel = cvel * refwavno
        endif
        call ssmpar(alias, freespec, refwavno, drivevel)

	return

	end

c ----------------------------------------------------------------
        subroutine b6init

	include 'infmtn.h'
	include 'altinf.h'
	include 'solinf.h'
	include 'extinf.h'
	include 'inparms.h'

	character*20 blanks
        integer i1mach
        real r1mach

	realmax = r1mach(2)    ! largest floating value
	maxintgr = i1mach(9)   ! largest integer value
	unknown = 'uninitialized'
	blanks = '                    '

	inftype = maxintgr 
        origin = unknown
	id   = 'Not specified'
	day  = 'Not specified'
        observ = blanks
	serialno = 0
	wstart = 0.0
	wstop = 1.0e6
	npo = maxintgr 
	data_is = blanks
	delw = 1.0
	resolutn = realmax 
	fboffs = 0
	bocode = maxintgr 
	xaxis_is = blanks
c File reduction parameters 
	wavcorr = 0.d0
	rdsclfct = 1.0
	noiselev = 0.0
c Observing parameters
	ftsmode = blanks
	user   = blanks
	nscan = maxintgr 
	timestr  = blanks
	eltim = realmax 
	spectype = blanks
	source  = blanks
	sample = unknown
	sample_i = unknown
	sample_a = unknown
	sample_b = unknown
	filter1 = unknown
	filter2 = unknown
        filter_i = unknown
        filter_a = unknown
        filter_b = unknown
	predisp = unknown
	input_is = blanks
	apin = realmax 
	apmax = realmax 
	spect  = blanks
	bmsplt  = blanks
	dtectrA = blanks
	gainA = blanks
	dtectrB = blanks
	gainB = blanks
	gainBadj = maxintgr 
	gainApB = maxintgr 
	gainAmB = maxintgr 
	lfcutApB = maxintgr 
	hfcutApB = maxintgr 
	lfcutAmB = maxintgr 
	hfcutAmB = maxintgr 
	modephzc = 0
        s_delay = maxintgr
c Scan parameters
        fdr = realmax
	nm = maxintgr 
	refwavno = realmax 
	fzeeman = realmax 
	fclock = realmax 
	alias = maxintgr 
	nk = maxintgr 
	sampfreq = realmax 
	decim = realmax
	ns = maxintgr 
	ctrav = 0.0           ! otherwise the format in header will overflow
	cvel = realmax 
        drivevel = 0.0
	motorvel = realmax 
	kernlen = maxintgr
	epsilon = realmax 
	fracmask = realmax 
	fracbw = realmax 
	scan_is = blanks
	dblpass = unknown 
	ratsig = realmax 
	ratrms = realmax 
	ratio_is = blanks
	glitchlo = realmax 
	glitchhi = realmax 
	gltchdev = realmax 
	hetpm1 = maxintgr 
	hetpm2 = maxintgr 
	modeff = realmax 
	srverr = realmax 
	pspect = 0.0
	tspect = realmax 
	hspect = 0.0
	flin = 0.0
	flout = 0.0
	acqvers  = unknown 
c Transform parameters
	nint = 0
	inskip = 0
	ncenter = 0
	nused = 0
	ntrans = 0
	aliaslo = 0.0
	aliashi = 0.0
	freespec = 0.0
	outlo = 0.0
	outhi = 0.0
	bandlo = 0.0
	bandhi = 0.0
	aircorr = blanks
	modeapod = 0
	apod1 = 0.0
	apod2 = realmax 
	apod3 = realmax 
	apod4 = realmax 
	apod5 = realmax 
	pvar1 = 0.0
	pvar2 = realmax 
	pvar3 = realmax 
	pvar4 = realmax 
	pvar5 = realmax 
	pvar6 = realmax 
	pvar7 = realmax 
	pvar8 = realmax 
	pvar9 = realmax 
	setlo1 = maxintgr 
	sethi1 = maxintgr 
	setval1 = realmax 
	setlo2 = maxintgr 
	sethi2 = maxintgr 
	setval2 = realmax 
	archname = blanks
	origtype = maxintgr 
c User variables
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
c Telescope parameters
	telescop = unknown 
	patm = realmax 
	tatm = realmax 
	hatm = realmax 
	julday = maxintgr 
	hastrt = realmax 
	hastop = realmax 
	dcmean = realmax 
	zenstrt = realmax 
	zenstop = realmax 
	amstrt = realmax 
	amstop = realmax 
	vcstrt = realmax 
	vcstop = realmax 
	sclim = realmax 
	dimage = realmax 
	soffst = realmax 
	eoffst = realmax 
	solmu = realmax 
	utstrt = unknown 
	utstop = unknown 
	ststrt = unknown 
	ststop = unknown 
	rareq  = unknown 
	decreq = unknown 
	raobs = unknown 
	decobs = unknown 
	apsep = realmax 
	apang = realmax 
c User comments for archive
	comment0 = blanks 
	comment1 = unknown
	comment2 = unknown
	comment3 = unknown
	comment4 = unknown
	comment5 = unknown
	comment6 = unknown
	comment7 = unknown
	comment8 = unknown
	comment9 = unknown

        return
	end
c ----------------------------------------------------------------

      subroutine b6infwrt(lu)

      include 'infmtn.h'
      include 'altinf.h'
      include 'solinf.h'
      include 'extinf.h'
      include 'inparms.h'
      
      integer i1mach
      real r1mach
      double precision tmptim
      
      realmax = r1mach(2)
      maxintgr = i1mach(9)
      unknown = 'uninitialized'
      
      ith = strtim/3600.
      tmptim = strtim - 3600.0d0*ith
      itm = tmptim/60.
      tmptim = tmptim - 60.0d0*itm
      write (timestr,11) ith,itm,tmptim
 11   format (i2,':',i2,':',f5.2)
      eltim = stptim - strtim
      if (eltim .lt. 0.0) eltim = eltim + 86400.

      if ( fdr .lt. realmax )  then ! NIST FTSs use fdr instead of M/K
         fdivr = fdr
      else
         fdivr = float(nm)/float(nk)
      end if
      if (refwavno .eq. 0.0) refwavno = 15798.0025d0
      freespec = 0.5d0*refwavno*fdivr
      aliaslo = (alias - 1)*freespec
      aliashi = aliaslo + freespec
      
      if (nwpp .eq. 1) data_is = 'Real' 
      if (nwpp .eq. 2) data_is = 'Complex' 
      
      bocode = mbocode          ! save the machine byte order code
      origtype = inftype
      inftype = 6
      rdsclfct = 1.0            ! always write scale factor 1.0

      rewind lu

      call hputc(lu, '        BASIC PARAMETERS')
      call hputs(lu, 60, 'id', id, '')

      if (serialno .ne. 0) 
     & call hputi(lu, 20, 'serialno', serialno, 'Scan serial number')

      call hputi(lu, 20, 'inftype', 6 ,'Info Block type')

      if (origin(1:13) .ne. unknown(1:13))
     & call hputs(lu, 20, 'origin', origin, 'Institution')

      if (spect(1:13) .ne. unknown(1:13))
     & call hputs(lu, 20, 'spect', spect, 'FTS instrument')

      if (day(1:3) .ne. 'Not')
     & call hputs(lu, 12, 'day', day, ' ')

      if (user(1:13) .ne. unknown(1:13))
     & call hputs(lu, 20, 'user', user, 'Observer')

      call getver(creator)
      call hputs(lu,20,'creator',creator,'Creator of this file')

      if (acqvers(1:13) .ne. unknown(1:13))
     & call hputs(lu, 20, 'acqvers', acqvers, 'Acquisition software')

        
      write(lu,'(a)') '/'
      call hputc(lu, '        FILE PARAMETERS')
      
      if (wstart .lt. realmax) 
     & call hputd(lu,20,'wstart',wstart,'First wavenumber in file')
      
      if (wstop .lt. realmax) 
     & call hputd(lu,20,'wstop',wstop,'Last wavenumber in file')
      
      if (npo .lt. maxintgr)
     & call hputi(lu, 20, 'npo', npo, 'Number of points in file')
      
      if (data_is(1:13) .ne. unknown(1:13))
     & call hputs(lu, 20, 'data_is', data_is, 'Real or Complex')
      
      if (delw .lt. realmax)
     & call hputd(lu, 20, 'delw', delw, 'Dispersion (cm-1/point)')
      
      if (resolutn .lt. realmax)
     & call hputr(lu,20,'resolutn',resolutn,
     & 'Spectral resolution (cm-1)')
      
      if (fboffs .lt. maxintgr)
     & call hputi(lu, 20, 'fboffs', fboffs, 'File byte offset')
      
      if (bocode .lt. maxintgr)
     & call hputi(lu,20,'bocode',bocode,
     & '0: little endian, 1: big endian')
      
      if (xaxis_is(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'xaxis_is',xaxis_is,
     & 'Wavenumber, Wavelength, Index, Other')

      
      write(lu,'(a)') '/'
      call hputc(lu, '        FILE REDUCTION PARAMETERS')
      
      if (wavcorr .lt. realmax)
     & call hputd(lu,20,'wavcorr',wavcorr,'Scale error correction')
      
      if (rdsclfct .lt. realmax)
     & call hputr(lu,20,'rdsclfct',rdsclfct,'Read scale factor')
      
      if (noiselev .lt. realmax)
     & call hputr(lu, 20,'noiselev',noiselev,'Assumed noise level')

      
      write(lu,'(a)') '/'
      call hputc(lu, '        OBSERVING PARAMETERS')
      
      if (ftsmode(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'ftsmode',ftsmode,
     & 'Lab,Sun,Stellar,Polarization,Minimum')
      
      if (nscan .lt. maxintgr)
     & call hputi(lu,20,'nscan',nscan,'Number of co-added scans')
      
      if (timestr(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'timestr',timestr,'Starting time (MST)')
      
      if (eltim .lt. realmax)
     & call hputd(lu,20,'eltim',eltim,'Elapsed time (seconds)')
      
      if (spectype(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'spectype',spectype,'Emission or Absorption')
      
      if (source(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'source',source,'Source')
      
      if (sample(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'sample',sample,'Sample')
      
      if (sample_i(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'sample_i',sample_i,'Sample at input')
      
      if (sample_a(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'sample_a',sample_a,'Sample at detector A')
      
      if (sample_b(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'sample_b',sample_b,'Sample at detector B')
      
      if (filter1(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'filter1',filter1, 'Optical filter #1')
      
      if (filter2(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'filter2',filter2, 'Optical filter #2')
      
      if (filter_i(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'filter_i',filter_i, 'Optical input filter')
      
      if (filter_a(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'filter_a',filter_a, 'Optical filter output A')
      
      if (filter_b(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'filter_b',filter_b, 'Optical filter output B')
      
      if (predisp(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'predisp',predisp,'Predisperser settings')
      
      if (input_is(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'input_is',input_is,'A, B, A-B, (A-B)/(A+B)')
      
      if (apin .lt. realmax)
     & call hputr(lu,20,'apin',apin,'Aperture diameter (mm)')
      
      if (apmax .lt. realmax)
     & call hputr(lu,20,'apmax',apmax,'Max aperture (mm)')
      
      if (bmsplt(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'bmsplt',bmsplt,'Beamsplitter')
      
      if (dtectrA(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'dtectrA',dtectrA,'Detector A')
      
      if (gainA(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'gainA',gainA,'Channel A HV or gain')
      
      if (dtectrB(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'dtectrB',dtectrB,'Detector B')

      if (gainB(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'gainB',gainB,'Channel B HV or gain')
      
      if (gainBadj .lt. maxintgr)
     & call hputi(lu,20,'gainBadj',gainBadj,'Channel B gain adjust')
      
      if (gainApB .lt. maxintgr)
     & call hputi(lu,20,'gainApB',gainApB,'A+B gain')
      
      if (gainAmB .lt. maxintgr)
     & call hputi(lu,20,'gainAmB',gainAmB,'A-B gain')
      
      if (lfcutApB .lt. maxintgr)
     & call hputi(lu,20,'lfcutApB',lfcutApB,
     & 'Low frequency cutoff A+B (Hz)')
      
      if (hfcutApB .lt. maxintgr)
     & call hputi(lu,20,'hfcutApB',hfcutApB,
     & 'High frequency cutoff A+B (Hz)')
      
      if (lfcutAmB .lt. maxintgr)
     & call hputi(lu,20,'lfcutAmB',lfcutAmB,
     & 'Low frequency cutoff A-B (Hz)')
      
      if (hfcutAmB .lt. maxintgr)
     & call hputi(lu,20,'hfcutAmB',hfcutAmB,
     & 'High frequency cutoff A-B (Hz)')      
      
      write(lu,'(a)') '/'
      call hputc(lu, '        SCAN PARAMETERS')
      
      if (alias .lt. maxintgr)
     & call hputi(lu,20,'alias',alias,'Alias')
      
      if (fdr .lt. realmax)
     &  call hputd(lu,20,'fdr',fdr,'Fringe division ratio')
      
      if (nm .lt. maxintgr)
     & call hputi(lu,20,'nM',nm,'M (fringe rate = fzeeman/nM)')
      
      if (nk .lt. maxintgr)
     & call hputi(lu,20,'nK',nk,'K (sample freq. = fzeeman/nK)')
      
      if (ns .lt. maxintgr)
     & call hputi(lu,20,'nS',ns,'S (related to scan length)')
      
      if (motorvel .lt. realmax)
     & call hputr(lu,20,'motorvel',motorvel,'Stepper Velocity (rps)')
      
      if (refwavno .lt. realmax)
     & call hputd(lu,20,'refwavno',refwavno,'Laser wavenumber (cm-1)')
      
      if (fzeeman .lt. realmax)
     & call hputd(lu,20,'fzeeman',fzeeman,
     & 'Control laser Zeeman frequency')
      
      if (fclock .lt. realmax)
     & call hputr(lu,20,'fclock',fclock,'Timing clock frequency (Hz)')
      
      if (sampfreq .lt. realmax)
     & call hputr(lu,20,'sampfreq',sampfreq,'Sampling frequency (Hz)')
      
      if (decim .lt. realmax)
     & call hputr(lu,20,'decim',decim,'Decimation factor')
      
      if (kernlen .lt. maxintgr)
     & call hputi(lu,20,'kernlen',kernlen,'DF kernel length ')
      
      if (epsilon .lt. realmax)
     & call hputr(lu,20,'epsilon',epsilon,'Digital filter parameter')
      
      if (fracmask .lt. realmax)
     & call hputr(lu,20,'fracmask',fracmask,
     & 'Fractional width of cosine mask')
      
      if (fracbw .lt. realmax)
     & call hputr(lu,20,'fracbw',fracbw,
     & 'Filter bandwidth / alias')
      
      if (ctrav .lt. realmax)
     & call hputr(lu,20,'ctrav',ctrav,'Carriage travel (cm)')
      
      if (cvel .lt. realmax)
     & call hputr(lu,20,'cvel',cvel,'OPD velocity (cm/sec)')
      
      if (scan_is(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'scan_is',scan_is,'Scan Direction (F,R,Both)')
      
      if (dblpass(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'dblpass',dblpass,'Double Pass - Yes/No')
      
      if (ratsig .lt. realmax)
     & call hputr(lu,20,'ratsig',ratsig,'Total signal (A+B) (Volt)')
      
      if (ratrms .lt. realmax)
     & call hputr(lu,20,'ratrms',ratrms,'Total RMS (A+B) (Volt)')
      
      if (ratio_is(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'ratio_is',ratio_is,'Ratio - On / Off')
      
      if (glitchlo .lt. realmax)
     & call hputr(lu,20,'glitchlo',glitchlo,'Glitch window low (Volt)')
      
      if (glitchhi .lt. realmax)
     & call hputr(lu,20,'glitchhi',glitchhi,'Glitch window high (Volt)')
      
      if (gltchdev .lt. realmax)
     & call hputr(lu,20,'gltchdev',gltchdev,
     & 'Glitch window width (%f.s.)')
      
      if (hetpm1 .lt. maxintgr)
     & call hputi(lu,20,'hetpm1',hetpm1,
     & 'Heterodyne param #1 - H (polarizer)')
      
      if (hetpm2 .lt. maxintgr)
     & call hputi(lu,20,'hetpm2',hetpm2,
     & 'Heterodyne param #2 - Hphi (polarizer)')
      
      if (modeff .lt. realmax)
     & call hputr(lu,20,'modeff',modeff,'Modulation efficiency')
      
      if (srverr .lt. realmax)
     & call hputr(lu,20,'srverr',srverr,
     & 'FTS servo RMS error (millifringes)')
      
      if (pspect .lt. realmax)
     & call hputr(lu,20,'pspect',pspect,'FTS tank pressure (Torr)')
      
      if (tspect .lt. realmax)
     & call hputr(lu,20,'tspect',tspect,'FTS tank temperature (C)')
      
      if (hspect .lt. realmax)
     & call hputr(lu,20,'hspect',hspect,'FTS tank H2O pressure (Torr)')
      
      if (flin .lt. realmax)
     & call hputr(lu,20,'flin',flin,'Input focal length (mm)')
      
      if (flout .lt. realmax)
     & call hputr(lu,20,'flout',flout,'Output focal length (mm)')
      
      
      write(lu,'(a)') '/'
      call hputc(lu, '        TRANSFORM PARAMETERS')
      
      if (nint .lt. maxintgr) 
     & call hputi(lu,20,'nint',nint,'Number of pts in interferogram')
      
      if (inskip .lt. maxintgr)
     & call hputi(lu,20,'inskip',inskip,
     & 'Number of pts skipped on input')
      
      if (ncenter .lt. maxintgr)
     & call hputi(lu,20,'ncenter',ncenter,
     & 'Point number of central fringe')
      
      if (nused .lt. maxintgr)
     & call hputi(lu,20,'nused',nused,
     & 'Number of pts used in transform')
      
      if (ntrans .lt. maxintgr)
     & call hputi(lu,20,'ntrans',ntrans,'Transform size (2**N)')
      
      if (aliaslo .lt. realmax)
     & call hputd(lu,20,'aliaslo',aliaslo,
     & 'Alias low wavenumber  (cm-1)')
      
      if (aliashi .lt. realmax)
     & call hputd(lu,20,'aliashi',aliashi,
     & 'Alias high wavenumber (cm-1)')
      
      if (freespec .lt. realmax)
     & call hputd(lu,20,'freespec',freespec,
     & 'Free spectral range (cm-1)')
      
      if (outlo .lt. realmax)
     & call hputr(lu,20,'outlo',outlo,'Saved lower data bound')
      
      if (outhi .lt. realmax)
     & call hputr(lu,20,'outhi',outhi,'Saved upper data bound')
      
      if (bandlo .lt. realmax)
     & call hputr(lu,20,'bandlo',bandlo,'Useful lower data bound')
      
      if (bandhi .lt. realmax)
     & call hputr(lu,20,'bandhi',bandhi,'Useful upper data bound')
      
      if (aircorr(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'aircorr',aircorr,
     & 'Air correction flag - Yes/No')
      
      if (modeapod .lt. maxintgr)
     & call hputi(lu,20,'modeapod',modeapod,
     & 'Apodization function')
      
      if (apod1 .lt. realmax)
     & call hputr(lu,20,'apod1',apod1,'Apodization variable #1')
      
      if (apod2 .lt. realmax)
     & call hputr(lu,20,'apod2',apod2,'Apodization variable #2')
      
      if (apod3 .lt. realmax)
     & call hputr(lu,20,'apod3',apod3,'Apodization variable #3')
      
      if (apod4 .lt. realmax)
     & call hputr(lu,20,'apod4',apod4,'Apodization variable #4')
      
      if (apod5 .lt. realmax)
     & call hputr(lu,20,'apod5',apod5,'Apodization variable #5')
      
      if (modephzc .lt. maxintgr)
     & call hputi(lu,20,'modephzc',modephzc,'Phase corr. mode')
      
      if (pvar1 .lt. realmax)
     & call hputd(lu,20,'pvar1',pvar1,'Phase corr. variable #1')
      
      if (pvar2 .lt. realmax)
     & call hputd(lu,20,'pvar2',pvar2,'Phase corr. variable #2')
      
      if (pvar3 .lt. realmax)
     & call hputd(lu,20,'pvar3',pvar3,'Phase corr. variable #3')
      
      if (pvar4 .lt. realmax)
     & call hputd(lu,20,'pvar4',pvar4,'Phase corr. variable #4')
      
      if (pvar5 .lt. realmax)
     & call hputd(lu,20,'pvar5',pvar5,'Phase corr. variable #5')
      
      if (pvar6 .lt. realmax)
     & call hputd(lu,20,'pvar6',pvar6,'Phase corr. variable #6')
      
      if (pvar7 .lt. realmax)
     & call hputd(lu,20,'pvar7',pvar7,'Phase corr. variable #7')
      
      if (pvar8 .lt. realmax)
     & call hputd(lu,20,'pvar8',pvar8,'Phase corr. variable #8')
      
      if (pvar9 .lt. realmax)
     & call hputd(lu,20,'pvar9',pvar9,'Phase corr. variable #9')
      
      if (setlo1 .lt. maxintgr)
     & call hputi(lu,20,'setlo1',setlo1,'Low side of setregion #1')
      
      if (sethi1 .lt. maxintgr)
     & call hputi(lu,20,'sethi1',sethi1,'High side of setregion #1')
      
      if (setval1 .lt. realmax)
     & call hputr(lu,20,'setval1',setval1,'Value used in setregion #1')  
      
      if (setlo2 .lt. maxintgr)
     & call hputi(lu,20,'setlo2',setlo2,'Low side of setregion #2')
      
      if (sethi2 .lt. maxintgr)
     & call hputi(lu,20,'sethi2',sethi2,'High side of setregion #2')
      
      if (setval2 .lt. realmax)
     & call hputr(lu,20,'setval2',setval2,'Value used in setregion #2')  

      if (archname(1:13) .ne. unknown(1:13))
     & call hputs(lu,20,'archname',archname,'Archive file name') 
      
      if (origtype .lt. maxintgr)
     & call hputi(lu,20,'origtype',origtype,'Original Info block type') 
      
      
      write(lu,'(a)') '/'
      call hputc(lu, '        USER VARIABLES')
      
      if (uvar0 .lt. realmax) 
     & call hputd(lu,20,'uvar0',uvar0,'User variable #0')

      if (uvar1 .lt. realmax) 
     & call hputd(lu,20,'uvar1',uvar1,'User variable #1')

      if (uvar2 .lt. realmax) 
     & call hputd(lu,20,'uvar2',uvar2,'User variable #2')

      if (uvar3 .lt. realmax) 
     & call hputd(lu,20,'uvar3',uvar3,'User variable #3')

      if (uvar4 .lt. realmax) 
     & call hputd(lu,20,'uvar4',uvar4,'User variable #4')

      if (uvar5 .lt. realmax) 
     & call hputd(lu,20,'uvar5',uvar5,'User variable #5')

      if (uvar6 .lt. realmax) 
     & call hputd(lu,20,'uvar6',uvar6,'User variable #6')

      if (uvar7 .lt. realmax) 
     & call hputd(lu,20,'uvar7',uvar7,'User variable #7')

      if (uvar8 .lt. realmax) 
     & call hputd(lu,20,'uvar8',uvar8,'User variable #8')

      if (uvar9 .lt. realmax) 
     & call hputd(lu,20,'uvar9',uvar9,'User variable #9')

      
      if (ftsmode(1:3) .ne. 'Lab') THEN
      
      write(lu,'(a)') '/'
      write (lu,1009)
 1009 format('/',6x,'        TELESCOPE PARAMETERS')
      
      if (telescop .ne. unknown) write (lu,701) telescop
 701  format ('telescop= ''',a20, '''/ Telescope')                
      
      if (patm .lt. realmax) write (lu,702) patm
 702  format ('patm    = ',f9.1,13x,'/ Atmospheric pressure (Torr)')
      
      if (tatm .lt. realmax) write (lu,703) tatm
 703  format ('tatm    = ',f9.1,13x,'/ Atmospheric temperature (C)')
      
      if (hatm .lt. realmax) write (lu,704) hatm
 704  format ('hatm    = ',f9.1,13x,'/ Atmospheric H2O press. (Torr)')
      
      if (julday .lt. maxintgr) write (lu,705) julday
 705  format ('julday  = ',i8,14x,'/ Julian day')
      
      if (hastrt .lt. realmax) write (lu,706) hastrt
 706  format ('hastrt  = ',f9.1,13x,'/ Hour angle at start (seconds)')
      
      if (hastop .lt. realmax) write (lu,707) hastop
 707  format ('hastop  = ',f9.1,13x,'/ Hour angle at stop (seconds)')
      
      if (dcmean .lt. realmax) write (lu,708) dcmean
 708  format ('dcmean  = ',f9.5,13x,'/ Mean declination (rad)')
      
      if (zenstrt .lt. realmax) write (lu,709) zenstrt
 709  format ('zenstrt = ',f9.5,13x,'/ Zenith angle at start (rad)')
      
      if (zenstop .lt. realmax) write (lu,710) zenstop
 710  format ('zenstop = ',f9.5,13x,'/ Zenith angle at stop (rad)')
      
      if (amstrt .lt. realmax) write (lu,711) amstrt
 711  format ('amstrt  = ',f9.4,13x,'/ Relative air mass at start')
      
      if (amstop .lt. realmax) write (lu,712) amstop
 712  format ('amstop  = ',f9.4,13x,'/ Relative air mass at stop')
      
      if (vcstrt .lt. realmax) write (lu,713) vcstrt
 713  format ('vcstrt  = ',f9.6,13x,'/ Solar vel. corr. at start V/C')
      
      if (vcstop .lt. realmax) write (lu,714) vcstop
 714  format ('vcstop  = ',f9.6,13x,'/ Solar vel. corr. at stop V/C')
      
      if (sclim .lt. realmax) write (lu,715) sclim
 715  format ('sclim   = ',f9.3,13x,'/ Image scale (mm/arcsecond)')
      
      if (dimage .lt. realmax) write (lu,716) dimage
 716  format ('dimage  = ',f9.3,13x,'/ Image diameter (mm)')
      
      if (soffst .lt. realmax) write (lu,717) soffst
 717  format ('soffst  = ',f9.3,13x,'/ N-S offset (mm)')
      
      if (eoffst .lt. realmax) write (lu,718) eoffst
 718  format ('eoffst  = ',f9.3,13x,'/ E-W offset (mm)')
      
      if (solmu .lt. realmax) write (lu,719) solmu
 719  format ('solmu   = ',f9.3,13x,'/ Limb position (cos theta)')
      
      if (utstrt .ne. unknown) write (lu,720) utstrt
 720  format ('utstrt  = ''',a20, '''/ Universal time at start')
      
      if (utstop .ne. unknown) write (lu,721) utstop
 721  format ('utstop  = ''',a20, '''/ Universal time at stop')
      
      if (ststrt .ne. unknown) write (lu,722) ststrt
 722  format ('ststrt  = ''',a20, '''/ Sidereal time at start')
      
      if (ststop .ne. unknown) write (lu,723) ststop
 723  format ('ststop  = ''',a20, '''/ Sidereal time at stop')
      
      if (rareq  .ne. unknown) write (lu,724) rareq 
 724  format ('rareq   = ''',a20, '''/ Right ascension requested')
      
      if (decreq .ne. unknown) write (lu,725) decreq
 725  format ('decreq  = ''',a20, '''/ Declination requested')
      
      if (raobs .ne. unknown) write (lu,726) raobs
 726  format ('raobs   = ''',a20, '''/ Right ascension observed')
      
      if (decobs .ne. unknown) write (lu,727) decobs
 727  format ('decobs  = ''',a20, '''/ Declination observed')
      
      if (apsep .lt. realmax) write (lu,728) apsep
 728  format ('apsep   = ',f9.3,13x,'/ Aperture separation (arcsec)')     
      
      if (apang .lt. realmax) write (lu,729) apang
 729  format ('apang   = ',f9.3,13x,'/ Aperture angle (degrees)')
      
      ENDIF 

      
      write(lu,'(a)') '/'
      call hputc(lu, '        USER COMMENTS FOR ARCHIVE')
      
      if (comment0(1:13) .ne. unknown(1:13))
     & call hputs(lu,68,'comment0',comment0,' ')

      if (comment1(1:13) .ne. unknown(1:13))
     & call hputs(lu,68,'comment1',comment1,' ')

      if (comment2(1:13) .ne. unknown(1:13))
     & call hputs(lu,68,'comment2',comment2,' ')

      if (comment3(1:13) .ne. unknown(1:13))
     & call hputs(lu,68,'comment3',comment3,' ')

      if (comment4(1:13) .ne. unknown(1:13))
     & call hputs(lu,68,'comment4',comment4,' ')

      if (comment5(1:13) .ne. unknown(1:13))
     & call hputs(lu,68,'comment5',comment5,' ')

      if (comment6(1:13) .ne. unknown(1:13))
     & call hputs(lu,68,'comment6',comment6,' ')

      if (comment7(1:13) .ne. unknown(1:13))
     & call hputs(lu,68,'comment7',comment7,' ')

      if (comment8(1:13) .ne. unknown(1:13))
     & call hputs(lu,68,'comment8',comment8,' ')

      if (comment9(1:13) .ne. unknown(1:13))
     & call hputs(lu,68,'comment9',comment9,' ')
      
      write (lu,'(3hEND)')
      
      return
      end

c------------------------------------------------------------------------

c read b6 header

	subroutine b6infrd(lu)

        include 'iounit.h'
	include 'infmtn.h'
	include 'altinf.h'
	include 'extinf.h'
        include 'solinf.h'
        include 'set.h'
        include 'headers.h'

        integer lu,ierr


* read the header and set up key/value tables in memory
        call rdheader(lu, ierr)
        if (ierr .eq. -1) then
           inftype = 0
           return
        end if
        if (ierr .gt. 0) then
           write(wrtbuf, '(a,a,a)') ' Error when reading keyword ',
     &           hvalue(ierr),' of NSO 6 header file.'
           inftype = 6
           return
        end if

* now read the header values from the table one by one
        call hgets('acqvers',acqvers)
        call hgets('aircorr',aircorr)
        call hgeti('alias',alias)
        call hgetd('aliashi',aliashi)
        call hgetd('aliaslo',aliaslo)
        call hgetr('amstop',amstop)
        call hgetr('amstrt',amstrt)
        call hgetr('apang',apang)
        call hgetr('apin',apin)
        call hgetr('apmax',apmax)
        call hgetr('apod1',apod1)
        call hgetr('apod2',apod2)
        call hgetr('apod3',apod3)
        call hgetr('apod4',apod4)
        call hgetr('apod5',apod5)
        call hgets('archname',archname)

        call hgetr('bandhi',bandhi)
        call hgetr('bandlo',bandlo)
        call hgets('bmsplt',bmsplt)
        call hgeti('bocode',bocode)

        call hgets('comment0',comment0)
        call hgets('comment1',comment1)
        call hgets('comment2',comment2)
        call hgets('comment3',comment3)
        call hgets('comment4',comment4)
        call hgets('comment5',comment5)
        call hgets('comment6',comment6)
        call hgets('comment7',comment7)
        call hgets('comment8',comment8)
        call hgets('comment9',comment9)
        call hgetr('ctrav',ctrav)
        call hgetr('cvel',cvel)

        call hgets('data_is',data_is)
        call hgets('day',day)
        call hgets('data_is',data_is)
        call hgetd('dcmean',dcmean)
        call hgetr('decim',decim)
        call hgets('decobs',decobs)
        call hgets('decreq',decreq)
        call hgetd('delw',delw)
        call hgetr('dimage',dimage)
        call hgetr('drivevel',drivevel)
        call hgets('dtectra',dtectra)
        call hgets('dtectrb',dtectrb)
        
        call hgetd('eltim',eltim)
        call hgetr('endgauss',endgauss)
        call hgetr('epsilon',epsilon)
        call hgetr('eoffst',eoffst)

        call hgeti('fboffs',fboffs)
        call hgetr('fclock',fclock)
        call hgets('filter1',filter1)
        call hgets('filter2',filter2)
        call hgets('filter_i',filter_i) ! for Alice > 1.0 
        call hgets('filter_a',filter_a)
        call hgets('filter_b',filter_b)
        call hgetr('flin',flin)
        call hgetr('flout',flout)
        call hgetr('fracbw',fracbw)
        call hgetr('fracmask',fracmask)
        call hgetd('freespec',freespec)
        call hgets('ftsmode',ftsmode)
        call hgetd('fzeeman',fzeeman)
        call hgetd('fdr',fdr)           ! files from Alice 0.x use 'fdr'

        call hgets('gaina',gainA)
        call hgets('gain_a',gainA)      ! for Alice > 1.0
        call hgeti('gainamb',gainAmB)
        call hgeti('gainapb',gainApB)
        call hgets('gainb',gainB)
        call hgets('gain_b',gainB)      ! for Alice > 1.0
        call hgeti('gainbadj',gainBadj)
        call hgetr('glitchhi',glitchhi)
        call hgetr('glitchlo',glitchlo)
        call hgetr('gltchdev',gltchdev)

        call hgetd('hastop',hastop)
        call hgetd('hastrt',hastrt)
        call hgetr('hatm',hatm)
        call hgeti('hetpm1',hetpm1)
        call hgeti('hetpm2',hetpm2)
        call hgeti('hfcutamb',hfcutAmB)
        call hgeti('hfcutapb',hfcutApB)
        call hgetr('hspect',hspect)

        call hgets('id',id)
        call hgeti('inftype',inftype)
        call hgets('input_is',input_is)
        call hgeti('inskip',inskip)

        call hgeti('julday',julday)

        call hgeti('kernlen',kernlen)

        call hgeti('lfcutamb',lfcutAmB)
        call hgeti('lfcutapb',lfcutApB)

        call hgeti('modeapod',modeapod)
        call hgetr('modeff',modeff)
        call hgeti('modephzc',modephzc)
        call hgetr('motorvel',motorvel)

        call hgeti('ncenter',ncenter)
        call hgeti('nint',nint)
        call hgeti('nk',nk)
        call hgeti('nm',nm)
        call hgetr('noiselev',noiselev)
        call hgeti('npo',npo)
        call hgeti('ns',ns)
        call hgeti('nscan',nscan)
        call hgeti('ntrans',ntrans)
        call hgeti('nused',nused)

        call hgets('origin',origin)
        call hgets('observer',observ)
        call hgeti('origtype',origtype)
        call hgetr('outhi',outhi)
        call hgetr('outlo',outlo)

        call hgetr('patm',patm)
        call hgets('predisp',predisp)
        call hgetr('pspect',pspect)
        call hgetd('pvar1',pvar1)
        call hgetd('pvar2',pvar2)
        call hgetd('pvar3',pvar3)
        call hgetd('pvar4',pvar4)
        call hgetd('pvar5',pvar5)
        call hgetd('pvar6',pvar6)
        call hgetd('pvar7',pvar7)
        call hgetd('pvar8',pvar8)
        call hgetd('pvar9',pvar9)

        call hgets('raobs',raobs)
        call hgets('rareq',rareq)
        call hgets('ratio_is',ratio_is)
        call hgetr('ratrms',ratrms)
        call hgetr('ratsig',ratsig)
        call hgetr('rdsclfct',rdsclfct)
        call hgetd('refwavno',refwavno)
        call hgetr('resolutn',resolutn)

        call hgetr('sampfreq',sampfreq)
        call hgets('sample',sample)
        call hgets('sample_i',sample_i)  ! for Alice > 1.0
        call hgets('sample_a',sample_a)
        call hgets('sample_b',sample_b)
        
        call hgets('scan_is',scan_is)
        call hgetr('sclim',sclim)
        call hgeti('s_delay',s_delay)
        call hgeti('serialno',serialno)
        call hgeti('sethi1',sethi1)
        call hgeti('sethi2',sethi2)
        call hgeti('setlo1',setlo1)
        call hgeti('setlo2',setlo2)
        call hgetr('setval1',setval1)
        call hgetr('setval2',setval2)
        call hgetr('soffst',soffst)
        call hgetr('solmu',solmu)
        call hgets('source',source)
        call hgets('spect',spect)        ! FTS instrument
        call hgets('spectype',spectype)

        call hgetr('tatm',tatm)
        call hgets('telescop',telescop)
        call hgets('timestr',timestr)
        call hgetr('tspect',tspect)

        call hgets('user',user)
        call hgets('utstop',utstop)
        call hgets('utstrt',utstrt)
        call hgetd('uvar0',uvar0)
        call hgetd('uvar1',uvar1)
        call hgetd('uvar2',uvar2)
        call hgetd('uvar3',uvar3)
        call hgetd('uvar4',uvar4)
        call hgetd('uvar5',uvar5)
        call hgetd('uvar6',uvar6)
        call hgetd('uvar7',uvar7)
        call hgetd('uvar8',uvar8)
        call hgetd('uvar9',uvar9)

        call hgetr('vcstop',vcstop)
        call hgetr('vcstrt',vcstrt)

        call hgetd('wavcorr',wavcorr)
        call hgetd('wstart',wstart)
        call hgetd('wstop',wstop)

        call hgets('xaxis_is',xaxis_is)

        call hgetd('zenstop',zenstop)
        call hgetd('zenstrt',zenstrt)


c all done - fix up some variables and quit
        if ( fdr .lt. realmax )  then   ! NIST FTSs use fdr instead of M/K
           fdivr = fdr
           ctrav = real(npo) / (fdr * refwavno)
        else
           freespec = (0.5d0*refwavno*nm)/nk
           fdivr = float(nm)/float(nk)
           ctrav = float(nk)*float(ns)/refwavno
           cvel  = fzeeman/(refwavno*nm)
        end if
	aliashi = alias * freespec
	aliaslo = aliashi - freespec

	read (timestr,11) ith,itm,tmptim
11	format (i2,1x,i2,1x,f5.2)
	strtim = tmptim + 60.0d0*(itm + 60.0d0*ith)
	stptim = strtim + eltim
c save delw, nwpp for future reads
	nwpp = 1
	if (data_is .eq. 'Complex' .or.
     &      data_is .eq. 'complex') nwpp = 2
        if (data_is .eq. 'Real' .or.
     &      data_is .eq. 'real')    nwpp = 1
        if (xaxis_is .eq. 'Wavelength') nwos = 1
        if (xaxis_is .eq. 'Wavenumber') nwos =-1 
        if (spectype .eq. 'Emission')   neoa = 1
        if (spectype .eq. 'Absorption') neoa =-1 
        if (input_is .eq. 'A only')   ninput = 1
        if (input_is .eq. 'Both A&B') ninput = 2 
        if (ratio_is.eq. 'Off') iratio = 0
        if (ratio_is.eq. 'On')  iratio = 1 
        if (aircorr .eq. 'No')  icrflg = 0 
        if (aircorr .eq. 'Yes') icrflg = 1
        if (scan_is .eq. 'Forward') scandir = 1

	disp = delw
	nwppt = nwpp
	inftype = 6

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
           write(wrtbuf,'(a,e18.6)') 
     &           ' Wavenumber correction factor : ', wavcorr
           call wrtstr(wrtbuf)
        else
           call wrtstr(' No wavenumber correction.')
        end if

	return
        end
