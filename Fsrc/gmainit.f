*
* initialization of Xgremlin
*
	subroutine mainit

c Initialization subroutine

	include 'datetc.h'
	include 'iounit.h'
	include 'altinf.h'
	include 'infmtn.h'
	include 'inparms.h'
	include 'outparms.h'
	include 'linparms.h'
	include 'plcommon.h'
	include 'set.h'
	include 'plot.h'
	include 'phase.h'
	include 'integrate.h'

	character*254 string,logpath
	character*254 sysfilnam,usrfilnam,pathname,scrpath
	logical lex
	integer getenv
	integer l, n
	real uni
	real dummy(1)               ! used for 'gopen'

	integer ihour, minu, isec, ihsec, iseed

	external getenv
	external getpid

* initial array sizes have to be set in common block
	ffta_size  = 262144
	nfftamax   = ffta_size
	rtr_size   = 262148
	r_size     = 131074
	tr_size    = 131074
	phz_size   = 65536
	nphzmax    = phz_size
	n2rtr      = 17          ! initial length of r as power of 2
	nrtr       = r_size - 2  
	linbuf_size= 100
	nlins      = 100
	nrefp      = 0

	blksiz   = 8192
	lenout   = 4096
	imagfl   = .false.
	realfl   = .false.
	cmplex   = .false.
	phcorfl  = .true.
	apodfl   = .false.
	onesidfl = .false.
	nomore   = .false.
	phapdz   = .false.
	nphap    = 0
	incntr   = -1
	ixtend   = 1
	irng1    = 10000000
	irng2    = 10000000
	xsigma   = 10.0
	nseq     = 0
	iprnt    = 1
	xcentr   = -1.
	inskip   = 0
	wstart   = 0.
	wstop    = 1.e38
	pref     = 1.0d0
	refwavno = 15798.0025d0        ! wavenumber of red He/Ne laser
	nphz     = 32768
	nlores   = 8192
	wdlim    = 1000.0              ! Voigt cutoff in addlin
	rdscale  = 2                   ! scaling from header file is default
	rdnorm   = .false.             ! no data normalizing
	rdsclfct = 1.0                 ! read scale factor
	margin   = 0                   ! no top/bottom margin initially
	lecho    = .false.             ! no echoing by default
	lvacuum  = .false.             ! air wavelengths by default
	l_bad    = .false.             ! no bad points
	lfirst   = .true.              ! data in 'r' array are new
	deglch   = .true.              ! deglitch data by default
	lglitch  = .true.              ! glitch check on by default

* initialize defaults for spline phase fits (see fitpack.f)
	stol = 1.0                     ! tolerance
	sten = 0.0                     ! tension factor
	smof = 0.0                     ! smoothing factor (Changed from 1.0, GN Feb 2015)
        swid = 6.0                     ! Min. distance between phase pts (GN Feb 2015)

* initialize default start and dispersion
	npt   = 1
	nop   = 1024
	delw  = 1.d0
	wref  = 1.d0
	nscan = 1
	neoa  = 1
	nwpp  = 1
	nwos  = -1
	id    = ' internally generated record'
	sc    = 0.0

* initialize some strings
	do i=1,30
	   fnames(i) = ' '
	end do
	cstag = '    '

* initialize some line list stuff
	lsqcnt = 1
	lsqlo  = 1
	lsqhi  = -1
	lsqref = 0.0
	lsqtol = 1.e-6
	svdtol = 1.e-5   ! tolerance for singular value editing
	damp   = 0.1
	xlast  = 26.0
	insflg = 0

* initialize NSO type 6 variables
	call b6init

* compile regular expressions for command decoder
	call rexcmp

* store endianness of machine (machine byte order code) in common block
	call gmboc( mbocode )

* store process id in common block
	call getpid( ipid )

* store the display type: 0 = colour; 1 = mono
	call dsptype( monodsp )

* initialize keyboard command lock
	call unlkbd

* initialize line integration module
	ptype = 0        ! profile type 'none'
	rvar = 0.0
	lhold = .false.  ! hold no variables

* initialize the random number generator with system time in seconds
	call gettim(ihour, minu, isec, ihsec)
	iseed = isec + 60 * minu + 3600 * ihour
	dummy(1) = uni( iseed )

*--- the X11 version is initialized by the applications default database

* the system wide configuration file Xgremlin.conf is either in /usr/local/lib
* or in the path defined in the XGREMLIN_CONF_PATH environment variable
        pathname = 'XGREMLIN_CONF_PATH'           ! check if it is set
        n = getenv( pathname, string)             ! read environment variable
        if (n .le. 0) then                        ! if not use default
	   string = '/usr/local/xgremlin/'
	   n = length( string )
        endif
	if (string(n:n) .ne. '/') then
	   n = n+1
	   string(n:n) = '/'
	endif
        sysfilnam = string(1:n)//'xgremlin.conf'  ! system wide config name
	inquire(file=sysfilnam,exist=lex)
	if (.not.lex) then
	   write(*,*) 'Fatal Error :  file  xgremlin.conf  not found.'
	   stop
	end if

* user's own configuration file; make one if it does not exist
	call homedir( string )                    ! get users home directory
	n = length( string )
	usrfilnam = string(1:n)//'/.xgremlinrc'   ! user's own config file
	inquire(file=usrfilnam,exist=lex)         
	if (.not.lex) then                        ! create one
	   open(99,file=usrfilnam,form='formatted',status='new')
	   write(99,'(a)') '# local Xgremlin initialization file'
	   write(99,'(a)') 'set noecho'
	   write(99,'(a)') 'break'
	   close( 99 )
	end if

* print an opening banner
	call page(1)                              ! print a banner

* open it in users home directory or where XGREMLIN_LOG_PATH points to 
        pathname = 'XGREMLIN_LOG_PATH'            ! check if it is set
        n = getenv( pathname, logpath)            ! read environment variable
        if (n .le. 0) then                        ! if not use default
	   call homedir( logpath )
	end if
	n = length( logpath )
	if (logpath(n:n) .ne. '/') then
	   n = n+1
	   logpath(n:n) = '/'
	endif
	call maknam( string )   
	l = length( string )
	alpha(1) = 'logfile'
	nalpha(1) = 7
	alpha(2) = logpath(1:n)//string(1:l)
	nalpha(2) = n + l
	nstrng = 2
	id = ' '
	call gopen(dummy)
	call page(2)

* open the scratch file if it is not locked by another Xgremlin
	pathname = 'XGREMLIN_SCRATCH_PATH'
	n = getenv( pathname, scrpath ) ! check if this is set
	alpha(1) = 'scratch'
	nalpha(1) = 7
	alpha(2) = scrpath
	nalpha(2) = length(scrpath)
	call gopen(dummy)
	   
* open system wide config file for reading
   	open (cardin,file=sysfilnam, status='old')
	uopen(cardin)  = 1
	fnames(cardin) = sysfilnam
	ipara = 0                      ! always read config file

* set up a default set of user configurable buttons
* (NOTE: the spaces are needed for later zero termination of strings
	call butedit( 1, 'plot all  ', 'plot all  ') 
	call butplot( 2, 'M+c  ',      'connect  ' )
	call butplot( 3, 'C+a  ',      'add lines  ' )
	call butplot( 4, 'C+d  ',      'del lin/php  ' )
	call butplot( 5, 'C+t  ',      'toggle actv.  ' )
	call butplot( 6, 'C+g  ',      'add c.grav.  ' )
	call butplot( 7, 'm  ',        'line info  ' )
	call butedit( 8, 'putlines  ', 'putlines  ' )

* now execute the initialization files
	n = length(sysfilnam)
	string = 'run '//sysfilnam(1:n)
	call runcmd( string )        ! pass it to the function dispatcher

	n = length(usrfilnam)
	string = 'run '//usrfilnam(1:n)
	call runcmd( string )

	return
	end




