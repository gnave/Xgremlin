c       gremlin\src\gopen.for   Ver. 1.0

*  record length for line list files reduced to 80
c  Revision 1.12  10.11.93  Remove mean from even orders in rawin
c  Revision 1.11  15.10.93  Datain opened as direct binary, recl=4096 
c  Revision 1.10  05/09/93  Open datain sets default read to npo
c  Revision 1.09  08.05.93  Altdata added
c  Revision 1.08  16.03.93  Infile added
c  Revision 1.07  15.03.93  Datain, dataot now transparent
c  Revision 1.06  29.08.91  New scratch length
c  Revision 1.05  19.08.91  Punch deleted from ionum
c  Revision 1.04  16.08.91  Even orders reversed when opened
c  Revision 1.03  04.06.91  rawin may be either .int or .dat file
c  Revision 1.02  26.05.91  page initialized when logfil opened
c  Revision 1.01  06.04.91  header and data read when rawin opened
c  Version  1.00  05.04.91  rawin,scratch now unformatted, recl= whole

	subroutine gopen(ffta)

	include 'iounit.h'
	include 'datetc.h'
	include 'infmtn.h'
	include 'altinf.h'
	include 'inparms.h'
	include 'linparms.h'
	include 'set.h'

	real ffta(*)

	logical lex, lcreat
	character filnam*80, string*20
	integer ik
	integer ierr
	double precision rmn

* decode arguments for the open call; check syntax
	lcreat = .false.
	ipara = -1
	if (nstrng .lt. 2) go to 500

* map name into number
	string = alpha(1)(1:nalpha(1))
	call lcase(string,nalpha(1))
	iparb = ionum(string,nalpha(1))

* check for unknown name
	if (iparb .eq. -1) go to 520
* error if unit (other than logfile) is already open.
	if (uopen(iparb) .eq. 1 .and. iparb .ne. logfil) go to 540
	if (nalpha(2) .gt. filchr) go to 900

	filnam = ' '
100	filnam = alpha(2)(1:nalpha(2))

* check if line list file should be created
	if (iparb.eq.lineio .and. nstrng.eq.3 
     &      .and. alpha(3).eq.'new' ) then
	    lcreat = .true.
	 end if

c Formatted, Unknown		cardout 

	if (iparb .eq. cardout) then
	    open (unit=iparb,file=filnam,access='sequential',
     &		form='formatted',status='unknown',err=600,iostat=ierr)
	    go to 400
	endif

c Formatted, New		PRINTED OUTPUT SPOOLING or LOGFILE

	if (iparb .eq. spool)
     &	    open (unit=iparb,file=filnam,access='sequential',
     &	        form='formatted',status='unknown',err=600,iostat=ierr)

	if (iparb .eq. logfil) then
	    if (uopen(logfil) .eq. 1) 
     &		close(logfil,status= 'delete',iostat=i)
	    
     	    open (unit=iparb,file=filnam,access='sequential',
     &		form='formatted',status='unknown',err=600,iostat=ierr)
	    call page(2)
	endif

c Formatted, Old		CARD INPUT or INFILE

	if (iparb .eq. cardin .or. iparb .eq. infile)
     &	    open (unit=iparb,file=filnam,access='sequential',
     &		form='formatted',status='old',err=600,iostat=ierr)

c Direct access, read only	-  DATAIN or ALTDATA

	ik = nalpha(2) + 1
	IF (iparb .eq. datain  .or.  iparb .eq. altdat) then
	   filnam(ik:ik+3) = '.hdr'
	   open (unit=iparb,file=filnam,access='sequential',
     &		 form='formatted',status='old',err=600,iostat=ierr)

c Always read info for datain; read for altdata only if datain not open
c For interferograms (open unit filename "int") reset variables 
c Set default readlength to npo

	   if (uopen(datain) .ne. 1) then
	      call infread(iparb)
	      if ((nstrng .eq. 3) .AND. 
     &	          (alpha(3)(1:3) .eq. 'int')) then
		 call wrtstr(' Opening interferogram')
		 wref = 1.d0
		 wstart = 1.d0
		 wstop = float(nint)
		 npo = nint
		 delw = 1.d0
		 disp = 1.d0
		 nwpp = 1
		 nwppt = 1
	      endif
	      write (wrtbuf,'(1x,a72)') id
	      call wrtstr( wrtbuf )
	      write (wrtbuf,251) npo, wstart, wstop, delw
	      call wrtstr(wrtbuf)
 251	      format (i8,' points, from',f12.3,' to',f12.3,
     &                ', delw=',f12.9)
	   endif

c open interferogram or spectrum
	    if ((nstrng .eq. 3) .AND. (alpha(3)(1:) .eq. 'int')) then
		filnam(ik:ik+3) = '.int'
      		open (unit=iparb+1,file=filnam,
     &            access='direct',recl=4096,
     &		  form='unformatted',status='old',
     &            err=260,iostat=ierr)
		call wrtstr(' Interferogram opened')
		go to 280
260		filnam(ik:ik+3) = '.dat'
      		open (unit=iparb+1,file=filnam,
     &            access='direct',recl=4096,
     &		  form='unformatted',status='old',
     &		  err=600,iostat=ierr)
		call wrtstr(' Interferogram opened')
	    else
		filnam(ik:ik+3) = '.dat'
      		open (unit=iparb+1,file=filnam,
     &            access='direct',recl=4096,
     &		  form='unformatted',status='old',
     &            err=270,iostat=ierr)
		call wrtstr(' Spectrum file opened')
		go to 280
270		filnam(ik:ik+3) = '.spe'
      		open (unit=iparb+1,file=filnam,
     &            access='direct',recl=4096,
     &		  form='unformatted',status='old',
     &            err=600,iostat=ierr)
		call wrtstr(' Spectrum file opened')
	    endif
280	    continue

* also line list file
	    filnam(ik:ik+3) = '.lin'
	    linbak = .false.     ! default
      	    open (unit=lineio,file=filnam,access='direct',recl=80,
     &		     form='unformatted',status='old',iostat=ierr)
	    if (ierr .ne. 0) then
	       call wrtstr(' No line file found')
	       uopen(lineio) = 0
	    else
	       call wrtstr(' Line file opened')
	       uopen(lineio) = 1
	       fnames(lineio) = filnam
	       linbak = .true.    ! create a backup upon first write
	    endif
	    rdlen = npo
	    if (rdlen .gt. 2*nrtr) rdlen = 2*nrtr
	END IF

* line list file only (create a .lin file if none exists)
	if ( iparb .eq. lineio ) then  ! LINE LIST
	    filnam(ik:ik+3) = '.lin'
	    inquire(file=filnam,exist=lex)
	    if (lex) then
	       open (unit=lineio,file=filnam,access='direct',recl=80,
     &  	     form='unformatted',status='old',iostat=ierr)
	       if (ierr .ne. 0) then
		  call wrtstr(' Error :  could not open .lin file')
		  uopen(lineio) = 0
		  return	  ! bail out
	       else
		  call wrtstr(' Line file opened')
		  uopen(lineio) = 1
		  fnames(lineio) = filnam
	       end if
	       linbak = .true.	  ! create a backup upon first write
	    else
	       if ( lcreat ) then ! only create one if none exists
		  open (unit=lineio,file=filnam,access='direct',recl=80,
     &                  form='unformatted',status='new',iostat=ierr)
		  call linhdr(0, ierr, 0.0, 0.0, 0.0)
		  nlgot = 0
		  nbfirst = 320
		  ntop = 320
		  if ( ierr .ne. 0 ) then
		     call wrtstr(' Error :  .lin file header failed')
		     close(lineio)
		  else
		     call wrtstr(' Line file created')
		     uopen(lineio) = 1
		     fnames(lineio) = filnam
		     linbak = .false. ! no backup for new file
		  end if
	       else
		  call wrtstr(' Error :  line list file does not exist')
		  return
	       end if
	    end if
	end if

c Transparent Access, New               DATAOUT binary data
	if (iparb .eq. dataot) then
	    filnam(ik:ik+3) = '.hdr'
      	    open (unit=iparb,file=filnam,access='sequential',
     &		form='formatted',status='old',err=600,iostat=ierr)

	    filnam(ik:ik+3) = '.dat'
      	    open (unit=iparb+1,file=filnam,access='direct',recl=4096,
     &		form='unformatted',status='old',err=600,iostat=ierr)

	    filnam(ik:ik+3) = '.lin'
      	    open (unit=lineio,file=filnam,access='direct',recl=80,
     &	         form='unformatted',status='old',iostat=ierr)
	    if (ierr .ne. 0) call wrtstr(' No line file found')
	    call wrtstr(' Header and data opened')
	end if

c Direct Access			                SCRATCH save, recall, etc.

	if (iparb .eq. scrtch) then
	   call opensdir( filnam )
	   ik = length(filnam)+1
	end if

c Transparent Access, old			RAWIN

	if (iparb .eq. rawin) then
c for IC style interferograms, read header, reset variables, save in 18
	   filnam(ik:ik+3) = '.hdr'
	   open (unit=iparb,file=filnam,access='sequential',
     &           form='formatted',status='old',err=600,iostat=ierr)
	   call infread(iparb)
	   wref = 1.d0
	   wstart = 1.d0
	   wstop = float(nint)
	   npo = nint
	   delw = 1.d0
	   disp = 1.d0
	   nwpp = 1
	   nwppt = 1
	   aparam(1) = 0.0
	   invflg = 0		! invert flag
	   nstrng = 0

* make sure ffta array is large enough
	   if ( npo .gt. ffta_size ) then
	      call wrtstr(' Error :  fft array too small for data')
	      write(wrtbuf,299) ffta_size
	      call wrtstr(wrtbuf)
	      write(wrtbuf,2991) npo
	      call wrtstr(wrtbuf)
	      call wrtstr(' open rawin aborted - increase fft array.' )
	      close(unit=rawin)
	      uopen(rawin) = 0
	      ipara = -1
	      keybd = .true.	! stop processing batch file
	      input = stdin	! and return to prompt
	      call newprom
	      return
	   end if
 299	   format(' FFT array size        : ',i8)
 2991	   format(' Number of data points : ',i8)
	   call oscrwrite(18)

c  open interferogram; accept .int or .dat files
	    filnam(ik:ik+3) = '.int'
      	    open (unit=iparb+1,file=filnam,
     &          access='direct',recl=4096,
     &		form='unformatted',status='old',err=300,iostat=ierr)
	    write(wrtbuf,*) ' Opening and reading .int file, ',
     &                       npo,' points'
	    call wrtstr(wrtbuf)
	    go to 350

300	    filnam(ik:ik+3) = '.dat'
      	    open (unit=iparb+1,file=filnam,
     &          access='direct',recl=4096,
     &		form='unformatted',status='old',err=600,iostat=ierr)
	    write(wrtbuf,*) ' Opening and reading .dat file, ',
     &			npo,' points'
	    call wrtstr(wrtbuf)
c do one large transfer 
350	    continue
	    nbstart = 0

	    if (ierr .ne. 0) then
		write(wrtbuf,*) ' Error ',ierr,
     &                ' detected opening rawin'
		call wrtstr(wrtbuf)
		return
	    end if

	    call readata(iparb,nbstart,ffta(1),4*npo,nerr)

* if necessary, change the byte order.  
	    if ( bocode .ne. mbocode ) then
	       call chibord( ffta, npo )
	    end if

c reverse spectra of even orders by multiplying int. by 1, -1, 1, -1....
c AFTER removing mean
	    if (alias .lt. 1) alias = 1
	    if (mod(alias,2) .eq. 0) then
	       rmn = 0.
	       do i=1,npo
		  rmn = rmn + ffta(i)
	       end do
	       rmn = rmn/npo
	       do i=1,npo
		  ffta(i) = ffta(i) - rmn
	       end do
	       do i=2,npo,2
		  ffta(i) = -ffta(i)
	       end do
	    endif 

	end if

400	uopen(iparb) = 1
	if ( iparb .ne. lineio ) filnam(ik:ik+3) = '    '
	fnames(iparb) = filnam
C 	if (iparb.eq.cardin .or. iparb.eq.lineio) 
 	if (iparb.eq.cardin ) 
     &      rewind iparb

c set return code of 0 for success
	ipara = 0
	return

500	call wrtstr(' ERROR -- syntax incorrect.')
	return

520	write (wrtbuf,530) alpha(1)(1:nalpha(1))
	call wrtout(output,wrtbuf)
530	format(' ERROR -- unknown unit name  ',a)
	return

540	write (wrtbuf,550) alpha(1)(1:nalpha(1))
	call wrtout(output,wrtbuf)
550	format(' WARNING: -- unit is already open  ',a)
	return

 600	write (wrtbuf,610) filnam
	call wrtout(output,wrtbuf)
610	format(' Error :  failed to open ',a)
	return

900	write (wrtbuf,910) filchr
	call wrtout(output,wrtbuf)
910	format (' file name contains too many characters...max of',i3)
	return

	end

c*************************************************************************

	subroutine dclose

	include 'iounit.h'
	include 'infmtn.h'
	include 'datetc.h'
	include 'linparms.h'


	character string*20
	integer i

	ipara = -1

c					check syntax
	if (nstrng .lt. 1) go to 500
c					map unit name into number
	string = alpha(1)(1:nalpha(1))
	call lcase (string,nalpha(1))
	iparb = ionum(string,nalpha(1))
c					error if unknown name
	if (iparb .eq. -1) go to 520
c					error if the unit is not open.
	if (uopen(iparb) .ne. 1) go to 560
c					default to keeping files.

	i = 0
	if (iparb .eq. cardout) close(iparb,status= 'keep',iostat=i)
	if (iparb .eq.  spool) close(iparb,status= 'keep',iostat=i)
	if (iparb .eq. logfil) close(iparb,status= 'keep',iostat=i)
	if (iparb .eq. cardin) close(iparb,status= 'keep',iostat=i)
	if (iparb .eq. infile) close(iparb,status= 'keep',iostat=i)
	if (iparb .eq.  rawin) then
	   close(iparb,status= 'keep',iostat=i)
	   close(iparb+1,status= 'keep',iostat=i)
	end if
*	if (iparb .eq. scrtch) then do nothing, scratch is always open
	if (iparb .eq. datain) then
	   close(iparb,status= 'keep',iostat=i)
	   close(iparb+1,status= 'keep',iostat=i)
	   nrefp = 0      ! reset absolute reference point
	   if ( uopen(lineio) .eq. 1 ) then
	      close(lineio,status= 'keep',iostat=i)
	      nol = 0
	      call linevis(0)
	      call llerase
	   end if
	end if
	if (iparb .eq. lineio) then
	   close(iparb,status= 'keep',iostat=i)	   
	      nol = 0
	      call linevis(0)
	      call llerase
	end if
	if (iparb .eq. dataot) then
	   close(iparb,status= 'keep',iostat=i)
	   close(iparb+1,status= 'keep',iostat=i)
	end if
	if (i .ne. 0) go to 800

* reset flags
	uopen(iparb) = 0
	if ( iparb .eq. datain ) then
	   uopen(iparb+1) = 0
	   uopen(iparb+2) = 0    ! line io
	end if
	if ( iparb.eq.dataot .or. iparb.eq.rawin ) then
	   uopen(iparb+1) = 0
	end if
	ipara = 0
	return

500	call wrtout(output,' Error :  syntax incorrect.')
	return
520	write (wrtbuf,530) alpha(1)(1:nalpha(1))
	call wrtout(output,wrtbuf)
530	format(' Error :  never seen THAT unit name: ',a)
	return
560	write (wrtbuf,570) alpha(1)(1:nalpha(1))
	call wrtout(output,wrtbuf)
570	format(' Warning :  unit is not open  ',a)
	return
800	write (wrtbuf,810)  i
	call wrtout(output,wrtbuf)
810	format(' Error :  failed to close file.',
     &         ' Error number (iostat) =',i4)
	return
	end

c***************************************************************************

	subroutine drewnd

	include 'iounit.h'
	include 'datetc.h'

	character string*20

	ipara = -1
	if (nstrng .lt. 1) go to 500
	string = alpha(1)(1:nalpha(1))
	call lcase (string,nalpha(1))
	iparb = ionum(string,nalpha(1))
	if (iparb .eq. -1) go to 520
	if (uopen(iparb) .ne. 1) go to 560
	rewind iparb
	ipara = 0
	return

500	call wrtout(output,' Error :  syntax incorrect.')
	return

520	write (wrtbuf,530) alpha(1)(1:nalpha(1))
	call wrtout(output,wrtbuf)
530	format(' Error :  unknown unit name  ',a)
	return

560	write (wrtbuf,570) alpha(1)(1:nalpha(1))
	call wrtout(output,wrtbuf)
570	format(' Warning :  unit is not open  ',a)
	return

	end

c*************************************************************************

	subroutine dfiles

	include 'iounit.h'

	integer i, nch, chrcnt

	external chrcnt
  
7500	call wrtout(output,
     &  ' unit open for i/o               : file attached to unit')
	do 7550 i=1,30
	   if (uopen(i) .ne. 1)	go to 7550
	   nch = max( chrcnt(fnames(i),128), 1 ) ! catch return value 0
	   if (i .eq. cardout)	write (wrtbuf,7520)  fnames(i)(1:nch)
	   if (i .eq. datain)	write (wrtbuf,7521)  fnames(i)(1:nch)
	   if (i .eq. lineio)   write (wrtbuf,75211) fnames(i)(1:nch) 
	   if (i .eq. dataot)	write (wrtbuf,7522)  fnames(i)(1:nch)
	   if (i .eq. altdat)	write (wrtbuf,7523)  fnames(i)(1:nch)
	   if (i .eq. cardin)	write (wrtbuf,7524)  fnames(i)(1:nch)
	   if (i .eq. spool)	write (wrtbuf,7525)  fnames(i)(1:nch)
	   if (i .eq. infile)	write (wrtbuf,7526)  fnames(i)(1:nch)
	   if (i .eq. logfil)	write (wrtbuf,7528)  fnames(i)(1:nch)
	   if (i .eq. scrtch)	write (wrtbuf,7529)  fnames(i)(1:nch)
	   if (i .eq. rawin)	write (wrtbuf,7530)  fnames(i)(1:nch)	    
	   call wrtout(output,wrtbuf)
7550	continue
7520	format(' outfile  ( formatted output )   : ',a)
7521	format(' datain   ( data input )         : ',a)
75211	format(' lineio   ( line list )          : ',a)
7522	format(' dataout  ( data output )        : ',a)
7523	format(' altdata  ( alternate input )    : ',a)
7524	format(' com      ( command input )      : ',a)
7525	format(' spool    ( spool output )       : ',a)
7526	format(' infile   ( ASCII input )        : ',a)
7528	format(' log      ( session log )        : ',a)
7529	format(' scratch  ( scratch directory )  : ',a)
7530	format(' rawin    ( interferogram )      : ',a)
	return
	end

c************************************************************************

	subroutine dexit

	include 'iounit.h'

  	close (cardout,  status='keep')
  	close (spool,    status='keep')
  	close (cardin,   status='keep')
  	close (logfil,   status='keep')
  	close (infile,   status='keep')
  	close (datain,   status='keep')
  	close (datain+1, status='keep')
	close (lineio,   status='keep')
  	close (dataot,   status='keep')
  	close (dataot+1, status='keep')
  	close (rawin,    status='keep')

	call closexgr   ! close Xgremlin main window

	stop            ! stops the Xgremlin 
	end

c****************************************************************************

	integer function ionum(str,n)

c  return the fortran unit number given the character name of the unit

	include 'iounit.h'

	character*20 str
	integer n

	ionum = -1
	if (str(1:n) .eq. 'cards')    ionum = cardin
	if (str(1:n) .eq. 'com')      ionum = cardin
	if (str(1:n) .eq. 'logfile')  ionum = logfil
	if (str(1:n) .eq. 'outfile')  ionum = cardout
	if (str(1:n) .eq. 'infile')   ionum = infile
	if (str(1:n) .eq. 'spool')    ionum = spool
	if (str(1:n) .eq. 'datain')   ionum = datain
	if (str(1:n) .eq. 'lineio')   ionum = lineio
	if (str(1:n) .eq. 'dataout')  ionum = dataot
	if (str(1:n) .eq. 'altdata')  ionum = altdat
	if (str(1:n) .eq. 'scratch')  ionum = scrtch
	if (str(1:n) .eq. 'rawin')    ionum = rawin
	return
	end

