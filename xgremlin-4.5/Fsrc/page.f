
	subroutine page(imode)
*
* prints another page of the log file
*
	include 'infmtn.h'
	include 'iounit.h'
	include 'datetc.h'

	character version*44
	character*50 host, osname, osversion, cversion, fversion
	integer logpage,imode

	external ddate
	external dtime

	call ddate(cdat)
	call dtime(ctim)

	if (imode .gt. 1) go to 520

* this section is executed with page(1) when log file is started

	call getver(version)

	day = ' '                   ! fill with blanks
 	id = ' '
	nstrng = 0
	nalpha(1) = 0
	logpage = 0
	return

520	continue
	logpage = logpage + 1

	write(logfil,1001) cdat,ctim,logpage
 1001	format(''/,a14,1x,a12,40x,'Page:',i3)

	if (logpage.eq.1) then
	   write(logfil,*) ' '
	   write(logfil,*) ' '
	   write(logfil,*) 
     & ' -----------------------------------------------------------'
           write(logfil,*) 
     &     ' Xgremlin :   FTS data analysis program'
           write(logfil,*) 
     &     ' Version  :  ', version
           write(logfil,*)
     & ' -----------------------------------------------------------'
	   write(logfil,*) ' '
	   write(logfil,*) ' '

* write out a version summary for trouble shooting

	   call versinfo(host,osname,osversion,cversion,fversion)

	   write(logfil,*) ' System version summary:'
	   write(logfil,*) ' -------------------------'
	   write(logfil,*) ' '
	   write(logfil,*) ' Compiled on:'
	   write(logfil,*) '       System name : ', host
	   write(logfil,*) '       OS name     : ', osname
	   write(logfil,*) '       OS version  : ', osversion
	   write(logfil,*) '       C compiler  : ', cversion
	   write(logfil,*) '       Fortran     : ', fversion
	   write(logfil,*) ' '

	   call sysinfo( host, osversion )

	   write(logfil,*) ' Running on:'
	   write(logfil,*) '       System name : ', host
	   write(logfil,*) '       OS version  : ', osversion
           write(logfil,*) ' '
	   if (mbocode .eq. 1) then
	      write(logfil,*) 
     &          ' Machine byte order: big endian (mbocode=1)'
	   else
	      write(logfil,*) 
     &          ' Machine byte order: little endian (mbocode=0)'
	   end if
	   write(logfil,*) ' '
	end if

	write(logfil,1003) day,id
 1003	format(a,2x,a60/)
	return

	end

c------------------------------------------------------------------

	subroutine maknam (name)
*
* this subroutine invents a nice looking and unique log file name
*
	character*(*) name
	character monam(12)*3
	integer iyr,imon,iday
	integer ihr,imin,isec,icsc
	integer len

	data monam/'jan','feb','mar','apr','may','jun',
     &             'jul','aug','sep','oct','nov','dec'/

 	call getdat (iyr,imon,iday)
	call gettim (ihr,imin,isec,icsc)

*  store day, hour and minute in name
	if ( ihr .lt. 10 ) then            ! hour is a single digit
	   write (name, 9001) iday, monam(imon), iyr, ihr, imin
 9001	   format('xgremlin-',i2,a3,i4,'-',i1,':',i2,'.log')
	else
	   write (name, 9002) iday, monam(imon), iyr, ihr, imin
 9002	   format('xgremlin-',i2,a3,i4,'-',i2,':',i2,'.log')
	end if
	len = length( name )
	if ( imin .lt. 10 ) then           ! overwrite blank in minute
	   name(len-5:len-5) = '0'
	end if
	if ( iday .lt. 10 ) then           ! overwrite blank in day
	   name(10:10) = '0'
	end if
	
        return
	end

c -------------------------------------------------------------------

        subroutine ddate (name)

* makes a nice date string :  mmm dd, yyyy

        character monam(12)*3
        character*(*) name
        integer iyr,imon,iday

        data monam/'Jan','Feb','Mar','Apr','May','Jun',
     &             'Jul','Aug','Sep','Oct','Nov','Dec'/

        call getdat (iyr,imon,iday)
        write (name,11) monam(imon),iday,iyr
11      format (1x,a3,1x,i2,',',i4)    ! use 4-digit year
        return
        end
 
* -------------------------------------------------------------------

	subroutine dtime (name)

* makes a nice time string

	character*(*) name
	integer ihr,imin,isec,icsc

	call gettim (ihr,imin,isec,icsc)  ! hh:mm:ss time format
	write (name,11) ihr,imin,isec
11	format (1x,i2,':',i2,':',i2,' ')
	if (ihr  .lt. 10) name(2:2) = '0'
	if (imin .lt. 10) name(5:5) = '0'
	if (isec .lt. 10) name(8:8) = '0'
	return
	end


*------------------------------------------------------------------------------

	subroutine remark(string)

* put a remark in text window and log file

	include 'datetc.h'
	include 'iounit.h'

	character*(*) string

	call cputime(ctim)
	write (wrtbuf,111) ctim, string
	call wrtstr(wrtbuf)
	write (logfil,111) ctim, string
 111	format(1x,a10,' REMARK >>>> ',a)
	return
	end

*------------------------------------------------------------------------------

	subroutine etime(ics)

* this version here returns the elapsed CPU time in seconds

	integer ics

	call getcpu( ics )

	return
	end
*------------------------------------------------------------------

	subroutine shwsiz(iret)

* print the size of this version of Xgremlin to text window and log file

	include 'datetc.h'
	include 'iounit.h'

	integer iret
	character sizstr*72

 111	format('    r,tr  :  ',i8)
 112	format('    fft   :  ',i8)
 113	format('    lines :  ',i8)
	call wrtstr(' Internal array sizes:')
	write(logfil,*) 'Internal array sizes:'

	write(sizstr,111) r_size
	call wrtstr(sizstr)
	write(logfil,*) sizstr

	write(sizstr,112) ffta_size
	call wrtstr(sizstr)
	write(logfil,*) sizstr

	write(sizstr,113) linbuf_size
	call wrtstr(sizstr)
	write(logfil,*) sizstr

	iret = 1    ! this is needed for the caller

	return
	end

*------------------------------------------------------------------
