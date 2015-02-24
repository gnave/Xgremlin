c  \plot\psmainit.for  Ver. 2.2

c  Revision 2.22  89/11/22  PRN handled in binary; continue after open fails
c  Revision 2.21  89/11/14  Bitmap in c:\ 
c
	subroutine mainit
c
c Initialization subroutine
c
        include 'parms.h'

	integer ifd,ifdp, length
	integer ixorg,iyoff
	common/pbuf/ifd,ifdp,ixorg,iyoff
        character*20 ts

	external length

c Clear the screen and write the program header:
	call  clear
	write (*,15)
15	format(//
     &     15x,'+----------------------------------------------+'/,
     &	   15x,'|','        Postscript Plot Program Shell         |'/,
     &	   15x,'|','         Unix Ver 3.0 of 11 May 1991          |'/,
     &	   15x,'|','               NCAR / NSO / NOAA              |'/,
     &	   15x,'|','         M. C. Abrams / J. W. Brault          |'/,
     &	   15x,'|','                                              |'/,
     &	   15x,'|','            NIST revision 2   3/1998          |'/,
     &	   15x,'+----------------------------------------------+'//)

c Obtain the date and time from the system
	call timestr( ts )
	write (output,501) ts
501	format(15x,'Date: ',a20/)
 
c initialize 

	keybd  = .true.
	frstim = .true.
	wavlen = .false.
	axlabl = .false.
	lbl    = .false.
	axis   = .false.
	notick = .false.
        boxflg = .false.
	verbos = .false.
	xscal  = 1.0
	yscal  = 1.0
	nlabl  = 0

c open all files

* open output file
	if ( length(outfil) .eq. 0 ) then
	   outfil = 'psplot.ps'
	end if
90	open (spool,file=outfil,access='sequential',
     &        status='replace')

* open input file created by Xgremlin
	if ( length(infil) .eq. 0 ) then
	   infil = 'atlas.data'
	end if
	open (unit=datain,file=infil,access='sequential',
     &		     form='formatted',status='old',err=99,
     &		     iostat=i)
        atls = .true.
	go to 100

99	write(*,*) ' Unable to open input file - continuing'
        atls = .false.

* open and process a local cards file (psplot.init)
100	kl =  0
	nc =  0
	if ( length(confil) .eq. 0 ) then
	   confil = 'psplot.init'
	end if
     	open (cardin,file=confil, status='old',err=200)
	uopen(cardin) = 1
	fnames(cardin) = 'plot.init'
	ipara = 0
        return

 200	write (*,*) ' No local init file'
	ipara = 1
        return

	end

*------------------------------------------------------------------

      integer function length(str)
*
*---  number of chars in a string
*
      integer icut
      character str*(*)
      do icut=len(str),1,-1
         if (str(icut:icut) .ne. ' ') then 
            length = icut
            return
         endif
      end do
      length = 0
      return
      end

*------------------------------------------------------------------

c
c Initialization Block Data subroutine
c
	BLOCK DATA bdsr

        include 'parms.h'

	integer ifd,ifdp
	integer ixorg,iyoff
	common/pbuf/ifd,ifdp,ixorg,iyoff

        data input /5/,output/6/,stdout/6/, stdin/5/
	data cardin/4/,spool/9/
	data datain/8/
        data outfile/23/, stderr/30/
	end
