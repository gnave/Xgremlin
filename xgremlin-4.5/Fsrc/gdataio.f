* This file is part of Xgremlin
*
* 23 Oct 95    ug    New scratch file subroutines for unlimited scratch files
*
*
	subroutine scrwrite( numkey, numpts, fileid, arr )
*
* for details of the new scratch file system see file 'Scratch.c'
*
* Syntax:  save key [r|tr] [numpts] ['id']
*
	include 'datetc.h'
	include 'infmtn.h'

	integer numkey, numpts
	character fileid*(*)

	real arr(*)

	call wlowscr( numkey, numpts, nwpp, wref, delw, arr, fileid )

	return
	end

c ----------------------------------------------------------------

	subroutine scrread( numkey, numpts, arr )

	include 'datetc.h'
	include 'infmtn.h'

	integer numkey, numpts
	real arr(*)

	call rlowscr( numkey, numpts, nwpp, wref, delw, arr, id )

	nop = numpts
	call disper

	return
	end

c ----------------------------------------------------------------

*
* subroutines to emulate the old scrread / scrwrite calls
*

	subroutine oscrwrite( numkey )

	include 'datetc.h'
	include 'infmtn.h'

	integer numkey

	if (length(id) .ne. 0) then
	   call write8kr(numkey,nwpp,wref,delw,id)
	else
	   call write8kr(numkey,nwpp,wref,delw,'internally generated  ')
	end if

	return
	end

*----

	subroutine oscrread( numkey )

	include 'datetc.h'
	include 'infmtn.h'

	integer numkey	

	call read8kr(numkey,nwpp,wref,delw,id)
	nop = 8192
	call disper

	return
	end


c ----------------------------------------------------------------

	subroutine readata(nunit,nbstart,buf,nbytes,nerr)

	include 'iounit.h'

	integer buf(*),ibuf(1024)

	nerr = 0
	lbk = 1024
	nwstart = nbstart/4 
	nrc = nwstart/lbk 
	nskip0 = nwstart - lbk*nrc
	nwords = nbytes/4
	ilim = 0
c take care of initial partial record
	if (nskip0 .ne. 0) then
	   ilim = nwords
	   if (lbk-nskip0 .le. nwords) ilim = lbk-nskip0
	   read (nunit+1,rec=nrc+1,err=470,iostat=nerr) 
     &	        (ibuf(j),j=1,nskip0+ilim)
	   do i=1,ilim
	      buf(i) = ibuf(nskip0+i)
	   enddo
	   nwords = nwords - ilim
	   if (nwords .eq. 0) return
	   nrc = nrc + 1
	endif

	nblks = nwords/lbk
	nrem = nwords - lbk*nblks
	if (nblks .gt. 0) then
	   do i=1,nblks
	      ia = lbk*(i-1) + ilim
	      read (nunit+1,rec=nrc+i,err=470,iostat=nerr) 
     &	           (buf(j),j=ia+1,ia+lbk)
	   enddo
	endif
	ia = lbk*nblks + ilim

c end partial record
	if (nrem .gt. 0) 
     &	    read (nunit+1,rec=nrc+nblks+1,err=470,iostat=nerr) 
     &			(buf(i),i=ia+1,ia+nrem)
	return    

* error...during disk read.
 470	continue
	if (nerr .eq. -1) then    ! we bumped into the file end
	   nerr = 0
	   return                 ! just carry on
	else                      ! something is really wrong
	   write(wrtbuf,9000) nerr
	   call wrtout(output,wrtbuf) 
 9000	   format(' disk read failed in READATA, error= ',i4)
	   nerr = 2
	   return
	end if

	end

c ----------------------------------------------------------------

	subroutine wrtdata(nunit,nbstart,buf,nbytes,nerr)

	include 'iounit.h'

	integer buf(*),ibuf(1024)

	nerr = 0
	lbk = 1024
	nwstart = nbstart/4 
	nrc = nwstart/lbk 
	nskip0 = nwstart - lbk*nrc
	nwords = nbytes/4
	ilim = 0

	if (nskip0 .ne. 0) then
c take care of initial partial record
c       read existing whole record
	   read (nunit+1,rec=nrc+1,err=470,iostat=nerr) 
     &	        (ibuf(j),j=1,lbk)
c write in upper part and replace
	   ilim = nwords
	   if (lbk-nskip0 .le. nwords) ilim = lbk-nskip0
	   do i=1,ilim
	      ibuf(nskip0+i) = buf(i)
	   enddo
	   write (nunit+1,rec=nrc+1,err=470,iostat=nerr) 
     &           (ibuf(j),j=1,lbk)
	   nwords = nwords - ilim
	   if (nwords .eq. 0) return
	   nrc = nrc + 1
	endif

	nblks = nwords/lbk
	nrem = nwords - lbk*nblks
	if (nblks .gt. 0) then
	   do i=1,nblks
	      ia = lbk*(i-1) + ilim
	      write (nunit+1,rec=nrc+i,err=470,iostat=nerr) 
     &              (buf(j),j=ia+1,ia+lbk)
	   enddo
	endif
c end partial record
	if (nrem .gt. 0) then
	   ia = lbk*nblks + ilim
	   read (nunit+1,rec=nrc+nblks+1,err=470,iostat=nerr) 
     &          (ibuf(j),j=1,lbk)
	   do i=1,nrem
	      ibuf(i) = buf(i+ia)
	   enddo
	   write (nunit+1,rec=nrc+nblks+1,err=470,iostat=nerr) 
     &           (ibuf(j),j=1,lbk)
	endif
	return    

c ******** no check yet on running into end of file*******

c					error...during disk read.
470	write (wrtbuf,9000) nerr
	call wrtout(output,wrtbuf)
9000	format(' disk write failed in WRTDATA, error= ',i4)
	nerr = 2
	return

	end

* ----------------------------------------------------------------

	subroutine readline( nunit, nbytes, nbuff, nerr )
*
* a streamlined subroutine for reading line list records which does not
* do any copying. The 'nbytes' parameter is used for compatibility with
* the general purpose 'readata' subroutine.
*
* nunit  : unit of file ( UNLIKE IN READATA )
* nbytes : where in the file to start reading the record
* nbuff  : buffer, to be filled with the record
* nerr   : return error status back to caller
*
	include 'iounit.h'
	include 'infmtn.h'

	integer nunit, nbytes, nbuff(*), nerr
	integer i
	double precision dpbuf(10)              ! for alignment
	integer locbuf(20)                      ! a local buffer
	integer*2 slocbuf(40)
	equivalence ( dpbuf(1), locbuf(1), slocbuf(1) )

	nrec = 1 + ( nbytes / 80 )
	if ( mbocode .eq. 1 ) then
	   read(nunit,rec=nrec,err=100,iostat=nerr)(locbuf(i),i=1,20)
	   if ( nrec .eq. 1 ) then              ! byte reverse header
	      call ibrev( locbuf(1) )           ! number of lines
	      call ibrev( locbuf(2) )           ! number of bytes
	      call fbrev( locbuf(3) )
	      call fbrev( locbuf(4) )
	      call fbrev( locbuf(5) )
	   else                                 ! byte reverse the whole thing
	      call dbrev( locbuf(1) )           ! sigma
	      call fbrev( locbuf(3) )           ! xint
	      call fbrev( locbuf(4) )           ! width
	      call fbrev( locbuf(5) )           ! dmping
	      call sbrev( slocbuf(11) )         ! itn
	      call sbrev( slocbuf(12) )         ! ihold
	      call fbrev( locbuf(8) )           ! epstot
	      call fbrev( locbuf(9) )           ! epsevn
	      call fbrev( locbuf(10) )          ! epsodd
	      call fbrev( locbuf(11) )          ! epsran
	      call fbrev( locbuf(12) )          ! spare
	   end if
	   do i=1,20
	      nbuff(i) = locbuf(i)
	   end do
	else
	   read(nunit,rec=nrec,err=100,iostat=nerr)(nbuff(i),i=1,20)
	end if
	return

 100	write(wrtbuf,200) nerr
        call wrtout(output,wrtbuf) 
 200	format(' disk read failed in READLINE; error= ',i4)
	nerr = 2
	return

	end

* ----------------------------------------------------------------

	subroutine wrtline( nunit, nbytes, nbuff, nerr )
*
* a streamlined subroutine for writing line list records which does not
* do any copying. The 'nbytes' parameter is used for compatibility with
* the general purpose 'wrtdata' subroutine.
*
* nunit  : unit of file( UNLIKE IN READATA )
* nbytes : where in the file to write the line record
* nbuff  : buffer with line record to be written to disk
* nerr   : return error status back to caller
*
	include 'iounit.h'
	include 'infmtn.h'

	integer nunit, nbytes, nbuff(*), nerr
	integer i
	double precision dpbuf(10)              ! for alignment
	integer locbuf(20)                      ! a local buffer
	integer*2 slocbuf(40)
	equivalence ( dpbuf(1), locbuf(1), slocbuf(1) )

	nrec = 1 + ( nbytes / 80 )
	if ( mbocode .eq. 1 ) then
	   do i=1,20
	      locbuf(i) = nbuff(i)
	   end do
	   if ( nrec .eq. 1 ) then              ! byte reverse header
	      call ibrev( locbuf(1) )           ! number of lines
	      call ibrev( locbuf(2) )           ! number of bytes
	      call fbrev( locbuf(3) )
	      call fbrev( locbuf(4) )
	      call fbrev( locbuf(5) )
	   else                                 ! byte reverse the whole thing
	      call dbrev( locbuf(1) )           ! sigma
	      call fbrev( locbuf(3) )           ! xint
	      call fbrev( locbuf(4) )           ! width
	      call fbrev( locbuf(5) )           ! dmping
	      call sbrev( slocbuf(11) )         ! itn
	      call sbrev( slocbuf(12) )         ! ihold
	      call fbrev( locbuf(8) )           ! epstot
	      call fbrev( locbuf(9) )           ! epsevn
	      call fbrev( locbuf(10) )          ! epsodd
	      call fbrev( locbuf(11) )          ! epsran
	      call fbrev( locbuf(12) )          ! spare
	   end if
	   write(nunit,rec=nrec,err=100,iostat=nerr)(locbuf(i),i=1,20)
	else
	   write(nunit,rec=nrec,err=100,iostat=nerr)(nbuff(i),i=1,20)
	end if
	return

 100	write(wrtbuf,200) nerr
        call wrtout(output,wrtbuf) 
 200	format(' disk write failed in WRTLINE; error= ',i4)
	nerr = 2
	return

	end

* ----------------------------------------------------------------

c This is the code used by CREATE to make new output .dat and .hdr files
c	syntax:  create <fname>  

	subroutine dstart(ierr)

	include 'datetc.h'
	include 'iounit.h'

	logical lex
	integer ierr
	character filnam*256

	ierr = 0
	ipara = -1
c  Errors:  no file name given; name too long; dataout still open. 
	if (nstrng .lt. 1) then
	   call wrtout(output,' Error :  no file name given.')
	   ierr = 1
	   return
	end if
	if (nalpha(1) .gt. filchr) then
	   write (wrtbuf,610) filchr
	   call wrtout(output,wrtbuf)
 610	   format (' too many characters in file name - max ',i3)
	   ierr = 2
	   return
	end if
	if (uopen(dataot) .eq. 1) then
	   call wrtout(output,' Error :  dataout is already open')
	   ierr = 3
	   return
	end if

	filnam = ' '
   	filnam = alpha(1)(1:nalpha(1))
	ik = nalpha(1) + 1

c All in order - create .hdr and .dat files if they don't exist
	filnam(ik:ik+3) = '.hdr'
	inquire(file=filnam,exist=lex)
	if ( lex ) then
	   call wrtout(output,' Error :  header file exists.')
	   ierr = 4
	   return
	end if
      	open (unit=dataot,file=filnam,access='sequential',
     *	     form='formatted',status='new',err=800)

	filnam(ik:ik+3) = '.dat'
	inquire(file=filnam,exist=lex)
	if ( lex ) then
	   call wrtout(output,' Error :  data file exists.')
	   ierr = 5
	   return
	end if
	open (unit=dataot+1,file=filnam,
     *		  access='direct',recl=4096,
     *		  form='unformatted',status='new',err=800)

c successful return
	filnam(ik:ik+3) = '    '
	fnames(dataot) = filnam
	uopen(dataot) = 1
	ipara = 0
	return

 800	call wrtstr(' Error :  could not create output file.')
	ierr = 6
	return
	end

* ----------------------------------------------------------------

	subroutine dump(r, tr, ffta, phz)
*
* dump any of the arrays to a file
*
        include 'datetc.h'
        include 'iounit.h'
        include 'infmtn.h'
        include 'altinf.h'
        include 'extinf.h'

	real r(*), tr(*), phz(*), ffta(*)
	real t1, t2
	integer i, j, k, ni, nf, ia, nerr
	character filnam*80
	logical lex


	j = nalpha(1)
	k = nalpha(2)

	if (alpha(1)(1:j).eq.'r' .or. alpha(1)(1:j).eq.'tr') then
	   if (nstrng .ne. 2) then
	      call wrtstr(
     &             ' Syntax error :  dump <array> <file> [ni nop]')
	      return
	   endif
	   ni = 1
	   nf = nop
	   if (ifx .eq. 1) then
	      ni = 1
	      nf = inum(1)
	   end if
	   if (ifx .eq. 2) then
	      ni = inum(1)
	      nf = ni + inum(2) - 1
	   end if
	else
	   if (nstrng .ne. 2 .or. ifx .ne. 2) then
	      call wrtstr(
     &             ' Syntax error :  dump <array> <file> [ni nop]')
	      return
	   endif
	   ni = inum(1)
	   nf = ni + inum(2) - 1
	end if
	   
	write (wrtbuf,1001) alpha(1)(1:j),alpha(2)(1:k)
	call wrtstr(wrtbuf)
	write (logfil,1001) alpha(1)(1:j),alpha(2)(1:k)
1001	format (' writing array ',a,' to file pair ',a)

	filnam = ' '
	filnam = alpha(2)(1:k)
	filnam(k+1:k+4) = '.dat'

	inquire(file=filnam, exist=lex)
	if (lex) then
	   call wrtstr(' Error :  output file exists.')
	   return
	end if
	open (34,file=filnam,status='new',
     &	      access='direct',recl=2048)

	ia = 1
	if (alpha(1)(1:j) .eq. 'r') then
	    do i=ni,nf,512
		write (34,rec=ia,err=1002,iostat=nerr) 
     &                (r(j),j=i,i+511)
		ia = ia + 1
	    enddo
	endif
	if (alpha(1)(1:j) .eq. 'tr') then
	    do i=ni,nf,512
		write (34,rec=ia,err=1002,iostat=nerr) 
     &                (tr(j),j=i,i+511)
		ia = ia + 1
	    enddo
	endif
	if (alpha(1)(1:j) .eq. 'phz') then
	    do i=ni,nf,512
		write (34,rec=ia,err=1002,iostat=nerr) 
     &                (phz(j),j=i,i+511)
		ia = ia + 1
	    enddo
	endif
	if (alpha(2)(1:j) .eq. 'ffta') then
	    do i=ni,nf,512
		write (34,rec=ia,err=1002,iostat=nerr) 
     &                (ffta(j),j=i,i+511)
		ia = ia + 1
	    enddo
	endif
	close (34)

C	k = index(filnam,'.')
C  Make sure .hdr extension goes on the end of the filename (gn, June,2006)

	filnam(k+1:k+4) = '.hdr'
	open (34,file=filnam,form='formatted',
     &	      status='new',access='sequential')

	t1 = wstart
	t2 = wstop
	i = npo

	call disper
	xparb = ni
	call ptow
	wstart = xparb
	xparb = nf
	call ptow
	wstop = xparb
	npo = nf - ni + 1

	nint = npo                  ! fix transform parameters
	ncenter = 0
	ntrans = 2
	do i=2,32
	   ntrans = 2 * ntrans
	   if (ntrans .ge. npo) goto 1003
	end do
 1003	continue

	call infwrt(34)
	close (34)
	wstart = t1
	wstop = t2
	npo = i
        return

 1002	write (wrtbuf,9000) nerr
	call wrtout(output,wrtbuf)
 9000	format(' disk write failed in DUMP, error= ',i4)
	return

	end

* ----------------------------------------------------------------

	subroutine dcopy(ir)
*
* copy sections of files into a new file
*
	include 'datetc.h'
	include 'infmtn.h'
	include 'altinf.h'
	include 'iounit.h'

	integer ir(*)   ! for consistency with definition of 'readata'
	character filnam*80
	double precision wstrt, wend, wr
	integer i
	integer nbstart, nbstop, nbout, nop2, nbs
	integer irvmode
	integer noffset, nerr

	irvmode = 0
	noffset = 0

c output file must have been CREATEd****************

        if (nstrng .ge. 2) go to 1000  ! FITS

* copy NSO type 6 file 
	irlflg = 0
	irvmode = bocode           ! byte order of datain
	noffset = fboffs
	if (nstrng .ne. 0) then
	   if (alpha(1)(1:4) .eq. 'real') then
	      if (nwpp .eq. 2) irlflg = 1
	   endif
	endif

	wstrt = wstart
	wend = wstop
	if (ifl .eq. 2) then
	   wstrt = xnum(1)
	   if (wstrt .lt. wstart) wstrt = wstart
	   wend = xnum(2)
	   if (wend .gt. wstop) wend = wstop

	   nbstart = (wstrt -wstart)/disp
	   if (nwpp .eq. 2) nbstart = 2*(nbstart/2) ! if complex, start real
	   nbstart = 4*nbstart*nwpp + noffset
	   nbstop = (wend - wstart + disp)/disp
	   if (nwpp .eq. 2) nbstop = 2*(nbstop/2)
	   nbstop = 4*nbstop*nwpp + noffset
	elseif (ifl .eq. 0) then
	   nbstart =  noffset
	   nbstop = 4*npo + noffset
	end if

	nbout = 0

	do 680 nbs = nbstart, nbstop, 4*nrtr
	   wr = wstart + 0.25*disp*(nbs - noffset)/nwpp
	   write (wrtbuf,'(2h +,f10.3)') wr
	   call prevline(wrtbuf)

	   nop2 = (nbstop - nbs)/4
	   if (nop2 .gt. nrtr) nop2 = nrtr
c read record(lu,nbstrt,buf,nbytes,err)
	   if (nop2 .eq. 0) go to 680
	   call readata(datain, nbs, ir(1), 4*nop2, nerr)
	   if (nerr .ne. 0) go to 997

* change byte order if necessary
	   if ( irvmode .ne. mbocode ) then
	      call chibord( ir, nop2 )
	   end if

c if real only, drop imaginary
	   if (irlflg .eq. 1) then
	      do i=3,nop2,2
		 j = (i+1)/2
		 ir(j) = ir(i)
	      end do
	      nop2 = nop2/2
	   endif

c  write
	   call wrtdata(dataot, nbout, ir(1), 4*nop2, nerr)
	   if (nerr .ne. 0) go to 998
	   nbout = nbout + 4*nop2
680	continue

c update .hdr for dataout , but save and restore changed parameters

	wend = wstop
	wstrt = wstart
	nop2 = npo
	nwt = nwpp
	i = bocode

	wstop  = ((nbstop-noffset)/(4*nwpp))*disp + wstart - disp
	wstart = ((nbstart-noffset)/(4*nwpp))*disp + wstart
	npo = (nbstop - nbstart)/4
	delw = disp
	fboffs = 0
	bocode = mbocode
	if (irlflg .eq. 1) then
	   npo = npo/2
	   nwpp = 1
	endif
	call infwrt(dataot)
	write (dataot,690) cdat,ctim
690	format (' File copy made  ',a10,2x,a10)

	fboffs = noffset
	bocode = i
	wstop = wend
	wstart = wstrt
	npo = nop2
	nwpp = nwt

	return

997	call wrtstr(' error on data read - abort')
	write (wrtbuf,'(a,i8,a,i8)') ' nbstart=',nbs,' nbytes=',4*nop2
	call wrtstr(wrtbuf)
	return
998	call wrtstr(' error on data write - abort')
	return


* FITS
 1000	continue
	if (alpha(1)(1:4).ne.'FITS' .and. 
     &      alpha(1)(1:4).ne.'fits') then
	   call wrtstr(' Error :  impossible conversion (FITS only).')
	   return
	end if

	filnam = alpha(2)(1:nalpha(2))
	irvmode = 1
	if (nstrng .eq. 3 .and. alpha(3)(1:6) .eq. 'nobrev') then
	   irvmode=0
	   call wrtstr(' Bytes will NOT be reversed')
	end if
 
	if (uopen(datain) .eq. 1) then
	   write (wrtbuf,550) alpha(1)(1:nalpha(1))
	   call wrtstr(wrtbuf)
 550	   format(' ABORT: -- datain is already open  ',a)
	   return
	endif

	idfile = datain + 1

	open (unit=idfile,file=filnam,
     &        access='direct',recl=4096,
     &        form='unformatted',status='old',err=2700,iostat=ierr)
	uopen(datain) = 1
	fnames(datain) = filnam
 
	call b6init          ! read FITS header
        call fitinfrd(nb)
        bocode = 0
        if (irvmode .eq. 0) bocode = 1
        call infwrt (dataot)
        nbstart = nb
        nbwrite = 0

c convert and write the data record.
        nbuf = (npo+719)/720
        write (wrtbuf,2301) nbuf
	call wrtstr(wrtbuf)
2301    format (1x,i5,' blocks.')

        do i=1,nbuf
	   call readata(datain,nbstart,ir(1),2880,nerr)
	   nbstart = nbstart + 2880

c do complete byteswap on FITS binary for PC, but not if 'nobrev'
	   if (irvmode .eq. 1) call chibord(ir, 720)
	       
c write record
	   call wrtdata(dataot, nbwrite, ir(1), 2880, nerr)
	   nbwrite = nbwrite + 2880
	   if (mod(i,20) .eq. 0) then
	      write (wrtbuf,'(2h +,i4)') i
	      call prevline(wrtbuf)
	   end if
        end do

        return

 2700	call wrtstr(' Error opening FITS file.')
	return

	end
