      subroutine readasc(name,ixcol,iycol,izcol,ncol,iadd,r,tr,wrkspc)
*
* read  data from a multi column (max 10 cols) ascii file into the r array
* the data must be equally spaced.
*
* name   : file name
* ixcol  : first colum
* iycol  : 2nd column
* izcol  : column with imaginary part
* ncol   : number of columns to read
* iadd   : =1 if new data are to be added to arrays, =0 otherwise
* r      : real data array
* tr     : contains imaginary part if needed
* wrkspc : a workspace array
*
      include 'infmtn.h'
      include 'datetc.h'
      include 'linparms.h'
      include 'iounit.h'
      
      real r(*), tr(*), wrkspc(*)

      integer maxnf
      parameter( maxnf = 10 )

      integer ixcol,iycol,izcol,ncol,nf,numpts,k,iadd,kflag
      character name*(*), fields(maxnf)*32,line*80,ch*1
      logical lex

*---  here starts it
      inquire(file=name,exist=lex)
      if (.not.lex) then
         write(wrtbuf,'(a,a,a)') ' Error :  file ',name,
     &                           ' does not exist.'
         call wrtstr(wrtbuf)
         return
      end if
      open(77,file=name,status='old',form='formatted')

      if ( ncol .lt. 3 ) then
         nwpp = 1       ! real data
      else
         nwpp = 2       ! complex data
      end if
      lfirst = .true.   ! new spectrum
      nwos = 1          ! default is wavelength
      numpts = 0

      nol = 0           ! zap internal line list if one exists
      nlgot = 0
      ipara = 0
      call linevis( 0 )
      call llerase

*---  skip over comment lines
 10   read(77,'(a80)',end=20) line
      ch = line(1:1)
      if ( ch.eq.';' .or. ch.eq.'#' .or. ch.eq.'!' ) then
         if ( index(line,'nwos') .ne. 0 ) then
            call split(line,fields,nf,maxnf)
            read(fields(3),*,err=90) nwos         ! read value of nwos
         end if
         goto 10
      end if
      
      call split(line, fields, nf, maxnf )        ! split into words
      if ( nf .lt. max(ixcol,iycol,izcol) ) then
         call wrtstr(' Error :  requested columns do not exist.')
         return
      end if

      numpts = numpts+1
      if ( numpts.gt.r_size .or. numpts.gt.ffta_size ) then
         numpts = numpts - 1
         write(wrtbuf,*) ' Error :  can only read ',numpts,' points'
         call wrtstr(wrtbuf)
         call wrtstr('          increase size of r or fft array')
         goto 20
      end if

      if ( ncol .eq. 1 ) then
         wrkspc(numpts) = real(numpts)                    ! use ffta for temp storage
         read(fields(ixcol),*,err=91) tr(numpts)          ! requires f2c
      else 
         read(fields(ixcol),*,err=91) wrkspc(numpts)      ! read x value
         if ( ncol .eq. 2 ) then
            read(fields(iycol),*,err=91) tr(numpts)       ! y value
         else
            read(fields(iycol),*,err=91) tr(2*numpts-1)   ! y value
            read(fields(izcol),*,err=91) tr(2*numpts  )   ! z value
         end if
      end if

      goto 10                   ! next line

 20   continue

*---  calculate wref, delw etc.
      if ( nwpp .eq. 1 ) then
         kflag = 2
         call ssort(wrkspc, tr, numpts, kflag) ! sort real data in ascending order
      end if
      wref = wrkspc(1)
      delw = ( wrkspc(numpts) - wrkspc(1) ) / ( numpts - 1 )
      aps = delw
      corr = 0.d0
      if ( delw .le. 0.0 ) then
         call wrtstr(' Error :  data are not in ascending order.' )
         return
      end if
      if ( nwpp .eq. 1 ) then
         nop  = numpts
      else
         nop = 2*numpts
      end if

*---  finally copy / add data in temp. array wrkspc to r array
      if ( iadd .eq. 1 ) then
         do k=1,nop
            r(k) = r(k) + tr(k)
         end do
      else
         do k=1,nop
            r(k) = tr(k)
         end do
      end if

 99   close(77)
      return

 90   call wrtstr(' Error :  unable to read  nwos.')
      goto 99

 91   call wrtstr(' Error :  unable to read data.')
      goto 99

      end

*-------------------------------------------------------------------

      subroutine writeasc( fnam, cchr, ia, ib, r )
*
* write out data in r array in ascii format suitable for reading by
* programs such as GLE
*
* fnam : name of output file
* cchr : comment character
* ia   : first point to write out
* ib   : last point to write out
* r    : data to write
*
      character fnam*(*), cchr*1
      integer ia, ib
      real r(*)

      logical lex
      integer nump
      character date*20
      double precision pos

      include 'infmtn.h'
      include 'iounit.h'
      include 'datetc.h'
      
* check sanity of parameters
      if ( ia .lt. 1 )   ia = 1
      if ( ib .gt. nop ) ib = nop

* then check if file exists already.
      inquire( file=fnam, exist=lex )
      if ( lex ) then     ! do nothin'
         call wrtstr( ' Error :  output file exists - aborted.' )
         return
      end if

* open output file and write data
      call timestr(date)
      open( unit=77,file=fnam,form='formatted',
     &      status='replace',err=100 )
      
      write(77,'(a,a,a)') cchr,' created by Xgremlin   ',date
      write(77,'(a)') cchr      
      write(77,'(a,a,i2)') cchr,' nwos ',nwos
      write(77,'(a)') cchr

      pos = wref + delw * (ia - 1)
      if ( nwpp .eq. 1 ) then         ! real data
         do nump=ia, ib
            write(77,111) pos*(1.d0 + wavcorr), r(nump)
            pos = pos + delw
         end do
      else if ( nwpp .eq. 2 ) then    ! complex data
         do nump=ia, ib
            write(77,112) pos*(1.d0 + wavcorr), r(2*nump-1), r(2*nump)
            pos = pos + delw
         end do
      end if

 111  format(1x,f12.5,g14.6)
 112  format(1x,f12.5,g14.6,g14.6)

      close(77)
      return

 100  call wrtstr( ' Error :  could not open output file.')
      return

      end

*-------------------------------------------------------------------

      subroutine writegle( fnam, title, ia, ib )
*
* writes an input file ready for reading by the GLE plotting 
* program
*
      character fnam*(*), title*(*)
      integer ia, ib

      include 'infmtn.h'

      character glefile*128, date
      integer length,n,nump,idx
      logical lex
      real w
      double precision wnum
      
      external length

      include 'transform.h'

* make output file name and check if it exists
      idx = index(fnam,'.')
      if ( idx .eq. 0 ) then
         glefile = fnam(1:length(fnam))//'.gle'
      else
         glefile = fnam(1:idx-1)//'.gle'
      end if
      inquire(file=glefile,exist=lex)
      if ( lex ) then
         call wrtstr(' Error :  GLE file exists - aborted.' )
         return
      end if

* write out GLE file
      open(unit=77,file=glefile,form='formatted',
     &             status='replace',err=100)
      
      call timestr(date)
      write(77,11) '! GLE file created by Xgremlin   ',date
      write(77,11) '!'
      write(77,11) '! general settings'
      write(77,11) 'size 28 19'
      write(77,11) 'set font rm   ! Roman'
      write(77,11) 'set lwidth 0.026'
      write(77,11) '!'
      write(77,11) 'begin graph'
      write(77,11) '   size 28 19'
      write(77,11) '   nobox'
      
      write(77,11) '! data'
      if ( nwpp .eq. 1 ) then   ! real data
         write(77,11) '   data ',fnam(1:length(fnam)),' d1'
         write(77,11) '   d1 line'
         write(77,11) '   d1 color black'
      end if

      if ( nwpp .eq. 2 ) then   ! complex data
         write(77,11) '   data ',fnam(1:length(fnam)),' d1 d2'
         write(77,11) '   d1 line'
         write(77,11) '   d1 color black'
         write(77,11) '   d2 line'
         write(77,11) '   d2 color blue'
      end if

      write(77,'(a,f12.5,a,f12.5)') '   xaxis min ',wnum(ia),
     &                              ' max ',wnum(ib)
         
      write(77,11) '!'
      write(77,11) '! title and labels'
      write(77,11) '   title "',title(1:length(title)),'"'
      write(77,11) '   ytitle "relative intensity"'
      if ( nwos .eq. 1 ) then
         write(77,11) '   xtitle "wavelength / nm"'
      else
         write(77,11) '   xtitle "wavenumber / cm^{-1}"'
      end if

      write(77,11) 'end graph'

      close(77)
      return

 11   format(a,a,a)
 100  call wrtstr(' Error :  could not create GLE file.' )

      return
      end

*-------------------------------------------------------------------

      subroutine split( line, fields, nfld, maxf )
*
*     breaks up a string into words returned in an array of strings.
*     nfld returns the number of words found
*

      character line*(*), fields(*)*(*), TAB*1
      integer i, ii, k, nfld, maxf

      TAB = char( 9 )
      ii  = 1
      k   = 1

*---  first clear all fields
      do i=1, maxf
         fields(i) = ' '
      end do

*---  skip whitespace at the beginning
 30   do i=ii, len( line )
         if ( line(i:i).ne.' '.and.line(i:i).ne.TAB ) goto 10
      end do
      goto 40

*---  copy the word into a field
 10   continue
      ii = i
      do i=ii, len( line )
         if ( line(i:i).eq.' '.or.line(i:i).eq.TAB ) goto 20
         fields(k)(i-ii+1:i-ii+1) = line(i:i)
      end do
      goto 40

*---  repeat until all words have been collected
 20   continue
      ii = i
      k = k+1
      goto 30

 40   nfld = k-1
      return
      end

*------------------------------------------------------------------------      

* Revision history:
* -----------------
* $Log: readasc.f,v $
* Revision 1.21  1996/07/23 01:52:51  ulf
* better error message in case of buffer overflow
*
* Revision 1.20  1996/07/15 00:36:02  ulf
* changed name of include files
*
* Revision 1.19  1996/07/06 17:40:09  ulf
* also set variable 'aps' when reading data (this seems to be a hangover of the
* old DECOMP and duplicates delw ).
*
* Revision 1.18  1996/06/30 00:49:18  ulf
* replace 'Numerical Recipes' sorting routines with free counterpart
*
* Revision 1.17  1996/06/25 02:44:16  ulf
* add a check to make sure that no more data are read in than we have
* room in the r-array
*
* Revision 1.16  1996/06/15 18:30:51  ulf
* moved call to 'clrfrm' up to 'dispatch.f'
*
* Revision 1.15  1996/06/15 03:23:29  ulf
* add clearing of plot mode undo stack
*
* Revision 1.14  1996/03/13 18:02:59  ulf
* modified for dynamic arrays
*
* Revision 1.13  1996/03/10  16:49:10  ulf
* modified 'readasc' for dynamic arrays
*
* Revision 1.12  1996/01/17  14:15:47  ulf
* change type of wnum from real to double precision
*
* Revision 1.11  1995/10/18  11:36:19  ulf
* modify function 'readasc' so that new data can be added to data already
* in the r array.
*
* Revision 1.10  1995/09/26  14:23:52  ulf
* had introduced a bug.
*
* Revision 1.9  1995/09/26  14:02:36  ulf
* checked with 'spag', removed unnecessary include files
*
* Revision 1.8  1995/09/06  01:40:04  ulf
* fixed error: ncols --> ncol
*
* Revision 1.7  1995/08/24  02:54:16  ulf
* cosmetic changes in GLE file
*
* Revision 1.6  1995/08/09  19:16:52  ulf
* erase internal line list when a new data file is read in
*
* Revision 1.5  1995/08/04  02:54:02  ulf
* add interface to GLE plotting program with subroutine 'writegle'
*
* Revision 1.4  1995/08/03  01:53:14  ulf
* add the subroutine 'writeasc' to write out data in multi column ascii format
*
* Revision 1.3  1995/07/16  02:51:41  ulf
* set 'lfirst' to TRUE when reading a new spectrum
*
* Revision 1.2  1995/07/15  21:49:35  ulf
* set nwpp to 1 to make sure that it has the right value even after working
* with a complex spectrum.
*
* Revision 1.1  1995/07/14  17:00:54  ulf
* Initial revision
*






