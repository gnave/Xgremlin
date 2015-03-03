*
* A package of subroutines to read and interpret NSO type header files
* that does not make use of NAMELIST I/O. This allows for much better
* portability, better diagnostics and more flexibility.
*
* Author: Ulf Griesmann
*

*-- input functions ------------------------------------------------

      subroutine rdheader(hunit, err)
*
* read the entire header and build up tables of keywords and values
* in memory. The variable 'err' is set to 0 if the file was read
* successfully, the line number where an error occured otherwise.
* 'err' is set to -1 if the file is not a NSO type 6 file.
* The header file must be opened before calling this function; the 
* logical unit associated with the header file is passed as an
* argument.
*
      integer hunit                       ! open header file unit
      integer err

      include 'headers.h'

      character line*256
      integer j, k, l1, l2, nret, thresh, length

      external thresh, length
      
*---  rewind and read header file
      rewind(hunit)
      do k=1,nmax
         read(hunit, '(a)', end=101, err=102) line
         if (line(1:3) .eq. 'END') then
            goto 101
         end if
         nret = thresh(line, hkey(k), hvalue(k))
         if (nret .eq. 1) cycle           ! get the next line
         if (line(9:9) .ne. '=') then     ! not a NSO type 6 file
            err = -1
            return
         end if
         if (nret .eq. -1) then
            err = k
            return
         end if
      end do

 101  idxmax = k

*---  collate any continued strings
      k = 2
      do while (k .le. idxmax)
         if (hkey(k)(1:8) .eq. 'continue') then
            l1 = length(hvalue(k-1))
            l2 = length(hvalue(k))
            if (l1+l2+1 .gt. 128) then    ! string gets too long
               err = k
               return
            end if
            hvalue(k-1)(l1+1:l1+1) = ' '
            hvalue(k-1)(l1+2:l1+2+l2) = hvalue(k)(1:l2)
            do j=k, idxmax-1
               hkey(j) = hkey(j+1)
               hvalue(j) = hvalue(j+1)
            end do
            idxmax = idxmax - 1
         else
            k = k + 1
         end if
      end do

      err = 0                             ! normal exit
      return

 102  err = k                             ! error reading a line
      return

      end

*-------------------------------------------------------------------

      subroutine hgets(name, str)
*
* Retrieve a character string from the keyword/value table
*
      include 'headers.h'

      character name*(*), str*(*)
      integer idx, k, l, length
      
      idx = keynum(name)
      if (idx .eq. 0) then
         return
      end if
      l = length(hvalue(idx))
      if (l .gt. len(str)) then
         l = len(str)
      end if
      do k=1, l
         str(k:k) = hvalue(idx)(k:k)
      end do
      do k=l+1, len(str)
         str(k:k) = ' '
      end do

      return
      end

*-------------------------------------------------------------------

      subroutine hgeti(name, inum)
*
* Retrieve an integer number from the keyword/value table
*
      include 'iounit.h'
      include 'headers.h'

      character name*(*)
      integer inum
      integer idx

      idx = keynum(name)
      if (idx .ne. 0) then
         read(hvalue(idx),*,err=10) inum
      end if
      
      return

 10   write(wrtbuf,100) name
 100  format (' Warning: error when reading variable :  ',a20)
      call wrtstr(wrtbuf)
      return

      end

*-------------------------------------------------------------------

      subroutine hgetr(name, anum)
*
* Retrieve a real number from the keyword/value table
*
      include 'iounit.h'
      include 'headers.h'

      character name*(*)
      real anum

      integer idx
      
      idx = keynum(name)
      if (idx .ne. 0) then
         read(hvalue(idx),*,err=10) anum
      end if

      return

 10   write(wrtbuf,100) name
 100  format (' Warning: error when reading variable :  ',a20)
      call wrtstr(wrtbuf)
      return

      end

*-------------------------------------------------------------------

      subroutine hgetd(name, dnum)
*
* Retrieve a double precision number from the keyword/value table
*
      include 'iounit.h'
      include 'headers.h'

      character name*(*)
      double precision dnum

      integer idx
      
      idx = keynum(name)
      if (idx .ne. 0) then
         read(hvalue(idx),*,err=10) dnum
      end if

      return

 10   write(wrtbuf,100) name
 100  format (' Warning: error when reading variable :  ',a20)
      call wrtstr(wrtbuf)
      return

      end

*-------------------------------------------------------------------

      integer function thresh(line, key, val)
*
* Strip comments from a line and break it up into keyword and value.
* In other words: separate the chaff from the wheat. Returns 0 if 
* all is well, 1 if the line is empty and -1 if an error occured.
*
      character line*(*), key*(*), val*(*)

      integer j, k, m

*---  clear strings
      do j=1, len(key)
         key(j:j) = ' '
      end do
      do j=1, len(val)
         val(j:j) = ' '
      end do

*---  check if line is blank
      j = 1
      do while (line(j:j).eq.' ' .and. j.le.len(line))
         j = j+1
      end do
      if (j.gt.len(line) .or. line(j:j).eq.'/') then
         thresh = 1
         return
      end if

*---  copy keyword
      do k=1,len(key)
         if (line(k:k).eq.'=' .or. line(k:k).eq.' ') then
            goto 100
         end if
         key(k:k) = line(k:k)
      end do
 100  call lcase(key, k)              ! map characters to lower case

*---  advance to start of value
      do m=k, len(line)
         if (line(m:m).ne.'=' .and. line(m:m).ne.' ') then
            goto 200
         end if
      end do
      if (m .ge. len(line)) then      ! no value for keyword
         thresh = -1
         return
      end if
      
*---  copy value
 200  j = 1
      if (line(m:m) .eq. "'") then    ! it's a string
         do k=m+1, len(line)          ! skip the '
            if (line(k:k) .eq. "'") then
               goto 300
            end if
            val(j:j) = line(k:k)
            j = j+1
         end do
         thresh = -1                  ! closing ' is missing
         return
      else                            ! it's a number
         do k=m, len(line)
            if (line(k:k).eq.' ' .or. line(k:k).eq. '/') then
               goto 300
            end if
            val(j:j) = line(k:k)
            j = j+1
         end do
      end if

 300  continue
      thresh = 0

      return
      end
      
*-------------------------------------------------------------------

      integer function keynum(key)
*
* Return the index of key in the key table. For now, we do a simple,
* linear keyword search. Not pretty but it works.
*
      include 'headers.h'
      character key*(*)
      integer k, l, length

      external length

      keynum = 0
      l = length(key)
      do k=1,idxmax
         if (key(1:1) .eq. hkey(k)(1:1)) then
            if (l .eq. length(hkey(k))) then
               if (key(1:l) .eq. hkey(k)(1:l)) then
                  keynum = k
                  return
               end if
            end if
         end if
      end do

      return
      end


*-- output functions -----------------------------------------------

      subroutine hputc(unit, comment)
*
* write out a comment line
*
      integer unit
      character comment*(*)
      character line*80

      line(1:2) = '/ '
      line(3:) = comment
      write(unit, '(a)') line

      return 
      end

*-------------------------------------------------------------------

      subroutine hputs(unit, nwid, var, value, comment)
*
* write out a string variable
*
      integer unit, nwid
      character var*(*), value*(*), comment*(*)
      character line*80
      integer length, ib, ie, il

      external length

      line = ' '
      line(1:8)  = var
      line(9:11) = '= '''

      ib = 1
      ie = 1
      il = length(value)
      do while (ie .le. il)
         ie = ie + nwid - 1
         if (ie .gt. il) then
            ie = il
         else
            if (value(ie+1:ie+1).ne.' ') then
               do while (value(ie:ie).ne.' ' .and. ie.gt.0)
                  ie = ie - 1
               end do
               if (ie.gt.0) ie = ie - 1  ! point to a character
            end if
         end if
         line(12:nwid+11) = value(ib:ie)
         if (length(comment).ne.0) then
            line(nwid+12:nwid+14) = '''/ '
            line(nwid+15:) = comment
         else
            line(nwid+12:nwid+12) = ''''
         end if
         write(unit, '(a)') line
         line(1:11) = 'continue= '''
         ie = ie + 2  ! skip blank
         ib = ie
      end do

      return 
      end

*-------------------------------------------------------------------

      subroutine hputi(unit, nwid, var, value, comment)
*
* write out an integer variable
*
      integer unit, nwid
      character var*(*), comment*(*)
      integer value
      character line*80
      integer length

      external length

      line = ' '
      line(1:8)  = var
      line(9:10) = '= '
      write(line(12:nwid+12), '(i8)') value
      if (length(comment).ne.0) then
         line(nwid+13:nwid+14) = '/ '
         line(nwid+15:) = comment
      end if
      write(unit, '(a)') line

      return 
      end

*-------------------------------------------------------------------

      subroutine hputr(unit, nwid, var, value, comment)
*
* write out a real variable
*
      integer unit, nwid
      character var*(*), comment*(*)
      real value
      character line*80
      integer length

      external length

      line = ' '
      line(1:8)  = var
      line(9:10) = '= '

      write(line(12:nwid+12), '(g16.6)') value
      if (length(comment).ne.0) then
         line(nwid+13:nwid+14) = '/ '
         line(nwid+15:) = comment
      end if
      write(unit, '(a)') line

      return 
      end

*-------------------------------------------------------------------

      subroutine hputd(unit, nwid, var, value, comment)
*
* write out a double precision variable
*
      integer unit, nwid
      character var*(*), comment*(*)
      double precision value
      character line*80
      integer length

      external length

      line = ' '
      line(1:8)  = var
      line(9:10) = '= '

      write(line(12:nwid+12), '(g18.11)') value
      if (length(comment).ne.0) then
         line(nwid+13:nwid+14) = '/ '
         line(nwid+15:) = comment
      end if
      write(unit, '(a)') line

      return 
      end

*-------------------------------------------------------------------

* Revision history:
* -----------------
* $Log: headers.f,v $
* Revision 1.9  1999/09/10 22:56:16  ulf
* fixed another one in the string collation code.
*
* Revision 1.8  1999/09/10 22:04:58  ulf
* fixed a bug in the code that collates continued strings.
*
* Revision 1.7  1999/09/09 22:45:27  ulf
* fixed another one: keyword strings were never converted to lower case.
*
* Revision 1.6  1999/09/09 21:52:09  ulf
* Fixed several bugs in 'hputs' and a bug in 'keynum'.
*
* Revision 1.5  1999/09/06 20:59:21  ulf
* Variable fields were one character too wide on output.
*
* Revision 1.4  1999/09/05 12:07:11  ulf
* Use G format descriptors to print real numbers.
*
* Revision 1.3  1999/09/01 23:02:22  ulf
* added subroutine 'hpute' to write out numbers in exponential format.
*
* Revision 1.2  1999/09/01 22:55:05  ulf
* Check comment length before appending.
*
* Revision 1.1  1999/09/01 22:46:41  ulf
* Initial revision
*
*
