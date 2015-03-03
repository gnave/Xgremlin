      integer function length(str)
*
*---  number of chars in a string
*
      character str*(*)
      integer icut

      do icut=len(str),1,-1
         if (str(icut:icut) .ne. ' ') then 
            length = icut
            return
         endif
      end do
      length = 0
      return
      end

*--------------------------------------------------------------------------
 
      subroutine lalign( s )
*
* left align the characters in string 's'
*
      character s*(*)
      integer k,l,n,length

      external length

*--- find beginning of string
      do k=1,len(s)
         if ( s(k:k) .ne. ' ' ) goto 1
      end do

*--- find end of string and number of characters
 1    l = length(s)
      n = l-k+1

*--- copy characters and fill trailer with blanks
      s(1:n) = s(k:l)
      s(n+1:) = ' '

      return
      end

*--------------------------------------------------------------------------
 
      subroutine ucase(str)
*
*---  flips all characters in str to upper case
*
      character str*(*)
      integer i,kord

      do i=1,len(str)
        kord = ichar( str(i:i) )
        if ((kord.gt.96).and.(kord.lt.123)) str(i:i) = char(kord-32)
      end do

      return
      end

*--------------------------------------------------------------------------
 
      subroutine lcase(str,n)

      character str*(*)
      integer i, j, n

c first n char: convert any upper case alphabetic characters to lower case
c 'A' = 65, 'Z' = 90,  'a' - 'A' = 32

      do i=1,n
         j = ichar( str(i:i) )
         if (j .ge. 65 .and. j .le. 90) str(i:i) = char(j + 32)
      enddo
      return
      end

*--------------------------------------------------------------------------
 
