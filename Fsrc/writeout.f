      subroutine wrtout( iu, str )
*
* catches writes to standard output 
* but does not display until dspstr is called
*
      implicit none

      integer istdo, istderr, iu, length
      character str*(*)

      data istdo/6/, istderr/0/

      if (iu .eq. istdo .or. iu .eq. istderr) then
         call wrtstr( str )
      else
         write(iu,*) str(1:length(str))       ! write to a file
      end if

      return
      end



