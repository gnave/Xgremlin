*
* initializes the Gremlin in Xgremlin before the unser interface
* is initialized and opens the initialization files
*
      subroutine gremlininit()

      include 'datetc.h'

      call mainit
      
      if (ipara .eq. 0) then
         nstrng = 0
         call comin
      endif

      return
      end
