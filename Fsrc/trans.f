c \gremlin\src\trans.for   Ver. 1.0

c Fixes needed in greg:
c	overwrite .hdr when ncenter changes

* Mar 7, 1996   ug   autotran removed (together with 'greg' command)
c Revision 1.09  05.09.93  incntr uses unchanged header ncenter 1st scan
c Revision 1.08  05.06.93  Autotran only sets nphap if =0
c Revision 1.07  29.05.93  Autotran added
c Revision 1.06  11.05.93  Use iprnt to control printing
c Revision 1.05  30.10.92  Allow 4 million points
c   on a 486 DX-33, 2**22 in 165s, 2**21 in 78s (fft only)
c   on a 486DX2-50, 2**22 in ???s, 2**21 in 52s (fft only)
c Revision 1.04  01.09.91  Altinf added
c Revision 1.03  26.05.91  Phase signs corrected
c Revision 1.02  09.05.91  Phase args now all d.p.
c Revision 1.01  18.04.91  Loop now ends at ngot
c Version  1.00  07.04.91


      subroutine trans(r,ffta,phz)

      include 'datetc.h'
      include 'altinf.h'
      include 'iounit.h'
      include 'inparms.h'
      include 'outparms.h'
      
      real r(*), ffta(*), phz(*)

      common/chek/sumin,sumout,pt1in,pt1out
      double precision sumin,sumout
      real pt1in,pt1out
      
      character*4 ial
      
* check if too many points
      if( 2**mpwr .gt. ffta_size ) then
         write (wrtbuf,*) ' FFA: Transform too big!'
         call wrtstr(wrtbuf)
         return
      endif
 
      pt1in = 0
      pt1out = 0

      if (xcentr .lt. 0) xcentr = imax
      if (xcentr .lt. 0) xcentr = 1.0
      if (imax .gt. ngot/2) onesidfl = .false.
      
      if (iprnt .gt. 0) then
         call remark(' Preparing input file for big FFT.')

         write(logfil,21) mpwr
         write( wrtbuf,21) mpwr
         call wrtout(output,wrtbuf)
 21      format(12x,' Transform options:  transform size = 2**',i2)

         ial = 'No '
         if (onesidfl .and. imax .gt. 1) ial = '   '
         write(logfil,23) ial
         write(wrtbuf,23) ial
         call wrtstr(wrtbuf)
 23      format(12x,a3,'one-sided taper will be applied.')

         ial = 'No '
         if (apodfl) ial = '   '
         write(logfil,25) ial,apod1
         write(wrtbuf,25) ial,apod1
         call wrtstr(wrtbuf)
 25      format(12x,a3,'apodization will be applied.',f10.0)

         write(logfil,27) rsum,ngot
         write(wrtbuf,27) rsum,ngot
         call wrtstr(wrtbuf)
 27      format(12x,' Scan corrected by subtracting',e14.6,
     *              ' from',i8, ' points.')
      endif

c zero correct, amertize and apodize; follow old disk structure for now 
c Form input cross check sum as data is read.

      jrec = (ngot + blksiz -1)/blksiz

      write (wrtbuf,101) jrec
      call wrtstr(wrtbuf)
 101  format (16x,'of',i4,' blocks expected.')

      sumin = 0.0d0
      xinsumsq = 0.0
      iptcnt = 1

      do i=1,jrec                              ! START LOOP
         call rmove (blksiz,ffta(iptcnt),r(1))
         call zerocor(r(1),blksiz,iptcnt)
         if (onesidfl .and. imax.gt.1)
     &      call amrtiz(r(1),blksiz,iptcnt)
         if (apodfl)   
     &      call apodiz(r(1),blksiz,iptcnt)
         
         do j=1,blksiz                         ! modifications complete
            sumin = sumin + r(j)               ! sum 
            xinsumsq = xinsumsq + r(j)*r(j)    ! sum 
         end do
         call rmove (blksiz,r(1),ffta(iptcnt)) ! and replace

         iptcnt = iptcnt + blksiz
         write (wrtbuf,'(2h +,10x,i4)') i
         call prevline(wrtbuf)
      end do                                   ! END LOOP

      call wrtstr(' Input modifications complete')

c The FFT is done by the IEEE routine FFA.
c   Large fft, real data ; data in array ffta(noptex)
c   Input, output and transform all REAL ndata = 2**mpwr (24 max)

c Save origin point before and after for cross check.

      call remark('Call FFA.')

      pt1in = ffta(noptex/2 + 1) ! origin now n/2 + 1
      call ffak(ffta(1),noptex)
      pt1out = ffta(1)

c before phase correction, sum output real points and squares
      sumout = -0.5d0*ffta(1)
      xpara = 0.0d0
      sumsq = 0.0
      xsumsq = 0.0
      do j=1,noptex,2
         sumout = sumout + ffta(j)
         xpara  = xpara  + ffta(j+1)
         sumsq  = sumsq  + ffta(j)*ffta(j)
         xsumsq = xsumsq + ffta(j+1)*ffta(j+1)
      end do
      sumout = (2.0d0*sumout + ffta(noptex+1))/noptex
c*********
c		write (logfil,*) ' input  sumsq = ',xinsumsq
c		write (logfil,*) ' output sum   = ',sumout
c		write (logfil,*) ' output sumsq = ',sumsq
c		write (logfil,*) ' output imsum = ',xpara
c		write (logfil,*) ' output xsumsq= ',xsumsq
c*********
      sumsq = 0.5*sumsq

c   print out check sums

      if (iprnt .gt. 0) then
         write(logfil,103)
         write( wrtbuf,103)
         call wrtstr(wrtbuf)
         write(logfil,104) sumin
         write( wrtbuf,104) sumin
         call wrtstr(wrtbuf)
         write(logfil,105) pt1out-sumin
         write( wrtbuf,105) pt1out-sumin
         call wrtstr(wrtbuf)
 103     format(' Cross checking information:')
 104     format('   Input data sum      ',1p,e17.8)
 105     format('   Difference from first word of output',e13.4)
      endif

c Compare sumout (with an estimate of accumulated rounding
c error) to the value at the transform origin

      err = 2.0*(2.0**(-24))*sqrt(sumsq)/noptex

      if (iprnt .gt. 0) then
         write(logfil,1303) sumout,err
         write(wrtbuf,1303) sumout,err
         call wrtstr(wrtbuf)
         write(logfil,1304) pt1in-sumout
         write(wrtbuf,1304) pt1in-sumout
         call wrtstr(wrtbuf)
         write(logfil,1305)
         write(wrtbuf,1305)
         call wrtstr(wrtbuf)
 1303    format(' Output data mean   ',1p,e12.5,' +-',e7.1)
 1304    format(' Difference from extended input data midpoint',e13.2)
 1305    format(' TRANSFORM COMPLETE.')
      endif

c ----------           ----------           -----------           -------

c if imax not at center of scan, do a phase shift on this block 
c corresponding to (ncenter - imax) points (mode bit 1 set)
c And/or if requested, do phase correction (mode bit 2 set)
      mode = 0
      if (xcentr .ge. 1.0) mode = 1
      if (phcorfl) mode = mode + 2
      if (mode .eq. 0) then
         call wrtstr(' No Phase correction requested')
         go to 600
      endif

c phase correction from old subroutine ftsout

      call remark(' Correction phase.  Options:')

      ial = 'No'
      if (phcorfl) ial = 'Do'
      xtmp = (noptex/2 + 1) - xcentr
      write(logfil,2000) xtmp,ial
      write(wrtbuf,2000) xtmp,ial
      call wrtstr(wrtbuf)
 2000 format(' Shift = ',f11.2,
     *       ' ',a2,' chromatic phase correction ')

      if (rclphz) write(logfil,2001)
 2001 format(2h +,29x,'using phase of an earlier scan.')

      lnth = blksiz             ! initialize lengths
      nblks = noptex/lnth
      if (iprnt .gt. 0) then
         write (wrtbuf,501) nblks
         call wrtstr(wrtbuf)
      end if
 501  format (' Performing phase correction',
     *        ' of',i4,' blocks expected')

      ipt = 1                   ! initialize counters
      iptcnt = 1
      
      do 500 k=1,nblks		! PHASE CORRECTION LOOP
                                ! fetch a complex block 
         call rmove (blksiz,ffta(iptcnt),r(1))
         if (mode .gt. 0) call shiftf(ipt,lnth,mode,r,phz)

c write corrected complex back to masstore for later output
         call rmove (blksiz,r(1),ffta(iptcnt))

         ipt = ipt + lnth/2     ! increment counters
         iptcnt = iptcnt + blksiz

         write (wrtbuf,'(2h +,10x,i4)') k ! display
         call prevline(wrtbuf)
 500  continue			! END PHASE CORRECTION LOOP

 600  call wrtstr('   ')        ! move to next line after counter
      return 
      end

c -------------------------------------------------------------
      subroutine shiftf(ipt,lnth,mode,r,phz)

      include 'datetc.h'
      include 'inparms.h'

      real r(*),phz(*)

      double precision arg,delarg,pi,xpc,xps
      double precision sarg,sdarg
      data pi/3.14159265358979d0/

c   shift data array 
c   ipt is counter for frequency

      delarg = 2.0d0*pi*((noptex/2 + 1) - xcentr)
      delarg = delarg/noptex
      arg = (ipt - 1)*delarg
      n = arg*0.5d0/pi
      sarg = arg - 2.0d0*pi*n
      sdarg = delarg
      ratn = (float(nphz))/float(noptex)

      if (mode .eq. 1) then
         do i=1,lnth,2
            xps = dsin(arg)
            xpc = dcos(arg)
            temp   = r(i)   * xpc - r(i+1) * xps
            r(i+1) = r(i+1) * xpc + r(i)   * xps
            r(i) = temp
            arg = arg + delarg
         end do
      end if

c If requested, calculate phase corrected output  - 

      if (mode .eq. 2) then
         xpts = 1.0 + ratn*float(ipt - 1)
         do i=1,lnth,2
            jpts = xpts
            delta = (phz(jpts+1) - phz(jpts)) * (xpts-jpts)
            rarg = phz(jpts) + delta
            sa = sin(-rarg)
            ca = cos(rarg)
            temp   = ca * r(i)   - sa * r(i+1)
            r(i+1) = ca * r(i+1) + sa * r(i)
            r(i) = temp
            xpts = xpts + ratn
         end do
      end if

c Or, do both--

      if (mode .eq. 3) then
         xpts = 1.0 + ratn*float(ipt - 1)
         do i=1,lnth,2
            jpts = xpts
            rarg = phz(jpts) + (phz(jpts+1)-phz(jpts))*(xpts-jpts)
            targ = sarg - rarg
            sa = sin(targ)
            ca = cos(targ)
            temp   = ca * r(i)   - sa * r(i+1)
            r(i+1) = ca * r(i+1) + sa * r(i)
            r(i) = temp
            xpts = xpts + ratn
            sarg = sarg + sdarg
         end do
      end if
      return

      end
