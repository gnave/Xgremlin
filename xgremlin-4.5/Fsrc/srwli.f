*
* Subroutines for FTS refractometry
*
* Author: Ulf Griesmann   Feb. 2000
*
*
* NOTE:
* This module provides functions to extract refractive index informaxtion
* from white light fringe spectrum. All functions are sub-commands
* of the 'index' command.
*

      subroutine srwli(r,ffta)

      parameter (iofc=100, pi=3.141592654)

      include 'inparms.h'
      include 'infmtn.h'
      include 'altinf.h'
      include 'iounit.h'
      include 'datetc.h'
      include 'linparms.h'

      real r(*), ffta(*)

      integer icent, ichrp, mwidth, mw2, bw
      integer isup, iinf, ibeg, iend
      real et, det        ! etalon thickness, uncertainty
      real opl, dopl, an, dan, fbw
      real rsup, rinf

      save et, det

      
*--- at least one string variable is expected
      if (nalpha(1) .eq. 0) then
         call wrtstr(' Syntax error for index command.')
         call wrtstr(' SYNTAX:')
         call wrtstr(' index set <thickness> [uncertainty]')
*        call wrtstr(' index set <rms noise>')                   ! maybe needed later
         call wrtstr(' index estimate [chirp width]')
         call wrtstr(' index mask <chirp width> [fracbw] [fft]')
         return
      end if


*--- Subcommands
      if (alpha(1)(1:nalpha(1)) .eq. 'set')   go to 1000
      if (alpha(1)(1:3)         .eq. 'est')   go to 2000
      if (alpha(1)(1:nalpha(1)) .eq. 'mask')  go to 3000


*--- SET various parameters
 1000 continue
      if (nalpha(2) .eq. 0) then
         call wrtstr(' Error :  missing parameter in set.')
         return
      end if
 
*--- noise
      if (alpha(2)(1:nalpha(2)) .eq. 'noise') then
         if (ifl .ne. 0) then
            noiselev = xnum(1)
            if (noiselev.lt.1.e-3 .or. noiselev.gt.1.e3 ) then
               write(wrtbuf,91) noiselev
            else
               write(wrtbuf,92) noiselev
            end if
            call wrtstr(wrtbuf)
 91         format(' RMS noise level set to :',3x,e15.6)
 92         format(' RMS noise level set to :',3x,f15.6)
         end if
      end if

*--- thickness of the etalon
      if (alpha(2)(1:5) .eq. 'thick') then
         if (ifl .ne. 0) then
            et = xnum(1)
            if (ifl .ge. 2) then
               det = xnum(2)
            else
               det = 0.0
               call wrtstr(' Warning :  thickness uncertainty not set.')
            end if
            write(wrtbuf,93) et, det
            call wrtstr(wrtbuf)
 93         format(' Etalon thickness set to : ',f10.6,
     &             ' +/- ',f10.6,' mm')
         end if
      end if

      return


*--- ESTIMATE the refractive index from the interferogram in 'r'
* Algorithm: 1) deterinfe location of center burst
*            2) deterinfe location of chirp signal following center
*            3) calculate the index with the supplied thickness
 2000 continue
*--- get the burst width
      if (ifx .eq. 0) then                        ! mask width is missing
         call wrtstr(' Warning : chirp width not specified.')
         mwidth = 0
      else
         mwidth = inum(1)
      end if

*--- find the center burst
      av = aravrg(r,nop)
      call arposmax(r,nop,rinf,iinf,rsup,isup)
      rinf = rinf - av       ! remove DC level
      rsup = rsup - av
      if (abs(rsup) .gt. abs(rinf)) then
         icent = isup
      else
         icent = iinf
      end if
      write(wrtbuf,95) icent
      call wrtstr(wrtbuf)
 95   format(' Position of center burst: ',i8)

*--- now find the largest chirp to the 'right'
      call arposmax(r(icent+iofc),nop-icent-iofc,rinf,iinf,rsup,isup)
      rinf = rinf - av       ! remove DC level
      rsup = rsup - av
      if (abs(rsup) .gt. abs(rinf)) then
         ichrp = isup
      else
         ichrp = iinf
      end if
      ichrp = ichrp + icent + iofc - 1
      write(wrtbuf,96) ichrp
      call wrtstr(wrtbuf)
 96   format(' Position of chirp signal: ',i8)

*--- compute the optical path length in the etalon in units of mm (!)
      opl = 5.0*real(ichrp-icent+1)/(refwavno * fdr)
      dopl= 5.0*real(mwidth)/(refwavno * fdr)
      write(wrtbuf,97) opl, dopl
      call wrtstr(wrtbuf)
 97   format(' Optical path length n*t = ',f11.7,' +/- ',f11.7,' mm')
      if (et .gt. 0.0) then
         an = opl / et
         dan = an * sqrt( (dopl/opl)**2 + (det/et)**2)
         write(wrtbuf,98) an, dan
         call wrtstr(wrtbuf)
 98      format(' Refractive index      n = ',f11.7,' +/- ',f11.7)
      end if

      return


*--- MASK: remove noise and center burst from the interferogram
*          leaving only the chirp signals. Needed to remove 
*          noise and unwanted signal from the interferogram after
*          the phase correction has been calculated.
*    index mask mwidth [fracbw] ['fft']
*
 3000 continue
*--- check if all command parameters are there
      if (ifx .eq. 0) then                        ! mask width is missing
         call wrtstr(' Error :  mask width not specified.')
         write(logfil,*) 
     &        ' ERROR :  index mask width not specified.'
         return
      else
         mwidth = inum(1)
      end if
      if (ifl .eq. 0) then                        ! fractional width not specified
         fbw = 0.9                                ! 90% fractional bandwidth
      else
         fbw = xnum(1)
      end if
      mw2 = 0.5 + real(mwidth)/2.0                ! half of mask width, rounded
      bw = 0.5 + mwidth * (1.0 - fbw) / 2.0       ! bell width

*--- check which array to mask: r or fft
      if (alpha(2)(1:nalpha(2)) .eq. 'fft') then  ! mask fft array (imax is center)

*--- center of interferogram in ffta is known to be at 'ncenter'
         icent = imax

*--- now find the largest chirp to the 'right'
         av = aravrg(ffta,noptex)
         call arposmax(ffta(icent+iofc),noptex-icent-iofc,
     &                 rinf,iinf,rsup,isup)
         rinf = rinf - av       ! remove DC level
         rsup = rsup - av
         if (abs(rsup) .gt. abs(rinf)) then
            ichrp = isup
         else
            ichrp = iinf
         end if
         ichrp = ichrp + icent + iofc - 1

*--- zero everything but the two largest chirp signals
         ibeg = 1
         iend = 2 * icent - ichrp - mw2
         do i=ibeg, iend
            ffta(i) = 0.0
         end do
         ibeg = 2 * icent - ichrp + mw2
         iend = ichrp - mw2
         do i=ibeg, iend
           ffta(i) = 0.0
         end do
         ibeg = ichrp + mw2
         iend = noptex
         do i=ibeg, iend
            ffta(i) = 0.0
         end do

*--- and round the edges with cosine bells
         k = 2 * icent - ichrp - mw2
         do i=1,bw
            fac = 0.5 * (1.0 - cos(pi * i / bw))
            ffta(i+k) = fac * ffta(i+k)
         end do
         k = 2 * icent - ichrp + mw2 - bw
         do i=1,bw
            fac = 0.5 * (1.0 + cos(pi * i / bw))
            ffta(i+k) = fac * ffta(i+k)
         end do
         k = ichrp - mw2
         do i=1,bw
            fac = 0.5 * (1.0 - cos(pi * i / bw))
            ffta(i+k) = fac * ffta(i+k)
         end do
         k = ichrp + mw2 - bw
         do i=1,bw
            fac = 0.5 * (1.0 + cos(pi * i / bw))
            ffta(i+k) = fac * ffta(i+k)
         end do

      else                                        ! mask r array

*--- first find the center in r
         av = aravrg(r,nop)
         call arposmax(r,nop,rinf,iinf,rsup,isup)
         rinf = rinf - av       ! remove DC level
         rsup = rsup - av
         if (abs(rsup) .gt. abs(rinf)) then
            icent = isup
         else
            icent = iinf
         end if

*--- now find the chirp to the 'right'
         call arposmax(r(icent+iofc),nop-icent-iofc,
     &                 rinf,iinf,rsup,isup)
         rinf = rinf - av       ! remove DC level
         rsup = rsup - av
         if (abs(rsup) .gt. abs(rinf)) then
            ichrp = isup
         else
            ichrp = iinf
         end if
         ichrp = ichrp + icent + iofc - 1

*--- zero everything but the two largest chirp signals
         ibeg = 1
         iend = 2 * icent - ichrp - mw2
         do i=ibeg, iend
            r(i) = 0.0
         end do
         ibeg = 2 * icent - ichrp + mw2
         iend = ichrp - mw2
         do i=ibeg, iend
            r(i) = 0.0
         end do
         ibeg = ichrp + mw2
         iend = nop
         do i=ibeg, iend
            r(i) = 0.0
         end do

*--- and round the edges with cosine bells
         k = 2 * icent - ichrp - mw2
         do i=1,bw
            fac = 0.5 * (1.0 - cos(pi * i / bw))
            r(i+k) = fac * r(i+k)
         end do
         k = 2 * icent - ichrp + mw2 - bw
         do i=1,bw
            fac = 0.5 * (1.0 + cos(pi * i / bw))
            r(i+k) = fac * r(i+k)
         end do
         k = ichrp - mw2
         do i=1,bw
            fac = 0.5 * (1.0 - cos(pi * i / bw))
            r(i+k) = fac * r(i+k)
         end do
         k = ichrp + mw2 - bw
         do i=1,bw
            fac = 0.5 * (1.0 + cos(pi * i / bw))
            r(i+k) = fac * r(i+k)
         end do
         
      end if

      return

      end

* Revision history:
* -----------------
* $Log$
