c \gremlin\src\read.for   Ver. 2.2

c  Revision 2.29  93/06/22  Merged with updated Decomp version
c  Revision 2.28  92/05/22  stretched read added; use 'set stretch' first
c  Revision 2.27  91/11/09  'read wavelength' added; use modew as flag
c  Revision 2.26  91/08/23  After byte swapping, store again as integer
c  Revision 2.25  91/05/22  delw handling updated for nwpp=2
c  Revision 2.24  90/02/17  use nrtr+2, not 4  
c  Revision 2.23  90/01/19  Long integer read added; bocode=3 added
c  Revision 1.05  05/30/93  nwpp handling updated
c  Revision 1.04  05/16/93  Fix bug in npt truncation
c  Revision 1.02  05/07/93  Fix bug in altdata reads
c  Revision 1.01  03/27/93  Allow read of 2*nrtr
   
c read wstart [wstop] ["unit name"]
c read wstart npts ["unit name"]
c The following commands set flags, but do not actually read:
c read "offset"  noffset
c read "wavelength"  	(modew   = 1)
c read "wavenumber"  	(modew   =-1)
  
c     wstart(wstop) -  starting(stopping)  wavenumber(length)
c     npts -  the number of points to read, max of 2*nrtr (131072)
c     "unit name" - GRAMMY unit name
c     noffset is in bytes; brev causes byte reversal
c     "integer" allows the reading of files of binary integers
c     A read with no unit specified defaults to the "datain" unit
c     Valid names are: datain, dataout and altdata.
c     These commands read a chunk of data starting
c     at the wavenumber(length) you specify.
 
      subroutine read(r,tr)

      integer nbpw
      parameter (nbpw = 4)  ! 4 bytes per float, now fixed

      include 'datetc.h'
      include 'infmtn.h'
      include 'iounit.h'
      include 'linparms.h'
      include 'set.h'
      include 'temp.h'

      real r(*),tr(*)

      integer npts, i, modew
      double precision wstrt, wstp
      integer maxpts
      integer noffset, nerr

      noffset = 0
      modew = 0

* flag that r contains new data
      lfirst = .true.

c set failure flag, test syntax
      ipara = 1
      if(ifx .gt. 1 .or. ifl .gt. 2) go to 9995
c test for special read flags
      if (nstrng .eq. 1 .and. ifl .eq. 0) go to 1000
c find unit
      lu = datain
      if(nstrng .gt. 0) lu = ionum(alpha(1)(1:nalpha(1)),nalpha(1))
      if (lu .eq. -1) lu = datain
c 			error if not proper unit or unit not open
      if (.not.(lu .eq. datain .or. lu .eq. dataot .or.
     &           lu .eq. altdat )) go to 9997
      if (uopen(lu) .eq. 0) go to 9998

c check for wavelength or wavenumber ranges
      if (modew .gt. 0) then
         if (ifl .eq. 1) then
            xparb = xnum(1)
            call wtos
            xnum(1) = xparb
         endif
         if (ifl .eq. 2) then
            xparb = xnum(2)
            call wtos
            xnum(2) = xparb
         endif
      endif

c recall original delw, nwpp; use fboffs (file byte offset) and 
c bocode (byte order code) if they appear in .hdr file
      delw = disp 
      nwpp = nwppt
      noffset = fboffs
c find number of bytes to read (default to 2k points)
      maxpts = nrtr + 2
      npts = maxpts                 ! read as much as possible by default
      if (ifx .eq. 1) npts = inum(1)
      if (ifl .eq. 2) npts = nwpp*(1 + (xnum(2)-xnum(1))/delw)
      if ( npts .lt. 1 ) then
         call wrtstr(
     &   ' Error :  invalid wavelength range or # of points')
         return
      end if
      if (npts .gt. maxpts) then
	 call wrtstr(
     &   ' Warning :  requested # of points exceeds capacity of r.')
         npts = nrtr + 2
         write (wrtbuf,111) npts
 111     format(' --- # of points reduced to ',i8)
         call wrtstr(wrtbuf)
      endif
 
c find start of read
      wstrt = xnum(1)
      if (rdref .eq. 1) wstrt = wstrt - 0.5*delw*npts/nwpp
      if (wstrt .lt. wstart) wstrt = wstart
      if (wstrt .ge. wstop) go to 9996
      nstart = nwpp*(wstrt - wstart)/delw
      if (nwpp.eq.2 .and. mod(nstart,2).ne.0) nstart = nstart + 1
      wstrt = wstart + delw*nstart/nwpp
      nbstart = nbpw*nstart + noffset
c adjust end point if necessary
      wstp = wstrt + delw*(npts-1)/nwpp
      if (wstp .gt. wstop) then
         npts = 0.5 + nwpp * (1.0 + (wstop - wstrt)/delw)
         wstp = wstrt + delw*(npts-1)/nwpp
      end if
      nbytes = nbpw*npts

* store the absolute point in the file where the buffer starts
      nrefp = nbstart / (nwpp * nbpw)

      call readata(lu, nbstart, r(1), nbytes,nerr)
      if (nerr .ne. 0) go to 9999
      wref = wstrt
      nop = npts
      
* if requested, change the byte order.  
      if ( bocode .ne. mbocode ) then
         call chibord( r, nop )
      end if

c check length for fft routines
      call strtrn
      if (warn .and. (2**me) .ne. nop)
     &	   call wrtout(output,' Warning :  length is not a power of 2')
      call disper

c either scale (using rdfctr from set)
      if ( .not.rdnorm .and. rdscale.gt.0 ) then
         do i=1,nop
            r(i) = r(i)*rdfctr
         end do
         scal = 1.0
         if (verbos) then
            write (wrtbuf,9011) wref
            call wrtout(output,wrtbuf)
 9011       format(' Wref = ',f15.5)
         end if
      else
c or normalize 
         absmax = 0.0
         do i=1,nop
            tmp = abs(r(i))
            if (tmp .gt. absmax) absmax = tmp
         end do
         scal = absmax
         do i=1,nop
            r(i) = r(i)/scal
         end do
         if (absmax.lt.100.0 .and. verbos) then
            write(wrtbuf,9006) wref, absmax
            call wrtout(output,wrtbuf)
         end if
 9006    format(' Wref = ',f15.5,' Absmax = ',f15.7)
         if (absmax.ge.100.0 .and. verbos) then
            write(wrtbuf,9007) wref, absmax
            call wrtout(output,wrtbuf)
         end if
 9007    format(' Wref = ',f15.5,' Absmax = ',f15.0)
      end if

c     if "stretch" has been set, do it:
      if (istret .ge. 2) then
         inum(1) = istret
         ifx = 1
         call strtch(r,tr)
      endif

* zap old line list in buffer
      nol = 0
      nlgot = 0
      ipara = 0
      call linevis( 0 )         ! flag line markers invisible
      call llerase              ! wipe out old plotter line list

c	write (*,*) 'nwpp=',nwpp,' irvmode=',irvmode,' istret=',istret
c	write (*,*) 'nbpw=',nbpw,' noffset=',noffset,' delw=',delw
c	write (*,*) 'npts=',npts,' nbstart=',nbstart,' nbytes=',nbytes
c	write (*,*) ' rdnorm=',rdnorm

      return
      
c Set special read flags.  Noffset is in bytes
1000  continue	
      if (alpha(1)(1:6)  .eq. 'offset') noffset = inum(1)
      if (alpha(1)(1:10) .eq. 'wavelength') modew = 1
      if (alpha(1)(1:10) .eq. 'wavenumber') modew = -1
      return
        
c error returns
9995  call wrtstr(' Error :  READ syntax error')
      return
9996  call wrtstr(' Error :  requested data outside window--no read ')
      return
9997  call wrtstr(' Error :  unacceptable I/O Unit for READ')
      return
9998  call wrtstr(' Error :  Specified unit not open')
      return
9999  call wrtstr(' Error :  read failed')
      return
      end
