c  \grammy\src\modify.for  Ver. 1.0   Amertiz,apodiz,aircor,zerocor,glitch

c Revision 1.07  07.07.93 New amertizing function corrected; u1 now 0.25
c Revision 1.06  31.10.92 New amertizing function installed
c Revision 1.05  01.09.91 Apodize default changed
c Revision 1.04  14.08.91 Delete handling of even orders here
c Revision 1.03  04.06.91 apodize using temporary parameter; center test fixed
c Revision 1.02  15.05.91 Glitch incorporated here; center print suppressed
c Revision 1.01  16.04.91 Mask moved up 1 pt
c Version  1.00  21.01.91  


      subroutine amrtiz(z,lnth,istrt)
c
c  amrtiz applies an amertizing function to onesided interferograms
c  The function is basically 0.5 from start to 2*imax, but with a
c  cubic transition zone of width u1 at both ends

c     z contains the current block
c     lnth contains the block length
c     istrt is the point number for the first word of the block

      include 'inparms.h'

      real z(*)

      if (istrt .gt. 2*imax) return
      u1 = 0.25

c x runs from -1. to 1.
      dx = 1.0/imax
      x0 = -1.0 + (istrt-1)*dx
      do j=1,lnth
         x = x0 + j*dx
         if (x .ge. 1.0) return
         if (x .lt. u1-1.0) then
            u = (x + 1.0)/u1
            z(j) = z(j)*u*u*(1.5 - u)
         elseif (x .lt. 1.0-u1) then
            z(j) = z(j)*0.5
         else
            u = (x - 1.0 + u1)/u1
            z(j) = z(j)*(0.5 + u*u*(1.5 - u))
         endif
      end do
 
      return
      end

c------------------------------------------------------------------------

      subroutine apodiz(z,lnth,istrt)

c if two-sided, apply cosine bell to 1st nrise points, and to
c               last (ending at 2*imax) nrise points
c if one-sided, apply only to last nrise points


c  z is a block of real-only data points.
c  lnth is the number of points in z.
c  istrt is the point number of the first point in z.

c  other pertinent parameters:
c  	imax is the point number of the scan maximum.
c  	rmax is the value of the scan maximum.
c  	rsum is the scan mean.
c  	noptex is the number of points in the extended scan.

      include 'inparms.h'
      include 'infmtn.h'
      include 'altinf.h'

      real z(*)

      nrise = 1024
      if (apodfl .or. apod1 .gt. 0.0) nrise = apod1

      dphi = 1.5707963/nrise
      nlast = ngot
      if (onesidfl) go to 10
      if (istrt .le. nrise) go to 100
      nlast = 2*imax
 10   nfall = nlast - nrise

c do nothing in center region
      j = istrt + lnth
      if (j .le. nfall) return

c we are in the fall region
      j = nlast - istrt
      ifst = 1
      if (j .gt. nrise) ifst = j - nrise
      do i=ifst,lnth
         x = j - i
         temp = 0.0
         if (x .gt. 0.0) temp = sin(dphi*x)
         z(i) = z(i) * temp * temp
      end do
      return

c we are in the rise region
 100  last = lnth
      if (istrt+lnth .gt. nrise) last = nrise - istrt
      do i=1,last
         x = i - 1 + istrt
         temp = sin(dphi*x)
         z(i) = z(i) * temp * temp
      end do
      return

      end

c------------------------------------------------------------------------

      subroutine aircor(delwp)

      include 'outparms.h'
      include 'infmtn.h'
      include 'altinf.h'
      
      double precision snext,rilasr,rindex,delwp
      external rindex

      rilasr = (1.0 + gcorr) * rindex(acorrp,acorrt,refwavno,acorrh)
      snext = wref + delwp*nop
      wref = wref * rilasr / rindex(acorrp,acorrt,wref,acorrh)
      snext = snext * rilasr / rindex(acorrp,acorrt,snext,acorrh)
      delw = (snext - wref) / nop

      return
      end

c--------------------------------------------------------------------

      subroutine zerocor(z,lnth,istrt)

c   zerocor corrects z by subtracting rsum from every value except
c   the zero values added to extend the array; those values are left
c   as zeroes.  rsum is the mean of the data actually read from the tape
c   ngot is the final number of points read from the tape.

c   z contains the current block
c   lnth is the block lnth
c   istrt is the point number of the first point in the current block.

      real z(1)

      include 'inparms.h'
      integer nc1r(10),nc2r(10)                                         
      real dcr(10)
      logical crflg(10)                                                 
      equivalence (nc1r(1),mfregn(1,1)), (nc2r(1),mfregn(1,2)),         
     *            (dcr(1),mfregn(1,3)), (crflg(1),mfregn(1,4))          

c  apply regional resets, if applicable

      if(nfregn .eq. 0) go to 18
      if(crflg(nfregn)) go to 15
      n1 = nc1r(nfregn) - istrt + 1
      if (n1 .lt. 1 .or. n1 .gt. lnth) go to 18
      n2 = min0(nc2r(nfregn)-istrt+1, lnth)
      zlast = z(n1)
      do j=n1,n2
         z(j) = zlast+dcr(nfregn)
         zlast=z(j)
      end do
      if(nc2r(nfregn) .le. istrt+lnth) go to 15
      nc1r(nfregn) = istrt + lnth + 1
      go to 18
 15   nfregn = nfregn - 1
      if(crflg(nfregn) .and. nfregn .gt. 0) go to 15

 18   npt = istrt
      if (npt . gt. ngot) return
      ln2 = min0(ngot-npt+1,lnth)

      do j=1,ln2
         z(j) = z(j) - rsum
      end do

      npt = npt + ln2
      return
      
      end

c--------------------------------------------------------------------

      subroutine glitch(r)

c test points in current block for glitches

	include 'inparms.h'
	include 'datetc.h'
	include 'iounit.h'
        
        real r(*)

	real dcr(10)
        double precision drms
	logical crflg(10)                                                 
	equivalence (dcr(1),mfregn(1,3)), (crflg(1),mfregn(1,4))          
	integer br(10),n1r(10),n2r(10),n3r(10),n4r(10),ncpr(10)           
	real c1r(10),c2r(10)                                              
	equivalence (br(1),mregn(1,1)),  (n1r(1),mregn(1,2)),             
     *            (n2r(1),mregn(1,3)), (n3r(1),mregn(1,4)),             
     *            (n4r(1),mregn(1,5)), (c1r(1),mregn(1,6)),             
     *            (c2r(1),mregn(1,7)), (ncpr(1),mregn(1,8))             


	data rold/0.0/
 
	nbad=0				! reset bad point counter each entry.
 
c   check for bad regions
	if(nregn .eq. 0)  go to 100

10	if(crflg(nregn)) go to 60
        if(br(nregn) .ne. curblk) go to 100
	if(n1r(nregn) .eq. 0) go to 30
        n1 = n1r(nregn)
        n2 = min0(n2r(nregn), blksiz)
        do i=n1,n2
	   c1r(nregn) = c1r(nregn) + r(i)
	end do

        if(n2r(nregn) .le. blksiz) go to 30
        br(nregn) = br(nregn) + 1
        n1r(nregn) = 1
        n2r(nregn) = n2r(nregn) - blksiz
        n3r(nregn) = n3r(nregn) - blksiz
        n4r(nregn) = n4r(nregn) - blksiz
        go to 100
30      n1r(nregn) = 0
        if(n2r(nregn) .eq. 0) go to 40
        n1 = n2r(nregn)
        n2 = min0(n3r(nregn), blksiz)
        temp = c1r(nregn)/ncpr(nregn)
        do i=n1,n2
	   r(i) = temp
	end do

        if(n3r(nregn) .le. blksiz) go to 40
        br(nregn) = br(nregn) + 1
        n2r(nregn) = 1
        n3r(nregn) = n3r(nregn) - blksiz
        n4r(nregn) = n4r(nregn) - blksiz
        go to 100

40	n2r(nregn) = 0
        if(n3r(nregn) .eq. 0) go to 100
        n1 = n3r(nregn)
        n2 = min0(n4r(nregn), blksiz)
        do i=n1,n2
	   c2r(nregn) = c2r(nregn) + r(i)
	end do

        if(n4r(nregn) .le. blksiz) go to 50
        br(nregn) = br(nregn) + 1
        n3r(nregn) = 1
        n4r(nregn) = n4r(nregn) - blksiz
        go to 100

50      dcr(nregn) = (c2r(nregn) - c1r(nregn) ) /
     *               (ncpr(nregn)*ncpr(nregn))
        nregn = nregn - 1
        go to 10

60      if(br(nregn) .ne. curblk) go to 100
        if(n1r(nregn) .eq. 0) go to 100
        n1 = n1r(nregn)
        n2 = min0(n2r(nregn), blksiz)
        do i=n1,n2
	   r(i) = c1r(nregn)
	end do

        if(n2r(nregn) .le. blksiz) go to 70
        br(nregn) = br(nregn) + 1
        n1r(nregn) = 1
        n2r(nregn) = n2r(nregn) - blksiz
        go to 100

70      n1r(nregn) = 0
        br(nregn) = 0
        nregn = nregn - 1
        go to 10

100	continue
 
c test points in current block for glitch
c xsigma, pts (nexmpt of them) in iexmpt from deglitch command
c irng from range command
c ncenter +- 20 points excluded
 
	nhere = ngot-(curblk-1)*blksiz
 
	drms = 0.0d0 				! compute rms and find peak
	do i=1,nhere
            drms = drms + dble(r(i))**2
	end do
	rms = real( dsqrt(drms/nhere) )
	norg = (curblk - 1)*blksiz

						! ************start of loop
	do 220 i=1,nhere
           npoint = i + norg
           if (incntr .ne. 0 .and. iabs(npoint-imax) .lt. 32) goto 220
           if (npoint .ge. irng1 .and. npoint .le. irng2) go to 220

           amp = r(i)
           if (abs(amp) .le. xsigma*rms) go to 220

           if (.not. deglch) go to 218

	   if (nexmpt .ne. 0) then
              do j=1,nexmpt
                 if (npoint .eq. iexmpt(j)) go to 220
              end do
	   end if

	   temp = r(i)
      	   r(i) = rold
      	   rsum=rsum-temp+r(i)
      	   write(wrtbuf,2101)npoint,r(i),temp
           call wrtout(output,wrtbuf)
      	   write(logfil,2101)npoint,r(i),temp
2101  	   format(' Point ',i6,' set to',f10.0,', old value was',
     &            f10.0)

      	   go to 219

218	   if (nbad .eq. 0) then
              write (wrtbuf,2111)
              call wrtstr(wrtbuf)
              write (logfil,2111)
 2111         format(
     &        ' Possible data glitch      at point     in block ')
	   end if
    	   write (wrtbuf,2105) amp,npoint,curblk
           call wrtstr(wrtbuf)
	   write(logfil,2105) amp,npoint,curblk
2105  	   format(1x,g18.8,i12,i12)
219   	   nbad = nbad + 1
      	   if(nbad.ge.300) then
              call wrtstr(' Warning :  More than 300 bad values.' )
           end if
220	rold = r(i)
						! ************end of loop

      	if (nbad .ne. 0) then
      	    write(wrtbuf,2102) curblk,rms
            call wrtout(output,wrtbuf)
	    write(logfil,2102) curblk,rms
2102	    format(' For block',i4,', RMS = ',f12.1)
	end if

	return
	end
