c  gremlin\src\fourier.for  Ver. 1.0

c  Revision 1.03  98.07.25   strtch: normalization across 32k bounday fixed
c  Revision 1.02  06.10.93   Tranmod: filter parameter handling changed
c  Revision 1.01  29.05.93   Tranmod uses low ffta for scratch; drop nq, etc
c  Version  1.0   10.04.91   Combined with tranmod

c determine maximum no. of points useable

	subroutine strtrn

	include 'datetc.h'
	include 'infmtn.h'

	do i=1,n2rtr
	   me = i
	   np = 2**i
	   if (np .ge. nop) go to 20
	end do

 20	if (np .gt. nop) then
	   me = me - 1
	   np = np/2
	endif
	nh = np/2
	nppt = np + 2

	return
	end

c ------------------------------------------------------------------

* scale and compute forward transform in r, output r
* if iamp = 1, leave amplitude and phase in tr

	subroutine fortrn(r,tr,iamp)

	include 'datetc.h'

	integer iamp
	real r(*), tr(*)

	call strtrn
	call ffak(r,np)

	if (iamp .ne. 0) call ampphs(r,tr)

	return
	end

c ------------------------------------------------------------------

	subroutine ampphs(r,tr)
*
* compute amplitude and phase of complex spectrum in r
* and put them into tr.
*
	include 'datetc.h'

	real r(*), tr(*)

	do i = 1, nh+1
	   ia = nh + i
	   tr(i) = sqrt(r(2*i-1)*r(2*i-1) + r(2*i)*r(2*i))
	   if (r(2*i-1) .ne. 0.0 .or. r(2*i) .ne. 0.0) then
	      tr(ia) = atan2(r(2*i),r(2*i-1)) / 6.283185307
	   else
	      tr(ia) = 0.0
	   end if
	end do

	return
	end

c ------------------------------------------------------------------

c compute inverse transform of the complex array in r, output in r

	subroutine invtrn(r)

	include 'datetc.h'

	real r(*)

   	call strtrn
	call ffsk(r,np)

   	return
	end

c ------------------------------------------------------------------

	subroutine pwrtrn(r,tr)

	include 'datetc.h'
	include 'infmtn.h'

	real r(*), tr(*)

	call fortrn(r,tr,0)
	call rext(r,tr)           ! move transform to tr

	nop = nh + 1
   	do i = 1, nop
	   r(i) =  tr(2*i-1)**2 + tr(2*i)**2
	end do

	nop = nop / 2
	nwpp = 1 
 
	return
	end
c ------------------------------------------------------------------

c ** this routine is used for filtering (iact=0), restoration (iact=1)
c ** and convolution (iact = -1).  
c ** The mean of the 1st and last nrise/2 points is subtracted before
c **  the mask is applied.

	subroutine trnmod(iact,r,tr,ffta)
 
	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'

	real r(*), tr(*), ffta(*)

c save contents of r(i) in ffta
	call rmove (nop,r(1),ffta(1))

c ** subtract mean of 1st and last nrise/2 points from data

 	xpara = 0.0
	ipara = inum(1)/2
  
	do i = 1,ipara
	   xpara = xpara - r(i) - r(nop-i+1)
	end do

	xpara = xpara/(2*ipara)
  
	do i=1,nop   
	   r(i) = r(i) + xpara
	end do
c					do transform of masked data
	call strtrn
 	iparb = nh
	iparc = inum(1)
	call mask(r)
	call fortrn(r,tr,1)
	call rext(r,tr)
c 
c ** if iact= 1 (restore), read in transform of instrument profile, 
c ** 	    normalize, fill r with 1/itran, but limit amplitude to 1000
c ** if iact= 0 (filter only), fill r with (1.0, 0.0)
c ** if iact=-1 (convolute), read in transform and normalize

	if (iact .eq. 0) then
	   do i=1,nppt,2
	      r(i) = 1.0
	      r(i+1) = 0.0
	   end do
	   go to 70
	end if

c - restoration or convolution - recall transform; zero nyquist point
c	call oscrread(inum(3))
	call rmove (np,ffta(nrtr+1),r(1))
	r(np+1) = 0.0
	r(nppt) = 0.0
	xpara = r(1)
	xparb = xpara*1.0e-6
  
c convolution - normalize transform 
	if (iact .eq. -1) then
	   do i=1,nppt
	      r(i) = r(i)/xpara
	   end do
	   write (wrtbuf, 8999) day, nrec
 8999	   format (' Convolution with ',a10, 'no. ',i3)
	end if
  
c - restoration  - fill with 1\tran, but limit
c      			remember 1/(x+iy) = (x-iy)/(x**2 + y**2)
	if (iact .eq. 1) then
	   do i = 1,nppt,2
	      temp = (r(i)*r(i) + r(i+1)*r(i+1))/xpara
	      if (temp .lt. xparb) temp = xparb
	      r(i) = r(i)/temp
	      r(i+1) = -r(i+1)/temp
	   end do
	   write (wrtbuf, 9000) day, nrec
 9000	   format (' Restoration with ',a10, 'no. ',i3)
	end if

70	continue
 
c ** multiply r (complex) by filter up to sharp cutoff
c ** skip cutoff if voigt optimum 
 
	if (alpha(1)(1:3) .eq. 'voi') go to 100
c not voigt - do sharp cutoff
	ipara = 2*inum(2)
	do i=ipara+1,nppt
	   r(i) = 0.0
	end do

c if only sharp cutoff filter, no further action needed here
	if (nstrng .eq. 0) then
	    write (wrtbuf, 9001) inum(2)
9001	    format (' Sharp cut-off filter, ncut =',i5)
	    go to 150
	endif
  
	if (alpha(1)(1:3) .eq. 'gau') go to 80
	if (alpha(1)(1:3) .eq. 'box') go to 90
	call wrtstr(' Error :  illegal filter type,')
	call wrtstr('          Must be voigt or gaussian or boxcar')
	return

c gaussian filter
80	write (wrtbuf, 9002) xnum(1), inum(2)
9002	format (' Gaussian filter, width =',f8.3,', ncut =',i5)
	do i=1,ipara,2
	   temp = (i-1)*xnum(1)
	   temp = 0.94342*temp/np
	   if (temp .lt. 9.3) then
	      temp =  exp(-temp*temp)
	   else
	      temp = 0.0
	   endif
	   r(i) = r(i)*temp
	   r(i+1) = r(i+1)*temp
	end do
	go to 150

c boxcar filter
90	write (wrtbuf, 9003) xnum(1), inum(2)
9003	format (' Boxcar filter, width =',f8.3,', ncut =',i5)
	xxx = 3.141592654*xnum(1)/(2*np)
	do i=3,ipara,2
	   temp = (i-1)*xxx
	   temp =  sin(temp)/temp
	   r(i) = r(i)*temp
	   r(i+1) = r(i+1)*temp
	end do
	go to 150

c voigt optimum filter
c ** guess at width, fit amplitude of voigt filter halfway to ncut

100	xparb = 1.5/inum(2)
	ipara = inum(2)
	temp = 0.0
	xpara = 0.0
  
	do i=2,ipara,2
	   s = xparb*i
	   s = -s*(1.233 + s)
	   xpara = xpara + (tr(i+1)*tr(i+1) + tr(i+2)*tr(i+2))*exp(s)
	   temp = temp + exp(2.0*s)
	end do
	xpara = xpara/(temp*xnum(1)*xnum(1))
c					adjust width to fit corner
	xparb = (sqrt(dlog(xpara) + 3.800d-1) - 0.6165)/inum(2)
	temp = np*xparb/2.0384
	write (wrtbuf, 9004) temp
9004	format (' Voigt optimum filter, voigt width',f6.1)

	do i=1,nppt,2
	   s = i/2
	   s = xparb*s
	   temp = 0.0
	   s = s*(1.233 + s)
	   if (s .lt. 100.0) temp = (xpara + 1.0)/(xpara + exp(s))
	   r(i) = r(i)*temp
	   r(i+1) = r(i+1)*temp
	end do

c				apply (filter - 1.0) to data transform
150	do i=1,nppt,2
	   r(i) = r(i) - 1.0
	   temp = tr(i)*r(i) - tr(i+1)*r(i+1)
	   tr(i+1) = tr(i)*r(i+1) + tr(i+1)*r(i)
	   tr(i) = temp
	end do

c ** do inverse transform to find correction to original data
c ** recall original and correct

	call rext(r,tr)
	call invtrn(r)
	call rext(r,tr)
	call rmove (nop,ffta(1),r(1))
	do i=1,nop
	   r(i) = r(i) + tr(i)
	end do

	call wrtstr(wrtbuf)
	return
  
	end
c ------------------------------------------------------------------

c  kernel     stretch kernel function in memory to nop points, centran,
c  ******     and save in 2nd quarter of ffta.  call - kernel  nop

	subroutine kernel(r,tr)

	include 'datetc.h'
	include 'infmtn.h'

	real r(*), tr(*)
c			apply a mask, rise width iparc, half width iparb
	iparb = nh
	iparc = np/10
	nop = inum(1)
 	call strtrn
	call mask(r)
	call momnts(r)
 	call fortrn(r,tr,0)
	xpara = nh + 1 - cgr
	call shiftr(r)
 	call invtrn(r)
	return
	end

c ------------------------------------------------------------------

c stretch	stretch data a factor of ns by Fourier interpolation

	subroutine strtch(r,tr)

	include 'datetc.h'
	include 'infmtn.h'

	real r(*), tr(*)

        ns = 2
        if (ifx .eq. 1) ns = inum(1)
        
        call fortrn(r,tr,0)
c                               set stretching to maximum number of points
        do i=nppt,nrtr+2
	   r(i) = 0.0
        end do
        r(np+1) = 0.5 * r(np+1)
	do i=1,np+1
	   r(i) = r(i) * ns
	end do 

        nop = ns*np
        call invtrn(r)

        delw = delw/ns
        call disper

        return
        end

c ------------------------------------------------------------------

c shift        shift data in r(i), assumed periodic.
c *****        calls - shift  distance  , shift  nsteps. 
c		use integer when possible

	subroutine shft(r,tr)

	include 'datetc.h'
	include 'infmtn.h'

	real r(*), tr(*)

	if (ifl .ne. 0) then
 	    call fortrn(r,tr,0)
	    xpara = xnum(1)
	    call shiftr(r)
 	    call invtrn(r)
	    return
	endif

c only integer shift needed
	ia = inum(1)/nop
	ia = inum(1) - nop*ia
	if (ia .eq. 0) return
	if (ia .lt. 0) ia = ia + nop
	do i=1,nop        ! shifted array in tr
	  j = i + ia
	  if (j .gt. nop) j = j - nop
	  tr(j) = r(i)
	enddo

	call rext(r,tr)   ! exchange r and tr
	return

	end
