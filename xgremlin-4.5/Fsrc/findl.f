c \grammy\src\findl.for   Ver. 2.2

c  Revision 2.25 93/05/11  Use 'verbos' to control output
c  Revision 2.24 93/03/22  Allow pt > 9999 in printout
c  Revision 2.23 91/03/25  If no extremum, lnseek returns iparc=-1
c  Revision 2.22 89/03/02  Limit with correction to a factor of 3
c  Revision 2.21 89/02/22  Line limit message given; ESC test moved
c  Revision 2.2  89/02/05  

	subroutine findl(r,
     &                   point, amp, wd, dmp, 
     &                   eps1,eps2,eps3,eps4,eps5,
     &                   nit, nhold, ctag, dent)
 
	double precision  temp,wavel,sigma

	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'
	include 'linparms.h'
	include 'set.h'

        real r(*)
        real amp(*), wd(*), dmp(*)
	real eps1(*),eps2(*),eps3(*),eps4(*),eps5(*)
        integer nit(*), nhold(*)
        double precision point(*)
        character ctag(*)*4, dent(*)*32
 
	integer itrflag
	external itrflag

	call itron
	mode = 0
	if(ifx .ge. 1) mode = inum(1)
	call disper                      ! sets aps
	if (ifl .eq. 0) xnum(1) = 0.0

	if (verbos)  then
	   write (wrtbuf,4426) xnum(1)
	   call wrtstr(wrtbuf)
	end if
4426	format (' Rough parameters of lines stronger than ', g14.6 )

	if (verbos) then
	   i = mod(mode,10)
	   if (i .eq. 0) then
	      write(wrtbuf,4407)
     &	      ' average of line extremum and mean of inflexion points.'
	      call wrtout(output,wrtbuf)
	   end if
	   if (i .eq. 1) then 
               write(wrtbuf,4407)
     &	      ' Line centre determined by the extremum intensity.'
 	       call wrtout(output,wrtbuf)
	    end if
	    if (i .eq. 2) then
	       write(wrtbuf,4407)
     &	      ' Line centre determined by mean of inflexion points'
	       call wrtout(output,wrtbuf)
	    end if
	    i = 2*(mode/10)
	    if (i .ne. 0) then
	       write (wrtbuf,4408) i
	       call wrtout(output, wrtbuf)
	    end if
 4407	    format (' Line centre determined by ',a )
 4408	    format (' after convolution with a gaussian of width ',i3)

	    if (nwos.ne.-1) then
	       write (wrtbuf,4427)
	       call wrtout(output,wrtbuf)
	    end if
 4427	    format(9x,'point',6x,'lambda',7x,'int.',5x,'width(ma)',5x,
     &             'e.w.(ma)',6x,'wavenumber')
	    if (nwos.eq.-1) then 
	       write (wrtbuf,4428)
	       call wrtout(output,wrtbuf)
	    end if
 4428	    format (9x,'point',3x,'wavenumber',6x,'int.',5x,'width(mk)',
     &              5x,'e.w.(mk)',6x, 'wavelength')
 4431	    format (a10, i3, 2x,a20, 25x,a10,a10)
	 endif
	 ipara = 15
	 nol = 0
	 call linevis( 0 )	! flag line markers invisible
	 call llerase		! wipe out old plotter line list
	 
4435	iparc = 1
	iparb = mode
	ipara = ipara - 1
	call lnseek(r)
	if (iparb .eq. 0) then
	    write (wrtbuf,'(1x,i6,a)') nol,' lines found'
	    call wrtstr(wrtbuf)
	    call itroff
	    return
	endif
	call procpend
  	if (itrflag() .eq. 1) then
	   call itroff
	   write (wrtbuf,'(a)') ' Warning :  findlines interrupted.'
	   call wrtstr(wrtbuf)
	   return
	end if
 	if (xpara .lt. xnum(1)) go to 4435

	nol = nol + 1
	dmp(nol) = 3.50
	nit(nol) = 0
	nhold(nol) = 0
	ctag(nol) = '   F'
	eps1(nol) = 0.0
	eps2(nol) = 0.0
	eps3(nol) = 0.0
	eps4(nol) = 0.0
	eps5(nol) = 0.0
	dent(nol) = 'no id'
	temp = xparb
	point(nol) = xparb
	call ptow
	wavel = xparb
	if (nwos .ne. -1) call wtos
	if (nwos .eq. -1) call stow
	sigma = xparb
	amp(nol) = xpara          ! this does not seem to work well
	wd(nol) = xparc * 0.5
	xparc = 1000.0 * aps * xparc
	ew = 1.1086 * xpara * xparc

	if (verbos) then 
	   write (wrtbuf,4446) temp,wavel,xpara,xparc,ew,sigma
	   call wrtout(output,wrtbuf)
	end if
4446	format (4x,f11.3,f13.5,1x,g12.6,f10.2,1x,g12.6,f16.4)

	if (nol .lt. nlins) go to 4435

	call wrtstr(
     &      ' Warning :  line limit reached - exiting findlines')
	call itroff
	return
	end

c ----------------------------------------------------------------------
c  Revision 1.0  03/06/89

	subroutine lnseek(r)

	include 'datetc.h'
	include 'infmtn.h'

	real r(*)

	integer mode, ia, iflag, istep, ic, ib
	double precision ypp, yp, temp, yppt
	double precision yph, ypma, ypmb, ypph, a, b, del, xmsa
	double precision xmsb
	real secnd(36),secnh(36),frst(36),frsth(36)
	equivalence (s1,secnd(1)),(s2,secnd(2)),(s3,secnd(3))
	equivalence (s4,secnd(4)),(s5,secnd(5)),(s6,secnd(6))
	equivalence (s7,secnd(7)),(s8,secnd(8)),(s9,secnd(9))
	equivalence (s10,secnd(10)),(s11,secnd(11)),(s12,secnd(12))
	equivalence (sh1,secnh(1)),(sh2,secnh(2)),(sh3,secnh(3))
	equivalence (sh4,secnh(4)),(sh5,secnh(5)),(sh6,secnh(6))
	equivalence (sh7,secnh(7)),(sh8,secnh(8)),(sh9,secnh(9))
	equivalence (sh10,secnh(10)),(sh11,secnh(11)),(sh12,secnh(12))
	equivalence (f1,frst(1)),(f2,frst(2)),(f3,frst(3))
	equivalence (f4,frst(4)),(f5,frst(5)),(f6,frst(6))
	equivalence (f7,frst(7)),(f8,frst(8)),(f9,frst(9))
	equivalence (f10,frst(10)),(f11,frst(11)),(f12,frst(12))
	equivalence (fh1,frsth(1)),(fh2,frsth(2)),(fh3,frsth(3))
	equivalence (fh4,frsth(4)),(fh5,frsth(5)),(fh6,frsth(6))
	equivalence (fh7,frsth(7)),(fh8,frsth(8)),(fh9,frsth(9))
	equivalence (fh10,frsth(10)),(fh11,frsth(11)),(fh12,frsth(12))
	data secnd/ -3.116064388, +1.833333333, -0.352564103,
     *		+0.100732601, -0.030219780, +0.008461538,
     *		-0.002073906, +0.000423246, -0.000068221,
     *		+0.000008085, -0.000000624, +0.000000023,
     *		-.623204500, .103255800, .196426900,
     *		 .011212820, .001185478, -.000659688,
     *		 .000230102, -.000057708, .000008918,
     *		-.000000076, -.000000367, .000000098,
     *		-.081390880, -.044727570, .015721370,
     *		 .036264120, .023120410, .008197459,
     *		 .001824692, .000266763, .000026429,
     *		 .000001754, .000000086, -.000000005/
	data secnh/ -1.061837943,+1.550041071, -0.707179259,
     *  	+0.308139436, -0.120642206, +0.040782336, 
     *		-0.011525124, +0.002631744, -0.000464804, 
     *  	+0.000059445, -0.000004891, +0.000000194,
     *		-.359396600, .290612400, .069438250,
     *		-.003086207, .004315281, -.002815057,
     *		 .001250536, -.000389654, .000078123,
     *		-.000005899, -.000001800, .000000618,
     *		-.071172190, -.012177280, .032162680,
     *		 .031619610, .014646800, .004093257,
     *		 .000732086, .000086657, .000008246,
     *		 .000000180, -.000000086, -.000000023/
	data frst/ +0.923076923, -.362637363, +0.161172161, 
     *		-0.067994505, +0.025597931, -0.008295626,
     *  	+0.002245432, -0.000491188, +0.000083164, 
     *		-0.000010206, +0.000000807, -0.000000031,
     *		 .326075700, .082232390, .002411050,
     *		 .001121659, -.001122911, .000644337,
     *		-.000260116, .000074204, -.000013342,
     *		 .000000560, .000000200, -.000000208,
     *		 .068436140, .081406890, .051329460,
     *		 .020347420, .005350749, .000951982,
     *		 .000117108, .000010302, .000000336,
     *		 .000000000, .000000000, .000000000/
	data frsth/ 1.246995625, -0.117238905, +0.030147147, 
     *		-0.009228719, +0.002791403, -0.000769434,
     *  	+0.000183632, -0.000036297, +0.000005652, 
     *		-0.000000646, +0.000000048, -0.000000002,
     *		 .269656900, .208010600, .021035430,
     *		 .000042229, -.000148524, .000067240,
     *		-.000021498, .000004989, -.000000680,
     *		-.000000019, .000000035, -.000000007,
     *		 .038971915, .082673520, .068894560,
     *		 .034100655, .010961240, .002368202,
     *		 .000349819, .000035712, .000002518,
     *		 .000000133, .000000002, .000000001/

c The first section uses individual variables equivalenced to the first 12
c elements of the kernel arrays, and is somewhat faster (~11%) as a result;
c this speed is the same as the previous version using literals.
c New kernels may be found by convoluting the original kernels with a line
c shape of unit area
c ***loops are not yet in best order for precision****

c input starting location in ipara.  
c iparc=0 if line nearest ipara wanted,
c iparc=1 if next line wanted.  
c refined location returned in iparb - iparb=0 means out of range.  
c return parameters - strength in xpara, exact position in xparb, 
c                     width in xparc.  damping of 0.05 assumed.
c iparb = 2 for 2nd derivative determination, 1 for 1st deriv, 0 for average

	ia = ipara
	mode=iparb
	iparb = 0
	iflag = iparc
	istep = 1
	if (mode .gt. 2) go to 3900
c  seek correct region of negative 2nd derivative
3000	ia = ia + istep
	if ( (ia .lt.13) .or. (ia .gt. (nop-13)))return
        ypp = neoa*( s1*r(ia)
     *  +s2*(r(ia-1)+r(ia+1)) +s3*(r(ia-2)+r(ia+2))
     *  +s4*(r(ia-3)+r(ia+3)) +s5*(r(ia-4)+r(ia+4))
     *  +s6*(r(ia-5)+r(ia+5)) +s7*(r(ia-6)+r(ia+6))
     *  +s8*(r(ia-7)+r(ia+7)) +s9*(r(ia-8)+r(ia+8))
     *  +s10*(r(ia-9)+r(ia+9)) +s11*(r(ia-10)+r(ia+10))
     *  +s12*(r(ia-11)+r(ia+11)) )
	if (ypp .le. 0.0) go to 3040
c we are in a region of positive 2nd derivative
	iflag = 0
	if (iparc .ne. 0) go to 3000
c working on closest line - set direction from 1st derivative
	yp = neoa*( r(ia-2) -r(ia+2) +8.0*( r(ia+1) -r(ia-1)))/12.0
	if (yp .lt. 0.0) istep = -1
	go to 3000
3040	if (iflag .ne. 0) go to 3000
c we are in the correct region - locate inflexion points
c save current position and 2nd deriv. and start left
	ib = ia
	temp = ypp
	istep = -1
3050	ia = ia + istep
	yppt = ypp
	if ( (ia .lt.13) .or. (ia .gt. (nop-13)))return
        ypp = neoa*( s1*r(ia)
     *  +s2*(r(ia-1)+r(ia+1)) +s3*(r(ia-2)+r(ia+2))
     *  +s4*(r(ia-3)+r(ia+3)) +s5*(r(ia-4)+r(ia+4))
     *  +s6*(r(ia-5)+r(ia+5)) +s7*(r(ia-6)+r(ia+6))
     *  +s8*(r(ia-7)+r(ia+7)) +s9*(r(ia-8)+r(ia+8))
     *  +s10*(r(ia-9)+r(ia+9)) +s11*(r(ia-10)+r(ia+10))
     *  +s12*(r(ia-11)+r(ia+11)) )
	if (ypp .le. 0.0) go to 3050
c just passed a zero crossing - locate it
c divide step in two, do linear interpolation on 2nd deriv
	ic = ia
	if (istep .eq. 1) ic = ia - 1
        ypph = neoa*( sh1*(r(ic) +r(ic+1))
     *  +sh2*(r(ic-1)+r(ic+2)) +sh3*(r(ic-2)+r(ic+3))
     *  +sh4*(r(ic-3)+r(ic+4)) +sh5*(r(ic-4)+r(ic+5))
     *  +sh6*(r(ic-5)+r(ic+6)) +sh7*(r(ic-6)+r(ic+7))
     *  +sh8*(r(ic-7)+r(ic+8)) +sh9*(r(ic-8)+r(ic+9))
     *  +sh10*(r(ic-9)+r(ic+10)) +sh11*(r(ic-10)+r(ic+11))
     *  +sh12*(r(ic-11)+r(ic+12)) )
	a = 2.0*(yppt +ypp) -4.0*ypph
	b = 4.0*ypph - 3.0*yppt - ypp
	del = (b -sqrt(b*b -4.0*a*yppt))/(2.0*a)
c back up one step
3062	ia = ia - istep
	ypp = yppt
        yp  = neoa*(
     *  +f1*( r(ia+1)-r(ia-1)) +f2*(r(ia+2)-r(ia-2))
     *  +f3*( r(ia+3)-r(ia-3)) +f4*(r(ia+4)-r(ia-4))
     *  +f5*( r(ia+5)-r(ia-5)) +f6*(r(ia+6)-r(ia-6))
     *  +f7*( r(ia+7)-r(ia-7)) +f8*(r(ia+8)-r(ia-8))
     *  +f9*( r(ia+9)-r(ia-9)) +f10*(r(ia+10)-r(ia-10))
     *  +f11*( r(ia+11)-r(ia-11)) +f12*( r(ia+12)-r(ia-12)) )
	if (istep .eq. 1) go to 3080
c finish lower zero
	xmsa = ia + del
	ypma = yp + 0.5*del*ypp
c start right from earlier position
	ia = ib
	istep = 1
	ypp = temp
	go to 3050
c finish upper zero
3080	xmsb = ia - del
	ypmb = yp - 0.5*del*ypp
c combine for final parameters
	xpara = 0.42360*(ypma-ypmb)*(xmsb-xmsa)
	xparb = 0.5*(xmsa+xmsb)
	iparb = xparb
	xparc = 1.20060*(xmsb-xmsa)
	ipara = ia
	if (mode .eq. 2) return
c if line has a true minimum, use it to define wavelength (mode=1)
c flag no extremum condition with iparc = -1
	if ( (ypma*ypmb) .ge. 0.0) then
		iparc = -1
		return
	endif
	ia = xmsa
	ia = xmsa
	yp = ypma
c starting at lower inflexion point, seek crossing to negative 1st deriv
3084	ypmb = yp
	ia = ia + 1
	if((ia-xmsb) .gt. 0.0) return
        yp  = neoa*(
     *  +f1*( r(ia+1)-r(ia-1)) +f2*(r(ia+2)-r(ia-2))
     *  +f3*( r(ia+3)-r(ia-3)) +f4*(r(ia+4)-r(ia-4))
     *  +f5*( r(ia+5)-r(ia-5)) +f6*(r(ia+6)-r(ia-6))
     *  +f7*( r(ia+7)-r(ia-7)) +f8*(r(ia+8)-r(ia-8))
     *  +f9*( r(ia+9)-r(ia-9)) +f10*(r(ia+10)-r(ia-10))
     *  +f11*( r(ia+11)-r(ia-11)) +f12*( r(ia+12)-r(ia-12)) )
	if (yp .gt. 0.0) go to 3084
	temp = xparb
	ic = ia
        yph = neoa*(
     *   fh1*(r(ic)  - r(ic-1) ) +fh2*(r(ic+ 1)-r(ic- 2)) 
     *  +fh3*(r(ic+ 2)-r(ic- 3)) +fh4*(r(ic+ 3)-r(ic- 4))
     *  +fh5*(r(ic+ 4)-r(ic- 5)) +fh6*(r(ic+ 5)-r(ic- 6)) 
     *  +fh7*(r(ic+ 6)-r(ic- 7)) +fh8*(r(ic+ 7)-r(ic- 8)) 
     *  +fh9*(r(ic+ 8)-r(ic- 9)) +fh10*(r(ic+ 9)-r(ic-10)) 
     *  +fh11*(r(ic+10)-r(ic-11)) +fh12*(r(ic+11)-r(ic-12)) )
	ic = ia -1
	if (yph .gt. 0.0) go to 3090
c zero is in 1st half
	xparb = ic +0.5*ypmb/(ypmb - yph)
	go to 3092
c zero is in 2nd half
3090	xparb = ic +0.5 +0.5*yph/(yph - yp)
3092	if (mode .eq. 1) return
c calculate the average of the two positions
	xparb= (temp+xparb)/2.0
	return

c Filtered kernels used here; about 11% slower

3900	imd = 12*(mode/10)
	mode = mod(mode,10)
c  seek correct region of negative 2nd derivative
4000	ia = ia + istep
	if ( (ia .lt.13) .or. (ia .gt. (nop-13)))return
	ypp = secnd(imd+1)*r(ia)
	do 4010 i=1,11
	ypp = ypp + secnd(i+imd+1)*( r(ia-i) + r(ia+i) )
4010	continue
        ypp = neoa*ypp
	if (ypp .le. 0.0) go to 4040
c we are in a region of positive 2nd derivative
	iflag = 0
	if (iparc .ne. 0) go to 4000
c working on closest line - set direction from 1st derivative
	yp = neoa*( r(ia-2) -r(ia+2) +8.0*( r(ia+1) -r(ia-1)))/12.0
	if (yp .lt. 0.0) istep = -1
	go to 4000
4040	if (iflag .ne. 0) go to 4000
c we are in the correct region - locate inflexion points
c save current position and 2nd deriv. and start left
	ib = ia
	temp = ypp
	istep = -1
4050	ia = ia + istep
	yppt = ypp
	if ( (ia .lt.13) .or. (ia .gt. (nop-13)))return
	ypp = secnd(imd+1)*r(ia)
	do 4054 i=1,11
	ypp = ypp + secnd(i+imd+1)*( r(ia-i) + r(ia+i) )
4054	continue
        ypp = neoa*ypp
	if (ypp .le. 0.0) go to 4050
c just passed a zero crossing - locate it
c divide step in two, do linear interpolation on 2nd deriv
	ic = ia
	if (istep .eq. 1) ic = ia - 1
        ypph = 0.0
	do 4056 i=1,12
	ypph = ypph + secnh(i+imd)*( r(ic-i+1) + r(ic+i) )
4056	continue
        ypph = neoa*ypph
	a = 2.0*(yppt +ypp) -4.0*ypph
	b = 4.0*ypph - 3.0*yppt - ypp
	del = (b -sqrt(b*b -4.0*a*yppt))/(2.0*a)
c back up one step
4062	ia = ia - istep
	ypp = yppt
        yp = 0.0
	do 4066 i=1,12
	yp = yp + frst(i+imd)*( r(ia+i) - r(ia-i) )
4066	continue
        yp = neoa*yp
	if (istep .eq. 1) go to 4080
c finish lower zero
	xmsa = ia + del
	ypma = yp + 0.5*del*ypp
c start right from earlier position
	ia = ib
	istep = 1
	ypp = temp
	go to 4050
c finish upper zero
4080	xmsb = ia - del
	ypmb = yp - 0.5*del*ypp
c combine for final parameters
	xparc = 1.20060*(xmsb-xmsa)
	xpara = 0.352824*(ypma-ypmb)*xparc
	xparb = 0.5*(xmsa+xmsb)
	iparb = xparb
	ipara = ia
c make empirical width correction.
	wdc = 2.4
	if (imd .eq. 24) wdc = 10.0
	xpara = xpara*xparc
	temp = wdc/xparc
c limit width correction to a factor of 2
	if (temp .gt. 0.5*xparc) temp = 0.5*xparc
	xparc = xparc - temp
	xpara = xpara/xparc
	if (mode .eq. 2) return
c if line has a true minimum, use it to define wavelength (mode=1)
c flag no extremum condition with iparc = -1
	if ( (ypma*ypmb) .ge. 0.0) then
		iparc = -1
		return
	endif
	ia = xmsa
	ia = xmsa
	yp = ypma
c starting at lower inflexion point, seek crossing to negative 1st deriv
4084	ypmb = yp
	ia = ia + 1
	if((ia-xmsb) .gt. 0.0) return
        yp = 0.0
	do 4086 i=1,12
	yp = yp + frst(i+imd)*( r(ia+i) - r(ia-i) )
4086	continue
        yp = neoa*yp
	if (yp .gt. 0.0) go to 4084
	temp = xparb
	ic = ia
        yph = 0.0
	do 4088 i=1,12
	yph = yph + frsth(i+imd)*( r(ic+i-1) - r(ic-i) )
4088	continue
        yph = neoa*yph
	ic = ia -1
	if (yph .gt. 0.0) go to 4090
c zero is in 1st half
	xparb = ic +0.5*ypmb/(ypmb - yph)
	go to 4092
c zero is in 2nd half
4090	xparb = ic +0.5 +0.5*yph/(yph - yp)
4092	if (mode .eq. 1) return
c calculate the average of the two positions
	xparb= (temp+xparb)/2.0
	return
	end
