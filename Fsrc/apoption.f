c \gremlin\src\apoption.for   Ver. 2.2 - Old (latest, maybe) apodisation option

c  Revision 2.34 93/10/06  number of lobes tested up to 20
c  Revision 2.34 91/03/26  DC boundary added
c  Revision 2.33 91/03/25  number of lobes tested increased
c  Revision 2.32 91/03/24  makeils dropped; ibf's in tr
c  Revision 2.32 91/03/24  auto allows dw=0; take dc3 to 3.0e-6
c  Revision 2.31 91/03/22  7th term added to series
c  Revision 2.30 91/03/21  gs print shortened
c  Revision 2.29 91/03/20  findextr skips if not extremum
c  Revision 2.28 91/03/19  option auto  output format changed; bound added
c  Revision 2.27 91/03/18  dc3 test tightened to 0.00001 in auto; c30 optional
c  Revision 2.26 91/03/17  ILS functions shortened; 6th function added
c  Revision 2.25 91/03/16  auto, b (bumps for an existing line) added
c  Revision 2.23 91/03/14  bumps, gs, nb added; generalized

        subroutine apoption(r,tr)

	include 'datetc.h'
	include 'infmtn.h'
	include 'altinf.h'
	include 'iounit.h'
	include 'set.h'

        real r(*), tr(*)

	integer itrflag
	character*80 chtmp
	character*37 name
        integer*4  ibf(3)
	real x,b(6)
	real pi,c10,c13,c20,c23,a(6)
	real xm(20),ym(20)
        double precision alph, z0,dpi,xx,wtemp

        external itrflag

	data pi/3.141592654d0/,name/'                    '/
	data dpi/3.141592654d0/

        if (nalpha(1) .eq. 0) go to 998
        if (alpha(1)(1:nalpha(1)) .eq. 'dolph') go to 8000
        if (alpha(1)(1:nalpha(1)) .eq. 'bound') go to 7000
        if (alpha(1)(1:nalpha(1)) .eq. 'auto') go to 6000
        if (alpha(1)(1:nalpha(1)) .eq. 'b') go to 5000
        if (alpha(1)(1:2) .eq. 'bu') go to 4000
        if (alpha(1)(1:nalpha(1)) .eq. 'gs') go to 3000
        if (alpha(1)(1:nalpha(1)) .eq. 'series') go to 2000

 998    call wrtstr(' Synatax error in apodize command.')
        call wrtstr(' Specified option not implemented; try')
        call wrtstr('    b, bumps, gs, series, auto, bound, dolph')
        return

c apodize dolph ripple  - Calculate the ILS for
c ********	Dolph-Chebyshev windows

8000	continue
	if (ifl .lt. 1 .or. ifx .ne. 0) then
	    call wrtstr(' apodize dolph ripple')
	    return
	endif
	nop = 2048
	alph = 1.0/xnum(1)
	z0 = alph + dsqrt(alph*alph - 1.0)
	z0 = dlog(z0)/nop
	z0 = 0.5d0*(dexp(z0) + dexp(-z0))
	do i=1,1025
	   xx = z0*dcos( (dpi*(i-1))/2048.d0)
           if (xx .ge. 1.0d0) then
              wtemp = 2048.0d0*dlog(xx + dsqrt(xx*xx -1.0d0))
              r(i+1024) = 0.5d0*(dexp(wtemp) + dexp(-wtemp))/alph
 	   else    
              wtemp = 2048.0d0*dacos(xx)
              r(i+1024) = dcos(wtemp)/alph
 	   end if
           w=wtemp
	   r(1026 -i) = r(1024 + i)
	end do
	return

c apodize auto n1 n2 n3 w1 w2 dw [c30] [name] - Minimize the peak for 
c ********	the named series at a given set of widths 

6000	continue
        call itron  ! make it interruptible
	if (ifl .lt. 3 .or. ifx .ne. 3) then
           call wrtstr(' apodize auto n1 n2 n3 w1 w2 dw [c30] [name]')
           return
	endif
	if (nstrng .eq. 2) then
           nch = nalpha(2)
           if (nch .gt. 37) nch = 37
           name = alpha(2)(1:nch)
	else
           call oscrread(12)
           name = id(1:37)
	endif
	ibf(1) = inum(1)
	ibf(2) = inum(2)
	ibf(3) = inum(3)
c copy individual ILS's to tr
        call oscrread(ibf(3)+10)
        do i=1,2048
           tr(i+4096) = r(i)
        end do
        call oscrread(ibf(2)+10)
        do i=1,2048
           tr(i+2048) = r(i) 
        end do
        call oscrread(ibf(1)+10)
        do i=1,2048
           tr(i) = r(i)
        end do

	w1 = xnum(1)
	w2 = xnum(2)
	dw = xnum(3)
	n = 1
	if (dw .ne. 0.0) n = 1 + (w2 + 1.0e-7 - w1)/dw
	w = w1 - dw
	c30 = 0.0
	if (ifl .eq. 4) c30 = xnum(4)
	dc3 = 1.0
	c3 = c30 - dc3
	write (logfil,6801) 
6801	format('    W      W/Wo      ymax   bound      C1        C2',
     *         '        C3      lnx   logy   %    name')
c start width loop
	do 6900 ii=1,n
           w = w + dw
           x = 9.65367308d0*w/16.0
           c3 = c3 + dc3
           call calcoefs(x,ibf,a,b,c10,c13,c20,c23,r)
           c2 = c20 + c23*c3
           c1 = c10 + c13*c3
c look for minimum for this width and c3.  Get starting value

c make ILS, find largest of first 20 extrema - ytst
           do i=1,2048
              r(i) = c1*tr(i) + c2*tr(i+2048) + c3*tr(i+4096)
           end do

           call findextr(xm,ym,r)
           ytst = 0.0
           do i=1,20
              if (abs(ym(i)) .gt. ytst) then
                 ytst = abs(ym(i))
                 ymax = ym(i)
              endif
           enddo
           wtst0 = ytst
c here we go - vary c3 to find minumum wtst for this width.  New c3:
 6500      c3 = c3 + dc3
           c2 = c20 + c23*c3
           c1 = c10 + c13*c3
c make ILS
           do i=1,2048
              r(i) = c1*tr(i) + c2*tr(i+2048) + c3*tr(i+4096)
           end do

           call findextr(xm,ym,r)
           ytst = 0.0
           do i=1,20
              if (abs(ym(i)) .gt. ytst) then
                 ytst = abs(ym(i))
                 ymax = ym(i)
              endif
           enddo
           wtst = ytst
c	    ndc3 = 1.0e6*dc3 + 0.5
c	    nc3 = 1.0e6*c3 + 0.5
c	    nym = 1.0e7*ymax + 0.5
c	    nts = 1.0e7*wtst0 + 0.5
c	    write (wrtbuf,6101) ndc3,nc3,nym,nts
c6101   	    format (' 10**6 dc3,c3,10**7ymax,wtst0 = ',4i9)
*           call wrtstr( wrtbuf )
c decide on size and direction of step
           if (wtst .lt. wtst0) then
              wtst0 = wtst
              ymax0 = ymax
              c3t0 = c3
	    else
               c3 = c3 - dc3
               dc3 = -0.5*dc3
               if (abs(dc3) .lt. 1.0e-6) go to 6800
	    endif
            call procpend
  	    if (itrflag() .eq. 1) then
               call itroff
               return
            end if
            go to 6500

 6800       continue
            wtst = wtst0
            ymax = ymax0
            c3 = c3t0

            wt = w*1.206709
            bound = boundary(w)
            xlog = log(wt)
            ylog10 = log10(sqrt(ymax*ymax))
            write (wrtbuf,6803)wt,w,ymax,bound,c1,c2,c3,xlog,ylog10,name
            call wrtstr( wrtbuf )
            write (logfil,6803)wt,w,ymax,bound,c1,c2,c3,xlog,ylog10,name
 6803       format (f9.5,f7.4,f10.7,f8.6,3f10.5,f6.4,f8.4,' % '
     &              ,a37)

            dc3 = c3 - c30
            c30 = c3
 6900    continue
         call itroff
         return

c apodize b - Calculate width and peak and position of bumps of the line in r
c ********

c normalize, find halfwidth 
5000	call findhalf(wout,r)

5801	format ('ILS width=',f10.6,'  W/Wo=',f10.6)
        call wrtstr(' ')
	write (wrtbuf,5801) wout,wout/1.206709
        call wrtstr( wrtbuf )
        call wrtstr(' ')
        write (logfil,*) ' '
	write (logfil,5801) wout,wout/1.206709
        write (logfil,*) ' '
	
	mode = 2
	ifl = 1

c apodize bu[mps] - Calculate width and peak and position of bumps
c **************

4000	if (ifl .eq. 0) then
	   call wrtstr(' apodize bumps C31 [dC3] [C32]')
           return
	endif
        call itron  ! make it interruptible
        c31 = xnum(1)
        n = 1
        dc3 = 0.
        if (ifl .ge. 2) then
           dc3 = xnum(2)
           n = 100
	endif
        if (ifl .eq. 3) then
           c32 = xnum(3)
           n = 1 + (c32 + 1.0e-7 - c31)/dc3
	endif
	c3 = c31 - dc3

c start loop
4100	do 4900 ia = 1,n
	
	if (mode .eq. 2) go to 4400

c make ILS
	    c3 = c3 + dc3
	    c2 = c20 + c23*c3
	    c1 = c10 + c13*c3
	    call oscrread(ibf(3)+10)
	    call rext(r,tr)
	    call oscrread(ibf(2)+10)
	    do i=1,2048
		tr(i) = c2*r(i) + c3*tr(i)
	    end do
	    call oscrread(ibf(1)+10)
	    do i=1,2048
		r(i) = c1*r(i) + tr(i)
	    end do

c normalize, find halfwidth 
	    call findhalf(wout,r)
4400	    delw = 2.0

c find first 20 extrema
	    call findextr(xm,ym,r)

 4802       format (' max no.   position       int.   ',
     &              ' max no.   position       int.')
 4803       format( (i4,f15.5,f13.7,i4,f15.5,f13.7) )
	    write (wrtbuf,4802)
            call wrtstr(wrtbuf)
	    write (wrtbuf,4803) (i,xm(i),ym(i),i=1,20)
            call wrtstr( wrtbuf )
	    write (wrtbuf,4802)
	    write (logfil,4803) (i,xm(i),ym(i),i=1,20)
	    ytst = 0.0
	    do i=1,20
               if (abs(ym(i)) .gt. ytst) then
                  ytst = abs(ym(i))
                  ymax = ym(i)
               endif
	    enddo
	    write (wrtbuf,4805) c1,c2,c3,ymax
            call wrtstr( wrtbuf )
	    write (logfil,4805) c1,c2,c3,ymax
4805	    format (' For C1,C2,C3 = ',3f12.6,', ymax = ',f13.7)
            call procpend
  	    if (itrflag() .eq. 1) then
               call itroff
               return
            end if
	    if (mode .eq. 2) then
               ylog = log10(abs(ymax))
               if (wout .le. 0.0) return
               wlog = log(wout)
               write(wrtbuf,4807) wlog,ylog
               call wrtstr(wrtbuf)
               write(logfil,4807) wlog,ylog
 4807          format (f10.6,f12.6)
	    endif

4900	continue
        call itroff
	return


c apodize gs w1 [auto  w2 dw] - Calculate Gaussian ILS functions
c ***********

3000	if (ifl .eq. 0) then
           call wrtstr(' apodize gs w1 [auto w2 dw]')
           return
	endif
        w1 = xnum(1)
        if (alpha(2)(1:nalpha(2)) .eq. 'auto') then
           w2 = xnum(2)
           dw = xnum(3)
           n = (w2 + dw - w1)/dw
	else
           n = 1
           dw = 0.
	endif
	w = w1

c start loop
3100	do 3900 ia = 1,n
	
	nop = 8192
	call strtrn
	delw = 1.0
	wref = 1.0e-20
	do i=1,8192
		r(i) = 0.
	end do
	gln2 = log(2.0)*4.0
	do i=3585,4609
		x = (i - 4097)/w
		r(i) = exp(-x*x*gln2)
	end do
	r(3585) = 0.5*r(3585)
	r(4609) = 0.5*r(4609)
	id = 'Gaussian apodising function'
	call oscrwrite(0)
c do in-place transform; keep real; make into a symmetric line
	call fast(r,np)
	do i=1,4096
           r(8193 - i) = r(8193 - 2*i)
	end do
	do i=1,1024
           r(1024 + i) = r(4096 + i)
           r(1025 - i) = r(4097 + i)
	end do
	nop = 2048

c normalize, find halfwidth 
	call findhalf(wout,r)
	delw = 2.0
c find first 20 extrema
	call findextr(xm,ym,r)

3801	format (' Width=',f12.3,'  ILS width=',f10.6,
     &          '  W/Wo=',f10.6)
	write (wrtbuf,3801) w,wout,wout/1.206709
        call wrtstr(wrtbuf)
	write (logfil,3801) w,wout,wout/1.206709
	write (wrtbuf,4803) (i,xm(i),ym(i),i=1,20)
        call wrtstr(wrtbuf)
 	write (logfil,4803) (i,xm(i),ym(i),i=1,20)

	w = w + dw
3900	continue
	return

c apodize series <name> [0] - Calculate the the basis functions of the 
c *************		named series in scratch 11-15 (unless 0 added)
c apodize series n1 n2 n3 w - Calculate the coefficients for the ILS 
c *************		functions of the named series at w

2000	if (nstrng .eq. 1 ) go to 2100

c initialize
	nstrng = 0
	    nop = 8192
	    call strtrn
	    delw = 1.0
	    wref = 1.0e-20
	    do i=1,8192
		r(i) = 0.
	    end do

c make the terms in the apodising function, save in 1-7
        if (alpha(2)(1:nalpha(2)) .eq. 'norton') then
	    name = 'norton'
	    call wrtstr(' Calculating Norton ')
	    do i=3585,4609
               r(i) = 1.0 
	    end do
	    r(3585) = 0.5
	    r(4609) = 0.5
	    id = 'Boxcar           '
	    call oscrwrite(1)
	    do i=3585,4609
               x = (i - 4097)/512.0
               r(i) = 1.0 - x*x
	    end do
	    id = 'Parabolic arch**1'
	    call oscrwrite(2)
	    do i=3585,4609
               x = (i - 4097)/512.0
               r(i) = (1.0 - x*x)**2
	    end do
	    id = 'Parabolic arch**2'
	    call oscrwrite(3)
	    do i=3585,4609
		x = (i - 4097)/512.0
		r(i) = (1.0 - x*x)**3
	    end do
	    id = 'Parabolic arch**3'
	    call oscrwrite(4)
	    do i=3585,4609
		x = (i - 4097)/512.0
		r(i) = (1.0 - x*x)**4
	    end do
	    id = 'Parabolic arch**4'
	    call oscrwrite(5)
	    do i=3585,4609
		x = (i - 4097)/512.0
		r(i) = (1.0 - x*x)**5
	    end do
	    id = 'Parabolic arch**5'
	    call oscrwrite(6)
	    do i=3585,4609
		x = (i - 4097)/512.0
		r(i) = (1.0 - x*x)**6
	    end do
	    id = 'Parabolic arch**6'
	    call oscrwrite(7)
	endif
        if (alpha(2)(1:nalpha(2)) .eq. 'filler') then
	    name = 'filler'
	    call wrtstr(' Calculating modified Filler ')
	    do i=3585,4609
		r(i) = 1.0 
	    end do
	    r(3585) = 0.5
	    r(4609) = 0.5
	    id = 'Boxcar           '
	    call oscrwrite(1)
	    do i=3585,4609
		x = (i - 4097)/512.0
		r(i) = cos(0.5*pi*x)
	    end do
	    id = 'cos Pi/2'
	    call oscrwrite(2)
	    do i=3585,4609
		x = (i - 4097)/512.0
		r(i) = 0.5*(cos(pi*x) + 1.0)
	    end do
	    id = 'cos Pi + 1'
	    call oscrwrite(3)
	    do i=3585,4609
		x = (i - 4097)/512.0
		r(i) = cos(1.5*pi*x)
	    end do
	    id = 'cos 3Pi/2'
	    call oscrwrite(4)
	    do i=3585,4609
		x = (i - 4097)/512.0
		r(i) = 0.5*(cos(2.0*pi*x) - 1.0)
	    end do
	    id = 'cos 2Pi - 1'
	    call oscrwrite(5)
	    do i=3585,4609
		x = (i - 4097)/512.0
		r(i) = cos(2.5*pi*x)
	    end do
	    id = 'cos 3Pi/2'
	    call oscrwrite(6)
	    do i=3585,4609
		x = (i - 4097)/512.0
		r(i) = 0.5*(cos(3.0*pi*x) + 1.0)
	    end do
	    id = 'cos 3Pi + 1'
	    call oscrwrite(7)
	endif
c Special case - apodising functions given in 1-7 
        if (alpha(2)(1:nalpha(2)) .eq. 'given') then
	    name = 'given'
	endif

c now make the ILS functions, save in 11-17
        call wrtstr('Transforming #')
	do ib=1,7
           write (wrtbuf,'(1x,i2)') ib
           call wrtstr(wrtbuf)
           call oscrread(ib)
c do in-place transform; keep real
c make into a symmetric line in 2048 by truncating wings
	   call fast(r,np)
	   do i=1,4096
	       r(8193 - i) = r(8193 - 2*i)
	   end do
	   do i=1,1024
		r(1024 + i) = r(4096 + i)
		r(1025 - i) = r(4097 + i)
	   end do
	   chtmp = id(1:60)
	   id(1:9) = 'ILS from '
	   id(10:69) = chtmp(1:60)
	    nop = 2048
	   call oscrwrite(ib+10)
	end do
	return

c apodize series n1 n2 n3 w - Calculate the coefficients for the ILS 
2100	continue
	if (ifx .eq. 3) then
           ibf(1) = inum(1)
           ibf(2) = inum(2)
           ibf(3) = inum(3)
	endif
c write out Norton-Beer boundary, widths
	wb = xnum(1)
	bound = boundary(wb)
	x = 9.65367308d0*xnum(1)/16.0
	write (wrtbuf,2101) xnum(1)*1.206709,xnum(1),x,bound
        call wrtstr(wrtbuf)
	write (logfil,2101) xnum(1)*1.206709,xnum(1),x,bound
2101	format (' ILS width=',f10.6,'  W/Wo=',f10.6,
     &          '   x=',f10.6,'  Boundary=',f10.6)

c The a's are the values at x=0; the b's are for x=w
c Both are obtained from the ILS functions on scratch
	call calcoefs(x,ibf,a,b,c10,c13,c20,c23,r)


c write out coeffs
	    ibf1 = ibf(1)
	    ibf2 = ibf(2)
	    ibf3 = ibf(3)

	    write (wrtbuf,101) b(ibf(1)),b(ibf(2)),b(ibf(3))
            call wrtstr(wrtbuf)
101	    format (' b(ibf1) =',f9.6,' b(ibf2) =',f9.6,
     *              ' b(ibf3) =',f9.6)
	    write (logfil,101) b(ibf(1)),b(ibf(2)),b(ibf(3))

	    write (wrtbuf,105) c10,c13
            call wrtstr(wrtbuf)
105	    format (' c10 =',f13.8,' c13 =',f13.8)
	    write (wrtbuf,107) c20,c23
            call wrtstr(wrtbuf)
107	    format (' c20 =',f13.8,' c23 =',f13.8)
	    write (logfil,105) c10,c13
	    write (logfil,107) c20,c23
c write out test sums
	    t00 = c10*a(ibf1) + c20*a(ibf2)
	    t03 = c13*a(ibf1) + c23*a(ibf2) +  a(ibf3)
	    t10 = c10*b(ibf1) + c20*b(ibf2)
	    t13 = c13*b(ibf1) + c23*b(ibf2) +  b(ibf3)
	    write (wrtbuf,119) t00,t03,t10,t13
            call wrtstr(wrtbuf)
119	    format (' t00,t03 =',f11.8, e11.4,
     *              '     t10,t13 =',f11.8, e11.4)
	    write (logfil,119) t00,t03,t10,t13
	return

c apodize bound  w1 w2 dw   - make a file for plotting the NB and Chebyshev
c ************		     boundaries

7000	continue

	w1 = xnum(1)
	w2 = xnum(2)
	dw = xnum(3)
	n = 1 + (w2 + 1.0e-6 - w1)/dw
	w = w1 - dw
	write (logfil,7001) 
7001	format ('   lnx    logy   %    name')
	name = 'NB Bound'
c start width loop
	do 7500 ii=1,n
	    w = w + dw
	  wt = w*1.206709
	  bound = boundary(w)
	  xlog = log(wt)
	  ylog10 = log10(bound)
	  if (ylog10 .lt. -5.0) go to 7600
	  write (wrtbuf,7003) xlog,ylog10,name
          call wrtstr(wrtbuf)
	  write (logfil,7003) xlog,ylog10,name
7003	  format (2f8.4,' % ',a8)
7500	continue
c repeat width loop
7600	name = 'DC Bound'
	w = w1 - dw
	do 7700 ii=1,n
	    w = w + dw
	  wt = w*1.206709
	  xlog = log(wt)
	  ylog10 = 0.1505 - 1.12557*w*w
	  if (ylog10 .lt. -5.0) return
	  write (wrtbuf,7003) xlog,ylog10,name
          call wrtstr(wrtbuf)
	  write (logfil,7003) xlog,ylog10,name
7700	continue

	return

        end

c ----------------------------------------------------------------
        real function boundary(x)

* calculate Norton-Beer boundary

	ex = 1.939 - x*(1.401 + x*0.597)
	boundary = 0.21723*( 10.0**ex )

	return
	end

c ----------------------------------------------------------------

	double precision function dsinc(x)

	double precision x,pi
	data pi/3.14159265359d0/

	if (x .eq. 0.0) then
           dsinc = 1.0d0
        else
           dsinc = dsin(pi*x)/(pi*x)
        end if

	return
	end

c ----------------------------------------------------------------

	subroutine findhalf(wout,r)

	include 'datetc.h'

        real r(*)

	double precision x1,r1,dx

c normalize
	rmax = r(1025)
	do i=1,2048
           r(i) = r(i)/rmax
           if (r(i) .ge. 0.5) imax = i
	end do
c find halfwidth - imax is last point .ge. 0.5
c do by binary division for simplicity and stability
	x1 = imax
	dx = 1.0
	igt = 1
5500	if (abs(dx) .lt. 1.0e-6) go to 5800
	xparb = x1 + dx
	x1 = xparb
	call interp(r)
	r1 = xparb
	if (r1 .ge. 0.5) then
           if (igt .eq. 1) go to 5500
           igt = 1
           dx = -0.5*dx
	else
           if (igt .eq. -1) go to 5500
           dx = -dx
           igt = -1
	endif
	go to 5500

5800	wout = 0.25*(x1 - 1025.)
	return
	end
c ----------------------------------------------------------------

	subroutine findextr(xm,ym,r)

	include 'datetc.h'
	include 'infmtn.h'

        real r(*)

	real xm(20),ym(20)
	real x1,r1

c find first 20 extrema
        imax = 1025
        neoa = 1
        do i=1,20
           neoa = -neoa
 50        ipara = imax
           iparb = 1
           iparc = 1
           call lnseek(r)
           imax = iparb
           if (iparc .eq. -1) go to 50
           x1 = xparb
           call interp(r)
           r1 = xparb
           xm(i) = 0.125*(x1-1025.)
           ym(i) = r1
        enddo
        neoa = 1
	return
	end
c ----------------------------------------------------------------

	subroutine calcoefs(x,ibf,a,b,c10,c13,c20,c23,r)

	include 'datetc.h'

        real r(*)

        integer  ibf(3)
	real x,b(6)
	real c10,c13,c20,c23,bot,a(6)

c - Calculate the coefficients for the ILS 
c The a's are the values at x=0; the b's are for x=w
c Both are obtained from the ILS functions on scratch

        do i=1,3
           call oscrread(10 + ibf(i))
           a(ibf(i)) = r(1025)
           xparb = 8.0*x + 1025
           call interp(r)
           b(ibf(i)) = xparb
        enddo

        ibf1 = ibf(1)
        ibf2 = ibf(2)
        ibf3 = ibf(3)

c coefficients for c1, c2 for n1-n2-n3 functions
        bot = b(ibf2)*a(ibf1) - b(ibf1)*a(ibf2)
        c10 = (b(ibf2) - a(ibf2)/2.0)/bot
        c13 = (b(ibf3)*a(ibf2) - b(ibf2)*a(ibf3))/bot
        c20 = (1.0 - a(ibf1)*c10)/a(ibf2)
        c23 = -(a(ibf3) + a(ibf1)*c13)/a(ibf2)
	
        return

        end
c ----------------------------------------------------------------






