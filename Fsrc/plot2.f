*
* The subroutines in this module were adapted from the plotting
* module of the original version of Gremlin
*
*--------------------------------------------------------------------

      subroutine cnorm(r,kx,yk,n,imode)

c N - normalize 1-3 points to 1.0   (imode= 1)
c B - subtract 1-3 points to 0.0    (imode=-1)
      
        include  'datetc.h'
        include  'infmtn.h'
 
        real r(*)

	integer n, kx(4),imode
	integer i1,i2,i3,di12,di13,di23
	real yk(4)
	real y1,y2,y3,dy12,dy13,au,al, temp

	xparc = 0.0
	xparb = 0.0
	i1 = kx(1)
	y1 = yk(1)
	xpara = y1
	if (n .ge. 2) then
	    i2 = kx(2)
	    y2 = yk(2)
	    dy12 = y1 - y2
	    di12 = i1 - i2
	    xparb = dy12/di12
	endif
	if (n .eq. 3) then
	    i3 = kx(3)
	    y3 = yk(3)
	    dy13 = y1 - y3
	    di13 = i1 - i3
	    di23 = i2 - i3
	    au = dy12 * di13 - dy13 * di12
	    al = float(di12) * float(di13) * float(di23)
	    xparc = au / al
	endif
	xparb = xparb - xparc * (i1 + i2)
	xpara = y1 - xparb * i1 - xparc * i1 * i1 
	if (n .eq. 0) then
	    call momnts(r)
	    xpara = absmx
	endif

* if mode=1, divide by parabola
	if (imode .eq. 1) call norm(r)

* if mode=-1, subtract parabola
	if (imode .eq. -1) then
	    do i = 1, nop
		temp = i
		r(i) = r(i) - (xpara + temp*(xparb + xparc*temp))
	    enddo
	endif
	return
	end

*--------------------------------------------------------------------

      subroutine cgline(r,kx,yk,cg,ln,insert,
     &                  point, amp, wd, dmp,
     &                  eps1,eps2,eps3,eps4,eps5,
     &                  nit, nhold, ctag, dent)
  
*
* Find the c.g. of the line marked by cursor saves 1 and 2, insert in list
* return the line number to the caller
* insert = 0 : do not insert c.g. into line list
* insert = 1 : insert calculated c.g. into line list
*
        include  'datetc.h'
        include  'infmtn.h'
        include  'linparms.h'
        include  'iounit.h'

        real r(*)
        real amp(*), wd(*), dmp(*)
        real eps1(*),eps2(*),eps3(*),eps4(*),eps5(*)
        integer nit(*), nhold(*)
        double precision point(*)
        character ctag(*)*4, dent(*)*32
 
	integer kx(4), ln, insert
	real yk(4)
	real rsum, rcg, rmx, cg
	integer i, nstrt, nstop, nmid

	y1 = yk(1)
	y2 = yk(2)
	nstrt = kx(1)
	nstop = kx(2)
	nmid = (nstrt + nstop)/2

* work with differences from straight-line base
	if (nstop .gt. nstrt) then
	    bmul = (y2 - y1)/(nstop - nstrt)
	else
	    call wrtstr(' ') 
            call wrtstr(' Error :  zero range - abort c.g.')
	    return
	endif

	rsum = 0.
	rcg = 0.
	rmx = 0.
    	do i = nstrt, nstop
	   tmp = (r(i) - bmul*(i - nstrt))*neoa
	   if (tmp .gt. rmx) rmx = tmp
	   rcg = rcg + tmp*(i - nmid)
	   rsum = rsum + tmp
	enddo
	rcg = nmid + rcg/rsum
	cg  = rcg                  ! return c.g. to caller

	if (rmx .eq. 0.0) then
	    write(wrtbuf,'(a)') 
     &            ' Error :  abort c.g. - max is zero. Check neoa'
            call wrtstr( wrtbuf )
	    return
	endif

	call disper
	xparb = rcg
	call ptow
        call wrtstr(' ')
	write(wrtbuf,9004) rsum, rsum*delw
	call wrtstr(wrtbuf)
	if ( nwos .eq. -1 ) then      ! wave number scale
	   write(wrtbuf,9005) rcg, xparb
	else
	   write(wrtbuf,9006) rcg, xparb
	end if
	call wrtstr(wrtbuf)
 9004	format(' sum of r(i)    = ',e12.5,'   E.W. = ',e12.5)
 9005	format(' ctr of gravity = ', f16.6,' pts / ', f14.6,' /cm')
 9006	format(' ctr of gravity = ', f16.6,' pts / ', f14.6,' nm')

        if ( insert .eq. 0 ) return

	xnum(1) = xparb
	xnum(2) = rmx
	xnum(3) = 1000.0*rsum*delw/(p(1)*rmx)
	xnum(4) = 0.0
	inum(1) = 0
        call inslin(ln,r,point, amp, wd, dmp,
     &     eps1,eps2,eps3,eps4,eps5,nit, nhold, ctag, dent)
	ctag(ln) = '   G'
C      hold all params of line so lsqfit does not fit it. (gn)
        nhold(ln) = 16
c for range, save only first digit and fraction in eps
	xparb = nstrt
	call ptow
	i = xparb/10.
	eps1(ln) = xparb - 10.0*i
	xparb = nstop
	call ptow
	i = xparb/10.
	eps2(ln) = xparb - 10.0*i
	eps3(ln) = y1
	eps4(ln) = y2
	return
	end

*--------------------------------------------------------------------

      subroutine synclin(point,amp,wd,dmp,dent)
*
* mark all lines on screen and put all lines from line buffer
* into plotter buffer
*
        include  'datetc.h'
        include  'linparms.h'

	real amp(*), wd(*), dmp(*)
        double precision point(*)
        character dent(*)*32
 
        integer ln
        real xw

        call llerase                 ! empty plotter line list
        do ln=1,nol                  ! synchronize lists
	   xparb = point(ln)
	   call ptow
           xw = 2000.0 * xpara * wd(ln)
           call insmarker(0, xparb, nint(point(ln)), amp(ln), 
     &                    xw, dent(ln))
	end do

        return
        end

*--------------------------------------------------------------------

      subroutine updatelin(point,amp,wd,tr)
*
* update the plotter line list but keep active/inactive line status
* uses the 'tr' array for temporary storage of wavenumbers
*
        include  'datetc.h'
        include  'linparms.h'

        real tr(*), amp(*), wd(*)
        double precision point(*)

        integer ln

        if ( nol .gt. nrtr ) then
           call wrtstr(
     &     ' Error :  tr array too small for temp. line list.')
           return
        end if

        do ln=1,nol                   ! calculate wavenumbers
           xparb = point(ln)
           call ptow
           tr(ln) = xparb             ! keep them in 'tr'
        end do
        call updlin(point,tr,amp,wd)  ! update plotter data

        return
        end

*--------------------------------------------------------------------

