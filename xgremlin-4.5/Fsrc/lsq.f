c \grammy\src\lsq.for  Ver. 2.2

c  Revision 2.31 09/06/18  (gn) change active so held lines not fitted
c                          (gn) hold all parameters for lines found using c.g.
c  Revision 2.30 93/11/05  default apod assumed to be 5%
c  Revision 2.29 92/05/31  ESC added to fitlines; quiet, verbose added to lsq
c			    pack deltas into eps1,4,5
c  Revision 2.28 92/02/22  Fitlines added
c  Revision 2.27 92/02/11  scrollon/scrolloff added using 'S'
c  Revision 2.26 91/10/25  Derivative range limiting fixed in lsq
c  Revision 2.25 89/08/07  Altinf added to provide ncenter
c  Revision 2.24 89/03/16  Filtered fit lines labelled 'R'
c  Revision 2.22 89/02/23  Holdat changed to use nhold=16 for hold, 
c				negative nit for drop

	subroutine lsqdvr(r,point,amp,wd,dmp,
     &                    eps1,eps2,eps3,eps4,eps5,
     &                    nit,nhold,ctag)
 
	include 'datetc.h'
	include 'infmtn.h'
	include 'altinf.h'
	include 'set.h'
	include 'linparms.h'
	include 'iounit.h'
	include 'temp.h'

	real r(*)
        real amp(*), wd(*), dmp(*)
	real eps1(*),eps2(*),eps3(*),eps4(*),eps5(*)
        integer nit(*), nhold(*)
        double precision point(*)
        character ctag(*)*4
   
	logical nochng,scrollon
	integer ntimes, nlow, nhigh, nfl, nfu, jb
	real wdo, xnr, sum, sigo, sigf, dbt, dxw, da, dxc
	real a, bt, xw, rmean, x, sigma, dsig,twd,dtwd
	real xreal
	integer*2 ireal2(2),i1
	equivalence (xreal,ireal2)   
	double precision wlt, xc
	integer itrflag
	integer ncut

	external itrflag

	data ncut/0/,scrollon/.false./

c check for filter instructions:
c   lsqfit filter nrise [ncut]  ( if ncut omitted, get from .hdr)
c   lsqfit filteroff            ( use ncut=0 as flag)
c if apod1=0, assume it was never set, reduce filter by 5%

	call itron
	if (nstrng .eq. 1) then
	   if (alpha(1)(1:nalpha(1)) .eq. 'filter' ) then
	      nrise = inum(1)
	      if (ifx .eq. 2) then
		 ncut = inum(2)	
	      else
		 if (resolutn .eq. 0.0) then
		    call wrtstr(
     &       ' Insufficient info for auto-cut - filter set aborted')
		    return
		 endif
		 ncut = (nop/2)*delw/resolutn
		 if (apod1 .eq. 0.0) then
		    ncut = 0.95*ncut
		 else
		    tmp = nint - ncenter
		    ncut = ncut*(tmp - apod1)/tmp
		 end if
	      end if
	      write (wrtbuf,11) nrise, ncut
	      call wrtstr(wrtbuf)
 11	      format (' nrise = ',i6,', ncut = ',i7)
	   end if
	   if (alpha(1)(1:nalpha(1)) .eq. 'filteroff' ) ncut = 0
	   if (alpha(1)(1:nalpha(1)) .eq. 'quiet' ) scrollon = .false.
	   if (alpha(1)(1:nalpha(1)) .eq. 'verbose') scrollon = .true.
	   return

	endif

	if (nol .eq. 0) return
	wdo = 5.0
	rbar = lsqref
	ntimes = inum(1)
	if (ifx .eq. 0) ntimes = lsqcnt
	xlast = 26.0
	nlow = lsqlo
	nhigh = lsqhi
	if (lsqhi .eq. -1) nhigh = nol
	if (ifx .le. 1) go to 2002
	if (ifx .eq. 2) inum(3) = inum(2)
	nlow = inum(2)
	nhigh = inum(3)
2002	if (ifl .ge. 2) wdo = xnum(2)
	if (ifl .ge. 1) rbar = xnum(1)
2010	nfl = point(nlow) - wdo * wd(nlow)
	nfu = point(nhigh) + wdo * wd(nhigh)
	if (nfl .gt. 0) go to 2012
	nlow = nlow + 1
	go to 2010
2012	if (nfu .le. nop) go to 2014
	nhigh = nhigh - 1
	go to 2010

2014	xnr = nfu - nfl + 1
	sum = 0.0
	xeoa = neoa
	call strtrn

	do jb = 0, ntimes
	    if (jb .eq. 0) go to 2078
	    if (scrollon .and. jb .eq. ntimes) then
	       write (wrtbuf,90001)
	       call wrtout(output,wrtbuf)
	       write (wrtbuf,90002)
	       call wrtout(output,wrtbuf)
	    end if
	    nochng = .true.

	    if (ncut .ne. 0) then
c If ncut not 0: mask, rise nrise; filter, sharp-cut at ncut
		iparb = nh
		iparc = nrise
		call mask(r)
		call fast(r(1),np)
		ipara = 2 * ncut
		do i = ipara+1, nppt, 2
		   r(i) = 0.0
		   r(i+1) = 0.0
		end do
		call fsst(r(1),np)
	    end if

	    do nl = nlow, nhigh
		del = 0.0
		sigf = 0.0
		dsig = 0.0
		dbt = 0.0
		dxw = 0.0
		da = 0.0
		dxc = 0.0
c	nit(nl) =< 0 implies inactive; nhold(nl) > 15 means line is held
		if (nit(nl) .le. 0 .or. nhold(nl) .gt. 15) go to 2070
c					Is the parameter unreasonable?
		if (wd(nl) .lt. 0.6) go to 2070
		if (amp(nl) .lt. 0.00010) go to 2070
		a = amp(nl)
		bt = dmp(nl)
		xc = point(nl)
		xw = wd(nl)
		call lsq(nl, wdo, rbar, sigo, sigf, del,
     &                    r,point,amp,wd,dmp,
     &                    eps1,eps2,eps3,nit,nhold)

c					if step size is 0.0, no improvement
		if (del .le. 0.0) go to 2070
		call addlin (r,xc, xw, a, bt, xeoa)
		nit(nl) = nit(nl) + 1
		call addlin
     &		     (r,point(nl), wd(nl), amp(nl), dmp(nl), -xeoa)
		if ( nit(nl) .eq. 1 ) then  ! mark line active in plotter list
		   call lineact( nl, 1 )
		end if
		ctag(nl)(1:4) = '   L'
		if (ncut .ne. 0) ctag(nl)(1:4) = '   R'

c					calculate deltas.
		da = amp(nl) - a
		dxc = point(nl) - xc
		dxw = wd(nl) - xw
		dbt = (dmp(nl) - bt)/25.0
		dsig = sigo - sigf
c					if the line changed less than the
c					tolerance, skip over update flag.
		if (dsig .le. lsqtol) go to 2070
		nochng = .false.
2070		xparb = point(nl)
		call ptow
		wlt = xparb
		bt = (dmp(nl) - 1.0)/25.0
		twd = 2000.0*delw*wd(nl)
		dtwd = 2000.0*delw*dxw
		dxc = 1000.0*delw*dxc
c pack deltas as real*2:
c   eps1 holds sigf, dsig
		xreal = dsig
		i1 = ireal2(2)
		xreal = eps1(nl)
		ireal2(1) = i1 
		eps1(nl) = xreal
c   eps4 holds dsigma, da
		xreal = da
		i1 = ireal2(2)
		xreal = dxc
		ireal2(1) = i1 
		eps4(nl) = xreal
c   eps5 holds dwidth, db
		xreal = dbt
		i1 = ireal2(2)
		xreal = dtwd
		ireal2(1) = i1 
		eps5(nl) = xreal

		if (scrollon .and. jb .eq. ntimes) then
         	   write (wrtbuf, 90011) nl, point(nl), wlt, amp(nl),
     *		   twd, bt, nit(nl), sigf
		   call wrtout(output,wrtbuf)
		   write(wrtbuf, 90012) dxc, da, dtwd, dbt, del, dsig 
		   call wrtout(output,wrtbuf)
		end if
	     end do
90011	     format (i5,f11.4,f12.5, f12.6, 2f10.4, i8,f10.6)
90012	     format (19x,f10.5, f12.6, 2f10.4, f8.4, f10.6)
 2078	     rmean = 0.0
	     sum = 0.0
	     do i = nfl, nfu
		rmean = rmean + r(i)
		x  = r(i) - rbar
		sum = sum + x * x
	     end do
	     sigma = sqrt (sum / xnr)
	     rmean = rmean / xnr
	     write (wrtbuf, 9002) sigma, rmean
	     call wrtout(output,wrtbuf)
	     if (jb .eq. 0) cycle
	     if (nochng) goto 2099
	     call procpend
	     if (itrflag() .eq. 1) go to 2099
	  end do
 2099	  call itroff
	  return

90001	format(' line      xc     wavexxxxxx    amp       width   ',
c               i2345 23456.8901 23456.89012 2345.789012 2345.7890
     *     '   damping     itn     sigma')
c            2345.7890 2345678 23.567890     
90002	format(21x,'1000dx  ',
c            234.67890
     *     '      da      1000dw       db      del      dsig')
c            2345.789012 2345.7890 2345.7890 23.5678 23.567890
9002	format (' mean=', g14.7, ', sigma=', g16.7)
	end

c ****************************************************************

	subroutine lsq(nl,wdf,rb,sigo,sigf,del,
     &                 r,point,amp,wd,dmp,
     &                 eps1,eps2,eps3,nit,nhold)
 
	include 'datetc.h'
	include 'infmtn.h'
	include 'linparms.h'

	real wdf, rb, sigo, sigf, del
	real r(*)
        real amp(*), wd(*), dmp(*)
	real eps1(*),eps2(*),eps3(*)
        integer nit(*), nhold(*)
        double precision point(*)

	real xeoa, xw, a, bt, xbt, xr, xnr 
	real cdw,cda,cwa,sgds,sgws,sgps,sgs 
	double precision xc, u, du
	integer nl
	integer itn
	integer nr, nfl, mb, mw, ml, ma, i, ia, nt

c ** save old parameter values.  set direction of damping increment,
c ** range of fit

	xeoa = neoa
	itn = nit(nl)
	xc = point(nl)
	xw = wd(nl)
	a = amp(nl)
	bt = dmp(nl)
	xbt = 1.0
	if (bt .ge. 25.0) xbt = -1.0
c keep nr even:
	xr =  wdf * xw
	if (xr .ge. real(lsqfit_size)) xr = real(lsqfit_size)
	nr = xr
	nr = 2*nr
	xnr = nr
c make sure line fits in buffer:
	nfl = xc - 0.5 * xnr - 1.0
	if (nfl .le. 0) go to 300
	if (nfl + nr .gt. nop)  go to 300
c					check for fixed variables
	mb = 0
	mw = 0
	ml = 0
	ma = 0
	ihl = nhold(nl)
	if (ihl .ge. 8) ma = 1
	if (mod(ihl,8) .ge. 4) ml = 1
	if (mod(ihl,4) .ge. 2) mw = 1
	if (mod(ihl,2) .ge. 1) mb = 1
c					form orthogonal functions
	cdw = 0.0
	cda = 0.0
	cwa = 0.0
	sgds = 0.0
	sgws = 0.0
	sgps = 0.0
	sgs = 0.0
	xparb = bt
	call vstart
	u = (nfl - xc) / xw
	uo = u
	du = 1.0 / xw
	duw = 0.95
	dux = 0.05
	do i = 1, nr
	   xparc = u + du
	   u = xparc
	   call voigt
	   tf(i) = xpara
	   sgs = sgs + tf(i) * tf(i)
	   if (mw .eq. 1) go to 22
	   xparc = u * duw
	   call voigt
	   xm(i) = a * (xpara - tf(i)) * 19.0 / xw
	   if (ma .eq. 0) cwa = cwa + tf(i) * xm(i)
22	   if (ml .eq. 1) go to 25
	   xparc = u - dux
	   call voigt
	   xmn = xpara
	   xparc = u + dux
	   call voigt
	   ypm(i) = a * (xmn - xpara) * 10.0 / xw
	   sgps = sgps + ypm(i) * ypm(i)
 25	   continue
	end do
  
	cwa = cwa / sgs
	if (mb .eq. 1) go to 41
	xparb = bt + xbt
	call vstart
	xparc = uo
  
	do i = 1, nr
	   xparc = xparc + du
	   call voigt
	   tfd(i) = a * (xpara - tf(i))
	   if (ma .eq. 0) cda = cda + tf(i) * tfd(i)
	end do
  
	cda = cda / sgs
41	if (mw .eq. 1) go to 51
  
	do i = 1, nr
	   xm(i) = xm(i) - cwa * tf(i)
	   sgws = sgws + xm(i) * xm(i)
	   if (mb .eq. 0) cdw = cdw + tfd(i) * xm(i)
	end do
  
	cdw = cdw / sgws
51	if (mb .eq. 1) go to 61
	do i = 1, nr
	   tfd(i) = tfd(i) - cdw * xm(i) - cda * tf(i)
	   sgds = sgds + tfd(i) * tfd(i)
	end do

c ** sum cross products, find variable increments

61	dbt = 0.0
	dxw = 0.0
	dxc = 0.0
	da = 0.0
	sigo = 0.0
	sd = 0.0
	sc = 0.0
	sw = 0.0
	sa = 0.0
  
	do i = 1, nr
	   ia = nfl + i
	   rs = xeoa * (rb - r(ia))
	   sigo = sigo + rs * rs
	   sa = sa + rs * tf(i)
	   sc = sc + rs * ypm(i)
	   sw = sw + rs * xm(i)
	   sd = sd + rs * tfd(i)
	end do
  
	if (ml .eq. 0) dxc = sc / sgps
	if (mb .eq. 0) dbt = sd / sgds
	if (mw .eq. 0) dxw = sw / sgws -cdw * dbt
	if (ma .eq. 0) da = sa / sgs - cwa * dxw - cda * dbt
  
	do i = 1, nr
	   ia = nfl + i
	   tf(i) = xeoa * (rb - r(ia)) - a * tf(i)
	end do
  
	del = 2.0
	sigt = sigo
100	del = 0.5 * del
	if (del .lt. 0.125) go to 240
105	amp(nl) = a - del * da
	point(nl) = xc - del * dxc
	wd(nl) = xw - del * dxw
	if (amp(nl) .lt.  .5 * a) go to 100
	if (wd(nl) .lt.  .5 * xw) go to 100
	if (wd(nl) .gt.  2.0 * xw) go to 100
112	nt = nl + 1
	if (dxc .lt. 0.) go to 130
	if (dxc .eq. 0.) go to 150
	nt = nl - 1
	xcmin = 1.0
	if (nt .le. 0) go to 125
	xcmin = point(nt)
125	if ((xc - xcmin) .lt. (2.0 * dxc * del)) go to 100
	go to 150
  
130	nt = nl + 1
	xcmax = nop
	if (nt .gt. nol) go to 140
	xcmax = point(nt)
140	if (xc .gt. (xcmax + 2.0 * dxc * del)) go to 100
  
150	b = bt - xbt * del * dbt
	if (b .ge. 1.0) go to 160
  
	b = 1.000001
	go to 166
  
160	if (b .le. 26.) go to 170
  
	b = 25.999999
166	dbt = (bt - b) / (xbt * del)
	if (mw .eq. 0) dxw = sw / sgws - cdw * dbt
	if (ma .eq. 0) da = sa / sgs - cwa * dxw - cda * dbt
	go to 105
  
170	sum = 0.0
	xparb = b
	call vstart
	xparc = (nfl - point(nl)) / wd(nl)
	du = 1.0 / wd(nl)
	do 175 i = 1, nr
	   xparc = xparc + du
	   call voigt
	   x = tf(i) + amp(nl) * xpara
175	   sum = sum + x * x
	sigf = sum
	if(sigf .ge. sigt) go to 190
	sigt = sigf
	delo = del
	dmp(nl) = abs( b )              ! make sure b > 0
	go to 100
  
190	if(sigt .ge. sigo) go to 100
c					time to return
240	if(sigt .lt. sigo) go to 245
c					no improvement
	del = 0.0
	sigf = sigo
	dmp(nl) = abs( bt )
	amp(nl) = a
	point(nl) = xc
	wd(nl) = xw
	go to 250
  
c					normal return with improvement
245	del = delo
	sigf = sigt
	amp(nl) = a - del * da
	point(nl) = xc - del * dxc
	wd(nl) = xw - del * dxw
250	sigo =  sqrt(sigo / xnr)
	sigf =  sqrt(sigf / xnr)
  
c save sigf, find and save even and odd components of error
	sum = 0.0
	sgs = 0.0
	do i = 1, nr/2
	   ia = nfl + i
	   xparb = 2.0*xc - ia
	   call interp(r)
	   seven = 0.5*(r(ia) + xparb) - rb
	   sodd = 0.5*(r(ia) - xparb)
	   sum = sum + seven*seven
	   sgs = sgs + sodd*sodd
	end do
	eps1(nl) =  sigf
	eps2(nl) =  sqrt(2.0*sum / xnr)
	eps3(nl) =  sqrt(2.0*sgs / xnr)
	return

c line out of range - hold
300	continue
	nhold(nl) = nhold(nl) + 16
	return
	end
 
c ****************************************************************

	subroutine addlin (r, xc, xw, a, bt, del)

	include 'datetc.h'
	include 'infmtn.h'

	real r(*)

	common/vstrt/c,ca,cb,cc,cd,ce,ulim,vf,lastv
	real c,ca,cb,cc,cd,ce,ulim
	real vf(50,26)
	real  du, xa, xb, ccc, cca, ulm

 	integer i
	real xw, a, bt, del
	double precision xc,xpc


c ** xc = point,  xw = wd,  a = amp,  bt = dmp,  del = xeoa

	xlast = -1.0
	xparb = bt
	call vstart
	cca = ca
	ccc = c
	ulm = ulim
	if (xparb .ge. 25.99999) then
	   cca = 1.0
	   ccc = 1.0
	   ulm = 4.7
	end if

	xpc = -xc/xw
	du = 1.0/xw
c					del = 1. for emission lines
        xa = a
        if (del .eq. -1.0) xa = -a
        xb = xa*cca

c points beyond which the function is negligible:
        ni = xc - wdlim*xw              ! kill at wdlim widths (default 1000)
                if (ni .lt. 1) ni = 1
        nf = xc + wdlim*xw              ! kill at wdlim widths
                if (nf .gt. nop) nf = nop

c points where the changeover to asymptotic form occurs:
        na = xc - ulm*xw
        nb = xc + ulm*xw

        xparc = (ni - 1 -xc)/xw
        du = 1.0/xw

        if (na .gt. 1) then
	   if (na .gt. nop) na = nop
	   do i=ni,na
	      xparc = xparc + du
 	      r(i) = r(i) + xb/(xparc*xparc + ccc)
	   end do
	   if (na .eq. nop) return
	   
	   if (nb .gt. nop) nb = nop

	   do  i = na+1, nb
	      xparc = xparc + du
	      call voigt
	      r(i) = r(i) + xa*xpara
	   end do
	   if (nb .eq. nop) return

	   do i=nb+1,nf
	      xparc = xparc + du
 	      r(i) = r(i) + xb/(xparc*xparc + ccc)
	   end do
	   return
	endif
	
	if (nb .gt. 1) then
	   if (nb .gt. nop) nb = nop

	   do i = 1, nb
	      xparc = xparc + du
	      call voigt
 	      r(i) = r(i) + xa*xpara
	   end do
	   if (nb .eq. nop) return

	   do i=nb+1,nf
	      xparc = xparc + du
	      r(i) = r(i) + xb/(xparc*xparc + ccc)
	   end do
	   return
        else
	   xparc = -xc/xw
	   do i=1,nop
	      xparc = xparc + du
 	      r(i) = r(i) + xb/(xparc*xparc + ccc)
	   end do
	   return
        endif
  
        end

c ***********************************************************************

	subroutine rmlin(r,
     &                   point, amp, wd, dmp,
     &                   eps1,eps2,eps3,eps4,eps5,
     &                   nit, nhold, ctag, dent)

	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'
	include 'linparms.h'

	real r(*)
        real amp(*), wd(*), dmp(*)
        real eps1(*),eps2(*),eps3(*),eps4(*),eps5(*)
        integer nit(*), nhold(*)
        double precision point(*)
        character ctag(*)*4, dent(*)*32 

	integer i, nl
	real xeoa

	xeoa = neoa
	if (ifx .ne. 1) go to 901
	nl = inum(1)
	if (nl .le. 0  .or.  nl .gt. nol) go to 902

c **					remove line if active

	if (nit(nl) .gt. 0) call addlin
     &		(r,point(nl), wd(nl), amp(nl), dmp(nl), xeoa)

	nol = nol - 1
	do i = nl, nol
	   point(i) = point(i+1)
	   amp(i) = amp(i+1)
	   wd(i) = wd(i+1)
	   dmp(i) = dmp(i+1)
	   ctag(i) = ctag(i+1)
	   nit(i) = nit(i+1)
	   nhold(i) = nhold(i+1)
	   eps1(i) = eps1(i+1)
	   eps2(i) = eps2(i+1)
	   eps3(i) = eps3(i+1)
	   eps4(i) = eps4(i+1)
	   eps5(i) = eps5(i+1)
	   dent(i) = dent(i+1)
	end do
	return
901	write (wrtbuf,9000)
	call wrtout(output,wrtbuf)
9000	format(' Only one line can be removed, "remove linenumber".')
	return
902	write (wrtbuf,9001) nol
	call wrtout(output,wrtbuf)
9001	format(' specified line is not in the table. nol = ',i5)
	return
	end

c **************************************************************************

	subroutine holdat(lact,kon,r,point,amp,wd,dmp,nit,nhold,ctag)
c
c  This subroutine changes line flags to reflect the line's status and adds
c  or subtracts the line from the data if necessary.
c  lact = 1 for hold, 2 for active, 3 for drop, 4 for parameter holds
c 1 - damping;  2 - width.  If < 1.0, interpret as fractional width
c 4 - wavenumber;  8 - amplitude

	include 'infmtn.h'
	include 'set.h'
	include 'linparms.h'
	include 'datetc.h'

	real r(*)
        real amp(*), wd(*), dmp(*)
        integer nit(*), nhold(*)
        double precision point(*)
        character ctag(*)*4

	integer lact,kon
	double precision var
	character*1 ctype

	call itron       ! activate stop button

    	var = 0.0
	if(ifl .eq. 1) var = xnum(1)
c set up list handling
	j = 1
	if (ifx .gt. 0) go to 2202
	ifx = 2
	inum(1) = 1
	inum(2) = -nol
c start list loop
 2202	k = 1
	ib = inum(j) - 1
	xlast = 26.0
	xeoa = neoa
	call disper
	aps = xpara
	if (nstrng .ne. 0) ctype = alpha(1)(1:1)
c more list handling logic
	if (j .eq. ifx) go to 2208
	if (j .gt. ifx) then
	   call itroff
	   return
	end if

c more elements to come.  If next is zero, error
	if (inum(j+1) .eq. 0) go to 2260
c if next is positive, last element is single
	if (inum(j+1) .gt. 0) go to 2208
c next is negative - handle range
	j = j + 1
	k = -inum(j) - ib


 2208	do 2250 ia = 1, k

	   call procpend       ! process pending events
	   if ( itrflag() .eq. 1 ) then
	      call itroff
	      return
	   end if

	   nl = ib + ia
	   if (nl .gt. nol) go to 2250

	   itn = nit(nl)
	   if (nstrng .eq. 1 .and. ctype .ne. ctag(nl)(1:1)) go to 2250

c 1=hold, 2=active, 3=drop, 4=holdxxxx
	   go to (2220, 2310, 2410, 2110), lact

 2110	   continue

c parameter holds
	   ihold = nhold(nl)

c If parameter is negative, just unlock at current value
	   if (var .lt. 0.) then
	     if (mod(ihold, 2*kon) .ge. kon) then
		nhold(nl) = nhold(nl) - kon
	     end if
	     goto 2250
	   endif

c No parameter - just lock at current value
	   if (ifl .eq. 0) go to 2125

c Parameter given  - lock at that value
	   if (itn .gt. 0) then
	      call addlin(r,point(nl),wd(nl),amp(nl),dmp(nl),xeoa)
	   end if

c 1 - damping
	   if (kon .eq. 1) then
	      if (var .gt. 1.0) then !var = Gaussian;adjust L,dmp
		 w = wd(nl)*2000.0*aps
		 g = var
		 tz = 7.7464*w*(1.0-sqrt(1.0-.24152*(1.-(g/w)**2)))
		 dmp(nl) = 25.0*tz/w + 1.0
	      else
		 dmp(nl) = 25.0*var + 1.0
	      endif
	   end if

c 2 - width.  If < 1.0, interpret as fractional width
	   if (kon .eq. 2) then
	      if (var .lt. 1.0) then
		 xparb = point(nl)
		 call ptow
		 var = var * xparb * 1000.0
	      endif
	      wd(nl) = var / (2000.0 * aps)
	   endif

c 4 - wavenumber
	   if (kon .eq. 4) then
	      xparb = var
	      call wtop
	      point(nl) = xparb
	   endif

c 8 - amplitude
	   if (kon .eq. 8) amp(nl) = var
	   if (itn .gt. 0) then
	      call addlin(r,point(nl),wd(nl),amp(nl),dmp(nl),-xeoa)
	   end if
 2125	   if (mod(ihold, 2*kon) .lt. kon) nhold(nl) = nhold(nl) + kon
	   goto 2250

c  hold
 2220	   if (nhold(nl) .gt. 16) go to 2250
C          If negative parameter given, unlock line (gn)

	   if (var .lt. 0.) then
		if(nhold(nl) .ge. 16) nhold(nl) = nhold(nl) - 16
           else
	        nhold(nl) = nhold(nl) + 16
           endif
	   goto 2250

c  active
C 2310	   nhold(nl) = mod(nhold(nl),16)
C   (gn) held lines are not activated
 2310	   continue
	   if (itn .gt. 0) go to 2250
	   nit(nl) = -nit(nl)
	   if (itn .eq. 0) nit(nl) = 1
	   xtmp = -xeoa
	   call lineact( nl, 1 )
	   goto 2325

c   drop
 2410	   if (itn .le. 0) go to 2250
	   nit(nl) = -nit(nl)
	   xtmp = xeoa
	   call lineact( nl, 0 )

 2325	   call addlin(r,point(nl),wd(nl),amp(nl),dmp(nl),xtmp)
	   

 2250	continue
  
	j = j + 1
	go to 2202
  

 2260	if (batch) ipara = -999999
	call wrtstr(' Error :  number of lines is zero')
	call itroff
	return

	end

c**************************************************************************

	subroutine inslin (nl,
     &                     r,
     &                     point, amp, wd, dmp,
     &                     eps1,eps2,eps3,eps4,eps5,
     &                     nit, nhold, ctag, dent)
 
c	nol is the number of lines in the table.  The insertion pt. 
c	is found by working backwards thru the table and finding the 
c	first line which has a lower wavenumber(wavelength).
c	nl is passed back to the calling routine to allow
c	the added line to be activated or whatever.

	include 'datetc.h'
	include 'infmtn.h'
	include 'linparms.h'

	real r(*)
        real amp(*), wd(*), dmp(*)
        real eps1(*),eps2(*),eps3(*),eps4(*),eps5(*)
        integer nit(*), nhold(*)
        double precision point(*)
        character ctag(*)*4, dent(*)*32
 
	integer k, nl, length

	external length

	if (nol .eq. nlins) then
	   nl = 0
	   call wrtstr(' Error :  Line table full')
	   call wrtstr(' Reduce wavenumber range or re-allocate table')
	   return
	endif
	call disper
	xlast = 26.0
	xeoa = neoa
	xparb = xnum(1)
	call wtop
	k = nol
	if (k .eq. 0) go to 2630
2610	if (xparb .ge. point(k)) go to 2630
	point(k+1) = point(k)
	amp(k+1) = amp(k)
	wd(k+1) = wd(k)
	dmp(k+1) = dmp(k)
	ctag(k+1) = ctag(k)
	nit(k+1) = nit(k)
	nhold(k+1) = nhold(k)
	eps1(k+1) = eps1(k)
	eps2(k+1) = eps2(k)
	eps3(k+1) = eps3(k)
	eps4(k+1) = eps4(k)
	eps5(k+1) = eps5(k)
	dent(k+1) = dent(k)
	k = k - 1
	if (k .ne. 0) go to 2610
2630	nl = k + 1
	nol = nol + 1
	point(nl) = xparb
	amp(nl) = xnum(2)
	wd(nl) = xnum(3) / (2000.0 * xpara)
	dmp(nl) = 25.0*xnum(4) + 1.0
	ctag(nl) = '   I'
	nit(nl) = inum(1)
	nhold(nl) = 0
	eps1(nl) = 0.0
	eps2(nl) = 0.0
	eps3(nl) = 0.0
	eps4(nl) = 0.0
	eps5(nl) = 0.0
	dent(nl) = alpha(1)(1:32)
	if ( length(dent(nl)) .eq. 0 ) then
	   dent(nl) = 'no id'
	end if
	if (nit(nl) .gt. 0) call addlin(r,point(nl), wd(nl), 
     &                                  amp(nl), dmp(nl), -xeoa)
	return
	end
c**************************************************************************

	subroutine chnglin(r,point,wd,amp,dmp,nit,ctag,dent)

c change parameters of line nl to wave, amp, wd, dmp
c        calls - change  nl wave amp wd dmp 
c		values of 0. for wa, amp, wd, and dmp act as place holders
c		Any negative value will set dmp = 0.0
c              - change  nl 'tags' '3 char of new tags'
c              - change  nl 'id' '32 char of new id'

	include 'datetc.h'
	include 'infmtn.h'
	include 'linparms.h'

	real r(*), wd(*), amp(*), dmp(*)
	integer nit(*)
	double precision point(*)
        character ctag(*)*4, dent(*)*32

	integer nl

	if (ifx .eq. 0) then
	   call wrtstr(' Error :  no line number given')
	   return
	end if
    	nl = inum(1)
	if (nstrng.eq.1 .and. ifl.eq.1) goto 1001
	if (nstrng .eq. 2)              goto 1002
	xlast = 26.0
	xeoa = neoa
	if (nit(nl) .gt. 0) call addlin(r,point(nl), wd(nl), 
     *                                amp(nl), dmp(nl), xeoa)
	call disper
	if (xnum(3) .ne. 0.d0) wd(nl)= xnum(3) / (2000.0 * xpara)
	if (xnum(2) .ne. 0.d0) amp(nl)= xnum(2)
	if (xnum(4) .ne. 0.d0) dmp(nl)= 25.0*xnum(4) + 1.0
	if (dmp(nl) .lt. 1.0)  dmp(nl) = 1.0
	if (dmp(nl) .gt. 26.0) dmp(nl) = 26.0
	xparb = xnum(1)
	call wtop
	if (xnum(1) .ne. 0.d0) point(nl) = xparb
	if (nit(nl) .gt. 0)    call addlin(r,point(nl), wd(nl), 
     *                                  amp(nl), dmp(nl), -xeoa)
	goto 2000

 1001	call disper
C  Fix call for amplitude. Fix damping so parameter matches lsqfit. (GN Feb, 2015)
	if (alpha(1)(1:3) .eq. 'int') amp(nl) = xnum(1)
	if (alpha(1)(1:3) .eq. 'amp') amp(nl) = xnum(1)
	if (alpha(1)(1:3) .eq. 'dam') dmp(nl) = 25.0*xnum(1)+1.0
	if (alpha(1)(1:3) .eq. 'dmp') dmp(nl) = 25.0*xnum(1)+1.0
	if (alpha(1)(1:3) .eq. 'wid') then
	   wd(nl)  = xnum(1) / (2000.0 * xpara)
	end if
	if (alpha(1)(1:3) .eq. 'sig') then
	   xparb = xnum(1)
	   call wtop
	   point(nl) = xparb
	end if
	goto 2000

c editing tag or id field:
 1002	if (alpha(1) .eq. 'id') then
	   dent(nl) = ' '
	   dent(nl) = alpha(2)(1:32)
	end if
	if (alpha(1) .eq. 'tags') ctag(nl)(1:3) = alpha(2)(1:3)

* update plotter line list
 2000	continue 
	xparb = point(nl)
	call ptow
	call updone(nl,point(nl),real(xparb),amp(nl),wd(nl))
	return

	end

c **************************************************************************

	subroutine fitlines(r,tr,point,amp,wd,dmp,
     &                      eps1,eps2,eps3,eps4,eps5,
     &                      nit,nhold,ctag,dent)

	include 'datetc.h'
	include 'infmtn.h'
	include 'set.h'
	include 'linparms.h'
	include 'temp.h'

	real r(*),tr(*)
        real amp(*), wd(*), dmp(*)
	real eps1(*),eps2(*),eps3(*),eps4(*),eps5(*)
        integer nit(*), nhold(*)
        double precision point(*)
        character ctag(*)*4, dent(*)*32
   
	integer itrflag

	external itrflag

c fitlines  wstart wstop
c **********************

	if (ifl .ne. 2 .or. ifx .ne. 0) go to 9990
	call itron
	nstrng = 0
	wi = xnum(1)
	wf = xnum(2)
c use a 1/8  overlap
	dw = rdlen*0.875d0*disp
	nend = rdlen*istret/16
	
	do i=1,1024
c read wi - use "set" to set npts
	    ifl = 1
	    xnum(1) = wi
	    ifx = 0
	    call read(r,tr) 
 	    if (ipara .ne. 0) goto 1111
c getlines  - (ifl .ne. 2 OK)
	    call getlin(r,point,amp,wd,dmp,eps1,eps2,eps3,
     &                  eps4,eps5,nit,nhold,ctag,dent)
	    if (nol .eq. 0) go to 1100
c active
	    ifl = 0
	    ifx = 0
	    call holdat(2,0,r,point,amp,wd,dmp,nit,nhold,ctag)
	    if (ipara .eq. -999999) go to 1100
c check ends
	    nl = 1
1050	    if (point(nl) .lt. nend) then
	        nhold(nl) = nhold(nl) + 16
		nl = nl + 1
	        go to 1050
	    endif
	    nl = nol
1060	    if (point(nl) .gt. nop - nend) then
	        nhold(nl) = nhold(nl) + 16
		nl = nl - 1
	        go to 1060
	    endif
c lsqfit - use "set" to fix iteration number ahead of time
	    ifx = 0
	    call lsqdvr(r,point,amp,wd,dmp,
     &                  eps1,eps2,eps3,eps4,eps5,
     &                  nit,nhold,ctag)
 
c putlines
	    call putlin(point,amp,wd,dmp,eps1,eps2,eps3,
     &                  eps4,eps5,nit,nhold,ctag,dent)
1100	    wi = wi + dw
	    if (wi .ge. wf) goto 1111
	    call procpend
  	    if (itrflag() .eq. 1) then
	       call itroff
	       return
	    end if
	end do
 1111	call itroff
	return

9990	call wrtstr(' syntax error for fitlines command')
        call wrtstr('   FITLINES wstart wstop')
        call wrtstr('      - do LSQFIT on a file in datain')
        call wrtstr('      use  set "lsq itnum"  to set iterations')
        call wrtstr('      use  set readlength  to set read length')
        call wrtstr('      use  set "stretch" to set stretch factor')
        call wrtstr('      use  lsqfit filter if ringfit is wanted')
	return
	end
