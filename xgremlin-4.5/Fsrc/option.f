c \grammy\src\option.for   Ver. 1.0  Dump
c  Revision 1.09  11/16/93  readint fixed
c  Revision 1.08  09/05/93  dump no longer sets delw=1.0
c  Revision 1.07  07/05/93  dump writes to .dat, not .spe
c  Revision 1.06  06/28/93  'invtran' becomes real command 'inverstran'
c				correlation smoothed slightly
c  Revision 1.05  05/24/93  Add oddeven etc
c  Revision 1.04  07/09/92  DOS calls in Lahey format
c  Revision 1.03  28/09/91  Drop dot, box, plt
c  Revision 1.02  10/08/91  Invtran added
c  Revision 1.01  13/05/91  Dump added
c  Version  1.00  10/04/91


        subroutine option(r,tr,ffta,phz)

	include 'inparms.h'
	include 'infmtn.h'
	include 'iounit.h'
	include 'datetc.h'

        real r(*), tr(*), ffta(*), phz(*)

	character*1 sgnchar

        if (nalpha(1) .eq. 0) go to 998
        if (alpha(1)(1:nalpha(1)) .eq. 'zc') go to 7000
        if (alpha(1)(1:nalpha(1)) .eq. 'deglitch') go to 2000
        if (alpha(1)(1:nalpha(1)) .eq. 'oddeven') go to 4000
        if (alpha(1)(1:nalpha(1)) .eq. 'undooddeven') go to 4500
        if (alpha(1)(1:nalpha(1)) .eq. 'oddcorr') go to 4800
        if (alpha(1)(1:nalpha(1)) .eq. 'remcorr') go to 4900

        call wrtstr(' specified option not implemented')
        return
998     call wrtstr(' syntax error for option command')
	call wrtstr('  readint outfile file1 nbytes1 file2 nbytes2..')
        call wrtstr('  oddeven')
        call wrtstr('  oddcorr')
        call wrtstr('  undo')
        return

c option deglitch  - remove all glitches in list
c ***************  call - option deglitch filename
2000	continue
	open (19,file=alpha(2)(1:nalpha(2)),status='old')

2010	read (19,2011,end=2200) n1,n2,sgnchar
2011	format (2i8,1x,a1)
	nrise = (n2 - n1)/10
	if (nrise .lt. 4) nrise = 4
	if (nrise .gt. 128) nrise = 128
	nop = n2 - n1 + 1 + 2*nrise
	iobias = n1 - nrise - 1 
	iebias = noptex + 2 - iobias
c		create masked odd in r: rise width iparc, half width iparb
	do i=1,nop
	    r(i) = ffta(i + iobias)
	enddo
	iparb = nop/2
	iparc = nrise
	call mask(r)

	if (sgnchar .eq. '+') xm = 1.0
	if (sgnchar .eq. '-') xm = -1.0
c make the corrections
	do i=1,nop
	    ffta(i + iobias) = ffta(i + iobias) - r(i)
	    ffta(iebias - i) = ffta(iebias - i) + xm*r(i)
	enddo

c loop on list
	go to 2010

2200	return

c option ZC  fmultiplier
c ********* - find zerocrossings in 'r', difference in tr and exchange 
c  		quantize to clock ticks
7000	continue
	j = 0
	fmult = xnum(1)
	iparb = 0.0
	do i=2,nop
	  if (r(i) .ge. 0.0 .and. r(i-1) .lt. 0.0) then
	    ipara = iparb
c do a binary seek
	    del = 0.5
	    xparc = i - 1
	    do k=1,14
		xparc = xparc + del
		xparb = xparc
		call interp(r)
!	if (i .lt. 15) write (logfil,'(3f15.5)') xparc,del,xparb
		if (xparb .gt. 0.0) xparc = xparc - del
		del = 0.5*del
	    enddo
		iparb = fmult*(xparc + del)
c		xparb = xparc
c		call interp(r)
c		temp = xparb
c		xparb = xparc + 2.0*del
c		call interp(r)
c		xparb = xparc - xparb/(temp - xparb)
c		xparb = i - r(i)/(r(i) - r(i-1))
		if (j .gt. 0) tr(j) = iparb - ipara
!	if (i .lt. 15) write (logfil,'(2f12.5)') xparb,tr(j)
		j = j + 1
	  endif
	enddo
	nop = j - 1
	call rext(r,tr)
        return

c option oddeven
c **************

4000	continue
	call wrtstr(' forming even and odd interferograms')
	do i=1,noptex/2
	    j = noptex + 2 - i
	    t = ffta(i) - ffta(j)
	    ffta(j) = ffta(i) + ffta(j)
	    ffta(i) = t
	enddo
	ffta(noptex+1) = 2.0*ffta(noptex+1)
	return

c option undooddeven
c **************

4500	continue
	noptex = inum(1)
	if (noptex .eq. 0) noptex = 2097152
	do i=1,noptex/2
	    j = noptex + 2 - i
	    t = 0.5*(ffta(i) + ffta(j))
	    ffta(j) = 0.5*(ffta(j) - ffta(i))
	    ffta(i) = t
	enddo
	ffta(noptex+1) = 2.0*ffta(noptex+1)
c	call wrtstr(' doing  2nd forward transform')
c	call ffak(ffta,noptex)
c	nseq = 2
	return

c option oddcorr  find odd-even local correlation
c **************

4800	continue
	noptex = inum(1)
	if (noptex .eq. 0) noptex = 2097152
	nop = 2048
	nibl = noptex/(2*nop)
	write (wrtbuf,'(a,i8)') ' Correlation block length = ',nibl
        call wrtstr(wrtbuf)
	do i=1,nop
	    sumfg = 0.0
	    sumff = 0.0
	    sumgg = 0.0
	    do ib=1,nibl
		j = nibl*(i-1) + ib
		k = noptex + 2 - j
		sumfg = sumfg + ffta(j)*ffta(k)
		sumff = sumff + ffta(j)*ffta(j)
		sumgg = sumgg + ffta(k)*ffta(k)
	    enddo
	    tr(i) = 0.0
	    if (sumgg .ne. 0.0) tr(i) = sumfg/sumgg
	    tr(i+nop) = sqrt(sumff)
	    tr(i+3*nop) = sqrt(sumgg)
	enddo
c do a little binomial smoothing:
	r(1) = tr(1)
	r(2) = tr(2)
	do i=3,nop-2
	    r(i) = 0.375*tr(i) + .25*(tr(i+1) + tr(i-1))
     &			+ 0.0625*(tr(i-2) + tr(i+2))
	enddo
	r(nop-1) = tr(nop-1)
	r(nop) = tr(nop)
	return

c option remcorr  remove odd-even local correlation from odd
c **************

4900	continue
	do i=1,nop
	    sumff = 0.0
	    do ib=1,nibl
		j = nibl*(i-1) + ib
		k = noptex + 2 - j
		ffta(j) = ffta(j) - r(i)*ffta(k)
		sumff = sumff + ffta(j)*ffta(j)
	    enddo
	    tr(i+2*nop) = sqrt(sumff)
	enddo
	return


        end


