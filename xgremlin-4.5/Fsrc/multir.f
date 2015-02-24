c \grammy\src\multir.for  Ver. 2.2

c  Revision 2.39 93/09/28 Bug in ratadd default handling fixed
c  Revision 2.38 93/05/15 Bugs in ratadd header handling fixed
c  Revision 2.37 93/05/07 Conversion from Microsoft Fortran
c  Revision 2.36 92/10/31 Complex-high-resolution-ratio added
c  Revision 2.35 91/11/18 bin,max,min use wstart, wstop for defaults
c  Revision 2.34 91/02/08 bocode=1 allowed in ratadd except for Hi-res ratio
c  Revision 2.33 90/02/27 jwb exp added
c  Revision 2.32 90/02/17 nrtr used explicitly
c  Revision 2.31 89/11/17 jwb ratadd errors corrected
c  Revision 2.3  89/09/21 MCA extended log, absorbance, high res ratio added
c  Revision 2.2  89/02/05  

	subroutine extnded(r,tr)

	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'

	real r(*), tr(*)

	integer mnxflg, irdflg, nch

	character*30 string

	if (nstrng .le. 0) go to 999
	nch = nalpha(1)
	string = alpha(1)(1:nalpha(1))

	call lcase(string,nch)

	if (string .eq. 'max')		  go to 1000
	if (string .eq. 'min')		  go to 1010
	if (string .eq. 'bin')		  go to 1015
	if (string .eq. 'ratio')	  go to 6000
	if (string .eq. 'add')		  go to 6010
	if (string .eq. 'log')		  go to 6020
	if (string .eq. 'exp')		  go to 6030
	if (string .eq. 'absorbance')	  go to 6040
	if (string .eq. 'high-res-ratio') go to 6060
	if (string .eq. 'complex-high-res-ratio') go to 6070
	if (string .eq. 'multiply')	  go to 6080

999	call wrtstr(' specified extended command not implemented')
	return


c extended max [wstrt wstop]
c ************************
c create low resolution spectrum of filtered maxima of file on "datain"

c extended min [wstrt wstop]
c **************************
c create low resolution spectrum of filtered minima of file on "datain"

c extended bin [wstrt wstop]
c **************************
c create low resolution spectrum of filtered averages of file on "datain"
c 	output is saved in 20, r has log power

c						max entry pt.
1000	mnxflg = 1
	go to 1020
c						min entry pt.
1010	mnxflg = -1
	go to 1020
c						bin entry pt.
1015	mnxflg = 0
1020	if (ifl .eq. 0) then
	    xnum(1) = wstart
	    xnum(2) = wstop
	endif
	call bin (mnxflg,r,tr)
	return


c **** The following commands must be preceded by  "create filename"  ****
c The low resolution function (if used) is not written to dataout

c extended ratio  label [wstrt wend] 
c ********************************
c ratio a data file on "datain" to the low res'n function saved in 'label'

c extended add  label [wstrt wend]
c ******************************
c add a low resolution function saved in 'label' to the data file "datain"

c extended log  [wstrt wend]
c*************************
c Take the logarithm of the data file "datain"

c extended exp  [wstrt wend]
c*************************
c Take the exponential of the data file "datain"

c extended absorbance  [wstrt wend]
c *******************************
c Convert the data file "datain" to absorbance

c extended high-res-ratio [wstrt wend]
c **********************************
c Ratio the data file "datain" to the data file "altdata"

c extended complex-high-res-ratio [wstrt wend]
c **********************************
c Ratio the complex data file "datain" to the complex data file "altdata"

c extended multiply  wstrt wend constant
c **************************************
c Multiply the data file "datain" by a constant

c					1  ratio entry  
6000	irdflg = 1
	go to 6090
c					-1  add entry  
6010	irdflg = -1
	go to 6090
c					2  log entry  
6020    irdflg = 2
	go to 6090
c					6  exp entry  
6030    irdflg = 6
	go to 6090
c					3  absorbance entry  
6040    irdflg = 3
	go to 6090
c					4  high-res-ratio entry  
6060    irdflg = 4
	go to 6090
c					7  complex-high-res-ratio entry  
6070    irdflg = 7
	go to 6090
c					5  multiply entry  
6080    irdflg = 5
6090	if (ifl .lt. 2) then
	    xnum(3) = xnum(1)
	    xnum(1) = wstart
	    xnum(2) = wstop
	endif
	call ratadd (irdflg,r,tr)
	return

	end

c ************************

	subroutine bin(mnxflg,r,tr)

	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'
	include 'set.h'

	real r(*),tr(*)

	double precision strt, stop
	real sm, s
	real cf(15), con
	integer*4 nt, nr, mnxflg
	integer i, j, it, ilim
c	integer ia, ib, iab, ic
	integer itrflag

	external itrflag

	data cf		/	0.000055452,	0.000746586,
     *		0.006737947,	0.040762204,	0.165298888,
     *		0.449328964,	0.818730753,	1.0,
     *		0.818730753,	0.449328964,	0.165298888,
     *		0.040762204,	0.006737947,	0.000746586,
     *		0.000055452	/
	data con	/	0.252313616	/

	delw = disp
	strt = xnum(1)
	if (strt .lt. (wstart+7.0*delw)) strt = wstart+7.0*delw
	stop = xnum(2)
	if (stop .gt. (wstop-7.0*delw)) stop = wstop-7.0*delw
c find bin width nr to make final record >= 4096
	nt = (stop - strt - delw)/delw
	nr = (nt + 4095)/4096
	write(wrtbuf,9003)  nr
	call wrtout(output,wrtbuf)
9003	format(' bin width = ',i4)

c initialize loop, parameters for read
        ifl = 1
        ifx = 1
	nstrng = 0
	rftemp = rdfctr
	rdfctr = 1.0
	rdnorm = .false.
c read raw data for start 
        xnum(1) = strt - 7.0*delw
        inum(1) = 4096
        call read(r,tr)
        if (ipara .ne. 0) go to 996
	call disper
c	aps = xpara
	ip = 8
	it = 1
c start loop.  ip is pointer in current buffer
	do 2300 i=1,nt,nr
c 				time for new buffer?
    	   if (ip .gt. 3500) then
		xparb = ip - 7
		call ptow
       	 	xnum(1) = xparb 
		inum(1) = nt - i + 1
        	if (inum(1) .gt. 4096) inum(1) = 4096
        	call read(r,tr)
        	if (ipara .ne. 0) go to 996
		ip = 8
	   end if
c Start loop of one bin width (= one output point)
	   sm = -1.e+38 * mnxflg
	   ilim = ip + nr - 1
c process nr points
	   do 2250  j=ip,ilim
c Now filter each point, convoluting with exp(-i*i/5) to kill ringing
c Do loop on filtering one point
c		 s = 0.
c		 iab = j - 8
c		 do 2200 ic = 1, 15
c2200		    s = s + cf(ic) * r(iab+ic)
c The do loop took 4.6 sec/blk; the following code cuts it to 2.8
		s = cf(1)*(r(j-7) + r(j+7))
     * 		  + cf(2)*(r(j-6) + r(j+6))
     * 		  + cf(3)*(r(j-5) + r(j+5))
     * 		  + cf(4)*(r(j-4) + r(j+4))
     * 		  + cf(5)*(r(j-3) + r(j+3))
     * 		  + cf(6)*(r(j-2) + r(j+2))
     * 		  + cf(7)*(r(j-1) + r(j+1))
     * 		  +          r(j) 
c process filtered point
		 if (mnxflg .eq.  1) sm = amax1(sm,s)
		 if (mnxflg .eq.  0) sm = sm + s
		 if (mnxflg .eq. -1) sm = amin1(sm,s)
2250	    continue
	    if (mnxflg .eq. 0) sm = sm / float(nr)
c write point
	    tr(it) = con * sm
	    it = it + 1
	    ip = ip + nr
c exit requested?
	    call procpend
            if (itrflag() .eq. 1) go to 2305
2300    continue
 
c  All done
2305    write (wrtbuf,2311) it-1
	call wrtstr(wrtbuf)
2311	format (i6, ' output points')
	rdfctr = rftemp 

c clear remainder of tr
	if (it .le. 4096) then
	   do 2400 i = it, 4096
2400	   tr(i) = 0.0
	endif
c					exchange
	call rext(r,tr)
	nop = 4096
	wref = strt + 0.5*delw*(nr-1)
	delw = nr*delw
	call disper
c					save(19)
	call oscrwrite(19)
	call itroff
	return

996     write(wrtbuf,'(a,f12.4,a,i8)') ' read failed, wref=',xnum(1),
     &                                 '     nop=',inum(1)
	call wrtstr(wrtbuf)
	call itroff
        return
	end

c ----------------------------------------------------------------------

c slopecorr	correct smoothed bin maximum spectrum for slope error
c *********

	subroutine slpcor(r,tr)

	include 'datetc.h'
	include 'infmtn.h'

	real r(*), tr(*)

	if (ifx .ne. 1) then
	   call wrtstr(' syntax error')
	   return
	end if
	temp = inum(1)
	temp = 0.25 * (temp-1.0) / temp
	tr(1) = r(1)
	tr(nop) = r(nop)
	do i=2, nop
	   tr(i) = r(i) - temp * abs(r(i+1)-r(i-1))
	end do
c	exchange r and tr
	call rext(r,tr)
	return
	end

c ***************************************************************

	subroutine ratadd (irdflg,r,tr)

	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'
	integer itrflag

	real r(*), tr(*)

	double precision wstrt, wend, wr
	integer nbstart, nbstop, nbout, i, nop2, ir, irdflg
	character*30 modlabel

	external itrflag

c	output file must have been CREATEd

	call itron
	modlabel = '  added'
	if (irdflg .eq. 1) modlabel = 'ratioed'
	if (irdflg .eq. 2) modlabel = 'logarithm'
	if (irdflg .eq. 3) modlabel = 'absorbance'
	if (irdflg .eq. 4) modlabel = 'high resolution ratio'
	if (irdflg .eq. 5) modlabel = 'multiply'
	if (irdflg .eq. 6) modlabel = 'exponential'
	if (irdflg .eq. 7) modlabel = 'complex high-res ratio'

	nsav = inum(1)
	wstrt = xnum(1)
	if (wstrt .lt. wstart) wstrt = wstart
	wend = xnum(2)
	if (wend .gt. wstop) wend = wstop
c save high-res .hdr in 19
c *****watch order in which files are opened - datain LAST
	nstrng = 0
	call oscrwrite(19)

c copy multiplication factor xnum(3) into uvar(1)
        if (irdflg .eq. 5)uvar1 = xnum(3)

c *****noffset not used here yet
	nbstart = nwpp*(wstrt -wstart)/disp
	nbstart = 4*nbstart
	nbstop = nwpp*(wend -wstart)/disp
	nbstop = 4*nbstop
	nbout = 0

	DO 6800 ir = nbstart, nbstop, 4*nrtr
	    nop2 = (nbstop - ir)/4
	    if (nop2 .gt. nrtr) nop2 = nrtr
	    wr = wstrt + 0.25*disp*(ir-nbstart)/nwpp
	    write (wrtbuf,'(2h +,f10.3,i9)') wr, nop2
	    call prevline(wrtbuf)

c		    if needed, recall low resolution and interpolate to tr
	    if (irdflg .lt. 2) then
	       call oscrread(nsav)
	       call disper
	       
	       do 6100 i = 1, nop2
		  xparb = wr + disp*(i-1)
		  call wtop
		  j = xparb
		  x = xparb - j
c 				use constant extension of ends if needed
		  if (j .lt. 1) then
		     j = 1
		     x = 0.
		  end if
		  if (j .gt. nop-1) then
		     j = nop - 1
		     x = 0.
		  end if
 6100	       tr(i) = r(j) + x*(r(j+1) - r(j))
	    end if
c	read record
	    call readata(datain, ir, r(1), 4*nop2, nerr)
* if requested, change the byte order. 
	    if ( bocode .ne. mbocode ) then
	       call chibord( r, nop2 )
	    end if
	    if (irdflg .eq. 4 .or. irdflg .eq. 7) then
	       	call readata(altdat, ir, tr(1), 4*nop2, nerr)
		if ( bocode .ne. mbocode ) then
		   call chibord( tr, nop2 )
		end if
	     end if

	    if (nerr .ne. 0) go to 997

c now perform the desired operation:
c					ratio
	    if (irdflg .eq. 1) then
		do 6200 i=1,nop2
6200		r(i) = r(i)/tr(i)
c					or add
	    elseif(irdflg .eq. -1) then
		do 6300 i=1,nop2
6300		r(i) = r(i) + tr(i)
c					or log
	    elseif(irdflg .eq. 2) then
		do 6400 i=1,nop2
               	if (r(i) .gt. 0.0e0) then
                      r(i) = alog( r(i))
                else
                      r(i) = 0.0e0
                endif
6400            continue
c					or exp
	    elseif(irdflg .eq. 6) then
		do 6450 i=1,nop2
               	if (r(i) .gt. -100.0e0) then
                      r(i) = exp( r(i))
                else
                      r(i) = 0.0e0
                endif
6450            continue
c					or absorbance
	    elseif(irdflg .eq. 3) then
		do 6500 i=1,nop2
         	if (r(i) .gt. 0.0e0) then
                      r(i) = - alog( r(i))
                else
                      r(i) = 0.0e0
                endif
6500            continue 
c					or high resolution ratio
	    elseif(irdflg .eq. 4) then
		do 6600 i=1,nop2
                if(tr(i) .ne. 0.0e0) then
                      r(i) = r(i)/tr(i)
                else
                      r(i) = 0.0e0
                endif
6600            continue
c					or complex high resolution ratio
	    elseif(irdflg .eq. 7) then
		do i=1,nop2,2
		    amsq = tr(i)**2 + tr(i+1)**2
                    if(amsq .ne. 0.0e0) then
                      temp = (r(i)*tr(i) + r(i+1)*tr(i+1))/amsq
                      r(i+1) = (-r(i)*tr(i+1) + r(i+1)*tr(i))/amsq
		      r(i) = temp
                    else
                      r(i) = 0.0e0
                      r(i+1) = 0.0e0
                    endif
		enddo
c					or multiply
	    elseif(irdflg .eq. 5) then
	       do 6700 i=1,nop2
		  r(i) = r(i)*uvar1
 6700	       continue
	    end if

c	    write
	    call wrtdata(dataot, nbout, r(1), 4*nop2, nerr)
	    if (nerr .ne. 0) go to 998
	    nbout = nbout + 4*nop2
	    call procpend
	    if (itrflag() .eq. 1) goto 6805
 6800	continue

c update .hdr for dataout 
C (gn)        call scrread(19)
6805    continue
	call oscrread(19)
	wstop  = (nbstop/4)*disp/nwpp + wstart - disp
	wstart = (nbstart/4)*disp/nwpp + wstart
	npo = (nbstop - nbstart)/4
	delw = disp
	i = bocode
	bocode = mbocode
	call infwrt(dataot)
	bocode = i
	if (irdflg .lt. 2) write (dataot,6991) modlabel,cdat,ctim
6991	format (' File ',a7,' to low resolution function ',a10,2x,a10)
	if (irdflg .ge. 2) write (dataot,6993) modlabel,cdat,ctim
6993	format (a28,' applied to file ',a10,2x,a10)
	call itroff
	return

997	call wrtstr(' error on data read - abort')
	call itroff
	return

998	call wrtstr(' error on data write - abort')
	call itroff
	return

	end

c  ****************************************

