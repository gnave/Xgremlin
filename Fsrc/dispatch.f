	subroutine dispatch( cmdstr, r, tr, ffta, phz,
     &                       point, amp, wd, dmp, eps1, eps2, eps3,
     &                       eps4, eps5, nit, nhold, ctag, dent )

* decodes and executes a command

	include 'iounit.h'
	include 'infmtn.h'
	include 'altinf.h'
	include 'extinf.h'
	include 'set.h'
	include 'inparms.h'
	include 'outparms.h'
	include 'linparms.h'
	include 'datetc.h'
	include 'plot.h'
	include 'phase.h'

* parameter types
	integer nit(*), nhold(*)
	real r(*), tr(*), ffta(*), phz(*), amp(*), wd(*), dmp(*)
	real eps1(*), eps2(*), eps3(*), eps4(*), eps5(*)
	double precision point(*)
	character ctag(*)*4, dent(*)*32

* local variables
	logical incmd            ! T if a command is being processed
	character cmdstr*(*)     ! the command string from Xgremlin
	character csiz*1
	integer nc1r(10),nc2r(10)
	integer ibeg, iend, n, ierr
	real dcr(10),rln
	double precision xtemp
	logical crflg(10)                                                 
	real avar(4)                                                 

	integer ia,ib,j,nop2

	common /kbdlock/ incmd

	equivalence (nc1r(1),mfregn(1,1)), (nc2r(1),mfregn(1,2)),         
     &              (dcr(1),mfregn(1,3)), (crflg(1),mfregn(1,4))         
	equivalence (avar(1),acorrp)                                      

* check if a command is currently being processed
	if ( incmd ) then
	   return
	end if
	incmd = .true.
 
c read and decode command

	cmdpnd = .false.
	call setline( cmdstr )

 7	call gboss	
  	call cputime( ctim )
	write (wrtbuf,2) ctim, kin(1:nc)
	call wrtout(logfil,wrtbuf)
	if (lecho) then
	   call wrtout(output,wrtbuf)
	end if
 2	format (1x,a9,3x,a)
	if (nact .le. 0) then
	   incmd = .false.
	   return
	end if
	if (nact .gt. 0) goto 10
 3	if (keybd) then
	   incmd = .false.
	   return		! to command editor
	end if
	call gboss

* if any bad phase points have been marked then write a bad points file
	if ( l_bad ) then
	   call markbad
	end if

c check for BEGIN
	if (nact .eq. 4) then
	   incmd = .false.
	   return		! to command editor
	end if

c check for BYE,END
 4	if (nact .eq. 1 .or. nact .eq. 2) goto 90000
	goto 3

*            1     2     3     4     5     6     7     8     9     10
 10	goto (
     &	   90000,90000,  300,    1,  500,  600,  700,  800,  900, 1000,
     1	    1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000,
     2	    2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000,
     3	    3100, 3200, 3300, 3400, 3500, 3600, 3700, 3800, 3900, 4000,
     4	    4100, 4200, 4300, 4400, 4500, 4600, 4700, 4800, 4900, 5000,
     5	    5100, 5200, 5300, 5400, 5500, 5600, 5700, 5800, 5900, 6000,
     6	    6100, 6200, 6300, 6400, 6500, 6600, 6700, 6800, 6900, 7000,
     7	    7100, 7200, 7300, 7400, 7500, 7600, 7700, 7800, 7900, 8000,
     8	    8100, 8200, 8300,    1, 8500, 8600, 8700, 8800, 8900, 9000,
     9	    9100, 9200, 9300, 9400, 9500, 9600, 9700, 9800, 9900,10000,
     &	   10100,10200,10300,10400,10500,10600,10700,10800,10900,11000,
     1	   11100,11200,11300,11400,11500,11600,11700,11800,11900,12000,
     2	   12100,12200,12300,12400,12500,12600,12700,12800,12900,13000,
     3	   13100,13200,13300,13400,13500,13600,13700,13800,13900,14000,
     4     14100,14200,14300,14400,14500,14600,14700,14800,14900,15000,
     5     15100,15200,15300,15400,15500,15600,15700,15800,15900,16000,
     6      5700 ) nact
 

c active (itn>0) all lines in list contribute to fit and may be 
c ****** refined by lsq fit unless frozen by a hold call.  
c        call - active (list)    For form of list, see hold
500	call holdat(2,0,r,point,amp,wd,dmp,nit,nhold,ctag)
	if (ipara .eq. -999999) goto 3
	goto 1


c add   add a constant to r(i).  calls - add  const
c ***
 600	if ( ifl .eq. 0 ) then
	   call wrtstr(' Error :  missing real operand')
	   goto 1
	end if
	if ( ifx .eq. 0 ) then
	   ibeg = 1
	   iend = nop
	else if ( ifx .eq. 1 ) then
	   ibeg = inum(1)
	   iend = nop
	else if ( ifx .eq. 2 ) then
	   ibeg = inum(1)
	   iend = inum(2)
	end if
	if ( ibeg .gt. iend ) then
	   call wrtstr(' Error :  <ibeg> greater than <iend>')
	   goto 1
	end if
	if ( ibeg .lt. 1 )   ibeg = 1
	if ( iend .gt. nop ) iend = nop
	if ( ibeg.ge.r_size .or. iend.ge.r_size ) then
	   call wrtstr(' Error :  outside array boundaries.')
	   goto 1
	end if
	do i=ibeg,iend
	   r(i) = r(i) + xnum(1)
	end do
	goto 1


c aircorr     apply correction to delw and wref for index of air.
c *******     this correction is done during the output phase.

700	acorrf = .true.
	if (ifl .eq. 0) then
	   acorrp = pspect
	   acorrt = tspect
	   gcorr = 0.0
	   acorrh = hspect
	else
	   do j=1,ifl
	      avar(j) = xnum(j)
	   end do
	end if
	goto 1


c alias		create a synonym for a command string
c *****
800	call aliasub
	goto 1


c apodize     apply an apodizing function to the interferogram.
c *******     Parameter list has been kept general to allow various 
c	      functions later; only inum(1) currently used

900	if (nstrng .ge. 1) then
	   call apoption(r,tr)
	   goto 1
	endif
	apodfl = .true.
	if (ifx .eq. 0) apod1 = -1.0
	if (ifx .eq. 1) apod1 = inum(1)
	if (ifx .eq. 0 .and. ifl .eq. 1) apod1 = xnum(1)
	apod2 = 0.0
	if (ifx .eq. 2) apod2 = inum(2)
	modeapod = 1
	goto 1


c area	compute mean, and the sum of the r array
c ****
1000	call area(r)
	goto 1


c asymmetric
c **********
1100	symflg = .false.
	goto 1


c bubbles  turn bubble help on and off
c **********
 1200   continue
        if (nstrng .eq. 0) then
           call bubbles(1)
           goto 1
        end if
        if (alpha(1).eq.'off') then
           call bubbles(0)
        else if (alpha(1).eq.'on') then
           call bubbles(1)
        end if
        goto 1


c begin  used with batch files to re-start after an abort
c *****


c alloc  allocate memory for arrays
c *****
1300   	continue
	if ( nstrng .eq. 0 ) then
	   nstrng = 1
	   alpha(1) = 'size'
	   nalpha(1) = 4
	   call dinfo
	   goto 1
	end if
	if ( ifx .gt. 0 ) then
	   ia = inum(1)
	else if ( ifx.eq.0 .and. nstrng.gt.1 ) then
	   n = length( alpha(2) )
	   csiz = alpha(2)(n:n)    ! last character
	   if ( csiz.eq.'k' .or. csiz.eq.'K' ) then
	      ia = 1024
	   else if ( csiz.eq.'m' .or. csiz.eq.'M' ) then
	      ia = 1024 * 1024
	   else
	      call wrtstr(' Error :  error in array size.')
	      goto 1
	   end if
	   alpha(2)(n:n) = ' '     ! remove multiplier
	   read(alpha(2),*) n
	   ia = n * ia
	else
	   call wrtstr(' Error :  array size not specified.')
	   goto 1
	end if

	if ( alpha(1) .eq. 'r' ) then
	   if ( ia .lt. 131072 ) ia = 131072  ! minimum size of r
	   ia = ia + 2                        ! need two more
	   call rtralloc( ia, ierr )
	   if ( ierr .eq. 0 ) then
	      r_size = ia
	      tr_size = ia
	      rtr_size = 2 * ia
	      phz_size = (ia-2)/2             ! must be a power of 2
	      nphzmax = phz_size
	      nrtr = r_size - 2
	      n2rtr = log(real(ia-2)) / log(2.0) + 0.5
	   end if
	else if ( alpha(1) .eq. 'fft' ) then
	   ia = log(real(ia)) / log(2.0) + 0.5
	   ia = 2**ia
	   if ( ia .lt. 262144 ) then
	      ia = 262144	              ! minimum size of ffta
	      call wrtstr(' Warning :  fft array size set to 262144')
	   end if
	   call fftalloc( ia, ierr )
	   if ( ierr .eq. 0 ) then
	      ffta_size = ia
	      nfftamax = ffta_size
	   end if
	else if ( alpha(1) .eq. 'lines' ) then
	   call linalloc( ia, ierr )
	   if ( ierr .eq. 0 ) then
	      linbuf_size = ia
	      nlins = ia
	   end if
	else
	   call wrtstr(
     &        ' Error :  invalid array name (use r, fft or lines).')
	   goto 1
	end if
	call shwsiz(ia)      ! display new array sizes
	goto 1


c change change parameters of line nl to wave, amp, wd, dmp
c ****** calls - change  nl wave amp wd dmp 
c        values of 0. for wa, amp, wd, and dmp act as place holders
c                    - change  nl 'tags' '4 char of new tags'
c                    - change  nl 'id' '32 char of new id'
1400	call chnglin(r,point,wd,amp,dmp,nit,ctag,dent)
	goto 1


c clear        set all of r(i) to 0.  call - clear
c *****
1500	if ( ifx .eq. 2 ) then
	   ibeg = inum(1)
	   iend = inum(2)
	else
	   ibeg = 1
	   iend = nrtr+2
	end if
	if ( ibeg .gt. iend ) then
	   call wrtstr(' Error :  <ibeg> greater than <iend>')
	   goto 1
	end if
	if ( ibeg .lt. 1 )      ibeg = 1
	if ( iend .gt. nrtr+2 ) iend = nrtr+2
	do i=ibeg, iend
	   r(i) = 0.0
	end do
	call clrfrm
	goto 1


c close close files.  call - close <unitname>
c *****
1600	call dclose
	goto 1


c cmplx	perform extended complex operations on r
c ***** 
1700	call complx(r,tr)
	call clrfrm
	goto 1


c combine      take weighted sum of r and tr - r(i) = a*r(i) +b*tr(i)
c *******      calls - combine(a,b), combine.  latter assumes a=b=1.0
1800	xparb = 1.0
	xpara = 1.0
	if (ifl .eq. 2) then
	   xpara = xnum(1)
	   xparb = xnum(2)
	end if
	do i=1,nop
	   r(i) = xpara*r(i) + xparb*tr(i)
	end do
	goto 1


c complex     output will be in real-imaginary form
c *******
 1900	if (nseq .ne. 2) then
	   write(wrtbuf,9801)
	   call wrtstr(wrtbuf)
	end if
	cmplex = .true.
	realfl = .false.
	imagfl = .false.
	call ftsout(r,ffta)
	goto 1


c connect	replace a section of data in R by a straight 
c *******	line connecting two specified points.
2000	call conect( r )
	goto 1


c hfs   runs hyperfine structure routines
c ***   FIX PARAMETERS (ALMOST NONE SHOULD BE PASSED)
2100    call hfs(r,tr,delw,wstart,nint,ntrans,ifl,ifx,inum, 
     &           xnum,alpha,nalpha,nstrng,freespec)
	goto 1


c convolute    convolute the data using a transform previously formed and
c *********    saved with a kernel command.  params as in restore.
c 		calls - convolute  nrise  ncut  ,
c   convolute  nrise  ncut  igw - (obsolete)gaussian filter, gaussian width igw
c   convolute  nrise  ncut  'gaussian' gw - gaussian filter, gaussian width gw
c   convolute  nrise  ncut  'boxcar' bxw - boxcar filter, width bxw
c   convolute  nrise  nsen  'voigt' ampnoise - voigt  approx. to optimum filter
2200	if (ifx .eq. 3) then
	   ifl = 1	
	   xnum(1) = inum(3)
	   ifx = 2
	   nstrng = 1
	   alpha(1)(1:8) = 'gaussian'
	end if
	call trnmod(-1,r,tr,ffta)
	goto 1
	

c copy           copy wstrt wend  or  copy.  Copies wstrt to wstop 
c ****           inclusive from datain (or all of datain) to dataout.
2300	call dcopy(r)
	goto 1


c create       create and initialize a new data output file
c ******       call - create "file name" (without extension)
2400	call dstart(ia)
	goto 1


c data       modify r(i) to (val), i=ni to nf by nd. 
c ****       if not present, assumes nf=ni, nd=1.
c			call - data  ni  nf  nd  val  
2500	call setdata(r,tr,ffta,phz)
	goto 1
	

c deglitch    remove glitches from the input file.
c ********    the points designated on the deglitch command will be
c             left as they are.
2600	do i=1,10
	   iexmpt(i) = 0
	end do
	deglch = .true.
	if (ifl .gt. 0) xsigma = xnum(1)
	nexmpt = ifx
	if (ifx .eq. 0) goto 1
	do i=1,ifx
	   iexmpt(i) = inum(i)
	end do
	goto 1


c derivative
c **********
2700	call deriv(r,tr)
	goto 1


c dispar	display line parameters, starting at line n
c ******        call - dispar n   Default is n=1
2800	call dispar(point,amp,wd,dmp,eps1,eps2,eps3,
     &              nit,nhold,ctag)
	goto 1


c divide       divide r(i) by a constant.  call - divide  const [iconst]
c ******     
2900	if (ifl .eq. 0 .and. ifx .eq. 1) xnum(1) = inum(1)
	if (xnum(1) .eq. 0.0) then
	   call wrtstr('attempt to divide by zero - abort')
	   goto 1
	endif
	do i=1,nop
	   r(i) = r(i)/xnum(1)
	enddo
	goto 1


c drop         (itn<0) all lines in list are dropped from fit, but 
c ****         parameters are retained in table.  call - drop(list)
c 		For form of list, see hold
3000	call holdat(3,0,r,point,amp,wd,dmp,nit,nhold,ctag)
	if (ipara .eq. -999999) goto 3
	goto 1


c end        close files and exit  see end of program
c *** 


* erase 
* *****
*
* Usage:
*
*   erase lines    : erases an internal line list
*
3100	continue
	if ( nstrng .ne. 0 ) then
	   if (alpha(1)(1:5) .eq. 'lines') then
	      nol = 0
	      call linevis( 0 )	! flag line markers invisible
	      call llerase	! wipe out old plotter line list
	   end if
	end if
	goto 1


c exchange     exchange r and tr.  call - exchange
c ********
3200	call rext(r,tr)
	goto 1


c exp   exponentiate - r(i) = exp( r(i)*log(base) )
c ***
3300	if (ifx .ne. 0) then   ! multiply r by ln(ifx) first
	   rln = log(real(inum(1)))
	   do i=1,nop
	      r(i) = r(i)*rln
	   enddo
	endif
	call expr(r)            ! exponentiate r
	goto 1


c extend      set flag to double or further extend input array
c ******
 3400	ixtend = 2
	if (ifx .ge. 1) ixtend = inum(1)
	if (ixtend .le. 1) ixtend = 2
	goto 1


c extended     operations that work on extended portions or the whole file
c ********
3500	call extnded(r,tr)
	call clrfrm
	goto 1


c files		list open files
c *****
3600	call dfiles
	goto 1


c filter       filter the data, masking with rise width (nrise) to avoid 
c ******       end effects.  calls -
c	filter  nrise  ncut   - sharp cut-off filter at ncut
c	filter  nrise  ncut  igw - (obsolete)gaussian filter, gaussian width igw
c	filter  nrise  ncut  'gaussian' gw - gaussian filter, gaussian width gw
c	filter  nrise  ncut  'boxcar' bxw - boxcar filter, width bxw
c	filter  nrise  nsen  voigt  ampnoise - voigt  approx. to optimum filter
c	       nsen = freq. where signal=noise.  ampnoise = amp. of white noise
3700	if (ifx .eq. 3) then
	   ifl = 1	
	   xnum(1) = inum(3)
	   ifx = 2
	   nstrng = 1
	   alpha(1)(1:8) = 'gaussian'
	end if
 	call trnmod(0,r,tr,ffta)
	goto 1


c findlines    find all lines stronger than (smin) 
c *********    calls - findlines  smin  mode    
c			(see linelist for explanation of mode)
c		findlines smin , findlines.  latter assumes smin = 0.0
c               npt,wref,and nord must be correct- use ref call if needed
3800	call findl(r,point,amp,wd,dmp,
     &             eps1,eps2,eps3,eps4,eps5,nit,nhold,ctag,dent)
 	goto 1


c diff   r = r - tr
c ****
3900  	if ( ifx .eq. 2 ) then
	   ia = inum(1)
	   ib = inum(2)
	else
	   ia = 1
	   ib = nop
	end if
	do i=ia,ib
	   r(i) = r(i) - tr(i)
	end do
	goto 1


c fitlines    fit all lines in given range
c ********    call - fitlines  smin  smax
4000   call wrtstr('Fitting lines in data file')  	
       if(data_is .eq. 'Complex' .or. data_is .eq. 'complex') then
             call wrtstr('Complex data file - lines not fitted')
             goto 1
	endif
        call fitlines(r,tr,point,amp,wd,dmp,
     &                eps1,eps2,eps3,eps4,eps5,
     &                nit,nhold,ctag,dent)
	goto 1


c fourtran	compute fourier transform, form amp and arg as well.
c ********	call - fourtran
4100	call fortrn(r,tr,1)
	nwpp = 2          ! flag as complex
	call clrfrm
	goto 1


c getlines        get lines from .lin file
c ********
4200  	continue
	if ( ifl .gt. 0 ) ifl = 0   ! do not allow to specify range interactively
	call getlin(r,point,amp,wd,dmp,eps1,eps2,eps3,
     &              eps4,eps5,nit,nhold,ctag,dent)
 	goto 1


* dump   write the contents of the r-array to a proper data file
* ****
* Usage: dump "array" ni nop "file name (no ext)"
*
 4300	continue
	call dump(r, tr, ffta, phz)
	goto 1


c hold         (ihold=16) all parameters of lines in list are to be held at
c ****         current values - lsq fit will ignore these lines.
c	call - hold(list)
c	list is of the form l1, l2,..., where the elements l are either:
c	a line number, nl, or
c	a range of line numbers, written as nl1,-nl2
c	an empty list implies 1, -nol
c	list may be qualified by "c", where c is any character
c *** hold must follow active ***
4400  	call holdat(1,0,r,point,amp,wd,dmp,nit,nhold,ctag)
	if (ipara .eq. -999999) goto 3
	goto 1


c holdamp     see holddamping
c *******
4500	call holdat(4,8,r,point,amp,wd,dmp,nit,nhold,ctag)
	if (ipara .eq. -999999) goto 3
	goto 1


c holddamping   calls - holddamping list con , holddamping list 
c ***********   for form of list, see hold
c		con positive - lock damping of all lines in list at (con)
c		con zero or absent - lock at current value
c		con negative - unlock at current value
4600	call holdat(4,1,r,point,amp,wd,dmp,nit,nhold,ctag)
	if (ipara .eq. -999999) goto 3
	goto 1


c  holdsigma  see holddamping
c  *********
4700	call holdat(4,4,r,point,amp,wd,dmp,nit,nhold,ctag)
	if (ipara .eq. -999999) goto 3
	goto 1


c holdwidth   see holddamping
c *********   con.ge.1.0 interpreted as width in milliangstroms
c               con.lt.1.0 interpreted as width/wavelength
4800	call holdat(4,2,r,point,amp,wd,dmp,nit,nhold,ctag)
	if (ipara .eq. -999999) goto 3
	goto 1


c imaginary   output will be imaginary only
c *********
 4900	if (nseq .ne. 2) then
	   write(wrtbuf,9801)
	   call wrtstr(wrtbuf)
	end if
	imagfl = .true.
	cmplex = .false.
	realfl = .false.
	call ftsout(r,ffta)
	goto 1


c info  change parts of information block.  calls -
c ****  info name [ new value ]
5000	call dinfo
	goto 1


c input read information block, skip blocks if necessary, read
c ***** in n points following skip, skip to eof if necessary.
 5100	nseq = 1
 5101	call ftsin(ieret,r,tr,ffta,phz)
	if (ieret .eq. 0) goto 1
	call wrtstr(
     &       ' Input error -  Searching for begin or end command.')
	goto 3


c browser   spawn HTML browser with help text
c *********   
5200	continue
	call htmlbrowser( alpha(1) )
	goto 1


c insert       insert a new line into table
c ******       call - insert wave amp wd dmp itn "up to 30 char of id" 
5300	call inslin (nl,r,point, amp, wd, dmp,
     &               eps1,eps2,eps3,eps4,eps5,nit,nhold,ctag,dent)
 	goto 1


c interpolate  interpolate data on a linear scale at spacing delw
c ***********  starting at point n or wavelength (wo), and going (nint) 
c              steps or to wavelength (wf) - 8192 points maximum
c              calls - interpolate delw wo nint, interpolate delw wo wf,
c              interpolate delw n nint.  if nint is not present, routine
c              will do as many points as possible, but no more than 8192
c              interpolate delw   -ditto, with n=3
c              **if delw is negative, it is interpreted as a wavenumber 
c              step. For wo, use highest wavenumber wanted, for wf, lowest.
5400	call intpl(r,tr)
	call clrfrm
	goto 1


c invtran      do inverse transform.  call - invtran
c *******      
5500    call invtrn(r)
	nwpp = 1
	call clrfrm
	goto 1


c kernel stretch kernel function in memory to nop points, centran,
c ****** label. call - kernel  nop
5600	call kernel(r,tr)
	call clrfrm
	goto 1


c keyboard      redirect the command input to the terminal keyboard.
c ********      this MUST be the LAST command on a line
5700	input = stdin
	keybd = .true.
	call newprom
	goto 1


c length       change length of record to (n).  call - length  n  
c ******
5800	nop = inum(1)
 	call strtrn
	goto 1


c linelist	produce a findlines list extending over a number of records
c ********	
5900	call linel(r,tr)
	goto 1


c log   take logarithm of r(i) = ln(r(i))/ln(b). 
c ***
6000	xpara = -10.0
	if (ifl .eq. 1) xpara = xnum(1)
	do i=1,nop
	   if (r(i) .gt. 0.0) then
	      r(i) = log( r(i) )
	   else
	      r(i) = xpara
	   end if
	enddo
	if (ifx .eq. 0) goto 1       ! divide r by log(ifx)
	rln = log(real(inum(1)))
	do i=1,nop
	   r(i) = r(i)/rln
	enddo
	goto 1


c lowres write a short file (with header) containing low res data
c ******
6100	call lores(r,ffta,phz)
	goto 1


c lsqfit      adjust line parameters by least-squares fit
c ******      call - lsqfit ntimes  nlow  nhigh  continuum  width factor  
c              if not present, width factor assumed to be 5.0, cont 1.0
c              (nlow,nhigh) may be dropped as a pair - 1,nol assumed
6200	call lsqdvr(r,point,amp,wd,dmp,
     &              eps1,eps2,eps3,eps4,eps5,nit,nhold,ctag)
	call updatelin(point,amp,wd,tr)  ! tr is for temp storage
	call plotr(1,r,r,tr,phz) 
	goto 1


c mask    multiply r(i) by a mask of rise width (nrise) and an optional
c ****    Gaussian of width gwidth, after optionally setting the average
c         calls - mask  nrise  [average]  ['gw' gwidth]
c         If average is specified, mean is changed to (average) before masking.
 6300	if (ifl .eq. 1 .and. nstrng .eq. 0   .or.
     & 		alpha(1)(1:2) .eq. 'gw' .and. ifl .eq. 2) then
	   call momnts(r)
	   xpara = xnum(1)-avg
	   do i=1,nop		! add xpara to r
	      r(i) = r(i) + xpara
	   end do
	   write(wrtbuf,'(a,f12.4)') ' Average set to ',xnum(1)
	   call wrtstr(wrtbuf)
	end if
c       multiply by Gaussian if 'gw'
	if (nstrng .eq. 1 .and.	alpha(1)(1:2) .eq. 'gw') then
	   gwidth = xnum(1)
	   if (ifl .eq. 2) gwidth = xnum(2)
	   if (gwidth .eq. 0.0) then
	      call wrtstr('Error :  Gaussian width is zero.')
	      goto 1
	   endif

	   nh = nop/2
	   do i = 1, nop
	      temp = 1.665109222*float(i - nh - 1)/gwidth
	      temp =  temp*temp
	      if (temp .lt. 70.0) then
		 temp =  exp(-temp)
	      else
		 temp = 0.0
	      endif
	      r(i) = r(i)*temp
	   end do
	   write (wrtbuf,'(a,f12.4)') ' Gaussian width = ',gwidth
	   call wrtstr(wrtbuf)
	endif

	iparb = nop/2		! mask, rise width iparc, half width iparb
	iparc = inum(1)
	call mask(r)
	goto 1
	

* background   determine continuous background and subtract
* **********   background <nbins> <thresh> [subtract] [onlysubtract]
*                                          [median] [average]
*
* ia : subtract/onlysubtract
* ib : median/average
6400    continue
        if (ifx .lt. 1) then
           call wrtstr(
     &          ' Error :  number of bins not specified.')
           goto 1
        end if
        if (ifl .eq. 0) then
           call wrtstr(' Error :  discriminator not specified.')
           goto 1
        end if

	ia = 0   ! dont subtract
	ib = 1   ! use median
	do i=1,nstrng
           if (alpha(i)(1:3) .eq. 'sub') then
	      ia = 1
           end if
           if (alpha(i)(1:4) .eq. 'only') then
	      ia = 2
           end if
           if (alpha(i)(1:4) .eq. 'aver') then
	      ib = 0
           end if
           if (alpha(i)(1:4) .eq. 'medi') then
	      ib = 1
           end if
        end do

	call background(r,tr,phz,
     &                  inum(1),10,real(xnum(1)),ib,ia)
        goto 1


c move		move bytes from one array to another
c ****		call - move 'src' <srcindex> 'dest' <destindex> <nwords>
6500	call movearray(r,tr,ffta,phz)
	call clrfrm
	goto 1


c multifile	options that work on two or more files of data
c *********
6600	continue
*       call multif
	goto 1


c multiply     multiply r(i) by a real constant.  call - multiply  const
c ********
 6700	if ( ifl .eq. 0 ) then
	   call wrtstr(' Error :  missing multiplier')
	   goto 1
	end if
	if ( ifx .eq. 0 ) then
	   ibeg = 1
	   iend = nop
	else if ( ifx .eq. 1 ) then
	   ibeg = inum(1)
	   iend = nop
	else if ( ifx .eq. 2 ) then
	   ibeg = inum(1)
	   iend = inum(2)
	end if
	if ( ibeg .gt. iend ) then
	   call wrtstr(' Error :  <ibeg> greater than <iend>')
	   goto 1
	end if
	if ( ibeg .lt. 1 )   ibeg = 1
	if ( iend .gt. nop ) iend = nop
	do i=ibeg,iend
	   r(i) = r(i)*xnum(1)
	end do
	goto 1


c ncenter
c *******
 6800	incntr = 0
	if (ifx .ge. 1) incntr = inum(1)
	goto 1
	

c nocorr      set flag for no correction
c ******
 6900	phcorfl = .false.
	goto 1


c noise
c *****
 7000	call noise(r)
	call clrfrm
	goto 1


c normalize    normalize data to given values at 1 - 3 given points, or
c *********    at maximum absolute value.  calls -
c		normalize npt val , normalize npt1 val1 npt2 val2
c		normalize npt1 val1 npt2 val2 npt3 val3
c		normalize value  - scales so that max. abs. val. = value
c		normalize with no params scales so that max.abs.val. = 1.0
c		normalize a b  divides by (a + b*i) so that normalization 
c		from one scan can be used on another related scan
c		normalize a b c  uses a quadratic
 7100	call norml(r)
	goto 1


c onesided
c ********
 7200	phcorfl = .true.
	onesidfl = .true.
	goto 1


c open		open files.  call - open unitname filename
c ****
 7300	call gopen(ffta)
	goto 1


c option       do it yourself.  call - option [your parameters]
c ******
 7400	call option(r,tr,ffta,phz)
	goto 1

* font  change font used in the plotter window.
* ****
* font small|large font-name
 7500	continue
	if (nstrng .ne. 2) then
	   call wrtstr(' Error :  syntax error in font command.')
	   goto 1
	end if
	ia = 0
	if (alpha(1)(1:3).eq.'sma') then
	   ia = 1
	end if
	if (alpha(1)(1:3).eq.'lar') then
	   ia = 2
	end if
	if (ia .eq. 0) then
	   call wrtstr(' Error :  wrong command parameter.')
	   goto 1
	end if
	call plotfont(ia, alpha(2))
	goto 1

c index
c *****
 7600	call srwli(r,ffta)
	goto 1


* pause   not needed any longer, gets ignored gracefully
* ******   
 7700	continue
	goto 1


c phcorr   set flag and params for phase correction
c ******   calls:
c	   phcorr 'fit' nterms cutfrac fracnoise [npts] [fraclim] [tol]
c	   phcorr 'spline' nterms cutfrac fracnoise [npts] [fraclim] [tol]
c     or   phcorr 'smooth' nwidth    (phapodization width)
c     or   phcorr                    (use default phapodization width)
7800    continue
	phcorfl = .true.
	if (ifx .eq. 0 .and. nstrng .eq. 0) then
	   phapdz = .true.
	   nphap = 0
	endif
	if (alpha(1)(1:nalpha(1)) .eq. 'smooth') then
	   phctyp = 0
	   phapdz = .true.
	   nphap = inum(1)
           if (ifx .eq. 2) then
	      nphi = 2
	      iphparms(2) = inum(2)
            endif
	endif
	if (alpha(1)(1:nalpha(1)) .eq. 'fit') then
	   phctyp = 1
	   phapdz = .false.
	   nphr = ifl
	   if (nphr .ne. 0) then
	      do i=1,nphr
		 phparms(i) = xnum(i)
	      end do
	   endif
	   nphi = ifx
	   if (nphi .ne. 0) then
	      do i=1,nphi
		 iphparms(i) = inum(i)
	      end do
	   endif
	endif
	if (alpha(1)(1:nalpha(1)) .eq. 'spline') then
	   phctyp = 2
	   phapdz = .false.
	   nphr = ifl
	   if (nphr .ne. 0) then
	      do i=1,nphr
		 phparms(i) = xnum(i)
	      end do
	   endif
	   nphi = ifx
	   if (nphi .ne. 0) then
	      do i=1,nphi
		 iphparms(i) = inum(i)
	      end do
	   endif
	endif
	goto 1
	

c photons/cm-1
c ************
7900	call etop(r)
	goto 1


c planck       Calculate the Planck function for temperature T
c ******       call - planck sigo sigf delsig T
8000	call planck(r)
	call clrfrm
	goto 1


c plot         plot r(i), mean of every (nmean) points between ni and nf
c ****         from xmin to xmax.  call - plot xmin xmax ni nf nmean 
c              any or all parameters may be dropped, but (xmin,xmax) and
c              (ni,nf) must be dropped in pairs.  if not present,
c              implied values are (0.0, 1.0, 1, nop, 1)
 8100   call plotr(0,r,r,tr,phz)       ! plot r array in edit mode
 	goto 1


* atlas  create input file for psplot
* *****
 8200	call atls(r,tr)
        goto 1


c power
c *****
8300	call pwrtrn(r,tr)
	call clrfrm
	goto 1


c UNUSED
c ******
8400	continue
	goto 1


c printparams
c ******
8500	call prprms(point, amp, wd, dmp,
     &              eps1,eps4,eps5,nit,nhold,ctag,dent)
	goto 1


c printout    set parameters for printout
c ********
 8600	iprnt = 1
	if (ifx .ge. 1) iprnt = inum(1)
	goto 1


c process     repeat the next inum(2) lines inum(1) times.  if inum(2)
c *******     			is zero, repeat only the next line.
 8700	continue
	call wrtstr(' Error :  command not supported in Xgremlin')
	goto 1

c product	multiply each element in r by the corresponding
c *******	element in tr and leave the result in r.  If an integer
c			argument is given, recall that record to tr first.
 8800	if (ifx .gt. 1 .or. ifl .ne. 0) goto 9998
	if (ifx .eq. 1) then
	   ia = inum(1)
	   if (ia .lt. 1 .or. ia .ge. 30) then
	      call wrtstr(' Error :  illegal recall label.')
	      goto 1
	   end if
	   ia = nop
	   call scrread(inum(1), ia, tr)  ! keep current 'nop'
	   write (wrtbuf,'(1x,a)') id
	   call wrtstr(wrtbuf)
	end if
	do i = 1, nop
	   r(i) = r(i) * tr(i)
	end do
	goto 1


c putlines        replace lines in .lin file
c ********
8900	call putlin(point,amp,wd,dmp,eps1,eps2,eps3,
     &              eps4,eps5,nit,nhold,ctag,dent)
	goto 1


c range       specifies a range protected from de-glitching.
c *****       If only one number is specified, the range is from there to 
c	      the end.  If two numbers are given, they are used as a range.
9000  if (ifx .le. 0) goto 9998
	irng1 = inum(1)
	irng2 = inum(2)
	if (ifx .lt. 2)   irng2 = 16*1024*1024
	goto 1


c ratio        take ratio of r and tr - r(i) = r(i)/tr(i)
c *****        call - ratio [zeroval] [n].  If an integer 
c		argument is given, recall that record to tr first.
 9100	if (ifx .gt. 1 .or. ifl .gt. 1) goto 9998
	if (ifx .eq. 1) then
	   ia = inum(1)
	   if (ia .lt. 1 .or. ia .ge. 30) then
	      call wrtstr(' Error :  illegal recall label.')
	      goto 1
	   end if
	   ia = nop
	   call scrread(inum(1),ia,tr)   ! keep current 'nop'
	   write (wrtbuf,'(1x,a)') id
	   call wrtstr(wrtbuf)
	end if
	xpara = 0.0
	if (ifl .eq. 1) xpara = xnum(1)		
	if (alpha(1)(1:7) .ne. 'complex') then
	   do i=1,nop
	      if (tr(i) .ne. 0.0) then
		 r(i) = r(i)/tr(i)
	      else
		 r(i) = xpara
	      end if
	   enddo
	else
	   do i=1,nop,2
	      ampsq = tr(i)**2 + tr(i+1)**2
	      if (ampsq .ne. 0.0) then
		 temp = r(i+1)
		 r(i+1) = (r(i+1)*tr(i) - r(i)*tr(i+1))/ampsq
		 r(i) = (r(i)*tr(i) + temp*tr(i+1))/ampsq
	      else
		 r(i) = xpara
		 r(i+1) = 0
	      end if
	   enddo
	endif
	goto 1


c rclphase    pick up saved phase array to use in the next phase correction.
c ********    If a filename is given, run that deck to get the phase array
9200	rclphz = .true.
	if (nstrng .ne. 0) goto 9210
	jj = nphz/2 + 1
	do j=1,jj
	    phz(j) = svphz(j)
	end do
	goto 1

9210	call comin
	do j=1,nop
	    phz(j) = r(j)
	end do
	nphz = 2*nop
	write(wrtbuf,'(a,i4)') ' nphz set to ',nphz
	call wrtstr(wrtbuf)
	goto 1


c read         read a standard format record from disk
c ****
9300	call read(r,tr) 
	call clrfrm
 	if (ipara .eq. 0) goto 1
	if (.not. batch) goto 1
	goto 90000


c readdata
c *********	read in data into the R array using the format specified
c		Uses "input" unit which may be redirected via  run  or
c		explicitly on the readdata command line.
c		calls:	readdata "format"
c			readdata "format" "unit-name"
9400	call rdcard(r)
	call clrfrm
	goto 1


c readlines    read line parameters from a file, put in .lin
c *********    call - readlines filename ('old' or 'syn') [dw  smul] 
9500	call rdlines
	goto 1


c readparams    read parameters from cards, print
c **********    calls - readparams  dw  smul  , readparams
c                all wavelengths are multiplied by (1.0 + dw), and all 
c	 	 intensities are multiplied by smul.
c                readparams dw smul "c"  - all lines tagged c, where
c                c is any character.
c                ****this must be last command on card ***********
9600	call rdprms(r,point,amp,wd,dmp,eps1,eps2,eps3,
     &              eps4,eps5,nit,nhold,ctag,dent)
	if (ipara .eq. -999999) goto 3
	goto 1


c readphase   read values from cards into the saved phase array.
c *********   currently reads 16 cards of 8 values per card.
c             readphase must be the last command on the card and the
c             next card must be the first card of phase values in
c             8f10.0 format.
 9700	continue
	jj = nphz/2
	read(input,9705,end=9710) (svphz(j),j=1,jj)
 9705	format(bz,8f10.0)
 9710	svphz(jj+1) = 0.0
	goto 1


c real        output will write real parts only
c ****
 9800	if (nseq .ne. 2) then 
	   call wrtstr('  ')
	   write(wrtbuf,9801) 
	   call wrtstr(wrtbuf)
	   call wrtstr('  ')
	end if
 9801	format(' WARNING :   No transform. ',
     &  'Output file will contain untransformed real data.')
	realfl = .true.
	cmplex = .false.
	imagfl = .false.
	call ftsout(r,ffta)
	goto 1


c recall       recall data stored by save.  call - recall  label
c ******
 9900	continue
	if (ifx .eq. 0) goto 9998
	if ( inum(1).lt.0 .or. inum(1).gt.99 ) then
	   call wrtstr(' Error :  illegal recall label')
	   goto 1
	end if
	if ( nstrng .eq. 0 ) then   ! recall to r array
	   if ( ifx .eq. 1 ) then
	      nop = -1
	   else
	      nop = inum(2)
	   end if
	   call scrread( inum(1), nop, r )	   
	   call clrfrm
	else
	   if ( alpha(1) .eq. 'r' ) then
	      if ( ifx .eq. 1 ) then
		 nop = -1  ! read them all
	      else
		 nop = inum(2)
	      end if
	      call scrread( inum(1), nop, r )	   
	   else if ( alpha(1) .eq. 'tr' ) then
	      if ( ifx .eq. 1 ) then
		 nop = -1  ! read them all
	      else
		 nop = inum(2)
	      end if
	      call scrread( inum(1), nop, tr )	   
	   else
	      goto 9998
	   end if
	end if
	write(wrtbuf,'(1x,a)') id
	call wrtout(output,wrtbuf)
	goto 1


* response
* ********  calculate an optical response function and put it in the r array
* Usage:  response "standard.data" [<normalization factor>] [sigma]
* the standard lamp spectrum must be in the 'r' array. After the call to
* the subroutine, the standard lamp spectrum will be saved in 'tr'.
10000   call response(r,tr)
        goto 1

 
c rem		remark field (use alpha field). No operation
c ***
10100	goto 1


c remove  remove a line from the internal table used by lsqfit.
c ******
10200	call rmlin(r,
     &             point, amp, wd, dmp,
     &             eps1,eps2,eps3,eps4,eps5,
     &             nit, nhold, ctag, dent)
	goto 1


c replacelines
c ******
10300	continue
	goto 1


c replot  does nothing in Xgremlin
c ******  call - replot
10400	goto 1


c rescale      undo the standard input scaling.  call - rescale
c *******      if used, this should follow the read command
10500	do i=1,nop
	   r(i) = r(i) * scal
	end do
	goto 1


c restore      restore the data using the kernel function transform
c *******      previously formed and saved with a kernel command.
c	filter options as above.  calls - restore nrise ncut ,
c	restore  nrise  ncut igw - (obsolete)gaussian filter, gaussian width igw
c	restore  nrise  ncut  'gaussian' gw - gaussian filter, gaussian width gw
c	restore  nrise  ncut  'boxcar' bxw - boxcar filter, width bxw
c	restore  nrise  nsen  voigt  ampnoise - voigt  approx. to optimum filter
10600	if (ifx .eq. 3) then
		ifl = 1	
		xnum(1) = inum(3)
		ifx = 2
		nstrng = 1
		alpha(1)(1:8) = 'gaussian'
	end if
	call trnmod(1,r,tr,ffta)
	call clrfrm
	goto 1


c reverse      reverse direction of data in r(i).  call - reverse
c *******
10700	npt = nop + 1 - npt
	ia = nop / 2
	do i = 1, ia
	    ib = nop + 1 - i
	    temp = r(i)
	    r(i) = r(ib)
	    r(ib) = temp
	end do
	wref = wref + (nop - 1) * delw
	delw = -1. * delw
	call clrfrm
	goto 1


c rewind	rewind the file on specified unit.
c ******	call - rewind "unit name"
10800	call drewnd
	goto 1


c run
c ***
10900	call comin
	goto 1


c save  save data in memory on scratch file with temporary label.
c ****  call - save  label   ' new ident'    
c       label must be 1-99
11000	iparc=0
        if ( ifx .eq. 0 ) goto 9998
	if ( inum(1).lt.1 .or. inum(1).ge.99 ) then
	   call wrtstr(' Error :  illegal save label')
	   goto 1
	end if
	if (inum(1).eq.17 .or. inum(1).eq.18 .or. inum(1).eq.19) then
	   call wrtstr(' Error :  reserved scratch file (17,18,19)')
	   goto 1
	end if
	if ( ifx .eq. 1 ) then
	   if ( nstrng .eq. 0 ) then
	      call scrwrite(inum(1),nop,'   ',r)
	   end if
	   if ( nstrng .eq. 1 ) then
	      if (alpha(1).eq.'tr') then
		 call scrwrite(inum(1),nop,'   ',tr)
	      else
		 call scrwrite(inum(1),nop,alpha(1),r)
	      end if
	   end if
	   if ( nstrng .eq. 2 ) then
	      if (alpha(1).eq.'tr') then
		 call scrwrite(inum(1),nop, alpha(2),tr)
	      else
		 call scrwrite(inum(1),nop, alpha(2),r)
	      end if
	   end if
	else
	   if ( nstrng .eq. 0 ) then
	      call scrwrite( inum(1),inum(2),'   ',r)
	   end if
	   if ( nstrng.eq.1 ) then
	      if (alpha(1).eq.'tr') then
		 call scrwrite(inum(1),inum(2),'   ',tr)
	      else
		 call scrwrite(inum(1),inum(2),alpha(1),r)
	      end if
	   end if
	   if ( nstrng .eq. 2 ) then
	      if (alpha(1).eq.'tr') then
		 call scrwrite(inum(1),inum(2), alpha(2), tr)
	      else
		 call scrwrite(inum(1),inum(2), alpha(2), r)
	      end if
	   end if
	end if
	goto 1

c snap  
c ****
11100	continue
	iprnt = 0
	cmplex = .false.
	realfl = .false.
	imagfl = .false.
	call ftsout(r,ffta)
	goto 1


c savephase   save the present phase array for later use in phase
c *********   correction of later scans.
11200  jj = nphz/2 + 1
	j1 = 1
	if (ifx .lt. 1) goto 11202
	if (ifx .lt. 2) goto 9995
	j1 = (jj * (inum(1)-1) / float(inum(2))) + 1.5
	jj = (jj *  inum(1)    / float(inum(2))) + 0.5
11202 	do j = j1,jj
	    svphz(j) = phz(j)
	end do
	goto 1


c screen  redirect output back to keyboard screen.
c ******
11300	output = stdout
	goto 1


c set		set default parameters
c ***
300	call dset
	goto 1


c setregion
c *********
11400	iprm = 1
	ixprm = 1

11405	if(ifx .le. 0) goto 1
	nfregn = nfregn + 1
	nc1r(nfregn) = inum(iprm)
	nc2r(nfregn) = inum(iprm+1)
	ifx = ifx - 2
	iprm = iprm + 2
	crflg(nfregn) = .false.
	if(ifl .le. 0) goto 11405
	dcr(nfregn) = xnum(ixprm)
	crflg(nfregn) = .true.
	ixprm = ixprm + 1
	ifl = ifl - 1
	goto 11405


c shift        shift data in r(i), assumed periodic.
c *****        calls - shift  distance  , shift  nsteps. 
c			use integer when possible
11500	call shft(r,tr)
	goto 1


c sinc
c ****
11600	call sinc(r)
	call clrfrm
	goto 1


c skip        set up to skip n words at beginning of scan
c ****
11700   if (ifx .eq. 0) goto 9995
	inskip = inum(1)
	goto 1


c slopecorr	correct smoothed bin maximum spectrum for slope error
c *********
11800	call slpcor(r,tr)
	goto 1


c spool  open a temporary file to hold printed output,
c *****  this will be directed to a remote(i.e. line printer) terminal.
11900	if (uopen(spool) .ne. 1) goto 9990
	output = spool
	goto 1


c standards  use an external file of standard lines to find wavcorr
c *********    call - standards filename
12000	call stds(r,tr,point,amp,wd,dmp,eps1,eps2,eps3,
     &            eps4,eps5,nit,nhold,ctag,dent)
	goto 1


c stretch stretch data a factor of ns by Fourier interpolation
c *******
12100	call strtch(r,tr)
	goto 1


c symmetric
c *********
12200	symflg = .true.
	goto 1

* zerofill
* ********  pad an array in the r array with zeros up the next power of 2
12300	continue
	nop2 = 1
 1235	nop2 = 2 * nop2
	if ( nop2 .gt. nop ) goto 1245
	goto 1235
 1245	if ( nop2 .gt. r_size ) then
	   call wrtstr(' Error :  too many points for r array')
	   return
	else
	   write(wrtbuf,'(a,i7)') ' Zero padding to nop =',nop2   
	   call wrtstr(wrtbuf)
	end if
	do j=nop+1,nop2
	   r(j) = 0.0
	end do
	ia = nop   ! save old nop
	nop = nop2 
	if ( nstrng.eq.1 .and. alpha(1)(1:5).eq.'shift') then
	   ifl = 0
	   ifx = 1
	   inum(1) = ( nop - ia ) / 2   ! shift to center
	   call shft(r,tr)
	end if
	call clrfrm
	goto 1

c transform   perform transform based on previously set flags
c *********
12400	if (nseq .eq. 1) then
	   nseq = 2
	   call trans(r,ffta,phz)
	   incntr = -1
	   inskip = 0
	   ixtend = 1
	   xsigma = 10.0
	   irng1 = 10000000
	   irng2 = 10000000
	   goto 1
	else
	   write(wrtbuf,*) 
     &       ' No input file found for transform.',
     &	     ' Searching for begin or end command.'
	   call wrtstr(wrtbuf)
	   goto 3
	endif


c twosided
c ********
12500	phcorfl = .true.
	onesidfl = .false.
	goto 1


c voigt		generate a voigt function of unit height, Lorentzian
c *****		width w or wl, Gaussian width wg, centered in nop pts.
c	calls:	voigt wl wg nop 
c		voigt w nop ncenter  generate Lorentzian centered on ncenter
c		voigt w nop  same as above with ncenter = nop / 2 + 1
12600	call voit(r,tr)
	call clrfrm
	goto 1

 
c wavelimits   set or clear wavenumber bounds for output
c **********
12700	if (ifl.eq.1 .or. ifl.gt.2) goto 9995
	if (ifl .eq. 0) then  ! clear wavenumber bounds
	   xwstart = 0.0
	   xwstop = 0.0
	else                  ! set them
	   xwstart = xnum(1)
	   xwstop = xnum(2)
	end if
	goto 1


c whats       find out what is in a particular array element of r or tr.
c *****
12800	call whats(r,tr,ffta,phz)
	goto 1


c writefile write data in ascii format to an output file.  Calls - 
c ********* writefile  filename [nop] ["format"]
12900	call writec(r)
	goto 1


c writelines write lines in ascii format to an output file.  Calls - 
c ********** writelines filename [wstrt wstop]
13000	call wrtlin(r)
	goto 1


c writeparams   write parameters to outfile  
c ***********   call - writeparams [ dw smul ] 
13100	call puprms(point,amp,wd,dmp,nit,nhold,ctag,dent)
	goto 1


* vactoair
* ********
* calculate air wavenumber, refractive index of air etc for a given wavenumber.
* vactoair  sigma  pspect  tspect  hoh  (pressures in Torr, temperatures in deg C)
13200	continue
	if ( xnum(1) .eq. 0.d0 ) then
	   call wrtstr(' Error :  missing wavenumber.')
	   goto 1
	end if
	if ( xnum(1) .gt. 50000.d0 ) then
	   call wrtstr(' Error :  wavenumber must be < 50000')
	   goto 1
	end if
	if ( xnum(2) .eq. 0.d0 ) xnum(2) = 760.d0   ! 760 Torr
	if ( xnum(3) .eq. 0.d0 ) xnum(3) = 20.d0    ! 20 degrees C
	call vactoair(xnum(1), xnum(2), xnum(3), xnum(4))
	goto 1


c movecenter	move ncenter by inum(1); recalculate phase
c ***********   call - movecenter npts
13300	incntr = ncenter + inum(1)
	call newcentr(ieret)
	goto 1


c inverstran Perform an inverse transform on the spectrum in ffta
c **** 	     call - inverstran [noptex]
13400	continue
	if (noptex .eq. 0) noptex = inum(1)
	if (noptex .eq. 0) noptex = 2097152
	call ffsk(ffta(1),noptex)
	goto 1


* sincos calculate sin and cos functions in the R and TR arrays
* ******
13500   continue
	call sincos(r,tr)
	goto 1


c decimate Reduce the size of the r array by decimation by idec
c ******** call - decimate idec
13600	call decimate(r)
	goto 1


c setnoise  Set epsran in the .lin file over a given wavenumber range
c ********  call - decimate idec
13700	call setnoise(r,point,amp,wd,dmp,eps1,eps2,eps3,
     &                eps4,eps5,nit,nhold,ctag,dent)
	goto 1

*
* ----- new things follow
*

* rmscratch  remove a scratch file
* *********  Usage:  rmscratch <scratch file numbers>
13800	continue
	if ( ifx .eq. 0 ) then
	   call wrtstr(' Error :  missing parameter')
	   goto 1
	end if
	do j=1,ifx
	   call rmscr( inum(j) )
	end do
	goto 1

* stom  store markers
* ****
* Usage stom [r|b]  (default: all) store positions of currently set markers
13900	continue
        if ( nstrng .gt. 0 ) then
           if ( alpha(1)(1:1).eq.'r' ) then
              call stom( 0 )
           else if ( alpha(1)(1:1).eq.'b' ) then
              call stom( 1 )
           end if
        else
           call stom( 0 )
           call stom( 1 )
        end if
        goto 1

* recm  recall stored markers
* ****
* Usage recm [r|b]  (default: red) store positions of currently set markers
14000   continue
        if ( nstrng .gt. 0 ) then
           if ( alpha(1)(1:1).eq.'r' ) then
              call recm( 0 )
           else if ( alpha(1)(1:1).eq.'b' ) then
              call recm( 1 )
           end if
        else
           call recm( 0 )
           call recm( 1 )
        end if
        goto 1

*
* writeasc write out data in r array in ascii format ready for e.g. GLE
* ******
14100	continue
	if ( ifx .eq. 0 ) then
	   inum(1) = n_left          ! use current window
	   inum(2) = n_right
	   ifx = 2
	end if
	if ( nstrng.lt.1 .or. ifx.ne.2 ) then
	   call wrtstr( ' Error :  wrong point range.' )
	   goto 1
	end if
	if ( nstrng .lt. 2 ) then
	   call writeasc( alpha(1), "!", inum(1), inum(2), r )
	else
	   if ( alpha(2)(1:1).ne.'#' .and.
     &          alpha(2)(1:1).ne.';' .and.
     &          alpha(2)(1:1).ne.'!' ) then
	      call writeasc( alpha(1), "!", inum(1), inum(2), r )
	      call writegle( alpha(1), alpha(2), inum(1), inum(2) )
	   else
	      call writeasc(alpha(1),alpha(2)(1:1),inum(1),inum(2),r)
	   end if
	end if
	
	goto 1

* readasc  load an ascii file into the r array
* ****
* Usage:  readasc <filename> <x-col> <y-col> <z-col> ['add']
14200	continue
	if ( nstrng.eq.0 ) then
	   call wrtstr(' Error :  file name missing.')
	   goto 1
	end if
	if ( nstrng.eq.2 .and. alpha(2).eq.'add' ) then
	   ia = 1
	else
	   ia = 0
	end if
	if ( ifx.eq.1 ) then    ! uses ffta for temp storage
	   call readasc(alpha(1),inum(1),0,0,1,ia,r,tr,ffta)
	else if ( ifx.eq.2 ) then
	   call readasc(alpha(1),inum(1),inum(2),0,2,ia,r,tr,ffta)
	else if ( ifx.eq.3 ) then
	   call readasc(alpha(1),inum(1),inum(2),inum(3),3,ia,r,tr,ffta)
	else
	   call readasc(alpha(1),1,2,0,2,ia,r,tr,ffta)
	end if
	call clrfrm
	goto 1

* printer  set the printer for screen dumps etc.
* *******
14300	continue
	if ( nstrng .gt. 0 ) then
	   call setprt( alpha(1) )
	else
	   call shwprt   ! display current printer
	end if
	goto 1

* fitpoly  fit a polynomial to a region of the r array described by markers
* *******
14400	continue
	if ( ifx.ne.0 ) then   
	   ib = 0
	   call fitpoly(r, tr, phz, inum(1), ib)
	else
	   ia = 1
	   ib = 0
	   call fitpoly(r, tr, phz, ia, ib)        ! fit a straight line
	end if
	goto 1


* ls    list the contents of the current directory
* *****
14500	continue
	call lsdir
	goto 1

* color/colour  configure colours in the plotting window
* ************
14600	continue    ! color
14700	continue    ! colour - for those on the right side of the Atlantic
	if ( nstrng .eq. 2 ) then
	   call setcol( alpha(1), alpha(2) )
	else
	   call wrtstr( ' Error :  wrong # of parameters.' )
	end if
	goto 1


* moments  print the moments up to a specified order of a line
* *******
*
* Usage:
*       moments <n>
* prints the moments up to n-th order (default n=3)
* the center of the line must be marked with a marker and width must be set
14800	continue
	if ( ifx .eq. 0 ) then
	   ia = 3
	   call moments( ia, r )
	else
	   call moments( inum(1), r )
	end if
	goto 1


* profile  set a line profile for subsequent 'integrate' commands
* *******
*
* Usage:
*       profile spline <smoothing-factor (real)> 
*       profile triangle
*       profile gauss
*       profile voigt
*       profile lorentz
*       profile hold .....
*       profile free
*
14900	continue
	call defprof
	goto 1


* subpoly  subtract a fitted polynomial (continuum/background) from the r array
* *******  the background is modelled with a Chebychev polynomial
* 
* Usage:
* subpoly [<order of polynomial>]
*
15000	continue
	if ( nstrng .gt. 0 ) then
	   if ( alpha(1)(1:4) .eq. 'rest' ) then
	      ia = -1
	      ib = 1
	      call fitpoly(r, tr, phz, ia, ib)  ! restore the r array
	      goto 1
	   end if
	end if
	if ( ifx.ne.0 ) then   
	   ib = 1
	   call fitpoly(r, tr, phz, inum(1), ib)
	else
	   ia = 1
	   ib = 1
	   call fitpoly(r, tr, phz, ia, ib)      ! subtract a straight line
	end if
	goto 1


* integrate  integrate / fit a line or a set of lines 
* *********  starting values are determined by markers
15100	continue
	call integprof(r,tr,phz)
	goto 1


* fitprofile  fit a single profile or a set of them and print parameters
* **********
15200	continue
	goto 1


* button assign a command to a button
* ****** 
* syntax:
*        button <num> <type> <"label"> <"command">
*        button <num> <type> <"label"> "<Modifier>+<char>"
* e.g.
*        button 1 plot area "M+a"               for a plot command
*        button 1 edit sinc "sinc 10. 200"      for an edit mode button
*        buttons are numbered 1-8
15300	continue
	if ( ifx.ne.1 .or. nstrng.ne.3 ) then
	   call wrtstr( ' Error :  malformed button command.' )
	else
	   if ( alpha(1)(1:4) .eq. 'plot' ) then
	      call butplot( inum(1), alpha(3), alpha(2) )
	   end if
	   if ( alpha(1)(1:4) .eq. 'edit' ) then
	      call butedit( inum(1), alpha(3), alpha(2) )
	   end if
	end if
	goto 1

* wplot plot between specified range of wave numbers
* ****  syntax is: wplot <begin> <end>
15400	continue
	if ( ifl .lt. 2 ) then
	   call wrtstr(' Error :  too few parameters' )
	   goto 1
	end if
	if ( nstrng.eq.1 .and. alpha(1)(1:2).eq.'nm' ) then ! convert
	   xparb = xnum(1)
	   call wtos
	   xnum(1) = xparb
	   xparb = xnum(2)
	   call wtos
	   xnum(2) = xparb
	   if ( xnum(1) .gt. xnum(2) ) then
	      xtemp   = xnum(1)
	      xnum(1) = xnum(2)
	      xnum(2) = xtemp
	   end if
	end if
	call plotr(2,r,r,tr,phz)
	goto 1


* goto  go to a specified wavenumber in the buffer and center plot around it
* ****  syntax is: goto <centre> [<width>] (both are floating pt arguments
15500	continue   ! translate into a wplot command
	if ( nstrng.eq.1 .and. alpha(1)(1:2).eq.'nm' ) then ! convert
	   xtemp = xnum(1)
	   xparb = xnum(1)        ! center
	   call wtos
	   xnum(1) = xparb
	   if ( ifl .gt. 1 ) then
	      xparb = xtemp + 0.5 * xnum(2)  ! range
	      call wtos
	      xnum(2) = 2.d0 * dabs( xparb - xtemp )
	   end if
	end if

* handle jumps to lines directly
	if ( nstrng.eq.1 .and. alpha(1)(1:4).eq.'next' ) then
	   ifx = 1
	   inum(1) = lstlin + 1
	   if (inum(1).gt.nol) inum(1) = nol
	end if
	if ( nstrng.eq.1 .and. alpha(1)(1:4).eq.'prev' ) then
	   ifx = 1
	   inum(1) = lstlin - 1
	   if (inum(1).eq.0) inum(1) = 1
	end if
	if ( ifx .eq. 1 ) then    ! go to that line
	   if ( nol.gt.0 .and. inum(1).le.nol ) then
	      xparb = point(inum(1))
	      call ptow
	      xtemp = xparb
	      xnum(1) = xtemp - 0.5 * ( w_right - w_left )
	      xnum(2) = xtemp + 0.5 * ( w_right - w_left )
	      ifl = 2
	      lstlin = inum(1)    ! store last visited line
	      goto 15501
	   end if 
	end if

	if ( ifl .eq. 1 ) then    ! same range as before
	   xtemp = xnum(1)
	   xnum(1) = xtemp - 0.5 * ( w_right - w_left )
	   xnum(2) = xtemp + 0.5 * ( w_right - w_left )
	   ifl = 2
	else
	   xtemp = xnum(1)
	   xnum(1) = xtemp - xnum(2)/2.0
	   xnum(2) = xtemp + xnum(2)/2.0
	   ifl = 2
	end if
15501	call plotr(2,r,r,tr,phz)
	goto 1

* echo  echo a string to the text window
* ****
15600	call echostr
	goto 1

* plotsize  change the size of the screen dump
* ********
15700	continue
	if ( nstrng .eq. 1 ) call pltsiz( alpha(1), 1 )
	if ( nstrng .eq. 2 ) then
	   if ( alpha(2)(1:4).eq.'land' ) then
	      call pltsiz(alpha(1),1)
	   else if ( alpha(2)(1:4).eq.'port' ) then
	      call pltsiz(alpha(1),0)
	   else 
	      call wrtstr(' Error :  wrong parameter')
	   end if
	end if
	goto 1

* cd    change working directory
* **
15800	call chngdir( alpha(1) )
	goto 1


* pwd   print working directory
* ***
15900	call pwdir
	goto 1


* break synonymous to keyboard
* *****
16000	input = stdin
	keybd = .true.
	call newprom      ! write a prompt if text was written
	goto 1


c ERROR MESSAGES

 9990	call wrtout(output,' Error :  unit is not open.')
	goto 1

 9995	call wrtout(output,' You goofed :  Syntax error.')
        call wrtout(output,' Searching for begin or end command.')
	goto 3

 9998	call wrtout(output, ' You goofed :  Syntax error.')
	goto 1

90000	call dexit

 1	continue
	if (cmdpnd) goto 7
	if (keybd) then
	   incmd = .false.
	   call butunb          ! unblock configurable buttons
	   return		! to command editor
	end if
	goto 7
	end

*------------------------------------------------------------------------

	subroutine unlkbd
*
* remove a keyboard command lock
*
	logical incmd
	common /kbdlock/ incmd

	incmd = .false.

	return
	end

*------------------------------------------------------------------------

* Revision history:
* -----------------
* $Log: dispatch.f,v $
* Revision 1.73  1996/09/11 03:10:28  ulf
* also pass tr array to complx function
*
* Revision 1.72  1996/09/07 04:58:44  ulf
* fixed an error message
*
* Revision 1.71  1996/09/07 03:37:57  ulf
* small cosmetic fix
*
* Revision 1.70  1996/08/30 03:04:19  ulf
* size of phase array 'phz_size' was not set to a power of two in an 'alloc r'
* command --> phase correction failed.
*
* Revision 1.69  1996/08/22 03:59:18  ulf
* fixed the alloc command to correctly set the variable 'n2rtr' which is
* number of points in the r array as 2^n2rtr. This allows 'fourtran' commands
* of arbitrary length.
*
* Revision 1.68  1996/08/08 02:47:30  ulf
* fixed a bug in the 'alloc' command: 1MB is 1024*1024
*
* Revision 1.67  1996/08/08 02:38:17  ulf
* fixed bugs in 'save' command.
*
* Revision 1.66  1996/07/22 04:03:46  ulf
* added command 'sincos' for calculation of sine and cosine functions
*
* Revision 1.65  1996/07/17 04:00:09  ulf
* typing the 'alloc' command without parameters prints the current
* array sizes
*
* Revision 1.64  1996/07/15 00:38:35  ulf
* .inc --> .h
*
* Revision 1.63  1996/06/15 19:04:53  ulf
* added the calls to 'clrfrm' which clear the plot mode undo stack
*
* Revision 1.62  1996/06/11 01:49:47  ulf
* fixed a number of array parameter mismatches and forgotten dynamic arrays which
* were not passed to subroutines.
*
* Revision 1.61  1996/05/13 03:18:51  ulf
* removed command 'print' because it is mostly duplicated by 'info'
*
* Revision 1.60  1996/05/12 02:57:55  ulf
* fix to the call of HTML browser
*
* Revision 1.59  1996/05/12 02:49:51  ulf
* new command 'browser' to spawn HTML browser with help text
*
* Revision 1.58  1996/04/29 02:13:57  ulf
* pass the 'r' array to subroutine dcopy (provides workspace for copying)
*
* Revision 1.57  1996/04/28 02:34:59  ulf
* plot updated line markers after a 'lsqfit' command.
*
* Revision 1.56  1996/03/25 17:13:22  ulf
* moved 'dump' command to a subroutine and made it work
*
* Revision 1.55  1996/03/23  11:27:30  ulf
* fixed function 'updatelin'
*
* Revision 1.54  1996/03/22  18:24:07  ulf
* pass phase array to subroutine 'fitpoly'
*
* Revision 1.53  1996/03/22  18:21:18  ulf
* pass phase array to subroutine 'integprof'
*
* Revision 1.52  1996/03/22  18:05:53  ulf
* forgot to pass phase array to 'plotr' subroutine
*
* Revision 1.51  1996/03/20  14:55:00  ulf
* print out error message if array name was mistyped in 'alloc' commands
*
* Revision 1.50  1996/03/18  16:32:35  ulf
* return an error code in memory allocation
*
* Revision 1.49  1996/03/18  08:12:51  ulf
* fixed a typo
*
* Revision 1.48  1996/03/18  07:59:16  ulf
* build in '256k' or '1m' notation for the array sizes in an 'alloc' command
*
* Revision 1.47  1996/03/17  17:24:38  ulf
* several of the line fitting related commands required fewer parameters; fixed.
*
* Revision 1.46  1996/03/17  17:02:27  ulf
* 'dent' is not passed to 'dispar'
*
* Revision 1.45  1996/03/17  17:00:58  ulf
* eps2,eps3 are not passed to 'prprms'
*
* Revision 1.44  1996/03/14  16:47:03  ulf
* fixup after 'ftnchek'
*
* Revision 1.43  1996/03/13  18:02:12  ulf
* modified for dynamic array allocation
*
* Revision 1.42  1996/03/02  12:03:23  ulf
* make the length of the fft array always a power of 2
*
* Revision 1.41  1996/03/02  11:46:03  ulf
* new command 'alloc' for re-sizing of work arrays in Xgremlin
*
* Revision 1.40  1996/02/28  17:03:17  ulf
* new command 'response' for intensity calibration. Replaces the commands
* 'miniarc' and 'optronix'
*
* Revision 1.39  1996/02/06  15:21:33  ulf
* add a call to unlock configurable buttons in case this has not happened yet
*
* Revision 1.38  1996/02/03  11:24:33  ulf
* added the 'goto <linenumber>' command
*
* Revision 1.37  1996/01/26  18:02:43  ulf
* added a 'diff' command which appeared in recent versions of Gremlin
*
* Revision 1.36  1995/11/20  17:15:13  ulf
* new command 'zerofill'. The 'fourtran' command now leaves a complex spectrum
* in the r array and the 'invtran' command the real spectrum.
*
* Revision 1.35  1995/11/11  12:04:49  ulf
* there was still an error in the 'plotsize' command
*
* Revision 1.34  1995/11/11  11:29:55  ulf
* fix a typo
*
* Revision 1.33  1995/11/11  10:33:45  ulf
* specify plot orientation (landscape, profile) with the 'plotsize' command
*
* Revision 1.32  1995/11/04  13:02:48  ulf
* make it impossible to specify range in an interactive 'getlines' command
* because it is inherently dangerous.
*
* Revision 1.31  1995/10/25  14:18:40  ulf
* made modifications to support multiple scratch files (new with version 2.3).
* new command 'rmscratch' for deleting a scratch file.
*
* Revision 1.30  1995/10/24  14:22:58  ulf
* changed 'save' and 'recall' commands to work with new scratch file subroutines
*
* Revision 1.29  1995/10/24  12:09:34  ulf
* add command 'rmscratch' to remove a file from the scratch file directory
*
* Revision 1.28  1995/10/18  11:37:14  ulf
* add the 'add' option to 'readasc' command
*
* Revision 1.27  1995/10/16  15:37:37  ulf
* changed call of 'defprof' subroutine for the extended 'profile' command
*
* Revision 1.26  1995/10/11  14:07:36  ulf
* remove variables that were used in the 'process' command.
*
* Revision 1.25  1995/10/05  08:00:41  ulf
* removed 'process' command. should be replaced by a proper loop construct
*
* Revision 1.24  1995/10/03  08:18:04  ulf
* added subroutine 'unlkbd' to turn on command processing after an exception
* was handled.
*
* Revision 1.23  1995/09/26  18:17:44  ulf
* cmplx is an intrinsic function: rename  cmplx --> complx
*
* Revision 1.22  1995/09/26  13:12:56  ulf
* checked with 'spag' from Polyhedron Software, removed unnecessary includes
*
* Revision 1.21  1995/09/23  02:13:43  ulf
* fix another bug in 'goto' which appeared when secondary units where used.
*
* Revision 1.20  1995/09/22  23:24:08  ulf
* put a lock around the whole command dispatcher to make sure that it is not
* called while it processes another command. Commands attached to buttons can
* no longer be executed while some other command is dispatched.
*
* Revision 1.19  1995/09/18  22:02:00  ulf
* allow to specify range in a 'wplot' command in secondary units nm.
*
* Revision 1.18  1995/09/18  21:00:44  ulf
* fix a bug in the 'goto' command. 'goto' with a single parameter uses the
* same wavenumber range as in previous plot command.
*
* Revision 1.17  1995/09/18  19:48:11  ulf
* allow a range to be specified in the 'clear' command.
*
* Revision 1.16  1995/09/13  20:25:23  ulf
* cosmetic fixes
*
* Revision 1.15  1995/09/07  02:34:05  ulf
* commands 'add' and 'multiply' now take a range of points as arguments
*
* Revision 1.14  1995/09/05  01:46:04  ulf
* change 'writeasc' command to write out plotted range of spectrum by
* default.
*
* Revision 1.13  1995/08/23  02:26:19  ulf
* changed 'printer' command to display current printer when no parameter is on
* the command line
*
* Revision 1.12  1995/08/21  19:21:38  ulf
* added a check for bad phase points at the beginning of the command interpreter
* to make sure that bad phase points get written to file.
*
* Revision 1.11  1995/08/17  01:20:37  ulf
* improved error messages
*
* Revision 1.10  1995/08/09  19:21:34  ulf
* print out elapsed CPU time in log file instead of wallclock time
*
* Revision 1.9  1995/08/04  03:16:35  ulf
* fix a bug in a comment
*
* Revision 1.8  1995/08/04  03:16:04  ulf
* remove linlog command, add commands 'stom' and 'recm' for storing and
* recalling markers and commands 'readasc' and 'writeasc' for reading
* and writing data files in multi column ascii
*
* Revision 1.7  1995/07/21  02:53:56  ulf
* add new command 'linlog' to toggle lin-log plotting mode
*
* Revision 1.6  1995/07/14  17:20:10  ulf
* added the 'load' command for reading ascii files
*
* Revision 1.5  1995/07/12  21:45:13  ulf
* narrower default range of 100 points in 'goto' command
*
* Revision 1.4  1995/07/11  02:48:48  ulf
* fixed two / in formats
*
* Revision 1.3  1995/07/07  21:13:04  ulf
* new command 'printer' to set the default printer from within Xgremlin.
*
* Revision 1.2  1995/06/27  04:42:40  ulf
* modified 'button' command to accept a plot command in <modifier>+<key> format.
* This makes the button command compatible with the command format used in the
* documentation and thus easier to use.
*
* Revision 1.1  1995/06/24  19:02:50  ulf
* Initial revision
*









