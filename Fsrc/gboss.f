c \gremlin\src\gboss.for  Ver. 2.2

c  Revision 2.32 93/11/16  6 strings allowed
c  Revision 2.31 93/11/09  drop findphase, center, inputonly
c  Revision 2.30 93/11/09  Number of aliases increased to 50
c  Revision 2.29 93/09/23  chkkey added here
c  Revision 2.28 92/05/22  Fitlines, replacelines, standards added
c  Revision 2.27 92/01/08  Divide added
c  Revision 2.26 92/01/03  Clean-up, alias extensions
c  Revision 2.25 91/06/29  Readlines added
c  Revision 2.24 90/07/23  Alias lengthened to 80
c  Revision 2.23 90/04/07  catalog added

	BLOCK DATA bossbd

	common /comtok/ inpcnt,cptr,eof,eoc,qstr,str,otoken,alnum,alcur
	common /comtkc/ line,alstrng,alnam
	character*12 alnam(50)
	character*80 alstrng(50)
	integer alnum,alcur
	character line*255
	integer inpcnt, cptr, eof, eoc, qstr, str, otoken
	data inpcnt /0/, cptr /2/
	data eof /-1/, eoc /0/, qstr /1/, str /2/
	data otoken /0/, alnum /0/
	end

c ------------------------------------------------------------------
	subroutine gboss

	include 'datetc.h'
	include 'iounit.h'
	include 'set.h'

	common /comtok/ inpcnt,cptr,eof,eoc,qstr,str,otoken,alnum,alcur
	common /comtkc/ line,alstrng,alnam
	character*12 alnam(50)
	character*80 alstrng(50)
	integer alnum,alcur
	character line*255
	integer inpcnt, cptr, eof, eoc, qstr, str, otoken

	character*12 coms(160),string
	character*256 argbuf           ! increased argbuf
	integer ncom, lenarg, maxint, maxflo, maxstr, i
	integer lenalf, argcnt, token, onc
	integer intger, ios
	double precision xdble

	integer  gttk
	external gttk

c list may contain 6 alpha, 20 i fields, and a total of 20 e/f fields
	data maxint /20/,maxflo /20/,maxstr /6/,lenalf/80/
c ***** note that nact=1-4 (bye,end,set,begin) have special references****

	data coms( 1)   /'bye         '/,  coms( 2) /'end         '/
	data coms( 3)   /'set         '/,  coms( 4) /'begin       '/
	data coms( 5)   /'active      '/,  coms( 6) /'add         '/
	data coms( 7)   /'aircorr     '/,  coms( 8) /'alias       '/
	data coms( 9)   /'apodize     '/,  coms(10) /'area        '/
	data coms(11)   /'asymmetric  '/,  coms(12) /'bubbles     '/
	data coms(13)   /'alloc       '/,  coms(14) /'change      '/
	data coms(15)   /'clear       '/,  coms(16) /'close       '/
	data coms(17)   /'cmplx       '/,  coms(18) /'combine     '/
	data coms(19)   /'complex     '/,  coms(20) /'connect     '/
	data coms(21)   /'hfs         '/,  coms(22) /'convolute   '/
	data coms(23)   /'copy        '/,  coms(24) /'create      '/
	data coms(25)   /'data        '/,  coms(26) /'deglitch    '/
	data coms(27)   /'derivative  '/,  coms(28) /'dispar      '/
	data coms(29)   /'divide      '/,  coms(30) /'drop        '/
	data coms(31)   /'erase       '/,  coms(32) /'exchange    '/
	data coms(33)   /'exp         '/,  coms(34) /'extend      '/
	data coms(35)   /'extended    '/,  coms(36) /'files       '/
	data coms(37)   /'filter      '/,  coms(38) /'findlines   '/
	data coms(39)   /'diff        '/,  coms(40) /'fitlines    '/
	data coms(41)   /'fourtran    '/,  coms(42) /'getlines    '/
        data coms(43)   /'dump        '/,  coms(44) /'hold        '/
	data coms(45)   /'holdamp     '/,  coms(46) /'holddamping '/
	data coms(47)   /'holdsigma   '/,  coms(48) /'holdwidth   '/
	data coms(49)   /'imaginary   '/,  coms(50) /'info        '/
	data coms(51)   /'input       '/,  coms(52) /'browser     '/
	data coms(53)   /'insert      '/,  coms(54) /'interpolate '/
	data coms(55)   /'invtran     '/,  coms(56) /'kernel      '/
	data coms(57)   /'keyboard    '/,  coms(58) /'length      '/
	data coms(59)   /'linelist    '/,  coms(60) /'log         '/
	data coms(61)   /'lowres      '/,  coms(62) /'lsqfit      '/
	data coms(63)   /'mask        '/,  coms(64) /'background  '/
	data coms(65)   /'move        '/,  coms(66) /'multifile   '/
	data coms(67)   /'multiply    '/,  coms(68) /'ncenter     '/
	data coms(69)   /'nocorr      '/,  coms(70) /'noise       '/
	data coms(71)   /'normalize   '/,  coms(72) /'onesided    '/
	data coms(73)   /'open        '/,  coms(74) /'option      '/
	data coms(75)   /'font        '/,  coms(76) /'index       '/
	data coms(77)   /'pause       '/,  coms(78) /'phcorr      '/
	data coms(79)   /'photons/cm-1'/,  coms(80) /'planck      '/
	data coms(81)   /'plot        '/,  coms(82) /'atlas       '/
	data coms(83)   /'power       '/,  coms(84) /'UNUSED      '/
	data coms(85)   /'printparams '/,  coms(86) /'printout    '/
	data coms(87)   /'process     '/,  coms(88) /'product     '/
	data coms(89)   /'putlines    '/,  coms(90) /'range       '/
	data coms(91)   /'ratio       '/,  coms(92) /'rclphase    '/
	data coms(93)   /'read        '/,  coms(94) /'readdata    '/
	data coms(95)   /'readlines   '/,  coms(96) /'readparams  '/
	data coms(97)   /'readphase   '/,  coms(98) /'real        '/
	data coms(99)   /'recall      '/,  coms(100) /'response    '/
	data coms(101)  /'rem         '/,  coms(102) /'remove      '/  
	data coms(103)  /'replacelines'/,  coms(104) /'replot      '/  
	data coms(105)  /'rescale     '/,  coms(106) /'restore     '/  
	data coms(107)  /'reverse     '/,  coms(108) /'rewind      '/  
	data coms(109)  /'run         '/,  coms(110) /'save        '/  
	data coms(111)  /'snap        '/,  coms(112) /'savephase   '/  
	data coms(113)  /'screen      '/,  coms(114) /'setregion   '/  
	data coms(115)  /'shift       '/,  coms(116) /'sinc        '/  
	data coms(117)  /'skip        '/,  coms(118) /'slopecorr   '/  
	data coms(119)  /'spool       '/,  coms(120) /'standards   '/  
	data coms(121)  /'stretch     '/,  coms(122) /'symmetric   '/  
	data coms(123)  /'zeropad     '/,  coms(124) /'transform   '/  
	data coms(125)  /'twosided    '/,  coms(126) /'voigt       '/  
	data coms(127)  /'wavelimits  '/,  coms(128) /'whats       '/  
	data coms(129)  /'writefile   '/,  coms(130) /'writelines  '/  
	data coms(131)  /'writeparams '/,  coms(132) /'vactoair    '/  
	data coms(133)  /'movecenter  '/,  coms(134) /'inverstran  '/  
	data coms(135)  /'sincos      '/,  coms(136) /'decimate    '/  
	data coms(137)  /'setnoise    '/,  coms(138) /'rmscratch   '/  
	data coms(139)  /'stom        '/,  coms(140) /'recm        '/  
	data coms(141)  /'writeasc    '/,  coms(142) /'readasc     '/  
	data coms(143)  /'printer     '/,  coms(144) /'fitpoly     '/  
	data coms(145)  /'ls          '/,  coms(146) /'color       '/  
	data coms(147)  /'colour      '/,  coms(148) /'moments     '/  
	data coms(149)  /'profile     '/,  coms(150) /'subpoly     '/  
	data coms(151)  /'integrate   '/,  coms(152) /'fitprofile  '/  
	data coms(153)  /'button      '/,  coms(154) /'wplot       '/  
	data coms(155)  /'goto        '/,  coms(156) /'echo        '/  
	data coms(157)  /'plotsize    '/,  coms(158) /'cd          '/  
	data coms(159)  /'pwd         '/,  coms(160) /'break       '/  
	data ncom /160/

	nc = 1
10	argcnt = 0
20	token = gttk(argbuf, lenarg)  ! this reads the next command
	argcnt = argcnt + 1
       
c  ERROR if end-of-file during input
	if (token .eq. eof) go to 901

c  end of command
	if (otoken .eq. eoc ) return

c  Skip blank input line
        if (token .eq. eoc  .and.  argcnt .eq. 1) go to 10

c  is it a cmd. or an arg. to a cmd.(argcnt > 1)?
	if (argcnt .gt. 1) go to 60

c  decode command - find matching command.
        nact = -1
        string = argbuf(1:lenarg)
        do i = 1, ncom
           j = i
           if (string .eq. coms(i)) go to 30
        end do

c  no valid command found - try aliases if available
	if (alnum .eq. 0) go to 910
	do i = 1, alnum
	   j = alnum + 1 - i
	   if (string .eq. alnam(j)) go to 28
	enddo
	go to 910

c found an alias - replace it in line and continue
28	alcur = j
	token = altk(argbuf,lenarg)
	nc = 1
	go to 10

c Found a command.  Initialize argument counts.
 30	nact = j
	kin = argbuf(1:lenarg)
	nc = lenarg
	ifl = 0
	ifx = 0
	nstrng = 0
	do i = 1, maxstr
	   alpha(i) = ' '
	   nalpha(i) = 0
	enddo
	do i = 1, maxint
	   inum(i) = 0
	enddo
	do i = 1, maxflo
	   xnum(i) = 0
	enddo
	go to 20

c put string into kin for Gremlin interface, for command logging and echoing.
c  nc +2 is to take into account the blank and bump to next char.
60      continue
	onc = nc + 2
c   put surrounding quotes on string if it was quoted on input.
	if (token .eq. qstr) then
	   kin(onc:onc) = '"'
	   onc = onc + 1
	endif
	nc = onc + lenarg - 1
	kin(onc:nc) = argbuf(1:lenarg)

	if (token .eq. qstr) then
	   nc = nc + 1
	   kin(nc:nc) = '"'
	endif

c quotes were used on input, to force a string definition.
	if (token .eq. qstr) go to 80

* call the regular expression matcher to decode argument types
        call argtype( argbuf(1:lenarg), ios )

        if ( ios .eq. 1 ) then           ! it is an integer
           read (argbuf(1:lenarg), '(i20)') intger
           call addint( intger )
           goto 20
        end if

        if ( ios .eq. 2 ) then           ! it is a float
           read (argbuf(1:lenarg), '(e20.0)') xdble
           call addflo( xdble )
           goto 20
        end if

 80	if (nstrng .ge. maxstr) go to 20 ! it must be a string
	if (argbuf(2:4) .eq. 'reg' .and. 
     &               (nact .ne. 3 .or. nstrng .gt. 0)) then

c a register is being accessed, NOT first one in 'set' (nact = 3)
	   i = ichar( argbuf(6:6) ) - ichar('0')
	   if (i .le. 0 .or. i .gt. 9) i=10
	   if (argbuf(1:1) .eq. 'x') then
	      call addflo( xreg(i) )
	      goto 20
	   endif
	   if (argbuf(1:1) .eq. 'i') then
	      call addint( ireg(i) )
	      goto 20
	   endif
	endif

c normal string
	nstrng = nstrng + 1
	if (lenarg .gt. lenalf) lenarg = lenalf
	nalpha(nstrng) = lenarg
	alpha(nstrng) = argbuf(1:lenarg)
	go to 20

c  ERROR HANDLING - eof on input.  If batch, send it to end
901     if (batch) then
	   nact = 25
	   return
	endif
	if (input .ne. cardin) go to 10
	call wrtstr(' Warning :  e-o-f during input from cards file.' )
	input = stdin
	nact = 0        ! no action
	keybd = .true.
	return

c Invalid/unknown command - throw away any accumulated input.
 910	call wrtstr(' Error :  unknown or invalid command' )
	cptr = inpcnt + 2
	otoken = eoc
	keybd = .true.   ! return to manual input in case we are in batch mode
	return
	end

c-----------------------------------------------------------------------

	subroutine addint( intger )
*
* add a new entry number to the integer parameter list
*
	integer intger
	integer maxint
	parameter (maxint=20)

	include 'datetc.h'
	
	if ( ifx .lt. maxint ) then
	   ifx = ifx + 1
	   inum(ifx) = intger
	end if

	return
	end

c-----------------------------------------------------------------------
	
	subroutine addflo( xflo )
*
* add a new entry to the floating point parameter list
*
	double precision xflo
	integer maxflo
	parameter (maxflo=20)

	include 'datetc.h'

	if ( ifl .lt. maxflo ) then
	   ifl = ifl + 1
	   xnum(ifl) = xflo
	end if
	
	return
	end

c-----------------------------------------------------------------------
	integer function gttk (argbuf, lenarg)

	character argbuf*(*)
	integer lenarg

        include 'iounit.h'
        include 'datetc.h'

	character*12 alnam(50)
	character*80 alstrng(50)
	integer alnum,alcur
	character line*255
	integer inpcnt, cptr, eof, eoc, qstr, str, otoken

	common /comtok/ inpcnt,cptr,eof,eoc,qstr,str,otoken,alnum,alcur
	common /comtkc/ line,alstrng,alnam

	character charac*1
	integer eptr, ocptr
	integer lenlin
	integer findlm, skpdlm
	integer chrcnt
	external chrcnt, findlm, skpdlm

	gttk = str
	lenlin = 127
	if (cptr .le. inpcnt) go to 20
	if (cptr .eq. (inpcnt+1)) then
	    gttk = eoc
  	    eptr = cptr
	    cmdpnd = .false.
	    go to 55
	endif

	if (cmdpnd) goto 20

 10	if (.not. keybd) go to 15
	inpcnt = length(line)  ! line was passed in common block
	go to 18

* read another line with commands from file
 15	read (input, '(a)', end= 901, err= 901) line
	inpcnt = chrcnt (line, lenlin)

c ignore lines beginning with the '#' character (comment lines)
 18	if (line(1:1) .eq. '#') go to 10
	cptr = 1

c find first valid character.
20	ocptr = cptr
	cptr = skpdlm (line(cptr:inpcnt), (inpcnt-cptr+1))
	if (cptr .eq. 0) then
	    gttk = eoc
	    cptr = inpcnt + 2
	    otoken = gttk
	    return
	endif
	cptr = cptr + ocptr - 1
	charac = line(cptr:cptr)

c end-of-command char?
	if (charac .eq. ';') then
	    gttk = eoc
	    eptr = cptr
	    cmdpnd = .true.        ! there may be one more command
	    go to 55
	endif

	if (charac .eq. '"' .or. charac .eq. '''') then
c " or ' found
	   do ia=cptr+1,inpcnt
	      if (line(ia:ia) .eq. charac) then
		 gttk = qstr
		 eptr = ia
		 go to 50
	      end if
	   enddo
	endif
c  no closing quote on line, backup and treat this as a regular string.
c  no enclosing quotes; just find a non-escaped delimiter
	eptr = findlm (line(cptr:inpcnt),(inpcnt-cptr+1))
	if (eptr .ne. 0) then
c  back up before the delimiter
	   eptr = eptr - 1
	   eptr = eptr + cptr - 1
	else
	   eptr = inpcnt  ! no delimiter found
	endif

c  found an arg; remove any enclosing quotes, return
 50	lenarg = eptr - cptr + 1
	argbuf = line(cptr:eptr)
	if (gttk .eq. qstr) then
	   lenarg = lenarg - 2
	   argbuf = line(cptr+1:eptr-1)
	endif

 55	cptr = eptr + 1
	otoken = gttk
	return

 901	gttk = eof
	cptr = inpcnt + 2
	return

c this entry is used to initialize gttk for an alias
c Save line above alias in argbuf; expand alias, add rest of line
	entry altk (argbuf, lenarg)
c       **********
	argbuf = line(cptr:inpcnt)
	nn = chrcnt (alstrng(alcur), 80)
	inpcnt = inpcnt + nn - lenarg
	if (inpcnt .gt. 255) then
	    inpcnt = 0
	    write(wrtbuf,*) ' Alias overflow - abort command line'
	    call wrtstr(wrtbuf)
	    altk = eoc
	    cptr = 1
	    return
	endif
	cptr = cptr - lenarg
	line(cptr:cptr+nn-1) = alstrng(alcur)
	line(cptr+nn:inpcnt) = argbuf

	altk = str
	cptr = 1
	return

	end
c-----------------------------------------------------------------------

	integer function findlm (str,n)
	character*(*) str
	integer n

	character char
	integer ptr

	findlm = 0
	ptr = 1
10      char = str(ptr:ptr)
	if (char .eq. ' ' .or. char .eq. ',' .or. char .eq. ';') goto 30
	ptr = ptr + 1
	if (ptr .gt. n) return
	go to 10

30      findlm = ptr
	return
	end

c-----------------------------------------------------------------------
	integer function skpdlm (line, lenlin)

	character line*(*), chr
	integer lenlin, ptr, optr

	skpdlm = 0
	ptr = 1
10	if (ptr .gt. lenlin) return
	optr = ptr
	chr = line(ptr:ptr)
	if (chr .eq. ' ' .or. chr .eq. ',') ptr = ptr + 1
	if (optr .ne. ptr) go to 10
	skpdlm = ptr
	return
	end
c-----------------------------------------------------------------------
	integer function chrcnt (string, nch)

	character string*(*)
	integer i, nch

c start at the end of the string.
	do i = nch,1,-1
	   if (string(i:i) .ne. ' ') then
	      chrcnt = i
	      return
	   endif
	enddo

	chrcnt = 0
	return
	end
c-----------------------------------------------------------------------
	subroutine frmchk (iarray,n)

        include 'datetc.h'

	character iarray*(*)
	integer i, ia, kef, kpf, lim, n
	character numstf*12, idp, iek, ibl, idk, chr1
	data numstf /'0123456789+-'/
	data idp, iek, ibl, idk/ '.', 'e', ' ', 'd'/

c check number field (n characters starting at iarray) for legal format
c return status in ipara: -1, illegal: 0, legal: +1, legal for real only

	ipara = 0
	kpf = 0
	kef = 0
	lim = 12
  
	do 680 i = 1, n
c  skip blanks, test for e and .
	   chr1 = iarray(i:i)
	   if (chr1 .eq. ibl) go to 680
	   if (chr1 .eq. iek) go to 640
	   if (chr1 .eq. idp) go to 660
	   if (chr1 .eq. idk) go to 640

c ** check for signs (legal if lim=12) and numerals (1-10)
	   do ia = 1, lim
	      if (iarray(i:i) .eq. numstf(ia:ia)) go to 675
	   enddo

c error exit
 620	   ipara = -1
	   return

c ** check legality of e and . (1 each allowed), set real flag
 640	   if (kef .ne. 0) go to 620
	   ipara = 1
	   kef = 1
	   lim = 12
	   go to 680
  
 660	   if (kpf .ne. 0) goto 620
	   ipara = 1
	   kpf = 1
 675	   lim = 10
 680	continue
  
	return
	end
c-----------------------------------------------------------------------

*--- 'sys' not required when Gremlin is running under an operating system

c-----------------------------------------------------------------------
	subroutine aliasub

        include 'datetc.h'
	include 'iounit.h'
	
	integer ia

	character*12 alnam(50)
	character*80 alstrng(50)
	integer alnum,alcur
	character line*255
	integer inpcnt, cptr, eof, eoc, qstr, str, otoken

	common /comtok/ inpcnt,cptr,eof,eoc,qstr,str,otoken,alnum,alcur
	common /comtkc/ line,alstrng,alnam

	if (nstrng .ne. 2) then
	   if ( alnum .eq. 1 ) then
	      write (wrtbuf,11) alnum
	   else
	      write (wrtbuf,12) alnum
	   end if
 11	   format( i3, ' alias currently defined:' )
 12	   format( i3, ' aliases currently defined:' )
	   call wrtstr( wrtbuf )
	   do ia=1, alnum
	      write( wrtbuf, 13 ) alnam(ia), alstrng(ia)
	      call wrtstr( wrtbuf )
	   end do
 13	   format (' ',a11, a60)
	   return
	endif

	nch = nalpha(1)
	if (nch .gt. 12) nch = 12
	alnum = alnum + 1
	if (alnum .gt. 50) then
	   call wrtstr(' Warning :  alias table full')
	   return
	endif
	alnam(alnum) = alpha(1)(1:nch)
	nch = nalpha(2) 
	alstrng(alnum) = alpha(2)(1:nch)
	write (wrtbuf,21) alnum
 21	format (' Alias no.', i3, ' entered')
	call wrtstr(wrtbuf)

	return
	end

c-----------------------------------------------------------------------

	subroutine comin

        include 'datetc.h'
        include 'iounit.h'

	real dummy(1)   ! open com never returns ffta

	if (nstrng .ne. 0) then
	   nalpha(2) = nalpha(1)
	   alpha(2) = alpha(1)
	   nalpha(1) = 3
	   alpha(1) = 'com'
	   if (uopen(cardin) .eq. 1) call dclose
	   nstrng = 2
	   call gopen(dummy)
	endif

c continue reading cardin
	if (uopen(cardin) .eq. 1) then
	   keybd = .false.
	   ipara = cardin
	   input = cardin
	else
	   call wrtstr(' Error :  unit not open')
	endif

	return
	end


*------------------------------------------------------------------------------

	subroutine setline( str )
*
* set the variable line which receives keyboard input
*
	character str*(*)
	character*12 alnam(50)
	character*80 alstrng(50)
	character line*255
	integer ilen
	common /comtkc/ line,alstrng,alnam

	line = ' '
	ilen = length( str )
	line(1:ilen) = str(1:ilen)

	return
	end
