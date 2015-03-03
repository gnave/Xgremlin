c /grammy/src/gboss.f  Ver. 1.0

c  Revision 1.01 04/26/90  Phapodize changed to findphase
c  Revision 1.0  03/06/89

        BLOCK DATA gbossbd

        common /comtok/ inpcnt, cptr, eof, eoc, qstr, str, otoken
        common /comtkc/ line,alstrng,alnam,alnum
        character*12 alnam(20)
        character*68 alstrng(20)
	integer*4 alnum
        character line*128
        integer inpcnt, cptr, eof, eoc, qstr, str, otoken
        data inpcnt /0/, cptr /2/
        data eof /-1/, eoc /0/, qstr /1/, str /2/
        data otoken /0/
        data alnum  /0/
        end

c *********************
        subroutine gboss

        include 'parms.h'
c       include 'include/datetc.inc'
c       include 'include/iounit.inc'
c       include 'include/set.inc'


        common /comtok/ inpcnt, cptr, eof, eoc, qstr, str, otoken
        common /comtkc/ line,alstrng,alnam,alnum
        character*12 alnam(20)
        character*68 alstrng(20)
	integer*4 alnum
        character line*128
        integer inpcnt, cptr, eof, eoc, qstr, str, otoken

        character*12 coms(60)
        character*127 argbuf
        character*12 string
        integer ncom, lenarg, maxint, maxflo, maxstr, i
        integer lenalf, argcnt, token, onc
        integer*4 intger
        double precision double

        integer  gttk
        external gttk

c                                       list may contain 3 alpha, 20 i fields,
c                                       and a total of 20 e and f fields
        data ncom /60/, maxint /20/, maxflo /20/, maxstr /3/
c ***** note that nact=1-4 (bye,end,set,begin) have special references****
        data coms( 1)   /'end         '/, coms( 2) /'begin       '/
        data coms( 3)   /'alias       '/, coms( 4) /'atlas       '/
	data coms( 5)   /'axis        '/, coms( 6) /'box         '/
        data coms( 7)   /'close       '/, coms( 8) /'connect     '/
        data coms( 9)   /'direction   '/, coms(10) /'dot         '/
	data coms(11)   /'draw        '/, coms(12) /'break       '/
	data coms(13)   /'erase       '/, coms(14) /'font        '/
        data coms(15)   /'grid        '/, coms(16) /'hardcopy    '/
	data coms(17)   /'break       '/, coms(18) /'histogram   '/
        data coms(19)   /'horizontal  '/, coms(20) /'justify     '/
        data coms(21)   /'keyboard    '/, coms(22) /'label       '/
        data coms(23)   /'limits      '/, coms(24) /'location    '/
        data coms(25)   /'ltype       '/, coms(26) /'lweight     '/
        data coms(27)   /'notick      '/, coms(28) /'open        '/
        data coms(29)   /'option      '/, coms(30) /'putlabel    '/
        data coms(31)   /'relocate    '/, coms(32) /'rewind      '/
	data coms(33)   /'run         '/, coms(34) /'screen      '/
	data coms(35)   /'size        '/, coms(36) /'slant       '/
        data coms(37)   /'spool       '/, coms(38) /'store       '/
        data coms(39)   /'UNUSED      '/, coms(40) /'ticksize    '/
	data coms(41)   /'vertical    '/, coms(42) /'view        '/
	data coms(43)   /'wavelabel   '/, coms(44) /'window      '/
        data coms(45)   /'xlabel      '/, coms(46) /'ylabel      '/
        data coms(47)   /'semilog     '/, coms(48) /'verbose     '/
        data coms(49)   /'scale       '/, coms(50) /'            '/
        data coms(51)   /'            '/, coms(52) /'            '/
        data coms(53)   /'            '/, coms(54) /'            '/
        data coms(55)   /'            '/, coms(56) /'            '/
        data coms(57)   /'            '/, coms(58) /'            '/
        data coms(59)   /'            '/, coms(60) /'            '/

        lenalf = len (alpha(1))
        nc = 1
10      argcnt = 0
20      token = gttk (argbuf, lenarg)
        argcnt = argcnt + 1
c                                       end-of-file during input - ERROR
       
        if (token .eq. eof) go to 901
c                                       blank input line
        if (token .eq. eoc  .and.  argcnt .eq. 1) go to 10
c                                       end of command.
        if (token .eq. eoc) return
c                                       is it a cmd. or an arg. to a cmd.
        if (argcnt .gt. 1) go to 60

c                                       decode command
 
        nact = -1
        string = argbuf(1:lenarg)
c                                       find matching command.
        do 24 i = 1, ncom
            j = i
            if (string .eq. coms(i)) go to 30
24      continue
c                                       no valid command found - try alias

	if (alnum .eq. 0) go to 910
        do 26 i = 1, alnum
            j = alnum + 1 - i
            if (string .eq. alnam(j)) go to 28
26      continue
        go to 910

c found an alias - shove it into line and start over
28	line = alstrng(j)
	token = altk(argbuf,lenarg)
	nc = 1
	go to 10

30      nact = j
c                                       initialize arg. counts.
        kin = argbuf(1:lenarg)
        nc = lenarg
        ifl = 0
        ifx = 0
        nstrng = 0
        do 40 i = 1, maxstr
            alpha(i) = ' '
40          nalpha(i) = 0
        do 45 i = 1, maxint
45          inum(i) = 0
        do 50 i = 1, maxflo
50          xnum(i) = 0

        go to 20

60      continue
c                         put string into kin for DECOMP interface, for command
c                         logging and echoing.   nc +2 is to take into account 
c                         the blank and bump to next char.
        onc = nc + 2
c                                       put surrounding quotes on
c                                       string if it was quoted on input.
        if (token .ne. qstr) go to 65
            kin(onc:onc) = '"'
            onc = onc + 1

65      nc = onc + lenarg - 1
        kin(onc:nc) = argbuf(1:lenarg)

        if (token .ne. qstr) go to 67
            nc = nc + 1
            kin(nc:nc) = '"'
67      continue
c                                       quotes were used on input,
c                                       to force a string definition.
        if (token .eq. qstr) go to 80
c                                       use F77 to decode args.

        read (argbuf(1:lenarg), '(i80)', err=70) intger
68      if (ifx .ge. maxint) go to 20
        ifx = ifx + 1
        inum(ifx) = intger
        go to 20

70      read (argbuf(1:lenarg), '(e80.0)', err=80) double
72      if (ifl .ge. maxflo) go to 20
        ifl = ifl + 1
        xnum(ifl) = double
        go to 20

80      if (nstrng .ge. maxstr) go to 20
	if (argbuf(2:4) .eq. 'reg' .and. 
     *       (nact .ne. 3 .or. nstrng .gt. 0)) then

c a register is being accessed, NOT first one in 'set' (nact = 3)
           i = ichar( argbuf(6:6) ) - ichar('0')
           if (i .le. 0 .or. i .gt. 9) i=10
           if (argbuf(1:1) .eq. 'x') then
              double = xreg(i)
              go to 72
           endif
           if (argbuf(1:1) .eq. 'i') then
              intger = ireg(i)
              go to 68
           endif
	endif

c normal string
        nstrng = nstrng + 1
        if (lenarg .gt. lenalf) lenarg = lenalf
        nalpha(nstrng) = lenarg
        alpha(nstrng) = argbuf(1:lenarg)
        go to 20

c                                       ERROR HANDLING
c                                       eof on input
901     if (batch) go to 905
        if (input .eq. com) go to 907
        go to 10
c if batch, send it to end
905     nact = 2
        return
907     write (stdout, *) ' e-o-f during input from cards file.'
        backspace input
        input = stdin
        keybd = .true.
        go to 10
c                                       invalid/unknown command.
910     write (output, *) ' unknown command.'
c                                       throw away any accum. input.
        cptr = inpcnt + 2
        otoken = eoc
        go to 10
        end

c******************************************************************************

        integer function findlm (str,n)
        character*127 str
        integer n

        character char, delim(3)
        integer ptr, i

        data delim /' ',  ',',  ';'/

        findlm = 0
        ptr = 1
10      char = str(ptr:ptr)
        do 20 i = 1, 3
            if (char .eq. delim(i)) go to 30
20      continue

        ptr = ptr + 1
        if (ptr .gt. n) return
        go to 10

30      findlm = ptr
        return
        end

c*****************************************************************************

        integer function gttk (argbuf, lenarg)
        character argbuf*127
        integer lenarg

        common /comtok/ inpcnt, cptr, eof, eoc, qstr, str, otoken
        common /comtkc/ line,alstrng,alnam,alnum
        character*12 alnam(20)
        character*68 alstrng(20)
	integer*4 alnum
        character line*128
        integer inpcnt, cptr, eof, eoc, qstr, str, otoken

        include 'parms.h'
c       include 'include/iounit.inc'
c       include 'include/datetc.inc'

        character strdlm(2), bslash, char, scolon
        integer i, j, iquot, eptr, ocptr
        integer*4 lenlin
        integer  findlm, skpdlm
        integer*4 chrcnt
        external chrcnt, findlm, skpdlm

        data strdlm /'"',''''/
        data bslash /'\\'/
        data scolon /';'/


        gttk = str
        lenlin = 127
        if (cptr .le. inpcnt) go to 20
        if (cptr .eq. (inpcnt+1)) go to 90

10      if (.not. keybd) go to 15
        if (otoken .eq. eoc .or. otoken .eq. eof) write (*,11)
11              format (' plot: ',$) 
        if (otoken .eq. str .or. otoken .eq. qstr) write (*,13)
13              format (' >  ',$) 

15      read (input, '(a)', end= 901, err= 901) line
c                               ignore lines beginning with the '#' character.
c                               This indicates a comment line.
        if (line(1:1) .eq. '#') go to 10

        inpcnt = chrcnt (line, lenlin)
        cptr = 1
20      continue

c                                       find first valid character.
        ocptr = cptr
        cptr = skpdlm (line(cptr:inpcnt), (inpcnt-cptr+1))
        if (cptr .eq. 0) go to 100
        cptr = cptr + ocptr - 1

        char = line(cptr:cptr)
c                                       continuation of previous line's input
        if (char .eq. bslash  .and.  cptr .eq. inpcnt) go to 10
c                                       end-of-command char?
        if (char .eq. scolon) go to 90
        iquot = 0
        do 30 i = 1, 2
            if (char .eq. strdlm(i)) iquot = i
30      continue
        if (iquot .eq. 0) go to 60
        j = cptr + 1
40      i = 0
        do 45 ia=j,inpcnt
        if (line(ia:ia) .eq. strdlm(iquot)) i=ia
45      continue
c                                       no closing quote on line,
c                                       backup and treat this as a
c                                       regular string.
        if (i .eq. 0) go to 60
        gttk = qstr
        eptr = i

c                                       found an arg, lets return...

50      lenarg = eptr - cptr + 1
        argbuf = line(cptr:eptr)
        if (gttk .ne. qstr) go to 55
c                                       remove enclosing quotes
        lenarg = lenarg - 2
        argbuf = line(cptr+1:eptr-1)
55      cptr = eptr + 1
        otoken = gttk
        return


c                                       no enclosing quotes just find
c                                       a non-escaped delimiter
60      continue
        j = cptr
70      eptr = findlm (line(j:inpcnt),(inpcnt-j+1))
        if (eptr .ne. 0) go to 80
        eptr = inpcnt
c                                       no delimiter found
        if (line(inpcnt:inpcnt) .eq. bslash) eptr = eptr - 1
        go to 50

80      continue
c                                       back up before the delimiter
        eptr = eptr - 1
        eptr = eptr + cptr - 1
        go to 50

90      gttk = eoc
        eptr = cptr
        go to 55

100     gttk = eoc
        cptr = inpcnt + 2
        otoken = gttk
        return

901     gttk = eof
        cptr = inpcnt + 2
        return


c this entry is used to initialize gttk for an alias
	entry altk(argbuf,lenarg)
c       **********
	altk = str
        inpcnt = chrcnt (line, lenlin)
        cptr = 1
	return

        end

c******************************************************************************

        integer function skpdlm (line, lenlin)
        character line*127
        integer lenlin

        character delim(2), chr
        integer ptr, optr, i

        data delim /' ' ,  ','/

        skpdlm = 0
        ptr = 1
10         if (ptr .gt. lenlin) return
           optr = ptr
           chr = line(ptr:ptr)
           do 20 i = 1, 2
              if (chr .eq. delim(i)) ptr = ptr + 1
20         continue
           if (optr .ne. ptr) go to 10
        skpdlm = ptr
        return
        end
c*****************************************************************************c
        integer*4 function chrcnt (string, nch)

        character string*127
        integer*4 i, j, nch

        chrcnt = 0
c                                       start at the end of the string.
        do 10 i = 1, nch
           j = nch - i + 1
           if (string(j:j) .ne. ' ') go to 20
10      continue
        return
20      chrcnt = j
        return
        end
c*****************************************************************************c
        integer function len (string)
        character string*127

        character NULL
        integer i, j

        NULL = char(0)
        len = 0

        do 10 i = 1, 127
           j = i
           if (string(i:i) .eq. NULL) go to 20
10      continue
20      len = j
        return
        end
c*****************************************************************************c
        subroutine lcase(str,n)

        character str*127
        integer i, j, biga, bigz, diff, n

c first n char: convert any upper case alphabetic characters to lower case

        biga = ichar('A')
        bigz = ichar('Z')
        diff = ichar('a') - biga

        DO 10 i=1,n
                j = ichar( str(i:i) )
                if (j .ge. biga .and. j .le. bigz) then
                        str(i:i) = char(j + diff)
                end if
10      continue
        return
        end

c **********************************************************
        subroutine frmchk (iarray,n)

        character iarray*127
        integer n

        include 'parms.h'
c       include 'include/datetc.inc'

        integer i, ia, kef, kpf, lim
        character chr1
        character numstf*12, idp, iek, ibl, idk
        data numstf /'0123456789+-'/
        data idp, iek, ibl, idk/ '.', 'e', ' ', 'd'/
c **
c ** check number field (n characters starting at iarray) for legal format
c ** return status in ipara: -1, illegal: 0, legal: +1, legal for real only
c **
        ipara = 0
        kpf = 0
        kef = 0
        lim = 12
  
        do 680 i = 1, n
c                                               skip blanks, test for e and .
            chr1 = iarray(i:i)
            if (chr1 .eq. ibl) go to 680
            if (chr1 .eq. iek) go to 640
            if (chr1 .eq. idp) go to 660
            if (chr1 .eq. idk) go to 640
c **
c ** check for signs (legal if lim=12) and numerals (1-10)
c **
            do 610 ia = 1, lim
                if (iarray(i:i) .eq. numstf(ia:ia)) go to 675
610         continue
c                                               error exit
620         ipara = -1
            return
c **
c ** check legality of e and . (1 each allowed), set real flag
c **
640         if (kef .ne. 0) go to 620
            ipara = 1
            kef = 1
            lim = 12
            go to 680
  
660         if (kpf .ne. 0) go to 620
            ipara = 1
            kpf = 1
675         lim = 10
680     continue
  
        return
        end
c
c **********************************************************

        subroutine alias
c
        include 'parms.h'
c       include 'include/datetc.inc'
c
        common /comtkc/ line,alstrng,alnam,alnum
        character*12 alnam(20)
        character*68 alstrng(20)
	integer*4 alnum
        character line*128

	if (nstrng .ne. 2) go to 999
	nch = nalpha(1)
	if (nch .gt. 12) nch = 12
	alnum = alnum + 1
	if (alnum .gt. 20) go to 998
	alnam(alnum) = alpha(1)(1:nch)
	nch = nalpha(2) 
	alstrng(alnum) = alpha(2)(1:nch)
	write (*,11) alnum
11	format (' Alias no.', i3, ' entered')

	return

998	write (*,*) ' Alias table full'
	return

999	write (*,1001) alnum,(alnam(i),alstrng(i),i=1,alnum)
1001	format (i3,' aliases currently defined:'/,(1x,a11,a68))
	return
	end
c ********************************************************************
        subroutine comin
c
        include 'parms.h'
c
	if (nstrng .eq. 0) go to 1000
        close(cardin,status='keep')
        filnam = alpha(1)(1:nalpha(1))
        open(cardin,file=filnam,status='old',err=9010)
	uopen(cardin) =  1

c continue reading cardin
1000	if (uopen(cardin) .ne. 1) go to 9000
	keybd = .false.
	ipara = cardin
	input = cardin
	return

9000	write (*,*) ' ERROR - unit not open'
	return
9010	write (*,*) ' ERROR - in open'
	return
	end
