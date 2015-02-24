
c Revision 1.12  17.11.93 add nphz to info
c  Revision 1.11  07.09.93 add nfrmk, atod to info; add krnl to whats
c  Revision 1.10  10.08.93 add more d.p.s to doubles < 1.0 in info
c  Revision 1.09  05.08.93 add clkcnt to info
c  Revision 1.08  28.06.93 add noptex to info
c  Revision 1.07  05.05.93 decomp additions to set; add phaseapod
c  Revision 1.06  30.05.93 fix ncenter in whats; obuf dimension and size
c  Revision 1.05  16.05.93 nint, npo, ftsmode  added to info
c  Revision 1.04  11.05.93 'quiet' added as alternative to 'verbose off'
c  Revision 1.03  29.03.93 'whats' to output, not *
c  Revision 1.02  22.03.93 set updated
c  Revision 1.01  13.04.91 ffta, phz added to "whats"
c  Version  1.00  10.04.91


	subroutine dinfo

c  examine/change information block entries

c  command syntax:  info "variable name"    this examines
c		    info "variable name" "new value"

	include 'datetc.h'
	include 'infmtn.h'
	include 'altinf.h'
	include 'extinf.h'
	include 'inparms.h'
	include 'iounit.h'
	include 'linparms.h'

	character*80 string

	if (nstrng .le. 0) then
	   if (ifx .eq. 2 .or. ifl .eq. 2) go to 9998 ! catch ireg, xreg
	   call infprn
	   return
	end if

	string = alpha(1)(1:nalpha(1))
	call lcase(string, nalpha(1))
	iret = 0
  
	if (string .eq. 'vacuum')  call setlog(lvacuum, iret)

	if (string .eq. 'nop')     call setint(nop,iret)
        if (string .eq. 'nrefp')   call setint(nrefp,iret)
	if (string .eq. 'noptex')  call setint(noptex,iret)
	if (string .eq. 'ntrans')  call setint(ntrans,iret)
	if (string .eq. 'npo')     call setint(npo,iret)
	if (string .eq. 'nint')    call setint(nint,iret)
	if (string .eq. 'neoa')    call setint(neoa,iret)
	if (string .eq. 'nwos')    call setint(nwos,iret)
	if (string .eq. 'nwpp')    call setint(nwpp,iret)
	if (string .eq. 'nphz')    call setint(nphz,iret)
	if (string .eq. 'nm')      call setint(nm,iret)
	if (string .eq. 'nk')      call setint(nk,iret)
	if (string .eq. 'ns')      call setint(ns,iret)
        if (string .eq. 'nol')     call setint(nol,iret)
	if (string .eq. 'inftype') call setint(inftype,iret)
	if (string .eq. 'fboffs')  call setint(fboffs,iret)
	if (string .eq. 'bocode')  call setint(bocode,iret)
	if (string .eq. 'margin')  call setint(margin,iret)
	if (string .eq. 'ncenter')  call setint(ncenter,iret)
  
	if (string .eq. 'resolutn')  call setreal(resolutn,iret)
	if (string .eq. 'noiselev')  call setreal(noiselev,iret)
	if (string .eq. 'rdsclfct')  call setreal(rdsclfct,iret)
	if (string .eq. 'pspect')    call setreal(pspect,iret)
	if (string .eq. 'tspect')    call setreal(tspect,iret)
	if (string .eq. 'hspect')    call setreal(hspect,iret)
	if (string .eq. 'wdlim')     call setreal(wdlim,iret)
	if (string .eq. 'stol')      call setreal(stol,iret)
	if (string .eq. 'sten')      call setreal(sten,iret)
	if (string .eq. 'smof')      call setreal(smof,iret)
	if (string .eq. 'swid')      call setreal(swid,iret)

	if (string .eq. 'wref')   call setdbl(wref,iret)
	if (string .eq. 'delw') then
	    call setdbl(delw,iret)
	    call disper
	endif
	if (string .eq. 'refwavno') call setdbl(refwavno,iret)
	if (string .eq. 'fzeeman')  call setdbl(fzeeman,iret)
	if (string .eq. 'wstart')   call setdbl(wstart,iret)
	if (string .eq. 'wstop')    call setdbl(wstop,iret)
	if (string .eq. 'wavcorr')  call setdbl(wavcorr,iret)
	if (string .eq. 'uvar0')    call setdbl(uvar0,iret)
	if (string .eq. 'uvar1')    call setdbl(uvar1,iret)
  
	if (string .eq. 'day')      call setstr(day,iret,10)
	if (string .eq. 'id')       call setstr(id,iret,78)
	if (string .eq. 'user')     call setstr(user,iret,20)
	if (string .eq. 'sample')   call setstr(sample,iret,20)
	if (string .eq. 'ftsmode')  call setstr(ftsmode,iret,20)
	if (string .eq. 'size')     call shwsiz(iret)   ! show Xgremlin size
	if (string .eq. 'pid') then
	   write(wrtbuf,'(a,i6)') ' process id :  ',ipid
	   call wrtstr(wrtbuf)
	   iret = 1
	end if
  
	if (iret .eq. 1) return
  
9998	write (wrtbuf, 9000) string(1:nalpha(1))
	call wrtstr(wrtbuf)
9000	format(' unknown/un-implemented info block entry  ',a)
	return
  
	end
  
c ------------------------------------------------------------------

	subroutine setlog(logvar,iret)

	logical logvar
	character lognam*10, cl*1

	include 'datetc.h'
	include 'iounit.h'

	iret = 1
	if ( logvar ) then
	   lognam = 'true'
	else
	   lognam = 'false'
	end if
	write(wrtbuf,9001) alpha(1)(1:nalpha(1)), lognam
	call wrtstr(wrtbuf)
9001	format(' current value of ',a,':',3x,a)
	if (nstrng .ne. 2 .and. ifx .ne. 1) return

	if ( ifx .eq. 1 ) then
	   if ( inum(1) .eq. 1 ) then
	      cl = 't'
	   else if ( inum(1) .eq. 0 ) then
	      cl = 'f'
	   end if
	else
	   cl = alpha(2)(1:1)
	end if

	if ( cl.eq.'t' .or. cl.eq.'T' ) then
	   logvar = .true.
	else if ( cl.eq.'f' .or. cl.eq.'F' ) then
	   logvar = .false.
	else
	   call wrtstr(' Error :  wrong parameter in info command')
	end if

	if ( cl .eq. 't' ) then
	   lognam = 'true'
	else
	   lognam = 'false'
	end if

	write (wrtbuf, 8001) lognam
	call wrtstr(wrtbuf)
8001	format(' new value is  ',10x,a)
	return
	end
  
c ------------------------------------------------------------------

	subroutine setint(intvar,iret)

	include 'datetc.h'
	include 'iounit.h'

	iret = 1
	write(wrtbuf,9001) alpha(1)(1:nalpha(1)), intvar
	call wrtstr(wrtbuf)
9001	format(' current value of ',a,':',3x,i10)
	if (ifx .ne. 1) return
	intvar = inum(1)
	write (wrtbuf, 8001) inum(1)
	call wrtstr(wrtbuf)
8001	format(' new value is  ',10x,i10)
	return
	end
  
c ------------------------------------------------------------------

	subroutine setreal(realvar,iret)

	include 'datetc.h'
	include 'iounit.h'

	iret = 1
	if ( realvar.lt.1.e-3 .or. realvar.gt.1.e3 ) then
	   write(wrtbuf,9002) alpha(1)(1:nalpha(1)), realvar
	else
	   write(wrtbuf,9003) alpha(1)(1:nalpha(1)), realvar
	end if
	call wrtstr(wrtbuf)
 9002	format(' current value of ',a,':',3x,e15.6)
 9003	format(' current value of ',a,':',3x,f15.6)
	if (ifl .ne. 1) return
	realvar = xnum(1)
	if ( realvar.lt.1.e-3 .or. realvar.gt.1.e3 ) then
	   write(wrtbuf,8011) realvar
	else
	   write(wrtbuf,8012) realvar
	end if
	call wrtstr(wrtbuf)
 8011	format(' new value is : ',10x,e15.6)
 8012	format(' new value is : ',10x,f15.6)
	return
	end

c ------------------------------------------------------------------

	subroutine setdbl(dblvar,iret)

	include 'datetc.h'
	include 'iounit.h'
	double precision dblvar

	iret = 1

	if ( dblvar .gt. 1.0) then
	   write(wrtbuf,9003) alpha(1)(1:nalpha(1)), dblvar
	end if
	if ( dblvar .le. 1.0) then
	   write(wrtbuf,9005) alpha(1)(1:nalpha(1)), dblvar
	end if
	call wrtstr(wrtbuf)
 9003	format(' current value of ',a,':',3x,e18.6)
 9005	format(' current value of ',a,':',3x,e18.12)
	if (ifl .ne. 1) return

	dblvar = xnum(1)
	if (dblvar .gt. 1.0) then
	   write(wrtbuf,8021) xnum(1)
	   call wrtstr(wrtbuf)
	end if
 8021	format(' new value is  ',10x,f18.6)
	if (dblvar .le. 1.0)  then
	   write(wrtbuf,8025) xnum(1)
	   call wrtstr(wrtbuf)
	end if
 8025	format(' new value is  ',10x,f18.12)
	return
	end

c ------------------------------------------------------------------
	subroutine setstr(strvar,iret,maxch)

	include 'datetc.h'
	include 'iounit.h'
	character*(*) strvar

	iret = 1
	write(wrtbuf,9004) alpha(1)(1:nalpha(1)), strvar
	call wrtstr(wrtbuf)
9004	format(' current value of ',a,':',3x,a)
	if (nstrng .ne. 2) return

	if (nalpha(2) .gt. maxch) nalpha(2) = maxch
	strvar = alpha(2)(1:nalpha(2))
8030	write (wrtbuf, 8031) alpha(2)(1:nalpha(2))
	call wrtstr(wrtbuf)
8031	format(' new value is  ',10x,a)
	return
	end
  
c -------------------------------------------------------------- 

	subroutine dset

c  examine/change information block entries; set registers

c  SYNTAX:  	set "parameter" "optional list"
c		set xreg(i) [ modifier ] [ value ]
c		set ireg(i) [ modifier ] [ value ]

	include 'datetc.h'
	include 'iounit.h'
	include 'infmtn.h'
	include 'set.h'
	include 'plcommon.h'
	include 'linparms.h'
	include 'inparms.h'
	include 'temp.h'


	character*80 string
  
	if (nstrng .eq. 0) then
	    string = 'all'
	else
	    string = alpha(1)(1:nalpha(1))
	endif
	call lcase(string, nalpha(1))
	if (string(2:4) .eq. 'reg') go to 8000

c						set phaseapod
	if (string .eq. 'phaseapod') then
	    nphap = inum(1)
	    return
	endif
c						set batch
	if (string .eq. 'batch') then
	    batch = .true.
	    return
	endif
c						set warn
	if (string .eq. 'warn') then
	    warn = .true.
	    return
	endif
c						set nowarn
	if (string .eq. 'nowarn') then
	    warn = .false.
	    return
	endif
c						set echo
	if (string .eq. 'echo') then
	    lecho = .true.
	    return
	endif
c						set noecho
	if (string .eq. 'noecho') then
	    lecho = .false.
	    return
	endif
c						set width
	if (string .eq. 'width') then
	    if (ifl .eq. 0) go to 9999
	    wdth = xnum(1)
	    return
	endif
c 						set damping
	if (string .eq. 'damping') then
	    if (ifl .eq. 0) go to 9999
	    damp = xnum(1)
	    return
	endif
c						set "readlength"
	if (string .eq. 'readlength') then
	   if ( inum(1) .eq. -1 ) then
	      rdlen = 2*nrtr
	   else
	      rdlen = inum(1)
	   end if
	   return
	endif
c 						set "stretch"
	if (string .eq. 'stretch') then
	    if (ifx .lt. 1) go to 9999
	    istret = inum(1)
	    return
	endif
c						set "read ref center"
c						set   centerread
	if (string .eq. 'centerread' .or. 
     *		string .eq. 'read ref center') then
	    rdref =  1
	    return
	endif
c						set "read ref first"
c						set  firstread 
	if (string .eq. 'firstread' .or. 
     *		string .eq. 'read ref first') then
	    rdref = 0
	    return
	endif

c						set "read scale"
c						set  scale
	if (string .eq. 'scale' .or. 
     &		string .eq. 'read scale') then
	   if (ifl .eq. 1) then
	      rdfctr = xnum(1)
	      rdscale = 1	! =1 : use internal scaling factor
	   else
	      rdscale = 2	! =2 : use scaling factor from header
	   end if
	   return
	endif

c						set "read no scale"
c						set  noscale
	if (string .eq. 'noscale' .or. 
     &		string .eq. 'read no scale') then
	   rdfctr = 1.0
	   rdscale = 0    ! =0 : no scaling at all
	   return
	endif

c						set "read no normalize"
c						set  nonorm
	if (string .eq. 'nonorm' .or. 
     &		string .eq. 'read no normalize') then
	    rdnorm = .false.
	    return
	endif

c						set "read normalize"
c						set  donorm
	if (string .eq. 'norm' .or. 
     &		string .eq. 'read normalize') then
	    rdnorm = .true.
	    return
	endif

*                                              set glitch
	if (string(1:4) .eq. 'glit') then
	   lglitch = .true.
	   return
	end if

*                                              set noglitch
	if (string(1:4) .eq. 'nogl') then
	   lglitch = .false.
	   return
	end if

c						set lsqcontinuum
	if (string(1:4) .eq. 'lsqc') then
	    if (ifl .lt. 1) go to 9999
	    lsqref = xnum(1)
	    rbar = lsqref
	    return
	endif
c						set lsqitnum
	if (string(1:4) .eq. 'lsqi') then
	    if (ifx .lt. 1) go to 9999
	    lsqcnt = inum(1)
	    return
	endif
c						set lsqrange
	if (string(1:4) .eq. 'lsqr') then
	    if (ifx .lt. 1) go to 9999
	    lsqlo = inum(1)
	    lsqhi = nol
	    if (ifx .ge. 2) lsqhi = inum(2)
	    return
	endif
c						set lsqtolerance
	if (string(1:4) .eq. 'lsqt') then
	    if (ifl .lt. 1) go to 9999
	    lsqtol = xnum(1)
	    return
	endif
c						set SVD tolerance
	if (string(1:4) .eq. 'svdt') then
	    if (ifl .lt. 1) go to 9999
	    svdtol = xnum(1)
	    return
	endif
c						set "plotrecall"
	if (string .eq. 'plotrecall') then
	    krnum = ifx
	    if (krnum .eq. 0) return
	    do i=1,ifx
		kr(i) = inum(i)
	    enddo
	    return
	endif
c						set "verbose"
	if (string .eq. 'verbose') then
	    verbos= .true.
	    return
	endif
c						set noverbose
	if (string .eq. 'noverbose') then
	    verbos = .false.
	    return
	endif
c						set "quiet"
	if (string .eq. 'quiet') then
	    verbos = .false.
	    return
	endif
c						set "inactive"
	if (string .eq. 'inactive') then
	    insflg = 0
	    return
	endif
c						set "active"
	if (string .eq. 'active') then
	    insflg = 1
	    return
	endif
c						set "menumode"
	if (string .eq. 'menumode') then
	    mnumode = inum(1)
	    return
	endif
c						set "tags"
	if (string .eq. 'tags') then
	    cstag = alpha(2)(1:4)
	    return
	endif
c						set "holds"
	if (string .eq. 'holds') then
	    nshold = inum(1)
	    return
	endif
c                                               set "complex"
	if (string .eq. 'complex') then
	    nwpp  = 2
	    return
	endif
c                                               set "real"
	if (string .eq. 'real') then
	    nwpp = 1
	    return
	endif
c                                               set "intregs"
	if (string .eq. 'intregs') then
	    do i=1,ifx
		ireg(i) = inum(i)
	    enddo
	    ireg(10) = ifx
	    return
	endif
c                                               set "realregs"
	if (string .eq. 'realregs') then
	    do i=1,ifl
		xreg(i) = xnum(i)
	    enddo
	    xreg(10) = ifl
	    return
	endif
c						set "verbose"
c						set "menumode"
c                                               set "complex"
c                                               set "intregs"
c                                               set "realregs"
	if (string .eq. 'all') then
	    write (wrtbuf,9001)
	    call wrtout(output,wrtbuf)
	    write (wrtbuf, 9002) batch, warn, lecho
	    call wrtout(output,wrtbuf)
	    if (rdnorm) then
	       write (wrtbuf, 9013) 
	       call wrtout(output,wrtbuf)
	    end if
	    if (.not. rdnorm) then
	       write (wrtbuf, 9003)  rdfctr
	       call wrtout(output,wrtbuf)
	    end if
	    if (rdref .eq. 0) then
	       write (wrtbuf, 9004)
	       call wrtout(output,wrtbuf)
	    end if
	    if (rdref .eq. 1) then
	       write (wrtbuf, 9005)
	       call wrtout(output,wrtbuf)
	    end if
	    write (wrtbuf, 9006) rdlen, istret
	    call wrtout(output,wrtbuf)
	    write (wrtbuf,9017) lsqref
	    call wrtout(output,wrtbuf)
	    write (wrtbuf,9018) lsqcnt
	    call wrtout(output,wrtbuf)
	    write (wrtbuf,9019) lsqlo,lsqhi
	    call wrtout(output,wrtbuf)
	    write (wrtbuf,9020) lsqtol
	    call wrtout(output,wrtbuf)

	    write (wrtbuf,9028)
	    call wrtout(output,wrtbuf)
	    write (wrtbuf,9029) wdth, damp
	    call wrtout(output,wrtbuf)
	    write (wrtbuf,9030) cstag, nshold, insflg
	    call wrtout(output,wrtbuf)
	    return
	endif

 9999	continue
	call wrtstr(' No parameter match - Syntax error for "set".')
	return

9001	format(' CURRENT VALUES FOR SET-ABLE VARIABLES')
9002	format(' Batch = ',l1,'   Warn = ',l1,'   Echo = ',l1     )
9003	format(' Autonormalize is OFF; read scale factor = ', 1p,e20.8)
9013	format(' Autonormalize is ON')
9004	format(' Read reference is the first point.')
9005	format(' Read reference is the center point.')
9006	format(' Read length = ',i7,'; Read stretching factor = ', 
     &         i4     )

 9017	format(' Lsqfit continuum = ',f10.5)
 9018	format(' Lsqfit iteration count =',i4)
 9019	format(' First/Last line in range to fit =',2i4)
 9020	format(' Delta-Sigma tolerance is ',f9.7)

 9028	format(' Settable line insertion parameters :')
 9029	format(' Width = ',f7.2,', Damping = ',f5.3)
 9030	format(' Tags = ',a4,', Holds =',i2,', Itn = ',i1)

c Register handling

8000	iadd = 0
	xadd = 0.
	imul = 1
	idiv = 1
	xmul = 1.
	ipara = inum(1)
	xpara = xnum(1)
	n = ichar( string(6:6) ) - ichar('0')
	if (n .le. 0 .or. n .gt. 9) n=10
	if (nstrng .ge. 2) then
	   if (string(1:1) .eq. 'i') ipara = ireg(n)
	   if (string(1:1) .eq. 'x') xpara = xreg(n)
	   if (alpha(2) .eq. 'up') then
	      if (string(1:1) .eq. 'i') iadd = inum(1)
	      if (string(1:1) .eq. 'x') xadd = xnum(1)
	   endif
	   if (alpha(2) .eq. 'down') then
	      if (string(1:1) .eq. 'i') iadd = -inum(1)
	      if (string(1:1) .eq. 'x') xadd = -xnum(1)
	   endif
	   if (alpha(2) .eq. 'times') then
	      if (string(1:1) .eq. 'i') imul = inum(1)
	      if (string(1:1) .eq. 'x') xmul = xnum(1)
	   endif
	   if (alpha(2) .eq. 'over') then
	      if (string(1:1) .eq. 'i') idiv = inum(1)
	      if (idiv .eq. 0) idiv = 1
	      if (xnum(1) .eq. 0.0) xnum(1) = 1.0
	      if (string(1:1) .eq. 'x') xmul = 1./xnum(1)
	   endif
	endif 
	if (string(1:1) .eq. 'i') then
	   ireg(n) = ipara*imul/idiv + iadd
	   write(wrtbuf,'(1x,2i4)') n, ireg(n)
	   call wrtstr(wrtbuf)
	   write(logfil,*) n, ireg(n)
	   return
	endif
	if (string(1:1) .eq. 'x') then
	   xreg(n) = xpara*xmul + xadd
	   write(wrtbuf,'(1x,i4,f12.4)') n, xreg(n)
	   call wrtstr(wrtbuf)
	   write(logfil,*) n, xreg(n)
	   return
	endif
	call wrtstr(' Illegal register type - no action')
	return
		
	end
c ---------------------------------------------------------------------

	subroutine echostr
*
* echo a string to the editor pane
*       
	include 'datetc.h'
	include 'iounit.h'
	
	if ( nstrng .gt. 0 ) then	   
	   write (wrtbuf,'(1x,a)') alpha(1)
	   call wrtstr( wrtbuf )
	end if

	return
	end

c -------------------------------------------------------------- 

	subroutine whats(r,tr,ffta,phz)

	include 'datetc.h'
	include 'infmtn.h' 
	include 'altinf.h' 
	include 'iounit.h' 
	include 'linparms.h' 
	include 'set.h' 

	real r(*),tr(*),ffta(*),phz(*)

	integer itrflag
	integer i
	external itrflag

	if (ifx .eq. 0) go to 1000
	call lcase(alpha(1)(1:1), 1)
	ipara = inum(2)
	call itron
	if (ifx .eq. 1) ipara = inum(1)
	do 620 i = inum(1), ipara
	  if (alpha(1)(1:1) .eq. 'r') then
	     write (wrtbuf,610) i, r(i)
	     call wrtout(output,wrtbuf)
	  end if
	  if (alpha(1)(1:2) .eq. 'tr') then 
	     write (wrtbuf,630) i, tr(i)
	     call wrtout(output,wrtbuf)
	  end if
	  if (alpha(1)(1:4) .eq. 'ffta') then 
	     write (wrtbuf,650) i, ffta(i)
	     call wrtout(output,wrtbuf)
	  end if
	  if (alpha(1)(1:3) .eq. 'phz') then 
	     write (wrtbuf,670) i, phz(i)
	     call wrtout(output,wrtbuf)
	  end if
	  call procpend
	  if (itrflag() .eq. 1) goto 700  ! hit the break
620	continue
700	continue
	call itroff
	return

610	format(   ' r(',i7,') = ',f18.8)
630	format(  ' tr(',i7,') = ',f18.8)
650	format(' ffta(',i8,') = ',f17.7)
670	format(' phz(',i7,') = ',f17.7)

1000	if (alpha(1)(1:3) .eq. 'nol') then
	   write (wrtbuf,'(i8)') nol
	   call wrtout(output,wrtbuf)
	else if (alpha(1)(1:5) .eq. 'linum') then
	   write (wrtbuf,'(i8)') linum
	   call wrtout(output,wrtbuf)
	else if (alpha(1)(1:4) .eq. 'ntop') then
	   write (wrtbuf,'(i8)') ntop
	   call wrtout(output,wrtbuf)
	else if (alpha(1)(1:7) .eq. 'intregs') then
	   write (wrtbuf,'(10i7)') 
     *		ireg(10),ireg(1),ireg(2),ireg(3),ireg(4),ireg(5),
     *		ireg(6),ireg(7),ireg(8),ireg(9)
	   call wrtout(output,wrtbuf)
	else if (alpha(1)(1:8) .eq. 'realregs') then
	   write (wrtbuf,'(5g14.5)') 
     *		xreg(10),xreg(1),xreg(2),xreg(3),xreg(4),xreg(5),
     *		xreg(6),xreg(7),xreg(8),xreg(9)
	   call wrtout(output,wrtbuf)
	else if (alpha(1)(1:4) .eq. 'uvar') then
	   write (wrtbuf,'(e20.8)') uvar9,
     *		uvar8,uvar7,uvar6,uvar5,uvar4,uvar3,uvar2,uvar1,uvar0
	    call wrtout(output,wrtbuf)
	 else if (alpha(1)(1:3) .eq. 'npo') then
	    write (wrtbuf,'(i8)') npo
	    call wrtout(output,wrtbuf)
	 else if (alpha(1)(1:7) .eq. 'ncenter') then
	    write (wrtbuf,'(i8)') ncenter
	    call wrtout(output,wrtbuf)
	 else if (alpha(1)(1:3) .eq. 'sav') then
	    call lssdir
	 else if (alpha(1)(1:4) .eq. 'disp') then
	    write (wrtbuf,'(3e20.8)') aps,corr,pref
	    call wrtout(output,wrtbuf)
	 else if (alpha(1)(1:2) .eq. 'up') then
	    call wrtstr(' Feeling funny, eh?')
	 else
	    call wrtstr(' Error :  unknown command option.')
	 end if
	 return

	end
