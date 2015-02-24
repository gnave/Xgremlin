c \decomp\src\atlsps.for   Ver. 2.4 - Atlas

c  Revision 2.46 92/10/13 Psplot version (postscript)
c  Revision 2.45 92/05/24 Laser conversion; output order now normal
c  Revision 2.44 92/02/17 Error message corrected
c  Revision 2.43 91/12/03 "noquantize" option added to final atlas call
c  Revision 2.42 91/08/24 minor corrections throughout
c  Revision 2.41 90/04/28 mca stretched log scale created
c  Revision 2.4  90/04/28 mca ATLS restructed to separate local, scale

        subroutine atls(r,tr)

        include 'datetc.h'
        include 'infmtn.h'
        include 'iounit.h'
        include 'set.h'

        real r(*), tr(*)

	character*80 line, label, fnam
        double precision s, sigma, dsig
        real ylow,yhigh,epsi,scl,rmax,rmin,sigo,sigf
        real x,plast,t,tlast,tt
        integer n,local, nchar
        integer itrflag
        logical atlabel

        external itrflag

        data fnam/'atlas.data'/

        if (nalpha(1) .eq. 0) go to 2000
        if (alpha(1)(1:nalpha(1)) .eq. 'rewind')     goto 600
        if (alpha(1)(1:nalpha(1)) .eq. 'close')     goto 700
        if (alpha(1)(1:nalpha(1)) .eq. 'local')      goto 800
        if (alpha(1)(1:nalpha(1)) .eq. 'scale')      goto 900
        if (alpha(1)(1:nalpha(1)) .eq. 'label')      goto 1000
        if (alpha(1)(1:nalpha(1)) .eq. 'noquantize') goto 2000
        if (alpha(1)(1:nalpha(1)) .eq. 'name' )      goto 4000

        call wrtstr(' Error :  specified operation not implemented')
        return

c atlas rewind
c ************
c rewind the plot file
 600    rewind plot
	return

c atlas close
c ***********
c close the plot file
700     close(unit=plot,status= 'keep',iostat=i)
        return
c 
c atlas local
c ***********
c If 'local' is specified, only data in r will be used
 800    local = 1
        return

c atlas scale [ylow yhigh]
c atlas scale epsi [stretched]
c atlas scale ylow yhigh epsi [stretched]
c ***********
c ylow and yhigh are the limits for linear plots
c epsilon is the noise level for loglin; if absent, plot is linear
c the optional parameter 'stretched' allows for a half scale loglin plot

c plot defaults:
900     ylow = 0.0
        yhigh = 1.0
        epsi = 0.0

	if (ifl .ge. 2) then
           ylow = xnum(1)
           yhigh = xnum(2)
	endif

        if (ifl .eq. 1) epsi = xnum(1)
	if (ifl .eq. 3) epsi = xnum(3)

c use ylow = -1.0 as a flag for the stretched loglinear scale
	if (alpha(2)(1:9) .eq. 'stretched') ylow = -1.0

        if (epsi .eq. 0.0) then
            scl = 945.0/(yhigh-ylow)
	else
	    rdnorm = .false.
	endif
        return

c atlas  label  "label 1-50 characters long" 
c but continue to accept old form:
c atlas  label  "label 1-50 characters long" ylow yhigh epsilon ['local']
c ***********************************************************************
c This call MUST precede the atlas call
1000    atlabel = .true.
	if (nstrng .eq. 3) then
	   local = 0
           if (alpha(3)(1:5) .eq. 'local') local = 1
        endif
	if (ifl .gt. 0) go to 900
c Write the label for atlas
1002    if (nstrng .lt. 2) go to 996
	nchar = nalpha(2)
        label(1:nchar) = alpha(2)(1:nchar)
        return

c atlas sigo sigf dispersion [noquantize]
c atlas sigo sigf ndat
c ******************************
c Write a file (unit=PLOT)in atlas format
c If "noquantize" is specified, the top panel begins at sigo

c - write Label; abort if no label given
2000    if (.not. atlabel) go to 999
	open (unit=plot,file=fnam,access='sequential',
     &       form='formatted',err=997)
        write (plot,2001) ylow,yhigh,epsi,nchar,label(1:nchar)
2001    format(2f10.4,e10.4,i4,a)

        call wrtstr(' Label written to plot')

c dispersion  is linear dispersion on plot, in WAVENUMBERS/CM
c If dispersion is given, assume 3030 rasters; otherwise use ndat
c write data (scaled to 1.0 = 945)
        if (ifl .eq. 3) then
           ndat = 6060
           dsp = xnum(3)
	elseif (ifl .eq. 2 .and. ifx .eq. 1) then
           ndat = 2*inum(1)
           dsp = (300.0/2.54)*(xnum(2) - xnum(1))/(inum(1) - 1)
	else
           go to 998
	endif

	sigo = xnum(1)
        sigf = xnum(2)
c dsig is final delw in WAVENUMBERS/RASTER
c   dp is delta sigma/panel.  
        dsig = dsp*2.54/300.0
        dp = 25.0*dsp
	call disper
c quantize limits unless 'noquantize' specified
        n = sigo/dp
	if(ifx .eq. 0 .and. nstrng .eq. 0) sigo = n*dp
c initialize loop, parameters for read
        m = (sigf - sigo)/dp
	if (m .eq. 0) m = 2
        if (mod(m,2) .eq. 1) m = m + 1
	if (ifx .eq. 1) then
           m = 1
           dp = 0
	endif
        sigf =  m*dp + sigo
        ifl = 1
        ifx = 1
        nstrng = 0
	ipara = 0
c ****** note that this destroys rdfctr for log-lin plots ****
        if (epsi .ne. 0.0) rdfctr = 1.0
c this number is a measure of coarseness; <1 is well interpolated,
c and >2 is quite coarse
        intflg = 2.0*dsig/delw

c start loop
        call itron       ! make it interruptible
        do 2700 ia=1,m
c DO NOT reverse page ordering for laser printer
c          jj = m - ia + 1
           jj = ia - 1
           sigma = sigo + jj*dp
c          if (mod(jj,2) .eq. 0) sigma = sigma - 2.0*dp
c read raw data for start of next panel
           if (local .eq. 1) go to 2100
           xnum(1) = sigma - dsig - delw*(16 + intflg/2)
           inum(1) = 4096
           call read(r,tr)
           if (ipara .ne. 0) then
              wref = xnum(1)
              nop = 4096
           endif
c interpolate a panel
 2100      s = sigma - dsig
           xparb = s 
           call wtop 
           plast = xparb 
           if (ipara .ne. 0) xparb = 0.0
           call conintrp(r)
           tlast = xparb 
c  
c These are barplots, not Bresenham's algorithm.  Each raster attempts to
c produce a bar that covers the entire range from the smallest value of the
c intensity to the largest in the bin covered by the raster.  For high
c resolution plots, it will run from the data value at the left edge to the
c data value at the right edge (so point values can be estimated directly 
c from the plot).  For very coarse plots, it simply covers the range of all 
c the raw data points in the bin.  For intermediate dispersion, the center
c point is also tested.
c There are thus ndat numbers for ndat/2 data points, since both the top and
c bottom of each raster is passed to atout.
  
           do 2320 i=1,ndat,2
              s = s + dsig
              xparb = s
              call wtop
              ii = xparb
              if (local .eq. 0 .and. ii .gt. nop-16-intflg/2) then
                 xnum(1) = s - delw*(16 + intflg/2)
                 inum(1) = 4096
                 call read(r,tr)
                 if (ipara .ne. 0) then
		    wref = xnum(1)
		    nop = 4096
                 endif
                 xparb = s
                 call wtop
              end if
              x = xparb
              if (ipara .ne. 0) xparb = 0.0
              call conintrp(r)
              t = xparb
              rmin = amin1(tlast,t)
              rmax = amax1(tlast,t)
c for coarser plots, test center point as well
              if (intflg .eq. 0 .or. ipara .ne. 0) go to 2310 
              xparb = s - 0.5*dsig
              call wtop
              if (ipara .ne. 0) xparb = 0.0
              call conintrp(r)
              tt = xparb
              rmax = amax1(tt,rmax)
              rmin = amin1(tt,rmin)
c for coarsest plots, test all intermediate raw points
              if (intflg .lt. 3) go to 2310
              ii = plast + 1.0
              jj = x
              do 2308 j=ii,jj
                 if (j .lt. 3 .or. j .gt. nop-4) go to 2308
                 rmax = amax1(rmax,r(j))
                 rmin = amin1(rmin,r(j))
 2308         continue
 2310         plast = x
              tlast = t
              tr(i) = rmin
              tr(i+1) = rmax
 2320      continue

           IF (epsi .eq. 0.0) then
c linear plot - normalize on positive peak only IF rdnorm is .true.
              rmax = 1.0
              if (rdnorm) then
                 rmax = -1.0e10
                 do i=1,ndat
		    if (tr(i) .gt. rmax)  rmax = tr(i)
                 end do
              end if

              do i=1,ndat
                 tr(i) = anint(( (tr(i)/rmax) -ylow)*scl)
              end do

           ELSE
c log-lin plot; if ylow = -1.0 use stretched loglinear scale
              xb = 1.0/(8.59596*epsi)
              do i=1,ndat
                 tr(i) = tr(i)*xb
                 if (tr(i) .le. 1.0 ) tr(i) = 0.086859*tr(i)
                 if (tr(i) .gt. 1.0 ) tr(i) = 0.086859*(1.0+alog(tr(i)))
                 if (tr(i) .lt. -2.0) tr(i) = -2.0
                 if (ylow  .eq. -1.0) tr(i) = 2.0*tr(i)
                 tr(i) = anint( tr(i)*945.0 )
              end do
           ENDIF

c write record
c mod for unix/pc compatibility - mca
           if (ia .gt. 1)write (plot,2001) 
     &        ylow,yhigh,epsi,nchar,label(1:nchar)
c mod for unix/pc file compatibility -mca
 2500      write (plot,2501) sigma,dsig,ndat/2
 2501      format(2e16.9,i8)
           write (plot,2502) (int(tr(i)),i=1,ndat)
 2502      format(15i5)
*          write(0,2601) ndat,sigma    ! muzzle it
* 2601     format (1x,i10,f15.5)
           call procpend
           if (itrflag() .eq. 1) then
              call itroff
              goto 2900
           end if
 2700   continue

 2900   call wrtstr(' data written to output file ')
        close(plot)

c Now write an info page to go with the atlas

	if (local .eq. 1) go to 2990
        if (inftype .eq. 0 .or. inftype .eq. 4) go to 2990
        open (unit=19,file='atlas.info',access='sequential',
     &       form='formatted',err=995)
        rewind datain
c header line
	read (datain,'(a80)') line
	write (19,'(///8x,a70/)') line
	call keepl(5)
	call skipl(2)
	call keepl(2)
        read (datain,'(a80)') line
        line(43:56) = 'atlas          '
        write (line(1:22),'(f14.0,8x)') sigo
        write (19,'(8x,a58)') line
        read (datain,'(a80)') line
        line(43:56) = 'atlas          '
        write (line(1:22),'(f14.0,8x)') sigf
        write (19,'(8x,a58)') line
	call skipl(2)
	call keepl(10)
	call skipl(1)
	call keepl(10)
	call skipl(2)
	call keepl(5)
	call skipl(3)
	call keepl(6)
	call skipl(16)
	call keepl(4)
	write (19,2950) epsi,cdat
 2950   format (/8x,f14.0,8x,'Assumed noise level in atlas'/
     &  12x,a10,8x,'Atlas date')
        call wrtstr(' info page written to file  atlas.info')
        close(19)

c reset flags and parameters
        atlabel = .false.
        ylow = 0.0
        yhigh = 1.0
        epsi = 0.0
        local = 0

 2990   goto 9999

*--- change output file name
 4000   fnam = alpha(2)
        return

c ERROR returns

 995    call wrtstr(' Error :  atlas.info  open failed')
        goto 9999
 999    continue
 996    call wrtstr(' Error :  atlas label command omitted')
        goto 9999
 997    call wrtstr(' Error :  failed to open output file')
        goto 9999
 998    call wrtstr(' Syntax error for atlas command - use:')
        call wrtstr('   atlas rewind')
        call wrtstr('   atlas local')
        call wrtstr('   atlas scale [ylow yhigh]')
        call wrtstr('   atlas scale epsi ["stretched"]')
        call wrtstr('   atlas label "up to 50 characters"')
        call wrtstr('   atlas name "output file name"')
        call wrtstr('   atlas sigo sigf dispersion')
        call wrtstr('   atlas sigo sigf ndat')

 9999   call itroff
        return

        end

c ------------------------------------------------------------------
	subroutine keepl(n)

        include 'iounit.h'

	character*80 line
	integer n

	do i=1,n
           read (datain,'(a80)') line
           write (19,'(8x,a58)') line
	end do
	return
	end
c ------------------------------------------------------------------
	subroutine skipl(n)

        include 'iounit.h'

	character*80 line
	integer n

	do i=1,n
           read (datain,'(a80)') line
	end do
	return
	end
c ------------------------------------------------------------------
	subroutine conintrp(r)
c conditional interpolation

        include 'datetc.h'
        include 'infmtn.h'

        real r(*)

	integer n
	n = xparb
	if (n .ge. 3 .and. n .lt. nop-4) then
           call interp(r)
	else
           xparb = 0.0
	endif
	return
	end
