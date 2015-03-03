c \decomp\atlas\plot\psplot.for

c PLOT PostScript Driver 
c Revision 1.01  11/14/89 mca  
c Version  1.0   12/03/90 mca  Genesis 
c
	subroutine plplot(ifend)
c
c this program makes atlas plots (and more) on Postscript printers
c
        include 'parms.h'

	integer ifd,ifdp,ifend
	integer ixorg,iyoff
	common/pbuf/ifd,ifdp,ixorg,iyoff

	integer ich(128),icw(128),icy(128),icx(128)
	common/pfs/ich,icw,icy,icx

        double precision wavel, x,dx,w,dw
	integer jy(3072,2)
	integer i,j,lxpl
	integer nchr, ijus, isl, isz 
	integer ia
	integer icnt,itick,itck
	integer izro
	integer mn,mj
	real epsi, scale, tck, stret
	character*80 label
	character*10 numch
	character*3 pmode
	character*6 gnum
        integer n, ni, nt, nref
	data  lxpl/3088/
c	data  ly/2400/
	
	ifend = 0
	ijus = -1
	idx = 0
	idy = 1
	isl = 0
	isz = 1

	iyoff = 0
        if (ilw .eq. 0) ilw = 1

c get the data header
        if (atls)then
	   read(datain,2001,end=2000) 
     &         ylow,yhigh,epsi,nchr,label(1:nchr)
 2001	   format(3f10.4,i4,a)
	   write(*,*) 'ylow  = ',ylow,', yhigh = ',yhigh
	   if(ylow .eq. -9999.0) then
	      ylow = -0.05 
	      stret = 1.0
	   endif
	endif
        if(limits)then
           iyras = nint((itop - ibot)/(gy2 - gy1))
           ylow = gy1
           yhigh = gy2
	   write(*,*) 'ylow  = ',ylow,', yhigh = ',yhigh, 'iyras =',iyras
        endif

        if (atls .or. limits) then
c Use the header info to set vertical scale factors for the data, box and
c tick marks.  dsp is the number of rasters per unit on the y axis 
	   icnt = 3088 - (itop + ibot)/2
	   scale = (itop - ibot)/945.
	   dsp = (itop - ibot)/(yhigh - ylow)
	   izro = ibot - (dsp*ylow)
	   itick = nint(dsp*sty)
	   write (*,*)' scale = ',scale,', rasters/unit = ',dsp
	   write (*,*)' zero at raster ',izro,', ',itick,' rasters/tick'
	   pmode(1:3) = 'lin'
	   if (epsi .ne. 0.0) pmode(1:3) = 'log'
	   if (logfl) pmode(1:3) = 'log'
        endif
c read the data. 1 and 2 are bot and top of plotted bars

        if (atls)then
	   read(datain,2501) sigma,dsig,ndat
	   read(datain,2502,end=2000) (jy(i,1),jy(i,2),i=1,ndat)
 2501	   format(2e16.9,i8)
 2502	   format(15i5)
	   write(*,*) 'wref  = ',sigma,', delw = ',dsig, 'ndat = ',ndat
	   write(*,*) pmode(1:3), nchr, label(1:nchr)
	endif

	if (limits)then
	   ndat = ixr - ixl
	   ixras = nint(ndat/(gx2 - gx1))
	   sigma = gx1
	   dsig = ((gx2 - gx1)/ndat)
	   write(*,*) 'wref  = ',sigma,', delw = ',dsig, 'ndat = ',ndat
	endif

c put in the offset and clip.
	do 50 i=1,ndat
	   do 50 ij=1,2
c rescale the data
c             jy(i,ij) = (jy(i,ij) - ylow)*scale
	      jy(i,ij) = jy(i,ij)*scale
	      if(epsi .ne. 0.0)then
		 j = jy(i,ij) + izro
	      else
		 j = jy(i,ij) + ibot
	      endif
	      if (j .lt. ibot) j = ibot
	      if (j .gt. itop) j = itop
	      jy(i,ij) = j
 50	continue
	write(*,*) ' data prepared'


c use fnt in effect at time box or grid was called for standard numbers
c but last font for x and y labels
	lfnt = fnt
	if (frstim) then
	   write (*,*) ' Clearing file '
	   if (epsfl)then
	      write (spool,10) 
 10	      format( '%begin(plot)')
	      write (spool,11) 
 11	      format( '%!PS-Adobe-2.0 EPSF-2.0')
	      write (spool,12) 
 12	      format( '%%BoundingBox: 0 0 612 792')
	      write (spool,13) 
 13	      format( '%%Pages: 0')
	      write (spool,14) 
 14	      format( '%%Creator: PSPLOT (NIST version)')
	      write (spool,15) 
 15	      format( '%%EndComments')
	      write (spool,16)
 16	      format( '%%EndProlog')
	      write (spool,17) 
 17	      format( '%%Page: "one" 1')
	   elseif(psfl)then
	      write (spool,18)
 18	      format( '%!PS')
	      write (spool,19) 
 19	      format( '%%Creator: PSPLOT (NIST version)')
	      write (spool,20) 
 20	      format( '%%EndComments')
	      write (spool,21)
 21	      format( 'clear')
	      write (spool,22)
 22	      format( 'initmatrix')
	      write (spool,'(f5.2,f5.2,6h scale)') xscal, yscal
	   endif
           write (spool,23)
23         format( '/scdict 3 dict def',/,
     *   '/subshow',/,
     *   ' {scdict begin',/,
     *   '  gsave',/,
     *   '   currentfont[.6 0 0 .6 0 subheight]makefont',/
     *   '    setfont',/
     *   '   show',/
     *   '   currentpoint',/
     *   '  grestore',/
     *   '  moveto',/)
           write (spool,25)
25         format( '  end',/
     *   ' }def',/
     *   ,/
     *   '/supshow',/
     *   ' {scdict begin',/
     *   '  gsave',/
     *   '   currentfont[.6 0 0 .6 0 supheight]makefont',/
     *   '    setfont',/
     *   '   show',/
     *   '   currentpoint',/)
           write (spool,26)
26         format( '  grestore',/
     *   '  moveto',/
     *   '  end',/
     *   ' }def',/
     *   ,/
     *   'scdict begin',/
     *   ' /supheight',/
     *   '   {gsave',/
     *   '      newpath',/
     *   '       0 0 moveto',/)
           write (spool,27)
27         format( '       (X) true charpath',/
     *   '      flattenpath',/
     *   '      pathbbox /capheight exch def pop pop pop',/
     *   '     grestore',/
     *   ,/
     *   '     capheight .6 mul',/
     *   '   } def',/
     *   ' /subheight',/
     *   '   {gsave',/
     *   '      newpath',/)
           write (spool,28)
28         format( '       0 0 moveto',/
     *   '       (X) true charpath',/
     *   '      flattenpath',/
     *   '      pathbbox /capheight exch def pop pop pop',/
     *   '     grestore',/
     *   ,/
     *   '     capheight -.5 mul',/
     *   '   } def',/
     *'end',//)
	else
	   write (*,*) ' Reading file '
	endif
c rotate the frame if necessary for landscape mode
	if (.not.horz)then
	   write (spool, 24)
 24	   format( /,'312 396 translate ',/,'90 rotate',/
     &            ,'-396 -312 translate ',/)
	endif

c use fnt in effect at time box or grid was called for standard numbers
	fnt = fntsav
        iszsav = fntsiz
        if (iszsav .eq. 0)iszsav = 10

c make the box rules 
	if (boxflg) then
	   call Xline(ixl,ixr,ibot)
	   call Xline(ixl,ixr,itop)
	   if(wavlen)call Xline(ixl,ixr,itop+6)
	   call Yline(ibot,itop,ixl)
	   call Yline(ibot,itop,ixr)
c setup the coordinate system for PostScript
c margins in the normal - portrait - mode
c margins in the rotated coordinate system
	end if
c       write (*,*) ibot,itop,izro
c       izdum = izro
	if(zeroline)then
	   if( ((izro +ii-1) .gt. ibot) .and. 
     *           ((izro +ii-1) .lt. itop) )then
c             call Xline(ixl,ixr,izro)
	      call Xline(ixl,ixr,izro)
	   endif
	endif
	if(draw)then
	   do ii = 0, nline
	      if (dlt(ii) .eq.  2)then
 53		 if (dx1(ii) .lt. dx2(ii))then
c                write (*,*)dx1(ii),dy1(ii),dx2(ii),dy2(ii),dlw(ii)
                    do jj = 0, dlw(ii)
		       call Xline(dx1(ii),dx1(ii)+10 ,dy1(ii)+jj-1)
                    enddo
                    dx1(ii) = dx1(ii) + 20
                    go to  53
		 endif
		 
 54		 if (dy1(ii) .lt. dy2(ii))then
c       write (*,*)dx1(ii),dy1(ii),dx2(ii),dy2(ii),dlw(ii)
                    do jj = 0, dlw(ii)
		       call Yline(dy1(ii),dy1(ii)+10,dx1(ii)+jj-1)
                    enddo
                    dy1(ii) = dy1(ii) + 20
                    go to  54
                 endif

	      else
		 do jj = 0, dlw(ii)
		    call Xline(dx1(ii),dx2(ii),dy1(ii)+jj-1)
		 enddo
		 do jj = 0, dlw(ii)
		    call Yline(dy1(ii),dy2(ii),dx1(ii)+jj-1)
		 enddo
	      endif
	   enddo
	endif
c       write (*,*) "******** NOP = ", nop
c Connect points with straight lines
c              IF (connect)then
c draw the actual lines.
	do ii = 1, nop-1
	   write (*,*) ixc(ii), iyc(ii)
	   islope = ((iyc(ii+1)-iyc(ii))/(ixc(ii+1)-ixc(ii)))
	   if(ixc(ii) .lt. ixl)then
	      iyc(ii) = iyc(ii) + islope*(ixl - ixc(ii))
	      ixc(ii) = ixl
	   elseif(ixc(ii) .gt. ixr)then
	      iyc(ii) = iyc(ii) + islope*(ixr - ixc(ii))
	      ixc(ii) = ixr
	   elseif(ixc(ii+1) .lt. ixl)then
	      iyc(ii+1) = iyc(ii+1) + islope*(ixl - ixc(ii+1))
	      ixc(ii+1) = ixl
	   elseif(ixc(ii+1) .gt. ixr)then
	      iyc(ii+1) = iyc(ii+1) + islope*(ixr-ixc(ii+1))
	      ixc(ii+1) = ixr
	   elseif(iyc(ii) .lt. ibot)then
	      ixc(ii) = ixc(ii) + (ibot - iyc(ii))/islope 
	      iyc(ii) = ibot
	   elseif(iyc(ii) .gt. itop)then
	      ixc(ii) = ixc(ii) + (itop - iyc(ii))/islope
	      iyc(ii) = itop
	   elseif(iyc(ii+1) .lt. ibot)then
	      ixc(ii+1) = ixc(ii+1) + (ibot - iyc(ii+1))/islope
	      iyc(ii+1) = ibot
	   elseif(iyc(ii+1) .gt. itop)then
	      ixc(ii+1) = ixc(ii+1) + (itop - iyc(ii+1))/islope
	      iyc(ii+1) = itop
	   endif
	   call xyplot(ixc(ii),iyc(ii),ixc(ii+1), iyc(ii+1))
 55	enddo
c       ENDIF
   
c y ticks
	IF (grid .and. idec .ne. 0) THEN
c 			*****Semi-log grid lines*****
	   itick = (itop - ibot)/idec
	   do i = 0, idec
	      itk = ibot + itick*i
c       minor grid lines
	      if (i .lt. idec) then
		 tck = 1.0
		 do j = 1, 8
		    tck = tck + 1.0
c       multiply by log(e) to obtain common logs
		    itck = itk + itick*alog(tck)*0.4342944e+0
		    call Xline(ixl,ixr,itck)
		 enddo
	      end if
c       major grid lines
	      if (i .gt. 0 .and. i .lt. idec) then
		 call Xline(ixl,ixr,itk)
		 call Xline(ixl,ixr,itk+1)
	      end if
c       Semi-log numbers
	      iexp = idec0 + i
	      if (iexp .le. -4) then
c       -99 to -4
		 write (gnum,'(''10e'',i3)') iexp
	      elseif (iexp .eq. -3) then
		 write (gnum,'('' 0.001'')') 
	      elseif (iexp .eq. -2) then
		 write (gnum,'(''  0.01'')') 
	      elseif (iexp .eq. -1) then
		 write (gnum,'(''   0.1'')') 
	      elseif (iexp .eq. 0) then
		 write (gnum,'(''   1.0'')') 
	      elseif (iexp .le. 3) then
c       1 to 3
		 write (gnum,'(f6.0)') float(10**iexp)
	      else
c       4 to 999
		 write (gnum,'(''10e'',i3)') iexp
	      endif
	      call Chplot(6,gnum,ixl-10,itk-10,1,1,0,isl,iszsav)
	   enddo
	ELSEIF (boxflg) THEN

	   If (.not. notick) Then
c big ticks 
	      if (bty .eq. 0.0)bty = 0.1
	      dbtk = dsp*bty
	      nmin = (ibot - izro)/dbtk
	      if (nmin .eq. 0)nmin = 1 + (ibot - izro)/dbtk
	      nmax =     (itop - izro)/dbtk
	      do 140 i=nmin,nmax
		 mj = izro + i*dbtk
		 if ((mj .lt. ibot) .or. (mj .gt. itop))go to 140
		 if (ltick)then
		    call Xline(ixl,ixl+20,mj)
		 elseif (rtick)then
		    call Xline(ixr-20,ixr,mj)
		 else
		    call Xline(ixl,ixl+20,mj)
		    call Xline(ixr-20,ixr,mj)
		 endif
		 if (grid) call Xline(ixl,ixr,mj)
		 if (grid) call Xline(ixl,ixr,mj+1)
 140	      continue

c small ticks 
	      if (sty .eq. 0.0)sty = 0.05
	      dstk = dsp*sty
	      nmin = (ibot - izro)/dstk
	      if (nmin .eq. 0)nmin = 1 + (ibot - izro)/dbtk
	      nmax =     (itop - izro)/dstk
	      do 141 i=nmin,nmax
		 mn = izro + i*dstk
		 if ((mn .lt. ibot) .or. (mn .gt. itop))go to 141
		 if (ltick)then
		    call Xline(ixl,ixl+10,mn)
		 elseif (rtick)then
		    call Xline(ixr-10,ixr,mn)
		 else
		    call Xline(ixl,ixl+10,mn)
		    call Xline(ixr-10,ixr,mn)
		 endif
		 if (grid) call Xline(ixl,ixr,mn)
 141	      continue
	      
c left numbers
c ***************temporary - hard-wired to -1.0 to 1.0, lty=0.2
	      if (noynum) go to 200
	      if (pmode .eq. 'lin')then
		 if(lty .eq. 0.0)lty = 0.2
		 dltk = dsp*lty
		 nmin =     (ibot - izro)/dltk
		 nmax =     (itop - izro)/dltk
		 do 150 i=nmin,nmax
		    itk = izro - 10 + i*dltk
		    y = lty*i
		    
		    if (lty .lt. 0.001) then
c       < 0.001
		       write (gnum,'(e6.1)') y
		    elseif (lty .lt. 0.01) then
c       0.001 to .00999
		       write (gnum,'(f6.3)') y
		    elseif (lty .lt. 0.1) then
c       0.01 to .0999
		       write (gnum,'(f6.2)') y
		    elseif (lty .lt. 1.0) then
c       0.1 to .999
		       write (gnum,'(f6.1)') y
		    elseif (lty .lt. 1000.0) then
c       1. to 999.
		       write (gnum,'(f6.0)') y
		    else
c       1000 and up
		       write (gnum,'(e6.1)') y
		    endif
		    if ((itk .lt. ibot) .or. (itk .gt. itop))go to 150
		    
		    if (ltick)then
		       call Chplot(6,gnum,ixl-10,itk,1,1,0,isl,iszsav)
		    elseif (rtick)then
		       call Chplot(6,gnum,ixr+20,itk,-1,1,0,isl,iszsav)
		    else
		       call Chplot(6,gnum,ixl-10,itk,1,1,0,isl,iszsav)
		    endif
		    
 150		 continue
	      endif
	   EndIf

	   If (pmode .eq. 'log') then
	      call Chplot(6,'Linear',
     &		          ixl-80,ibot,ijus,0,1,isl,iszsav)
	      call Chplot(3,'Log',
     &	                  ixl-80,ibot+210,ijus,0,1,isl,iszsav)
	      
	      if(lty .eq. 0.0)lty = 0.2
	      dltk = dsp*lty
	      nmin =     (ibot - izro)/dltk
	      nmax =     (itop - izro)/dltk
	      do 190 i=nmin,nmax
		 j = i
                 if (j .lt. 1)go to 190
		 if(stret .eq. 1.0)then
		    if (j .eq. 2)go to 190
		    if (j .eq. 3)j = j
		    if (j .eq. 4)go to 190
		    if (j .eq. 5)j = j
		 endif
c changed 1/25/91 for reg. log scale, not checked for stretched form!!
c       itk = izro - 10 + j*dltk
		 itk = izro - 10 + j*dltk -dltk/2.0
		 if(stret .eq. 1.0)then
		    if (j .eq. 3)j = j - 1
		    if (j .eq. 5)j = j - 2
		 endif
		 call Chplot(2,'10',
     *		    ixl-30,itk,1,1,0,isl,iszsav)
		 call Chplot(1,char(j+48),
     *		    ixl-10,itk+10,1,1,0,isl,iszsav-2)
 190	      continue
	      call Chplot(1,'0',
     *			ixl-30,izro-10,1,1,0,isl,iszsav)
 200	   End If

	ENDIF

c write cm-1 and Angstrom labels if necessary
	if (cmlabel) call Chplot(4,'cm-1',
     *			ixr-78,ibot-30,ijus,1,0,isl,iszsav)
	if (wavlen) then
	   if (nanomet)then
	      call Chplot(2,'nm',ixr-45,itop+38,ijus,1,0,6,iszsav)
	   else
	      call Chplot(1,'A',ixr-26,itop+38,ijus,1,0,6,iszsav)
	   endif
	endif

c now add bottom x tick marks and wave scale
c
        IF (.not. boxflg) GO TO 320
c       if (notick) go to 215

        n = 0.0
	dx = 0.0
	ni = 0.0
	nref = 0.0
	x = 0.0
	mn = 0.0
c find number of 0.1 units/tick, quantize 1,2,5,10,20...=nt
c ticks to be >48 units apart.  dx is rasters/tick
        n =480.0*dsig + 0.999999
        call qantiz(n,nt,nm)
        dx = nt*0.1/dsig
c       write (*,*) 'n, nt, nm = ', n, nt, nm
c nint truncates to nearest integer
c       ni = nint(sigma/nt  + 0.00001)
        ni = int(sigma/nt  + 0.00001)
        nref = ni*nt
c begin tick loop. n is number of tenths from zero. x is in rasters
        n = 10*nref
        x = (nref - sigma)/dsig
c       write (*,*) sigma, ni, nref, n, x
        if (sigma .le. 0.)then
c this permits negative tick marks
          x =  (sigma - nref)/dsig
        endif
c       if(sigma .le. 0.)x = (sigma-nref)/dsig
c       if(sigma .le. 0.)x = (sigma)/dsig
c       if(sigma .le. 0.)x = (nref + sigma)/dsig
205     mn = nint(x)
        mnum = mn
c       write (*,*) n,nt,nm,ni,nref,sigma,dsig,x, mn,mnum
c       write (*,*) n, n/10
c the next line piles all x numbers at x = 0
c       if (sigma .le. 0)mnum = nint(nref - sigma/dsig)

c       if (mn .lt. 0) go to 210
c       if (mn .lt. ixl) go to 210
        if (mn .gt. ixr -ixl) go to 215
	if(mn .eq. 0) then
		mn = 1
c                mnum = 1
	endif
c set size of tick
            iht = 10
            if (mod(n,  5*nm) .eq. 0) iht =  15
            if (mod(n, 10*nm) .eq. 0) iht = 18
            if (mod(n, 50*nm) .eq. 0) iht = 25

c draw the tick marks on the top and bottom of the box
                if (grid) then
                 if (mod(n, 5*nm) .eq. 0) call Yline(ibot,itop,ixl+mn)
                 if (mod(n,10*nm) .eq. 0) call Yline(ibot,itop,ixl+mn+1)

                else
c                  write (*,*) ixl,ixr,mn,ixl+mn
c			call Yline(ibot,ibot+iht,ixl+mn)
c			call Yline(itop-iht,itop,ixl+mn)
                  if (ixl+mn .gt. ixl)call Yline(ibot,ibot+iht,ixl+mn)
                  if (ixl+mn .gt. ixl)call Yline(itop-iht,itop,ixl+mn)
                endif
c every tenth (or ltx'th) tick gets a number
208         ntst = 10*nt
          if (noxnum) go to 210
c            if (ltx .ne. 0.0)ntst = (nt*ltx)/(2*stx)
	    if (ltx .ne. 0.0)ntst = (nt*ltx)/(stx)
            if ( mod(n,ntst) .ne. 0) go to 210
        	if (mn-60 .ge. ixr -ixl) go to 215
c find number offset using '0' as model
c                call Chplot(0,numch,0,0,0,0,1,isl,iszsav)
c                ino = isz*ich(48) + 15
                IF (sigma .ge. 0)then
                if (n .gt. 99999) then
			write(numch,'(i5)') n/10
			nch = 5
                elseif (n .gt. 9999) then
			write(numch,'(i4)') n/10
			nch = 4
                elseif (n .gt. 999) then
			write(numch,'(i3)') n/10
			nch = 3
                elseif (n .gt. 99) then
			write(numch,'(i2)') n/10
			nch = 2
                elseif (n .gt. .99) then
			write(numch,'(i1)') n
			nch = 1
                else
			write(numch,'(i1)') n/10
			nch = 1
		endif
                ELSE
                if (n .lt. -99999) then
			write(numch,'(i6)') n/10
			nch = 6
                elseif (n .lt. -9999) then
			write(numch,'(i5)') n/10
			nch = 5
                elseif (n .lt. -999) then
			write(numch,'(i4)') n/10
			nch = 4
                elseif (n .lt. -99) then
			write(numch,'(i3)') n/10
			nch = 3
                elseif (n .lt. -.99) then
			write(numch,'(i2)') n
			nch = 2
                elseif (n .gt. 99999) then
			write(numch,'(i5)') n/10
			nch = 5
                elseif (n .gt. 9999) then
			write(numch,'(i4)') n/10
			nch = 4
                elseif (n .gt. 999) then
			write(numch,'(i3)') n/10
			nch = 3
                elseif (n .gt. 99) then
			write(numch,'(i2)') n/10
			nch = 2
                elseif (n .gt. .99) then
			write(numch,'(i1)') n
			nch = 1
                else
			write(numch,'(i1)') n
			nch = 1
		endif
                ENDIF
                if (ixl+mnum .le. ixr)then
c               if (ixl+mnum .lt. (ixr - 30))then
c                 write (*,*) sigma, n, n/10, nch
                  call Chplot(nch,numch,
     *			ixl+mnum,ibot-40,0,1,0,isl,iszsav)
c               else
c                call Chplot(nch,numch,
c    *			ixl+mnum,ibot-40,0,1,0,isl,iszsav)
                endif
210       x = x + dx
          n = n + nt
          go to 205

215     continue
        if(wavlen)then
c now do ticks on wavelength scale
c find number of 0.1 units/tick, quantize 1,2,5,10,20...=nt
c ticks to be >48 units apart
        call sigtow(sigma,wavel)
        if (sigma .ne. 0.0)  dlam = dsig*wavel/sigma
        x = sigma + lxpl*dsig
        call sigtow(x,wavel)
        if (sigma .eq. 0.0)  dlam = 0.005*wavel
        n = 480.0*dlam + 0.999999
        call qantiz(n,nt,nm)
        dw = nt*0.1
        ni = wavel/nt  + 0.00001
        nref = ni*nt
c begin tick loop.  n is number of tenths from zero. x is in points
        n = 10*nref
        w = nref
220     call wavtos(x,w)
        x = (x - sigma)/dsig
	if (sigma .eq. 0.0 .and. x .lt. 500.0) go to 300
	mn = nint(x + 0.5)
	mnum = mn
	if (mn .lt. 0) go to 300
        if (mn .gt. ixr -ixl) go to 225
        if (mn-60 .ge. ixr -ixl) go to 225
c set size of tick
            iht = 10
            if (mod(n,  5*nm) .eq. 0) iht =  15
            if (mod(n, 10*nm) .eq. 0) iht = 20
            if (mod(n, 50*nm) .eq. 0) iht = 25
c draw the tick
		call Yline(itop+6,itop+6+iht,ixl+mn)
c every tenth tick gets a number
222       if ( mod(n,10*nt) .ne. 0) go to 225
		if (mnum .lt. 250) go to 225
		if (ixr-mnum .lt. 250) go to 225
		if (mnum .lt. 1) go to 300

                if (nanomet)then

                if (n .le. 999999) then
			write(numch,'(i5)') n/10
		call Chplot(4,numch,
     *			ixl+mnum,itop+43,0,1,0,6,iszsav)
		else
			write(numch,'(i6)') n/10
                 call Chplot(5,numch,ixl+mnum-6,itop+43,0,1,0,6,iszsav)
		endif

                else

                if (n .le. 999999) then
			write(numch,'(i5)') n/10
		call Chplot(5,numch,
     *			ixl+mnum,itop+43,0,1,0,6,iszsav)
		else
			write(numch,'(i6)') n/10
                 call Chplot(6,numch,ixl+mnum-6,itop+43,0,1,0,6,iszsav)
		endif

                endif

225       w = w + dw
          n = n + nt
          go to 220
300	continue
        end if
320     CONTINUE
c
c make the barplot raster 
        if(atls)then
c	   do 400 i=ixl,ixr
	   do 400 i=ixl,ixr-1
		ia = i + 1 - ixl
		ij1 = jy(ia,1)
		ij2 = jy(ia,2)
c if the line has no vertical extent, make it 1/300 (one point) tall
                 if (ij2 .eq. ij1)ij2 = ij1 + 1
		 call Yline(ij1,ij2,i)
                 if (mod(ia, 250) .eq. 0) then
                  write(spool,401)ilw*0.25
401               format(1x, f4.2, ' setlinewidth stroke')
                 endif
400	   continue
         else
	   fnt = dotfnt
c calculate the dot character offset based on its size
           idxoff = (300./72.)*.38*dotsz
           idyoff = (300./72.)*.355*dotsz
           do i = 1, nop
c check to make sure the dot is inside the box
              if ((ixc(i) .le. ixl) .or. (ixc(i) .ge. ixr))go to 410
              if ((iyc(i) .le. ibot) .or. (iyc(i) .ge. itop))go to 410
              ixc(i) = ixc(i) - idxoff
              iyc(i) = iyc(i) - idyoff
       	      call Chplot(1, dt, ixc(i), iyc(i), -1, 1, 0, 0, dotsz )
410        enddo 
         endif

c
c print label
c
c  Place a label at the position (lbx,lby) on the image.
800	  if (nlabl .ne. 0) then
	  	write (*,'(a,$)') ' Writing label No. '
		do 900 i=1,nlabl
	  		write (*,'(i2,$)') i
			fnt = lfont(i)
        		call Chplot( nlb(i), labl(i)(1:nlb(i)),
     *			lbx(i), lby(i), ijust(i),
     *			idlx(i), idly(i),islt(i),isiz(i) )
900		continue
	   end if
	   write (*,*) ' '


c remove the rotation necessary for landscape mode

        if (.not.horz) then
	   write (spool, 1001)
 1001	   format( /,'396 312 translate',/ '270 rotate',/
     &	   '-312 -396 translate')
        endif
c reset flags

	horz = .false.
	atls = .true.
	axis = .false.
	notick = .false.
        limits = .false.
        noxnum = .false.
        noynum = .false.
	boxflg = .false.
	grid = .false.
	axlabl = .false.
	aylabl = .false.
	lbl = .false.
	nlabl = 0
	zeroline = .false.
	oneline = .false.
	wavlen = .false.
	cmlabel = .false.
	rtscale = .false.
        draw = .false.
        connect = .false.
        rtick = .false.
        ltick = .false.

c normal termination
        return

600	write(*,*) 'open error'
        return

2000	write(*,*) 'Unexpected EOF in data - aborted'
	ifend = 1
        return
	end


c-------------------------------------------------------------------

	subroutine dump

        include 'parms.h'

	if (epsfl) then
	   write (spool,*)' '
	   write (spool,101)ilw*0.25
 101	   format(1x,f4.2, ' setlinewidth stroke')
	   write (spool,*)' '
	   write (spool,102)
 102	   format( '%%Trailer')
	   write (spool,103)
 103	   format( '%end(plot)')
	elseif(psfl)then
	   write (spool,*)' '
	   write (spool,101)ilw*0.25
	   write (spool,104)
 104	   format( 'showpage')
	endif
	write (*,*) '>>>>>   page complete   <<<<<'
	write (*,*) ' '

c       reset the label parameters to zero
c       x-axis label
        ncxl = 0
        xlabl = ' '
        jus1 = 0
c       y-axis label 
        ncyl = 0
        ylabl = ' '
        jus2 = 0
c       generic labels
        nlabl = 0
	return

        end
