	program psplot

	include 'parms.h'

        character ctim*10, arg*80
	integer ijstsv, islsav, idxsav, idysav, iargc, k
	integer ifend
	integer ich(128),icw(128),icy(128),icx(128)
	common/pfs/ich,icw,icy,icx
C	external iargc
	external dtime

	data ijstsv/0/, islsav/0/, iszsav/1/, idxsav/1/, idysav/0/


* parse command line  (f2c specific)
* usage :  psplot <inputfile> [-i <init file>] [-o <output file>]
*
	infil  = ' '
	outfil = ' '
	confil = ' '
C (GN, Feb 2015) Replace old iargc to conform to Fortran 2003.
        iargc = command_argument_count()
	if ( iargc .ne. 0 ) then
	   call getarg(1,arg)
	   if ( arg(1:1).ne.'-' ) then ! is input file name
	      infil(1:80) = arg(1:80)
	   end if
	   do k=1, iargc                   ! clumsy but works
	      call getarg(k,arg)
	      if ( arg(1:2).eq.'-i' ) then   ! config file
		 if ( length(arg) .gt. 2 ) then
		    confil = arg(3:)
		 else
		    call getarg( k+1, confil )
		 end if
	      end if
	      if ( arg(1:2).eq.'-o' ) then   ! output file
		 if ( length(arg) .gt. 2 ) then
		    outfil = arg(3:)
		 else
		    call getarg( k+1, outfil )
		 end if
	      end if
	   end do
	end if

c initialize and open init files if present
	call mainit
	if (ipara .eq. 0) then
	   nstrng = 0
	   call comin
	endif

c read and decode command

11	lp(1:) = ' '
1	call gboss
	if (verbos) then
	   call dtime(ctim)
	   write (output,2) ctim, kin(1:nc)
 2	   format (1x,a9,3x,a)
	end if
	if (nact .lt. 0) go to 100
	if (nact .gt. 0) go to 10
3	if (keybd) go to 1
	call gboss
c check for BEGIN
	if (nact .eq. 4) go to 1
c check for BYE,END
4	if (nact .eq. 1 .or. nact .eq. 2) go to 100
	go to 3
10	go to (
     &		 100, 100, 300, 400, 500, 600, 700, 800, 900,1000,
     &		1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,
     &		2100,2200,2300,2400,2500,2600,2700,2800,2900,3000,
     &		3100,3200,3300,3400,3500,3600,3700,3800,3900,4000,
     &		4100,4200,4300,4400,4500,4600,4700,4800,4900,5000,
     &		5100,5200,5300,5400,5500,5600,5700,5800,5900,6000)
     &          nact
 
c
c alias		define an alias or synomym for a command string
c ******
300	call alias
	go to 1
c
c unused
c *******
400  	continue
	go to 1
c
c axis    
c *****
500  	axis = .true.           
	go to 1
c
c box         Request a box
c ***
600	boxflg = .true.
	zeroline = .true.
	fntsav = fnt
        fntsiz = iszsav
	go to 1
c
c close		close files.  call - close <unitname>
c *****
700  	continue
	go to 90
c
c connect     connect the points in the data arrays with a line 
c ******
  800 continue                
        iprint = inum(1)
        do i = 1, nop
          ixc(i) = ixl + xras*(xcol(i) - gx1)
          iyc(i) = ibot + yras*(ycol(i) - gy1)
          if (iprint .ne. 0)write (*,*) xcol(i), ixc(i), ycol(i), iyc(i)
        enddo
        connect = .true.
       go to 1
c
c direction	- of lettering: 'up', 'down', 'right', 'left'
c *********
900  	  if (alpha(1)(1:nalpha(1)) .eq. 'right') then
		idxsav = 1
		idysav = 0
	  endif
  	  if (alpha(1)(1:nalpha(1)) .eq. 'left') then
		idxsav = -1
		idysav = 0
	  endif
  	  if (alpha(1)(1:nalpha(1)) .eq. 'down') then
		idxsav = 0
		idysav = -1
	  endif
  	  if (alpha(1)(1:nalpha(1)) .eq. 'up') then
		idxsav = 0
		idysav = 1
	  endif
	go to 1
c
c dot         Place a dot on the screen
c ***
1000 	continue                
c calculate the raster positions corresponding to the global coordinate 
c positions.
        lbl = .true.
        dotsz = inum(1)
        dotfnt = 13
        do i = 1, nop
          ixc(i) = ixl + xras*(xcol(i) - gx1)
          iyc(i) = ibot + yras*(ycol(i) - gy1)
          dt = alpha(1)(1:1)
          if (inum(2).ne.0) write (*,*) xcol(i),ycol(i),ixc(i),iyc(i)
        enddo

       go to 1
c
c draw        Connect the points (x1,y1) and (x2,y2) with a line 
c ****
1100 	nline = nline + 1
        dx1(nline) = nxsav
	dy1(nline) = nysav
        dx2(nline) = inum(1)
        dy2(nline) = inum(2)
        dlw(nline) = inum(3)
        dlt(nline) = inum(4)
        draw = .true.
        go to 1

* break    an alias for  keyboard
*******
1200 	continue
	input = stdin
	keybd = .true.
	go to 1

c
c erase       Clear the bitmap 
c *****
 1300  frstim = .true.
       go to 1
c
c font   
c ****
 1400	fnt = inum(1)
	if(fnt .eq. 0)then
	   write(*,*)' The available fonts are:' 
	   write(*,*)' 1. ten point sans serif - plss10'
	   write(*,*)' 2. twenty point sans serif - plss20'
	   write(*,*)' 3. ten point Times Roman - pltr10 (default)'
	   write(*,*)' 4. twenty point Times Roman - pltr20'
	   write(*,*)' 5. twenty point bold sans serif - plbs20'
          write(*,*)' 6. twenty point symbols - plsy20'
	endif
	go to 1
c
c grid        Define a grid of lines at every major tick mark
c ******
1500	grid = .true.           
c	zeroline = .false.
	fntsav = fnt
c       fntsiz = iszsav
c      go to 1
c

c dump Dump the bitmap to the printer
c *****
1600	call dump
	frstim = .true.
         go to 1
c
c help  not implemented in NIST version
c ****	
 1700	input = stdin
	keybd = .true.
	go to 1

c histogram    Generate a histogram of the data
c *********
 1800 continue                
	go to 90
c
c horizontal	Switch to horizontal mode 
c **********      
1900  horz=.true.
	go to 1
c
c justify 
c *******
2000  if (alpha(1)(1:nalpha(1)) .eq. 'left') then
		ijstsv = -1
	endif
	if (alpha(1)(1:nalpha(1)) .eq. 'right') then
		ijstsv = 1
	endif
	if (alpha(1)(1:nalpha(1)) .eq. 'center') then
		ijstsv = 0
	endif
	go to 1
c
c keyboard      redirect the command input to the terminal keyboard.
c ********         this MUST be the LAST command on a line
 2100	input = stdin
	keybd = .true.
	go to 1
c
c label		Place the label defined with 'label' at the position (x,y)
c ***** 
 2200 continue 
       go to 1
c
c limits	Define the logical limits of the plot
c ******      
2300  if (.not. locat)then
         write (*,*) ' Physical coordinates NOT set 
     * - use the LOCATION command'
         go to 1
      endif
      gx1 = xnum(1)
      gx2 = xnum(2)
      gy1 = xnum(3)
      gy2 = xnum(4)
c determine the relation between rasters and global coordinates
        ndat = ixr - ixl
        xras = (ndat/(gx2 - gx1))
        yras = ((itop - ibot)/(gy2 - gy1))
        write (*,*) 'ndat = ', ndat,'xras = ',xras, 'yras = ',yras
       ylow = gy1
       yhigh = gy2
       sigma = gx1
       dsig = ((gx2 - gx1)/ndat)

       write(*,*) 'ylow  = ',ylow,', yhigh = ',yhigh
       write(*,*) 'wref  = ',sigma,', delw = ',dsig, 'ndat = ',ndat
       limits = .true.
       go to 1
c
c location	Define the placement of the image on the printer
c ********      
2400  	ixl = inum(1)
	ixr = inum(2)
	ibot = inum(3)
	itop = inum(4)
        locat = .true.
	go to 1 
c
c ltype        Define the line type to be used for drawing 
c *****
 2500 continue     
       go to 1
c
c lweight      Define the line weight (width) to be used for drawing
c *******       
 2600  ilw = inum(1)
       if (ilw .eq. 0) ilw = 1
       go to 1
c
c notick 	eliminate the tick marks if desired.      
c ******
2700  notick = .true.         
	go to 1
c
c open		open files.  call - open unitname filename
c ****
2800  	continue
c	call dopen
	go to 90
c
c option - user coded and implemented
c ******
2900	call option             
	go to 1
c
c putlabel        Writes a string placed on the page at the position 
c ********        specified with relocate 
 3000	continue
	lbl = .true.
	nlabl = nlabl + 1
	lbx(nlabl) = nxsav
	lby(nlabl) = nysav
	lfont(nlabl) = fnt
	nlb(nlabl) = nalpha(1)
	labl(nlabl) = alpha(1)(1:nalpha(1))
	if (ifx .ne. 5) go to 3010
	ijust(nlabl) = inum(1)
	islt(nlabl) = inum(2)
	isiz(nlabl) = inum(3)
	idlx(nlabl) = inum(4)
	idly(nlabl) = inum(5)
	go to 1
 3010	ijust(nlabl) = ijstsv
	islt(nlabl) = islsav
	isiz(nlabl) = iszsav
	idlx(nlabl) = idxsav
	idly(nlabl) = idysav
	go to 1
c
c relocate     Move the cursor to the position (x,y) without drawing a line 
c ********
 3100  continue
          nxsav = inum(1)
          nysav = inum(2)
       go to 1
c
c rewind       Rewind the command unit  (com)
c ******
 3200	continue
	if (uopen(cardin) .eq. 1) then
	   rewind(cardin)
	else
	   write(*,*) 'Error :  unit cardin not open.'
	end if
	go to 1
c
c run         Switch the control input from the keyboard to a file
c ***         This MUST be the LAST command on a line.
 3300	continue
	nump = 1
        call comin
	go to 1
c
c screen      Return output to the screen
c ******       
 3400  output = stdout
       go to 1
c
c size
c ****
3500  iszsav = inum(1)
	go to 1
c
c slant   
c *****
3600  islsav = inum(1)
	go to 1
c
c spool       Re-direct output to a file named "spool"
c *****       
 3700  output = spool
       go to 1
c
c store        Add the current lines and labels to the bitmap image 
c *****       
 3800	call plplot(ifend)
	frstim = .false.
        locat = .false.
	if (ifend .eq. 1) then
	   input = stdin   ! equivalent to a keyboard command
	   keybd = .true.
	else
	   go to 1
	end if

* UNUSED
********
 3900	continue
	goto 1

c
c ticksize    Set the spacing between ticks and labels
c ********	stx 	- interval between small ticks on the x axis
c        	btx 	- interval between big ticks on the x axis
c        	ltx 	- interval between tick labels on the x axis
c        	sty 	- interval between small ticks on the y axis
c        	bty 	- interval between big ticks on the y axis
c        	lty 	- interval between tick labels on the y axis
 4000 stx = xnum(1)
      btx = xnum(2)
      ltx = xnum(3)
      sty = xnum(4)
      bty = xnum(5)
      lty = xnum(6)
	go to 1
c
c vertical    Switch to 'landscape' or vertical mode 
c ********
 4100 horz=.false.
	go to 1
c
c view      
c ****
4200  call view
	go to 1
c
c wavelabel 
c *********
4300  wavlen = .true.           
      if (alpha(1)(1:nalpha(1)) .eq. 'nm') nanomet = .true.
	go to 1
c
c window      Define and switch between windows (max = 6)
c ******
4400  inx = inum(1) 
      iny = inum(2)
      ink = inum(3)
	go to 1
c
c xlabel      Writes the specified string as the x label, centered 
c ******      under the lower side of a coordinate box made by 'BOX'
c             call:  xlabel 'label'
c
4500	if (boxflg) then
	   lfnt = fnt
	   nxsav = (ixr + ixl)/2
c       modified to work under PostScript
	   nysav = ibot -80
	   idxsav = 1
	   idysav = 0
	   ijstsv = 0
	   if(iszsav .eq. 0) iszsav = 10
	   go to 3000
	endif
	go to 1
c
c ylabel      Writes the specified string as the y label, centered 
c ******      vertically at the left side of a coordinate box made 
c             by 'BOX' - call:  ylabel 'label'
c 
4600	IF (boxflg) then
	   nxsav = ixl - 90
	   nysav = (itop + ibot)/2
	   idxsav = 0
	   idysav = 1
	   ijstsv = 0
	   if(iszsav .eq. 0) iszsav = 10
	   if (inum(1) .ne. 0) nxsav = nxsav - 100
	   if (rtick) then
	      nxsav = ixr + 100
	      if (inum(1) .ne. 0) nxsav = nxsav + 100
	      idxsav = 0
	      idysav = -1
	   endif
	   go to 3000
	ENDIF
	go to 1
c
c semilog	Produce a semi-log plot covering inum(1) decades
c *********	Bottom at 10**inum(2)
 4700	idec = inum(1)
	idec0 = inum(2)
	go to 1
c
c verbose
c *********
 4800	continue
	verbos = .true.
        go to 90
c
c scale       Scale the Postscript output in x and y directtion
c scale   xscal  yscal
c *********
4900	continue
	if (ifl .eq. 2) then
	   xscal = xnum(1)
	   yscal = xnum(2)
	end if
	go to 1
c
c unused
c *********
 5000   continue
	go to 1
c
c unused
c *********
5100    continue
	go to 1

c
c unused
c *********
5200	continue
	go to 1

c
c unused
c *********
5300	continue 
	go to 1 

c
c unused
c *********
5400	continue
	go to 1

c
c unused
c *********
5500	continue
	go to 1

c
c unused
c *********
5600	continue
	go to 1

c
c unused
c *********
5700	continue
	go to 1

c
c unused
c *********
5800	continue
	go to 1
c
c unused
c *********
5900    continue
	go to 1

c
c unused
c *********
6000	continue
	go to 1


c ERROR MESSAGES

9990	write (output,9991)
9991	format(' ERROR -- unit is not open.')
	go to 1

90	write (*,*) ' Command not currently implemented'
	go to 1

9998	write (output, 9999)
9999	format (' Syntax error for last command')
	go to 11

100	continue
c close files
c	common /iounit/ input,output,outfile,stdin,stdout,infile
c	common /iounit/ spool,hlpfil,com,datain,dataot,cardin, stderr
c 	close (spool,  status='keep')
  	close (cardin, status='keep')
  	close (datain, status='keep')
	end

*------------------------------------------------------------------

        subroutine dtime (name)

* makes a nice time string

        character*(*) name
        integer ihr,imin,isec,icsc

        call gettim (ihr,imin,isec,icsc)  ! hh:mm:ss time format
        write (name,11) ihr,imin,isec
11      format (1x,i2,':',i2,':',i2,' ')
        if (ihr  .lt. 10) name(2:2) = '0'
        if (imin .lt. 10) name(5:5) = '0'
        if (isec .lt. 10) name(8:8) = '0'
        return
        end

 

