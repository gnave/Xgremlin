c gremlin\src\stds.for   Ver. 2.2

c  Revision 2.31 06/18/98  change "discarded" screen message
c  Revision 2.30 05/26/98  standards reports weighted correction as well
c  Revision 2.29 02/21/97  standards reports # out of tolerance
c  Revision 2.28 08/04/95  Optronics uses nanometers
c  Revision 2.27 07/13/95  7 d.p.; unc uses noiselev if > 0
c  Revision 2.26 06/11/94  Allow standards to be used locally with findlines
c  Revision 2.25 04/12/94  initial read removed; print changed
c  Revision 2.24 01/20/94  Stds window moves with wavcorr
c  Revision 2.23 06/20/93  Characters equivalenced as well
c  Revision 2.22 08/09/92  Now 1600 stds max; 4 chars saved, too
c  Revision 2.21 06/04/92  Standards added; equivalenced to r, not tr
c  Revision 2.2  89/02/05  

	subroutine stds(r,tr,point,amp,wd,dmp,eps1,eps2,eps3,
     &                  eps4,eps5,nit,nhold,ctag,dent)

c standards 'filename' [<tolerance>] [<devmaxfactor>] [<lines>]
c *********************

	include 'datetc.h'
	include 'infmtn.h'
	include 'altinf.h'
	include 'iounit.h'
	include 'set.h'
	include 'linparms.h'

	integer nit(*), nhold(*)
        real r(*),tr(*),amp(*),wd(*),dmp(*)
	real eps1(*),eps2(*),eps3(*),eps4(*),eps5(*)
        double precision point(*)
        character ctag(*)*4, dent(*)*32

	parameter (maxstd=1600)

	real stdcor(maxstd),del(maxstd)
	real sps(maxstd),spc(maxstd),dp(maxstd)
	real psd
	character*4 chid(maxstd)
	double precision stdsig(maxstd)
	double precision rilaser,rindex,sigma,xnum1,xnum2
	character*80 filnam
	integer acflg, lfn, npl, lines

* check if line list is in memory
	if (uopen(lineio) .eq. 0) then
	   call wrtstr(' Error :  unit lineio not open - aborted.')
	   return
	end if
	if (nol .eq. 0) then
	   call wrtstr(' Error :  line list not in memory.')
	   return
	end if

* initialize tolerances
	tolm = 0.02
	tolp = 0.02
	dmax = 100.0

* set tolerances
	if (uvar0.gt.0.d0 .and. uvar1.gt.0.d0) then
	   tolp = uvar0
	   tolm = uvar1
	   if (ifl .ne. 0) then
	      dmax = xnum(1)
	   end if
	else
	   if (ifl .ge. 1) then
	      tolm = xnum(1)
	      tolp = xnum(1)
	   end if
	   if (ifl .eq. 2) then
	      dmax = xnum(2)
	   end if
	end if

* lines in plot
	lines = 1
	if (ifx .ne. 0) then
	   lines = inum(1)
	end if
	if (lines .gt. 180) lines = 180

c make sure variables are set for getlines
	call disper

c check for air correction; do only if p > 1 torr
	acflg = 0
	if (pspect .gt. 1.0) then
	   rilaser = rindex(pspect,tspect,refwavno,hspect)
	   acflg = 1
	   write(wrtbuf,'(a,f12.4)') 
     &        ' Air correction applied; laser index = ', rilaser
	   call wrtout(output,wrtbuf)
	   write(logfil,*) ' Air correction applied; laser index = ',
     &				rilaser
	endif
	if (wavcorr .ne. 0.0) then
	   write(wrtbuf,2005) wavcorr
	   call wrtstr(wrtbuf)
 2005	   format (' Wavenumber correction applied; wavcorr = ',
     &             1p,e11.3)
	   write(logfil,2005)	wavcorr
	endif

c get stds file; read in to stdsig array
	lfn = nalpha(1)
	filnam = ' '
	filnam = alpha(1)(1:lfn)
      	open (19,file=filnam,access='sequential',
     &		 form='formatted',status='old',err=2300)
	n = 1
	do while (n .le. maxstd)
	   read (19,'(1x,a4,f13.7)',end=2050) chid(n),stdsig(n)
	   n = n + 1
	end do
 2050	n = n - 1
	close(19)

* open the calibration results file for writing
	lfn = index(fnames(lineio),' ') - 1
	filnam = fnames(lineio)
	filnam(lfn+1:lfn+4) = '.cal'
      	open (19,file=filnam,access='sequential',
     &		 form='formatted',status='replace',err=2301)
	write(19,1001) fnames(lineio)
 1001	format(' Calibration of file : ',a)
	write(19,*) '---------------------------------------------'
	write(19,*) ' '

* get to work
	write(wrtbuf,'(1x,i4,a)') n, ' Standards read in'
	call wrtstr(wrtbuf)
	write(19,'(1x,i4,a)') n, ' Standards read in'
	write(wrtbuf,'(a,2f12.4)') ' Using tolerances: ',-tolm,tolp
	call wrtstr(wrtbuf)
	write(19,'(a,2f12.4)') ' Using tolerances: ',-tolm,tolp
	write(wrtbuf,'(a,f8.0,a,f7.5)') 
     &       ' Maximum deviation accepted =', dmax,
     &       ' * Uncertainty; noise=', noiselev
	call wrtstr(wrtbuf)
	write(19,'(a,f8.0,a,f7.5)') 
     &       ' Maximum deviation accepted =', dmax,
     &       ' * Uncertainty; noise=', noiselev
	write(19,*) ' '
	write(wrtbuf,2051)
	call wrtstr(wrtbuf)
	write(logfil,2051)
	write(19,2051)
 2051	format(t8,'Standard',t23,'Observed',t37,'Amp',t44,'Width',
     &         t53,'Unc',t61,'Delta',t72,'Wcorr')

c initialize loop
	l = 0
	sum = 0.0		!sum of correction factors
	suminvwt = 0.0		!sum of inverse weights
	sumwty = 0.0		!sum of weighted cor
	nstrng = 0
	ifl = 2
	nl = 0
	verbos = .false.
	nmissed = 0
	npl = 0

c loop -------------------------------------------------------------

	do i = 1,n
	   if (stdsig(i) .lt. wstart) cycle
	   if (stdsig(i) .gt. wstop) exit
	   xnum1 = stdsig(i)*(1.0d0 - wavcorr) - tolm
	   xnum2 = stdsig(i)*(1.0d0 - wavcorr) + tolp
	   stdcor(i) = 1.0e+30
 2060	   nl = nl + 1
	   if (nl .gt. nol) exit 
	   xparb = point(nl)
	   call ptow
	   if (xparb .lt. xnum1) go to 2060
	   if (xparb .gt. xnum2) then
	      nl = nl - 1
	      nmissed = nmissed + 1
	      cycle
	   endif 

c change point # from getlines into a wavenumber
	   xparb = point(nl)
	   call ptow
	   sigma = xparb 
	   xw = 2.0*delw*wd(nl)
	   unc1 = 0.01
	   if (noiselev .ne. 0.0) unc1 = noiselev
	   unc = unc1*sqrt(xw*resolutn)/amp(nl)
	   
c and correct for air and wavcorr from header
	   if (acflg .eq. 1) then
	      sigma = sigma*rilaser
     &                   / rindex(pspect,tspect,sigma,hspect)
	      sigma = sigma*(1.0d0 + wavcorr)
	   end if

* prepare deviation plot
	   npl = npl + 1
	   sps(npl) = stdsig(i)
	   spc(npl) = sigma

c calculate diff, cor factor; sum for average
	   del(i) = stdsig(i) - sigma
	   if (abs(del(i)) .gt. dmax*unc .and. unc .gt. 0.0) then
	      write (wrtbuf,2111) 
     &              '*DEL',stdsig(i),del(i),amp(nl),xw,unc
	      call wrtstr(wrtbuf)
	      write (logfil,2111) 
     &              '*DEL',stdsig(i),del(i),amp(nl),xw,unc
	      cycle
	   end if

	   stdcor(i) = del(i)/stdsig(i)
	   uncwt     = unc   /stdsig(i)
	   sum = sum + stdcor(i)
	   suminvwt = suminvwt + (1.0/uncwt)**2	  ! sum of (1/unc**2)
	   sumwty = sumwty + stdcor(i)/(uncwt)**2 !sum of weighted cor
	   l = l + 1
	   write (wrtbuf,2111) chid(i),stdsig(i),sigma,amp(nl),xw,
     &			 unc, del(i), stdcor(i)
	   call wrtstr(wrtbuf)
	   write (logfil,2111) chid(i),stdsig(i),sigma,amp(nl),xw,
     &			 unc, del(i), stdcor(i)
	   write (19,2111) chid(i),stdsig(i),sigma,amp(nl),xw,
     &			 unc, del(i), stdcor(i)
 2111	   format(a4,2f14.7,f7.2,f8.4,2f10.7,1p,e12.3)
	end do

c end loop --------------------------------------------------------------

	write(wrtbuf,'(1x,i4,a)') nmissed,
     &       ' standards out of tolerance.'
	call wrtstr(wrtbuf)
	write(19,'(1x,i4,a)') nmissed,
     &       ' standards out of tolerance.'
	write(19,*) ' '
	if (l .ne. 0) then
	   cor = sum/l
	   wtcor = sumwty/suminvwt
	   sum = 0.0
	   do i=1,n
	      if (stdcor(i) .ne. 1.0e+30) then
		 sum = sum + (stdcor(i) - cor)**2
	      endif
	   enddo

	   devmean = sqrt(sum/(l*(l-1)) )
	   write (wrtbuf,2201) cor,devmean
	   call wrtstr(wrtbuf)
	   write (logfil,2201) cor,devmean
	   write (19,2201) cor,devmean
 2201	   format (' Average correction factor :',1Pe12.5,' +-',e11.4)

	   wtdev = sqrt(1.0/suminvwt)
	   write (wrtbuf,2203) wtcor,wtdev
	   call wrtstr(wrtbuf)
	   write (logfil,2203) wtcor,wtdev
	   write (19,2203) wtcor,wtdev
 2203	   format (' Weighted correction factor:',1Pe12.5,' +-',e11.4)

* plot the results for inspection and outlier identification
	   write(19,*) ' '
	   write(19,*) 'Weighted wavenumber correction result:'
	   do i=1,npl
	      dp(i) = sps(i) - spc(i) * (1.d0 + wtcor)
	   end do
	   call standev(dp,npl,psd)
	   call pltstd(19,lines,sps,dp,psd,npl)
	   close(19)
	endif

	return

 2300	call wrtstr(' Error: failed to open standards file.')
	return

 2301	call wrtstr(' Error: failed to open calibration file.')
	return

	end

c -----------------------------------------------------------------------

      subroutine pltstd(luno,nline,x,yd,stdv,n)
*
*---  Create a nice plot of the 'standards' results
*
*     Author: Ulf Griesmann
*
      parameter (maxnlin = 128, maxdot = 1024)
      character line*61,linhdr*61,linbeg*61,linend*61,linblk*61
      integer   n,luno,i,idx,table(0:maxnlin,0:maxdot)
      real      x(*),yd(*),stdv,stdv2

      data
     &linhdr
     &/'              -S          Std - Line        +S'/,
     &linbeg
     &/'+--------------+------------- 0 -------------+--------------+'/,
     &linblk
     &/'|              |              |              |              |'/,
     &linend
     &/'+--------------+--------------+--------------+--------------+'/

*---  calculate index table
      do i=0,nline
	 do k=0,maxdot
	    table(i,k) = 0
	 end do
      end do
      do i=1,n
	 idx = int(.5d0 + nline * (x(i) - x(1)) / (x(n) - x(1)))
	 if (table(idx,0) .le. maxdot) then
	    table(idx,table(idx,0)+1) = i
	    table(idx,0) = table(idx,0) + 1
	 end if
      end do

*---  create plot
      write(luno,'(a)') ' '
      write(luno,10) stdv
 10   format(1x,'Standard deviation  S =',g14.6)
      write(luno,'(a)') ' '
      write(luno,'(18x,a)') linhdr
      write(luno,'(18x,a)') linbeg
      stdv2 = 2. * stdv
      do i=0,nline
        line = linblk
        if (table(i,0) .ne. 0) then
	   do k=1,table(i,0)
	      if (yd(table(i,k)) .gt. stdv2) then
		 idx = 61
		 line(idx:idx) = '>'
	      else if (yd(table(i,k)) .lt. -stdv2) then
		 idx = 1
		 line(idx:idx) = '<'
	      else
		 idx = 1+int(30.0+(15.*yd(table(i,k))/stdv))
		 line(idx:idx) = '*'
	      end if
	      write(luno,'(g17.9,1x,a,g14.6)') 
     &              x(table(i,k)),line,yd(table(i,k))
	      line = linblk
	   end do
	else
	   write(luno,'(18x,a)') line
        end if
      end do
      write(luno,'(18x,a)') linend

      return
      end

c -----------------------------------------------------------------------

c  converts r from ergs/nm to photons/cm-1
c  (relative only)

	subroutine etop(r)

	include 'datetc.h'
	include 'infmtn.h'

	real r(*)

	double precision temp
	integer i

	id(64:80) = ' in photons/cm-1 '
	if (nstrng.eq.1 .and. alpha(2).eq.'nm') then
	   do i = 1,nop
	      xparb = i
	      call ptow
	      temp = xparb / 10000.0  ! wavenumbers are of the order 1e+5
	      r(i) = r(i) / (temp**3)
	   end do
	else
	   do i = 1,nop
	      xparb = i
	      call ptow
	      temp = xparb / 10000.0
	      r(i) = r(i) / temp
	   end do
	end if

	return
	end

c -----------------------------------------------------------------------

      SUBROUTINE standev(v,nv,stdev)
*
* calculate standard deviation of the samples and the
* standard deviation of the mean
*
      REAL v(*),stdev,s
      INTEGER nv,i
      s = 0.d0
      DO i=1,nv
         s = s + v(i) * v(i)
      END DO
      stdev = SQRT(s / (real(nv) - 1.0) )
      RETURN
      END
