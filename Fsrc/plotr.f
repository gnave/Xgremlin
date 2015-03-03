*-------------------------------------------------------------------
*
* Xgremlin plotting module
*
*--------------------------------------------------------------------
*
* Copyright (C) 1994, 1995, 1996
* Ulf Griesmann, Gaithersburg MD 20878, U.S.A.
*
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the
* Free Software Foundation; either version 2, or (at your option) any
* later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* (see file COPYING) along with this program; if not, write to the
* Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*

*
* $Id: plotr.f,v 1.7 1996/09/11 04:01:43 ulf Exp $
*
	subroutine plotr(iact,r,c,tr,phz)
*
* Central subroutine to plot the contents of the r array (or parts thereof).
* This subroutine must always be called non-interactively ( iact = 0 )
* before any further plotting may be done. 'c' is a complex alias
* for the 'r' array i.e. the r array must be passed twice, in r and in
* c (this is just punishment for having used 'equivalence' before).
* the 'tr' array must be passed for phase plotting.
*
* iact = 0 : non-interactive mode, plotr called from command line
* iact = 1 : interactive mode, called by buttons and action keys
* iact = 2 : plot a wavenumber range, not a point range
* iact = 3 : overplotting. draw into existing coordinate system
*
	include 'datetc.h'      ! contains alpha and consorts
 	include 'infmtn.h'
	include 'iounit.h'
	include 'inparms.h'
	include 'plot.h'
 	include 'phase.h'
	include 'color.h'
	include 'background.h'

	real r(*),tr(*),phz(*)
	complex c(*)              ! must be same pointer as r

	integer iact              
	integer npwid, nphei, navs, navw
	integer k, n, nbeg, nend, numpts, nopp
	integer nump, nphp, nphbeg
	real gamin, gamax, amin, amax, a, b, w
	real wbeg, wend, rbin, drbin, deltas, deltaw, gmarg
	double precision wnum

	include 'transform.h'

* check zoom status
	call chkzoom( l_pzoom )

* set up plotmode and synchronize with nwpp for sanity
	if (lfirst) then 
	   if ( nwpp .eq. 1 ) p_mode = 'n'      ! start in normal mode
	   if ( nwpp .eq. 2 ) p_mode = 'c'      ! start in complex mode
	end if

	if ( nwpp.eq.1 .and. p_mode.ne.'n' ) then
	   p_mode = 'n'
	end if
	if ( nwpp.eq.2 .and. p_mode.eq.'n' ) then
	   p_mode = 'c'
	end if

	if ( iact .eq. 0 .and. l_phase ) then   ! turn off phase plotting
	   l_phase = .false.
	   call phaseplt( 0 )	! turn off phase plotting
	end if

	if ( iact .eq. 3 ) then
	   if (lfirst) return     ! no plot exists to plot over
	   goto 1111		  ! only plot data
	end if

* read current plotter window size in pixels
* Note: the first (0th)  and last pixel of the plotter window are 
* used for the axes; plots go on the middle npwid-2 pixels
	call getpwid( npwid )
	npwid = npwid - 2
	call getphei( nphei )

* number of points in r array
	nopp = nop / nwpp

* convey unit to plotting section
	call setunit( nwos )

* the following section figures out what and how much is to be plotted
	if ( iact .eq. 0 ) then
	   
*  make sure global coordinate system gets calculated
	   if (lfirst) then 
	      if (p_mode.eq.'n') then
		 call arminmax(r(1),nopp,w_gl_min,w_gl_max)
	      else
		 if ( nwpp .eq. 2 ) then
		    call zminmax(c(1),nopp,zrmin,zrmax,zimin,zimax)
		 else  ! make sure it does not crash if nwpp is wrong
		    call zminmax(c(1),nopp/2,zrmin,zrmax,zimin,zimax)
		 end if
		 if (p_mode.eq.'r') then
		    w_gl_max = zrmax
		    w_gl_min = zrmin
		 else if (p_mode.eq.'i') then
		    w_gl_max = zimax
		    w_gl_min = zimin
		 else if (p_mode.eq.'c') then
		    w_gl_max = rnumax(zrmax,zimax)
		    w_gl_min = rnumin(zrmin,zimin)
		 end if
	      end if
	   end if

* check for extra parameters on command line
	   if ( ifx .eq. 0 ) then
	      if (lfirst) then
		 nbeg = 1
		 nend = nopp
	      else
		 nbeg = n_left
		 nend = n_right
	      end if
	   else if ( ifx .eq. 1 ) then
	      nbeg = inum(1)
	      nend = nopp
	   else if ( ifx .eq. 2 ) then
	      nbeg = inum(1)
	      nend = inum(2)
	   else
	      call wrtstr(
     &        ' Error :  wrong number of integer parameters.')
	   end if
	   if ( nbeg .ge. nend ) then
	      call wrtstr(' Error :  wrong command parameters.')
	   end if

	   if (nstrng .gt. 0) then      ! plot all data ?
	      if (alpha(1)(1:3) .eq. 'all') then
		 nbeg = 1
		 nend = nopp
		 gamin = w_gl_min
		 gamax = w_gl_max
              else if (alpha(1)(1:4) .eq. 'data') then
		 l_phase = .false.
		 l_backg = .false.
		 call phaseplt( 0 )     ! turn off phase plotting
	      else if (alpha(1)(1:5) .eq. 'phase') then
		 nbeg = 1               ! plot whole low res spectrum
		 nend = nopp
		 gamin = w_gl_min
		 gamax = w_gl_max
		 call plotph(0,tr,phz)  ! initialize phase plot parameters
		 if ( phapdz ) then
		    l_phase = .true.
		    call phaseplt( 1 )
		 else
		    call rdphase( l_phase )
		    if (l_phase) call phaseplt( 1 )
		 end if
		 l_bad = .false.        ! no buffered bad points yet
	      end if
	   end if

	   numpts = nend - nbeg + 1	   

* always re-scale a non-interactive plot command
	   if (p_mode.eq.'n') then
	      call arminmax( r(nbeg), numpts, gamin, gamax )
	   else
	      if ( nwpp .eq. 2 ) then
		 call zminmax(c(nbeg),numpts,zrmin,zrmax,zimin,zimax)
	      else
		 call zminmax(c(nbeg),numpts/2,zrmin,zrmax,zimin,zimax)
	      end if
	      if (p_mode.eq.'r') then
		 gamax = zrmax
		 gamin = zrmin
	      else if (p_mode.eq.'i') then
		 gamax = zimax
		 gamin = zimin
	      else if (p_mode.eq.'c') then
		 gamax = rnumax(zrmax,zimax)
		 gamin = rnumin(zrmin,zimin)
	      end if
	   end if

	   if ( ifl .eq. 1 ) then
	      gamin = xnum(1)
	   else if ( ifl .eq. 2 ) then
	      gamin = xnum(1)
	      gamax = xnum(2)
	   end if
	   if ( gamin .ge. gamax ) then
	      gamin = gamin - 0.1 * gamin
	      gamax = gamax + 0.1 * gamax
	   end if

	end if

	if (iact .eq. 1) then   ! interactive mode

	   if (lfirst) then     ! plot from command line first
	      call clrplot      ! new frame after resize
	      return
	   end if

	   nbeg = n_left
	   nend = n_right
	   numpts = n_pts
	   if ( l_resc .and. .not.(l_pzoom.eq.1) ) then
	      if (p_mode.eq.'n') then
		 call arminmax( r(nbeg), numpts, gamin, gamax )
	      else
		 if ( nwpp .eq. 2 ) then
		  call zminmax(c(nbeg),numpts,zrmin,zrmax,zimin,zimax)
		 else
		  call zminmax(c(nbeg),numpts/2,zrmin,zrmax,zimin,zimax)
		 end if
		 if (p_mode.eq.'r') then
		    gamax = zrmax
		    gamin = zrmin
		 else if (p_mode.eq.'i') then
		    gamax = zimax
		    gamin = zimin
		 else if (p_mode.eq.'c') then
		    gamax = rnumax(zrmax,zimax)
		    gamin = rnumin(zrmin,zimin)
		 end if
	      end if
	   else if ( l_resc .and. (l_pzoom.eq.1) ) then
	      do k=1,nphpts
		 if ( ph_w(k) .ge. w_left ) goto 201
	      end do
 201	      nphbeg = k
	      do k=1,nphpts
		 if ( ph_w(k) .ge. w_right ) goto 202
	      end do
 202	      nphp = k - nphbeg
	      call arminmax( ph_ob(nphbeg), nphp, ph_bot, ph_top )
	   else
	      gamin = w_bottom  ! do not change y scale
	      gamax = w_top
	   end if

	end if

	if (iact .eq. 2) then  ! wavenumber range mode

	   if (lfirst) then
	      call wrtstr(' Error :  plot  must precede wplot or goto.')
	      return
	   end if
	   
	   if ( ifl .ne. 2 ) then   ! parameters missing
	      call wrtstr(' Error :  wrong or missing parameter(s).' )
	      return
	   end if

	   wbeg = xnum(1)
	   wend = xnum(2)

	   if ( wbeg .ge. wend ) then  ! wrong range
	      call wrtstr(' Error :  w_begin > w_end.' )
	      return
	   end if

	   nbeg = nump(wbeg)
	   nend = nump(wend)
	   numpts = nend - nbeg + 1

	   if ( nend.lt.1 .or. nbeg.gt.nopp ) then
	      call wrtstr(' Error :  range outside current buffer')
	      return
	   end if

	   if ( nbeg.lt.1 .or. nend.gt.nopp ) then  ! keep it simple for now
	      call wrtstr(
     &        ' Warning :  range partially outside current buffer.')
	      if ( nbeg .lt. 1 ) then
		 nbeg = 1
		 nend = numpts
	      else 
		 nbeg = nopp - numpts + 1
		 nend = nopp
	      end if
	   end if
	   
	end if

* check the sanity of all parameters befor plotting
	if ( numpts .gt. nopp ) then
	   numpts = nopp
	end if
	if ( nbeg .lt. 1 ) then
	   nbeg = 1
	   nend = nbeg + numpts - 1
	end if
	if ( nend .gt. nopp ) then
	   nend = nopp
	   nbeg = nend - numpts + 1
	end if
	if ( gamin .eq. gamax ) then
	   gamin = 0.0                ! dumb but safe
	   gamax = 1.0
	end if

* leave a top and bottom margin if one is wanted
        gmarg = abs( gamax - gamin ) * real(margin) / 100.0
        gamax = gamax + gmarg
        gamin = gamin - gmarg
 
* If the number of points is smaller than twice 
* the number of pixels then plot all points else plot as many bins
* of data as there are pixels.

	call mclear( button1 )        ! clear any stored mouse markers
	call mclear( button2 )

	call clrplot                  ! clear the plot
	call firstcol                 ! reset overplotting color index

	wbeg = wnum(nbeg)             ! first wavenumber
	wend = wnum(nend)	      ! last wavenumber
                                      ! set up coord. sys.
	call svcsys( wbeg, wend, gamin, gamax, w_gl_min, w_gl_max ) 
	call sabsref( nrefp )         ! absolute buffer position in file

	call defcol( col_grey )       ! zero line
	if ( gamin .lt. 0.0 .and. gamax .gt. 0.0 ) then ! draw zero line
	   call gvmove( real(wnum(nbeg)), 0.0 )
	   call gvline( real(wnum(nend)), 0.0 )
	end if

	call marklines                ! draw any line markers
	call dlabels                  ! and line labels
	call defcol( col_real )       ! plotting color for real data

* entry point for overplotting	
 1111	continue

        call inhibit                  ! defer further drawing
*
* the following section plots real or complex data in the case that
* no binning is required.
*
	if (numpts .le. npwid) then   ! no binning is required, plot all numpts

	   nbin = 1

* plot all data in r array
	   if (p_mode.eq.'n') then
	      a = wnum(nbeg)          ! keep # of calls to wnum a.s.a.p.
	      do k=nbeg+1, nend	
		 b = wnum(k)
		 call gvcvect(a,r(k-1),b,r(k) )
		 a = b
	      end do
	   end if

*
* Note: if the array contains complex data (nwpp=2) then nopp is the
* number of COMPLEX points in the r array. If the real (imaginary) part
* of the r array is to be plotted, the real (imaginary) parts of
* all nopp points must be plotted. If nwpp=1 and the real (imaginary) part
* of r is to be plotted, only numpts/2 points are plotted.
*

* plot real part of complex data
	   if (p_mode.eq.'r' .or. p_mode.eq.'c') then
	      a = wnum(nbeg)
	      do k=nbeg+1, nend
		 b = wnum(k)
		 call gvcvect(a,real(c(k-1)),b,real(c(k)) )
		 a = b
	      end do
	   end if

* plot imag part of complex data
	   if (p_mode.eq.'i' .or. p_mode.eq.'c') then
	      if (iact.eq.3) then
		 call nextcol
	      else
		 call defcol( col_imag )
	      end if
	      if ( monodsp .eq. 1 ) call dashsty( 1 )  ! dashed lines
	      a = wnum(nbeg)
	      do k=nbeg+1, nend
		 b = wnum(k)
		 call gvcvect(a,aimag(c(k-1)),b,aimag(c(k)) )
		 a = b
	      end do
	      call defcol( col_black )	  ! normal plotting color
	      if ( monodsp .eq. 1 ) call dashsty( 0 )
	   end if

	   call shpix( 0 )                ! no binning

	else                              ! bin the data

	   drbin  = real(numpts) / real(npwid)
	   nbin  = drbin                  ! truncate to number of points per bin
	   navs  = nbin + 1               ! # of points in short bin
	   navw  = nbin + 2               ! # of points in wide bin

* Algorithm:
* ----------
* 1.) calculate the number of points per bin (rbin). This will not be an integer
*     number in most cases. The bins are chosen to be  nbin = int(rbin)
*     wide with nbin+1 wide bins interspersed. The algorithm used to 
*     distribute the wider bins evenly is similar to the algorithm used
*     to draw a straight line on a raster display.
* 2.) The bins overlap by one point.  The first bin is narrow and has to
*     be treated outside the loop to allow for overlapping bins. Bins overlap
*     by their first and last points. If bins were not overlapping the plot
*     would look very grungy.

	   if (p_mode.eq.'n') then                                 ! normal mode

	      call arminmax( r(nbeg), nbin, amin, amax )           ! first bin
	      call gpbin( 1, amax, amin )
	      i = nbin		! i = number of points accounted for
	      rbin = drbin	! real bin position
 	      
	      do k=2, npwid-1	! as many points as pixels
		 rbin = rbin + drbin
		 deltas = rbin - real(i + nbin) 
		 deltaw = rbin - real(i + nbin + 1)
		 if ( abs(deltaw) .lt. abs(deltas) ) then          ! wide bin
		    call arminmax( r(nbeg+i-1), navw, amin, amax ) ! bin an extra point
		    i = i + nbin + 1
		 else
		    call arminmax( r(nbeg+i-1), navs, amin, amax )
		    i = i + nbin
		 end if
		 call gpbin( k, amin, amax )
	      end do

	      call arminmax( r(nbeg+i-1), numpts-i, amin, amax ) ! last point
	      call gpbin( npwid, amin, amax )

	   end if

	   if ( p_mode.eq.'r' .or. p_mode.eq.'c' ) then

	      call zminmax(c(nbeg),nbin,amin,amax,zimin,zimax)
	      call gpbin( 1, amin, amax )
	      i = nbin		! i = number of points accounted for
	      rbin = drbin	! real bin position
	      
	      do k=2, npwid-1	! as many points as pixels
		 rbin = rbin + drbin
		 deltas = rbin - real(i + nbin) 
		 deltaw = rbin - real(i + nbin + 1)
		 if ( abs(deltaw) .lt. abs(deltas) ) then ! a wide bin is better
		    call zminmax(c(nbeg+i-1),navw,amin,amax,zimin,zimax)
		    i = i + nbin + 1
		 else
		    call zminmax(c(nbeg+i-1),navs,amin,amax,zimin,zimax)
		    i = i + nbin
		 end if
		 call gpbin( k, amin, amax )
	      end do

	      call zminmax(c(nbeg+i-1),numpts-i,amin,amax,zimin,zimax)
	      call gpbin( npwid, amin, amax )

	   end if

	   if ( p_mode.eq.'i' .or. p_mode.eq.'c' ) then

	      if (iact.eq.3) then
		 call nextcol
	      else
		 call defcol( col_imag )
	      end if
	      if ( monodsp .eq. 1 ) call dashsty( 1 ) 

	      call zminmax(c(nbeg),nbin,zrmin,zrmax,amin,amax)
	      call gpbin( 1, amin, amax )
	      i = nbin		! i = number of points accounted for
	      rbin = drbin	! real bin position
	      
	      do k=2, npwid-1	! as many points as pixels
		 rbin = rbin + drbin
		 deltas = rbin - real(i + nbin) 
		 deltaw = rbin - real(i + nbin + 1)
		 if ( abs(deltaw) .lt. abs(deltas) ) then ! a wide bin is better
		    call zminmax(c(nbeg+i-1),navw,zrmin,zrmax,amin,amax)
		    i = i + nbin + 1
		 else
		    call zminmax(c(nbeg+i-1),navs,zrmin,zrmax,amin,amax)
		    i = i + nbin
		 end if
		 call gpbin( k, amin, amax )
	      end do

	      call zminmax(c(nbeg+i-1),numpts-i,zrmin,zrmax,amin,amax)
	      call gpbin( npwid, amin, amax )

	      call defcol( col_black )	   ! normal plotting color
	      if ( monodsp .eq. 1 ) call dashsty( 0 )

	   end if

	   call shpix( nbin )

	end if

        call update                         ! now draw everything at once
	call defcol( col_black )

	if ( iact .ne. 3 ) then
	   call bmeter(nopp,nbeg,nend,real(wnum(1)),real(wnum(nopp)))
	   call xaxis( wbeg, wend )         ! and axes
	   call yaxis( gamin, gamax )
	   call shmode( p_mode )            ! display mode
	   call dflush		            ! force out pending X requests

* save parameters of the plot in common block for further plotting
	   lfirst   = .false.               ! some of those are
	   w_left   = wnum(nbeg)            ! needed for phase plot
	   w_right  = wnum(nend)
	   w_top    = gamax - gmarg         ! take into account margins
	   w_bottom = gamin + gmarg
	   n_left   = nbeg
	   n_right  = nend
	   n_pts    = numpts
	   call stofrm(n_left, n_right, w_top, w_bottom) ! store frame in undo-list

	   if ( l_phase ) then	            ! overlay phase plot
	      if ( phapdz ) then
		 nop = nopph                ! adjust number of points
		 delw = delwph
		 call plotph( 2, tr, phz )
		 nop = noppl
		 delw = delwpl
	      else
		 call plotph( 1, tr, phz )
	      end if
	   end if

	   if ( l_backg ) then
	      call plotbg                   ! overlay background plot
	   end if

	end if

	return
	end

* Revision history:
* -----------------
* $Log: plotr.f,v $
* Revision 1.7  1996/09/11 04:01:43  ulf
* fixed a bug: plotting did not work if minimum and maximum of data in the r
* array were identical.
*
* Revision 1.6  1996/07/15 00:37:31  ulf
* *** empty log message ***
*
* Revision 1.5  1996/07/08 02:37:57  ulf
* fixed an error message to be indented by 1 space
*
* Revision 1.4  1996/06/15 03:14:26  ulf
* add subroutine call to store plotted frame in plot mode undo-list
*
* Revision 1.3  1996/03/22 18:40:24  ulf
* add the phase array to the arguments of subroutine plotr because it
* is needed for plotting phase points.
*
* Revision 1.2  1996/03/17  17:16:16  ulf
* fixed a typo (syntax error) in the revision history
*
* Revision 1.1  1996/03/13  18:05:00  ulf
* Initial revision
*
