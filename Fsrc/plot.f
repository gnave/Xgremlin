*-------------------------------------------------------------------
*
* Xgremlin plotting module
*
*--------------------------------------------------------------------
*
* Copyright (C) 1994, 1995
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
* $Id: plot.f,v 1.80 1996/07/15 00:37:31 ulf Exp $
*
	subroutine gdispatch(ich,shift,ctrl,meta,
     &                       r, tr, phz,
     &                       point, amp, wd, dmp, 
     & 	                     eps1,eps2,eps3,eps4,eps5,
     &                       nit, nhold, ctag, dent)
  
*
* this subroutine is called by the key and mouse event handlers 
* in plotting mode.
*
	include  'datetc.h'
	include  'infmtn.h'
	include  'iounit.h'
	include  'linparms.h'
	include  'set.h'
	include  'plot.h'
	include  'phase.h'

	integer ich,shift,ctrl,meta
	real r(*), tr(*), phz(*)
	real amp(*), wd(*), dmp(*)
	real eps1(*),eps2(*),eps3(*),eps4(*),eps5(*)
	integer nit(*), nhold(*)
	double precision point(*)
	character ctag(*)*4, dent(*)*32

	integer moupts
	parameter (moupts=32)

	character ch,sdent*32
	integer modif
	integer ncentre, nump, n, reftmp, nl, nr
	real factor, wcentre, wrange, wpeak, apeak, w, wt, wb
	double precision wnum, rsum, rmean, rdiff, rpeak
	real ax1, ay1, ax2, ay2
	integer ipoint1, ipoint2, iclick, iclick1, iclick2, lidx, idx
	integer ln, kx(4), insrt, kflag, idxpk
	real yk(4)
	logical ivis
	integer mpbuf(moupts)  ! for buffering 32 mouse click positions
	real    xmbuf(moupts)
 
	equivalence (iclick, iclick1)  

	data ivis/.true./              ! line markers are visible

	include 'transform.h'

* hell breaks loose here
	ch = char(ich)
        modif = shift + ctrl + meta     ! >0 if a modifier was pressed

* check zoom status
	call chkzoom( l_pzoom )

	call dclick( button1, iclick )  ! was button 1 clicked ?
	call dclick( button2, iclick2 ) ! was button 2 clicked ?

*
* Moving about
*
	
* . :  plot to right, do not rescale y axis
        if ( ch.eq.'.'.and. modif.eq.0 ) then
	   if ( iclick .eq. 1 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      n_left = ipoint1
	      n_right = ipoint1 + n_pts - 1
	      call mclear( button1 )
	   else
	      n_left = n_left + (3*n_pts)/4
	      n_right = n_right + (3*n_pts)/4
	   end if
	   l_resc = .false.                        ! do not rescale y
	   call plotr(1,r,r,tr,phz)
           goto 999
        end if

* > :  plot to right, rescale y axis
        if ( ch.eq.'>'.and. shift.eq.1 ) then
	   if ( iclick .eq. 1 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      n_left = ipoint1
	      n_right = ipoint1 + n_pts - 1
	      call mclear( button1 )
	   else
	      n_left = n_left + (3*n_pts)/4
	      n_right = n_right + (3*n_pts)/4
	   end if
	   l_resc = .true.
	   call plotr(1,r,r,tr,phz)
           goto 999
        end if

* ,  :  plot to left, do not rescale y axis
        if ( ch.eq.','.and. modif.eq.0 ) then
	   if ( iclick .eq. 1 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      n_right = ipoint1
	      n_left = ipoint1 - n_pts +1
	      call mclear( button1 )
	   else
	      n_left = n_left - (3*n_pts)/4
	      n_right = n_right - (3*n_pts)/4
	   end if
	   l_resc = .false.
	   call plotr(1,r,r,tr,phz)
           goto 999
        end if

* <  :  plot to left, rescale y axis
        if ( ch.eq.'<'.and. shift.eq.1 ) then
	   if ( iclick .eq. 1 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      n_right = ipoint1
	      n_left = ipoint1 - n_pts +1
	      call mclear( button1 )
	   else
	      n_left = n_left - (3*n_pts)/4
	      n_right = n_right - (3*n_pts)/4
	   end if
	   l_resc = .true.
	   call plotr(1,r,r,tr,phz)
           goto 999
        end if

* z : move zero line to centre of plot
	if (ch.eq.'z' .and. modif.eq.0) then
	   wrange   = abs( w_top - w_bottom )
	   wcentre  = 0.0
	   w_bottom = wcentre - wrange / 2.0
	   w_top    = wcentre + wrange / 2.0
	   l_resc = .false.
	   call plotr(1,r,r,tr,phz)
	   goto 999
	end if
	   
* e  : move zero line to bottom of plot
	if (ch.eq.'e' .and. modif.eq.0) then
	   wrange   = abs( w_top - w_bottom )
	   w_bottom = 0.0
	   w_top    = w_bottom + wrange 
	   l_resc = .false.
	   call plotr(1,r,r,tr,phz)
	   goto 999
	end if

* chr(201) : center in x direction
	if (ich .eq. 201) then
	   if ( iclick .eq. 1 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      n_left  = ipoint1 - n_pts / 2
	      n_right = ipoint1 + n_pts / 2
	      call mclear( button1 )
	      l_resc = .false.
	      call plotr(1,r,r,tr,phz)
	   end if
	   goto 999
	end if

* chr(202) : centre in y direction
	if (ich .eq. 202) then
	   if ( iclick .eq. 1 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      if ( l_pzoom .eq. 1 ) then
		 wrange = abs( ph_top - ph_bot )
		 ph_top = ay1 + wrange / 2.0
		 ph_bot = ay1 - wrange / 2.0		 
	      else
		 wrange   = abs( w_top - w_bottom )
		 w_top    = ay1 + wrange / 2.0
		 w_bottom = ay1 - wrange / 2.0
	      end if
	      call mclear( button1 )
	      l_resc = .false.
	      call plotr(1,r,r,tr,phz)
	   end if
	   goto 999
	end if

* chr(203) : plot between vertical section markers
	if (ich .eq. 203) then
	   if ( iclick .eq. 2 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      call dmouse( button1, 1, ax2, ay2, ipoint2 )
	      n_left  = min(ipoint1, ipoint2)
	      n_right = max(ipoint1, ipoint2)
	      n_pts   = n_right - n_left + 1
	      call mclear( button1 )
	      l_resc = .false.
	      call plotr(1,r,r,tr,phz)
	   end if
	   goto 999
	end if

* chr(204) : plot between horizontal section between markers
	if (ich .eq. 204) then
	   if ( iclick .eq. 2 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      call dmouse( button1, 1, ax2, ay2, ipoint2 )
	      if ( l_pzoom .eq. 1 ) then
		 ph_top = rnumax( ay1, ay2 )
		 ph_bot = rnumin( ay1, ay2 )
	      else
		 w_top    = rnumax( ay1, ay2 )
		 w_bottom = rnumin( ay1, ay2 )
	      end if
	      l_resc   = .false.
	      call mclear( button1 )
	      call plotr(1,r,r,tr,phz)
	   end if
	   goto 999
	end if
	   	   
*
* Zoom around
* 

* x : expand plot horizontally (zoom in)
	if (ch .eq. 'x'.and. .not. meta.eq.1) then
	   if ( ctrl .eq. 1 ) then
	      factor = 0.1
	   else if ( shift .eq. 1 ) then
	      factor = 0.25
	   else
	      factor = 0.5
	   end if
	   if ( iclick .eq. 1 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      ncentre = ipoint1
	      call mclear( button1 )
	   else
	      ncentre = ( n_right + n_left ) / 2
	   end if
	   n_pts   = factor * n_pts
	   n_left  = ncentre - n_pts / 2
	   n_right = ncentre + n_pts / 2
	   l_resc = .false.
	   call plotr(1,r,r,tr,phz)
	   goto 999
	end if

* <meta>x : expand plot horizontally (zoom out)
	if (ch .eq. 'x' .and. meta.eq.1) then
	   if ( ctrl .eq. 1 ) then
	      factor = 10.0
	   else if ( shift .eq. 1 ) then
	      factor = 4.0
	   else
	      factor = 2.0
	   end if
	   if ( iclick .eq. 1 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      ncentre = ipoint1
	      call mclear( button1 )
	   else
	      ncentre = ( n_right + n_left ) / 2
	   end if
	   n_pts   = factor * n_pts
	   n_left  = ncentre - n_pts / 2
	   n_right = ncentre + n_pts / 2
	   l_resc = .false.
	   call plotr(1,r,r,tr,phz)
	   goto 999
	end if
	   	   
* y : expand plot vertically (zoom in) around zero or mouseclick
	if (ch.eq.'y' .and. .not. meta.eq.1) then
	   if ( ctrl .eq. 1 ) then
	      factor = 0.1
	   else if ( shift .eq. 1 ) then
	      factor = 0.25
	   else
	      factor = 0.5
	   end if
	   if ( l_pzoom .eq. 1 ) then
	      wrange = abs( ph_top - ph_bot )
	   else
	      wrange = abs( w_top - w_bottom )
	   end if
	   if ( iclick .eq. 1 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      wcentre = ay1
	      call mclear( button1 )
	   else
	      wcentre  = 0.0          ! zoom in around 0
	   end if
	   if ( l_pzoom .eq. 1 ) then
	      ph_bot = wcentre - factor * wrange * 0.5
	      ph_top = wcentre + factor * wrange * 0.5
	   else
	      if ( p_mode.eq.'c' .or. wcentre.ne.0.0 ) then
		 w_bottom = wcentre - factor * wrange * 0.5
		 w_top    = wcentre + factor * wrange * 0.5
	      else
		 w_bottom = wcentre - factor * wrange * 0.15
		 w_top    = wcentre + factor * wrange * 0.85
	      end if
	   end if
	   l_resc = .false.
	   call plotr(1,r,r,tr,phz)
	   goto 999
	end if
	   
* <meta>y : shrink plot vertically (zoom out)
	if (ch.eq.'y' .and. meta.eq.1) then
	   if ( ctrl .eq. 1 ) then
	      factor = 10.0
	   else if ( shift .eq. 1 ) then
	      factor = 4.0
	   else
	      factor = 2.0
	   end if
	   if ( l_pzoom .eq. 1 ) then
	      wrange = abs( ph_top - ph_bot )
	   else
	      wrange = abs( w_top - w_bottom )
	   end if
	   if ( iclick .eq. 1 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      wcentre = ay1
	      call mclear( button1 )
	   else
	      wcentre  = 0.0
	   end if
	   if ( l_pzoom .eq. 1 ) then
	      ph_bot = wcentre - factor * wrange * 0.5
	      ph_top = wcentre + factor * wrange * 0.5
	   else
	      if ( p_mode.eq.'c' .or. wcentre.ne.0.0 ) then
		 w_bottom = wcentre - factor * wrange * 0.5
		 w_top    = wcentre + factor * wrange * 0.5
	      else
		 w_bottom = wcentre - factor * wrange * 0.15
		 w_top    = wcentre + factor * wrange * 0.85
	      end if
	   end if
	   l_resc = .false.
	   call plotr(1,r,r,tr,phz)
	   goto 999
	end if
	   
* ^ : full scale in y direction
	if (ch.eq.'^' .and. shift.eq.1) then
	   l_resc = .true.
	   call plotr(1,r,r,tr,phz)
	   goto 999
	end if
	   
* P : replot ( and put zeroline at cursor )
	if ( ch.eq.'P' .and. shift.eq.1) then
	   if ( iclick .eq. 1 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      n_left  = ipoint1 - n_pts / 2
	      n_right = ipoint1 + n_pts / 2
	      if ( l_pzoom .eq. 1 ) then
		 ph_top = ph_top - ay1
		 ph_bot = ph_bot - ay1
	      else
		 w_top    = w_top    - ay1
		 w_bottom = w_bottom - ay1
	      end if
	      call mclear( button1 )
	   end if
	   l_resc = .false.
	   call plotr(1,r,r,tr,phz)
	   goto 999
	end if

* U : undo the last plot mode operation if it was not a simple redraw
	if ( ch.eq.'U' .and. shift.eq.1 ) then
	   call recfrm(nl, nr, wt, wb, n)
	   if ( n .eq. 1 ) then   ! last frame was recalled
	      if ( nl.ne.n_left .or. nr.ne.n_right .or.
     &             wt.ne.w_top  .or. wb.ne.w_bottom ) then
		 n_left  = nl
		 n_right = nr
		 n_pts   = n_right - n_left + 1
		 w_top   = wt
		 w_bottom= wb
		 l_resc = .false.
		 call plotr(1,r,r,tr,phz)
	      end if
	   end if
	   goto 1000
	end if

* p : overplot whatever is currently in the r array
	if ( ch.eq.'p' .and. modif.eq.0 ) then
	   call nextcol
	   call plotr(3,r,r,tr,phz)
	   goto 1000
	end if

* ^r : exchange r and tr array and overplot tr array over current plot
*      (i.e. with identical world coordinates)
	if (ch.eq.'r' .and. ctrl.eq.1 .and. 
     &      .not.(l_pzoom.eq.1) ) then
	   call rext(r,tr)  ! exchange r and tr
	   call lineinfo('Warning :  R and TR arrays exchanged.')
	   call nextcol     ! use first overplotting color
	   call plotr(3,r,r,tr,phz)
	   goto 1000
	end if

*
* accessing other data
*

*
* Plot modes
*
	
* n : normal, r contains a real array (the default)
	if ( ch.eq.'n' .and. modif .eq.0 ) then
	   p_mode = 'n'
	   l_resc = .false.
	   call plotr(1,r,r,tr,phz)
	   goto 999
	end if

* r : r contains complex data, plot real part only
	if ( ch.eq.'r' .and. modif .eq.0 ) then
	   p_mode = 'r'
	   l_resc = .true.
	   call plotr(1,r,r,tr,phz)
	   goto 999
	end if

* i : r contains complex data, plot imaginary part only
	if ( ch.eq.'i' .and. modif .eq.0 ) then
	   p_mode = 'i'
	   l_resc = .true.
	   call plotr(1,r,r,tr,phz)
	   goto 999
	end if

* c : r contains complex data, plot real and imaginary part in different colors
	if ( ch.eq.'c' .and. modif .eq.0 ) then
	   p_mode = 'c'
	   l_resc = .true.
	   call plotr(1,r,r,tr,phz)
	   goto 999
	end if

*
* line related stuff
*

* A ( shift a) : activate all lines in the list
        if (ch.eq.'A' .and. shift.eq.1) then
            ifl = 0
            ifx = 0
            call holdat(2,0,r,point,amp,wd,dmp,nit,nhold,ctag)
	    call allactive(1,r,r)
            go to 1000
	 end if

* a, o : mark all lines displayed
	 if ( (ch.eq.'a' .or. ch.eq.'o') .and. modif.eq.0 ) then
           if (nol .gt. 0) then
	      call synclin(point,amp,wd,dmp,dent) 
	      call linevis(1)
	      call plotr(1,r,r,tr,phz)
	   end if
           go to 1000
	end if
 
* ^a : add a line
	if (ch.eq.'a' .and. ctrl.eq.1) then
           if (wdth .eq. 0.0) then
	      call wrtstr(' Error :  width not set (must be in mK)')
              goto 1000
           endif
* get the parameters of the marked line (wavenumber, intensity)
	   if ( iclick .ge. 1 ) then
	      if (nol+iclick .ge. nlins) then
		 call wrtstr(' Error :  internal line buffer full.')
		 goto 1000
	      endif
	      do lidx=1,iclick
		 call dmouse( button1, lidx-1, ax1, ay1, ipoint1 )
		 if ( nbin .gt. 1 ) then ! use peak in the bin
		    call findpeak( ipoint1, wpeak, apeak, r )
		    xnum(1) = wpeak
		    xnum(2) = apeak
		 else
		    xnum(1) = ax1
		    wpeak   = ax1
		    xnum(2) = ay1
		 end if
		 xnum(3) = wdth
		 xnum(4) = damp
		 inum(1) = 0
		 alpha(1)(1:32) = ' '
                 call inslin(ln,r,point, amp, wd, dmp,
     &              eps1,eps2,eps3,eps4,eps5,nit, nhold, ctag, dent)
		 nhold(ln) = nshold
		 ctag(ln)(1:3) = cstag(1:3)
		 call smclear(button1, lidx-1)   ! only clears marker
		 call mline(ln, dble(wpeak), 1,point,amp,wd,dmp,
     &                      nit, nhold, ctag, dent)
		 if (insflg .ne. 0) then
		    xeoa = neoa
		    call addlin(r,point(ln),wd(ln),amp(ln),dmp(ln),
     &                          -xeoa)
		    nit(ln) = 1
		 end if
	      end do
	      call dlabels    ! re-number displayed line markers
	      call linecnt    ! show total number of lines in the buffer
	      goto 999
	   end if
        end if

* v : moVe a line
	if ( ch.eq.'v' .and. modif.eq.0 ) then
	   if ( iclick1.eq.1 .and. iclick2.eq.1 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      call remline( ipoint1, nbin, ln ) ! find out which line it was, zap it
	      call smclear(button1, 0)          ! clear mouse markers
	      call smclear(button2, 0) 
	      ifx = 1
	      call disper
	      inum(1) = ln
	      xnum(3) = 2000.0 * xpara * wd(ln) ! keep old width and damping
	      xnum(4) = (dmp(ln) - 1.0) / 25.0
	      sdent = dent(ln)                  ! keep the id
	      call rmlin(r,point,amp,wd,dmp,
     &                   eps1,eps2,eps3,eps4,eps5,nit,nhold,ctag,dent)
	      call shwmarker( ax1 )             ! show old position
	      call dmouse( button2, 0, ax2, ay2, ipoint2 ) ! set new marker
	      if ( nbin .gt. 1 ) then ! use peak in the bin
		 call findpeak( ipoint2, wpeak, apeak, r )
		 xnum(1) = wpeak
		 xnum(2) = apeak
	      else
		 xnum(1) = ax2
		 wpeak   = ax2
		 xnum(2) = ay2
	      end if
	      inum(1) = 0
	      alpha(1)(1:32) = sdent
	      call inslin(ln,r,point, amp, wd, dmp,
     &           eps1,eps2,eps3,eps4,eps5,nit, nhold, ctag, dent)
	      nhold(ln) = nshold
	      ctag(ln)(1:3) = cstag(1:3)
	      call mline(ln, dble(wpeak), 1,point,amp,wd,dmp,
     &                   nit, nhold, ctag, dent)
	      if (insflg .ne. 0) then
		 xeoa = neoa
		 call addlin (r,point(ln),wd(ln),amp(ln),dmp(ln),-xeoa)
		 nit(ln) = 1
	      end if
	      call dlabels   ! re-number displayed line markers
	   else
	      call wrtstr(
     &        ' Error :  one red and one blue mouse marker must be set')
	   end if
	   goto 999
	end if

*
* Note: in order to remove several marked lines in one go they have to
* be ordered and deleted from the top down.
* In phase mode: mark phase points in box as bad.
*
* ^d : delete a cursor-marked line from the linelist
        if (ch.eq.'d' .and. ctrl.eq.1) then
	   if (l_phase) then
	      if ( iclick .eq. 2 ) then
		 call pdmouse( button1, 0, ax1, ay1, ipoint1 )
		 call pdmouse( button1, 1, ax2, ay2, ipoint2 )
		 call badph( ax1, ay1, ax2, ay2 )          ! mark points bad
		 call plotr(1,r,r,tr,phz)                  ! show 'em
	      end if
	   else
	      if ( iclick.gt.0 .and. iclick.le.moupts ) then
		 do lidx=1,iclick
		    call dmouse( button1, lidx-1, ax1, ay1, ipoint1 )
		    mpbuf(lidx) = ipoint1
		    xmbuf(lidx) = ax1
		    call smclear(button1, lidx-1)         ! only clears marker
		 end do
		 kflag = 2
		 call issort(mpbuf, xmbuf, iclick, kflag)
		 do lidx=iclick,1,-1                      ! remove lines from top down
		    call remline( mpbuf(lidx), nbin, ln ) ! first from plotter list
		    ifx = 1                               ! then from internal list
		    inum(1) = ln
		    call rmlin(r,point,amp,wd,dmp,
     &                   eps1,eps2,eps3,eps4,eps5,nit,nhold,ctag,dent)
		 end do
		 call plotr(1,r,r,tr,phz)
		 if ( iclick.eq.1 ) then
		    write (wrtbuf,481) ln
 481		    format ('Line ',i5,' removed')
		 else
		    write (wrtbuf,482) iclick
 482		    format (i5,' lines removed')		    
		 end if
		 call lineinfo( wrtbuf )
	      end if
	      call linecnt	! show total number of lines in buffer
	   end if
	   goto 999
        endif

* m : print the line data of the line marked with the red cursor
	if (ch.eq.'m' .and. modif.eq.0) then
	   if ( iclick1 .eq. 1 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      call numline( ipoint1, nbin, ln )      ! find out which line it was
	      call mline(ln, 0.0d0, 2,point,amp,wd,dmp,
     &                   nit, nhold, ctag, dent)
	   end if
	   goto 999
	end if
	
* ^s : use last marked line for width, damping etc ...
	if (ch.eq.'s' .and. ctrl.eq.1 ) then
           wdth = 2000. * delw * wd(ln)
           damp = 0.04*(dmp(ln) - 1.0)
           nshold = nhold(ln)
           cstag = ctag(ln)
	   goto 1000
	end if

* ^t : toggle a marked line (number of lines) active / inactive
        if (ch.eq.'t' .and. ctrl.eq.1) then
	   if (iclick .ge. 1) then
	      do lidx=1,iclick
		 call dmouse( button1, lidx-1, ax1, ay1, ipoint1 )
		 call numline( ipoint1, nbin, ln ) ! find out which line it was
		 if ( ln .ne. 0 ) then
		    if (nit(ln) .eq. 0) nit(ln) = -1
		    xeoa = neoa
		    if (nit(ln) .lt. 0) xeoa = -neoa
		    call addlin(r,point(ln),wd(ln),amp(ln),dmp(ln),xeoa)
		    nit(ln) = -nit(ln)
		    if ( nit(ln) .lt. 0 ) then
		       call lineact( ln, 0 ) ! toggle inactive
		    else
		       call lineact( ln, 1 )
		    end if
		 else
		    call wrtstr(' Error :  mouse marker not on line')
		 end if
	      end do
	      call mclear( button1 )
	      call plotr(1,r,r,tr,phz)
	   end if
           goto 999
	end if
 
* V : toggle lines visible / invisible
	if (ch.eq.'V' .and. shift.eq.1) then
	   if ( ivis ) then
	      ivis = .false.
	      call linevis( 0 )
	   else
	      ivis = .true.
	      call linevis( 1 )
	   end if
	   call plotr(1,r,r,tr,phz)
	   goto 1000
	end if

* <meta><ctrl>g : do a getlines 'inactive'
        if (ch.eq.'g' .and. meta.eq.1 .and. ctrl.eq.1) then
           nstrng = 1
           nalpha(1) = 8
           alpha(1)(1:8) = 'inactive'
           call getlin(r,point,amp,wd,dmp,eps1,eps2,eps3,
     &                 eps4,eps5,nit,nhold,ctag,dent)
           nstrng = 0
           if (nol .gt. 0) then
	      call linevis(1)
	      call plotr(1,r,r,tr,phz)
	   end if
	   goto 1000
        end if

* g : calculate the C.G. of lines, print result but do not insert into line list
* <ctrl>g : insert C.G. of line(s) at cursor(s), save range
        if (ch.eq.'g') then
	   if ( iclick .eq. 2 ) then
	      insrt = 0
	      if (ctrl.eq.1) then
		 insrt = 1
	      end if
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      call dmouse( button1, 1, ax2, ay2, ipoint2 )
	      kx(1) = ipoint1
	      yk(1) = ay1
	      kx(2) = ipoint2
	      yk(2) = ay2
	      call cgline(r,kx,yk,wx,ln,insrt, ! insert in line list
     &                    point, amp, wd, dmp,
     &                    eps1,eps2,eps3,eps4,eps5,
     &                    nit, nhold, ctag, dent)
 	      if (insrt.eq.1) then
		 call mline(ln, dble(wx), 1,point,amp,wd,dmp,
     &                      nit, nhold, ctag, dent)
		 call dlabels	               ! renumber and display
	      end if
	   end if
           go to 999
        endif
 
*
* intensity related commands
*

* <meta>a : calculate area
        if (ch.eq.'a'  .and. meta.eq.1) then
	   if ( iclick .eq. 2 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      call dmouse( button1, 1, ax2, ay2, ipoint2 )
	      inum(1) = nump(ax1)
	      inum(2) = nump(ax2)
	      ifx = 2
	      call area(r)
	   end if
           goto 999
        end if
 
*
* Miscellaneous
*

* ^z : set r array between two markers to zero
	if ( ch.eq.'z' .and. ctrl.eq.1 ) then
	   if ( iclick .eq. 2 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      call dmouse( button1, 1, ax2, ay2, ipoint2 )
	      if (ipoint1 .lt. ipoint2) then
		 ipoint1 = nump(ax1)           ! use them for temp storage
		 ipoint2 = nump(ax2)
	      else
		 ipoint2 = nump(ax1)
		 ipoint1 = nump(ax2)
	      end if
	      do i=ipoint1,ipoint2
		 r(i) = 0.0
	      end do
	   end if
	   goto 999
	end if

* l or M+l : display the wavenumber or secondary unit at the set markers
	if ( ch.eq.'l' ) then
	   if ( iclick.gt.0 ) then
	      do lidx=1, iclick
		 call dmouse( button1, lidx-1, ax1, ay1, ipoint1 )
		 call smclear( button1, lidx-1 )
		 xparb = dble(ax1)
		 if ( nwos .eq. 1 ) then  ! transform to wavenumbers
		    call wtos
		 end if
		 if ( meta .eq. 0 ) then
		    call drawlbl( ax1, ay1, real(xparb), 1 )
		 end if
		 if ( meta .eq. 1 ) then
		    call drawlbl( ax1, ay1, real(xparb), 2 )
		 end if
	      end do
	   end if
	   goto 999
	end if

* w : find corresponding point with same intensity on 'other' side of line
*     print width and centroid
* ^w : as 'w' but also insert centroid into internal line list
	if ( ch.eq.'w' .and. meta.eq.0 ) then
	   if (iclick .ge. 1) then
	      insrt = 0
	      if (ctrl.eq.1) then
		 insrt = 1
	      end if
	      do lidx=1, iclick
		 call dmouse( button1, lidx-1, ax1, ay1, ipoint1 )
		 call smclear( button1, lidx-1 )
		 call centroid(ax1, ay1, ipoint1, insrt, r,
     &                         point, amp, wd, dmp,
     &                         eps1,eps2,eps3,eps4,eps5,
     &                         nit, nhold, ctag, dent)
	      end do
	   end if
	   goto 999
	end if

* <meta>w : calculate 'width' from difference of 2 markers, set width
	if ( ch.eq.'w' .and. meta.eq.1 ) then
	   if ( iclick .eq. 2 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      call dmouse( button1, 1, ax2, ay2, ipoint2 )
	      call smclear( button1, 0 )
	      call smclear( button1, 1 )
	      wdth = abs( ax2 - ax1 )
	      if ( nwos .eq. -1 ) then
		 write(wrtbuf,'(a,f12.5,a)')
     &           'width set to ',wdth,' /cm'
	      else
		 write(wrtbuf,'(a,f12.5,a)')
     &           'width set to ',wdth,' nm'
	      end if
	      call wrtstr( wrtbuf )
	   end if
	   goto 999
	end if
	
* # : print 10 points of the r array around the markers
        if (ch.eq.'#' .and. modif.eq.0) then
	   if ( iclick .ge. 1 ) then
	      do lidx=1,iclick
		 call dmouse( button1, lidx-1, ax1, ay1, ipoint1 )
		 call printr( ipoint1, ax1, r )
	      end do
	   end if
           goto 999
        end if

* N : put polynomial through 1-3 markers and normalize to 1.0
	if (ch.eq.'N' .and. shift.eq.1) then
	   if ( iclick .gt. 0 ) then
	      if ( iclick .gt. 3 ) iclick = 3
	      do idx=1, iclick
		 call dmouse( button1, idx-1, ax1, ay1, ipoint1 )
		 kx(idx) = ipoint1
		 yk(idx) = ay1
	      end do
	      call cnorm(r,kx,yk,iclick,1)
	      call mclear( button1 ) ! red markers
	      l_resc = .false.
	      call plotr(1,r,r,tr,phz)
	   end if
           goto 1000
	end if
  
* B : put polynomial through 1-3 markers and subtract from data
	if (ch.eq.'B' .and. shift.eq.1) then
	   if ( iclick .gt. 0 ) then
	      if ( iclick .gt. 3 ) iclick = 3
	      do idx=1, iclick
		 call dmouse( button1, idx-1, ax1, ay1, ipoint1 )
		 kx(idx) = ipoint1
		 yk(idx) = ay1
	      end do
	      call cnorm(r,kx,yk,iclick,-1)
	      call mclear( button1 ) ! red markers
	      l_resc = .false.
	      call plotr(1,r,r,tr,phz)
	   end if
           goto 1000
	end if

* <meta>c : connect 2 points (replace by straight line) in the r array
	if (ch.eq.'c' .and. meta.eq.1) then
	   if ( iclick .ge. 2 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      do i=1,iclick-1
		 call dmouse( button1, i, ax2, ay2, ipoint2 )
		 inum(1) = min(ipoint1, ipoint2)
		 inum(2) = max(ipoint1, ipoint2)
		 ifx = 2
		 call conect(r)
		 ax1 = ax2
		 ay1 = ay2
		 ipoint1 = ipoint2
	      end do
	      call mclear( button1 ) ! red markers
	      l_resc = .false.
	      call plotr(1,r,r,tr,phz)
	   end if
           goto 1000
	end if

* <meta>d : calculate and print distance between two mouse markers
	if (ch.eq.'d' .and. meta.eq.1) then
	   if ( iclick .ge. 2 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      call dmouse( button1, 1, ax2, ay2, ipoint2 )
	      call wrtstr(' ')
	      write(wrtbuf,*) 'Distance in points      : ',
     &                        abs(ipoint2 - ipoint1)
	      call wrtstr(wrtbuf)
	      write(wrtbuf,'(a,f12.7)') ' Distance in wavenumbers : ',
     &        abs(wnum(ipoint2) - wnum(ipoint1))
	      call wrtstr(wrtbuf)
	      call mclear( button1 )
	      l_resc = .false.
	      call plotr(1,r,r,tr,phz)
	   end if
           goto 1000
	end if

* u : cycle secondary unit used for plotting : nm / Hz / GHz
	if ( ch.eq.'u' .and. modif.eq.0 ) then
	   call nxtunit
	   goto 1000
	end if

* @ : read 2k points around set marker
	if ( ch.eq.'@' .and. modif.eq.0 ) then
	   if ( iclick .eq. 1 ) then
	      call dmouse( button1, 0, ax1, ay1, ipoint1 )
	      xnum(1) = ax1
	      ifl = 1
	      reftmp = rdref
	      rdref = 1
	      call read(r,tr)
	      rdref = reftmp
	      call plotr(0,r,r,tr,phz)  ! make sure everything is plotted
	   end if
	   goto 999
	end if

* C : find center of interferogram in r and display
	if ( ch.eq.'C' .and. shift.eq.1 ) then
	   rsum = 0.d0
	   do k=1,nop
	      rsum = rsum + r(k)
	   end do
	   rmean = rsum / nop
	   rpeak = 0.d0
	   idxpk = 0
	   do k=1,nop
	      rdiff = abs(r(k) - rmean)
	      if (rdiff .gt. rpeak) then
		 rpeak = rdiff
		 idxpk = k
	      end if
	   end do
	   n_pts   = 40                    ! display 40 points around center
	   n_left  = idxpk - n_pts / 2
	   n_right = idxpk + n_pts / 2
	   l_resc = .false.
	   call plotr(1,r,r,tr,phz)
	   goto 1000
	end if

*
* Phase correction
*
* <meta>p : toggle phase plot mode for bad point selection

	if ( ch.eq.'p' .and. meta.eq.1 ) then
	   if ( .not. l_phase ) then         ! read in phase data
	      call plotph( 0, tr, phz )
	      l_bad = .false.                ! no bad points yet
	      call rdphase( l_phase )
	      if ( l_phase ) then
		 call phaseplt( 1 )          ! activate button
	      end if
	   else
	      l_phase = .false.
	      call phaseplt( 0 )             ! de-activate button
	      if ( l_bad ) then              ! save any bad points
		 call markbad
	      end if
	   end if
	   call plotr(1,r,r,tr,phz)          ! plot data and phase
	   goto 999
	end if

* <meta>t : save bad points from buffer to bad points file
	if ( ch.eq.'b' .and. meta.eq.1 ) then
	   if (l_bad) then
	      call markbad	             ! write out bad points to file
	   end if
	   goto 1000
	end if

*
* exits / clear button click queues
*
 999	continue
	call mclear( button1 )   ! red markers
	call mclear( button2 )   ! blue markers

 1000	continue
	iclick1 = 0
	iclick2 = 0
	return
	end

*--------------------------------------------------------------------

        subroutine pltaux( npts, x, y, color )
*
* for plotting auxiliary graphs into a plot. Never re-scales.
* 'npts' data points (x,y) are plotted as a line graph.
* x : wave number / length
* y : y value at that point
* all parameters are unaltered at exit
*
* Note: subroutine does no data binning
*
        include 'infmtn.h'
        include 'color.h'
        include 'plot.h'

        integer npts, color, kbeg
        real x(*),y(*)

        integer k
        real a1, a2, b1, b2

        call defcol( color )
        
* clipping in y direction is done in PlotMode.c
* we clip (crudely) here in x direction
        do k=1,npts
           if (x(k).ge.w_left .and. x(k).le.w_right) then
              goto 10
           end if
        end do
        goto 20     ! data not in frame, exit

 10     kbeg = k
        a1 = x(kbeg)
        b1 = y(kbeg)

        do k=kbeg+1, npts                   
           a2 = x(k)
           b2 = y(k)
           if (a2 .gt. w_right) then
              goto 20
           end if
           call gvcvect(a1,b1,a2,b2)
           a1 = a2
           b1 = b2
        end do

 20     call defcol( col_black )

        return
	end

*--------------------------------------------------------------------

	subroutine findpeak( ixr, wpeak, peak, r )
*
* finds the wavenumber at which the intensity in the r array peaks in
* the bin centered at point ixr of the r array
* This subroutine is only called when data were binned for plotting.
*
* ixr   : a point in the r array
* wpeak : wave number of the peak
* peak  : peak value
* r     : the array with data
*
	include 'datetc.h'
	include 'infmtn.h'
	include 'plot.h'

	real r(*)

	integer lnpt, i, n, nump
	real amin, peak, w
	double precision wnum

	include 'transform.h'

	lnpt = ixr - nbin / 2          ! where to start looking for peak
	
	if ( lnpt .lt. 1 ) then          ! make sure values are sane
	   lnpt = 1                      ! should never happen
	end if
	if ( lnpt+nbin .gt. nop/nwpp ) then
	   lnpt = nop/nwpp - nbin
	end if
	
* find maximum and corresponding wavenumber
	call arminmax( r(lnpt), nbin, amin, peak )
	do i=lnpt,lnpt+nbin
	   if ( r(i) .eq. peak ) then   ! this is dodgy ...
	      goto 10
	   end if
	end do
 10	wpeak = wnum(i)

	return
	end

*--------------------------------------------------------------------

	subroutine mline(ln,wpeak,ipmode,point,amp,wd,dmp,
     &                   nit, nhold, ctag, dent)
*
* marks a line at the position of the mouse marker
* ln     : line number
* wpeak  : peak wavelength
* ipmode : flags if line info is to be plotted below plot
* 
	include 'linparms.h'
	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'

	real amp(*), wd(*), dmp(*)
        double precision point(*)
	integer nit(*), nhold(*)
        character ctag(*)*4, dent(*)*32
 
	integer ln, ipmode, n, nump
	real w
	double precision wnum, wpeak

	include 'transform.h'

 	call disper
        xparb = point(ln)
        call ptow
        xw = 2000.0 * xpara * wd(ln)

	if ( ipmode .eq. 2 ) goto 111   ! only print the line info

	call insmarker(1, wpeak, nint(point(ln)), amp(ln), 
     &                 xw, dent(ln))
	if ( ipmode .eq. 0 ) return

 111	bt = (dmp(ln) - 1.0)/25.0
        write(wrtbuf,20) ln, xparb, amp(ln), xw, bt, nit(ln),
     *          nhold(ln), ctag(ln)
 20	format('Line ',i5,':',f11.4,', I:',g12.6,', W:',f7.1,
     *          ', D:',f5.3,', It:',i5,', Hl:',i3,', Tag:',1x,a4)
        call lineinfo(wrtbuf)
 	
	return
	end

*------------------------------------------------------------------

	subroutine printr( ip, w, r )
*
* prints 11 points of the r array around point ip
*
	integer nrange
	parameter ( nrange=11 )

	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'

	real r(*)

	integer i, nb, ne, n, nump
	real w
	double precision wnum

	include 'transform.h'

	nb = ip - nrange / 2 
	ne = ip + nrange / 2

	if ( nb .lt. 1 )   nb = 1
	if ( nb .gt. nop ) ne = nop

	write(wrtbuf, '(1h )' )
	call wrtstr( wrtbuf )
	write(wrtbuf,'(22h Central wavenumber = ,g15.7)') w
	call wrtstr(wrtbuf)
	write(wrtbuf,'(38h point #     wavenumber      intensity)')
	call wrtstr(wrtbuf)
	do i=nb, ne
	   write(wrtbuf,'(1x,i7,5h     ,g15.7,5h     ,g10.4)') 
     &           i,wnum(i),r(i)
	   call wrtstr( wrtbuf )
	end do

	return
	end

*------------------------------------------------------------------
* auxiliary functions
*------------------------------------------------------------------

	real function rnumax( r1, r2 )
*
* returns the max of r1, r2 (have had problems with amax in f2c+gcc)
*
	implicit none
	real r1, r2

	if ( r1 .gt. r2 ) then
	   rnumax = r1
	else
	   rnumax = r2
	end if

	return
	end

*--------------------------------------------------------------------

	real function rnumin( r1, r2 )
*
* returns the min of r1, r2 (have had problems with amax in f2c+gcc)
*
	implicit none
	real r1, r2

	if ( r1 .lt. r2 ) then
	   rnumin = r1
	else
	   rnumin = r2
	end if

	return
	end

*--------------------------------------------------------------------

	real function rderiv( ipt, xx,  npoi, r )
*
* calculate derivative in r array around point ipt using n points
* Algorithm:
* Fit a 3rd order Chebychev polynomial to the data in the r array in
* the vicinity of the point where the derivative is to be estimated.
* ( Fitting a polynomial yields a good estimate for the derivative even
* in the presence of noise. ) The Chebychev coefficients of the derivative
* are then calculated and finally the desired derivative of the r array
* at point 'xx' (where ipt = int(xx+0.5) ).
*
	include 'datetc.h'
	include 'set.h'
	include 'infmtn.h'
	include 'plot.h'
	include 'integrate.h'
	include 'chebychev.h'

	real r(*)

	integer ipt, npoi, i, k, ipts, n, nump, nord, icoef
	real chisq, cder(ncoef), xx
	double precision wnum

	external chepol

	include 'transform.h'

	nord = 3  ! use 3rd order polynomial

	if ( npoi .gt. mxpts/5 ) then
	   call wrtstr(' ')
	   call wrtstr(' Error :  too many points/pixel.')
	   rderiv = 0.0
	   return
	end if
	
	icbeg = ipt - npoi/2
	icend = ipt + npoi/2
	if (icbeg .lt. n_left)  icbeg = n_left
	if (icbeg .gt. n_right) icbeg = n_right
	if (icend .lt. n_left)  icend = n_left
	if (icend .gt. n_right) icend = n_right

	k=1
	do i=icbeg,icend        ! copy over data from r array
	   xint(k) = wnum(i)
	   yint(k) = r(i)
	   k = k+1
	end do
	ipts = k-1

	do i=1, ipts
	   sigma(i) = 1.0
	end do

	bma = 0.5 * ( xint(ipts) - xint(1) )
	bpa = 0.5 * ( xint(ipts) + xint(1) )
	icoef = nord + 1

	call svdlsq(xint,yint,sigma,npts,chcoef,icoef,uu,vv,ww,
     &              chisq,std,svdtol,chepol)
	call cheder(chcoef, cder, icoef )
        rderiv = chebev(cder,icoef,xx)

	return
	end

*--------------------------------------------------------------------

	subroutine centroid(x, y, ipt, insert, r,
     &                      point, amp, wd, dmp,
     &                      eps1,eps2,eps3,eps4,eps5,
     &                      nit, nhold, ctag, dent)
  
*
* calculates and displays the bisection of a line. One of the edges of
* the line must be marked at (x,y,ipt)
* insert = 0 : do not insert into internal line list
* insert = 1 : insert into internal line list
*
	include 'datetc.h'
	include 'infmtn.h'
	include 'iounit.h'
	include 'linparms.h'
	include 'set.h'
	include 'plot.h'
	include 'color.h'

	real r(*)
        real amp(*), wd(*), dmp(*)
        real eps1(*),eps2(*),eps3(*),eps4(*),eps5(*)
        integer nit(*), nhold(*)
        double precision point(*)
        character ctag(*)*4, dent(*)*32
 
	integer ibeg, iend, i, n, nump, ln, ncp, insert
	real deriv, wx, ab, rderiv, x, y, x1, y1, w
	double precision wnum

	external rderiv

	include 'transform.h'

	deriv = rderiv( ipt, x, 5*nbin, r ) * neoa    ! derivative at ax1
	if ( deriv .eq. 0.0 ) return                  ! not on a line ?
	ibeg=ipt

* figure out in which direction to search for 'other' edge
	if ( neoa .eq. 1 ) then	                      ! emission spectrum
	   if ( deriv .gt. 0.0 ) then                 ! we are at left edge
	      do i=ibeg+1,n_right                     ! search right edge
		 if ( r(i) .lt. y ) goto 777
	      end do
	   else
	      do i=ibeg-1,n_left,-1                   ! search left edge
		 if ( r(i) .lt. y ) goto 777
	      end do
	   end if
	else			                      ! absorption spectrum
	   if ( deriv .gt. 0.0 ) then
	      do i=ibeg+1,n_right                     ! search right edge
		 if ( r(i) .gt. y ) goto 777
	      end do
	   else
	      do i=ibeg-1,n_left,-1                   ! search left edge
		 if ( r(i) .gt. y ) goto 777
	      end do
	   end if
	end if

 777	iend = i-1		                      ! calculate (x1,y1)
	y1 = y	      
	x1 = (wnum(iend) - wnum(iend-1)) * 
     &       (y1 - r(iend-1))/(r(iend) - r(iend-1)) +
     &       wnum(iend-1)
	ab  = ( x1 + x ) / 2.0                        ! bisector
	wx  = abs(x1 - x)	                      ! width
	ncp = nump(ab)                                ! point of centroid
	call wrtstr(' ')
	if ( nwos .eq. -1 ) then
	   write(wrtbuf,'(1x,a,f14.6,a,i10,a)') ' peak :    ',ab,
     &           ' /cm  ( ',ncp,' points )'
	   call wrtstr(wrtbuf)
	   write(wrtbuf,'(1x,a,f14.4,a)') ' r(peak) : ',r(ncp),' /cm'
	else
	   write(wrtbuf,'(1x,a,f14.6,a,i10,a)') ' peak :    ',ab,
     &           ' nm  ( ',ncp,' points )'
	   call wrtstr(wrtbuf)
	   write(wrtbuf,'(1x,a,f14.4,a)') ' r(peak) : ',r(ncp),' nm'
	end if
	call wrtstr(wrtbuf)
	if ( nwos .eq. -1 ) then
	   write(wrtbuf,'(1x,a,f14.6,a)')       ' width   : ',wx,' /cm'
	else
	   write(wrtbuf,'(1x,a,f14.6,a)') ' width   : ',wx,' nm'
	end if
	call wrtstr(wrtbuf)

	call defcol( col_imag )                       ! mark width
	call gvmove( x, y )
	if ( x1 .lt. w_left  ) x1 = w_left            ! gvline does not clip
	if ( x1 .gt. w_right ) x1 = w_right
	call gvline( x1, y1 )
	call defcol( col_black )

	if ( insert.eq.0 ) then
	   call auxmark( ab )	
	else
	   xnum(1) = ab                               ! the centroid
	   xnum(2) = r(ncp)                           ! amplitude there
	   xnum(3) = 1000.0 * wx                      ! centroid width in mK
	   xnum(4) = damp
	   inum(1) = 0
	   alpha(1)(1:32) = ' '
	   call inslin(ln,r,point, amp, wd, dmp,
     &        eps1,eps2,eps3,eps4,eps5,nit, nhold, ctag, dent)
	   ctag(ln) = '   P'                          ! P = peak
           call mline(ln,dble(ab),2,point,amp,wd,dmp, ! plot line info
     &                nit, nhold, ctag, dent)
	   call insmarker(1, dble(ab), ncp, r(ncp), 1000.0*wx, 'no id')
	   call dlabels                               ! draw line labels
	end if

	return
	end

*--------------------------------------------------------------------

	subroutine fncpinfo( fn, ncpt )
*
* returns the name of the file attached to 'datain' and the point
* at which a mouse marker is set.
*
* fn   : file name attached to 'datain'
* ncpt : absolute data point in the data file (not relative to buffer)
*
	character fn*(*)
	integer ncpt, nclick
	real x, y

	include 'iounit.h'
	include 'infmtn.h'
	include 'plot.h'

	fn = fnames(datain)
        call dclick( button1, nclick ) 
	if ( nclick .gt. 0 ) then
	   call dmouse( button1, 0, x, y, ncpt )
	   ncpt = ncpt + nrefp
	else
	   ncpt = 0
	end if

	return
	end

*--------------------------------------------------------------------

* Revision history:
* -----------------
* $Log: plot.f,v $
* Revision 1.80  1996/07/15 00:37:31  ulf
* *** empty log message ***
*
* Revision 1.79  1996/07/09 01:47:32  ulf
* fixed subroutine synclin: call 'insmarker' instead of 'mline'
*
* Revision 1.78  1996/07/09 00:59:26  ulf
* fixed several cases where variable 'ln' was mis-spelt 'nl'
*
* Revision 1.77  1996/07/08 01:42:18  ulf
* removed variables 'nord' and 'icoef' from header file 'chebychev.h'
*
* Revision 1.76  1996/07/03 03:51:15  ulf
* call 'svdlsq' in 'fitpoly'
*
* Revision 1.75  1996/06/30 00:47:25  ulf
* replace 'Numerical Recipes' sorting routines with PD routines
*
* Revision 1.74  1996/06/15 20:06:55  ulf
* fixed a stupid one in the new plot mode undo command
*
* Revision 1.73  1996/06/15 03:32:29  ulf
* added plot mode undo command S+u
*
* Revision 1.72  1996/06/11 01:27:40  ulf
* array 'phz' was not declared.
*
* Revision 1.71  1996/03/22 18:19:04  ulf
* pass phase array to subroutine 'plotr'
* return absolute point position for 'fill in' button in phase plot window
*
* Revision 1.70  1996/03/17  17:14:51  ulf
* corrected argument type in call to 'insmarker'
*
* Revision 1.69  1996/03/17  16:56:15  ulf
* fixed the call to 'usvdfit': w-->ww
*
* Revision 1.68  1996/03/13  18:02:59  ulf
* modified for dynamic arrays
*
* Revision 1.67  1996/02/01  15:21:10  ulf
* passed wrong argument to subroutine 'insmarker'. Fixed.
*
* Revision 1.66  1996/01/31  15:10:22  ulf
* fixed the line width that is passed to the 'insmarker' subroutine
* in 'mline'. Should fix the width that is displayed when moving the
* mouse cursor on a line marker.
*
* Revision 1.65  1996/01/31  14:04:33  ulf
* beautify the output of the line info plot mode command
*
* Revision 1.64  1996/01/30  14:36:23  ulf
* removed a type bug in call to 'insmarker'
*
* Revision 1.63  1996/01/30  14:31:23  ulf
* improve error message when line width is not set
*
* Revision 1.62  1996/01/30  14:29:15  ulf
* put the correct width in mK into the plotter line list in the call to
* 'insmarker'.
*
* Revision 1.61  1996/01/29  18:14:51  ulf
* clear mouse markers before plotting anything with subroutine 'plotr'.
* This hopefully gets rid of the annoying bug which prevents mouse based
* zoom commands from working after a 'plot ...' command because some
* 'ghost' mouseclicks seem to be in the marker list.
*
* Revision 1.60  1996/01/29  17:07:47  ulf
* make subroutine 'fncpinfo' return whatever is the contents of  filename(datain)
* even if the unit is not open. This ensures that the subroutine returns
* something meaningful e.g. after a phase plot when the datain unit is
* already closed.
*
* Revision 1.59  1996/01/17  15:07:29  ulf
* fixed a couple of bugs: wnum must be cast to real when argument in subroutine
* call.
*
* Revision 1.58  1996/01/17  14:55:39  ulf
* subroutines 'mline' and 'insmarker' now have a double precision wavenumber argument
*
* Revision 1.57  1996/01/17  14:21:00  ulf
* changed type of wnum from real to double precision
*
* Revision 1.56  1996/01/12  10:19:21  ulf
* forgot to set phase plotting flag l_phase
*
* Revision 1.55  1996/01/12  10:10:42  ulf
* don't read in any phase points if phase function is continuous
*
* Revision 1.54  1996/01/12  07:00:49  ulf
* and add   include 'inparms.h' to subroutine  plotr
*
* Revision 1.53  1996/01/12  06:59:57  ulf
* add subroutine call for continuous phase plots
*
* Revision 1.52  1996/01/10  17:19:56  ulf
* add subroutine 'fncpinfo' for phase form
*
* Revision 1.51  1996/01/08  09:31:35  ulf
* also store the line width when setting a new line marker
*
* Revision 1.50  1996/01/08  08:59:04  ulf
* check that mouse marker was on a line before toggling the line
*
* Revision 1.49  1995/11/24  17:38:02  ulf
* a couple of cosmetic fixes
*
* Revision 1.48  1995/11/24  16:51:04  ulf
* new plot mode command '@' to do a read center at the mouse marker.
*
* Revision 1.47  1995/11/23  10:09:11  ulf
* un-done a typo (strings are delimited with " " ...)
*
* Revision 1.46  1995/11/22  18:51:29  ulf
* changed a bug in the 'plot phase' command (missing parameter initialization).
* added the subroutine calls to print out line info below the plot
* changed interface of subroutine 'insmarker'
*
* Revision 1.45  1995/11/21  17:32:40  ulf
* subroutine 'insmarker' now requires a line intensity argument
*
* Revision 1.44  1995/11/16  04:00:23  ulf
* added display of total number of lines in internal buffer whenever lines
* are added or removed
*
* Revision 1.43  1995/11/12  15:05:51  ulf
* use 'inhibit' and 'update' more sparingly (something seems to be wrong with them).
*
* Revision 1.42  1995/10/31  16:53:15  ulf
* fix a typo
*
* Revision 1.41  1995/10/31  16:51:12  ulf
* in the 'add line' plot command the check for internal line buffer overflow
* was not correct; fixed.
*
* Revision 1.40  1995/10/26  16:25:10  ulf
* added new plot mode command '<meta>w' to set the width used in fitting
* interactively
*
* Revision 1.39  1995/10/22  14:13:02  ulf
* something must have changed - forgotten what....
*
* Revision 1.38  1995/10/15  09:01:30  ulf
* do not call inhibit/update in function 'pltaux' any longer
* because it leads to unpleasant flickering if 'pltaux' is called
* several times.
*
* Revision 1.37  1995/10/14  15:22:37  ulf
* use the new colour numbers in subroutine 'pltaux'
*
* Revision 1.36  1995/10/10  15:05:11  ulf
* change calls to set plotting colors to use  'real-data' and
* 'imag-data' colours which can be set by users.
*
* Revision 1.35  1995/10/10  11:35:38  ulf
* (l_phase .eq. .false.) is not Fortran 77, .eqv. must be used. fixed.
*
* Revision 1.34  1995/10/03  13:23:16  ulf
* add plot mode command 'v' (vernier) to move a line marker
*
* Revision 1.33  1995/09/26  18:52:40  ulf
* fixed a stupid bug
*
* Revision 1.32  1995/09/26  18:01:35  ulf
* fixed N and B plot mode commands (thanks to ftnchek)
*
* Revision 1.31  1995/09/26  13:55:41  ulf
* check with source code analyzer 'spag' from Polyhedron Software:
* removed unnecessary includes and fixed several bugs
*
* Revision 1.30  1995/09/23  15:48:30  ulf
* if the computer uses a monochrome display dashed lines are used to plot
* the imaginary part of a spectrum to make them distinguishable from
* the real part.
*
* Revision 1.29  1995/09/18  22:00:06  ulf
* fix a bug
*
* Revision 1.28  1995/09/18  21:45:46  ulf
* if iact=2 check that plot range is within current buffer at all.
*
* Revision 1.27  1995/09/18  20:59:39  ulf
* fixed a bug in 'goto' and 'wplot' commands
*
* Revision 1.26  1995/09/18  17:26:07  ulf
* use new 'lineact' function
*
* Revision 1.25  1995/09/12  00:32:51  ulf
* zoom-in/out in y direction centers the 0 line when a complex spectrum
* is being displayed.
*
* Revision 1.24  1995/09/07  21:09:00  ulf
* fix bug: the label in secondary coordinates was always printed even if not
* wanted in the 'l' or 'M+l' plot commands.
*
* Revision 1.23  1995/08/24  21:56:24  ulf
* forgotten include file
*
* Revision 1.22  1995/08/24  20:06:31  ulf
* simplified handling of default colors
*
* Revision 1.21  1995/08/21  19:19:16  ulf
* removed some unnecessary checks for bad phase points in buffer.
*
* Revision 1.20  1995/08/20  02:18:19  ulf
* A 'plot' command turns off phase plotting automatically
*
* Revision 1.19  1995/08/19  19:39:49  ulf
* phase plots were initialized unnecessarily often
*
* Revision 1.18  1995/08/19  19:22:16  ulf
* make all changes required for phase plotting
*
* Revision 1.17  1995/08/06  21:15:47  ulf
* add commands 'l' and 'M+l' for labelling of plots, add command 'u' for
* cycling secondary units. Start adding support for phase correction and
* plotting.
*
* Revision 1.16  1995/08/03  02:29:47  ulf
* add plot commands  l  and  M+l  to label individual lines with a
* wavelength or wavenumber label in the plotting window.
*
* Revision 1.15  1995/08/01  03:01:10  ulf
* added the changes needed for phase plotting. Spectrum is plotted dark grey when
* phase plotting is active.
*
* Revision 1.14  1995/07/19  01:00:30  ulf
* move clipping in binning mode out of 'plotr' into function 'gpbin' where
* it belongs.
*
* Revision 1.13  1995/07/16  03:17:25  ulf
* fix complex plotting which was completely broken. Plotting complex data in
* binned mode was not even implemented. When the r array contains complex data
* the program plots in complex mode by default.
*
* Revision 1.12  1995/07/12  03:33:13  ulf
* fix a bug that prevented simultaneous plotting of real and imaginary part
*
* Revision 1.11  1995/07/04  04:18:08  ulf
* change tag character 'C' to 'P' for 'peak' in subroutine 'centroid'
*
* Revision 1.10  1995/07/04  03:54:32  ulf
* rename subroutine  allines --> synclin  and put call to llerase into 'synclin'
*
* Revision 1.9  1995/07/01  14:48:45  ulf
* insert the width in mK in the 'centroid' command rather than in cm^-1.
*
* Revision 1.8  1995/07/01  00:00:24  ulf
* use 'peak' instead of 'centroid' in w commands
*
* Revision 1.7  1995/06/28  21:45:27  ulf
* the plotting widget only clips in vertical direction. The horizontal line
* plotted in subroutine 'centroid' must be clipped before drawing.
*
* Revision 1.6  1995/06/22  17:34:54  ulf
* print value of r array at centroid in 'w' and '<ctrl>w' plot
* commands.
*
* Revision 1.5  1995/06/21  04:21:13  ulf
* bug fixes in subroutine centroid: save the amplitude with line
* parameters as well as the width.
*
* Revision 1.4  1995/06/21  04:02:52  ulf
* include 'parameters.h' where necessary
* new 'g' plot command
* new <ctrl>w plot command, inserts centroid into line list
*
* Revision 1.3  1995/06/19  02:57:16  ulf
* move all subroutines that were adapted from the original Gremlin
* program into a seperate module (plot2.f) to avoid confusion
* over the copyright.
*
* Revision 1.2  1995/06/18  02:03:38  ulf
* new 'w' plot command for calculating bisections
* fixed a bug in centre of gravity command. c.g. is now
* added to the marker list.
*
* Revision 1.1  1995/06/14  04:07:03  ulf
* Initial revision
*
*
