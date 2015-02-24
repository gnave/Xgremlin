*--------------------------------------------------------------------
* this file is part of Xgremlin / interactive phase correction
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

      subroutine plotph( iact, tr, phz )
*
* phase plotting subroutine (assumes that plotr has been called first
* so global parameters of the plot have been determined).
*
* iact = 0 : initialize global phase data
* iact = 1 : plot the phase of a line spectrum (phase points)
* iact = 2 : plot the phase of a continuum spectrum
*
      integer pskip
      parameter (pskip=4) ! number of pixels to skip in phase plotting

      real pi
      parameter (pi = 3.141592654)
      
      include 'datetc.h'
      include 'infmtn.h'
      include 'plot.h'
      include 'phase.h'
      include 'color.h'

      integer iact
      real tr(*), phz(*)

      integer i, k, ibeg, iend, n, nump, npwid, nplpt
      integer numpts, ninc, navw
      real phw, pho, w, rbin, drbin, deltas, deltaw
      real a1, b1, a2, b2, amin, amax
      double precision wnum

      include 'transform.h'

* initialize global data
      if (iact.eq.0) then                 ! init phase data
         ph_top =  pi
         ph_bot = -pi
         l_phase = .false.
         nphpts = 0
         return
      end if

      call defcol( col_phase )            ! used for phase plotting
      call svpsys( ph_bot, ph_top )       ! set coord system
      call phaxis( ph_bot, ph_top )       ! draw phase axis

      call defcol( col_pink )             ! zero line
      if ( ph_bot.lt.0.0 .and. ph_top.gt.0.0 ) then ! draw phase zero line
         call pgvmove( w_left,  0.0 )
         call pgvline( w_right, 0.0 )
      end if
      call defcol( col_phase )
      call getpwid( npwid )               ! width of plotting window
      ibeg = nump(w_left)                 ! window to be drawn
      iend = nump(w_right)
      numpts = iend - ibeg + 1            ! number of data points
   
      call inhibit                        ! defer drawing

      if (iact.eq.1) then                 ! plot the phase points
         
* first plot the phase polynomial if one was calculated
         if ( ph_std .gt. 0.0 ) then

            nplpt = npwid / pskip         ! number of points to plot            

            if ( numpts .le. nplpt ) then
               ninc = 1
            else
               ninc = numpts / nplpt
            end if

            if ( monodsp .eq. 1) call dashsty( 1 )  ! use dashes on mono displays

            call defcol( col_sdev )       ! draw confidence interval first
            a1 = w_left
            b1 = phz(ibeg) - ph_std
            do i=ibeg+ninc,iend,ninc
               a2 = wnum(i)
               b2 = phz(i) - ph_std
               call pgvcvect( a1, b1, a2, b2 )
               a1 = a2
               b1 = b2
            end do

            a1 = w_left
            b1 = phz(ibeg) + ph_std
            do i=ibeg+ninc,iend,ninc
               a2 = wnum(i)
               b2 = phz(i) + ph_std
               call pgvcvect( a1, b1, a2, b2 )
               a1 = a2
               b1 = b2
            end do

            if ( monodsp .eq. 1) call dashsty( 0 )

            call defcol( col_fit )       ! then the actual fit
            a1 = w_left
            do i=ibeg+ninc,iend,ninc
               a2 = wnum(i)
               call pgvcvect( a1, phz(i-ninc), a2, phz(i) )
               a1 = a2
            end do

         end if

* plot bad phase points first to make sure that they do not
* overplot any good ones

         do i=1,nphpts                    ! plot bad phase points
            if ( ipgood(i) .eq. 0 ) then
               phw = ph_w(i)              ! wave number
               pho = ph_ob(i)
               if (phw.gt.w_left .and. phw.lt.w_right  .and.
     &             pho.gt.ph_bot .and. pho.lt.ph_top ) then
                  call drwphp( 0, phw, ph_ob(i) )
               end if
            end if
         end do

         do i=1,nphpts                    ! plot good phase points
            if ( ipgood(i) .eq. 1 ) then
               phw = ph_w(i)              ! wave number
               pho = ph_ob(i)
               if (phw.gt.w_left .and. phw.lt.w_right  .and.
     &             pho.gt.ph_bot .and. pho.lt.ph_top ) then
                  call drwphp( 1, phw, ph_ob(i) )
               end if
            end if
         end do

      end if

      if ( iact .eq. 2 ) then             ! plot continuous phase

         if ( numpts .le. npwid ) then    ! no binning
         
            nbin = 1
            a1 = wnum(ibeg)               ! plot all data
            do k=ibeg+1, iend
               b1 = wnum(k)
               call pgvcvect(a1,tr(k-1),b1,tr(k) )  ! phase is in tr
               a1 = b1
            end do
 
         else                             ! plot with binning

            drbin  = real(numpts) / real(npwid)
            nbin  = drbin       ! truncate to number of points per bin
            navs  = nbin + 1    ! # of points in short bin
            navw  = nbin + 2    ! # of points in wide bin
            call arminmax( tr(ibeg), nbin, amin, amax ) ! first bin
            call pgpbin( 1, amax, amin )
            i = nbin            ! i = number of points accounted for
            rbin = drbin        ! real bin position

            do k=2, npwid-1     ! as many points as pixels
               rbin = rbin + drbin
               deltas = rbin - real(i + nbin)
               deltaw = rbin - real(i + nbin + 1)
               if ( abs(deltaw) .lt. abs(deltas) ) then ! wide bin
                  call arminmax( tr(ibeg+i-1), navw, amin, amax ) ! bin an extr
                  i = i + nbin + 1
               else
                  call arminmax( tr(ibeg+i-1), navs, amin, amax )
                  i = i + nbin
               end if
               call pgpbin( k, amin, amax )
            end do
            
            call arminmax( tr(ibeg+i-1), numpts-i, amin, amax ) ! last point
            call pgpbin( npwid, amin, amax )
  
         end if

      end if
      
      call update                         ! draw phase data
      call defcol( col_black )

      return
      end

*--------------------------------------------------------------------

      subroutine badph( x1, y1, x2, y2 )
*
* marks the phase points in the box with corners (x1,y1), (x2,y2)
* as bad phase points
*
      include 'phase.h'
      
      integer i
      real wavnum,wmin,wmax

      do i=1,nphpts
         wavnum = ph_w(i)
         wmin = min(x1,x2)
         wmax = max(x1,x2)
         pmin = min(y1,y2)
         pmax = max(y1,y2)
         if ( (wavnum.gt.wmin   .and. wavnum.lt.wmax  ) .and.
     &        (ph_ob(i).gt.pmin .and. ph_ob(i).lt.pmax) ) then
            ipgood(i) = 0
            l_bad = .true.
         end if
      end do

      return
      end

*--------------------------------------------------------------------

      subroutine rdphase( lgood )
*
* read the data from the phase file
*
      include 'datetc.h'
      include 'iounit.h'
      include 'phase.h'

      character chfit*128
      integer length, lenfn
      integer ndum
      real sg, chi, rdum
      logical lgood

      external length

      do i=1,nphmax             ! initialize the phase flag array
         ipgood(i) = 0
      end do

      chfit = fnames(rawin)     ! cobble together phase data file name
      lenfn = length(chfit)
      chfit(lenfn+1:lenfn+1+6) = '.phase'
      open (19,file=chfit,status='old',err=70)
      read (19,*)               ! skip 1st line
      do i=1,nphmax             ! read phase data
         read (19,*,end=60) ndum,ph_pt(i),ph_ob(i),ph_dif(i),
     &                      sg,chi,rdum,ph_w(i)
         ipgood(i) = 1          ! flag the point as 'good'
         nphpts = i
      end do
 60   close (19)

      lgood = .true.
      return

 70   call wrtstr(' Error :  could not open phase data file')
      nphpts = 0
      lgood = .false.
      return

      end
              
*--------------------------------------------------------------------

* Revision history:
* -----------------
* $Log: phase.f,v $
* Revision 1.17  1996/07/15 02:22:41  ulf
* further changed format of .phase file
*
* Revision 1.16  1996/07/15 02:18:28  ulf
* changed format of .phase file (slightly).
*
* Revision 1.15  1996/07/15 00:37:31  ulf
* *** empty log message ***
*
* Revision 1.14  1996/03/17 17:12:00  ulf
* fixed a typo (variable 'navw' was declared twice)
*
* Revision 1.13  1996/03/13  18:02:59  ulf
* modified for dynamic arrays
*
* Revision 1.12  1996/01/29  17:52:40  ulf
* phase point plot function plotted too many points nphmax instead of nphpts.
* this should fix the bogus bad points which occasionally turn up in phase
* plots.
*
* Revision 1.11  1996/01/17  14:13:45  ulf
* change type of wnum from real to double precision
*
* Revision 1.10  1996/01/12  10:22:09  ulf
* fixed a bug in continuous phase function plotting
*
* Revision 1.9  1996/01/12  09:25:44  ulf
* added phase plotting for continuous spectra
*
* Revision 1.8  1996/01/08  08:53:03  ulf
* tweaked phase point plotting a bit
*
* Revision 1.7  1995/11/07  14:55:26  ulf
* add clipping to phase plot
*
* Revision 1.6  1995/10/10  10:45:08  ulf
* use dashed lines for phase fit confidence interval on monochrome displays
*
* Revision 1.5  1995/09/26  20:00:01  ulf
* removed typo: numcc
*
* Revision 1.4  1995/09/26  13:48:16  ulf
* checked with source code analyzer 'spag' from Polyhedron Software:
* removed unneeded includes and fixed a bug in 'rdphase'
*
* Revision 1.3  1995/08/24  19:59:32  ulf
* simplify handling of default plotting colours
*
* Revision 1.2  1995/08/20  04:05:27  ulf
* Simpler name for phase fit file: the file gets the extension '.phase'
*
* Revision 1.1  1995/08/19  18:36:29  ulf
* Initial revision
*
*
