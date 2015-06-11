! \xgremlin\src\hfs.f
!

!
! Routine to fit hyperfine structure patterns.
! Calls:
!
! hfs close hfslev            Closes levels file
! hfs close hfsline           Closes lines file   
! hfs delete <linnum>     - Deletes a line from linelist
! hfs disp <line1> [<line2>]  - prints out parameters of lines between line1 and line2
! hfs fit <line1> [<line2> <line3>] - Fits current line
! hfs get - Gets whole hfs linelist into memory
! hfs hold <line> <parameter>    - Holds parameters at current value
! hfs insert <waveno> <inten>     - Inserts a line at waveno into linelist
! hfs level <lev_value> - lists all line connecting one level
! hfs open hfslev <fname>   - Opens level file <fname> - an ascii file
! hfs open hfsline <fname>  - Opens hfs line file <fname> - a binary file
! hfs plot <line1> [<line2> <line3> ....]
! hfs release <line> <parameter>    - Releases parameters so they can be fitted
! hfs save -  Saves whole linelist to hfsline file
! hfs set <linnum> <parameter> <value>  - Sets <parameter> to <value>, and releases it.
! hfs write <fname>  - Writes all of hfs linelist to ascii file <fname>
!       

      subroutine hfs(r,tr,delw,wstart,nint,ntrans,
     &               ifl,ifx,inum,xnum,alpha,nalpha,nstrng,freespec) 

      include 'hfsline.h'       ! Contains hfs parameters from Phits
      include 'levelfile.h'     ! Contains level info. from Phits
      include 'specfile.h'      ! Contains ident.num_intrpt_pts for display_ftns.f
      include 'fit_draw.h'      ! contains conv_voigt, voigt, total_fit, sinc ... for display_ftns.f
      include 'color.h'         ! contains colours for plotting

      include 'linparms.h'      ! To access lookup table p for integral of Voigt function
  
      
      real      r(*), tr(*)
      double precision  delw, wstart, xnum(20), freespec, uew, totew
      integer   nint, ntrans, ifx, ifl, nstrng, inum(20), nalpha(6)
      character alpha(6)*80

      integer    MAXLINES, MAXPREDICT
      parameter (MAXLINES=10000, MAXPREDICT=50000)
! Rewrite for gfortran                                  
!      structure /hfs_params/     ! Order is important for reading and writing unformatted files.
      type hfs_params
      sequence
         double precision  waveno,snr,inten,fwhm,damp,al,au,bl,bu,
     &          delwaveno,delsnr,delinten,delfwhm,deldamp,
     &          delal,delau,delbl,delbu
         real jl, ju
         logical hwaveno, hsnr, hfwhm, hdamp, hal, hau, hbl, hbu
         integer high,idl,idu,user1    ! Higher level (1=odd, -1 = even) for Phits compatability
                                       ! idl = Phits identifier for lower level
                                       ! idu = Phits identifier for upper level 
         integer user2,user3,user4,user5 ! add 5 integers for dummy storage - make up to 256bytes

         character el*20,eu*20     ! 20 characters for energy level identifier
         character comment*40      ! 40 characters for a comment
       end type
!      end structure

! Rewrite for gfortran
!      record /hfs_params/ line(MAXLINES), bufline(MAXLINES)
      type(hfs_params) line(MAXLINES), bufline(MAXLINES)

! Rewrite for gfortran
!      structure /predictions/
      type predictions
          double precision waveno
          real jl, ju, al, au, bl, bu
          character el*20, eu*20
          integer   high, idl, idu
       end type
!      end structure
      
! Rewrite for gfortran
!      record /predictions/ predict(MAXPREDICT)
      type(predictions) predict(MAXPREDICT)

      double precision  minwaveno, maxwaveno, wncpt,
     &       l_damp, ew, sum_comp, eps, bcoeff, eta     ! For calculating integrated inten.
      real  jdif, jsum
      real  xfit_int(768+32*NLEN2)         ! Same size for xint as total_fit
      integer ierr, i, j, k, parity, no_predict,lenstr,iostatus, thisline
      logical iexist, found,isave
      integer hfslev, hfsline, tmpline, no_lines, hfswrtline, scratch
      character wrtbuf*132,  fname*20, parl*1, paru*1
! Rewrite for gfortran
!      record /level/ all_level       
      type(level) all_level

C     Modified for use with gfortran (GN,6Apr09)
C      INTEGER*2  kWhere, kWtAvg, kCentroid, kDerv, kHFS3Fit, kHgFit ! From global_constants
      INTEGER*2   kHFS3Fit ! From global_constants
C      PARAMETER  (kWhere = 1, kWtAvg = 2, kCentroid = 3, kDerv = 4, ! From global_constants
C     &            kHFS3Fit = 5, kHgFIt = 6,
C     &            hfslev=40,hfsline=41,hfswrtline=42,tmpline=43, scratch=44)       ! file units
      PARAMETER(kHFS3Fit=5,hfslev=40,hfsline=41,
     &          hfswrtline=42,tmpline=43,scratch=44)       ! file units

      common /this_hfsline/  line ! Save line for C routines
 
      save no_lines, /this_hfsline/, /h_fit/, /levels/              ! Save all things that have been changed
      
      if(nalpha(1) .eq. 0) then
         call wrtstr(' Usage:  hfs close hfslev <fname>')
         call wrtstr('         hfs close hfsline <fname>')  
         call wrtstr('         hfs delete <linnum>')
         call wrtstr('         hfs disp <line1>  [<line2>]')
         call wrtstr('         hfs fit <iter>')
         call wrtstr('         hfs get <line1> [<line2> <line3>]')
         call wrtstr('         hfs hold <line> <parameter>')
         call wrtstr('         hfs insert <waveno> <inten>')
         call wrtstr('         hfs level <level value>')
         call wrtstr('         hfs open hfslev <fname>')
         call wrtstr('         hfs open hfsline <fname>')
         call wrtstr('         hfs plot')
         call wrtstr('         hfs release <line> <parameter>')
         call wrtstr('         hfs save')
         call wrtstr('         hfs set <parameter> <value>')
         call wrtstr('         hfs write <fname>')
         return
      endif

      select case (alpha(1)(1:nalpha(1)))

!
! hfs close hfslev            Closes levels file
! hfs close hfsline           Closes lines file   
! *****************
!

      case('close')
         if (alpha(2)(1:nalpha(2)) .eq. 'hfslev') then    ! We want a levels file
            close(hfslev)
         else if (alpha(2)(1:nalpha(2)) .eq. 'hfsline') then 
            close(hfsline)
         else
            write(wrtbuf,'(a,a)') 'Unknown file ',alpha(2)
            call wrtstr(wrtbuf)
         endif

         return

!
! hfs delete <linnum>     - Deletes a line from linelist
! *******************
!

      case ('delete')
         if(ifx .ne. 1) then
            call wrtstr(' Usage: hfs delete <linnum>')
         endif
!
! Bug fix - make sure it doesn't delete any lines that aren't there
!           and gets the number of lines correct
! (GN, March 2015)
!
         if(inum(1)>no_lines) then
              write(wrtbuf, 2005) no_lines
2005          format('Last line is', i6)
              call wrtstr(wrtbuf)
         else
            do j=inum(1),no_lines-1
               line(j) = line(j+1)
            enddo
            line(no_lines)=line(no_lines+1)
            no_lines = no_lines-1
         endif
         return

!
! hfs disp <line1> [<line2>]  - prints out parameters of lines between line1 and line2
! **************************
!

      case ('disp')

!
! Bug fix - make sure it doesn't write out more lines than there are in linelist
!           also use 'hfs disp' to print out whole list
! (GN, March 2015)
!
         if(ifx .lt. 1) then
            minlin = 1
            maxlin = no_lines
         else
            minlin = inum(1)
            if(ifx .ge. 2) maxlin = inum(2)
         endif
         if(maxlin .gt. no_lines) maxlin = no_lines
         if(ifx .eq. 1) then
            i = minlin
            write(wrtbuf,2010) i,line(i)%waveno, line(i)%snr, 
     1            line(i)%fwhm, line(i)%damp,line(i)%el, line(i)%jl, 
     1            line(i)%al, line(i)%bl, line(i)%eu, line(i)%ju,
     1            line(i)%au, line(i)%bu 
            call wrtstr(wrtbuf)
         else 
            do i=minlin,maxlin
               write(wrtbuf,2010) i,line(i)%waveno, line(i)%snr, 
     1               line(i)%fwhm, line(i)%damp, line(i)%el,line(i)%jl, 
     1               line(i)%al, line(i)%bl, line(i)%eu, line(i)%ju, 
     1               line(i)%au,line(i)%bu 
               call wrtstr(wrtbuf)
            enddo
         endif
2010     format(i4,1xf10.4,1x,f7.1,1x,f6.1,1x,f4.2,1x,2(a10,1x,f5.1,
     1          2f7.2,1x))
         return


!
! hfs fit <line1> [<line2> <line3>] [<start_point> <end_point>] - Fits current line
! ************************************************

      case ('fit')
         if(ifx .lt. 1) then
            call wrtstr('Usage:  hfs fit <line1> [<line2> <line3> ] 
     1 [<start_point> <end_point>]')
            return
         endif
         if(ifx .gt. MAX_NUM_STRUCT) then
            write(wrtbuf,'(a,i3,a)') ' Maximum of ',MAX_NUM_STRUCT, 
     1 ' structures can be fitted. '
            call wrtstr(wrtbuf)
            return
         endif

         fit%peak%type = khfs3fit     ! khfs3fit is currently type 5
         fit%num_structures = ifx
         fit%refractive_index = 1.    ! All wavenumbers are measured in vacuum.
         fit%num_params = ifx*8

         ident%spectrum_order =  wstart/freespec +1  ! alias number
         ident%npts_measured = nint/2
         ident%npts_apodized = 0      ! no apodization    ??
         ident%transform_npts = ntrans/2  ! number of points in inteferogram
	 ident%rms_noise = 1          ! noise level (assumed to be 1)
         spec_cons = freespec / ident%transform_npts
         spec_factor = float(nint)/float(ntrans)

         ! fit.p(1) = Odd A
         ! fit.p(2) = odd B
         ! fit.p(3) = even A
         ! fit.p(4) = even B
         ! fit.p(5) = wavenumber
         ! fit.p(6) = intensity of strongest component (SNR)
         ! fit.p(7) = damping parameter (Kielkopf Eta)
         ! fit.p(8) = halfwidth in number of points
         
         ! fit.matrix(1,1) = standard deviation of odd A
         ! fit.matrix(2,2) = standard deviation of odd B etc
         ! rest of diagonal elements in fit.matrix are uncertainties of parameters in fit.p
         
         do i=0,ifx-1
            ! Copy all parameters into fit.p - fit routines want params in cm-1.
            if (line(inum(1+i))%high .eq. 1) then               ! Odd level is higher
               fit%p(1+i*8) = line(inum(1+i))%au/1000.          ! Odd A
               fit%p(2+i*8) = line(inum(1+i))%bu/1000.          ! Odd B
               fit%p(3+i*8) = line(inum(1+i))%al/1000.          ! Even A
               fit%p(4+i*8) = line(inum(1+i))%bl/1000.          ! Even B
               fit%switch(1+i*8) = line(inum(1+i))%hau           ! If Odd A held
               fit%switch(2+i*8) = line(inum(1+i))%hbu           
               fit%switch(3+i*8) = line(inum(1+i))%hal
               fit%switch(4+i*8) = line(inum(1+i))%hbl
               ! Calculate the A and B splitting factor coefficients and relative intensities. 
               call calc_hfs_coeffs(nuclear_spin(1), 
     1              dble(line(inum(1+i))%ju), dble(line(inum(1+i))%jl), 
     1              dble(line(inum(1+i))%high), 
     1              fit%struct(i+1)%num_components, fit%struct(i+1)%cf,
     1              fit%struct(i+1)%rel_intensity)
            else                                                ! Even level is higher
               fit%p(1+i*8) = line(inum(1+i))%al/1000.          ! Odd A
               fit%p(2+i*8) = line(inum(1+i))%bl/1000.          ! Odd B
               fit%p(3+i*8) = line(inum(1+i))%au/1000.          ! Even A
               fit%p(4+i*8) = line(inum(1+i))%bu/1000.          ! Even B
               fit%switch(1+i*8) = line(inum(1+i))%hal           ! If Odd A held
               fit%switch(2+i*8) = line(inum(1+i))%hbl
               fit%switch(3+i*8) = line(inum(1+i))%hau
               fit%switch(4+i*8) = line(inum(1+i))%hbu
               ! Calculate the A and B splitting factor coefficients and relative intensities. 
               call calc_hfs_coeffs(nuclear_spin(1), 
     1              dble(line(inum(1+i))%jl), dble(line(inum(1+i))%ju), 
     1              dble(line(inum(1+i))%high), 
     1              fit%struct(i+1)%num_components, fit%struct(i+1)%cf,
     1              fit%struct(i+1)%rel_intensity)
            endif
            fit%p(5+i*8) = line(inum(1+i))%waveno            ! Wavenumber
            fit%p(6+i*8) = line(inum(1+i))%snr               ! Signal-to-noise ratio
            fit%p(7+i*8) = line(inum(1+i))%damp              ! Damping parameter
            fit%p(8+i*8) = line(inum(1+i))%fwhm/(2000.*delw) ! FWHM in points.
            fit%switch(5+i*8) = line(inum(1+i))%hwaveno
            fit%switch(6+i*8) = line(inum(1+i))%hsnr
            fit%switch(7+i*8) = line(inum(1+i))%hdamp
            fit%switch(8+i*8) = line(inum(1+i))%hfwhm
         enddo


         !
         ! Determine limits of fit
         ! Fit should go from left-most component-5*fwhm to right-most component+5*fwhm
         !
         minwaveno = 500000.      ! Limit for minimum wavenumber (20nm should keep us going for a bit!)
         maxwaveno = 0.           ! Limit for maximum wavenumber
         do j=1,fit%num_structures
            k=8*(j-1)
            do i=1,fit%struct(j)%num_components
               wncpt = fit%struct(j)%cf(1,i)*fit%p(k+1) +             ! Odd A
     1                 fit%struct(j)%cf(2,i)*fit%p(k+2) +             ! Odd B
     1                 fit%struct(j)%cf(3,i)*fit%p(k+3) +             ! Even A
     1                 fit%struct(j)%cf(4,i)*fit%p(k+4) + fit%p(k+5)  ! Even B + c. of g. wavenumber.
               if (minwaveno .gt. wncpt) minwaveno = wncpt
               if (maxwaveno .lt. wncpt) maxwaveno = wncpt
            enddo
         enddo
         minwaveno = minwaveno - 5.*line(inum(1))%fwhm/1000.        ! go 5*fwhm further than first component
         maxwaveno = maxwaveno + 5.*line(inum(ifx))%fwhm/1000.      ! go 5*fwhm further than last component

         !
         ! If we ask for a fitting region, use it
         !
         
         if(ifl .eq. 2) then
            minwaveno =  xnum(1)      ! xnum(1) in cm-1
            maxwaveno =  xnum(2)
         endif

         ! Convert to point numbers
         fit%beg_abspt = dint((minwaveno - wstart ) /delw) + 1
         fit%end_abspt = dint((maxwaveno - wstart ) /delw) 
         fit%npts = abs(fit%end_abspt - fit%beg_abspt) +1

         do i=1,fit%npts
            fit%data_pts(i) = r(i+fit%beg_abspt - 1)
            fit%weight(i) = 1.                         ! Equal weighting for all points (as in Phits)
         enddo
         
         call miniz(delw)

         !
         ! Copy fitted data back to line array
         !

         do i=0,ifx-1
            ! Copy all parameters into fit.p - fit routines want params in cm-1.
            if (line(inum(1+i))%high .eq. 1) then               ! Odd level is higher
               line(inum(1+i))%au = fit%p(1+i*8)*1000.     ! Odd A
               line(inum(1+i))%bu = fit%p(2+i*8)*1000.     ! Odd B
               line(inum(1+i))%al = fit%p(3+i*8)*1000.     ! Even A
               line(inum(1+i))%bl = fit%p(4+i*8)*1000.     ! Even B
               line(inum(1+i))%delau = fit%matrix(1+i*8,1+i*8)*1000.     ! Odd A
               line(inum(1+i))%delbu = fit%matrix(2+i*8,2+i*8)*1000.     ! Odd B
               line(inum(1+i))%delal = fit%matrix(3+i*8,3+i*8)*1000.     ! Even A
               line(inum(1+i))%delbl = fit%matrix(4+i*8,4+i*8)*1000.     ! Even B
               
               ! Calculate the A and B splitting factor coefficients and relative intensities. 
            else                                                ! Even level is higher
               line(inum(1+i))%al =  fit%p(1+i*8)*1000.   ! Odd A
               line(inum(1+i))%bl =  fit%p(2+i*8)*1000.   ! Odd B
               line(inum(1+i))%au =  fit%p(3+i*8)*1000.   ! Even A
               line(inum(1+i))%bu =  fit%p(4+i*8)*1000.   ! Even B
               line(inum(1+i))%delal = fit%matrix(1+i*8,1+i*8)*1000.     ! Odd A
               line(inum(1+i))%delbl = fit%matrix(2+i*8,2+i*8)*1000.     ! Odd B
               line(inum(1+i))%delau = fit%matrix(3+i*8,3+i*8)*1000.     ! Even A
               line(inum(1+i))%delbu = fit%matrix(4+i*8,4+i*8)*1000.     ! Even B
               ! Calculate the A and B splitting factor coefficients and relative intensities. 
            endif
            line(inum(1+i))%waveno =  fit%p(5+i*8)            ! Wavenumber
            line(inum(1+i))%snr    =  fit%p(6+i*8)            ! Signal-to-noise ratio
            line(inum(1+i))%damp   =  fit%p(7+i*8)            ! Damping parameter
            line(inum(1+i))%fwhm   =  fit%p(8+i*8)*2000.*delw ! FWHM in mK
            line(inum(1+i))%delwaveno =  fit%matrix(5+i*8,5+i*8)            ! Wavenumber
            line(inum(1+i))%delsnr    =  fit%matrix(6+i*8,6+i*8)            ! Signal-to-noise ratio
            line(inum(1+i))%deldamp   =  fit%matrix(7+i*8,7+i*8)            ! Damping parameter
            line(inum(1+i))%delfwhm   =  fit%matrix(8+i*8,8+i*8)*2000.*delw ! FWHM in mK

            ! Calculate integrated intensity. Voigt function as in Brault's routine.
            
            ! First calculate damping parameter in Brault units. l_damp = eta*g^2 / (1-eta)

            eps = 0.099
            bcoeff = 1-1/log(2.) - eps
            eta = line(inum(1+i))%damp
            if (eta .le. 0) then
               l_damp = 0
            else
               l_damp = ( (1-eta*bcoeff) - sqrt( (eta*bcoeff-1)**2 
     1           - 4*eps*eta*eta/log(2.)) )/(2*eps*eta)
            endif

            ! Then calculate integrated intensity of strongest component.

            l_damp = l_damp*25. +1.             ! Brault damping runs from 1 to 26.
            j = l_damp
            if (j .ne. 26) then 
               ew = p(j) + (l_damp-j)*(p(j+1)-p(j)) ! factor is in lookup table p
            else 
               ew = p(26)
            end if
            ew = ew * line(inum(1+i))%fwhm * line(inum(1+i))%snr   ! ew for strongest comp. 

            ! Then multiply by ratio of all components to strongest component 
            !      (=1 in fit.struct.rel_intensity)

            sum_comp = 0.
            do j=1, fit%struct(i+1)%num_components
               sum_comp = sum_comp + fit%struct(i+1)%rel_intensity(j)
            enddo
            
            line(inum(1+i))%inten = ew * sum_comp
            
         enddo
         
         ! Now calculate standard uncertainty of integrated intensity. 
         ! Use sqrt (Chi^2 N/(N-M) ), where N=no. points in fit, N-M
         ! is no. degrees of freedom. (Eqn 15.1.6 in Numerical Recipies).
         ! Must convert to points scale by multiplying by resolution.
         ! resolution (not delw) because we want no. of independent points.

         ! Also, work out %age uncertainty for blended profile, and then
         ! multiply by intensity of each line.

         ! Chi^2 is fit.matrix(np1,np1)
!         print*, fit.lpts, fit.err, delw, spec_factor, line(inum(1)).inten
         uew = sqrt(fit%err*float(fit%lpts))*delw*1000./spec_factor
         if (ifx .eq. 1) then        ! one line only
            line(inum(1))%delinten = uew
         else  if(ifx .gt. 1) then  ! more than 1 line
            totew = 0.
            do i=1,ifx
               totew = totew + line(inum(i))%inten
            enddo
            do i=1,ifx
               line(inum(i))%delinten = uew*line(inum(i))%inten/totew
            enddo
         endif

         return

!
! hfs get - Gets whole hfs linelist into memory
! *******
!

      case ('get')
         ! Check if file open

         inquire(hfsline, iostat=ierr, opened = iexist)
         if( ierr .ne. 0 ) then
            call wrtstr('error in opening file')
            return
         endif
         if( .not. iexist) then
            call wrtstr('File not open')
            return
         endif
         
         do i=1,MAXLINES              ! loop over all the lines
            read(hfsline, rec=i, iostat=iostatus) line(i)
            if(iostatus .gt. 0) goto 3010
            if(iostatus .lt. 0) goto 3020
         enddo
         write(wrtbuf,'(a,i4,a)') ' Warning - maximum of ',MAXLINES,
     1   ' read in'
         call wrtstr(wrtbuf) 
3010     no_lines = i-1
         write(wrtbuf,'(i4,a)') no_lines,' lines read from file'
         call wrtstr(wrtbuf)            
         return
3020     call wrtstr('Error in reading lines from file')
         return

!
! hfs hold <line> <parameter>    - Holds parameters at current value
! ***************************
!

      case('hold')
         
         if( ifx .ne. 1) then   ! Are we dealing with more than one hyperfine structure?
            call wrtstr(' Usage: hfs hold <line> <parameter>')
         endif

         select case (alpha(2)(1:nalpha(2)))
         case ('al')
            line(inum(1))%hal = .true.
         case ('bl')
            line(inum(1))%hbl = .true.
         case ('au')
            line(inum(1))%hau = .true.
         case ('bu')
            line(inum(1))%hbu = .true.
         case ('waveno')
            line(inum(1))%hwaveno = .true.
         case ('snr')
            line(inum(1))%hsnr = .true.
         case ('damp')
            line(inum(1))%hdamp = .true.
         case ('fwhm')
            line(inum(1))%hfwhm = .true.
         case  default 
            write(wrtbuf,'(a,a)') 'Unknown parameter',
     1            alpha(2)(1:nalpha(2))
            call wrtstr(wrtbuf)
            call wrtstr('Possible parameters: al, bl, au, bu, 
     1                  waveno, snr, damp, fwhm')
         end select
         
         
         return

!
! hfs insert <waveno> <inten>     - Inserts a line at waveno into linelist
! ***************************
!

      case ('insert')
         if(ifl .ne. 2) then
            call wrtstr(' Usage: hfs insert <waveno> <inten>')
         endif
         if (no_lines+1 .gt. MAXLINES) then
            call wrtstr(' Number of lines exceeds capacity - 
     1 line not inserted')
            return
         endif
         do i=1,no_lines
            bufline(i) = line(i)         ! Copy linelist to buffer
         enddo
         do i=1,no_lines
            if( line(i)%waveno .gt. xnum(1)) exit    ! i set to insert position
         enddo
         line(i)%waveno = xnum(1)
         line(i)%snr = xnum(2)
         line(i)%damp = 0.
         do j=i,no_lines
            line(j+1) = bufline(j)
         enddo
         no_lines = no_lines+1

         ! If line in predicted list, fill in energy level information
         ! Line must match within 0.001 cm-1 (round-off error)

         do k=1,no_predict
            if (abs(predict(k)%waveno-line(i)%waveno) .lt. 0.001) then
               line(i)%el = predict(k)%el
               line(i)%eu = predict(k)%eu
               line(i)%jl = predict(k)%jl
               line(i)%ju = predict(k)%ju
               line(i)%al = predict(k)%al
               line(i)%au = predict(k)%au
               line(i)%bl = predict(k)%bl
               line(i)%bu = predict(k)%bu
               line(i)%high = predict(k)%high
               line(i)%idl = predict(k)%idl
               line(i)%idu = predict(k)%idu
               exit
            endif
         write(wrtbuf,'(a,i3)') ' Line inserted at position ', i
         enddo
         call wrtstr(wrtbuf)
         thisline = i            ! Save position for access from hfs.c
         return
!
! hfs level <lev_value> - lists all line connecting one level
! *********************
!

      case ('level')
         found = .false.
         call wrtstr('Wavenumber   ID(lower)          J_l    A_l    
     1B_l ID(upper)           J_u    A_u    B_u')
         do k=1, no_predict

!           where (predict%eu .eq. alpha(2)(1:nalpha(2)) )
            if(predict(k)%eu(1:nalpha(2)).eq.alpha(2)(1:nalpha(2)))then
               if(predict(k)%high .eq. -1) then     ! Even level higher
                  parl = 'o'
                  paru = 'e'
               else
                  parl = 'e'
                  paru = 'o'
               endif
               write(wrtbuf,4010) predict(k)%waveno,predict(k)%el,parl,
     1               predict(k)%jl, predict(k)%al, predict(k)%bl, 
     1               predict(k)%eu,paru,predict(k)%ju, predict(k)%au,
     1               predict(k)%bu
               call wrtstr(wrtbuf)
4010           format(f12.4," ",a16,a2,f4.1,f7.1,f7.1," ",a16,a2,f5.1,
     1                f7.1,f7.1)
               found = .true.
            
!           elsewhere (predict%el .eq. alpha(2)(1:nalpha(2)) )
            else if ( predict(k)%el(1:nalpha(2)) .eq. 
     1                alpha(2)(1:nalpha(2)) ) then
               if(predict(k)%high .eq. -1) then     ! Even level higher
                  parl = 'o'
                  paru = 'e'
               else
                  parl = 'e'
                  paru = 'o'
               endif
               write(wrtbuf,4010) predict(k)%waveno,predict(k)%el,parl,
     1               predict(k)%jl, predict(k)%al, predict(k)%bl, 
     1               predict(k)%eu,paru,predict(k)%ju, predict(k)%au,
     1               predict(k)%bu
               call wrtstr(wrtbuf)
               found = .true.
            endif
         enddo
!        elsewhere
         if(.not. found) call wrtstr(' Level not found') 
!        endwhere
         return
!
! hfs open hfslev <fname>   - Opens level file <fname> - an ascii file
!                             and read in levels.
! hfs open hfsline <fname>  - Opens hfs line file <fname> - a binary file
! ************************
      case ('open')

         if (nstrng .lt. 3) then
            call wrtstr('Missing filename')
            return
         endif
         if (alpha(2)(1:nalpha(2)) .eq. 'hfslev') then    ! We want a levels file
            
            open (unit=hfslev,file= (alpha(3)(1:nalpha(3))),
     1            status='old',access='sequential', err=5030)
         
            !
            ! Format of levels file is:
            !     1st line : nuclear spin
            !     subsequent lines:
            !     level id ; j value, energy level, parity, lifetime, A-coeff, B-coeff.
            !     (no spaces allowed in fields)
            !     level id is up to 32 characters
            !     parity is integer*4   odd parity is 0, even parity is 1 (!)
            !     rest are real
            !
         
            read(hfslev,*) nuclear_spin(1)                  ! Read in the energy levels
            num_odd_levels(1) = 0
            num_even_levels(1) = 0
            do i=1,MAX_NUM_LEVELS
               read(hfslev,*,end=5020,iostat=ierr) all_level%name(1:13),
     1              all_level%j_value,all_level%energy_level, 
     1              parity,wrtbuf,all_level%a_split,all_level%b_split
               if(parity .eq. 0) then 
                  num_odd_levels(1) = num_odd_levels(1)+1
                  odd_level(num_odd_levels(1),1) = all_level
                  odd_level(num_odd_levels(1),1)%lev_id 
     1                      = num_odd_levels(1)
               else if(parity .eq. 1) then 
                  num_even_levels(1) = num_even_levels(1)+1
                  even_level(num_even_levels(1),1) = all_level
                  even_level(num_even_levels(1),1)%lev_id 
     1                       = num_even_levels(1)
               else 
                  write(wrtbuf,'(a,a)') 'invalid parity on level', 
     1                  all_level%name
                  call wrtstr(wrtbuf)
               endif
               if (ierr .eq. 1) then
                   write(wrtbuf,'(a,a)') 
     1                  'Not enough parameters for level',all_level%name
	           call wrtstr(wrtbuf)
               endif
            enddo
5020        continue
            write(wrtbuf,'(i4,a)') num_odd_levels(1), 
     1             ' odd-parity levels read in'
            call wrtstr(wrtbuf)
            write(wrtbuf,'(i4,a)') num_even_levels(1), 
     1             ' even-parity levels read in'
            call wrtstr(wrtbuf)
            
            ! Fill array of predicted transitions.
            ! This is sorted by odd levels, then even levels.

            k=1
            do i=1,num_odd_levels(1)
               do j=1,num_even_levels(1)
                  jdif = abs(odd_level(i,1)%j_value - 
     1                   even_level(j,1)%j_value)
                  jsum = odd_level(i,1)%j_value + 
     1                   even_level(j,1)%j_value
                  if((jdif .le. 1.) .and. (jsum .gt. 0)) then     ! J Selection rule
                     predict(k)%waveno = odd_level(i,1)%energy_level 
     1               - even_level(j,1)%energy_level
                     if(predict(k)%waveno .lt. 0) then            ! Even level higher
                        predict(k)%waveno = -1.*predict(k)%waveno ! make it positive
                        predict(k)%high = -1
                        predict(k)%el   = odd_level(i,1)%name 
                        predict(k)%eu   = even_level(j,1)%name
                        predict(k)%jl   = odd_level(i,1)%j_value
                        predict(k)%ju   = even_level(j,1)%j_value
                        predict(k)%al   = odd_level(i,1)%a_split
                        predict(k)%au   = even_level(j,1)%a_split
                        predict(k)%bl   = odd_level(i,1)%b_split
                        predict(k)%bu   = even_level(j,1)%b_split
                        predict(k)%idl  = odd_level(i,1)%lev_id
                        predict(k)%idu  = even_level(j,1)%lev_id
                     else                                         ! Odd level higher
                        predict(k)%high = 1
                        predict(k)%eu   = odd_level(i,1)%name 
                        predict(k)%el   = even_level(j,1)%name
                        predict(k)%ju   = odd_level(i,1)%j_value
                        predict(k)%jl   = even_level(j,1)%j_value
                        predict(k)%au   = odd_level(i,1)%a_split
                        predict(k)%al   = even_level(j,1)%a_split
                        predict(k)%bu   = odd_level(i,1)%b_split
                        predict(k)%bl   = even_level(j,1)%b_split
                        predict(k)%idu  = odd_level(i,1)%lev_id
                        predict(k)%idl  = even_level(j,1)%lev_id
                     endif
                     k = k+1
                  endif
               enddo
            enddo
            no_predict = k
            write(wrtbuf,'(i6,a)') no_predict, ' predicted transitions'
            call wrtstr(wrtbuf)
            close(hfslev)
            return

         else if (alpha(2)(1:nalpha(2)) .eq.'hfsline') then      ! We want a hfs linelist file
            
            open (unit=hfsline,file=(alpha(3)(1:nalpha(3))),
     1            access='direct', recl=296,err=5030)     ! Open the hfslin file
            return
         else
            write(wrtbuf,'(a,a)') 'Unknown unit name: ', 
     1            alpha(2)(1:nalpha(2))
            call wrtstr(wrtbuf)
            return
         endif
5030     call wrtstr('File not found')
         return

!
! hfs plot <line1> [<line2> <line3> ....] [<start_point> <end_point>]
! ********

      case ('plot')
         if(ifx .lt. 1) then
            call wrtstr('Usage:  hfs plot <line1> [<line2> <line3> ....]
     1 [<start_point> <end_point>] ')
            return
         endif
         if(ifx .gt. MAX_NUM_STRUCT) then
            write(wrtbuf,'(a,i3,a)') ' Maximum of ',MAX_NUM_STRUCT, 
     1            ' structures can be fitted. '
            call wrtstr(wrtbuf)
            return
         endif

         fit%peak%type = khfs3fit     ! khfs3fit is currently type 5
         fit%num_structures = ifx
         ident%spectrum_order =  wstart/freespec + 1     ! alias number
         if(ident%num_intrpt_pts .eq. 0 ) ident%num_intrpt_pts = 1     ! no interpolation unless requested
         ident%npts_measured = nint/2
         ident%free_spectral_range = freespec
         ident%npts_apodized = 0      ! no apodization    ??
         ident%transform_npts = ntrans/2  ! number of points in inteferogram
         ident%temperature = 20       ! Usual temperature (not important if pressure = 0)
         ident%pressure = 0.          ! Pressure of 0. ensures all wavenumbers are vacuum 

         do i=0,ifx-1
            ! Copy all parameters into fit.p - calc_display_fit wants params in cm-1.
            if (line(inum(1+i))%high .eq. 1) then               ! Odd level is higher
               fit%p(1+i*8) = line(inum(1+i))%au/1000.          ! Odd A
               fit%p(2+i*8) = line(inum(1+i))%bu/1000.          ! Odd B
               fit%p(3+i*8) = line(inum(1+i))%al/1000.          ! Even A
               fit%p(4+i*8) = line(inum(1+i))%bl/1000.          ! Even B
               ! Calculate the A and B splitting factor coefficients and relative intensities. 
               call calc_hfs_coeffs(nuclear_spin(1),
     1              dble(line(inum(1+i))%ju), dble(line(inum(1+i))%jl), 
     1              dble(line(inum(1+i))%high), 
     1              fit%struct(i+1)%num_components, fit%struct(i+1)%cf,
     1              fit%struct(i+1)%rel_intensity)
            else                                                ! Even level is higher
               fit%p(1+i*8) = line(inum(1+i))%al/1000.          ! Odd A
               fit%p(2+i*8) = line(inum(1+i))%bl/1000.          ! Odd B
               fit%p(3+i*8) = line(inum(1+i))%au/1000.          ! Even A
               fit%p(4+i*8) = line(inum(1+i))%bu/1000.          ! Even B
               ! Calculate the A and B splitting factor coefficients and relative intensities. 
               call calc_hfs_coeffs(nuclear_spin(1), 
     1              dble(line(inum(1+i))%jl), dble(line(inum(1+i))%ju), 
     1              dble(line(inum(1+i))%high), 
     1              fit%struct(i+1)%num_components, fit%struct(i+1)%cf,
     1              fit%struct(i+1)%rel_intensity)
            endif
            fit%p(5+i*8) = line(inum(1+i))%waveno            ! Wavenumber
            fit%p(6+i*8) = line(inum(1+i))%snr               ! Signal-to-noise ratio
            fit%p(7+i*8) = line(inum(1+i))%damp              ! Damping parameter
            fit%p(8+i*8) = line(inum(1+i))%fwhm/(2000.*delw) ! FWHM in points.
         enddo

         !
         ! Determine limits of fit
         ! Fit should go from left-most component-5*fwhm to right-most component+5*fwhm
         !
         
         minwaveno = 500000.      ! Limit for minimum wavenumber (20nm should keep us going for a bit!)
         maxwaveno = 0.           ! Limit for maximum wavenumber

         !
         ! If we ask for a fitting region, use it
         !
         
         if(ifl .eq. 2) then
            minwaveno =  xnum(1)      ! xnum(1) in cm-1
            maxwaveno =  xnum(2)
         endif

         do j=1,fit%num_structures
            k=8*(j-1)
            do i=1,fit%struct(j)%num_components
               wncpt = fit%struct(j)%cf(1,i)*fit%p(k+1) +             ! Odd A
     1                 fit%struct(j)%cf(2,i)*fit%p(k+2) +             ! Odd B
     1                 fit%struct(j)%cf(3,i)*fit%p(k+3) +             ! Even A
     1                 fit%struct(j)%cf(4,i)*fit%p(k+4) + fit%p(k+5)  ! Even B + c. of g. wavenumber.
               if (minwaveno .gt. wncpt) minwaveno = wncpt
               if (maxwaveno .lt. wncpt) maxwaveno = wncpt
            enddo
         enddo
         minwaveno = minwaveno - 20.*line(inum(1))%fwhm/2000.        ! go 5*fwhm further than first component
         maxwaveno = maxwaveno + 20.*line(inum(ifx))%fwhm/2000.      ! go 5*fwhm further than last component

         ! Convert to point numbers
         fit%beg_abspt = dint((minwaveno - wstart) /delw) + 1
         fit%end_abspt = dint((maxwaveno - wstart) /delw)
         fit%npts_intrpt = abs(fit%end_abspt - 
     1                  fit%beg_abspt)*ident%num_intrpt_pts +1
   
         call display_inst_ftn            ! Calculate the instrument profile
         call display_voigt               ! Calculate the Voigt function
         call calc_display_fit            ! Calculate the whole profile - output in total_fit(768+32*NLEN2)

! case - ordinary plot

         do i=1,fit%npts_intrpt
            xfit_int(i) = (real((i-1))/real(ident%num_intrpt_pts)
     1                    + fit%beg_abspt -1)*delw + wstart
         enddo

         call pltaux(fit%npts_intrpt, xfit_int, total_fit, col_red)

! case - plot components

         if( (nalpha(2) .ne. 0) .and. (alpha(2)(1:nalpha(2)) 
     1          .eq. 'comp')) then  !plot components
            if( (nalpha(3).ne.0) .and. (alpha(3)(1:nalpha(3))
     1          .eq.'save')) then
               isave = .true.
               fname = alpha(4)(1:nalpha(4))
               fname(nalpha(4)+1:nalpha(4)+5)='.comp'
               open (unit=scratch,file=fname,status='new',
     1               access='sequential')
            else
               isave = .false.
            endif
            call calculate_comp(wstart,delw,isave,scratch)
            if(isave) then
               close (scratch)
               open (unit=scratch,file=(alpha(4)(1:nalpha(4))),
     1               status='new',access='sequential')
               write(scratch,'(f10.4,1x, e11.3)')(xfit_int(i),
     1               total_fit(i),i=1,fit%npts_intrpt)
               close(scratch)
            endif
         endif

         if( (nalpha(2) .ne. 0) .and. (alpha(2)(1:nalpha(2)) 
     1        .eq. 'save')) then  ! save to data file
            open (unit=scratch,file= (alpha(3)(1:nalpha(3))),
     1            status='new',access='sequential')
            write(scratch, '(f10.4,e10.3)') (xfit_int(i), total_fit(i), 
     1            i=1,fit%npts_intrpt)
            close (scratch)
         endif
         return
! case - plot residuals


!
! hfs release <line> <parameter>    - Releases parameters so they can be fitted
! ******************************
!

      case('release')
         
         if( ifx .ne. 1) then   ! Are we dealing with more than one hyperfine structure?
            call wrtstr(' Usage: hfs release <line> <parameter>')
         endif

         select case (alpha(2)(1:nalpha(2)))
         case ('al')
            line(inum(1))%hal = .false.
         case ('bl')
            line(inum(1))%hbl = .false.
         case ('au')
            line(inum(1))%hau = .false.
         case ('bu')
            line(inum(1))%hbu = .false.
         case ('waveno')
            line(inum(1))%hwaveno = .false.
         case ('snr')
            line(inum(1))%hsnr = .false.
         case ('damp')
            line(inum(1))%hdamp = .false.
         case ('fwhm')
            line(inum(1))%hfwhm = .false.
         case  default 
            write(wrtbuf,'(a,a)') 'Unknown parameter',
     1            alpha(2)(1:nalpha(2))
            call wrtstr(wrtbuf)
            call wrtstr('Possible parameters: al, bl, au, bu, waveno, 
     1                  snr, damp, fwhm')
         end select
         
         
         return


!
! hfs save -  Saves whole linelist to hfsline file
! ********
!

      case ('save')
         ! Check if file open

         inquire(hfsline,iostat=ierr, opened = iexist, name=fname )
         lenstr = len_trim(fname)             ! Remove trailing blanks from name

         if(.not. iexist)  then
            call wrtstr('hfsline file not open')
            return

         else if (iexist) then   ! Make a backup file if it exists already
            do i=1,MAXLINES                  ! read data into temporary buffer
               read(hfsline,rec=i,iostat=iostatus ) bufline(i)
               if(iostatus .gt. 0) goto 6010
               if(iostatus .lt. 0) goto 6020
            enddo
            if (i .eq. MAXLINES) then
               write(wrtbuf, '(a,i4,a)') 'Maximum of ',MAXLINES,' 
     1               saved in backup file'
            endif
6010        wrtbuf = fname(1:lenstr)//'.save'
            open(unit=tmpline,file=wrtbuf,access='direct',recl=296,
     1           action='write') ! open backup file
            do j=1,i-1                       ! write data to backup file
               write(tmpline,rec=i,err=6030) bufline(j)
            enddo
            close(tmpline)                   ! close backup file
            close(hfsline)                  
            open (unit=hfsline,file=fname,access='direct', !&
     1           recl=296,err=6040)                        ! re-open the hfslin file
         endif
         
         do i=1,no_lines              ! loop over all the lines
            write(hfsline,rec=i,err=6030) line(i)
         enddo
         write(wrtbuf,'(i4,a)') no_lines,' saved to file'
         call wrtstr(wrtbuf)
         if(no_lines .gt. MAXLINES) then
            write(wrtbuf,'(a,i4,a)') 'Warning - maximum of ',MAXLINES,
     1            ' saved.'
            call wrtstr(wrtbuf) 
         endif
         continue
            
         return
 6020    call wrtstr('File not open? No lines saved')
         return
 6030    call wrtstr('Error on write - Full disk?')
         return
 6040    call wrtstr('Cannot open hfsline file')
         return

!
! hfs set <linnum> <parameter> <value>  - Sets <parameter> to <value>, and releases it so it can be fitted.
! ************************************
!            

      case('set')
         
         if( ifx .ne. 1) then 
            call wrtstr(' Usage: hfs set <linnum> <parameter> <value>')
            call wrtstr('        hfs set interp <interpolation>')
            return
         endif


         select case (alpha(2)(1:nalpha(2)))

         case ('al')
            line(inum(1))%al = xnum(1)     ! Lower A
            line(inum(1))%hal = .false.     ! Lower A
         case ('bl')
            line(inum(1))%bl = xnum(1)     ! Lower B
            line(inum(1))%hbl = .false.     ! Lower B
         case ('au')                   
            line(inum(1))%au = xnum(1)     ! Upper A
            line(inum(1))%hau = .false.     ! Upper A
         case ('bu')                   
            line(inum(1))%bu = xnum(1)     ! Upper B
            line(inum(1))%hbu = .false.     ! Upper B
         case('waveno')                
            line(inum(1))%waveno = xnum(1) ! Centroid of line
            line(inum(1))%hwaveno = .false. ! Centroid of line
         case ('snr')                
            line(inum(1))%snr = xnum(1)    ! Intensity of strongest component
            line(inum(1))%hsnr = .false.    ! Intensity of strongest component
         case ('damp')                 
            line(inum(1))%damp = xnum(1)    ! Damping parameter 
            line(inum(1))%hdamp = .false.   ! Damping parameter 
         case ('fwhm')                
            line(inum(1))%fwhm = xnum(1)    ! Halfwidth of line in mK
            line(inum(1))%hfwhm = .false.   ! Halfwidth of line in mK
         case ('el')
            line(inum(1))%el = alpha(3)(1:nalpha(3)) ! Lower energy level
            do i=1, max(num_odd_levels(1), num_even_levels(1))
               if ( odd_level(i,1)%name(1:nalpha(3)) .eq. 
     1               alpha(3)(1:nalpha(3))) then               ! Lower level is odd
                  line(inum(1))%idl = odd_level(i,1)%lev_id
                  line(inum(1))%jl = odd_level(i,1)%j_value
                  line(inum(1))%high = -1
                  return
               else if ( even_level(i,1)%name(1:nalpha(3)) .eq. 
     1                   alpha(3)(1:nalpha(3))) then            ! Lower level is even
                  line(inum(1))%idl = even_level(i,1)%lev_id
                  line(inum(1))%jl = even_level(i,1)%j_value
                  line(inum(1))%high = 1
                  return
               endif
            enddo
            call wrtstr(' level not found in list of energy levels ')    
            return
         case ('eu')
            line(inum(1))%eu = alpha(3)(1:nalpha(3))               ! Upper energy level
            do i=1, max(num_odd_levels(1), num_even_levels(1))
               if ( odd_level(i,1)%name(1:nalpha(3)) .eq. 
     1              alpha(3)(1:nalpha(3))) then                    ! Upper level is odd
                  line(inum(1))%idu = odd_level(i,1)%lev_id
                  line(inum(1))%ju = odd_level(i,1)%j_value
                  line(inum(1))%high = 1
                  return
               else if ( even_level(i,1)%name(1:nalpha(3)) .eq. 
     1                   alpha(3)(1:nalpha(3))) then               ! Upper level is even
                  line(inum(1))%idu = even_level(i,1)%lev_id
                  line(inum(1))%ju = even_level(i,1)%j_value
                  line(inum(1))%high = -1
                  return
               endif
            enddo
            call wrtstr(' Warning: level not found in list of energy 
     1 levels - parity and j value not changed') 
            return
         case ('comment') 
            line(inum(1))%comment = alpha(3)(1:nalpha(3))
            return

         case ('interp')
            ident%num_intrpt_pts = inum(1)
            return

         case default
            write(wrtbuf,'(a,a)') 'Unknown parameter ',
     1            alpha(2)(1:nalpha(2))
            call wrtstr(wrtbuf)
            call wrtstr('Possible parameters: al, bl, au, bu, waveno, 
     1 snr, damp, fwhm, comment, interp')
         end select

         
         return
!
! hfs write <fname>  - Writes all of hfs linelist to ascii file <fname>
! *****************
!            

      case ('write')
         if(nalpha(2) .eq. 0) then
            call wrtstr('Usage: hfs write <fname>')
         endif

         open(unit=hfswrtline, file = alpha(2)(1:nalpha(2)), 
     1        action='write', status='new', iostat = ierr)
         if (ierr .ne. 0) then
            call wrtstr('File already exists')
            return
         endif


         write(hfswrtline,'(a)') 'no.  Wavenumber    SNR    FWHM  Eta  I
     1ntensity Lower level  J_l   A_l     B_l Upper level  J_u   A_u  
     1 B_u   Comment'
         do i=1, no_lines
            write(hfswrtline,8010,err=8030) i,line(i)%waveno, 
     1            line(i)%snr, line(i)%fwhm, line(i)%damp,
     1            line(i)%inten, line(i)%el, line(i)%jl, line(i)%al, 
     1            line(i)%bl, line(i)%eu, line(i)%ju,
     1            line(i)%au, line(i)%bu, line(i)%comment 
            write(hfswrtline,8020,err=8030) line(i)%delwaveno, 
     1            line(i)%delsnr, line(i)%delfwhm, line(i)%deldamp,
     1            line(i)%delinten,line(i)%delal, line(i)%delbl, 
     1            line(i)%delau, line(i)%delbu
         enddo
!
! Add another digit onto A constants (GN, March 2015)
!
8010     format(i4,1xf10.4,1x,f7.1,1x,f6.1,1x,f4.2,1x,e10.3,1x,
     1          2(a10,1x,f5.1,f7.2,f7.1,1x),a40)
8020     format(5x,f10.4,1x,f7.1,1x,f6.1,1x,f4.2,1x,e10.3,1x,
     1          2(16x,f7.2,f7.1,1x))
         close (hfswrtline)
         return
 8030    call wrtstr('Error on write - full disk?')
         return


      case default
         call wrtstr(" No match for case")
         return

      end select

      end

      subroutine setj(thisline, lowj, uppj)
      implicit none
      integer NUM_LEN, MAXLINES, OTHER_LEN
      parameter ( NUM_LEN = 32, MAXLINES =  10000, OTHER_LEN = 256)
      real*8 lowj, uppj
      integer thisline
! Re-write for gfortran      
!      structure /hfs_params/     ! Order is important
      type hfs_params
         sequence
         real*8 waveno, snr, inten, fwhm, damp, al, au, bl, bu
         real*8 delwaveno, delsnr, delinten, delfwhm, deldamp
         real*8 delal, delau, delbl, delbu
         real*4 jl, ju
         logical hwaveno, hsnr, hfwhm, hdamp, hal, hau, hbl, hbu ! Logical*4 
         integer high,idl,idu,user1  
         integer user2,user3,user4,user5 ! add 5 integers for dummy storage
         character el*20,eu*20     ! 20 characters for energy level identifier
         character comment*40      ! 40 characters for a comment
      end type
!      end structure

! Rewrite for gfortran
!      record /hfs_params/ line(MAXLINES)
      type(hfs_params) line(MAXLINES)

      common /this_hfsline/ line

      save /this_hfsline/

      lowj = line(thisline)%jl
      uppj = line(thisline)%ju
      return
      end

      subroutine getinfo( thisline, lowj, uppj, lowA, uppA, lowB, uppB, 
     1           wave, snr, damp, fwhm, alh, blh, auh, buh, waveh, 
     1           intenh, damph, fwhmh, lowlev, upplev, comment)

      implicit none

      integer NUM_LEN, MAXLINES, OTHER_LEN
      parameter ( NUM_LEN = 32, MAXLINES =  10000, OTHER_LEN = 256)

      real*8 lowj, uppj, lowA, uppA, lowB, uppB, wave, snr, damp, fwhm
      logical*4 alh, blh, auh, buh, waveh, intenh, damph, fwhmh
      character (len=*) :: lowlev, upplev, comment

      integer thisline
! Rewrite for gfortran      
!      structure /hfs_params/     ! Order is important
       type hfs_params 
         sequence
         real*8 waveno, snr, inten, fwhm, damp, al, au, bl, bu
         real*8 delwaveno, delsnr, delinten, delfwhm, deldamp
         real*8 delal, delau, delbl, delbu
         real*4 jl, ju
         logical hwaveno, hsnr, hfwhm, hdamp, hal, hau, hbl, hbu ! Logical*4 
         integer high,idl,idu,user1  
         integer user2,user3,user4,user5 ! add 5 integers for dummy storage
         character el*20,eu*20     ! 20 characters for energy level identifier
         character comment*40      ! 40 characters for a comment
      end type
!      end structure

! Rewrite for gfortran
!      record /hfs_params/ line(MAXLINES)
      type(hfs_params) line(MAXLINES)

      common /this_hfsline/ line

      save /this_hfsline/

      lowj = line(thisline)%jl
      uppj = line(thisline)%ju
      lowA = line(thisline)%al
      uppA = line(thisline)%au
      lowB = line(thisline)%bl
      uppB = line(thisline)%bu
      wave = line(thisline)%waveno
      snr = line(thisline)%snr
      damp = line(thisline)%damp
      fwhm = line(thisline)%fwhm
      alh =  line(thisline)%hal
      blh =  line(thisline)%hbl
      auh =  line(thisline)%hau
      buh =  line(thisline)%hbu
      waveh =  line(thisline)%hwaveno
      intenh =  line(thisline)%hsnr
      damph =  line(thisline)%hdamp
      fwhmh =  line(thisline)%hfwhm
      lowlev = line(thisline)%el
      upplev = line(thisline)%eu
      comment = line(thisline)%comment
      
      return

      end

      subroutine findline( thisline, waveno )

      implicit none

      real*8 waveno
      integer thisline

      integer MAXLINES
      parameter ( MAXLINES =  10000)

! Rewrite for gfortran      
!      structure /hfs_params/     ! Order is important
       type hfs_params 
         sequence
         real*8 waveno, snr, inten, fwhm, damp, al, au, bl, bu
         real*8 delwaveno, delsnr, delinten, delfwhm, deldamp
         real*8 delal, delau, delbl, delbu
         real*4 jl, ju
         logical hwaveno, hsnr, hfwhm, hdamp, hal, hau, hbl, hbu ! Logical*4 
         integer high,idl,idu,user1  
         integer user2,user3,user4,user5 ! add 5 integers for dummy storage
         character el*20,eu*20     ! 20 characters for energy level identifier
         character comment*40      ! 40 characters for a comment
      end type
!      end structure

      integer i

! Rewrite for gfortran
!      record /hfs_params/ line(MAXLINES)
      type(hfs_params) line(MAXLINES)

      common /this_hfsline/ line

      do i = 1,MAXLINES
         if ( abs(waveno - line(i)%waveno ) .lt. 0.01)  then
            thisline = i
            return
         endif
      enddo
      call wrtstr ("Warning - line not found ")

      return
      end
