
      program adduew

      character linfile*20, outfile*20
      integer MAXLINES
      parameter (MAXLINES=10000)
      double precision pminus, pplus, l_damp, u_damp, perr
      real p(26)

      structure /hfs_params/     ! Order is important.
         double precision  waveno,snr,inten,fwhm,damp,al,au,bl,bu,
     &          delwaveno,delsnr,delinten,delfwhm,deldamp,
     &          delal,delau,delbl,delbu
         real jl, ju
         logical hwaveno, hsnr, hfwhm, hdamp, hal, hau, hbl, hbu
         integer high,idl,idu,user1    ! Higher level (1=odd, -1 = even) 
                                       ! idl = Phits id for lower level
                                       ! idu = Phits id for upper level
         integer user2,user3,user4,user5 ! add 5 integers -make up to 256 bytes
         character el*20,eu*20     ! 20 characters for energy level identifier
         character comment*40      ! 40 characters for a comment
      end structure
      record /hfs_params/ line

c      data (p(i),i= 1, 26) - used for voigt area calculations
      data p  /  1.064467, 1.081922, 1.099638, 1.117619,
     *     1.135864, 1.154377, 1.173157, 1.192206, 1.211524, 1.231108,
     *     1.250959, 1.271072, 1.291443, 1.312067, 1.332934, 1.354034,
     *     1.375351, 1.396866, 1.418555, 1.440383, 1.462309, 1.484277,
     *     1.506214, 1.528029, 1.549605, 1.570797/
c note that p(b1=0.05) = 1.108595
      
      ! Read in and open files

      print*, 'binary hfsline file for input ? '
      read*, linfile
      print*, 'binary hfsline file for output ? '
      read*, outfile
      
      open (unit=10,file=linfile,access='direct', !&
     1     recl=296,err=100)   ! Open the hfslin file
      open (unit=11,file=outfile,access='direct', !&
     1     recl=296,err=200)   ! Open the hfslin file
      
      ! Loop over all lines - calculate unc. in intensity

      do i=1,MAXLINES
         read(10,rec=i, iostat=iostatus) line
         if(iostatus .gt. 0) goto 20   ! End of file
         if(iostatus .lt. 0) goto 300  ! Error

         print '(f10.4 f8.2 f6.1 f7.4 e10.2)',
     1        line.waveno,line.snr, line.fwhm, line.damp, line.inten
         print '(10x  f8.2 f6.1 f7.4 e10.2)',
     1        line.delsnr, line.delfwhm, line.deldamp, line.delinten

         ! Calculate uncertainty in line intensity
         ! this is sqrt ( line.delsnr^2 + line.delfwhm^2 + line.p^2 )
         ! get line.p from line.deldamp via lookup table p
         ! Calculate integrated intensity. Voigt function as in Brault's routine.
            
         ! First calculate damping parameter in Brault units. l_damp = eta*g^2 / (1-eta)

         pminus = line.damp - line.deldamp
         pplus = line.damp + line.deldamp
         eps = 0.099
         bcoeff = 1-1/log(2.) - eps

         if (pminus .le. 0) then
            l_damp = 0
         else
            l_damp = ( (1-pminus*bcoeff) - sqrt( (pminus*bcoeff-1)**2 
     1                  - 4*eps*pminus*pminus/log(2.)) )
     1           /(2*eps*pminus)
         endif

         if (pplus .le. 0) then
            u_damp = 0
         else
            u_damp = ( (1-pplus*bcoeff) - sqrt( (pplus*bcoeff-1)**2 
     1                  - 4*eps*pplus*pplus/log(2.)) )
     1           /(2*eps*pplus)
         endif
         
         ! Then calculate integrated intensity of strongest component.
         
         l_damp = l_damp*25. +1. ! Brault damping runs from 1 to 26.
         j = l_damp
         if (j .le. 26) then 
            pminus = p(j) + (l_damp-j)*(p(j+1)-p(j)) ! factor is in lookup table p
         else 
            pminus = p(26)
         end if

         u_damp = u_damp*25. +1. ! Brault damping runs from 1 to 26.
         j = u_damp
         if (j .le. 26) then 
            pplus = p(j) + (u_damp-j)*(p(j+1)-p(j)) ! factor is in lookup table p
         else 
            pplus = p(26)
         end if

         perr = (pminus-pplus)/(0.5*(pminus+pplus))
         line.delinten = sqrt( perr*perr +  
     1        line.delsnr*line.delsnr/(line.snr*line.snr) + 
     1        line.delfwhm*line.delfwhm/(line.fwhm*line.fwhm))

         print '(f8.5 f8.5 f9.5 f8.5)', pminus,pplus,perr,line.delinten


      enddo
 20   continue
      close (10)
      close(11)
      stop
 100  print*, 'Error in opening hfsline file'
      stop
 200  print*, 'Error in opening output file'
      stop
 300  print*, 'Error in reading input file'
      end
