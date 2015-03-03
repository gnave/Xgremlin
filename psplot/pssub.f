c pssub.for  Version 1.0
c
c Version 1.0  12/03/90  Genesis
c
c lay down a line in x from ia to ib at y=ic
c
	subroutine xline(ifa,ifb,ifc)
c       **************************
        include 'parms.h'
        real xia, xib, xic, xbpos, ybpos, xb, yb

	integer*4 ifa,ifb,ifc
c
	if (horz) then
		xia = ifa/4.1666
		xib = ifb/4.1666
		xic = ifc/4.1666
		go to 1000
	else
		xia = ifa/4.1666
		xib = ifb/4.1666
		xic = ifc/4.1666
                go to 1000
	endif
100	continue

        if ((xic .gt. 999.) .or. (xia .gt. 999.)) go to 103
        write(spool,101)xic, xia
101     format(1x,f7.2,1x,f7.2,' moveto')

        if ((xic .gt. 999.) .or. (xib .gt. 999.)) go to 103
        write(spool,102)xic, xib
102     format(1x,f7.2,1x,f7.2,' lineto')

103     return
c
c lay down a line in y from ia to ib at x=ic

	entry yline(ifa,ifb,ifc)
c       *********************
c            call Yline(ibot,itop,ixl)
	if (horz) then
		xia = (ifa)/4.1666
		xib = (ifb)/4.1666
		xic = ifc/4.1666
		go to 100
	else
		xia = ifa/4.1666
		xib = ifb/4.1666
		xic = ifc/4.1666
                go to 100
	endif
1000	continue 

        if ((xic .gt. 999.) .or. (xia .gt. 999.)) go to 1003
        write(spool,1001)xia, xic
1001    format(1x,f7.2,1x,f7.2,' moveto')

        if ((xic .gt. 999.) .or. (xib .gt. 999.)) go to 1003
        write(spool,1002)xib, xic
1002    format(1x,f7.2,1x,f7.2,' lineto')

1003    return
c
	entry xyclear
c       *************

	return
c
c this subroutine lays down the dots in the array using the Bresingham
c algorithm
	entry xyplot(ix1, iy1, ix2, iy2)
c       *******************

	if (horz) then
	  xbpos = ix1/4.1666
	  xb = ix2/4.1666
	  ybpos = iy1/4.1666
	  yb = iy2/4.1666
        else
          xbpos = ix1/4.1666
          xb = ix2/4.1666
          ybpos = iy1/4.1666
          yb = iy2/4.1666
	endif

        if ((xbpos .gt. 999.) .or. (ybpos .gt. 999.)) go to 2007
        write(spool,2005)xbpos,ybpos
2005    format(1x,f7.2,1x,f7.2,' moveto')

        if ((xb .gt. 999.) .or. (yb .gt. 999.)) go to 2007
        write(spool,2006)xb,yb
2006    format(1x,f7.2,1x,f7.2,' lineto')

2007	return
	end
c **********************************************************************
c ________________________________________________________________________
c
c this subroutine lays down the characters in the array
c the string is in  chstr , nc characters long.  It will be plotted at
c pixel coordinates (ix,iy).  ix is down the page (long side) and iy is
c across the page; i.e., landscape mode is assumed.
c ijus = -1 for left justification, =0 for center, =1 for right
c idx and idy give the direction in which the string is to flow; one should 
c be zero and the other +1 or -1.  
c isz is the character size multiplier; 1 is best, 2 usable, 3 pretty poor.
c isl is slant (tested on reversed lettering mca 4/16/89), pixels/cap height
c
c syntax
c /Helvetica findfont
c 10 scalefont setfont
c 175 500 moveto
c  (Figure 3.   Wavenumber differences for two overlapping) show  
                                                      
	subroutine chplot(nchr,chstr,ixf,iyf,ijus,idxf,idyf,isl,isz)
c       ************************************************************

        include 'parms.h'
        character*132 chstr,chst2, chst3, chst4
        character*10 formt
        real xix, xiy
c set the direction of the fonts
        if ((idxf .eq. 1) .and. (idyf .eq. 0))then
c right do nothing, this is the default
		xix = (ixf)/4.1666
		xiy = (iyf)/4.1666
        elseif ((idxf .eq. -1) .and. (idyf .eq. 0))then
c left
        write (spool, *) '312 396 translate'
        write (spool, *) ' 180 rotate'
		xix = 396 - iyf/4.1666  
		xiy = 312 - ixf/4.1666
        elseif ((idxf .eq. 0) .and. (idyf .eq. -1))then
c down
        write (spool, *) '312 396 translate'
        write (spool, *) ' 270 rotate'
c		xix = 312 - ixf/4.1666
c		xiy = iyf/4.1666 - 396
		xix = -1*(iyf/4.1666 - 396)
		xiy = -1*(312 - ixf/4.1666)
        elseif ((idxf .eq. 0) .and. (idyf .eq. 1))then
c up
        write (spool, *) '312 396 translate'
        write (spool, *) ' 90 rotate'
c calculate the position relative to the center of the page
		xix = iyf/4.1666 - 396
		xiy = 312 - ixf/4.1666
        endif
c
c  The standary PostScript fonts available are:
c    Helvetica
c      Helvetica-Oblique
c      Helvetica-Bold
c      Helvetica-BoldOblique
c    Times-Roman
c      Times-Italic
c      Times-Bold 
c      Times-BoldItalic
c    Courier  
c      Courier-Oblique
c      Courier-Bold
c      Courier-BoldOblique
c    ZapfDingbat
c    Symbol
c    AvantGarde-Book
c      AvantGarde-BookOblique
c      AvantGarde-Demi
c      AvantGarde-DemiOblique
c    Bookman-Light
c      Bookman-LightItalic
c      Bookman-Demi 
c      Bookman-DemiItalic
c    Helvitica-Narrow
c      Helvitica-Narrow-Oblique
c      Helvitica-Narrow-Bold
c      Helvitica-Narrow-BoldOblique
c    NewCenturySchlbk-Roman
c      NewCenturySchlbk-Italic
c      NewCenturySchlbk-Bold
c      NewCenturySchlbk-BoldItalic
c    Palatino-Roman 
c      Palatino-Italic
c      Palatino-Bold 
c      Palatino-BoldItalic
c    ZapfChancery-MediumItalic
c
        if (fnt .eq. 0) fnt = 5
        if (fnt .eq. 1) then
           write(spool,*)' /Helvetica findfont'          
        elseif (fnt .eq. 2) then
           write(spool,*)' /Helvetica-Oblique findfont'          
        elseif (fnt .eq. 3) then
           write(spool,*)' /Helvetica-Bold findfont'          
        elseif (fnt .eq. 4) then
           write(spool,*)' /Helvetica-BoldOblique findfont'          
        elseif (fnt .eq. 5) then
           write(spool,*)' /Times-Roman findfont'          
        elseif (fnt .eq. 6) then
           write(spool,*)' /Times-Italic findfont'          
        elseif (fnt .eq. 7) then
           write(spool,*)' /Times-Bold findfont'          
        elseif (fnt .eq. 8) then
           write(spool,*)' /Times-BoldItalic findfont'          
        elseif (fnt .eq. 9) then
           write(spool,*)' /Courier findfont'          
        elseif (fnt .eq. 10) then
           write(spool,*)' /Courier-Oblique findfont'          
        elseif (fnt .eq. 11) then
           write(spool,*)' /Courier-Bold findfont'          
        elseif (fnt .eq. 12) then
           write(spool,*)' /Courier-BoldOblique findfont'          
        elseif (fnt .eq. 13) then
           write(spool,*)' /ZapfDingbats findfont'          
        elseif (fnt .eq. 14) then
           write(spool,*)' /Symbol findfont'          
        elseif (fnt .eq. 15) then
           write(spool,*)' /AvantGarde-Book findfont'
        elseif (fnt .eq. 16) then
           write(spool,*)' /AvantGarde-BookOblique findfont'
        elseif (fnt .eq. 17) then
           write(spool,*)' /AvantGarde-Demi findfont'
        elseif (fnt .eq. 18) then
           write(spool,*)' /AvantGarde-DemiOblique findfont'
        elseif (fnt .eq. 19) then
           write(spool,*)' /Bookman-Light findfont'
        elseif (fnt .eq. 20) then
           write(spool,*)' /Bookman-LightItalic findfont'
        elseif (fnt .eq. 21) then
           write(spool,*)' /Bookman-Demi  findfont'
        elseif (fnt .eq. 22) then
           write(spool,*)' /Bookman-DemiItalic findfont'
        elseif (fnt .eq. 23) then
           write(spool,*)' /Helvitica-Narrow findfont'
        elseif (fnt .eq. 24) then
           write(spool,*)' /Helvitica-Narrow-Oblique findfont'
        elseif (fnt .eq. 25) then
           write(spool,*)' /Helvitica-Narrow-Bold findfont'
        elseif (fnt .eq. 26) then
           write(spool,*)' /Helvitica-Narrow-BoldOblique findfont'
        elseif (fnt .eq. 27) then
           write(spool,*)' /NewCenturySchlbk-Roman findfont'
        elseif (fnt .eq. 28) then
           write(spool,*)' /NewCenturySchlbk-Italic findfont'
        elseif (fnt .eq. 29) then
           write(spool,*)' /NewCenturySchlbk-Bold findfont'
        elseif (fnt .eq. 30) then
           write(spool,*)' /NewCenturySchlbk-BoldItalic findfont'
        elseif (fnt .eq. 31) then
           write(spool,*)' /Palatino-Roman findfont'
        elseif (fnt .eq. 32) then
           write(spool,*)' /Palatino-Italic findfont'
        elseif (fnt .eq. 33) then
           write(spool,*)' /Palatino-Bold findfont'
        elseif (fnt .eq. 34) then
           write(spool,*)' /Palatino-BoldItalic findfont'
	endif
	lastfnt = fnt
	init = 1

        
c specify the font size in points

        if (isz .eq. 0)isz = 10
        write(spool,300)isz
300     format(1x,i3,' scalefont setfont')

c specify the text in PostScript format

c clear any left over text characters
          do ii = 1, 132
            chst2(ii:ii) = ' '
          enddo

c convert the string and strip or interprete and special characters

c parse the string looking for superscripts (^) and subscripts (_)
c with delimiters {} if more than 1 character
        jcount = 1
        jextra = 19
        jc = 0
        jj = 0
        nchar  = nchr 
c       write (*,*) 'nchar = ', nchar 
        do j = 1, nchar
           jc = jc + 1
           jj = jj + 1
c for an ordinary character, include it, and increment the counter
           chst2(jj:jj) = chstr(jc:jc)
	   nch = jj
c          write (*,*) jj, jc, chstr(jc:jc)
c the characters ( and ) are special characters, insert a \ to make them
c valid
           IF (chstr(jc:jc) .eq. '(')then
             chst2(jj:jj+1) = '\\('
             jj = jj + 1
	     nch = jj 
           ELSEIF (chstr(jc:jc) .eq. ')')then
             chst2(jj:jj+1) = '\\)'
             jj = jj + 1
	     nch = jj
           ELSEIF (chstr(jc:jc) .eq. '^')then
c enter superscript mode
             if (chstr(jc+1:jc+1) .ne. '{')then
               chst2(jj:jj+7) = ') show ('
               chst2(jj+8:jj+8) = chstr(jc+1:jc+1)
               chst2(jj+9:jj+19) = ') supshow ('
	       nch = nch + jextra
               jc = jc + jcount
               jj = jj + jextra
             else
c then there is more than 1 character in the element in the 
c superscript string
               chst2(jj:jj+7) = ') show ('
               do jjj = 2, nchar
c                write (*,*) chstr(jc+jjj:jc+jjj)
                 if (chstr(jc+jjj:jc+jjj) .ne. '}')then
                  chst2(jj+6+jjj:jj+6+jjj) 
     *                 = chstr(jc+jjj:jc+jjj)
                 else
                  chst2(jj+6+jjj:jj+16+jjj) = ') supshow ('
	          nch = nch + 16 + jjj
                  go to 3006
                 endif
               enddo
3006           continue
               jc = jc + jjj
               jj = jj + 16 + jjj

             endif
           ELSEIF (chstr(jc:jc) .eq. '_')then
c enter subscript mode
             if (chstr(jc+1:jc+1) .ne. '{')then
               chst2(jj:jj+7) = ') show ('
               chst2(jj+8:jj+8) = chstr(jc+1:jc+1)
               chst2(jj+9:jj+19) = ') subshow ('
	       nch = nch + jextra
               jc = jc + jcount
               jj = jj + jextra
             else
c then there is more than 1 character in the element in the 
c subscript string
               chst2(jj:jj+7) = ') show ('
               do jjj = 2, nchar
c                write (*,*) chstr(jc+jjj:jc+jjj)
                 if (chstr(jc+jjj:jc+jjj) .ne. '}')then
                  chst2(jj+6+jjj:jj+6+jjj) 
     *                 = chstr(jc+jjj:jc+jjj)
                 else
                  chst2(jj+6+jjj:jj+16+jjj) = ') subshow ('
	          nch = nch + 16 + jjj
                  go to 3007
                 endif
               enddo
3007           continue
               jc = jc + jjj
               jj = jj + 16 + jjj

             endif
c          ELSE
           ENDIF

c              write (*,*) chst2(1:nch), nch
               if (jc .eq. nchar) go to 3009
        enddo
3009    continue

c measure the string length (non special characters) for justification
c it just a boring character - eliminate special characters in chstr
           jfin = 2
           jchar = 1
           do k = 1, nchar
c            write (*,*) '*', chstr(jchar:jchar), jchar
            if ( (chstr(jchar:jchar) .eq. '(') .or.
     *      (chstr(jchar:jchar) .eq. ')') .or.
     *      (chstr(jchar:jchar) .eq. '{') .or.
     *      (chstr(jchar:jchar) .eq. '}') .or.
     *      (chstr(jchar:jchar) .eq. '^') .or.
     *      (chstr(jchar:jchar) .eq. '_')  )go to 3008
             chst4(jfin:jfin) = chstr(jchar:jchar)
c            write (*,*) '**', chst4(1:jfin), jfin
             jfin = jfin + 1
3008         jchar = jchar + 1
           enddo

c then encase the string in ( and ) show for postscript
          chst4(1:1) = '('
          chst4(jfin:jfin) = ')'

          chst3(1:1)='('
          chst3(2:nch+1)=chst2
          chst3(nch+2:nch+7)=') show'

c          write (*,*) '***', chst4(1:jfin), jfin
c          write (*,*) '***', chstr(1:jfin), jfin

          write(formt,102) nch+7
102       format('(a',i3,')')
c format statement for string measurement
          write(format,104) jfin 
c         write(format,104) nch+2
104       format('(a',i3,')')
c

c calculate the correct position accounting for justification.
c move to the correct postion
c left justified:
        if (ijus .eq. -1)then
           write (spool,101)xix,xiy
	   write (spool,formt) chst3
c center justified:
        elseif(ijus .eq. 0)then
             write (spool,105) xix
	     write (spool,format) chst4
             write (spool,106)
             write (spool,103)xiy
	     write (spool,formt) chst3
c          endif
c right justified:
        elseif(ijus .eq. 1)then
             write (spool,105) xix
	     write (spool,format) chst4
             write (spool,107) 
             write (spool,103)xiy
	     write (spool,formt) chst3
        endif
101          format(1x,f7.2,1x,f7.2,' moveto')

103          format(1x,f7.2,' moveto')

105          format (1x,f7.2)
106          format(1x, 'stringwidth pop 2 div sub')
107          format(1x, 'stringwidth pop sub')

c reset the page position  and oreigntation 
c       if ((idxf .eq. 1) .and. (idyf .eq. 0))then
c right do nothing, this is the default
c       elseif
        if ((idxf .eq. -1) .and. (idyf .eq. 0))then
c left
        write (spool, *) ' 180 rotate'
        write (spool, *) '-312 -396 translate'
        elseif ((idxf .eq. 0) .and. (idyf .eq. -1))then
c down
        write (spool, *) ' 90 rotate'
        write (spool, *) '-312 -396 translate'
        elseif ((idxf .eq. 0) .and. (idyf .eq. 1))then
c up
        write (spool, *) ' 270 rotate'
        write (spool, *) '-312 -396 translate'
        endif
        return
        end

c *********************************************************************
        subroutine qantiz (in,iq,im)
        integer*4 in, iq, im
        iq = 1
10      ni = iq
        if (iq .ge. in) go to 20
        iq = 2*ni
        if (iq .ge. in) go to 20
        iq = 5*ni
        if (iq .ge. in) go to 20
        iq = 10*ni
        go to 10
20	im = 1
        if(iq .ge. 10) im = 10
        if(iq .ge. 100) im = 100
        if(iq .ge. 1000) im = 1000
	return
        end
c *********************************************************************

        subroutine sigtow(sigma,wavel)
        double precision sigma, wavel, z,ri
c
        z = 1.0e-4*sigma
        z = z*z
        ri = 1.0 +1.0e-7*(643.28 +294981./(146.-z) +2554./(41.-z))
	wavel = 9999999.999
        if (sigma .ne. 0.0) wavel = 1.0e8/(ri*sigma)
        return
c 
        entry wavtos(sigma,wavel)
c       *************************

	sigma = 9999999.999
        if (wavel .ne. 0.0) then
	   z = 0.99972e4/wavel
	   z = z*z
	   ri = 1.0 +1.0e-7*(643.28 +294981./(146.-z) +2554./(41.-z))
	   sigma = 1.0e8/(ri*wavel)
	endif
        return
        end
c
c *****************************************************************
c plot new cursor via XOR; igmode=-1 leaves a copy behind
c igmode=0 for original, igmode=1 to erase old first

      subroutine gcurs(ixx,iyy,idxx,idyy,igmode)

      integer*4 ixx,iyy,ixsv,iysv,igmode,idxx,idyy,idxsv,idysv

	intor = 2
      if (igmode .gt. 0) then
c erase old cursor
      		call dcmove (ixsv,iysv-idysv)
      		call dcdraw (ixsv,iysv+idysv,intor)
      		call dcmove (ixsv-idxsv,iysv)
      		call dcdraw (ixsv+idxsv,iysv,intor)
	endif
c for normal cursor, save current setting
      if (igmode .ge. 0) then
      		ixsv = ixx
      		iysv = iyy
      		idxsv = idxx
      		idysv = idyy
	endif
c plot new cursor via XOR; igmode=-1 leaves a copy behind
      call dcmove (ixsv,iysv-idysv)
      call dcdraw (ixsv,iysv+idysv,intor)
      call dcmove (ixsv-idxsv,iysv)
      call dcdraw (ixsv+idxsv,iysv,intor)
c reset intensity
	call level(inton)
      return
      end
 
c **********************************************************************
      subroutine dcmove (ixx,iyy)
 
c move pen to (ixx,iyy) without writing
 
	call putpt(ixx,iyy)
	return
	end
c **********************************************************************
      subroutine dcdraw (ixx,iyy,int)
 
c draw a line of intensity (int) from last point to (ixx,iyy)

	integer*4 int
 
	call level(int)
	call dline(ixx,iyy)
	return
	end
c **********************************************************************
