c ______________________________________________________________________

      subroutine option
c
        include 'parms.h'

	if (nalpha(1) .eq. 0) go to 999
	if (alpha(1)(1:nalpha(1)) .eq. 'readdata')  go to 2000
	if (alpha(1)(1:nalpha(1)) .eq. 'show')  go to 2000
	if (alpha(1)(1:nalpha(1)) .eq. 'xcol')  go to 2000
	if (alpha(1)(1:nalpha(1)) .eq. 'ycol')  go to 2000
	if (alpha(1)(1:nalpha(1)) .eq. 'close')  go to 2000
	if (alpha(1)(1:nalpha(1)) .eq. 'zeroline')  go to 3000
	if (alpha(1)(1:nalpha(1)) .eq. 'nonumbers')  go to 4000
	if (alpha(1)(1:nalpha(1)) .eq. 'eps')  go to 5000
	if (alpha(1)(1:nalpha(1)) .eq. 'ps')  go to 5050
	if (alpha(1)(1:nalpha(1)) .eq. 'log')  go to 6000
	if (alpha(1)(1:nalpha(1)) .eq. 'ltick')  go to 5060
	if (alpha(1)(1:nalpha(1)) .eq. 'rtick')  go to 5070

999	write(output,*) ' specified option not implemented'
	return

998	write(output,*) ' syntax error for option command'
	return

997	write(output,*) ' unable to read requested record'
	return


c option readdata        
c *****************************
c  
2000    call rdcard 
        atls = .false.
        go to 10000
c option zeroline
c *****************************
c
3000    zeroline = .true.
        go to 10000

c option nonumbers x
c option nonumbers y
c option nonumbers 
c *****************************
c
4000    if (alpha(2)(1:1) .eq. 'x')noxnum = .true.
        if (alpha(2)(1:1) .eq. 'y')noynum = .true.
        go to 10000
c option eps - create an encapsulated PostScript File
c option ps  - create a PostScript File.
c ******************************
5000    epsfl = .true.
        go to 10000
5050    psfl = .true.
        go to 10000
5060    ltick = .true.
        go to 10000
5070    rtick = .true.
        go to 10000
6000    logfl = .true.
10000   return
	end

	subroutine rdcard

        include 'parms.h'

	maxpts = 8192 

        if (alpha(1)(1:nalpha(1)) .eq. 'show') go to 2900
        if (alpha(1)(1:nalpha(1)) .eq. 'xcol') go to 2940
        if (alpha(1)(1:nalpha(1)) .eq. 'ycol') go to 2950
        if (alpha(1)(1:nalpha(1)) .eq. 'close') close (16)
	if (ifx .gt. 0) then
         nop = inum(1)
         ncol = inum(2)
         iprint = inum(3)
        endif
	if (nstrng .ge. 2) format = alpha(2)(1:nalpha(2))
	if (nstrng .ge. 2) filenam = alpha(3)(1:nalpha(3))

     	open (16,file=filenam,access='sequential',
     *		     form='formatted',status='old',err=2998,
     *		     iostat=i)
c					format was not provided on cmnd. line.
c					assume its the 1st line in the file.

	if (nstrng .eq. 0) read (16, 9000, end=2997) format
9000	format(a)

c					don't allow buffer overrun.
	if (nop .gt. maxpts) nop = maxpts

        do i = 1, nop 
	read (16, format, end=2997, err=2999) (col(j,i), j= 1, ncol)
        enddo

        if (iprint .ne. 0) write(*,*) col(1,1), col(2,1), 
     *   col(3,1), col(4,1)
        go to 2997
2900    do i = 1, nop
        iprint = inum(1)
        if (iprint .ne. 0) write (*,*) (col(j,i), j = 1, ncol)
        enddo
        go to  2997
2940    ixcol = inum(1)
        iprint = inum(2)
        do i = 1, nop
        xcol(i) = col(ixcol,i)
        if (iprint .ne. 0) write (*,*) xcol(i)
        enddo
        go to 2997
2950    iycol = inum(1)
        iprint = inum(2)
        do i = 1, nop
          if (xnum(1) .eq. 0.0e0)xnum(1) = 1.0e0
          ycol(i) = col(iycol,i)*xnum(1)
          if (iprint .ne. 0) write (*,*) ycol(i)
        enddo

2997	return

2998    write (*,*) 'ERROR: file not found'
        return

2999	write (*,*) 'Error on readcards - data may not be valid'
	return

	end

