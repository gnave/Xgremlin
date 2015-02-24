c \gremlin\src\cmplx.for   Ver. 2.2 

c  Revision 2.24 93/10/11  angle added
c  Revision 2.23 92/12/02  spiral added
c  Revision 2.22 91/05/22  nwpp handling updated
c  Revision 2.21 89/05/05  Real, imag and amp now call disper
c  Revision 2.2  89/02/05  

	subroutine complx( r, tr )

	real twopi
	parameter( twopi = 6.28318531 )

        include 'datetc.h'
        include 'infmtn.h'
        include 'iounit.h'

	real r(*), tr(*)

	real u,v,ut,rtmp
	integer nopp

	nopp = nop/nwpp    ! number of complex points in r

	if (nalpha(1) .eq. 0) go to 999
	if (alpha(1)(1:nalpha(1)) .eq. 'linecorr') goto 10
	if (alpha(1)(1:nalpha(1)) .eq. 'angle')    goto 10
	if (alpha(1)(1:nalpha(1)) .eq. 'rotate')   goto 1000
	if (alpha(1)(1:nalpha(1)) .eq. 'real')     goto 2000
	if (alpha(1)(1:nalpha(1)) .eq. 'imag')     goto 3000
	if (alpha(1)(1:nalpha(1)) .eq. 'amp')      goto 4000
	if (alpha(1)(1:nalpha(1)) .eq. 'phase')    goto 4500
	if (alpha(1)(1:nalpha(1)) .eq. 'compexp')  goto 5000
	if (alpha(1)(1:nalpha(1)) .eq. 'spiral')   goto 6000
	if (alpha(1)(1:nalpha(1)) .eq. 'conjug')   goto 7000
	if (alpha(1)(1:nalpha(1)) .eq. 'add')      goto 8000
	if (alpha(1)(1:nalpha(1)) .eq. 'combine')  goto 10000
	if (alpha(1)(1:nalpha(1)) .eq. 'multiply') goto 11000
	if (alpha(1)(1:nalpha(1)) .eq. 'product')  goto 12000
	if (alpha(1)(1:nalpha(1)) .eq. 'ratio')    goto 12000
	if (alpha(1)(1:nalpha(1)) .eq. 'one')      goto 13000
	if (alpha(1)(1:nalpha(1)) .eq. 'complex')  goto 14000
      


999	call wrtout(output,' Error :  unknown complex operation.')
	return

998	call wrtout(output,' Error :  syntax error for complex command')
	return


c complex "linecorr"  nstrt nstop
c complex "angle"  nstrt nstop
c *******************************
c Find the phase of a part of a complex array in r; correct all in r.
c Input and output in r
c If no range is given, default is 1,nop

10	ia = 1
	ic = nop/nwpp       ! nop is the number of REALs in the r array
	if (ifx .eq. 2) then
	   ia = inum(1)     ! ia, ic refer to complex points
	   ic = inum(2)
	endif
	re = 0.0
	xim = 0.0
	call zsum( r, ia, ic, re, xim )  ! treats r as complex array
	theta = atan2(xim,re)
	u = cos(theta)
	v = sin(theta)
	write (wrtbuf,111) theta
	call wrtout(output, wrtbuf)
111	format (' apparent phase angle = ',f7.4)
	if (alpha(1)(1:nalpha(1)) .eq. 'angle') return
	if (alpha(2)(1:nalpha(2)) .eq. 'local') then
	   ia = 2 * ia - 1  ! now ia, ic refer to array indices in r
	   ic = 2 * ic - 1
	else
	   ia = 1
	   ic = nop
	end if
	do i=ia,ic,2        ! rotate by phase angle
	   temp = r(i)*u + r(i+1)*v
	   r(i+1) = r(i+1)*u - r(i)*v
	   r(i) = temp
	end do
	return

c complex "rotate" real imag
c *****************
c complex "rotate" theta
c ********************************
c Given real and imaginary values, rotate the array to make that vector real.
c Input and output in r

 1000	if (ifl .gt. 2 .or. ifl .eq. 0) go to 998
    	if (ifl .eq. 2) then
	   ut = sqrt(xnum(1)**2 + xnum(2)**2)
	   u = xnum(1)/ut
	   v = xnum(2)/ut
	else	
	   u = cos(xnum(1))
	   v = sin(xnum(1))
	endif
	do i=1,nop,2
	   temp = r(i)*u + r(i+1)*v
	   r(i+1) = r(i+1)*u - r(i)*v
 	   r(i) = temp
	end do
	return

c complex "real" 
c ************
c Keep only the real part of a complex array in r.
c Input and output in r

2000	do i=1,nopp
	   r(i) = r(2*i-1)
	end do
	goto 2100

c complex "imag" 
c ************
c Keep only the imag part of a complex array in r.
c Input and output in r

3000	do  i=1,nopp
	   r(i) = r(2*i)
	end do
	go to 2100

c complex "amp" 
c ************
c Form the amplitude of a complex array in r.
c Input and output in r

4000	do i=1,nopp
	   r(i) = sqrt( r(2*i-1) * r(2*i-1) + r(2*i) * r(2*i) )
	end do
	go to 2100


c complex "phase" 
c ************
c Form the phase of a complex array in r.
c Input and output in r

4500	do i=1,nopp
	   if ( r(2*i-1).ne.0.0 .or. r(2*i).ne.0.0 ) then
	      r(i) = atan2( r(2*i), r(2*i-1) ) / twopi
	   else
	      r(i) = 0.0
	   end if
	end do
	go to 2100


c complex "COMPEXP" 
c ***************
c change phase in upper half of nop pts to complex exponential
c and leave the result in r

 5000	do i=1,nopp
	   y = r(i+nopp)*twopi
	   r(2*i-1) = cos(y)
	   r(2*i)   = sin(y)
	end do
	r(1) = 1.0
	r(2) = 0.0
	r(nop+1) = 1.0
	r(nop+2) = 0.0
	nwpp = 2        ! data is now complex
	return

c complex "spiral" N n0
c ********************************
c rotate the array by exp{i 2pi*(n-n0)/N)}
c Input and output in r

6000	if (ifx .ne. 2) go to 998
	n  = inum(1) * nwpp   ! n and n0 refer to comlex points
	n0 = inum(2) * nwpp
	do i=1,nop,2
	   arg = twopi*(i - n0)/n
	   u = cos(arg)
	   v = sin(arg)
	   temp   = r(i)   * u + r(i+1) * v
	   r(i+1) = r(i+1) * u - r(i)   * v
	   r(i) = temp
	end do
	return


* complex conjug
* *****************
* complex conjugate of data in the r array
*
 7000	continue
	if ( nwpp .ne. 2 ) then
	   call wrtstr(' Error :  data in r array are not complex.')
	   return
	end if
	do i=1,nopp
	   r(2*i) = -r(2*i)
	end do
	return


* complex add
* ***********
* cmplx <real> <imag>
* add a complex constant to complex data in the r array	
 8000	continue
	if ( nwpp .ne. 2 ) then
	   call wrtstr(' Error :  data in r array are not complex.')
	   return
	end if
	if ( ifl .ne. 2 ) then
	   call wrtstr(' Error :  imaginary part of operand missing.')
	   return
	end if
	do i=1,nopp
	   r(2*i-1) = r(2*i-1) + xnum(1)
	   r(2*i)   = r(2*i)   + xnum(2)
	end do
	return


* complex combine
* ***************
* cmplx combine
* complex vector addition of data in the r array
 9000	continue
	if ( nwpp .ne. 2 ) then
	   call wrtstr(' Error :  data in r array are not complex.')
	   return
	end if
	do i=1,nop
	   r(i) = r(i) + tr(i)
	end do
	return

	
* complex multiply
* ****************
* cmplx multiply <real> <imag>
* multiply a complex array in the r array by a complex factor
10000	continue
	if ( nwpp .ne. 2 ) then
	   call wrtstr(' Error :  data in r array are not complex.')
	   return
	end if
	if ( ifl .ne. 2 ) then
	   call wrtstr(' Error :  imaginary part of operand missing.')
	   return
	end if
	do i=1,nopp
	   rtmp     = r(2*i-1)
	   r(2*i-1) = r(2*i-1) * xnum(1) - r(2*i) * xnum(2)
	   r(2*i)   = rtmp     * xnum(2) + r(2*i) * xnum(1)
	end do
	return


* complex product
* ***************
* cmplx product
* compute the vector product of two complex arrays in r and tr
* and put the result in r
11000	continue
	if ( nwpp .ne. 2 ) then
	   call wrtstr(' Error :  data in r array are not complex.')
	   return
	end if
	do i=1,nop,2
            rtmp = r(i)*tr(i) - r(i+1)*tr(i+1)
            r(i+1) = r(i+1)*tr(i) + r(i)*tr(i+1)
            r(i) = rtmp
	end do
	return


* complex ratio
* *************
* compute the ratio of two complex arrays in r and tr and
* put the result in r
12000	continue
	if ( nwpp .ne. 2 ) then
	   call wrtstr(' Error :  data in r array are not complex.')
	   return
	end if
	do i=1,nop,2
            u = r(i)*tr(i) + r(i+1)*tr(i+1)
            v = r(i+1)*tr(i) - r(i)*tr(i+1)
            ut = tr(i)*tr(i) + tr(i+1)*tr(i+1)
          if (ut .ne. 0.0) then
            r(i+1) = v/ut
            r(i) = u/ut
          else
            r(i+1) = 0.0
            r(i) = 0.0
          endif
        enddo
	return


* complex one
* ***********
* cmplx one [<npts>]
* fill the complex r array with <npts> real numbers.
13000	continue
	if ( ifx.eq.0 .or. ifx.gt.nrtr/2) then
	   nop = nrtr
	else
	   nop = 2 * inum(1)
	end if
	do i=1,nop/2             ! nwpp is 1 here
	   r(2*i-1) = 1.0
	   r(2*i)   = 0.0
	end do
	nwpp = 2
	delw = 1.0
	call disper
	return


* complex complex
* combine real vectors in the r and tr array into a complex vector in the r array
14000	continue
	if ( nwpp .ne. 1 ) then
	   call wrtstr(' Error :  data in r array is already complex.')
	   return
	end if
	do i=nop,1,-1
	   r(2*i)   = tr(i)
	   r(2*i-1) = r(i)
	end do
	nwpp = 2
	nop = nwpp * nop
	return


2100	nop = nop/2     ! adjust number of points
	nwpp = 1        ! data is now real
	call disper
	return

	end
