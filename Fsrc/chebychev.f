*-------------------------------------------------------------------------------
* Several subroutines needed in Chebychev approximation
*-------------------------------------------------------------------------------

      subroutine chepol(x,t,m)
*
* calculates the Chebychev polynomials from their recursion formula
*
      integer i,m
      real t(*),x,y,bpa,bma
      common /border/bma,bpa
      y = (x - bpa) / bma
      t(1) = 1.0
      t(2) = y
      do i=3,m
         t(i) = 2.0 * y * t(i-1) - t(i-2)
      end do
      return
      end

*-------------------------------------------------------------------------------

      real function chebev(c,m,x)
*
* evaluate a Chebychev polynomial
*
      integer m
      real x,c(m)
      integer j
      real bpa, bma
      double precision d,dd,sv,y,y2
      common /border/bma,bpa

      d=0.d0
      dd=0.d0
      y=dble(x-bpa)/dble(bma)
      y2=2.d0*y
      do j=m,2,-1
        sv=d
        d=y2*d-dd+dble(c(j))
        dd=sv
      end do
      chebev=real( y*d-dd+dble(c(1)) )
      return
      end

*-------------------------------------------------------------------------------

      subroutine chebpc(c,d,n)
*
* calculate polynomial coefficients from Chebychev coefficients
*
      include 'integrate.h'
      include 'chebychev.h'

      integer n
      real c(n),d(n)
      integer j,k
      real sv,dd(ncoef)
      do  j=1,n
         d(j)=0.0
         dd(j)=0.0
      end do
      d(1)=c(n)
      do j=n-1,2,-1
         do k=n-j+1,2,-1
            sv=d(k)
            d(k)=2.0 * d(k-1) - dd(k)
            dd(k)=sv
         end do
         sv=d(1)
         d(1)=-dd(1) + c(j)
         dd(1)=sv
      end do
      do j=n,2,-1
         d(j)=d(j-1) - dd(j)
      end do
      d(1)=-dd(1) + 0.5 * c(1)
      return
      end

*-------------------------------------------------------------------------------

      subroutine pcshft(d,n)
*
* transform polynomial coefficients from interval [-1,1] --> [a,b]
*
      integer n
      real d(n)
      integer j,k
      real const,fac,bma,bpa
      common /border/bma,bpa

      const=1.0/bma            ! bma = 0.5 * (b - a)
      fac=const                ! bpa = 0.5 * (b + a)
      do j=2,n
         d(j)=d(j)*fac
         fac=fac*const
      end do
      const=bpa            
      do j=1,n-1
         do k=n-1,j,-1
            d(k)=d(k)-const*d(k+1)
         end do
      end do
      return
      end

*-------------------------------------------------------------------------------

      subroutine cheder(c,cder,n)
*
* calculate derivative of Chebychev polynomial
*
      integer n
      real bma,bpa,c(n),cder(n)
      integer j
      real con
      common /border/bma,bpa
      cder(n)=0.
      cder(n-1)=2*(n-1)*c(n)
      if ( n.ge.3 ) then
         do j=n-2,1,-1
            cder(j)=cder(j+2)+2*j*c(j+1)
         end do
      endif
      con=2.0/bma
      do j=1,n
        cder(j)=cder(j)*con
      end do
      return
      end


