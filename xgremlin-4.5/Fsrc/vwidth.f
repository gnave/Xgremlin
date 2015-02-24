      double precision function vwidth( y )
*
* uses the approximation of Olivero and Longbothum to calculate the
* total width (FWHM) of the Voigt function from the Gaussian and
* Lorentzian widths. The returned Voigt width is accurate to 
* within 0.01%
*
* y : damping  = sqrt(ln 2) * Lorentz width / Gauss width
*
* Reference:
* J J Olivero and R L Longbothum, 
* Journal of Quant. Spectr. and Radiative Transfer 17, 233 (1977)
*
* Ulf Griesmann,  June 22, 1996
*
      double precision y, R, x, t

      double precision srln2
      parameter (srln2 = 0.83255461115769769d0)

      R(x) = 1.d0 - 0.1821d0 * ( 1.d0 - x**2 ) +
     &       (0.023665d0 * exp(0.6d0*x) + 0.00418d0 * exp(-1.9d0*x)) *
     &       sin(3.1415926535898d0 * x)

      t = ( y - srln2 ) / ( y + srln2 )
      vwidth = ( y + srln2 ) * R(t)

      return
      end

      
