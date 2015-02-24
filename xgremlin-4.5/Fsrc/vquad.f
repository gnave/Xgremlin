      double precision function vquad( pos, fwhm, gwid, dmp )
*
* calculates the integral of a Voigt profile numerically
*
* pos  : position of the peak of the profile
* fwhm : FWHM of the profile
* gwid : Gaussian width
* dmp  : damping
*
      double precision epswing, epsquad
      parameter (epswing = 1.0d-3, epsquad = 5.0d-2)   ! accuracies

      double precision pos, fwhm, gwid, dmp
      double precision pk, t, a, b, v
      double precision result(8)
      double precision vprof
      double precision vq_gwid, vq_dmp, vq_pos
      double precision dvoigt
      integer k, npts, icheck

      common /vq/ vq_gwid, vq_dmp, vq_pos    ! communicate parameters to vprof

      external vprof,dvoigt

* first find out how far out we have to integrate
      pk = dvoigt( pos, pos, gwid, dmp )
      t = pos
 10   t = t + fwhm
      v = dvoigt(t,pos,gwid,dmp) / pk
      if ( v .gt. epswing ) goto 10
      a = pos - t
      b = pos + t

* calculate the line integral
      vq_pos  = pos
      vq_gwid = gwid
      vq_dmp  = dmp
      call quad( a, b, result, k, epsquad, npts, icheck, vprof )

      vquad = result(k)

      return
      end

*************

      double precision function vprof(x)
*
* calculate Voigt profile for QUAD
*
      double precision x
      double precision vq_gwid, vq_dmp, vq_pos
      double precision dvoigt

      common /vq/ vq_gwid, vq_dmp, vq_pos    ! communicate parameters to vprof

      vprof = dvoigt(x, vq_pos, vq_gwid, vq_dmp)

      return
      end

* Revision history:
* -----------------
* $Log: vquad.f,v $
* Revision 1.2  1996/06/25 02:12:15  ulf
* fix comment
*
* Revision 1.1  1996/06/25 01:54:08  ulf
* Initial revision
*
