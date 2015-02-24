* -*-Fortran-*-
*
* global data for phase correction
*
      integer nphmax
      parameter (nphmax = 512)

      logical l_phase, l_bad
      integer nphpts, l_pzoom
      real ph_top, ph_bot, ph_std

      integer noppl, nopph, phctyp
      integer ph_pt(nphmax), ipgood(nphmax)
      real ph_ob(nphmax), ph_dif(nphmax), ph_w(nphmax)
      double precision delwpl, delwph

      common /phasedata/ delwpl, delwph, noppl, nopph, phctyp
      common /phasedata/ ph_ob, ph_dif,ph_w,ph_top,ph_bot,ph_std
      common /phasedata/ l_pzoom, ipgood, ph_pt, nphpts
      common /phasedata/ l_phase, l_bad

*--------------------------------------------------------------------
* noppl      : number of points in spectrum
* nopph      : number of points in phase curve
* phctyp     : phase curve type
* nphmax     : max number of phase points
* ph_top     : top of phase world
* ph_bot     : bottom of phase world
* nphpts     : number of good phase points read from phase data file
* l_phase    : .true. if phase plot mode is active
* l_pzoom    : zoom buttons act on phase plot, not data if .true.
* l_bad      : .true. if unwritten bad phase points are in buffer
* ph_pt      : point in r array where phase was determined
* ph_r       : corresponding value of r array
* ph_ob      : observed phase
* ph_dif     : observed phase - calc phase
* ph_w       : wavenumber of phase points
* ph_sdv     : standard deviation of phase fit
* ipgood     : the good phase points

