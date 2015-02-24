        integer button1, button2, button3
        parameter ( button1=0, button2=1, button3=2 )
 
	real w_left, w_right, w_top, w_bottom
	real w_gl_min, w_gl_max
	integer n_left, n_right, n_pts, nbin, monodsp
	character p_mode*1
	logical l_resc
	common /plotdata/ w_left, w_right, w_top, w_bottom
	common /plotdata/ w_gl_min, w_gl_max
	common /plotdata/ n_left, n_right, n_pts, nbin, monodsp
        common /plotdata/ l_resc, p_mode

*-----------------------------------------------------------------------
* w_left     : left side of the world (coordinates)
* w_right    : right side of the world
* w_top      : top of the world
* w_bottom   : bottom of the world
* w_gl_min   : global minimum of data in r array
* w_gl_max   : global maximum of data in r array
* n_left     : left point of currently plotted window into r array
* n_right    : right point of currently plotted window in r array
* n_pts      : number of points in the currently plotted data window
* nbin       : number of points in a bin when binning is used
* monodsp    : integer, = 1 if the display is a monochrome display, otherw. 0 
* p_mode     : normal/real/complex plotting mode
* l_resc     : re-scale data window before plotting?
*-----------------------------------------------------------------------

