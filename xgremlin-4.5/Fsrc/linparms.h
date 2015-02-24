
* modified for Xgremlin

	integer linrec_size               ! size of a line record
	integer lsqfit_size
	parameter (linrec_size = 80, lsqfit_size = 1000)

        real vt(50), ds(50), dm(50)
        real xm(lsqfit_size), ypm(lsqfit_size) 
	real tf(lsqfit_size), tfd(lsqfit_size)
        integer nlins, nol
        character idcard*80

        common/linfit/ vt, ds, dm
        common/linfit/ xm, ypm, tf, tfd
        common/linfit/ nlins,nol
        common/linfit/ idcard

c       common for communication of .lin file parameters

        integer nlgot,nlfirst,nbfirst,linum,ntop,nbot,lstlin
        common /linpar/ nlgot,nlfirst,nbfirst,linum,ntop,nbot,lstlin

c voigt parameters

	real p(26), bgg(51), xlast
        common /voigtcon/ p, bgg, xlast

