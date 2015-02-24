c \grammy\src\extinf.inc

	real apang,apsep,bandlo,bandhi
	real flin,flout,realmax,setval1,setval2

	integer gainBadj,setlo1,setlo2,sethi1,sethi2
	integer maxintgr,modephzc,origtype

        character*128 data_is,input_is,ratio_is,scan_is,xaxis_is
        character*128 acqvers,aircorr,archname,dblpass
	character*128 decreq,decobs,ftsmode,predisp
	character*128 rareq,raobs,spectype,telescop,timestr,unknown
        character*128 comment0,comment1,comment2,comment3,comment4
        character*128 comment5,comment6,comment7,comment8,comment9


	common/extinf/ apang,apsep,bandlo,bandhi
	common/extinf/ flin,flout,realmax,setval1,setval2

	common/extinf/ gainBadj,setlo1,sethi1,setlo2,sethi2
	common/extinf/ maxintgr,modephzc,origtype

	common/extinf/ data_is,input_is,ratio_is,scan_is,xaxis_is
	common/extinf/ acqvers,aircorr,archname,dblpass
	common/extinf/ decreq, decobs,ftsmode,predisp
	common/extinf/ rareq, raobs, spectype,telescop,timestr,unknown
        common/extinf/ comment0,comment1,comment2,comment3,comment4
        common/extinf/ comment5,comment6,comment7,comment8,comment9

