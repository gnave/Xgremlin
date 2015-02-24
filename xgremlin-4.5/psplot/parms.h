c parms
c Revision 1.02  26 Nov 89: basedec added
c Revision 1.01  before 13 Nov 89: fntsav added; 64 in font tables

	common /dcparm/ xpara, xparb, xparc, xnum(20) 
	common /dcparm/ ipara, iparb, iparc, inum(20)
	common /dcparm/ ifl, ifx, nstrng, nalpha(3)
	common /dcparm/ nact,nc,kl
	common /dcparm/ keybd,batch

	double precision xpara, xparb, xparc, xnum 
	integer ipara, iparb, iparc, inum
	integer ifl, ifx, nstrng, nalpha
	integer nact, nc, kl
	logical keybd,batch
 
	common /datacr/ kin, lp, alpha(3), iac, filnam
	character kin*127, lp*127, alpha*132, iac*20, filnam*80

        common/plotlog/ wavlen, cmlabel, rtscale, axlabl, aylabl
        common/plotlog/ lbl, boxflg
        common/plotlog/ frstim, horz, axis, notick, zeroline,oneline
        common/plotlog/ nodata, grid, locat, atls, noynum, noxnum
        common/plotlog/ limits, epsfl, psfl, ltick, rtick, logfl
        common/plotlog/ nanomet, verbos

        logical wavlen, cmlabel, rtscale, axlabl, aylabl, lbl, boxflg
        logical frstim, horz, axis, notick, zeroline,oneline
        logical nodata, grid, locat, atls, noynum, noxnum,limits
        logical epsfl, psfl, ltick, rtick, logfl, nanomet, verbos

        common/plotpar/ gx1,gx2,gy1,gy2
        common/plotpar/ inx, iny, ink, jus1, jus2
        common/plotpar/ ixl, ixr, ibot, itop, ilw
        common/fonts/ fnt, fntsav, fntsiz, iszsav

        real gx1, gx2, gy1, gy2
        integer inx, iny, ink, jus1, jus2
        integer ixl, ixr, ibot, itop, ilw
        integer fnt, fntsav, fntsiz, iszsav
 
        common /label/ nlb(200),lbx(200),lby(200),lfont(200)
	common /label/ ijust(200),islt(200),idlx(200),idly(200)
        common /label/ isiz(200),labl(200), xlabl, ylabl
        common /label/ nlabl, ncxl,ncyl

        integer nlb, lbx, lby, lfont
        integer ijust, islt, idlx, idly, isiz
        character*132 labl
        character*80 xlabl, ylabl
	integer nlabl, ncxl, ncyl

	common /iounit/ input,output,outfile,stdin,stdout,infile
        common /iounit/ spool,com,datain,dataot,cardin,stderr
        common /iounit/ uopen(30) 
        common /iochar/ fnames(30), infil, outfil, confil
        common /iochar/ iret
 
	integer input, output, outfile, stdin, stdout, infile 
        integer spool,com,datain, dataot, cardin, stderr
        character*256 fnames, infil, outfil, confil
        integer uopen
        integer iret
 
	common /ticks/ stx, btx, ltx, sty, bty, lty
        common /ticks/ idec,idec0

        real stx, btx, ltx, sty, bty, lty
        integer idec,idec0

	common /set/  xreg(10)
	common /set/  ireg(10)
 
	double precision xreg
	integer ireg

        common /lines/ dx1(64),dy1(64),dx2(64),dy2(64),dlw(64),dlt(64)
        common /lines/ xl(64),yl(64),draw, connect
        common /lines/ nline

        integer dx1,dy1,dx2,dy2,dlw,dlt
        logical xl,yl,draw, connect
        integer nline

        common /data/ sigma, dsig, xras, yras
        common /data/ col(12,8192), xcol(8193), ycol(8193), ylow, yhigh
        common /data/ xscal, yscal

        double precision sigma, dsig
        real xras, yras, col, xcol, ycol, ylow, yhigh, xscal, yscal

	common /data1/ maxpts, ncol, nop, iprint, ixcol, iycol
	common /data1/ ixc(8193), iyc(8193), dotfnt, ndot
        common /data1/ ndat,dotsz
	common /data1/ format, filenam
	common /data1/ dt

	integer maxpts, ncol, nop, iprint, ixcol, iycol
        integer ixc, iyc, dotfnt, ndot
        integer ndat,dotsz
	character*256 format, filenam
        character*1 dt

	common /data2/ xxx(512),yyy(512),a(11),chisqr,xf,yf
        common /data2/ ixxx(3100),iyyy(3100), nppts, mode, nterms
        common /data2/ ytsav

	double precision xxx,yyy,a,chisqr,xf,yf
        integer ixxx,iyyy, nppts, mode, nterms, ytsav
