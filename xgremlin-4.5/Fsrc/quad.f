      subroutine quad (A, B, RESULT, K, EPSIL, NPTS, ICHECK, F)
c
c     This quadrature program uses formulae due to T. N. L. Patterson,
c     Mathematics of computation, Volume 22, 1968, pages 847-856, as
c     modified by F. T. Krogh and W. V. Snyder, ACM Transactions on
c     Mathematical Software 17, 4 (December 1991) pp 457-461.  It is a
c     functional replacement for Algorithm 468, T. N. L. Patterson,
c     Communications of the ACM 16, 11 (November 1973) 694-699.
c
c     *****     Formal Arguments     ***********************************
c
c Input:
c  A, B   Lower and upper limits of integration, respectively.
c  EPSIL  Relative accuracy required.  When the relative difference of
c         two successive formulae does not exceed EPSIL the last formula
c         computed is taken as the result.
c  F      A FUNCTION subprogram that evaluates the integrand at a given
c         abscissa.  F is invoked F(X).  F MUST BE MENTIONED IN AN
c         EXTERNAL STATEMENT IN THE CALLING PROGRAM.
c Output:
c  RESULT This array, which should be declared to have at least 8
c         elements, holds the results obtained by the 1, 3, 7, etc.
c         point formulae.  The number of formulae computed depends on
c         EPSIL.
c  K      RESULT(K) holds the value of the integral to the specified
c         relative accuracy.
c  NPTS   Reports the number of integrand evaluations.
c  ICHECK On exit normally ICHECK=0.  However if convergence to the
c         accuracy requested is not achieved ICHECK=1 on exit.
c
      double precision A, B, EPSIL, F
      external F
      double precision RESULT(8)
      integer K, NPTS, ICHECK
c
c     *****     Local Variables     ************************************
c
c ACUM    is the accumulating estimate of the integral.
c DELTA   is B - A.
c DIFF    is 0.5 * DELTA.
c FH, FL  contain subscripts to indicate where to store integrand
c         samples in WORK.
c FNCVAL  is an integrand sample, or a sum of two symmetrically placed
c         integrand samples.
c IP      is a subscript used to index P.
c J       is a subscript and loop inductor.
c J1, J2  are bounds of the indexes at which integrand samples are
c         stored in WORK.
c JH, JL  are bounds for the loop that accumulates new function values
c         into the integral estimate.
c KH, KL  are bounds for indexes at which integrand samples are
c         retrieved from WORK in order to begin applying a quadrature
c         formula.
c KK      is a loop inductor and subscript.
c KX      is a list of bounds of subscripts into KH and KL.  KH is
c         indexed by K.  The set of indexes from which to retrieve
c         integrand samples is given by the set of bounds in KL and KH
c         indexed by KX(K-1)+1 to KX(K) inclusively.
c P       contains the coefficients necessary to the quadrature
c         formulae.  Their organization is described below in the
c         section on DATA statements.
c PACUM   is the previous value of ACUM.
c X       is the distance of the abscissa from the boundary of the
c         region .
c WORK    is space in which to store function values to be used in the
c         next formula.
c
      double precision ACUM
      double precision DELTA, DIFF
      integer FH(2:8), FL(2:8)
      double precision FNCVAL
      integer IP
      integer J, J1, J2, JH, JL
      integer KH(11), KK, KL(11), KX(1:8)
      double precision P(305)
      double precision PACUM
      double precision WORK(17), X
c
c     *****     Data Statements     ************************************
c
      data FL /2, 3, 5, 9,12,14, 1/
      data FH /2, 4, 8,16,17,17, 0/
      data KL    /1, 1, 1, 1, 1, 3, 5, 9, 5, 9,12/
      data KH    /1, 2, 4, 8,16, 3, 6,17, 5, 9,17/
      data KX /0, 1, 2, 3, 4, 5,       8,      11/
c
c     In the comments below, F(K,I) refers to the function value
c     computed for the I'th node of the K'th formula.  The abscissae and
c     weights are stored in order according to the distance from the
c     boundary of the region, not from the center.  Since we store
c     1 - |abscissa|, the first "node" coefficient for each formula is
c     the smallest.
c
c     Corrections, nodes and weights for the 3-point formula.
c
c     Correction for F(1,1).
      data P(  1)/-.11111111111111111111d+00/
c     Node and weight for F(2,1).
      data P(  2)/+.22540333075851662296d+00/
      data P(  3)/+.55555555555555555556d+00/
c
c     Corrections, nodes and weights for the 7-point formula.
c
c     Corrections for F(1,1) and F(2,1).
      data P(  4)/+.647209421402969791d-02  /
      data P(  5)/-.928968790944433705d-02  /
c     Nodes and weights for F(3,1-2)
      data P(  6)/+.39508731291979716579d-01/
      data P(  7)/+.10465622602646726519d+00/
      data P(  8)/+.56575625065319744200d+00/
      data P(  9)/+.40139741477596222291d+00/
c
c     Corrections, nodes and weights for the 15-point formula.
c
c     Corrections for F(1,1), F(2,1), F(3,1-2).
      data P( 10)/+.5223046896961622d-04    /
      data P( 11)/+.17121030961750000d-03   /
      data P( 12)/-.724830016153892898d-03  /
      data P( 13)/-.7017801099209042d-04    /
c     Nodes and weights for F(4,1-4).
      data P( 14)/+.61680367872449777899d-02/
      data P( 15)/+.17001719629940260339d-01/
      data P( 16)/+.11154076712774300110d+00/
      data P( 17)/+.92927195315124537686d-01/
      data P( 18)/+.37889705326277359705d+00/
      data P( 19)/+.17151190913639138079d+00/
      data P( 20)/+.77661331357103311837d+00/
      data P( 21)/+.21915685840158749640d+00/
c
c     Corrections, nodes and weights for the 31-point formula.
c
c     Corrections for F(1,1), F(2,1), F(3,1-2), F(4,1-4).
      data P( 22)/+.682166534792        d-08/
      data P( 23)/+.12667409859336      d-06/
      data P( 24)/+.59565976367837165   d-05/
      data P( 25)/+.1392330106826       d-07/
      data P( 26)/-.6629407564902392    d-04/
      data P( 27)/-.704395804282302     d-06/
      data P( 28)/-.34518205339241      d-07/
      data P( 29)/-.814486910996        d-08/
c     Nodes and weights for F(5,1-8).
      data P( 30)/+.90187503233240234038d-03/
      data P( 31)/+.25447807915618744154d-02/
      data P( 32)/+.18468850446259893130d-01/
      data P( 33)/+.16446049854387810934d-01/
      data P( 34)/+.70345142570259943330d-01/
      data P( 35)/+.35957103307129322097d-01/
      data P( 36)/+.16327406183113126449d+00/
      data P( 37)/+.56979509494123357412d-01/
      data P( 38)/+.29750379350847292139d+00/
      data P( 39)/+.76879620499003531043d-01/
      data P( 40)/+.46868025635562437602d+00/
      data P( 41)/+.93627109981264473617d-01/
      data P( 42)/+.66886460674202316691d+00/
      data P( 43)/+.10566989358023480974d+00/
      data P( 44)/+.88751105686681337425d+00/
      data P( 45)/+.11195687302095345688d+00/
c
c     Corrections, nodes and weights for the 63-point formula.
c
c     Corrections for F(1,1), F(2,1), F(3,1-2), F(4,1-4), F(5,1-8).
      data P( 46)/+.371583              d-15/
      data P( 47)/+.21237877            d-12/
      data P( 48)/+.10522629388435      d-08/
      data P( 49)/+.1748029             d-14/
      data P( 50)/+.3475718983017160    d-06/
      data P( 51)/+.90312761725         d-11/
      data P( 52)/+.12558916            d-13/
      data P( 53)/+.54591               d-15/
      data P( 54)/-.72338395508691963   d-05/
      data P( 55)/-.169699579757977     d-07/
      data P( 56)/-.854363907155        d-10/
      data P( 57)/-.12281300930         d-11/
      data P( 58)/-.462334825           d-13/
      data P( 59)/-.42244055            d-14/
      data P( 60)/-.88501               d-15/
      data P( 61)/-.40904               d-15/
c     Nodes and weights for F(6,1-16).
      data P( 62)/+.12711187964238806027d-03/
      data P( 63)/+.36322148184553065969d-03/
      data P( 64)/+.27937406277780409196d-02/
      data P( 65)/+.25790497946856882724d-02/
      data P( 66)/+.11315242452570520059d-01/
      data P( 67)/+.61155068221172463397d-02/
      data P( 68)/+.27817125251418203419d-01/
      data P( 69)/+.10498246909621321898d-01/
      data P( 70)/+.53657141626597094849d-01/
      data P( 71)/+.15406750466559497802d-01/
      data P( 72)/+.89628843042995707499d-01/
      data P( 73)/+.20594233915912711149d-01/
      data P( 74)/+.13609206180630952284d+00/
      data P( 75)/+.25869679327214746911d-01/
      data P( 76)/+.19305946804978238813d+00/
      data P( 77)/+.31073551111687964880d-01/
      data P( 78)/+.26024395564730524132d+00/
      data P( 79)/+.36064432780782572640d-01/
      data P( 80)/+.33709033997521940454d+00/
      data P( 81)/+.40715510116944318934d-01/
      data P( 82)/+.42280428994795418516d+00/
      data P( 83)/+.44914531653632197414d-01/
      data P( 84)/+.51638197305415897244d+00/
      data P( 85)/+.48564330406673198716d-01/
      data P( 86)/+.61664067580126965307d+00/
      data P( 87)/+.51583253952048458777d-01/
      data P( 88)/+.72225017797817568492d+00/
      data P( 89)/+.53905499335266063927d-01/
      data P( 90)/+.83176474844779253501d+00/
      data P( 91)/+.55481404356559363988d-01/
      data P( 92)/+.94365568695340721002d+00/
      data P( 93)/+.56277699831254301273d-01/
c
c     Corrections, nodes and weights for the 127-point formula.
c
c     Corrections for F(3,1), F(4,1-2), F(5,1-3), F(6,1-6).
      data P( 94)/+.1041098             d-15/
      data P( 95)/+.249472054598        d-10/
      data P( 96)/+.55                  d-20/
      data P( 97)/+.290412475995385     d-07/
      data P( 98)/+.367282126           d-13/
      data P( 99)/+.5568                d-18/
      data P(100)/-.871176477376972025  d-06/
      data P(101)/-.8147324267441       d-09/
      data P(102)/-.8830920337          d-12/
      data P(103)/-.18018239            d-14/
      data P(104)/-.70528               d-17/
      data P(105)/-.506                 d-19/
c     Nodes and weights for F(7,1-32).
      data P(106)/+.17569645108401419961d-04/
      data P(107)/+.50536095207862517625d-04/
      data P(108)/+.40120032808931675009d-03/
      data P(109)/+.37774664632698466027d-03/
      data P(110)/+.16833646815926074696d-02/
      data P(111)/+.93836984854238150079d-03/
      data P(112)/+.42758953015928114900d-02/
      data P(113)/+.16811428654214699063d-02/
      data P(114)/+.85042788218938676006d-02/
      data P(115)/+.25687649437940203731d-02/
      data P(116)/+.14628500401479628890d-01/
      data P(117)/+.35728927835172996494d-02/
      data P(118)/+.22858485360294285840d-01/
      data P(119)/+.46710503721143217474d-02/
      data P(120)/+.33362148441583432910d-01/
      data P(121)/+.58434498758356395076d-02/
      data P(122)/+.46269993574238863589d-01/
      data P(123)/+.70724899954335554680d-02/
      data P(124)/+.61679602220407116350d-01/
      data P(125)/+.83428387539681577056d-02/
      data P(126)/+.79659974529987579270d-01/
      data P(127)/+.96411777297025366953d-02/
      data P(128)/+.10025510022305996335d+00/
      data P(129)/+.10955733387837901648d-01/
      data P(130)/+.12348658551529473026d+00/
      data P(131)/+.12275830560082770087d-01/
      data P(132)/+.14935550523164972024d+00/
      data P(133)/+.13591571009765546790d-01/
      data P(134)/+.17784374563501959262d+00/
      data P(135)/+.14893641664815182035d-01/
      data P(136)/+.20891506620015163857d+00/
      data P(137)/+.16173218729577719942d-01/
      data P(138)/+.24251603361948636206d+00/
      data P(139)/+.17421930159464173747d-01/
      data P(140)/+.27857691462990108452d+00/
      data P(141)/+.18631848256138790186d-01/
      data P(142)/+.31701256890892077191d+00/
      data P(143)/+.19795495048097499488d-01/
      data P(144)/+.35772335749024048622d+00/
      data P(145)/+.20905851445812023852d-01/
      data P(146)/+.40059606975775710702d+00/
      data P(147)/+.21956366305317824939d-01/
      data P(148)/+.44550486736806745112d+00/
      data P(149)/+.22940964229387748761d-01/
      data P(150)/+.49231224246628339785d+00/
      data P(151)/+.23854052106038540080d-01/
      data P(152)/+.54086998801016766712d+00/
      data P(153)/+.24690524744487676909d-01/
      data P(154)/+.59102017877011132759d+00/
      data P(155)/+.25445769965464765813d-01/
      data P(156)/+.64259616216846784762d+00/
      data P(157)/+.26115673376706097680d-01/
      data P(158)/+.69542355844328595666d+00/
      data P(159)/+.26696622927450359906d-01/
      data P(160)/+.74932126969651682339d+00/
      data P(161)/+.27185513229624791819d-01/
      data P(162)/+.80410249728889984607d+00/
      data P(163)/+.27579749566481873035e-01/
      data P(164)/+.85957576684743982540d+00/
      data P(165)/+.27877251476613701609d-01/
      data P(166)/+.91554595991628911629d+00/
      data P(167)/+.28076455793817246607d-01/
      data P(168)/+.97181535105025430566d+00/
      data P(169)/+.28176319033016602131d-01/
c
c     Corrections, nodes and weights for the 255-point formula.
c
c     Corrections for F(4,1), F(5,1), F(6,1-2), F(7,1-4).
      data P(170)/+.3326                d-18/
      data P(171)/+.114094770478        d-11/
      data P(172)/+.2952436056970351    d-08/
      data P(173)/+.51608328            d-15/
      data P(174)/-.110177219650597323  d-06/
      data P(175)/-.58656987416475      d-10/
      data P(176)/-.23340340645         d-13/
      data P(177)/-.1248950             d-16/
c     Nodes and weights for F(8,1-64).
      data P(178)/+.24036202515353807630d-05/
      data P(179)/+.69379364324108267170d-05/
      data P(180)/+.56003792945624240417d-04/
      data P(181)/+.53275293669780613125d-04/
      data P(182)/+.23950907556795267013d-03/
      data P(183)/+.13575491094922871973d-03/
      data P(184)/+.61966197497641806982d-03/
      data P(185)/+.24921240048299729402d-03/
      data P(186)/+.12543855319048853002d-02/
      data P(187)/+.38974528447328229322d-03/
      data P(188)/+.21946455040427254399d-02/
      data P(189)/+.55429531493037471492d-03/
      data P(190)/+.34858540851097261500d-02/
      data P(191)/+.74028280424450333046d-03/
      data P(192)/+.51684971993789994803d-02/
      data P(193)/+.94536151685852538246d-03/
      data P(194)/+.72786557172113846706d-02/
      data P(195)/+.11674841174299594077d-02/
      data P(196)/+.98486295992298408193d-02/
      data P(197)/+.14049079956551446427d-02/
      data P(198)/+.12907472045965932809d-01/
      data P(199)/+.16561127281544526052d-02/
      data P(200)/+.16481342421367271240d-01/
      data P(201)/+.19197129710138724125d-02/
      data P(202)/+.20593718329137316189d-01/
      data P(203)/+.21944069253638388388d-02/
      data P(204)/+.25265540247597332240d-01/
      data P(205)/+.24789582266575679307d-02/
      data P(206)/+.30515340497540768229d-01/
      data P(207)/+.27721957645934509940d-02/
      data P(208)/+.36359378430187867480d-01/
      data P(209)/+.30730184347025783234d-02/
      data P(210)/+.42811783890139037259d-01/
      data P(211)/+.33803979910869203823d-02/
      data P(212)/+.49884702478705123440d-01/
      data P(213)/+.36933779170256508183d-02/
      data P(214)/+.57588434808916940190d-01/
      data P(215)/+.40110687240750233989d-02/
      data P(216)/+.65931563842274211999d-01/
      data P(217)/+.43326409680929828545d-02/
      data P(218)/+.74921067092924347640d-01/
      data P(219)/+.46573172997568547773d-02/
      data P(220)/+.84562412844234959360d-01/
      data P(221)/+.49843645647655386012d-02/
      data P(222)/+.94859641186738404810d-01/
      data P(223)/+.53130866051870565663d-02/
      data P(224)/+.10581543166444097714d+00/
      data P(225)/+.56428181013844441585d-02/
      data P(226)/+.11743115975265809315d+00/
      data P(227)/+.59729195655081658049d-02/
      data P(228)/+.12970694445188609414d+00/
      data P(229)/+.63027734490857587172d-02/
      data P(230)/+.14264168911376784347d+00/
      data P(231)/+.66317812429018878941d-02/
      data P(232)/+.15623311732729139895d+00/
      data P(233)/+.69593614093904229394d-02/
      data P(234)/+.17047780536259859981d+00/
      data P(235)/+.72849479805538070639d-02/
      data P(236)/+.18537121234486258656d+00/
      data P(237)/+.76079896657190565832d-02/
      data P(238)/+.20090770903915859819d+00/
      data P(239)/+.79279493342948491103d-02/
      data P(240)/+.21708060588171698360d+00/
      data P(241)/+.82443037630328680306d-02/
      data P(242)/+.23388218069623990928d+00/
      data P(243)/+.85565435613076896192d-02/
      data P(244)/+.25130370638306339718d+00/
      data P(245)/+.88641732094824942641d-02/
      data P(246)/+.26933547875781873867d+00/
      data P(247)/+.91667111635607884067d-02/
      data P(248)/+.28796684463774796540d+00/
      data P(249)/+.94636899938300652943d-02/
      data P(250)/+.30718623022088529711d+00/
      data P(251)/+.97546565363174114611d-02/
      data P(252)/+.32698116976958152079d+00/
      data P(253)/+.10039172044056840798d-01/
      data P(254)/+.34733833458998250389d+00/
      data P(255)/+.10316812330947621682d-01/
      data P(256)/+.36824356228880576959d+00/
      data P(257)/+.10587167904885197931d-01/
      data P(258)/+.38968188628481359983d+00/
      data P(259)/+.10849844089337314099d-01/
      data P(260)/+.41163756555233745857d+00/
      data P(261)/+.11104461134006926537d-01/
      data P(262)/+.43409411457634557737d+00/
      data P(263)/+.11350654315980596602d-01/
      data P(264)/+.45703433350168850951d+00/
      data P(265)/+.11588074033043952568d-01/
      data P(266)/+.48044033846254297801d+00/
      data P(267)/+.11816385890830235763d-01/
      data P(268)/+.50429359208123853983d+00/
      data P(269)/+.12035270785279562630d-01/
      data P(270)/+.52857493412834112307d+00/
      data P(271)/+.12244424981611985899d-01/
      data P(272)/+.55326461233797152625d+00/
      data P(273)/+.12443560190714035263d-01/
      data P(274)/+.57834231337383669993d+00/
      data P(275)/+.12632403643542078765d-01/
      data P(276)/+.60378719394238406082d+00/
      data P(277)/+.12810698163877361967d-01/
      data P(278)/+.62957791204992176986d+00/
      data P(279)/+.12978202239537399286d-01/
      data P(280)/+.65569265840056197721d+00/
      data P(281)/+.13134690091960152836d-01/
      data P(282)/+.68210918793152331682d+00/
      data P(283)/+.13279951743930530650d-01/
      data P(284)/+.70880485148175331803d+00/
      data P(285)/+.13413793085110098513d-01/
      data P(286)/+.73575662758907323806d+00/
      data P(287)/+.13536035934956213614d-01/
      data P(288)/+.76294115441017027278d+00/
      data P(289)/+.13646518102571291428d-01/
      data P(290)/+.79033476175681880523d+00/
      data P(291)/+.13745093443001896632d-01/
      data P(292)/+.81791350324074780175d+00/
      data P(293)/+.13831631909506428676d-01/
      data P(294)/+.84565318851862189130d+00/
      data P(295)/+.13906019601325461264d-01/
      data P(296)/+.87352941562769803314d+00/
      data P(297)/+.13968158806516938516d-01/
      data P(298)/+.90151760340188079791d+00/
      data P(299)/+.14017968039456608810d-01/
      data P(300)/+.92959302395714482093d+00/
      data P(301)/+.14055382072649964277d-01/
      data P(302)/+.95773083523463639678d+00/
      data P(303)/+.14080351962553661325d-01/
      data P(304)/+.98590611358921753738d+00/
      data P(305)/+.14092845069160408355d-01/
c
c     *****     Executable Statements     ******************************
c
      icheck=0
      delta=b-a
      diff=0.5*delta
      ip=1
      jh=0
c
c     Apply 1-point Gauss formula (Midpoint rule).
c
      fncval=f(a+diff)
c     Don't write "0.5*(b+a)" above if the radix of arithmetic isn't 2.
      npts=1
      work(1)=fncval
      acum=fncval*delta
      result(1)=acum
c
      do 40 k = 2, 8
c
c       Go on to the next formula.
c
        pacum=acum
        acum=0.0
c
c       Compute contribution to current estimate due to function
c       values used in previous formulae.
c
        do 20 kk = kx(k-1)+1, kx(k)
          do 10 j = kl(kk), kh(kk)
            acum=acum+dble(p(ip)*work(j))
            ip=ip+1
10        continue
20      continue
c
c       Compute contribution from new function values.
c
        jl=jh+1
        jh=jl+jl-1
        j1=fl(k)
        j2=fh(k)
        do 30 j = jl, jh
          x=p(ip)*diff
          fncval=f(a+x)+f(b-x)
          npts=npts+2
          acum=acum+dble(p(ip+1)*fncval)
          if (j1.le.j2) then
            work(j1)=fncval
            j1=j1+1
          end if
          ip=ip+2
30      continue
        acum=dble(diff)*acum+0.5d0*pacum
        result(k)=acum
        if (abs(result(k)-result(k-1)).le.abs(epsil*result(k))) go to 50
40    continue
      icheck=1
      k=8
50    return
      end
