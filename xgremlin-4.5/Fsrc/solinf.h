c \grammy\src\solinf.inc

c Revision 1.01 08/03/93  Revised for type6
c Revision 1.00 31/08/91  Creation

        double precision hastrt,hastop,dcmean,zenstrt,zenstop
        double precision s8dum1,s8dum2

        common /solr8/ hastrt,hastop,dcmean,zenstrt,zenstop
        common /solr8/ s8dum1,s8dum2


        real           patm, tatm, hatm
        real           s4dum1, s4dum2, s4dum3, s4dum4
        real           vrel, amstrt,amstop,vcstrt,vcstop
        real           sclim,dimage,soffst,eoffst,solmu
 
        common /solr4/ patm, tatm, hatm
        common /solr4/ s4dum1, s4dum2, s4dum3, s4dum4
        common /solr4/ vrel, amstrt,amstop,vcstrt,vcstop
        common /solr4/ sclim,dimage,soffst,eoffst,solmu
 
 
        integer      hetpm1, hetpm2, julday, isdum1
        integer      isdum2,isdum3,isdum4,isdum5
        integer      lfcutApB, hfcutApB, lfcutAmB, hfcutAmB
        integer      isdum6,isdum7
        integer      gainApB, gainAmB

        common /soli4/ hetpm1, hetpm2, julday, isdum1
        common /soli4/ isdum2,isdum3,isdum4,isdum5
        common /soli4/ lfcutApB, hfcutApB, lfcutAmB, hfcutAmB
        common /soli4/ isdum6, isdum7
        common /soli4/ gainApB, gainAmB

 
        character*128   utstrt, utstop, ststrt, ststop
        common /solch/ utstrt, utstop, ststrt, ststop
