!
!     ...1.........2.........3.........4.........5.........6.........7.........8
      program main
      implicit none
      integer(4),parameter  :: nelx=71
      integer(4)            :: nel
      character(2)          :: sy(nelx)
      real(8)        :: v0(nelx)
      real(8)        :: b0(nelx)
      real(8)        :: b1(nelx)
      real(8)        :: etot(nelx)
      real(8)        :: ecoh_thrs(nelx)
      real(8)        :: v0_thrs(nelx)
      real(8)        :: b0_thrs(nelx)
      real(8)        :: b1_thrs(nelx)
      integer(4)     :: icry
      integer(4)     :: i
      character(64)  :: string
      character(128) :: fmt
!     **************************************************************************
      CALL READPAWDATA(NELX,NEL,SY,ETOT,V0,B0,B1)
      DO I=1,NEL
        CALL WIENDATA(SY(I),ECOH_THRS(I),V0_THRS(I),B0_THRS(I),B1_THRS(I))
        WRITE(*,FMT='(I3,A3,20F20.5)')I,SY(I) &
     &                 ,V0(I),V0_THRS(I) &
     &                 ,B0(I),B0_THRS(I) &
     &                 ,B1(I),B1_THRS(I) 
      ENDDO
      STOP
      END
!
!     ...1.........2.........3.........4.........5.........6.........7.........8
      subroutine readpawdata(nelx,nel,sy,etot,v0,b0,b1)
!     **************************************************************************
      implicit none
      integer(4)  ,intent(in) :: nelx
      integer(4)  ,intent(out):: nel
      character(2),intent(out):: sy(nelx)
      real(8)     ,intent(out):: etot(nelx)
      real(8)     ,intent(out):: v0(nelx)
      real(8)     ,intent(out):: b0(nelx)
      real(8)     ,intent(out):: b1(nelx)
      integer(4)  ,parameter  :: nfil=100
      character(32)           :: file='readpaw.in'
      integer(4)              :: i
!     **************************************************************************
      open(nfil,file=file)
      do i=1,nelx 
        read(nfil,*,end=1000)sy(i),etot(i),v0(i),b0(i),b1(i)
        nel=i
      enddo
      close(nfil)
1000  continue
      return
      end
!
!     ...1.........2.........3.........4.........5.........6.........7.........8
      subroutine wiendata(sy,ecoh,v0,b0,b1)
!     **************************************************************************
!     ** Data from WIEN.txt of                                                **
!     ** K. Lejaeghere, V. Van Speybroeck, G. Van Oost,  S. Cottenier         **
!     ** Critical Reviews in Solid State and Materials Sciences 39,1 (2014)   **
!     ** DOI: 10.1080/10408436.2013.772503                                    **
!     **                                                                      **
!     **  225=fcc
!     **  229=bcc
!     **  194=hcp 
!     **  227=dia
!     **  Li,B,As,Sb,Bi=166
!     **  P,Cl,Ga,Br,I=64
!     **  Se,Te=152
!     **  In,Hg=139
!     **  N=205
!     **  O =12
!     **  F=15
!     **  S=70
!     **  Mn=217
!     **  Po=221
!     **************************************************************************
      implicit none
      type LIST1_type
        character(2) :: sy
        real(8)      :: v0     ! volume 
        real(8)      :: b0     ! bulk modulus
        real(8)      :: b1     ! pressure derivative of bulk modulus
        integer(4)   :: icry   !space group number
      end type LIST1_type
      type LIST2_type
        character(2)    :: sy
        real(8)         :: ecohvasp
        real(8)         :: ecohexpwzpe
        real(8)         :: ecohexpwozpe
        real(8)         :: Tdebye
        real(8)         :: ezpe
      end type LIST2_type
      character(2),intent(out) :: sy
      real(8)     ,intent(out) :: ecoh
      real(8)     ,intent(out) :: v0
      real(8)     ,intent(out) :: b0
      real(8)     ,intent(out) :: b1
      REAL(8)     ,PARAMETER   :: KJBYMOL=1.D0/2625.500D0
      REAL(8)     ,PARAMETER   :: ANGSTROM=1.D0/0.529177D0
      REAL(8)     ,PARAMETER   :: KELVIN=1.D0
      REAL(8)     ,PARAMETER   :: NEWTON=1.213779D+7
      REAL(8)     ,PARAMETER   :: METER=1.D10*ANGSTROM
      REAL(8)     ,PARAMETER   :: GPA=1.D+9*NEWTON/METER**2
      INTEGER(4)  ,PARAMETER   :: NEL=71
      TYPE(LIST1_TYPE),SAVE    :: X(NEL)    
      TYPE(LIST2_TYPE),SAVE    :: Y(NEL)    
      LOGICAL(4)      ,SAVE    :: TINI=.FALSE.
      integer(4)               :: index
      INTEGER(4)               :: ICRY
!     **************************************************************************
      if(.not.tini) then
        TINI=.TRUE.
        X(  1)=LIST1_TYPE('H_',    17.387D0,    10.315D0,     3.025D0,194)
        X(  2)=LIST1_TYPE('HE',    17.778D0,     0.847D0,     6.534D0,195)
        X(  3)=LIST1_TYPE('LI',    20.216D0,    13.879D0,     3.754D0,166)
        X(  4)=LIST1_TYPE('BE',     7.915D0,   123.039D0,     3.154D0,194)
        X(  5)=LIST1_TYPE('B_',     7.245D0,   237.599D0,     3.476D0,166)
        X(  6)=LIST1_TYPE('C_',    11.654D0,   209.648D0,     3.565D0,194)
        X(  7)=LIST1_TYPE('N_',    28.896D0,    54.288D0,     3.753D0,205)
        X(  8)=LIST1_TYPE('O_',    18.564D0,    51.231D0,     3.927D0, 12)
        X(  9)=LIST1_TYPE('F_',    19.347D0,    35.015D0,     4.246D0, 15)
        X( 10)=LIST1_TYPE('NE',    24.349D0,     1.242D0,     8.664D0,225)
        X( 11)=LIST1_TYPE('NA',    37.089D0,     7.728D0,     3.736D0,166)
        X( 12)=LIST1_TYPE('MG',    22.934D0,    35.748D0,     4.263D0,194)
        X( 13)=LIST1_TYPE('AL',    16.503D0,    77.512D0,     4.667D0,225)
        X( 14)=LIST1_TYPE('SI',    20.553D0,    88.673D0,     4.227D0,227)
        X( 15)=LIST1_TYPE('P_',    21.614D0,    68.863D0,     4.416D0, 64)
        X( 16)=LIST1_TYPE('S_',    17.346D0,    85.495D0,     4.440D0, 70)
        X( 17)=LIST1_TYPE('CL',    39.175D0,    19.324D0,     4.489D0, 64)
        X( 18)=LIST1_TYPE('AR',    52.209D0,     0.704D0,     7.838D0,225)
        X( 19)=LIST1_TYPE('K_',    73.710D0,     3.586D0,     3.734D0,229)
        X( 20)=LIST1_TYPE('CA',    42.208D0,    17.300D0,     3.167D0,225)
        X( 21)=LIST1_TYPE('SC',    24.621D0,    54.559D0,     3.402D0,194)
        X( 22)=LIST1_TYPE('TI',    17.407D0,   112.712D0,     3.591D0,194)
        X( 23)=LIST1_TYPE('V_',    13.517D0,   185.231D0,     3.731D0,229)
        X( 24)=LIST1_TYPE('CR',    11.910D0,   183.841D0,     7.374D0,229)
        X( 25)=LIST1_TYPE('MN',    11.611D0,   131.159D0,     0.852D0,217)
        X( 26)=LIST1_TYPE('FE',    11.451D0,   196.127D0,     6.137D0,229)
        X( 27)=LIST1_TYPE('CO',    10.948D0,   216.191D0,     5.076D0,194)
        X( 28)=LIST1_TYPE('NI',    10.986D0,   204.685D0,     4.852D0,225)
        X( 29)=LIST1_TYPE('CU',    12.017D0,   143.702D0,     5.198D0,225)
        X( 30)=LIST1_TYPE('ZN',    15.270D0,    75.617D0,     5.377D0,194)
        X( 31)=LIST1_TYPE('GA',    20.377D0,    49.087D0,     5.330D0, 64)
        X( 32)=LIST1_TYPE('GE',    24.094D0,    59.486D0,     5.084D0,227)
        X( 33)=LIST1_TYPE('AS',    22.571D0,    69.732D0,     4.294D0,166)
        X( 34)=LIST1_TYPE('SE',    29.973D0,    47.637D0,     4.548D0,152)
        X( 35)=LIST1_TYPE('BR',    39.780D0,    22.629D0,     5.154D0, 64)
        X( 36)=LIST1_TYPE('KR',    65.226D0,     0.851D0,    22.009D0,225)
        X( 37)=LIST1_TYPE('RB',    91.130D0,     2.842D0,     2.321D0,229)
        X( 38)=LIST1_TYPE('SR',    54.561D0,    11.333D0,     4.172D0,225)
        X( 39)=LIST1_TYPE('Y_',    32.864D0,    41.320D0,     2.952D0,194)
        X( 40)=LIST1_TYPE('ZR',    23.402D0,    94.061D0,     3.247D0,194)
        X( 41)=LIST1_TYPE('NB',    18.155D0,   168.691D0,     3.504D0,229)
        X( 42)=LIST1_TYPE('MO',    15.825D0,   260.410D0,     4.264D0,229)
        X( 43)=LIST1_TYPE('TC',    14.472D0,   301.391D0,     4.555D0,194)
        X( 44)=LIST1_TYPE('RU',    13.810D0,   315.359D0,     4.955D0,194)
        X( 45)=LIST1_TYPE('RH',    14.082D0,   260.576D0,     5.440D0,225)
        X( 46)=LIST1_TYPE('PD',    15.326D0,   170.442D0,     5.854D0,225)
        X( 47)=LIST1_TYPE('AG',    17.855D0,    91.328D0,     5.799D0,225)
        X( 48)=LIST1_TYPE('CD',    22.866D0,    44.247D0,     7.089D0,194)
        X( 49)=LIST1_TYPE('IN',    27.497D0,    34.759D0,     4.870D0,139)
        X( 50)=LIST1_TYPE('SN',    36.883D0,    36.020D0,     5.038D0,227)
        X( 51)=LIST1_TYPE('SB',    31.786D0,    50.718D0,     4.495D0,166)
        X( 52)=LIST1_TYPE('TE',    35.009D0,    44.806D0,     4.703D0,152)
        X( 53)=LIST1_TYPE('I_',    50.339D0,    18.699D0,     5.222D0, 64)
        X( 54)=LIST1_TYPE('XE',    87.318D0,     0.569D0,    -0.724D0,225)
        X( 55)=LIST1_TYPE('CS',   117.748D0,     1.964D0,     3.592D0,229)
        X( 56)=LIST1_TYPE('BA',    63.200D0,     8.431D0,     3.299D0,229)
        X( 57)=LIST1_TYPE('LU',    29.060D0,    47.725D0,     4.048D0,194)
        X( 58)=LIST1_TYPE('HF',    22.551D0,   108.082D0,     3.225D0,194)
        X( 59)=LIST1_TYPE('TA',    18.296D0,   193.730D0,     4.774D0,229)
        X( 60)=LIST1_TYPE('W_',    16.175D0,   302.590D0,     4.300D0,229)
        X( 61)=LIST1_TYPE('RE',    14.987D0,   364.610D0,     4.466D0,194)
        X( 62)=LIST1_TYPE('OS',    14.320D0,   402.199D0,     4.352D0,194)
        X( 63)=LIST1_TYPE('IR',    14.533D0,   341.743D0,     6.958D0,225)
        X( 64)=LIST1_TYPE('PT',    15.678D0,   251.750D0,     5.342D0,225)
        X( 65)=LIST1_TYPE('AU',    17.993D0,   139.863D0,     5.985D0,225)
        X( 66)=LIST1_TYPE('HG',    29.716D0,     8.155D0,     8.077D0,139)
        X( 67)=LIST1_TYPE('TL',    31.456D0,    26.683D0,     4.565D0,194)
        X( 68)=LIST1_TYPE('PB',    32.000D0,    39.551D0,     5.954D0,225)
        X( 69)=LIST1_TYPE('BI',    36.939D0,    42.556D0,     4.626D0,166)
        X( 70)=LIST1_TYPE('PO',    37.570D0,    45.523D0,     5.018D0,221)
        X( 71)=LIST1_TYPE('RN',    92.765D0,     0.544D0,    13.133D0,225)
!
!       ========================================================================
!       == unit conversion to hartree atomic units                            ==
!       ========================================================================
        x(:)%v0=x(:)%v0*angstrom**3
        x(:)%b0=x(:)%b0*GPA
!
!       ========================================================================
!       == list with cohesive energies                                        ==
!       == data from table 2.1 of supplementary material                      ==
!       ==  lejaeghere14_critrevsolstmatsci39_1                               ==
!       == 1) cohesive energy in kj/mol VASP-PBE                              ==
!       == 2) cohesive energy in kj/mol experiment with zero-point correction ==
!       == 3) cohesive energy in kj/mol experiment w/o zero-point correction  ==
!       == 4) debye temperature in K                                          ==
!       == 5) zero-point energy in kj/mol                                     ==
!       ========================================================================
        Y(  1)=LIST2_TYPE('H_', 219.00D0,  219.00D0, 219.00D0,    0.0D0,  0.0D0)
        Y(  2)=LIST2_TYPE('HE',   1.10D0,    1.10D0,   1.10D0,    0.0D0,  0.0D0)
        Y(  3)=LIST2_TYPE('LI', 155.00D0,  161.00D0, 158.00D0,  344.0D0,  3.2D0)
        Y(  4)=LIST2_TYPE('BE', 360.00D0,  333.00D0, 320.00D0, 1440.0D0, 13.5D0)
        Y(  5)=LIST2_TYPE('B_', 600.00D0,  569.00D0, 561.00D0,  805.0D0,  7.5D0)
        Y(  6)=LIST2_TYPE('C_', 758.00D0,  719.00D0, 715.00D0,  402.0D0,  3.8D0)
        Y(  7)=LIST2_TYPE('N_', 503.00D0,  475.00D0, 474.00D0,   70.0D0,  0.7D0)
        Y(  8)=LIST2_TYPE('O_', 294.00D0,  252.00D0, 251.00D0,   93.0D0,  0.5D0)
        Y(  9)=LIST2_TYPE('F_', 114.00D0,   81.00D0,  81.00D0,   65.0D0,  0.5D0)
        Y( 10)=LIST2_TYPE('NE',   1.80D0,    2.62D0,   1.92D0,   75.0D0,  0.7D0)
        Y( 11)=LIST2_TYPE('NA', 104.00D0,  108.00D0, 107.00D0,  158.0D0,  1.5D0)
        Y( 12)=LIST2_TYPE('MG', 145.00D0,  149.00D0, 145.00D0,  400.0D0,  3.7D0)
        Y( 13)=LIST2_TYPE('AL', 331.00D0,  331.00D0, 327.00D0,  428.0D0,  4.0D0)
        Y( 14)=LIST2_TYPE('SI', 439.00D0,  452.00D0, 446.00D0,  645.0D0,  6.0D0)
        Y( 15)=LIST2_TYPE('P_', 336.00D0,  333.00D0, 331.00D0,  231.0D0,  2.2D0)
        Y( 16)=LIST2_TYPE('S_', 294.00D0,  277.00D0, 275.00D0,  182.0D0,  1.7D0)
        Y( 17)=LIST2_TYPE('CL', 141.00D0,  137.00D0, 135.00D0,  142.0D0,  2.0D0)
        Y( 18)=LIST2_TYPE('AR',   9.16D0,    8.60D0,   7.74D0,   92.0D0,  0.9D0)
        Y( 19)=LIST2_TYPE('K_',  83.90D0,   91.00D0,  90.10D0,   91.0D0,  0.9D0)
        Y( 20)=LIST2_TYPE('CA', 185.00D0,  180.00D0, 178.00D0,  230.0D0,  2.2D0)
        Y( 21)=LIST2_TYPE('SC', 400.00D0,  379.00D0, 376.00D0,  360.0D0,  3.4D0)
        Y( 22)=LIST2_TYPE('TI', 528.00D0,  472.00D0, 468.00D0,  420.0D0,  3.9D0)
        Y( 23)=LIST2_TYPE('V_', 571.00D0,  516.00D0, 512.00D0,  380.0D0,  3.6D0)
        Y( 24)=LIST2_TYPE('CR', 390.00D0,  401.00D0, 395.00D0,  630.0D0,  5.9D0)
        Y( 25)=LIST2_TYPE('MN', 372.00D0,  286.00D0, 282.00D0,  410.0D0,  3.8D0)
        Y( 26)=LIST2_TYPE('FE', 468.00D0,  417.00D0, 413.00D0,  470.0D0,  4.4D0)
        Y( 27)=LIST2_TYPE('CO', 496.00D0,  428.00D0, 424.00D0,  445.0D0,  4.2D0)
        Y( 28)=LIST2_TYPE('NI', 466.00D0,  432.00D0, 428.00D0,  450.0D0,  4.2D0)
        Y( 29)=LIST2_TYPE('CU', 335.00D0,  339.00D0, 336.00D0,  343.0D0,  3.2D0)
        Y( 30)=LIST2_TYPE('ZN', 106.00D0,  133.00D0, 130.00D0,  327.0D0,  3.1D0)
        Y( 31)=LIST2_TYPE('GA', 254.00D0,  274.00D0, 271.00D0,  320.0D0,  3.0D0)
        Y( 32)=LIST2_TYPE('GE', 361.00D0,  375.00D0, 372.00D0,  374.0D0,  3.5D0)
        Y( 33)=LIST2_TYPE('AS', 286.00D0,  287.90D0, 285.30D0,  282.0D0,  2.6D0)
        Y( 34)=LIST2_TYPE('SE', 251.00D0,  238.00D0, 237.00D0,   90.0D0,  0.8D0)
        Y( 35)=LIST2_TYPE('BR', 131.00D0,  120.00D0, 118.00D0,  104.0D0,  1.6D0)
        Y( 36)=LIST2_TYPE('KR',   2.34D0,   11.90D0,  11.20D0,   72.0D0,  0.7D0)
        Y( 37)=LIST2_TYPE('RB',  74.70D0,   82.70D0,  82.20D0,   56.0D0,  0.5D0)
        Y( 38)=LIST2_TYPE('SR', 156.00D0,  167.00D0, 166.00D0,  147.0D0,  1.4D0)
        Y( 39)=LIST2_TYPE('Y_', 402.00D0,  425.00D0, 422.00D0,  280.0D0,  2.6D0)
        Y( 40)=LIST2_TYPE('ZR', 604.00D0,  606.00D0, 603.00D0,  291.0D0,  2.7D0)
        Y( 41)=LIST2_TYPE('NB', 669.00D0,  733.00D0, 730.00D0,  275.0D0,  2.6D0)
        Y( 42)=LIST2_TYPE('MO', 603.00D0,  662.00D0, 658.00D0,  450.0D0,  4.2D0)
        Y( 43)=LIST2_TYPE('TC', 658.00D0,  665.00D0, 661.00D0,  387.0D0,  3.6D0)
        Y( 44)=LIST2_TYPE('RU', 644.00D0,  656.00D0, 650.00D0,  600.0D0,  5.6D0)
        Y( 45)=LIST2_TYPE('RH', 567.00D0,  558.00D0, 554.00D0,  480.0D0,  4.5D0)
        Y( 46)=LIST2_TYPE('PD', 357.00D0,  379.00D0, 376.00D0,  274.0D0,  2.6D0)
        Y( 47)=LIST2_TYPE('AG', 240.00D0,  286.00D0, 284.00D0,  225.0D0,  2.1D0)
        Y( 48)=LIST2_TYPE('CD',  71.50D0,  114.00D0, 112.00D0,  209.0D0,  2.0D0)
        Y( 49)=LIST2_TYPE('IN', 223.00D0,  244.00D0, 243.00D0,  108.0D0,  1.0D0)
        Y( 50)=LIST2_TYPE('SN', 306.00D0,  305.00D0, 303.00D0,  200.0D0,  1.9D0)
        Y( 51)=LIST2_TYPE('SB', 261.00D0,  267.00D0, 265.00D0,  211.0D0,  2.0D0)
        Y( 52)=LIST2_TYPE('TE', 233.00D0,  212.00D0, 211.00D0,  153.0D0,  1.4D0)
        Y( 53)=LIST2_TYPE('I_', 125.00D0,  109.00D0, 107.00D0,   87.0D0,  1.6D0)
        Y( 54)=LIST2_TYPE('XE',   2.69D0,   16.50D0,  15.90D0,   64.0D0,  0.6D0)
        Y( 55)=LIST2_TYPE('CS',  69.00D0,   78.00D0,  77.60D0,   38.0D0,  0.4D0)
        Y( 56)=LIST2_TYPE('BA', 182.00D0,  184.00D0, 183.00D0,  110.0D0,  1.0D0)
        Y( 57)=LIST2_TYPE('LU', 395.00D0,  430.00D0, 428.00D0,  210.0D0,  2.0D0)
        Y( 58)=LIST2_TYPE('HF', 606.00D0,  623.00D0, 621.00D0,  252.0D0,  2.4D0)
        Y( 59)=LIST2_TYPE('TA', 766.00D0,  784.00D0, 782.00D0,  240.0D0,  2.2D0)
        Y( 60)=LIST2_TYPE('W_', 852.00D0,  863.00D0, 859.00D0,  400.0D0,  3.7D0)
        Y( 61)=LIST2_TYPE('RE', 760.00D0,  779.00D0, 775.00D0,  430.0D0,  4.0D0)
        Y( 62)=LIST2_TYPE('OS', 770.00D0,  793.00D0, 788.00D0,  500.0D0,  4.7D0)
        Y( 63)=LIST2_TYPE('IR', 676.00D0,  674.00D0, 670.00D0,  420.0D0,  3.9D0)
        Y( 64)=LIST2_TYPE('PT', 514.00D0,  566.00D0, 564.00D0,  240.0D0,  2.2D0)
        Y( 65)=LIST2_TYPE('AU', 300.00D0,  370.00D0, 368.00D0,  165.0D0,  1.5D0)
        Y( 66)=LIST2_TYPE('HG',  20.00D0,   66.00D0,  65.00D0,   71.9D0,  0.7D0)
        Y( 67)=LIST2_TYPE('TL', 146.00D0,  183.00D0, 182.00D0,   78.5D0,  0.7D0)
        Y( 68)=LIST2_TYPE('PB', 189.00D0,  197.00D0, 196.00D0,  105.0D0,  1.0D0)
        Y( 69)=LIST2_TYPE('BI', 200.00D0,  211.00D0, 210.00D0,  119.0D0,  1.1D0)
        Y( 70)=LIST2_TYPE('PO', 157.00D0,  145.00D0, 144.00D0,   92.0D0,  0.9D0)
        Y( 71)=LIST2_TYPE('RN',   3.78D0,   19.60D0,  19.50D0,   15.0D0,  0.1D0)
!
!       ========================================================================
!       == unit conversion to hartree atomic units                            ==
!       ========================================================================
        y(:)%ecohvasp    =y(:)%ecohvasp    *kjbymol
        y(:)%ecohexpwzpe =y(:)%ecohexpwzpe *kjbymol
        y(:)%ecohexpwozpe=y(:)%ecohexpwozpe*kjbymol
        y(:)%Tdebye      =y(:)%Tdebye      *kelvin
        y(:)%ezpe        =y(:)%ezpe        *kjbymol
      END IF
      do index=1,nel
        if(sy.ne.x(index)%sy) cycle
        ECOH=Y(INDEX)%ECOHVASP
        V0=X(INDEX)%V0
        B0=X(INDEX)%B0
        B1=X(INDEX)%B1
        ICRY=X(INDEX)%ICRY
        RETURN
      enddo
      print*,'symbol ',sy,' not recognized'
      stop 'stop in wiendata'
      end


