!
!     ...1.........2.........3.........4.........5.........6.........7.........8
      program main
      implicit none
      integer(4),parameter :: nel=71
      character(2) :: sy
      real(8)      :: v0
      real(8)      :: b0
      real(8)      :: b1
      integer(4) :: icry
      integer(4) :: i
      integer(4) :: nfil=10
      character(64) :: string
      character(128) :: fmt

!     **************************************************************************
      do i=1,nel
        call wiendata(i,SY,v0,b0,b1,icry)
      ENDDO
      stop
      end
!
!     ...1.........2.........3.........4.........5.........6.........7.........8
      subroutine wiendata(index,sy,v0,b0,b1,icry)
!     **************************************************************************
!     ** Data from WIEN.txt of                                                **
!     ** K. Lejaeghere, V. Van Speybroeck, G. Van Oost,  S. Cottenier         **
!     ** Critical Reviews in Solid State and Materials Sciences 39,1 (2014)   **
!     ** DOI: 10.1080/10408436.2013.772503                                    **
!
! 225=fcc
! 229=bcc
! 194=hcp 
! 227=dia
! Li,B,As,Sb,Bi=166
! P,Cl,Ga,Br,I=64
! Se,Te=152
! In,Hg=139
! N=205
! O =12
! F=15
! S=70
! Mn=217
! Po=221
!     **************************************************************************
      implicit none
      type LIST_type
        character(2) :: sy
        real(8)      :: v0
        real(8)      :: b0
        real(8)      :: b1
        integer(4)      :: icry   !space group number
      end type LIST_type
      integer(4)  ,intent(in) :: index
      character(2),intent(out)  :: sy
      real(8)     ,intent(out) :: v0
      real(8)     ,intent(out) :: b0
      real(8)     ,intent(out) :: b1
      integer(4)  ,intent(out) :: icry
      integer(4),parameter :: nel=71
      type(list_type),save :: X(nel)    
      logical(4)     ,save :: tini=.false.
      integer(4)           :: i
!     **************************************************************************
      if(.not.tini) then
        TINI=.TRUE.
        X(  1)=LIST_TYPE('H ',    17.387D0,    10.315D0,     3.025D0,194)
        X(  2)=LIST_TYPE('HE',    17.778D0,     0.847D0,     6.534D0,195)
        X(  3)=LIST_TYPE('LI',    20.216D0,    13.879D0,     3.754D0,166)
        X(  4)=LIST_TYPE('BE',     7.915D0,   123.039D0,     3.154D0,194)
        X(  5)=LIST_TYPE('B ',     7.245D0,   237.599D0,     3.476D0,166)
        X(  6)=LIST_TYPE('C ',    11.654D0,   209.648D0,     3.565D0,194)
        X(  7)=LIST_TYPE('N ',    28.896D0,    54.288D0,     3.753D0,205)
        X(  8)=LIST_TYPE('O ',    18.564D0,    51.231D0,     3.927D0, 12)
        X(  9)=LIST_TYPE('F ',    19.347D0,    35.015D0,     4.246D0, 15)
        X( 10)=LIST_TYPE('NE',    24.349D0,     1.242D0,     8.664D0,225)
        X( 11)=LIST_TYPE('NA',    37.089D0,     7.728D0,     3.736D0,166)
        X( 12)=LIST_TYPE('MG',    22.934D0,    35.748D0,     4.263D0,194)
        X( 13)=LIST_TYPE('AL',    16.503D0,    77.512D0,     4.667D0,225)
        X( 14)=LIST_TYPE('SI',    20.553D0,    88.673D0,     4.227D0,227)
        X( 15)=LIST_TYPE('P ',    21.614D0,    68.863D0,     4.416D0, 64)
        X( 16)=LIST_TYPE('S ',    17.346D0,    85.495D0,     4.440D0, 70)
        X( 17)=LIST_TYPE('CL',    39.175D0,    19.324D0,     4.489D0, 64)
        X( 18)=LIST_TYPE('AR',    52.209D0,     0.704D0,     7.838D0,225)
        X( 19)=LIST_TYPE('K ',    73.710D0,     3.586D0,     3.734D0,229)
        X( 20)=LIST_TYPE('CA',    42.208D0,    17.300D0,     3.167D0,225)
        X( 21)=LIST_TYPE('SC',    24.621D0,    54.559D0,     3.402D0,194)
        X( 22)=LIST_TYPE('TI',    17.407D0,   112.712D0,     3.591D0,194)
        X( 23)=LIST_TYPE('V ',    13.517D0,   185.231D0,     3.731D0,229)
        X( 24)=LIST_TYPE('CR',    11.910D0,   183.841D0,     7.374D0,229)
        X( 25)=LIST_TYPE('MN',    11.611D0,   131.159D0,     0.852D0,217)
        X( 26)=LIST_TYPE('FE',    11.451D0,   196.127D0,     6.137D0,229)
        X( 27)=LIST_TYPE('CO',    10.948D0,   216.191D0,     5.076D0,194)
        X( 28)=LIST_TYPE('NI',    10.986D0,   204.685D0,     4.852D0,225)
        X( 29)=LIST_TYPE('CU',    12.017D0,   143.702D0,     5.198D0,225)
        X( 30)=LIST_TYPE('ZN',    15.270D0,    75.617D0,     5.377D0,194)
        X( 31)=LIST_TYPE('GA',    20.377D0,    49.087D0,     5.330D0, 64)
        X( 32)=LIST_TYPE('GE',    24.094D0,    59.486D0,     5.084D0,227)
        X( 33)=LIST_TYPE('AS',    22.571D0,    69.732D0,     4.294D0,166)
        X( 34)=LIST_TYPE('SE',    29.973D0,    47.637D0,     4.548D0,152)
        X( 35)=LIST_TYPE('BR',    39.780D0,    22.629D0,     5.154D0, 64)
        X( 36)=LIST_TYPE('KR',    65.226D0,     0.851D0,    22.009D0,225)
        X( 37)=LIST_TYPE('RB',    91.130D0,     2.842D0,     2.321D0,229)
        X( 38)=LIST_TYPE('SR',    54.561D0,    11.333D0,     4.172D0,225)
        X( 39)=LIST_TYPE('Y ',    32.864D0,    41.320D0,     2.952D0,194)
        X( 40)=LIST_TYPE('ZR',    23.402D0,    94.061D0,     3.247D0,194)
        X( 41)=LIST_TYPE('NB',    18.155D0,   168.691D0,     3.504D0,229)
        X( 42)=LIST_TYPE('MO',    15.825D0,   260.410D0,     4.264D0,229)
        X( 43)=LIST_TYPE('TC',    14.472D0,   301.391D0,     4.555D0,194)
        X( 44)=LIST_TYPE('RU',    13.810D0,   315.359D0,     4.955D0,194)
        X( 45)=LIST_TYPE('RH',    14.082D0,   260.576D0,     5.440D0,225)
        X( 46)=LIST_TYPE('PD',    15.326D0,   170.442D0,     5.854D0,225)
        X( 47)=LIST_TYPE('AG',    17.855D0,    91.328D0,     5.799D0,225)
        X( 48)=LIST_TYPE('CD',    22.866D0,    44.247D0,     7.089D0,194)
        X( 49)=LIST_TYPE('IN',    27.497D0,    34.759D0,     4.870D0,139)
        X( 50)=LIST_TYPE('SN',    36.883D0,    36.020D0,     5.038D0,227)
        X( 51)=LIST_TYPE('SB',    31.786D0,    50.718D0,     4.495D0,166)
        X( 52)=LIST_TYPE('TE',    35.009D0,    44.806D0,     4.703D0,152)
        X( 53)=LIST_TYPE('I ',    50.339D0,    18.699D0,     5.222D0, 64)
        X( 54)=LIST_TYPE('XE',    87.318D0,     0.569D0,    -0.724D0,225)
        X( 55)=LIST_TYPE('CS',   117.748D0,     1.964D0,     3.592D0,229)
        X( 56)=LIST_TYPE('BA',    63.200D0,     8.431D0,     3.299D0,229)
        X( 57)=LIST_TYPE('LU',    29.060D0,    47.725D0,     4.048D0,194)
        X( 58)=LIST_TYPE('HF',    22.551D0,   108.082D0,     3.225D0,194)
        X( 59)=LIST_TYPE('TA',    18.296D0,   193.730D0,     4.774D0,229)
        X( 60)=LIST_TYPE('W ',    16.175D0,   302.590D0,     4.300D0,229)
        X( 61)=LIST_TYPE('RE',    14.987D0,   364.610D0,     4.466D0,194)
        X( 62)=LIST_TYPE('OS',    14.320D0,   402.199D0,     4.352D0,194)
        X( 63)=LIST_TYPE('IR',    14.533D0,   341.743D0,     6.958D0,225)
        X( 64)=LIST_TYPE('PT',    15.678D0,   251.750D0,     5.342D0,225)
        X( 65)=LIST_TYPE('AU',    17.993D0,   139.863D0,     5.985D0,225)
        X( 66)=LIST_TYPE('HG',    29.716D0,     8.155D0,     8.077D0,139)
        X( 67)=LIST_TYPE('TL',    31.456D0,    26.683D0,     4.565D0,194)
        X( 68)=LIST_TYPE('PB',    32.000D0,    39.551D0,     5.954D0,225)
        X( 69)=LIST_TYPE('BI',    36.939D0,    42.556D0,     4.626D0,166)
        X( 70)=LIST_TYPE('PO',    37.570D0,    45.523D0,     5.018D0,221)
        X( 71)=LIST_TYPE('RN',    92.765D0,     0.544D0,    13.133D0,225)
      END IF
      sy=x(index)%sy
      v0=x(index)%v0
      B0=x(index)%b0
      B1=x(index)%b1
      icry=x(index)%icry
      return
      end


