!***********************************************************************
!***********************************************************************
!**                                                                   **
!**  INTERFACES TO SCIENTIFIC LIBRARY ROUTINES                        **
!**  USES THE ESSL LIBRARY                                            **
!**                                                                   **
!***********************************************************************
!***********************************************************************
! CPPVAR_FFTW      USE FFTW FOR FOURIRT TRANSFORMS
! CPPVAR_FFT_ESSL      USE ESSL FOR FOURIER TRANSFORMS
! CPPVAR_FFTPACK      USE EXPLICIT FFT
!
! CPPVAR_BLAS_ATLAS    USE ATLAS BLAS
! CPPVAR_BLAS_ESSL     USE ESSL
! CPPVAR_BLAS_EXPLICIT USE EXPLICIT BLAS
!
! CPPVAR_LANGEXT_XLF   LANGUAGE EXTENSIONS XLF
!
! CPPVAR_SUPPORT_XLF
! 1 
! CPPVAR_SUPPORT_DEC
!
! CPPVAR_ERF_EXPLICIT  USE EXPLICIT ERF AND ERFC
! CPPVAR_ERF_IMPLICIT  USE IMPLICIT ERF AND ERFC
!
! CPPVAR_USAGE_EXIST  C-ROUTINE GETRUSAGE AVAILABLE
!
!=============== ENVIRONMENT ABSOFT COMPILER =========================
!    IT ASSUMES THE ATLAS LAPACK AND BLAS ROUTINES TO BE LINKED
!    IT ASSUMES THE ABSOFT SUPPORT LIBRARY LIBU77.A  TO BE LINKED
!    IT ASSUMES THE FAST FOURIER TRANSFORM LIBRARY LIBFFTW.A  TO BE LINKED
!#if defined(CPPVARIABLE_ABS)
!#define CPPVAR_FFTW
!#define CPPVAR_BLAS_ATLAS
!#define 1 
!=============== ENVIRONMENT IBM AIX ===================================
!    IT ASSUMES THE ESSL LIBRARY TO BE LINKED
!    IT USES XLF SPECIFIC SUPPORT ROUTINES AND LANGUAGE EXTENSIONS
!#elif defined(CPPVARIABLE_XLF)
!#define CPPVAR_FFT_ESSL
!#define CPPVAR_BLAS_ESSL
!#define CPPVAR_SUPPORT_XLF
!#define CPPVAR_LANGEXT_XLF
!#define CPPVAR_USAGE_EXIST
!#UNDEFINE EXLICITERF
!=============== ENVIRONMENT DEC ALPHA =================================
!#elif defined(CPPVARIABLE_DEC)
!#define CPPVAR_FFTW
!#define CPPVAR_BLAS_ATLAS
!=============== FREE ENVIRONMENT ===== =================================
! MAKES NO ASSUMPTIONS ABOUT THE ENVIRONMENT
!#else
!#define CPPVAR_FFTPACK
!#define CPPVAR_BLAS_EXPLICIT
!#endif
!#if defined(IBMLICENSE)
!
!     .................................................................
      SUBROUTINE LIB__GETUSAGE(ID,VALUE)
!     *****************************************************************
!     **                                                             **
!     **  PROVIDES INFORMATION  ON THE USAGE OF SYSTEM RESOURCES     **
!     **                                                             **
!     **  USES STANDARD C-LIBRARY ROUTINE GETRUSAGE                  **
!     **                                                             **
!     **                                                             **
!     **                                                             **
!     *****************************************************************
      IMPLICIT NONE
      TYPE USG_TYPE
        SEQUENCE
        INTEGER(4) :: UTIME(2)   ! USER TIME    (SECONDS,MICROSECONDS)
        INTEGER(4) :: STIME(2)   ! SYSTEM TIME  (SECONDS,MICROSECONDS)
        INTEGER(4) :: MAXRSS     ! MAX SIZE [KBYTE]
        INTEGER(4) :: IXRSS      ! INTEGRAL SHARED MEMORY SIZE [KBYTE*SEC]
        INTEGER(4) :: IDRSS      ! INTEGRAL UNSHARED DATA [KBYTE*SEC]
        INTEGER(4) :: ISRSS      ! INTEGRAL UNSHARED STACK [KBYTE*SEC]
        INTEGER(4) :: MINFLT     ! #(PAGE RECLAIMS)
        INTEGER(4) :: MAJFLT     ! #(PAGE FAULTS)
        INTEGER(4) :: NSWAP      ! #(SWAPPING PROCESS OUT OF MAIN)
        INTEGER(4) :: INBLOCK    ! #(FILE INPUTS)
        INTEGER(4) :: OUBLOCK    ! #(FILE OUTPUTS)
        INTEGER(4) :: MSGSND     ! #(IPC MESSAGES SEND)
        INTEGER(4) :: MSGRCV     ! #(IPC MESSAGES RECEIVED)
        INTEGER(4) :: NSIGNALS   ! #(SIGNALS SENT)
        INTEGER(4) :: NVCSW      ! #(VOLUNTARY CONTEXT SWITCHES)
        INTEGER(4) :: NIVCSW     ! #(INVOLUNTARY CONTEXT SWITCHES)
      END TYPE USG_TYPE
      CHARACTER(*),INTENT(IN)  :: ID
      REAL(8)     ,INTENT(OUT) :: VALUE
      TYPE(USG_TYPE)           :: USG
      REAL(8)                  :: KBYTE=2.D0**10
      INTEGER(4)               :: GETRUSAGE
      INTEGER(4)               :: RC
      REAL(8)                  :: CPUTIME

!     ******************************************************************
      USG%UTIME=(/0,0/)
      USG%STIME=(/0,0/)
      USG%MAXRSS=0
      USG%IXRSS=0
      USG%IDRSS=0
      USG%ISRSS=0
      USG%MINFLT=0
      USG%MAJFLT=0
      USG%NSWAP=0
      USG%INBLOCK=0
      USG%OUBLOCK=0
      USG%MSGSND=0
      USG%MSGRCV=0
      USG%NSIGNALS=0
      USG%NVCSW=0
      USG%NIVCSW=0
!     ==================================================================
!     ==================================================================

!
      CPUTIME=(REAL(USG%UTIME(1)+USG%STIME(1),KIND=8) &
     &        +REAL(USG%UTIME(2)+USG%STIME(2),KIND=8))*1.D-6
      cputime=max(cputime,1.d-6)
      IF(ID.EQ.'MAXMEM') THEN
        VALUE=REAL(USG%MAXRSS,KIND=8)*KBYTE
      ELSE IF(ID.EQ.'USRTIME') THEN
        VALUE=(REAL(USG%UTIME(1),KIND=8)+REAL(USG%UTIME(2),KIND=8))*1.D-6
      ELSE IF(ID.EQ.'SYSTIME') THEN
        VALUE=(REAL(USG%STIME(1),KIND=8)+REAL(USG%STIME(2),KIND=8))*1.D-6
      ELSE IF(ID.EQ.'CPUTIME') THEN
        VALUE=CPUTIME
      ELSE IF(ID.EQ.'PAGEFAULTRATE') THEN
        VALUE=REAL(USG%MAJFLT,KIND=8)/CPUTIME
      ELSE IF(ID.EQ.'SWAPRATE') THEN
        VALUE=REAL(USG%NSWAP,KIND=8)/CPUTIME
      ELSE IF(ID.EQ.'SWITCHRATE') THEN
        VALUE=REAL(USG%NVCSW+USG%NIVCSW,KIND=8)/CPUTIME
      ELSE
        CALL ERROR__MSG('ID NOT RECOGNIZED')
        CALL ERROR__STOP('MEMORY__GET')
      END IF
      RETURN
      END

!
!     ..................................................................
      SUBROUTINE LIB__ERFR8(X,Y)
!     ******************************************************************
!     **  COPYRIGHT(C) 1996 TAKUYA OOURA                              **
!     **  (EMAIL: OOURA@MMM.T.U-TOKYO.AC.JP).                         **
!     **  YOU MAY USE, COPY, MODIFY THIS CODE FOR ANY PURPOSE AND     **
!     **  WITHOUT FEE. YOU MAY DISTRIBUTE THIS ORIGINAL PACKAGE.        **
!     ******************************************************************
      IMPLICIT NONE
      REAL(8),INTENT(IN) :: X
      REAL(8),INTENT(OUT):: Y
      REAL(8)            :: W
      REAL(8)            :: T
      INTEGER(4)         :: K,I
      REAL(8)            :: A(0:64)
      REAL(8)            :: B(0:64)
!     ******************************************************************
      DATA (A(I), I = 0, 12) / &
     &    0.00000000005958930743D0, -0.00000000113739022964D0, &
     &    0.00000001466005199839D0, -0.00000016350354461960D0, &
     &    0.00000164610044809620D0, -0.00001492559551950604D0, &
     &    0.00012055331122299265D0, -0.00085483269811296660D0, &
     &    0.00522397762482322257D0, -0.02686617064507733420D0, &
     &    0.11283791670954881569D0, -0.37612638903183748117D0, &
     &    1.12837916709551257377D0 /
      DATA (A(I), I = 13, 25) /                                &
     &    0.00000000002372510631D0, -0.00000000045493253732D0, &
     &    0.00000000590362766598D0, -0.00000006642090827576D0, &
     &    0.00000067595634268133D0, -0.00000621188515924000D0, &
     &    0.00005103883009709690D0, -0.00037015410692956173D0, &
     &    0.00233307631218880978D0, -0.01254988477182192210D0, &
     &    0.05657061146827041994D0, -0.21379664776456006580D0, &
     &    0.84270079294971486929D0 /
      DATA (A(I), I = 26, 38) /                                &
     &    0.00000000000949905026D0, -0.00000000018310229805D0, &
     &    0.00000000239463074000D0, -0.00000002721444369609D0, &
     &    0.00000028045522331686D0, -0.00000261830022482897D0, &
     &    0.00002195455056768781D0, -0.00016358986921372656D0, &
     &    0.00107052153564110318D0, -0.00608284718113590151D0, &
     &    0.02986978465246258244D0, -0.13055593046562267625D0, &
     &    0.67493323603965504676D0 /
      DATA (A(I), I = 39, 51) /                                &
     &    0.00000000000382722073D0, -0.00000000007421598602D0, &
     &    0.00000000097930574080D0, -0.00000001126008898854D0, &
     &    0.00000011775134830784D0, -0.00000111992758382650D0, &
     &    0.00000962023443095201D0, -0.00007404402135070773D0, &
     &    0.00050689993654144881D0, -0.00307553051439272889D0, &
     &    0.01668977892553165586D0, -0.08548534594781312114D0, &
     &    0.56909076642393639985D0 /
      DATA (A(I), I = 52, 64) /                                &
     &    0.00000000000155296588D0, -0.00000000003032205868D0, &
     &    0.00000000040424830707D0, -0.00000000471135111493D0, &
     &    0.00000005011915876293D0, -0.00000048722516178974D0, &
     &    0.00000430683284629395D0, -0.00003445026145385764D0, &
     &    0.00024879276133931664D0, -0.00162940941748079288D0, &
     &    0.00988786373932350462D0, -0.05962426839442303805D0, &
     &    0.49766113250947636708D0 /
      DATA (B(I), I = 0, 12) /                                 &
     &   -0.00000000029734388465D0,  0.00000000269776334046D0, &
     &   -0.00000000640788827665D0, -0.00000001667820132100D0, &
     &   -0.00000021854388148686D0,  0.00000266246030457984D0, &
     &    0.00001612722157047886D0, -0.00025616361025506629D0, &
     &    0.00015380842432375365D0,  0.00815533022524927908D0, &
     &   -0.01402283663896319337D0, -0.19746892495383021487D0, &
     &    0.71511720328842845913D0 /
      DATA (B(I), I = 13, 25) /                                 &
     &   -0.00000000001951073787D0, -0.00000000032302692214D0,  &
     &    0.00000000522461866919D0,  0.00000000342940918551D0,  &
     &   -0.00000035772874310272D0,  0.00000019999935792654D0,  &
     &    0.00002687044575042908D0, -0.00011843240273775776D0,  &
     &   -0.00080991728956032271D0,  0.00661062970502241174D0,  &
     &    0.00909530922354827295D0, -0.20160072778491013140D0,  &
     &    0.51169696718727644908D0 /
      DATA (B(I), I = 26, 38) /                                 &
     &    0.00000000003147682272D0, -0.00000000048465972408D0,  &
     &    0.00000000063675740242D0,  0.00000003377623323271D0,  &
     &   -0.00000015451139637086D0, -0.00000203340624738438D0,  &
     &    0.00001947204525295057D0,  0.00002854147231653228D0,  &
     &   -0.00101565063152200272D0,  0.00271187003520095655D0,  &
     &    0.02328095035422810727D0, -0.16725021123116877197D0,  &
     &    0.32490054966649436974D0 /
      DATA (B(I), I = 39, 51) /                                &
     &    0.00000000002319363370D0, -0.00000000006303206648D0, &
     &   -0.00000000264888267434D0,  0.00000002050708040581D0, &
     &    0.00000011371857327578D0, -0.00000211211337219663D0, &
     &    0.00000368797328322935D0,  0.00009823686253424796D0, &
     &   -0.00065860243990455368D0, -0.00075285814895230877D0, &
     &    0.02585434424202960464D0, -0.11637092784486193258D0, &
     &    0.18267336775296612024D0 /
      DATA (B(I), I = 52, 64) /                                &
     &   -0.00000000000367789363D0,  0.00000000020876046746D0, &
     &   -0.00000000193319027226D0, -0.00000000435953392472D0, &
     &    0.00000018006992266137D0, -0.00000078441223763969D0, &
     &   -0.00000675407647949153D0,  0.00008428418334440096D0, &
     &   -0.00017604388937031815D0, -0.00239729611435071610D0, &
     &    0.02064129023876022970D0, -0.06905562880005864105D0, &
     &    0.09084526782065478489D0 /
      W = ABS(X)
      IF (W .LT. 2.2D0) THEN
          T = W * W
          K = INT(T)
          T = T - REAL(K,8)
          K = K * 13
          Y = (((((((((((( A(K    )*T + A(K+ 1)) * T + &
     &        A(K+ 2))*T + A(K+ 3))*T + A(K+ 4)) * T + &
     &        A(K+ 5))*T + A(K+ 6))*T + A(K+ 7)) * T + &
     &        A(K+ 8))*T + A(K+ 9))*T + A(K+10)) * T + &
     &        A(K+11))*T + A(K+12))*W
      ELSE IF (W .LT. 6.9D0) THEN
          K = INT(W)
          T = W - REAL(K,8)
          K = 13 * (K - 2)
          Y = (((((((((((B(K) * T + B(K + 1)) * T + &
     &        B(K + 2)) * T + B(K + 3)) * T + B(K + 4)) * T + &
     &        B(K + 5)) * T + B(K + 6)) * T + B(K + 7)) * T + &
     &        B(K + 8)) * T + B(K + 9)) * T + B(K + 10)) * T + &
     &        B(K + 11)) * T + B(K + 12)
          Y = Y * Y
          Y = Y * Y
          Y = Y * Y
          Y = 1 - Y * Y
      ELSE
          Y = 1
      END IF
      IF(X.LT.0) Y=-Y
      RETURN
      END
!     .......................................................................
      SUBROUTINE LIB__ERFCR8(X,Y)
!     **  COPYRIGHT(C) 1996 TAKUYA OOURA                              **
!     **  (EMAIL: OOURA@MMM.T.U-TOKYO.AC.JP).                         **
!     **  YOU MAY USE, COPY, MODIFY THIS CODE FOR ANY PURPOSE AND     **
!     **  WITHOUT FEE. YOU MAY DISTRIBUTE THIS ORIGINAL PACKAGE.        **
      IMPLICIT NONE
      REAL(8),INTENT(IN) :: X
      REAL(8),INTENT(OUT):: Y
      REAL(8)     ,PARAMETER :: PA  = 3.97886080735226000D+00
      REAL(8)     ,PARAMETER :: P0  = 2.75374741597376782D-01
      REAL(8)     ,PARAMETER :: P1  = 4.90165080585318424D-01
      REAL(8)     ,PARAMETER :: P2  = 7.74368199119538609D-01
      REAL(8)     ,PARAMETER :: P3  = 1.07925515155856677D+00
      REAL(8)     ,PARAMETER :: P4  = 1.31314653831023098D+00
      REAL(8)     ,PARAMETER :: P5  = 1.37040217682338167D+00
      REAL(8)     ,PARAMETER :: P6  = 1.18902982909273333D+00
      REAL(8)     ,PARAMETER :: P7  = 8.05276408752910567D-01
      REAL(8)     ,PARAMETER :: P8  = 3.57524274449531043D-01
      REAL(8)     ,PARAMETER :: P9  = 1.66207924969367356D-02
      REAL(8)     ,PARAMETER :: P10 =-1.19463959964325415D-01
      REAL(8)     ,PARAMETER :: P11 =-8.38864557023001992D-02
      REAL(8)     ,PARAMETER :: P12 = 2.49367200053503304D-03
      REAL(8)     ,PARAMETER :: P13 = 3.90976845588484035D-02
      REAL(8)     ,PARAMETER :: P14 = 1.61315329733252248D-02
      REAL(8)     ,PARAMETER :: P15 =-1.33823644533460069D-02
      REAL(8)     ,PARAMETER :: P16 =-1.27223813782122755D-02
      REAL(8)     ,PARAMETER :: P17 = 3.83335126264887303D-03
      REAL(8)     ,PARAMETER :: P18 = 7.73672528313526668D-03
      REAL(8)     ,PARAMETER :: P19 =-8.70779635317295828D-04
      REAL(8)     ,PARAMETER :: P20 =-3.96385097360513500D-03
      REAL(8)     ,PARAMETER :: P21 = 1.19314022838340944D-04
      REAL(8)     ,PARAMETER :: P22 = 1.27109764952614092D-03
      REAL(8)                :: T,U
!     **********************************************************
      T = PA / (PA + ABS(X))
      U = T - 0.5D0
      Y = (((((((((P22 * U + P21) * U + P20) * U + &
     &    P19) * U + P18) * U + P17) * U + P16) * U + &
     &    P15) * U + P14) * U + P13) * U + P12
      Y = ((((((((((((Y * U + P11) * U + P10) * U + &
     &    P9) * U + P8) * U + P7) * U + P6) * U + P5) * U + &
     &    P4) * U + P3) * U + P2) * U + P1) * U + P0) * T * &
     &    EXP(-X * X)
      IF (X .LT. 0) Y = 2 - Y
      RETURN
      END

!
!.......................................................................
MODULE RANDOM_MODULE
  INTEGER(8),SAVE        :: SEED=1
  INTEGER(8),PARAMETER   :: RANFAC1=22222221
  INTEGER(8),PARAMETER   :: RANFAC2=2**24
! CHOICE EXPLICIT CPPVARIABLE_XLF RNG
! INTEGER(8),SAVE        :: SEED=1_8
! INTEGER(8),PARAMETER   :: RANFAC1=44485709377909_8
! INTEGER(8),PARAMETER   :: RANFAC2=2_8**48
END MODULE RANDOM_MODULE
!
!     ..................................................................
      SUBROUTINE LIB__RANDOM(X)
!     ******************************************************************
!     **  RETURNS A RANDOM NUMBER                                     **
!     ******************************************************************
      USE RANDOM_MODULE
      IMPLICIT NONE
      REAL(8)   ,INTENT(OUT) :: X
!     ******************************************************************
      SEED=MODULO(RANFAC1*SEED,RANFAC2)
      X=REAL(SEED,8)/REAL(RANFAC2,8)
!     == CHOICE EXPLICIT CPPVARIABLE_XLF RNG
!     CALL RANDOM_NUMBER(Y)
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE LIB__RANDOMSEED
!     ******************************************************************
!     **  CHOOSE A SEED  FOR THE NADOM NUMBER GENERATOR               **
!     ******************************************************************
      USE RANDOM_MODULE
      IMPLICIT NONE
!     ******************************************************************
      SEED=1
!     == CHOICE EXPLICIT CPPVARIABLE_XLF RNG
!     SEED=1_8
!     == CHOICE ORIGINAL CPPVARIABLE_XLF RNG
!     CALL RANDOM_SEED(GENERATOR=2)
!     CALL RANDOM_SEED
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE LIB__INVERTR8(N,A,AINV)
!     ******************************************************************
!     **                                                              **
!     **  INVERTS THE REAL, SQUARE MATRIX A                           **
!     **                                                              **
!     **  DEPENDENCIES:                                               **
!     **    ESSL: DGEICD                                              **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      INTEGER(4),INTENT(IN) :: N
      REAL(8)   ,INTENT(IN) :: A(N,N)
      REAL(8)   ,INTENT(OUT):: AINV(N,N)
      INTEGER(4)            :: NAUX
      REAL(8)               :: AUX(100*N)
      INTEGER(4)            :: I,J

      INTEGER(4)            :: IPIV(N)
      INTEGER(4)            :: INFO

!     ******************************************************************
      NAUX=100*N
      AINV(1:N,1:N)=A(1:N,1:N)


      CALL DGETRF(N,N,AINV,N,IPIV,INFO) !LAPACK
      CALL DGETRI(N,AINV,N,IPIV,AUX,NAUX,INFO) !LAPACK

      RETURN
      END
!
!     ..................................................................
      SUBROUTINE LIB__DIAGR8(N,H,E,U)
!     ******************************************************************
!     **                                                              **
!     **  DIAGONALIZES THE REAL, SQUARE MATRIX H AFTER SYMMETRIZATION **
!     **  AND RETURNS EIGENVALUES, AND EIGENVECTORS                   **
!     **                                                              **
!     **         U(K,I)*H(K,L)*U(L,J)=DELTA(I,J)*E(I)                 **
!     **                                                              **
!     **  DEPENDENCIES:                                               **
!     **    ESSL DSPEV  MATRIX DIAGONALIZATION P727                   **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **   1) THE EIGENVECTORS ARE REAL BECAUSE IN CASE THEY ARE      **
!     **      COMPLEX REAL AND IMAGINARY PART ARE DEGENERATE          **
!     **      CAN THUS CAN ACT AS EIGENVECTORS THEMSELVES             **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE

      LOGICAL(4) ,PARAMETER :: TESSLERR=.FALSE.
      INTEGER(4),INTENT(IN) :: N
      REAL(8)   ,INTENT(IN) :: H(N,N)
      REAL(8)   ,INTENT(OUT):: E(N)
      REAL(8)   ,INTENT(OUT):: U(N,N)
      REAL(8)               :: WORK1((N*(N+1))/2)

      REAL(8)               :: WORK2(3*N)

      INTEGER(4)            :: K,I,J
      CHARACTER(8)          :: SAV2101
      INTEGER(4)            :: I1,I2
      INTEGER(4)            :: INFO
      INTEGER               :: INF1
      INTEGER               :: INF2
!     ******************************************************************
!
!     ==================================================================
!     ====  STORE IN LOWER PACKED STORAGE MODE FOR DIAGONALIZATION    ==
!     ==================================================================
      K=0
      DO J=1,N
        DO I=J,N
          K=K+1
          WORK1(K)=0.5D0*(H(I,J)+H(J,I))
        ENDDO
      ENDDO
!
!     ==================================================================
!     == DIAGONALIZE                                                  ==
!     ==================================================================

      CALL DSPEV('V','L',N,WORK1,E,U,N,WORK2,INFO) !->LAPACK
      IF(INFO.NE.0) THEN
        CALL ERROR__MSG('DIAGONALIZATION NOT CONVERGED')
        CALL ERROR__STOP('DIAG')
      END IF

      RETURN
!
!     ==================================================================
!     == ESSL ERROR HANDLING                                          ==
!     ==================================================================

      END
!
!     ..................................................................
      SUBROUTINE LIB__DIAGC8(N,H,E,U)
!     ******************************************************************
!     **                                                              **
!     **  DIAGONALIZES THE HERMITEAN, SQUARE MATRIX H                 **
!     **  AND RETURNS EIGENVALUES, AND EIGENVECTORS                   **
!     **                                                              **
!     **      CONJG(U(K,I))*H(K,L)*U(L,J)=DELTA(I,J)*E(I)             **
!     **                                                              **
!     **  DEPENDENCIES:                                               **
!     **    ESSL: ZHPEV :  MATRIX DIAGONALIZATION  P727               **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **   1) THE EIGENVECTORS ARE REAL BECAUSE IN CASE THEY ARE      **
!     **      COMPLEX REAL AND IMAGINARY PART ARE DEGENERATE          **
!     **      CAN THUS CAN ACT AS EIGENVECTORS THEMSELVES             **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE EINFO(ICODE,INF1,INF2) !ESSL ERROR HANDLING ROUTINE
        INTEGER                       :: ICODE
        INTEGER ,INTENT(OUT),OPTIONAL :: INF1
        INTEGER ,INTENT(OUT),OPTIONAL :: INF2
        END SUBROUTINE EINFO
      END INTERFACE
      LOGICAL(4) ,PARAMETER :: TESSLERR=.FALSE.
      INTEGER(4),INTENT(IN) :: N
      COMPLEX(8),INTENT(IN) :: H(N,N)
      REAL(8)   ,INTENT(OUT):: E(N)
      COMPLEX(8),INTENT(OUT):: U(N,N)
      COMPLEX(8)            :: WORK1((N*(N+1))/2)

      COMPLEX(8)            :: CWORK(2*N)
      REAL(8)               :: RWORK(3*N)

      INTEGER(4)            :: K,I,J
      CHARACTER(8)          :: SAV2101
      INTEGER(4)            :: I1,I2
      LOGICAL(4),PARAMETER  :: TTEST=.FALSE.
      COMPLEX(8)            :: CSVAR
      LOGICAL(4)            :: TCHK
      COMPLEX(8),ALLOCATABLE:: WORK3(:,:)
      INTEGER(4)            :: INFO
!clemens
      INTEGER               :: INF1
      INTEGER               :: INF2
!     ******************************************************************

!     ==================================================================
!     ====  ESSL ERROR HANDLING                                       ==
!     ==================================================================
!
!     ==================================================================
!     ====  STORE IN LOWER PACKED STORAGE MODE FOR DIAGONALIZATION    ==
!     ==================================================================
      K=0
      DO J=1,N
        DO I=J,N
          K=K+1
          WORK1(K)=0.5D0*(H(I,J)+CONJG(H(J,I)))
        ENDDO
      ENDDO
!
!     ==================================================================
!     == DIAGONALIZE                                                  ==
!     ==================================================================

      CALL ZHPEV('V','L',N,WORK1,E,U,N,CWORK,RWORK,INFO) !LAPACK
      IF(INFO.NE.0) THEN
        CALL ERROR__MSG('DIAGONALIZATION NOT CONVERGED')
        CALL ERROR__STOP('DIAG')
      END IF

!
!     ==================================================================
!     ====  OPTIONAL TEST                                             ==
!     ==================================================================
      IF(TTEST) THEN
        ALLOCATE(WORK3(N,N))
        TCHK=.TRUE.
        DO I=1,N
          DO J=1,N
            CSVAR=(0.D0,0.D0)
            DO K=1,N
              CSVAR=CSVAR+U(I,K)*E(K)*CONJG(U(J,K))
            ENDDO
            IF(ABS(CSVAR-H(I,J)).GT.1.D-8) THEN
              WRITE(*,FMT='(2I5,5F10.5)')I,J,CSVAR,H(I,J),ABS(CSVAR-H(I,J))
              TCHK=.FALSE.
            END IF
          ENDDO
        ENDDO
        DEALLOCATE(WORK3)
        IF(.NOT.TCHK) THEN
          CALL ERROR__MSG('DIAGONALIZATION TEST FAILED')
          CALL ERROR__STOP('LIB__DIAGC8')
        END IF
      END IF
      RETURN
!
!     ==================================================================
!     == ESSL ERROR HANDLING                                          ==
!     ==================================================================

      END
!
!     ..................................................................
      SUBROUTINE LIB__FFTC8(DIR,LEN,NFFT,X,Y)
!     ******************************************************************
!     **  1-D FFT                                                     **
!     **    DIR='GTOR' => Y(R)=     SUM_G X(G) EXP( I*G*R)            **
!     **    DIR='RTOG' => Y(G)=1/NR SUM_R X(R) EXP(-I*G*R)            **
!     **                                                              **
!     **  PACKAGES THE ESSL ROUTINE DCFT                              **
!     **  REMARK: X AND Y MAY BE IDENTICAL ARRAYS                     **
!     **                                                              **
!     **  USE FFTW AS STANDARD                                        **
!     **  USE FFTESSL IF ESSL IS INSTALLED                            **
!     **  USE FFTPACK AS BACKUP IF C-ROUTINES CANNOT BE LINKED OR     **
!     **      FFTW IS NOT AVAILABLE                                   **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: DIR
      INTEGER(4)  ,INTENT(IN) :: LEN
      INTEGER(4)  ,INTENT(IN) :: NFFT
      COMPLEX(8)  ,INTENT(IN) :: X(LEN,NFFT)
      COMPLEX(8)  ,INTENT(OUT):: Y(LEN,NFFT)
!     *******************************************************************

      CALL LIB_FFTW(DIR,LEN,NFFT,X,Y)


      RETURN
      END
!

!

!     ..................................................................
      SUBROUTINE LIB_FFTW(DIR,LEN,NFFT,X,Y)
!     ******************************************************************
!     **  1-D FFT                                                     **
!     **    DIR='GTOR' => Y(R)=     SUM_G X(G) EXP( I*G*R)            **
!     **    DIR='RTOG' => Y(G)=1/NR SUM_R X(R) EXP(-I*G*R)            **
!     **                                                              **
!     **  PACKAGES THE ESSL ROUTINE DCFT                              **
!     **  REMARK: X AND Y MAY BE IDENTICAL ARRAYS                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      INCLUDE 'FFTW_F77.I'
      CHARACTER(*),INTENT(IN) :: DIR
      INTEGER(4)  ,INTENT(IN) :: LEN
      INTEGER(4)  ,INTENT(IN) :: NFFT
      COMPLEX(8)  ,INTENT(IN) :: X(LEN,NFFT)
      COMPLEX(8)  ,INTENT(OUT):: Y(LEN,NFFT)
      CHARACTER(4),SAVE       :: DIRSAVE=''
      INTEGER(4)  ,SAVE       :: LENSAVE=0
      INTEGER(4)  ,SAVE       :: NFFTSAVE=0
      INTEGER(4)  ,SAVE       :: ISIGN
      REAL(8)     ,SAVE       :: SCALE
      INTEGER(4)              :: IFFT,I
      COMPLEX(8)              :: XDUMMY(LEN,NFFT)
      INTEGER(4),SAVE         :: NP=0
      INTEGER(4),PARAMETER    :: NPX=10 ! #(DIFFERENT FFT PLANS)

      INTEGER(4),SAVE         :: PLANS(NPX,2),PLAN=-1

      LOGICAL                 :: DEF
!     ******************************************************************
      XDUMMY=X
!
      IF(DIR.NE.DIRSAVE.OR.LEN.NE.LENSAVE) THEN
        IF (DIR.EQ.'GTOR') THEN
          ISIGN=1
        ELSE IF (DIR.EQ.'RTOG') THEN
          ISIGN=-1
        ELSE
          CALL ERROR__MSG('DIRECTION ID NOT RECOGNIZED')
          CALL ERROR__MSG('DIR MUST BE "GTOR" OR "RTOG"')
          CALL ERROR__CHVAL('DIR',TRIM(DIR))
          CALL ERROR__STOP('1D-FFTW')
        END IF
!
!       == FIND PLAN IN THE LIST
        DEF=.FALSE.
        DO I=1,NP
          IF((LEN*ISIGN).EQ.PLANS(I,1)) THEN
            DEF=.TRUE.
            PLAN=PLANS(I,2)
            EXIT
          END IF
        END DO
!
!       == CREATE NEW PLAN IF NOT IN THE LIST ==========================
        IF(.NOT.DEF) THEN
          WRITE(*,*) 'FFTW CREATE PLAN FOR: ', ISIGN,LEN,NP
          NP=NP+1
          IF(NP.GE.NPX) NP=NPX ! ALLOW ONLY NPX PLANS
          CALL FFTW_F77_CREATE_PLAN(PLANS(NP,2),LEN,ISIGN,FFTW_MEASURE)
          PLANS(NP,1)=ISIGN*LEN
          PLAN=PLANS(NP,2)
        END IF
        LENSAVE=LEN
        DIRSAVE=DIR
        SCALE=1.D0/REAL(LEN,KIND=8)
      END IF
!
!     ==================================================================
!     ==  NOW PERFORM FFT                                             ==
!     ==================================================================
      CALL FFTW_F77(PLAN,NFFT,XDUMMY(1,1),1,LEN,Y(1,1),1,LEN)
      IF (DIR.EQ.'RTOG') THEN
        DO IFFT=1,NFFT
          DO I=1,LEN
            Y(I,IFFT)=Y(I,IFFT)*SCALE
          ENDDO
        ENDDO
      END IF
      RETURN
      END

!
!DCFT:  1-D FFT P765

!
!DCFT:  1-D FFT P765

!
!     ...................................................FESSL..........
      SUBROUTINE LIB__FFTADJUSTGRD(NR)
!     ******************************************************************
!     **  THIS ROUTINE RETURNS THE ALLOWED FOURIER TRANSFORM LENGTH   **
!     **  THAT IS EQUAL OR LARGER THAN THE LENGTH SUPPLIED, BUT       **
!     **  BUT OTHERWISE AS SMALL AS POSSIBLE.                         **
!     ******************************************************************
      IMPLICIT NONE
      INTEGER(4),INTENT(INOUT):: NR
      INTEGER(4),PARAMETER  :: MAXI=300
      LOGICAL(4),SAVE       :: TINIT=.TRUE.
      INTEGER(4),SAVE       :: COUNT
      INTEGER(4),SAVE       :: IFR(MAXI)
      INTEGER(4)            :: H,I,J,K,M
      INTEGER(4)            :: ISVAR
      REAL(8)               :: SVAR
!     ******************************************************************
      IF (TINIT) THEN
        TINIT=.FALSE.

        COUNT=0
        OUTER: DO H=1,25
          DO I=0,2
            DO J=0,1
              DO K=0,1
                DO M=0,1
                  IF(COUNT.GE.MAXI) EXIT OUTER
                  SVAR = 2.D0**H * 3.D0**I * 5.D0**J * 7.D0**K *11.D0**M
                  IF(SVAR.GT.37748736.D0) CYCLE
                  COUNT=COUNT+1
                  IFR(COUNT)=NINT(SVAR)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO OUTER

        DO I=1,COUNT
          DO J=I+1,COUNT
            IF(IFR(I).GT.IFR(J)) THEN
              ISVAR=IFR(I)
              IFR(I)=IFR(J)
              IFR(J)=ISVAR
            END IF
          ENDDO
        ENDDO
      ENDIF
!
      DO I=2,COUNT
        IF(IFR(I).GE.NR) THEN
          NR=IFR(I)
          RETURN
        END IF
      ENDDO
      CALL ERROR__MSG('REQUESTED GRIDPOINT OUT OF RANGE')
      CALL ERROR__I4VAL('NR',NR)
      CALL ERROR__STOP('PLANEWAVE__ADJUSTFFTGRD')
      STOP
      END
!
!     ..................................................................
      SUBROUTINE LIB__3DFFTC8(DIR,N1,N2,N3,X,Y)
!     ******************************************************************
!     **  3-D FFT                                                     **
!     **    DIR='GTOR' => Y(R)=     SUM_G X(G) EXP( I*G*R)            **
!     **    DIR='RTOG' => Y(G)=1/NR SUM_R X(R) EXP(-I*G*R)            **
!     **                                                              **
!     **    USES THE 3D FFTW ROUTINES                                 **
!     **                                        CLEMENS FOERST, 2001  **
!     ******************************************************************
      IMPLICIT NONE
      CHARACTER(4)            :: DIR
      INTEGER(4)              :: N1,N2,N3
      COMPLEX(8)              :: X(N1,N2,N3)
      COMPLEX(8)              :: Y(N1,N2,N3)
!     ******************************************************************

      CALL LIB_3DFFTW(DIR,N1,N2,N3,X,Y)

      RETURN
      END
!

!     ..................................................................
      SUBROUTINE LIB_3DFFTW(DIR,N1,N2,N3,X,Y)
!     ******************************************************************
!     **  3-D FFT                                                     **
!     **    DIR='GTOR' => Y(R)=     SUM_G X(G) EXP( I*G*R)            **
!     **    DIR='RTOG' => Y(G)=1/NR SUM_R X(R) EXP(-I*G*R)            **
!     **                                                              **
!     **    USES THE 3D FFTW ROUTINES                                 **
!     **                                        CLEMENS FOERST, 2001  **
!     ******************************************************************
      IMPLICIT NONE
      CHARACTER(4)            :: DIR
      INTEGER(4)              :: DIM(3)
      INTEGER(4)              :: N1,N2,N3
      COMPLEX(8)              :: X(N1,N2,N3)
      COMPLEX(8)              :: Y(N1,N2,N3)
      INTEGER(4)              :: PLAN
      INTEGER(4)  ,SAVE       :: NP=0
      INTEGER(4),PARAMETER    :: NPX=10
      INTEGER(4)  ,SAVE       :: PLANS(NPX,4)
      REAL(8)     ,SAVE       :: SCALE
      INTEGER(4)  ,SAVE       :: DIMSAVE(3)=0
      CHARACTER(4),SAVE       :: DIRSAVE=''
      LOGICAL                 :: DEF
      INTEGER(4)              :: I
      INTEGER(4)  ,SAVE       :: ISIGN
      INCLUDE 'FFTW_F77.I'
!     ******************************************************************
      DIM(1)=N1
      DIM(2)=N2
      DIM(3)=N3
      IF(DIM(1).NE.DIMSAVE(1).OR.DIM(2).NE.DIMSAVE(2).OR. &
     &  DIM(3).NE.DIMSAVE(3).OR.DIR.NE.DIRSAVE) THEN
        IF (DIR.EQ.'GTOR') THEN
          ISIGN=1
        ELSE IF (DIR.EQ.'RTOG') THEN
          ISIGN=-1
        ELSE
          CALL ERROR__MSG('DIRECTION ID NOT RECOGNIZED')
          CALL ERROR__MSG('DIR MUST BE "GTOR" OR "RTOG"')
          CALL ERROR__CHVAL('DIR',TRIM(DIR))
          CALL ERROR__STOP('3D-FFTW')
        END IF
!
!       == FIND PLAN IN THE LIST
        DEF=.FALSE.
        DO I=1,NP
          IF((DIM(1)*ISIGN).EQ.PLANS(I,1).AND.(DIM(2)*ISIGN).EQ.PLANS(I,2)&
     &                     .AND.(DIM(3)*ISIGN).EQ.PLANS(I,3)) THEN
            DEF=.TRUE.
            PLAN=PLANS(I,4)
            EXIT
          END IF
        END DO
!
!       == CREATE NEW PLAN IF NOT IN THE LIST ==========================
        IF(.NOT.DEF) THEN
          WRITE(*,*) '3D-FFTW CREATE PLAN FOR: ', ISIGN,DIM
          NP=NP+1
          IF(NP.GE.NPX) NP=NPX ! ALLOW ONLY NPX PLANS
          CALL FFTWND_F77_CREATE_PLAN(PLAN,3,DIM,ISIGN,FFTW_ESTIMATE)
          PLANS(NP,1:3)=ISIGN*DIM
          PLANS(NP,4)=PLAN
        END IF
        DIMSAVE=DIM
        DIRSAVE=DIR
        IF(DIR.EQ.'RTOG') SCALE=1.D0/REAL(N1*N2*N3,KIND=8)
      END IF
!
!     ==================================================================
!     ==  NOW PERFORM FFT                                             ==
!     ==================================================================
      CALL FFTWND_F77_ONE(PLAN,X,Y)
      IF (DIR.EQ.'RTOG') Y=Y*SCALE
      RETURN
      END
!

!
!DGEMUL: MATRIX MULTIPLICATION P441
!     ..................................................................
      SUBROUTINE LIB__MATMULR8(N,M,L,A,B,C)
!     ******************************************************************
!     **  MATRIX MULTPLICATION   A*B=C                                **
!     ******************************************************************
      IMPLICIT NONE
      INTEGER(4),INTENT(IN) :: N
      INTEGER(4),INTENT(IN) :: M
      INTEGER(4),INTENT(IN) :: L
      REAL(8)   ,INTENT(IN) :: A(N,M)
      REAL(8)   ,INTENT(IN) :: B(M,L)
      REAL(8)   ,INTENT(OUT):: C(N,L)
      CHARACTER(8),PARAMETER:: LIB='ESSL'
      REAL(8)     ,PARAMETER:: ONE=1.D0
      REAL(8)     ,PARAMETER:: ZERO=0.D0
!     ******************************************************************

      CALL DGEMM('N','N',N,L,M,ONE,A,N,B,M,ZERO,C,N)

      RETURN
      END
!
!ZGEMUL: MATRIX MULTIPLICATION P441
!     . ................................................................
      SUBROUTINE LIB__MATMULC8(N,M,L,A,B,C)
!     ******************************************************************
!     **  MATRIX MULTPLICATION   A*B=C                                **
!     ******************************************************************
      IMPLICIT NONE
      INTEGER(4),INTENT(IN) :: N
      INTEGER(4),INTENT(IN) :: M
      INTEGER(4),INTENT(IN) :: L
      COMPLEX(8),INTENT(IN) :: A(N,M)
      COMPLEX(8),INTENT(IN) :: B(M,L)
      COMPLEX(8),INTENT(OUT):: C(N,L)
      CHARACTER(8),PARAMETER:: LIB='ESSL'
      COMPLEX(8)  ,PARAMETER:: ONE=(1.D0,0.D0)
      COMPLEX(8)  ,PARAMETER:: ZERO=(0.D0,0.D0)
!     ******************************************************************

      CALL ZGEMM('N','N',N,L,M,ONE,A,N,B,M,ZERO,C,N)

      RETURN
      END
!
!ZGEMM(TRANSA,TRANSB,L,M,N,ALPHA,A,LDA,B,LDB,BETA,C,LDC):
!C=BETA*C+ALPHA*A*B P456
!     ..................................................................
      SUBROUTINE LIB__ADDPRODUCTC8(TID,N,M,L,A,B,C)
!     ******************************************************************
!     **  ADD THE PRODUCT OF TWO MATRICES                             **
!     **  c=c+matmul(a,b)                                             **
!     **  for tid=.true., a and c may use identical storage           **
!     ******************************************************************
      IMPLICIT NONE
      LOGICAL(4),INTENT(IN)   :: TID
      INTEGER(4),INTENT(IN)   :: N
      INTEGER(4),INTENT(IN)   :: M
      INTEGER(4),INTENT(IN)   :: L
      COMPLEX(8),INTENT(IN)   :: A(N,M)
      COMPLEX(8),INTENT(IN)   :: B(M,L)
      COMPLEX(8),INTENT(INOUT):: C(N,L)
      COMPLEX(8)              :: SUM
      COMPLEX(8),ALLOCATABLE  :: WORK(:,:)
      INTEGER(4)              :: I,J,K
!     ******************************************************************
      if(tid) then
        ALLOCATE(WORK(N,M))
        WORK=A
!       ==  c=c+matmul(work,b)
        CALL ZGEMM('N','N',N,L,M,(1.D0,0.D0),work,N,B,M,(1.D0,0.D0),C,N)
        DEALLOCATE(WORK)
      ELSE
!       ==  c=c+matmul(a,b)
        CALL ZGEMM('N','N',N,L,M,(1.D0,0.D0),A,N,B,M,(1.D0,0.D0),C,N)
      END IF
      RETURN
      END
!
!ZHERK  C=BETA*C+ ALPHA*A^TA P477
!     ..................................................................
      SUBROUTINE LIB__DYADSUMR8(LEN1,LEN2,N,PSI1,PSI2,OPERATOR)
!     ******************************************************************
!     **   OPERATOR = SUM_I |PSI1(I)><PSI2(I)|                        **
!     ******************************************************************
      IMPLICIT NONE
      INTEGER(4),INTENT(IN) :: LEN1
      INTEGER(4),INTENT(IN) :: LEN2
      INTEGER(4),INTENT(IN) :: N
      REAL(8)   ,INTENT(IN) :: PSI1(LEN1,N)
      REAL(8)   ,INTENT(IN) :: PSI2(LEN2,N)
      REAL(8)   ,INTENT(OUT):: OPERATOR(LEN1,LEN2)
      INTEGER(4)            :: I,J,K
      REAL(8)               :: SUM
!     ******************************************************************

!     == operator=matmul(psi1,transpose(psi2))
      CALL DGEMM('N','T',LEN1,LEN2,N,1.D0,PSI1,LEN1,PSI2,LEN2,0.D0 &
     &          ,OPERATOR,LEN1)

      RETURN
      END
!
!ZHERK  C=BETA*C+ ALPHA*A^TA P477
!     ..................................................................
      SUBROUTINE LIB__DYADSUMC8(LEN1,LEN2,N,PSI1,PSI2,OPERATOR)
!     ******************************************************************
!     **   OPERATOR = SUM_I |PSI1(I)><PSI2(I)|                        **
!     ******************************************************************
      IMPLICIT NONE
      INTEGER(4),INTENT(IN) :: LEN1
      INTEGER(4),INTENT(IN) :: LEN2
      INTEGER(4),INTENT(IN) :: N
      COMPLEX(8),INTENT(IN) :: PSI1(LEN1,N)
      COMPLEX(8),INTENT(IN) :: PSI2(LEN2,N)
      COMPLEX(8),INTENT(OUT):: OPERATOR(LEN1,LEN2)
      INTEGER(4)            :: I,J,K
      COMPLEX(8)            :: SUM
!     ******************************************************************

!     == operator=matmul(psi1,transpose(psi2))
      CALL ZGEMM('N','C',LEN1,LEN2,N,(1.D0,0.D0) &
     &          ,PSI1(:,:),LEN1,PSI2(:,:),LEN2,(0.D0,0.D0),OPERATOR,LEN1)

      RETURN
      END
!
!ZHERK  C=BETA*C+ ALPHA*A^TA P477
!     ..................................................................
      SUBROUTINE LIB__SCALARPRODUCTR8(TID,LEN,N1,PSI1,N2,PSI2,OVERLAP)
!     ******************************************************************
!     ** PERFORMS THE SCALAR PRODUCTS OF TWO ARRAYS OF COMPLEX        **
!     ** STATE VECTORS                                                **
!     ******************************************************************
      IMPLICIT NONE
      LOGICAL(4),INTENT(IN) :: TID
      INTEGER(4),INTENT(IN) :: LEN
      INTEGER(4),INTENT(IN) :: N1
      INTEGER(4),INTENT(IN) :: N2
      REAL(8)   ,INTENT(IN) :: PSI1(LEN,N1)
      REAL(8)   ,INTENT(IN) :: PSI2(LEN,N2)
      REAL(8)   ,INTENT(OUT):: OVERLAP(N1,N2)
      INTEGER(4)            :: I,J,K
      REAL(8)               :: SUM
!     ******************************************************************
      IF(TID.AND.N1.NE.N2) THEN
        CALL ERROR__MSG('PSI2 AND PSI1 DIFFER FOR TID=.TRUE.')
        CALL ERROR__STOP('LIB__SCALARPRODUCTR8')
      END IF
      IF(TID) THEN
!       ==  OVERLAP(I,J) = 0.D0*OVERLAP+1.D0*SUM_K:PSI1(K,I)*PSI1(K,J) =
        CALL DSYRK('U','T',N1,LEN,1.D0,PSI1,LEN,0.D0,OVERLAP,N1)
        DO I=1,N1
          DO J=I+1,N2
            OVERLAP(J,I)=OVERLAP(I,J)
          ENDDO
        ENDDO
      ELSE

         CALL DGEMM('T','N',N1,N2,LEN,1.D0,PSI1(:,:),LEN,PSI2(:,:),LEN &
     &             ,0.D0,OVERLAP,N1)

      END IF
      RETURN
      END
!
!ZHERK  C=BETA*C+ ALPHA*A^TA P477
!     ..................................................................
      SUBROUTINE LIB__SCALARPRODUCTC8(TID,LEN,N1,PSI1,N2,PSI2,OVERLAP)
!     ******************************************************************
!     ** PERFORMS THE SCALAR PRODUCTS OF TWO ARRAYS OF COMPLEX        **
!     ** STATE VECTORS                                                **
!     ******************************************************************
      IMPLICIT NONE
      LOGICAL(4),INTENT(IN) :: TID
      INTEGER(4),INTENT(IN) :: LEN
      INTEGER(4),INTENT(IN) :: N1
      INTEGER(4),INTENT(IN) :: N2
      COMPLEX(8),INTENT(IN) :: PSI1(LEN,N1)
      COMPLEX(8),INTENT(IN) :: PSI2(LEN,N2)
      COMPLEX(8),INTENT(OUT):: OVERLAP(N1,N2)
      INTEGER(4)            :: I,J,K
      COMPLEX(8)            :: SUM
!     ******************************************************************
      IF(TID.AND.N1.NE.N2) THEN
        CALL ERROR__MSG('PSI2 AND PSI1 DIFFER FOR TID=.TRUE.')
        CALL ERROR__STOP('LIB__SCALARPRODUCTC8')
      END IF
      IF(TID) THEN
!       == attention: scalar factors are supposed to be real as they are
        CALL ZHERK('U','C',N1,LEN,1.D0,PSI1,LEN,0.D0,OVERLAP,N1)
        DO I=1,N1
          DO J=I+1,N2
            OVERLAP(J,I)=CONJG(OVERLAP(I,J))
          ENDDO
        ENDDO
      ELSE

        CALL ZGEMM('C','N',N1,N2,LEN,(1.D0,0.D0),PSI1,LEN,PSI2,LEN &
     &            ,(0.D0,0.D0),OVERLAP,N1)

      END IF
      RETURN
      END
!
!ZAXPY(N,ALPHA,X,INCX,Y,INCY)  Y=Y+ALPHA*X P267
!     ..................................................................
      SUBROUTINE LIB__VECTORADDC8(N,X,FAC,Y)
!     ******************************************************************
!     **  ADD TWO VECTORS                                             **
!     **  X=X+FAC*Y  WHERE X,Y ARE VECTORS AND FAC IS A SCALAR        **
!     ******************************************************************
      IMPLICIT NONE
      INTEGER(4),INTENT(IN)    :: N
      COMPLEX(8),INTENT(INOUT) :: X(N)
      COMPLEX(8),INTENT(IN)    :: FAC
      COMPLEX(8),INTENT(IN)    :: Y(N)
      INTEGER(4)               :: I
      CHARACTER(8),PARAMETER   :: LIB='ESSL'
!     ******************************************************************
      CALL ZAXPY(N,FAC,X,1,Y,1)
      RETURN
      END
!
!DAXPY(N,ALPHA,X,INCX,Y,INCY)  Y=Y+ALPHA*X P267
!DVES(N,X,INCX,Y,INCY,X,INCZ)  Z=X-Y       P313
!     ..................................................................
      SUBROUTINE LIB__VECTORADDR8(N,X,FAC,Y)
!     ******************************************************************
!     **  ADD TWO VECTORS                                             **
!     **  X=X+FAC*Y  WHERE X,Y ARE VECTORS AND FAC IS A SCALAR        **
!     ******************************************************************
      IMPLICIT NONE
      INTEGER(4),INTENT(IN)    :: N
      REAL(8)   ,INTENT(INOUT) :: X(N)
      REAL(8)   ,INTENT(IN)    :: FAC
      REAL(8)   ,INTENT(IN)    :: Y(N)
      INTEGER(4)               :: I
      CHARACTER(8),PARAMETER   :: LIB='ESSL'
!     ******************************************************************
      IF(FAC.EQ.-1.D0) THEN

        CALL DAXPY(N,FAC,X,1,Y,1)

      ELSE
        CALL DAXPY(N,FAC,X,1,Y,1)
      END IF
      RETURN
      END
!
!DGEF(A,LDA,N,IPVT)            MATRIX FACTORIZATION P507
!DGES(A,LDA,N,IPVT,BX)         BX=A^{-1}*BX (USES IPVT FROM DGEF)
!DGESVF                        SINGULAR VALUE DECOMPOSITION P696
!DGESVS  LEAST SQUARES SOLUTION USING SINGULAR VALUE DECOMPOSITION P703
!     ..................................................................
      SUBROUTINE LIB__MATRIXSOLVE(N,M,NEQ,A,X,B)
!     ******************************************************************
!     **  SOLVES THE LINEAR EQUATION SYSTEM AX=B                      **
!     **  WHERE A IS A(N,M)                                           **
!     **  IF A IS NOT SQUARE, THE EQUATION IS SOLVED IN A LEAST       **
!     **  SQUARE SENSE                                                **
!     ******************************************************************
      IMPLICIT NONE
      INTEGER(4),INTENT(IN) :: N
      INTEGER(4),INTENT(IN) :: M
      INTEGER(4),INTENT(IN) :: NEQ
      REAL(8)   ,INTENT(IN) :: A(N,M)
      REAL(8)   ,INTENT(OUT):: X(M,NEQ)
      REAL(8)   ,INTENT(IN) :: B(N,NEQ)
      REAL(8)   ,ALLOCATABLE:: AFACT(:,:)
      REAL(8)   ,ALLOCATABLE:: BFACT(:,:)
      INTEGER(4),ALLOCATABLE:: IPVT(:)
      REAL(8)   ,ALLOCATABLE:: AUX(:)
      REAL(8)   ,ALLOCATABLE:: S(:)
      INTEGER(4)            :: NAUX
      INTEGER(4)            :: NM
      INTEGER(4)            :: IRANK
      REAL(8)               :: TAU=1.D-6
      INTEGER(4)            :: INFO
      INTEGER(4)            :: I
!     ******************************************************************
      IF(N.EQ.M) THEN  !MATRIX FACTORIZATION
        ALLOCATE(AFACT(N,N))
        ALLOCATE(IPVT(N))
        AFACT=A

        CALL DGEFA(AFACT,N,N,IPVT,INFO)
        X=B
        CALL DGESL(AFACT,N,N,IPVT,X,0)

        DEALLOCATE(AFACT)
        DEALLOCATE(IPVT)
      ELSE   !SINGULAR VALUE DECOMPOSITION
        NM=MAX(1,MAX(N,M))

        ALLOCATE(AFACT(N,M))
        AFACT(:,:)=A(:,:)
        ALLOCATE(BFACT(NM,NEQ))
        BFACT(1:N,:)=B(:,:)
        ALLOCATE(S(M))    !SINGULAR VALUES
        NAUX=3*NM+MAX(2*MIN(M,N),NM,NEQ)
        ALLOCATE(AUX(NAUX))
        CALL DGELSS(N,M,NEQ,AFACT,NM,BFACT,N,S,TAU,IRANK,AUX,NAUX,INFO) !->LAPACK
        IF(INFO.NE.0) THEN
          IF(INFO.LT.0) THEN
            CALL ERROR__MSG('INCORRECT ARGUMENT FOR SINGULAR VALUE DECOMPOSITION')
            CALL ERROR__I4VAL('WRONG ARGUMENT NR.',-I)
          ELSE IF(INFO.GT.0) THEN
            CALL ERROR__MSG('SINGULAR VALUE DECOMPOSITION FAILED TO CONVERGE')
            CALL ERROR__I4VAL('#(SING. VALUES WITHOUT CONVERGENCE)',I)
          END IF
          CALL ERROR__STOP('LIB__MATRIXSOLVE')
        END IF
        DEALLOCATE(AUX)
        X(:,:)=BFACT(1:M,:)
        DEALLOCATE(S)
        DEALLOCATE(BFACT)
        DEALLOCATE(AFACT)

      END IF
      RETURN
      END
!
!ISORT  SORTS ELEMENTS IN A SEQUENCE P904
!     .................................................................
      SUBROUTINE LIB__SORTI4(N,X)
!     ******************************************************************
!     **  SORTS THE ARRAY X IN ASCENDING ORDER                        **
!     ******************************************************************
      IMPLICIT NONE
      INTEGER(4),INTENT(IN)   :: N
      INTEGER(4),INTENT(INOUT):: X(N)
      INTEGER(4)              :: IHELP
      INTEGER(4)              :: I,J
!     ******************************************************************

      DO I=1,N-1
        DO J=I+1,N
          IF(X(I).GT.X(J)) THEN
            IHELP=X(I)
            X(I) =X(J)
            X(J) =IHELP
          ENDIF
        ENDDO
      ENDDO

      RETURN
      END
!
!     ..................................................................
      SUBROUTINE LIB__GETHOSTNAME(HOSTNAME)
!     *********************************************************************
!     **  COLLECTS THE HOST NAME OF THE EXECUTING MACHINE                **
!     *********************************************************************
      CHARACTER(*),INTENT(OUT)  :: HOSTNAME
      INTEGER(4)                :: RC
!     *********************************************************************

      HOSTNAME='UNKNOWN'
!     HOSTNM_=GETHOSTNAME(HOSTNAME,%VAL(LEN(HOSTNAME)))

      RETURN
      END
!
!     ......................................................................
      SUBROUTINE LIB__FLUSHFILE(N)
!     *********************************************************************
!     ** FLUSHES THE BUFFER FOR THE FILE CONNECTED TO FORTRAN UNIT N     **
!     *********************************************************************
      INTEGER(4),INTENT(IN) :: N
!     *********************************************************************

      CALL FLUSH(N) ! FROM ABSOFT SUPPORT LIBRARY (UNDERSCORE)

      RETURN
      END
!

!
!     ..................................................................
      SUBROUTINE DGEFA(A,LDA,N,IPVT,INFO)
!     ******************************************************************
!     ** DGEFA FACTORS A DOUBLE PRECISION MATRIX BY GAUSSIAN          **
!     ** ELIMINATION.                                                 **
!     **                                                               **
!     ** DGEFA IS USUALLY CALLED BY DGECO, BUT IT CAN BE CALLED       **
!     ** DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.     **
!     ** (TIME FOR DGECO) = (1 + 9/N)*(TIME FOR DGEFA).               **
!     **                                                              **
!     **  ON RETURN, A IS AN UPPER TRIANGULAR MATRIX                  **
!     **  AND THE MULTIPLIERS, WHICH WERE USED TO OBTAIN IT.          **
!     **  THE FACTORIZATION CAN BE WRITTEN  A = L*U,                  **
!     **  WHERE L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER         **
!     **  TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.            **
!     **                                                              **
!     **  INFO                                                        **
!     **     = 0  NORMAL VALUE.                                       **
!     **     = K  IF U(K,K).EQ.0.0 , THIS IS NOT AN ERROR CONDITION   **
!     **          FOR THIS SUBROUTINE, BUT IT DOES INDICATE           **
!     **          THAT DGESL OR DGEDI WILL DIVIDE BY ZERO IF CALLED.  **
!     **          USE RCOND IN DGECO FOR A RELIABLE INDICATION        **
!     **          OF SINGULARITY.                                     **
!     **                                                              **
!     **  SUBROUTINES AND FUNCTIONS CALLED:                           **
!     **     BLAS ROUTINES: DAXPY,DSCAL,IDAMAX                        **
!     **                                                              **
!     **  LINPACK. THIS VERSION DATED 08/14/78 .                      **
!     **  CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.**
!     ******************************************************************
      IMPLICIT NONE
      INTEGER(4),INTENT(IN)   :: N        ! ORDER OF MATRIX A
      INTEGER(4),INTENT(IN)   :: LDA      ! LEADING DIM. OF MATRIX A
      REAL(8)   ,INTENT(INOUT):: A(LDA,N) ! MATRIX TO BE FACTORED
      INTEGER(4),INTENT(OUT)  :: IPVT(N)  ! PIVOT INDICES
      INTEGER(4),INTENT(OUT)  :: INFO     ! RETURN CODE
      REAL(8)                 :: T
      INTEGER(4)              :: IDAMAX,J,K,KP1,L,NM1
!     ******************************************************************
!     ==================================================================
!     ==  GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING                  ==
!     ==================================================================
      INFO=0
      NM1 =N-1
      IF(NM1.LT.1) THEN
        IPVT(N)=N
         IF(A(N,N).EQ.0.0D0) INFO=N
        RETURN
      END IF
!
      DO K=1,NM1
        KP1=K+1
!       ==  FIND L = PIVOT INDEX ======================================
        L=IDAMAX(N-K+1,A(K,K),1) + K-1  !IDAMAX<-BLAS LIBRARY
        IPVT(K)=L
!       == ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED ======
        IF(A(L,K).NE.0.0D0) THEN
!         ==  INTERCHANGE IF NECESSARY ===============================
          IF(L.NE.K) THEN
            T=A(L,K)
            A(L,K)=A(K,K)
            A(K,K)=T
          END IF
!         == COMPUTE MULTIPLIERS ======================================
          T=-1.0D0/A(K,K)
          CALL DSCAL(N-K,T,A(K+1,K),1)
!         ==  ROW ELIMINATION WITH COLUMN INDEXING  ===================
          DO J=KP1,N
            T=A(L,J)
            IF(L.NE.K) THEN
              A(L,J)=A(K,J)
              A(K,J)=T
            END IF
            CALL DAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)
          ENDDO
        ELSE
          INFO=K
        END IF
      ENDDO
      IPVT(N)=N
      IF(A(N,N).EQ.0.0D0) INFO=N
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE DGESL(A,LDA,N,IPVT,B,JOB)
!     ******************************************************************
!     ** DGESL SOLVES THE DOUBLE PRECISION SYSTEM                     **
!     **       A*X=B  OR  TRANS(A)*X=B                                **
!     **  USING THE FACTORS COMPUTED BY DGECO OR DGEFA.               **
!     **                                                              **
!     **  JOB=0       TO SOLVE  A*X = B ,                             **
!     **     =NONZERO TO SOLVE  TRANS(A)*X = B,                       **
!     **                        ,WHERE TRANS(A)  IS THE TRANSPOSE.    **
!     **                                                              **
!     **  ERROR CONDITION: A DIVISION BY ZERO WILL OCCUR              **
!     **    IF THE INPUT FACTOR  CONTAINS A ZERO ON THE DIAGONAL.     **
!     **    TECHNICALLY THIS INDICATES SINGULARITY BUT IT IS OFTEN    **
!     **    CAUSED BY IMPROPER ARGUMENTS OR IMPROPER SETTING OF LDA.  **
!     **    IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY **
!     **    AND IF DGECO HAS SET RCOND>0. OR DGEFA HAS SET INFO=0.    **
!     **                                                              **
!     **  TO COMPUTE  INVERSE(A)*C WHERE C IS A MATRIX WITH P COLUMNS **
!     **    CALL DGECO(A,LDA,N,IPVT,RCOND,Z)                          **
!     **    IF (RCOND IS TOO SMALL) THEN                              **
!     **      PRINT ERROR MESSAGE AND STOP...                         **
!     **    END IF                                                    **
!     **    DO J=1,P                                                  **
!     **      CALL DGESL(A,LDA,N,IPVT,C(1,J),0)                       **
!     **    ENDDO                                                     **
!     **                                                              **
!     **   SUBROUTINES AND FUNCTIONS CALLED                           **
!     **      BLAS ROUTINES: DAXPY,DDOT                               **
!     **                                                              **
!     **  LINPACK. THIS VERSION DATED 08/14/78 .                      **
!     **  CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.**
!     ******************************************************************
      IMPLICIT NONE
      INTEGER(4),INTENT(IN)   :: N        ! ORDER OF A
      INTEGER(4),INTENT(IN)   :: LDA      ! LEADING DIMENSION OF A
      REAL(8)   ,INTENT(IN)   :: A(LDA,N) ! OUTPUT OF DGECO OR DGEFA
      REAL(8)   ,INTENT(INOUT):: B(N)     ! INPUT:  RIGHT HAND SIDE B
                                          ! OUTPUT: SOLUTION VECTOR X
      INTEGER(4),INTENT(IN)   :: IPVT(N)  ! PIVOT VECTOR FROM DGECO OR DEGFA
      INTEGER(4),INTENT(IN)   :: JOB      ! A*X=B OR TRANS(A)X=B
      REAL(8)                 :: DDOT
      REAL(8)                 :: T
      INTEGER(4)              :: K,KB,L,NM1
!     ******************************************************************
      NM1=N-1
!     ==================================================================
!     == JOB=0: SOLVE A*X=B                                           ==
!     ==================================================================
      IF(JOB.EQ.0) THEN
!       == FIRST SOLVE  L*Y = B ========================================
        IF(NM1.GE.1) THEN
          DO K=1,NM1
            L = IPVT(K)
            T = B(L)
            IF(L.NE.K) THEN
              B(L) = B(K)
              B(K) = T
            END IF
            CALL DAXPY(N-K,T,A(K+1,K),1,B(K+1),1)
          ENDDO
        END IF
!       ==  NOW SOLVE  U*X = Y ==========================================
        DO KB=1,N
          K =N+1-KB
          B(K)=B(K)/A(K,K)
          T =-B(K)
          CALL DAXPY(K-1,T,A(1,K),1,B(1),1)
        ENDDO
!
!     ==================================================================
!     == JOB = NONZERO, SOLVE  TRANS(A) * X = B                       ==
!     ==================================================================
     ELSE

!       == FIRST SOLVE  TRANS(U)*Y=B ===================================
        DO K = 1, N
          T = DDOT(K-1,A(1,K),1,B(1),1)
          B(K) = (B(K) - T)/A(K,K)
        ENDDO
!       == NOW SOLVE TRANS(L)*X = Y =====================================
        IF (NM1.GE.1) THEN
          DO KB=1,NM1
            K = N-KB
            B(K)=B(K) + DDOT(N-K,A(K+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF(L.NE.K) THEN
              T = B(L)
              B(L) = B(K)
              B(K) = T
            END IF
          ENDDO
        END IF
      END IF
      RETURN
      END


!#endif(ibmlicense)










