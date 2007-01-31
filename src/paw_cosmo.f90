MODULE COSMO_MODULE
LOGICAL,SAVE             :: TON=.FALSE.    ! ACTIVATES COSMO
LOGICAL,SAVE             :: TINI=.FALSE. 
LOGICAL,SAVE             :: START=.FALSE. 
LOGICAL,SAVE             :: TISO=.TRUE.   ! TREATS AS ISOLATED CLUSTER/ PERIODIC SYSTEM
LOGICAL,SAVE             :: TSTOP=.FALSE.  ! SETS INITIAL VELOCITIES TO ZERO
LOGICAL,SAVE             :: TADIABATIC=.FALSE.  ! MINIMIZATION IN EACH STEP
LOGICAL,SAVE             :: TMULTIPLE=.FALSE.   !MULTIPLE TIME STEP DYNAMOCS
REAL(8),SAVE             :: eTOL=-1.d0  ! energy TOLERANCE FOR CHARGE-POTENTIAL CONVERGENCE
REAL(8),SAVE             :: qTOL=-1.d0  ! charge TOLERANCE FOR CHARGE-POTENTIAL CONVERGENCE

!==  PARAMETERS THAT DEFINE THE ENERGY FUNCTIONAL
REAL(8),SAVE             :: DT=10.D0        ! TIME STEP
REAL(8),SAVE             :: ANNE=0.D0      ! FRICTION
INTEGER,SAVE             :: NMULTIPLE=30   ! #(MULTIPLE STEPS PER ITERATION)
REAL(8),SAVE             :: QMASS=1.D+5    ! MASS FOR SURFACE CHARGES
REAL(8),SAVE             :: KSELF=10.D0
REAL(8),SAVE             :: GAMMA=6.3739D-6 !SEE SECTION IV.B IN SENN ET AL.
REAL(8),SAVE             :: BETA=2.1567D-3 !SEE SECTION IV.B IN SENN ET AL.
REAL(8),SAVE             :: FDIEL=1.D0    ! (E_R-1)/(E_R+0.5)
INTEGER,SAVE             :: NAT=0
REAL(8),SAVE,ALLOCATABLE :: RSOLV(:) !(NAT) SOLVATION RADIUS
REAL(8),SAVE             :: DISMIN
REAL(8),SAVE             :: vpauli=0.d0
!== 
INTEGER                  :: NQ=0     !#(CHARGES)=SUM(NQAT)
INTEGER,SAVE,ALLOCATABLE :: NQAT(:)
INTEGER,SAVE,ALLOCATABLE :: IQFIRST(:)
REAL(8),SAVE,ALLOCATABLE :: QRELPOS(:,:)
REAL(8),SAVE,ALLOCATABLE :: Q0(:)
REAL(8),SAVE,ALLOCATABLE :: QM(:)
REAL(8),SAVE,ALLOCATABLE :: QP(:)
REAL(8),SAVE,ALLOCATABLE :: cutofftheta(:)
INTEGER,SAVE             :: NPTESS=0
REAL(8),SAVE,ALLOCATABLE :: RTESS(:,:)
!== SASCHA - PRINTOUT CHARGES
LOGICAL(4) ::  TCHARGES
!======
!
! I have introduced a cutofftheta to save theta for printout. This is a fudge.
CONTAINS
!
!     .......................................................................
      FUNCTION ETA(X) RESULT(Y)
!     **                                                                   **
!     **  CUTOFF FUNCTION ETA USED TO BLANK OUT CHARGES IN THE OVERLAP     **
!     **  REGION                                                           **
!     **                                                                   **
      IMPLICIT NONE
      REAL(8) :: X,Y
!     ***********************************************************************
      IF(X.GT.-0.9D0) THEN
        Y= 1.0D0 - DEXP(-(X+0.9D0)**48.D0)
      ELSE
        Y= 0.D0
      ENDIF
      END FUNCTION ETA
!
!     ........................................................................
      FUNCTION DLNETA(X) RESULT(Y)
!     **                                                                   **
!     **  LOGARITHMIC DERIVATIVE OF THE CUTOFF FUNCTION USED TO BLANK OUT  **
!     **  CHARGES IN THE OVERLAP REGION                                    **
!     **      Y=D(LN(ETA(X))/DX                                            **
!     **                                                                   **
      IMPLICIT NONE
      REAL(8) :: X,Y,ARG,SVAR
!     ***********************************************************************
      IF(X.GT.-0.9D0) THEN
        ARG=X+0.9D0
        SVAR=DEXP(-ARG**48.D0)
        Y = 48.D0*ARG**47.D0*SVAR/(1.D0-SVAR)
      ELSE
        Y = 0.D0
      END IF
      END FUNCTION DLNETA
END MODULE COSMO_MODULE
!
!     .......................................................................
      SUBROUTINE COSMO$SETR8A(ID,LEN,VAL)
!     **                                                                   **
      USE COSMO_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      INTEGER(4)  ,INTENT(IN) :: LEN
      REAL(8)     ,INTENT(IN) :: VAL(LEN)
!     ***********************************************************************
      IF(ID.EQ.'RSOLV') THEN
        IF(NAT.EQ.0) NAT=LEN
        IF(LEN.NE.NAT) THEN
          CALL ERROR$MSG('ARRAY SIZE INCONSISTENT')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$STOP('COSMO$SETR8A')
        END IF
        IF(.NOT.ALLOCATED(RSOLV))ALLOCATE(RSOLV(NAT))
        RSOLV(:)=VAL(:)
!       ============================================================      
      ELSE IF(ID.EQ.'Q0') THEN
        IF(NQ.EQ.0) NQ=LEN
        IF(LEN.NE.NQ) THEN
          CALL ERROR$MSG('ARRAY SIZE INCONSISTENT')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$STOP('COSMO$SETR8A')
        END IF
        IF(.NOT.ALLOCATED(Q0))ALLOCATE(Q0(NQ))
        Q0(:)=VAL(:)
!       ============================================================      
      ELSE
        CALL ERROR$MSG('ID NOT RECOGNIZED')
        CALL ERROR$CHVAL('ID',ID)
        CALL ERROR$STOP('COSMO$SETR8A')
      END IF
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO$SETR8(ID,VAL)
!     **                                                                   **
      USE COSMO_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      REAL(8)     ,INTENT(IN) :: VAL
!     ***********************************************************************
      IF(ID.EQ.'DT') THEN
        DT=VAL
      ELSE IF(ID.EQ.'FRICTION') THEN
        ANNE=VAL
      ELSE IF(ID.EQ.'ETOL') THEN
        ETOL=VAL
      ELSE IF(ID.EQ.'QTOL') THEN
        QTOL=VAL
      ELSE IF(ID.EQ.'MASS') THEN
        QMASS=VAL
      ELSE IF(ID.EQ.'GAMMA') THEN
        GAMMA=VAL
      ELSE IF(ID.EQ.'BETA') THEN
        BETA=VAL
      ELSE IF(ID.EQ.'KSELF') THEN
        KSELF=VAL
      ELSE IF(ID.EQ.'EPSILON') THEN
!        FDIEL=(VAL-1)/(VAL+0.5D0)  ! NOT USED
        IF(VAL.LT.1.D-8) THEN
          CALL ERROR$MSG('COSMO WITHOUT DIELECTRIC CONSTANT DOES NOT MAKE SENSE')
          CALL ERROR$MSG('STOPPING TO AVOID DIVIDE BY ZERO')
          CALL ERROR$R8VAL('EPSILONR',VAL)
          CALL ERROR$STOP('COSMO$SETR8')
        END IF
        FDIEL=(VAL-1)/VAL
      ELSE IF(ID.EQ.'VPAULI') THEN
        VPAULI=VAL
      ELSE
        CALL ERROR$MSG('ID NOT RECOGNIZED')
        CALL ERROR$CHVAL('ID',ID)
        CALL ERROR$STOP('COSMO$SETR8')
      END IF
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO$SETI4(ID,VAL)
!     **                                                                   **
      USE COSMO_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      INTEGER(4)  ,INTENT(IN) :: VAL
!     ***********************************************************************
      IF(ID.EQ.'MULTIPLE') THEN
        NMULTIPLE=VAL
        TMULTIPLE=(NMULTIPLE.NE.1)
      ELSE
        CALL ERROR$MSG('ID NOT RECOGNIZED')
        CALL ERROR$CHVAL('ID',ID)
        CALL ERROR$STOP('COSMO$SETI4')
      END IF
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO$SETL4(ID,VAL)
!     **                                                                   **
      USE COSMO_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      LOGICAL(4)  ,INTENT(IN) :: VAL
!     ***********************************************************************
      IF(ID.EQ.'ON') THEN
        TON=VAL
      ELSE IF(ID.EQ.'STOP') THEN
        TSTOP=VAL
      ELSE IF(ID.EQ.'START') THEN
        START=VAL
      ELSE IF(ID.EQ.'ADIABATIC') THEN
        TADIABATIC=VAL
      ELSE IF(ID.EQ.'PERIODIC') THEN
        TISO=.NOT.VAL
      ELSE IF(ID.EQ.'CHARGES') THEN
         TCHARGES=VAL
      ELSE         
        CALL ERROR$MSG('ID NOT RECOGNIZED')
        CALL ERROR$CHVAL('ID',ID)
        CALL ERROR$STOP('COSMO$SETL4')
      END IF
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO$GETL4(ID,VAL)
!     **                                                                   **
      USE COSMO_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      LOGICAL(4)  ,INTENT(OUT):: VAL
!     ***********************************************************************
      IF(ID.EQ.'ON') THEN
        VAL=TON
      ELSE
        CALL ERROR$MSG('ID NOT RECOGNIZED')
        CALL ERROR$CHVAL('ID',ID)
        CALL ERROR$STOP('COSMO$GTL4')
      END IF
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO_INIT()
!     **                                                                   **
!     NAT,NQ,RSOLV,IQFIRST,NQAT,QRELPOS,FDIEL,K,BETA,GAMMA
!     **                                                                   **
      USE COSMO_MODULE
      IMPLICIT NONE
      INTEGER(4)       :: IAT,IQ1,IQ2
      REAL(8)          :: SVAR
      REAL(8)          :: PI      
!     ***********************************************************************
      IF(TINI) RETURN
      PI=4.D0*DATAN(1.D0)
!
!     =======================================================================
!     == CHECK IF TESSELATION FILE HAS BEEN READ                           ==
!     =======================================================================
      IF(NPTESS.EQ.0) THEN
        NPTESS=60
        ALLOCATE(RTESS(3,NPTESS))
        CALL COSMO_TESSELATIONFILE(NPTESS,RTESS)
      END IF
!
!     =======================================================================
!     == CHECK CONSISTENCY OF DIMENSIONS AND COMPLETE                       ==
!     =======================================================================
      IF(NAT.EQ.0) THEN
        CALL ERROR$MSG('INTERNAL VALUE FOR NAT NOT SET')
        CALL ERROR$STOP('COSMO_INIT')
      END IF
      IF(.NOT.ALLOCATED(RSOLV)) THEN
        CALL ERROR$MSG('RSOLV HAS NOT BEEN SET')
        CALL ERROR$STOP('COSMO_INIT')
      END IF
      IF(NQ.EQ.0)THEN 
        NQ=NAT*NPTESS
      END IF
      IF(NQ.NE.NAT*NPTESS) THEN
        CALL ERROR$MSG('NQ INCONSISTENT WITH NAT AND NPTESS')
        CALL ERROR$STOP('COSMO_INIT')
      END IF
      IF(.NOT.ALLOCATED(Q0)) THEN
        ALLOCATE(Q0(NQ))
        Q0(:)=0.D0
      END IF
      IF(.NOT.ALLOCATED(QM)) THEN
        ALLOCATE(QM(NQ))
        QM(:)=Q0(:)
      END IF
      IF(.NOT.ALLOCATED(QP)) THEN
        ALLOCATE(QP(NQ))
        QP(:)=0.D0
      END IF
!
!     =======================================================================
!     == OBTAIN TESSELATION INFORMATION                                    ==
!     =======================================================================
      ALLOCATE(NQAT(NAT))
      ALLOCATE(IQFIRST(NAT))
      ALLOCATE(QRELPOS(3,NQ))
      DO IAT=1,NAT
        IQ1=1+NPTESS*(IAT-1)
        IQ2=NPTESS*IAT
        IQFIRST(IAT)=IQ1
        NQAT(IAT)=NPTESS
        QRELPOS(:,IQ1:IQ2)=RTESS(:,:)*RSOLV(IAT)
      ENDDO
!
!     =======================================================================
!     == DETERMINE CUTOOFF FOR NEARBY SURFACE CHARGES                      ==
!     =======================================================================
      SVAR=4.D0*PI*SUM(RSOLV(:)**2)/REAL(NQ)
      DISMIN=2.D0*FDIEL/(2.D0*SQRT(SVAR))/(1.07D0*SQRT(PI))
      TINI=.TRUE.
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO$READ(NFIL,NFILO,TCHK)
!     **                                                                   **
!     **                                                                   **
      USE COSMO_MODULE
      USE RESTART_INTERFACE
      USE MPE_MODULE
      IMPLICIT NONE
      INTEGER(4)            ,INTENT(IN)  :: NFIL
      INTEGER(4)            ,INTENT(IN)  :: NFILO
      LOGICAL(4)            ,INTENT(OUT) :: TCHK
      TYPE (SEPARATOR_TYPE),PARAMETER    :: MYSEPARATOR &
                 =SEPARATOR_TYPE(2,'COSMO','NONE','JAN2006','NONE')
      TYPE (SEPARATOR_TYPE)              :: SEPARATOR
      INTEGER(4)                         :: NTASKS,THISTASK
!     ***********************************************************************
      IF(.NOT.TON) RETURN
      CALL MPE$QUERY('MONOMER',NTASKS,THISTASK)
      TCHK=.NOT.START
      SEPARATOR=MYSEPARATOR
      IF(THISTASK.EQ.1)CALL RESTART$READSEPARATOR(SEPARATOR,NFIL,NFILO,TCHK)
      CALL MPE$BROADCAST('MONOMER',1,TCHK)
      IF(.NOT.TCHK) RETURN
!
!     =======================================================================
!     ==  READ CHARGES                                                     ==
!     =======================================================================
      IF(THISTASK.EQ.1) THEN
        IF(SEPARATOR%VERSION.NE.MYSEPARATOR%VERSION) THEN
          CALL ERROR$MSG('VERSION CONFLICT')
          CALL ERROR$STOP('COSMO$READ')
        END IF
        READ(NFIL)NQ,NAT
        ALLOCATE(Q0(NQ))
        ALLOCATE(QM(NQ))
        READ(NFIL)Q0,QM
      END IF
      CALL MPE$BROADCAST('MONOMER',1,Q0)
      CALL MPE$BROADCAST('MONOMER',1,QM)
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO$WRITE(NFIL,NFILO,TCHK)
!     **                                                                   **
!     **                                                                   **
      USE COSMO_MODULE
      USE RESTART_INTERFACE
      USE MPE_MODULE
      IMPLICIT NONE
      INTEGER(4)            ,INTENT(IN)  :: NFIL
      INTEGER(4)            ,INTENT(IN)  :: NFILO
      LOGICAL(4)            ,INTENT(OUT) :: TCHK
      TYPE (SEPARATOR_TYPE),PARAMETER    :: MYSEPARATOR &
                 =SEPARATOR_TYPE(2,'COSMO','NONE','JAN2006','NONE')
      INTEGER(4)                         :: NTASKS,THISTASK
!     ***********************************************************************
      IF(.NOT.TON) RETURN
      CALL MPE$QUERY('MONOMER',NTASKS,THISTASK)
!
!     =======================================================================
!     ==  WRITE CHARGES                                                    ==
!     =======================================================================
      IF(THISTASK.EQ.1) THEN
        CALL RESTART$WRITESEPARATOR(MYSEPARATOR,NFIL,NFILO,TCHK)
        WRITE(NFIL)NQ,NAT
        WRITE(NFIL)Q0,QM
      END IF
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO$READTESSELATION(FILEID)
      USE COSMO_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: FILEID
      LOGICAL                 :: TCHK
      REAL(8)                 :: R(3)
!     ***********************************************************************
      CALL COSMO_READTESSELATION(FILEID,1,NPTESS,R,TCHK)
      IF(ALLOCATED(RTESS)) THEN
        CALL ERROR$MSG('TESSELATION FILE MUST ONLY BE READ IN ONLY ONCE!')
        CALL ERROR$STOP('COSMO$READTESSELATION')
      END IF
      ALLOCATE(RTESS(3,NPTESS))
      CALL COSMO_READTESSELATION(FILEID,NPTESS,NPTESS,RTESS,TCHK)
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO_READTESSELATION(FILEID,NX,N,R,TCHK)
!     **                                                                   **
!     **  READ TESSELATION FILE                                            **
!     **                                                                   **
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: FILEID
      INTEGER     ,INTENT(IN) :: NX
      LOGICAL     ,INTENT(OUT):: TCHK  ! IF FALSE, NX WAS TOO SMALL. TRY AGAIN WITH NX=N
      INTEGER     ,INTENT(OUT):: N
      REAL(8)     ,INTENT(OUT):: R(3,NX)
      INTEGER(4)              :: NFIL
      INTEGER                 :: NC ! #(CORNERS)
      REAL(8)     ,ALLOCATABLE:: C(:,:) !CORNERS
      INTEGER                 :: I,ISVAR,I1,I2,I3       
!     ***********************************************************************
      CALL FILEHANDLER$UNIT(FILEID,NFIL)
      REWIND(NFIL)
      READ(NFIL,*)NC
      READ(NFIL,*)N
      IF(N.GT.NX) THEN
        TCHK=.FALSE.
        R(:,:)=0.D0
        RETURN
      ELSE
        TCHK=.TRUE.
      END IF
!
!     =======================================================================
!     == READ THE FACE-CORNERS                                             ==
!     =======================================================================
      ALLOCATE(C(3,NC))
      DO I=1,NC
        READ(NFIL,*)C(:,I)
      ENDDO
!
!     =======================================================================
!     == DETERMINE FACE CENTERS                                            ==
!     =======================================================================
      DO I=1,N
        READ(NFIL,*)ISVAR,I1,I2,I3
        IF(ISVAR.NE.I.OR.I1.GT.N.OR.I2.GT.N.OR.I3.GT.N) THEN
          CALL ERROR$STOP('TESSELATION FILE CORRUPTES')
          CALL ERROR$STOP('COSMO_READTESSELATION')
        END IF
        R(:,I)=C(:,I1)+C(:,I2)+C(:,I3)
        R(:,I)=R(:,I)/SQRT(SUM(R(:,I)**2))
      ENDDO
      DEALLOCATE(C)
      CALL FILEHANDLER$CLOSE(FILEID)
!== THIS IS FOR CREATING THE INPUT FOR COSMO_TESSELATIONFILE ================
!!$PRINT*,N
!!$DO I=1,N
!!$WRITE(*,FMT='("R(:,",I3,")=(/",F10.5,",",F10.5,",",F10.5"/)")')I,R(:,I)
!!$ENDDO
!!$STOP
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO_TESSELATIONFILE(N,R)
!     **                                                                   **
!     **                                                                   **
      INTEGER,INTENT(IN)  :: N
      REAL(8),INTENT(OUT) :: R(3,N)
      INTEGER             :: I
      REAL(8)             :: SVAR
!     ***********************************************************************
      IF(N.EQ.60) THEN
        R(:,  1)=(/   0.10960,   0.33732,   0.93499/)
        R(:,  2)=(/   0.35468,   0.00000,   0.93499/)
        R(:,  3)=(/  -0.28695,   0.20848,   0.93499/)
        R(:,  4)=(/  -0.28695,  -0.20848,   0.93499/)
        R(:,  5)=(/   0.10960,  -0.33732,   0.93499/)
        R(:,  6)=(/   0.28695,   0.20848,  -0.93499/)
        R(:,  7)=(/   0.28695,  -0.20848,  -0.93499/)
        R(:,  8)=(/  -0.10960,   0.33732,  -0.93499/)
        R(:,  9)=(/  -0.35468,   0.00000,  -0.93499/)
        R(:, 10)=(/  -0.10960,  -0.33732,  -0.93499/)
        R(:, 11)=(/   0.20941,   0.64449,   0.73538/)
        R(:, 12)=(/   0.67766,   0.00000,   0.73538/)
        R(:, 13)=(/   0.78726,   0.33732,   0.51617/)
        R(:, 14)=(/   0.56409,   0.64449,   0.51617/)
        R(:, 15)=(/  -0.54824,   0.39832,   0.73538/)
        R(:, 16)=(/  -0.07754,   0.85297,   0.51617/)
        R(:, 17)=(/  -0.43863,   0.73564,   0.51617/)
        R(:, 18)=(/  -0.54824,  -0.39832,   0.73538/)
        R(:, 19)=(/  -0.83518,   0.18984,   0.51617/)
        R(:, 20)=(/  -0.83518,  -0.18984,   0.51617/)
        R(:, 21)=(/   0.20941,  -0.64449,   0.73538/)
        R(:, 22)=(/  -0.43863,  -0.73564,   0.51617/)
        R(:, 23)=(/  -0.07754,  -0.85297,   0.51617/)
        R(:, 24)=(/   0.78726,  -0.33732,   0.51617/)
        R(:, 25)=(/   0.56409,  -0.64449,   0.51617/)
        R(:, 26)=(/   0.96460,   0.20848,   0.16149/)
        R(:, 27)=(/   0.96460,  -0.20848,   0.16149/)
        R(:, 28)=(/   0.49635,   0.85297,   0.16149/)
        R(:, 29)=(/   0.09980,   0.98182,   0.16149/)
        R(:, 30)=(/  -0.65784,   0.73564,   0.16149/)
        R(:, 31)=(/  -0.90292,   0.39832,   0.16149/)
        R(:, 32)=(/  -0.90292,  -0.39832,   0.16149/)
        R(:, 33)=(/  -0.65784,  -0.73564,   0.16149/)
        R(:, 34)=(/   0.09980,  -0.98182,   0.16149/)
        R(:, 35)=(/   0.49635,  -0.85297,   0.16149/)
        R(:, 36)=(/   0.90292,   0.39832,  -0.16149/)
        R(:, 37)=(/   0.65784,   0.73564,  -0.16149/)
        R(:, 38)=(/  -0.09980,   0.98182,  -0.16149/)
        R(:, 39)=(/  -0.49635,   0.85297,  -0.16149/)
        R(:, 40)=(/  -0.96460,   0.20848,  -0.16149/)
        R(:, 41)=(/  -0.96460,  -0.20848,  -0.16149/)
        R(:, 42)=(/  -0.49635,  -0.85297,  -0.16149/)
        R(:, 43)=(/  -0.09980,  -0.98182,  -0.16149/)
        R(:, 44)=(/   0.90292,  -0.39832,  -0.16149/)
        R(:, 45)=(/   0.65784,  -0.73564,  -0.16149/)
        R(:, 46)=(/   0.83518,   0.18984,  -0.51617/)
        R(:, 47)=(/   0.83518,  -0.18984,  -0.51617/)
        R(:, 48)=(/   0.43863,   0.73564,  -0.51617/)
        R(:, 49)=(/   0.07754,   0.85297,  -0.51617/)
        R(:, 50)=(/  -0.56409,   0.64449,  -0.51617/)
        R(:, 51)=(/  -0.78726,   0.33732,  -0.51617/)
        R(:, 52)=(/  -0.78726,  -0.33732,  -0.51617/)
        R(:, 53)=(/  -0.56409,  -0.64449,  -0.51617/)
        R(:, 54)=(/   0.07754,  -0.85297,  -0.51617/)
        R(:, 55)=(/   0.43863,  -0.73564,  -0.51617/)
        R(:, 56)=(/   0.54824,   0.39832,  -0.73538/)
        R(:, 57)=(/  -0.20941,   0.64449,  -0.73538/)
        R(:, 58)=(/  -0.67766,   0.00000,  -0.73538/)
        R(:, 59)=(/  -0.20941,  -0.64449,  -0.73538/)
        R(:, 60)=(/   0.54824,  -0.39832,  -0.73538/)
      ELSE
        CALL ERROR$MSG('NUMBER OF GRID POINTS INCONSISTENT')
        CALL ERROR$MSG('WITH AVAILABLE TESSELATIONS')
        CALL ERROR$I4VAL('N',N)
        CALL ERROR$STOP('COSMO_TESSELATIONFILE')
      END IF
!
!     =======================================================================
!     == RENORMALIZE TO COMPESATE ROUNDING ERRORS                          ==
!     =======================================================================
      DO I=1,N
        SVAR=1.D0/SQRT(SUM(R(:,I)**2))
        R(:,I)=R(:,I)*SVAR
      ENDDO
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO_NEIGHBORLIST(TISO,NAT,RAT,RBAS,RMAX,NNX,NNN,NNLIST)
!     **                                                                   **
!     **  SETS UP A NEIGBORLIST OF ALL ATOMS WITHIN A DISTANCE OF RC       **
!     **                                                                   **
!     **                                                                   **
      IMPLICIT NONE
      LOGICAL,INTENT(IN)  :: TISO       ! ISOLATED CLUSTER/PERIODIC SYSTEM
      INTEGER,INTENT(IN)  :: NAT        ! #(ATOMS)
      REAL(8),INTENT(IN)  :: RAT(3,NAT) ! ATOMIC POSITIONS
      REAL(8),INTENT(IN)  :: RBAS(3,3)  ! LATTICE VECTORS
      REAL(8),INTENT(IN)  :: RMAX       ! CUTOFF RADIUS FOR NEIGBORLIST
      INTEGER,INTENT(IN)  :: NNX        ! X( #(NEIGBORS)/ATOM )
      INTEGER,INTENT(OUT) :: NNN(NAT)   ! #(NEIGBORS) FOR A GIVEN ATOM
      INTEGER,INTENT(OUT) :: NNLIST(4,NNX,NAT)
      INTEGER             :: IFOLD(3,NAT)
      REAL(8)             :: RATF(3,NAT)
      REAL(8)             :: XAT(3)     ! POSITION IN RELATIVE COORDINATES
      REAL(8)             :: RBASIN(3,3)! RBAS**(-1)
      REAL(8)             :: DR12(3)    ! DISTANCE VECTOR
      REAL(8)             :: DR(3)
      REAL(8)             :: DT1(3)
      INTEGER(4)          :: MIN1,MAX1,MIN2,MAX2,MIN3,MAX3
      REAL(8)             :: DIS
      REAL(8)             :: DISARR(NNX,NAT)
      INTEGER             :: IWORK(4)
      INTEGER(4)          :: FROM,TO
      INTEGER(4)          :: ISVAR
      INTEGER             :: IN,IAT1,IAT2,IT1,IT2,IT3
!     ***********************************************************************
!
!     =======================================================================
!     ==  FOLD ATOMS INTO FIRST UNIT CELL                                  ==
!     =======================================================================
      RATF(:,:)=RAT(:,:)
      IFOLD(:,:)=0
      IF(.NOT.TISO) THEN
        CALL LIB$INVERTR8(3,RBAS,RBASIN)
        DO IAT1=1,NAT
          XAT(:)=MATMUL(RBASIN,RAT(:,IAT1))
          IFOLD(:,IAT1)=INT(XAT(:)+1000.D0)-1000
          XAT(:)=XAT(:)-REAL(IFOLD(:,IAT1))
          RATF(:,IAT1)=MATMUL(RBAS,XAT)
        ENDDO
      END IF
!
!     =======================================================================
!     ==  DETERMINE DISTANCE MATRIX AND SORT NEIGGHBORS                    ==
!     =======================================================================
      NNN(:)=0.D0
      DISARR(:,:)=HUGE(DISARR)
      NNLIST(:,:,:)=0
      DO IAT1=1,NAT
        DO IAT2=1,IAT1
          DR12=RATF(:,IAT2)-RATF(:,IAT1)
          IF(TISO) THEN
            MIN1=0
            MAX1=0
            MIN2=0
            MAX2=0
            MIN3=0
            MAX3=0
          ELSE
            CALL BOXSPH(RBAS,-DR12(1),-DR12(2),-DR12(3),RMAX &
    &                ,MIN1,MAX1,MIN2,MAX2,MIN3,MAX3)
          END IF     
          DO IT1=MIN1,MAX1
            DO IT2=MIN2,MAX2
              DT1(:)=RBAS(:,1)*REAL(IT1)+RBAS(:,2)*REAL(IT2)
              DO IT3=MIN3,MAX3
                DR(:)=DR12(:)+DT1(:)+RBAS(:,3)*REAL(IT3)
                DIS=SQRT(SUM(DR**2))  ! |R2+T-R1|
                IF(DIS.GT.RMAX) CYCLE
                IF(DIS.LT.1.D-8) THEN
                  IF(IAT2.NE.IAT1.OR.IT1.NE.0.OR.IT2.NE.0.OR.IT3.NE.0) THEN
                    CALL ERROR$MSG('TWO ATOMS ON SAME POSITION')
                    CALL ERROR$I4VAL('IAT1',IAT1)
                    CALL ERROR$I4VAL('IAT2',IAT2)
                    CALL ERROR$STOP('COSMO$NEIGHBORLIST')
                  END IF
                  CYCLE
                END IF
!               ============================================================
                IN=NNN(IAT1)+1
                IF(IN.GT.NNX) THEN
                  CALL ERROR$MSG('ARRAY FOR NEIGHBORLIST TOO SMALL')
                  CALL ERROR$STOP('COSMO$NEIGHBORLIST')
                END IF
                NNLIST(1,IN,IAT1)=IAT2
                NNLIST(2,IN,IAT1)=IT1
                NNLIST(2,IN,IAT1)=IT2
                NNLIST(4,IN,IAT1)=IT3
                DISARR(IN,IAT1)=DIS
                NNN(IAT1)=IN
!               ============================================================
                IN=NNN(IAT2)+1
                IF(IN.GT.NNX) THEN
                  CALL ERROR$MSG('ARRAY FOR NEIGHBORLIST TOO SMALL')
                  CALL ERROR$STOP('COSMO$NEIGHBORLIST')
                END IF
                NNLIST(1,IN,IAT2)=IAT1
                NNLIST(2,IN,IAT2)=-IT1
                NNLIST(3,IN,IAT2)=-IT2
                NNLIST(4,IN,IAT2)=-IT3
                DISARR(IN,IAT2)=DIS
                NNN(IAT2)=IN
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO
!
!     ============================================================================
!     == UNFOLD POSITIONS                                                       ==
!     ============================================================================
      IF(.NOT.TISO) THEN
        DO IAT1=1,NAT
          DO IN=1,NNN(IAT1)
            IAT2=NNLIST(1,IN,IAT1)
            NNLIST(2:4,IN,IAT1)=NNLIST(2:4,IN,IAT1)+IFOLD(:,IAT2)-IFOLD(:,IAT1)
          ENDDO
        ENDDO
      END IF
!
!     ============================================================================
!     == SORT NEIGBORLIST SO THAT NEAREST NEIGHBORS ARE FIRST                   ==
!     == THIS BLANKS OUT AS MANY CHARGES EARLY ON AND SPEEDS UP                ==
!     ============================================================================
      DO IAT1=1,NAT
        ISVAR=NNN(IAT1)  ! CONVERSION TO DEFAULT INTEGER
        CALL SORT$SET(ISVAR,DISARR(:NNN(IAT1),IAT1))
        CALL SORT$RESTART
        CALL SORT$FLIP(FROM,TO)
        DO WHILE (FROM.NE.0.OR.TO.NE.0)
          IF(TO.EQ.0) THEN
            IWORK(:)=NNLIST(:,FROM,IAT1)
            DIS=DISARR(FROM,IAT1)
          ELSE IF(FROM.EQ.0) THEN
            NNLIST(:,TO,IAT1)=IWORK(:)
            DISARR(TO,IAT1)=DIS
          ELSE
            NNLIST(:,TO,IAT1)=NNLIST(:,FROM,IAT1)
            DISARR(TO,IAT1)=DISARR(FROM,IAT1)
          END IF
          CALL SORT$FLIP(FROM,TO)
        ENDDO
        CALL SORT$UNSET
      ENDDO
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO_CUTOFF(NAT,NQ,RAT,RBAS,RSOLV,NNX,NNN,NNLIST,IQFIRST,NQAT,RQ &
     &                       ,ZEROTHETA,THETA)
!     **                                                                   **
!     **  CALCULATE THETA, THE CUTOFF FUNCTION                             **
!     **  CHARGES THAT ARE COMPLETELY BLANKED OUT ARE IDENTIFIED BY ZEROTHETA
!     **                                                                   **
      USE COSMO_MODULE, ONLY : ETA
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: NAT
      INTEGER,INTENT(IN) :: NQ
      INTEGER,INTENT(IN) :: NNX
      INTEGER,INTENT(IN) :: NNN(NAT)
      INTEGER,INTENT(IN) :: NNLIST(4,NNX,NAT)
      REAL(8),INTENT(IN) :: RAT(3,NAT)
      REAL(8),INTENT(IN) :: RBAS(3,3)
      REAL(8),INTENT(IN) :: RSOLV(NAT)
      INTEGER,INTENT(IN) :: IQFIRST(NAT)
      INTEGER,INTENT(IN) :: NQAT(NAT)
      REAL(8),INTENT(IN) :: RQ(3,NQ)     ! ABSOLUTE POSITIONS OF CHARGE
      LOGICAL,INTENT(OUT):: ZEROTHETA(NQ)
      REAL(8),INTENT(OUT):: THETA(NQ)
      REAL(8),PARAMETER  :: DRSOLVPLUS=0.2D0   !1.D0     
      REAL(8),PARAMETER  :: DRSOLVMINUS=0.05D0 !0.9D0    
      INTEGER            :: IAT1,IAT,IAT2,IQ
      REAL(8)            :: DR(3),DIS
      REAL(8)            :: R2(3)
      LOGICAL            :: TFAR(NNX)
      REAL(8),ALLOCATABLE:: DISARR(:,:)
      LOGICAL            :: TALLZERO ! IDENTIFIES AN ATOM WITH ALL CHARGES 
                                     ! BLANKED OUT
      INTEGER(4)         :: ISVAR,IQ1,IQ2,IN,IT1,IT2,IT3
!     ***********************************************************************
      ISVAR=MAXVAL(NQAT)
      ALLOCATE(DISARR(ISVAR,NNX))
      ZEROTHETA(:)=.FALSE.
      THETA(:)=1.D0
      DO IAT1=1,NAT
!
!       ===================================================================
!       ==  ZERO OUT CHARGES IN THE OVERLAP REGION,                      ==
!       ==  CALCULATE DISTANCE MATRIX AND BLANK OUT DISTANT ATOMS        ==
!       ===================================================================
        TFAR(:)=.FALSE.
        DISARR(:,:)=0.D0
        TALLZERO=.FALSE.
        IQ1=IQFIRST(IAT1)
        IQ2=IQFIRST(IAT1)-1+NQAT(IAT1)
        DO IN=1,NNN(IAT1)
          IAT2=NNLIST(1,IN,IAT1)
          IT1 =NNLIST(2,IN,IAT1)
          IT2 =NNLIST(3,IN,IAT1)
          IT3 =NNLIST(4,IN,IAT1)
          R2(:)=RAT(:,IAT2) &
     &         +RBAS(:,1)*REAL(IT1)+RBAS(:,2)*REAL(IT2)+RBAS(:,3)*REAL(IT3)
          DR(:)=R2(:)-RAT(:,IAT1)
          DIS=SQRT(SUM(DR**2))
          IF(DIS.GT.RSOLV(IAT2)+RSOLV(IAT1)+DRSOLVPLUS) THEN
            TFAR(IN)=.TRUE.
            CYCLE             ! SOLVATION SPHERES DO NOT OVERLAP 
          END IF
          TALLZERO=.TRUE.
          DO IQ=IQ1,IQ2
            IF(ZEROTHETA(IQ)) CYCLE
            DIS=SQRT(SUM((RQ(:,IQ)-R2(:))**2))
            ZEROTHETA(IQ)=(DIS.LT.RSOLV(IAT2)-DRSOLVMINUS)
            IF(ZEROTHETA(IQ)) CYCLE
            DISARR(IQ-IQ1+1,IN)=DIS
            TALLZERO=.FALSE.
          ENDDO
        ENDDO
!
!       == ALL CHARGES ON THIS ATOM BLANKED OUT, CONSIDER NEXT ONE
        IF(TALLZERO) CYCLE
!
!       ===================================================================
!       ==  CALCULATE CUTOFF FUNCTION 
!       ===================================================================
        DO IN=1,NNN(IAT1)
          IF(TFAR(IN)) CYCLE
          IAT2=NNLIST(1,IN,IAT1)
          DO IQ=IQ1,IQ2
            IF(ZEROTHETA(IQ)) CYCLE
            DIS=DISARR(IQ-IQ1+1,IN)
            IF(DIS.GT.RSOLV(IAT2)+DRSOLVPLUS) CYCLE ! CONTRIBUTION IS 1; CONTINUE ..
            THETA(IQ)=THETA(IQ)*ETA(DIS-RSOLV(IAT2)) 
          ENDDO
        ENDDO
      ENDDO
!
!     =====================================================================
!     == SET CHARGES TO ZERO THAT ARE BLANKED OUT                        ==
!     =====================================================================
      IAT=0
      DO IQ=1,NQ
        IF(ZEROTHETA(IQ)) THETA(IQ)=0.D0
        IF(ZEROTHETA(IQ)) IAT=IAT+1
      ENDDO
!     PRINT*,'FRACTION OF NONZERO CHARGES ',REAL(IAT)/REAL(NQ)
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO_THETAFORCE(NAT,NQ,RAT,RBAS,RSOLV,NNX,NNN,NNLIST,IQFIRST,NQAT,RQ &
     &                       ,ZEROTHETA,THETA,VTHETA,FAT)
!     **                                                                   **
!     **  CALCULATE THETA, THE CUTOFF FUNCTION                             **
!     **  CHARGES THAT ARE COMPLETELY BLANKED OUT ARE IDENTIFIED BY ZEROTHETA
!     **                                                                   **
      USE COSMO_MODULE, ONLY : DLNETA
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: NAT
      INTEGER,INTENT(IN) :: NQ
      INTEGER,INTENT(IN) :: NNX
      INTEGER,INTENT(IN) :: NNN(NAT)
      INTEGER,INTENT(IN) :: NNLIST(4,NNX,NAT)
      REAL(8),INTENT(IN) :: RAT(3,NAT)
      REAL(8),INTENT(IN) :: RBAS(3,3)
      REAL(8),INTENT(IN) :: RSOLV(NAT)
      INTEGER,INTENT(IN) :: IQFIRST(NAT)
      INTEGER,INTENT(IN) :: NQAT(NAT)
      REAL(8),INTENT(IN) :: RQ(3,NQ)     ! ABSOLUTE POSITIONS OF CHARGE
      LOGICAL,INTENT(in) :: ZEROTHETA(NQ)
      REAL(8),INTENT(IN) :: THETA(NQ)
      REAL(8),INTENT(IN) :: VTHETA(NQ)
      REAL(8),INTENT(OUT):: FAT(3,NAT)
      REAL(8),PARAMETER  :: DRSOLVPLUS=0.2D0   !1.D0     
      REAL(8),PARAMETER  :: DRSOLVMINUS=0.05D0 !0.9D0    
      INTEGER            :: IAT1,IAT2,IQ
      REAL(8)            :: DR(3),DIS
      REAL(8)            :: R2(3)
      REAL(8)            :: SVAR
      INTEGER(4)         :: IQ1,IQ2,IN,IT1,IT2,IT3
!     ***********************************************************************
      FAT(:,:)=0.D0
      DO IAT1=1,NAT
        IQ1=IQFIRST(IAT1)
        IQ2=IQFIRST(IAT1)-1+NQAT(IAT1)
        DO IN=1,NNN(IAT1)
          IAT2=NNLIST(1,IN,IAT1)
          IT1 =NNLIST(2,IN,IAT1)
          IT2 =NNLIST(3,IN,IAT1)
          IT3 =NNLIST(4,IN,IAT1)
          R2(:)=RAT(:,IAT2) &
       &         +RBAS(:,1)*REAL(IT1)+RBAS(:,2)*REAL(IT2)+RBAS(:,3)*REAL(IT3)
          DR(:)=R2(:)-RAT(:,IAT1)
          DIS=SQRT(SUM(DR**2))
          IF(DIS.GT.RSOLV(IAT2)+RSOLV(IAT1)+DRSOLVPLUS) CYCLE
          DO IQ=IQ1,IQ2
            IF(ZEROTHETA(IQ)) CYCLE
            DR(:)=R2(:)-RQ(:,IQ)
            DIS=SQRT(SUM(DR**2))
            IF(DIS.GT.RSOLV(IAT2)+DRSOLVPLUS) CYCLE ! CONTRIBUTION IS 1; CONTINUE ..
            SVAR=VTHETA(IQ)*THETA(IQ)*DLNETA(DIS-RSOLV(IAT2))/DIS 
            FAT(:,IAT2)=FAT(:,IAT2)-SVAR*DR(:)
            FAT(:,IAT1)=FAT(:,IAT1)+SVAR*DR(:)
          ENDDO
        ENDDO
      ENDDO
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO_Pauli(NAT,ng,rc,qmad,NQ,RAT,RBAS,NNX,NNN,NNLIST,IQFIRST,NQAT,RQ &
     &                       ,ZEROTHETA,vpauli,epauli,FAT,vmad)
!     **                                                                   **
!     **  CALCULATES ENERGY CORRESPONDING TO A POTENTIAL ON THE SURFACE    **
!     **  CHARGES THAT ACT ON THE MODEL CHARE DENSITY                      **
!     **  this potential shall mimick the pauli repulsion by the solvent.  **
!     **                                                                   **
!     **                                                                   ** 
     IMPLICIT NONE
      INTEGER,INTENT(IN) :: NAT
      INTEGER,INTENT(IN) :: ng
      real(8),intent(in) :: rc(ng,nat)
      real(8),intent(in) :: qmad(ng,nat)
      INTEGER,INTENT(IN) :: NQ
      INTEGER,INTENT(IN) :: NNX
      INTEGER,INTENT(IN) :: NNN(NAT)
      INTEGER,INTENT(IN) :: NNLIST(4,NNX,NAT)
      REAL(8),INTENT(IN) :: RAT(3,NAT)
      REAL(8),INTENT(IN) :: RBAS(3,3)
      INTEGER,INTENT(IN) :: IQFIRST(NAT)
      INTEGER,INTENT(IN) :: NQAT(NAT)
      REAL(8),INTENT(IN) :: RQ(3,NQ)     ! ABSOLUTE POSITIONS OF CHARGE
      LOGICAL,INTENT(in) :: ZEROTHETA(NQ)
      REAL(8),INTENT(in) :: vpauli
      REAL(8),INTENT(OUT):: epauli
      REAL(8),INTENT(OUT):: FAT(3,NAT)
      REAL(8),INTENT(OUT):: vmad(ng,NAT)
      INTEGER            :: IAT1,IAT2,IQ
      REAL(8)            :: DR(3),DIS
      REAL(8)            :: R2(3)
      REAL(8)            :: SVAR
      INTEGER(4)         :: IQ1,IQ2,IN,IT1,IT2,IT3,ig
      real(8)            :: ffac,rc1
      real(8)            :: pi,sqpi
!     ***********************************************************************
      pi=4.d0*datan(1.d0)
      sqpi=sqrt(pi)
      epauli=0.D0
      FAT(:,:)=0.D0
      vmad(:,:)=0.D0
      DO IAT1=1,NAT
        IQ1=IQFIRST(IAT1)
        IQ2=IQFIRST(IAT1)-1+NQAT(IAT1)
        DO IN=1,NNN(IAT1)
          IAT2=NNLIST(1,IN,IAT1)
          IT1 =NNLIST(2,IN,IAT1)
          IT2 =NNLIST(3,IN,IAT1)
          IT3 =NNLIST(4,IN,IAT1)
          R2(:)=RAT(:,IAT2) &
       &         +RBAS(:,1)*REAL(IT1)+RBAS(:,2)*REAL(IT2)+RBAS(:,3)*REAL(IT3)
          DO IQ=IQ1,IQ2
            IF(ZEROTHETA(IQ)) CYCLE
            DR(:)=R2(:)-RQ(:,IQ)
            DIS=SQRT(SUM(DR**2))
            ffac=0.d0
            do ig=1,ng
              rc1=rc(ig,iat2)
              svar=vpauli/(sqpi*rc1)**3*exp(-(dis/rc1)**2)
              vmad(ig,iat2)=vmad(ig,iat2)+svar
              svar=svar*qmad(ig,iat2)
              epauli=epauli+svar
              ffac=ffac-2.d0/rc1**2*svar
            enddo
            FAT(:,IAT2)=FAT(:,IAT2)-SVAR*DR(:)
            FAT(:,IAT1)=FAT(:,IAT1)+SVAR*DR(:)
          ENDDO
        ENDDO
      ENDDO
do iat1=1,nat
  write(*,*)'qmad ',iat1,qmad(:,iat1)
enddo
do iat1=1,nat
  write(*,*)'rc ',iat1,rc(:,iat1)
enddo
do iat1=1,nat
  write(*,*)'vmad ',iat1,vmad(:,iat1)
enddo
print*,'epauli ',epauli
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO_LONGRANGE(TISO,RBAS,NQ,NAT,DISMIN,ZEROTHETA &
     &                     ,QBAR,RQ,QATBAR,RAT,ETOT,VQBAR,FQ,VATBAR,FAT)
!     **                                                                   **
!     **  CALCULATES THE LONG-RANGED COULOMB INTERACTION OF SURFACE CHARGES**
!     **  AND ATOMIC CHARGES. THE INTERACTION BETWEEN ATOMIC CHARGES IS    **
!     **  REMOVED AGAIN.                                                   **
!     **  THE CHARGES QBAR ARE MULTIPLIED WITH THETA, I.E. QBAR=Q*THETA    **
!     **  THE CHARGES QATBAR ARE MULTIPLIED WITH FDIEL,                    **
!     **                                             I.E. QATBAR=QAT*FDIEL **
!     **  THE ENERGY AND ITS DERIVATIVES NEED TO BE DIVIDED BY FDIEL       **
!     **  THE DERIVATIVES ARE                                              **
!     **    VQBAR=DE/DQBAR                                                 **
!     **    VATBAR=DE/DQATBAR                                              **
!     **    FQ=-DE/DRQ                                                     **
!     **    FAT=-DE/DRAT                                                   **
!     **                                                                   **
      IMPLICIT NONE
      LOGICAL,INTENT(IN) :: TISO
      REAL(8),INTENT(IN) :: RBAS(3,3)
      INTEGER,INTENT(IN) :: NQ
      INTEGER,INTENT(IN) :: NAT
      LOGICAL,INTENT(IN) :: ZEROTHETA(NQ)
      REAL(8),INTENT(IN) :: DISMIN
      REAL(8),INTENT(IN) :: QBAR(NQ)
      REAL(8),INTENT(IN) :: RQ(3,NQ)
      REAL(8),INTENT(IN) :: QATBAR(NAT)
      REAL(8),INTENT(IN) :: RAT(3,NAT)
      REAL(8),INTENT(OUT):: ETOT
      REAL(8),INTENT(OUT):: VQBAR(NQ)
      REAL(8),INTENT(OUT):: FQ(3,NQ)
      REAL(8),INTENT(OUT):: VATBAR(NAT)
      REAL(8),INTENT(OUT):: FAT(3,NAT)
      INTEGER            :: NQEFF
      INTEGER            :: IQ
      REAL(8),ALLOCATABLE:: QEFF(:)
      REAL(8),ALLOCATABLE:: VEFF(:)
      REAL(8),ALLOCATABLE:: REFF(:,:)
      REAL(8),ALLOCATABLE:: FEFF(:,:)
      REAL(8)            :: ETOT1
      REAL(8)            :: V1(NAT)
      REAL(8)            :: F1(3,NAT)
!     ***********************************************************************
      NQEFF=NAT
      DO IQ=1,NQ
        IF(.NOT.ZEROTHETA(IQ)) NQEFF=NQEFF+1
      ENDDO
      ALLOCATE(QEFF(NQEFF))
      ALLOCATE(VEFF(NQEFF))
      ALLOCATE(REFF(3,NQEFF))
      ALLOCATE(FEFF(3,NQEFF))
      CALL COSMO_MAPTOEFF(NAT,NQ,NQEFF,ZEROTHETA &
     &                   ,QBAR,RQ,QATBAR,RAT,QEFF,REFF)
      IF(TISO) THEN
        CALL COSMO_ISOLATEDHARTREE(NQEFF,REFF,QEFF,ETOT,VEFF,FEFF,DISMIN)
        CALL COSMO_ISOLATEDHARTREE(NAT,RAT,QATBAR,ETOT1,V1,F1,DISMIN)
      ELSE
        CALL COSMO_MADELUNG(NQEFF,RBAS,REFF,QEFF,ETOT,VEFF,FEFF,DISMIN)
        CALL COSMO_MADELUNG(NAT,RBAS,RAT,QATBAR,ETOT1,V1,F1,DISMIN)
      END IF
      ETOT=ETOT-ETOT1
      VEFF(NQEFF-NAT+1:NQEFF)=VEFF(NQEFF-NAT+1:NQEFF)-V1(:)
      FEFF(:,NQEFF-NAT+1:NQEFF)=FEFF(:,NQEFF-NAT+1:NQEFF)-F1(:,:)
      CALL COSMO_MAPFROMEFF(NAT,NQ,NQEFF,ZEROTHETA &
     &                     ,VQBAR,FQ,VATBAR,FAT,VEFF,FEFF)
      DEALLOCATE(QEFF)
      DEALLOCATE(VEFF)
      DEALLOCATE(REFF)
      DEALLOCATE(FEFF)
      RETURN
      END
!      
!     ......................................................MADELUNG....
      SUBROUTINE COSMO_MADELUNG(NBAS,RBAS,BAS,Q,EMAD,VMAD,FMAD,DISMIN)
!     ******************************************************************
!     **                                                              **
!     ** EVALUATES MADELUNG ENERGY, POTENTIAL AND FORCES              **
!     **                                                              **
!     ** REFERENCE: "MOLECULAR SIMULATIONS" FRENKEL/SMIT              **
!     ** USES: MPE$QUERY                                              **
!     **       MPE$COMBINE                                            **
!     **       GBASS                                                  **
!     **       BOXSPH                                                 **
!     **                                                              **
!     ******************************************************************
!     USE MPE_MODULE
      IMPLICIT NONE
      COMPLEX(8),PARAMETER  :: CI=(0.D0,1.D0)
      REAL(8),   PARAMETER  :: TOL=1.D-8
      REAL(8),   INTENT(IN) :: RBAS(3,3)     ! LATTICE VECTORS
      INTEGER,   INTENT(IN) :: NBAS          ! NUMBER OF CHARGES
      REAL(8),   INTENT(IN) :: BAS(3,NBAS)   ! POSITIONS OF CHARGES
      REAL(8),   INTENT(IN) :: Q(NBAS)       ! CHARGES
      REAL(8),   INTENT(OUT):: EMAD          ! MADELUNG ENERGY   
      REAL(8),   INTENT(OUT):: VMAD(NBAS)    ! POTENTIAL AT CHARGE POSITIONS
      REAL(8),   INTENT(OUT):: FMAD(3,NBAS)  ! FORCES ON CHARGES
      REAL(8),   INTENT(IN) :: DISMIN
      REAL(8)               :: GBAS(3,3)     ! RECIPROCAL LATTICE VECTORS
      COMPLEX(8)            :: EIGR(NBAS)    ! FORM FACTOR
      REAL(8)               :: PI,FOURPI,ROOT2
      REAL(8)               :: VOL
      REAL(8)               :: RC,SVAR,FAC
      INTEGER               :: I,IR,IR1,IR2
      REAL(8)               :: GMAX,RMAX
      INTEGER               :: IG1MIN,IG1MAX
      INTEGER               :: IG2MIN,IG2MAX
      INTEGER               :: IG3MIN,IG3MAX
      INTEGER               :: IG1,IG2,IG3
      REAL(8)               :: T1,T2,T3
      REAL(8)               :: GSQUARE,GVEC(3)
      REAL(8)               :: RCG1SQUARE,RCG2SQUARE,RCG3SQUARE
      REAL(8)               :: SINFAC,COSFAC
      REAL(8)               :: Q12
      REAL(8)               :: DR(3),DR12(3)
      INTEGER               :: IT1MIN,IT1MAX
      INTEGER               :: IT2MIN,IT2MAX
      INTEGER(4)            :: IT3MIN,IT3MAX
      INTEGER(4)            :: IT1,IT2,IT3
      REAL(8)               :: DX,DLEN
      REAL(8)               :: RFAC1,RFAC2,DV,QTOT
      REAL(8)               :: GR,G2MAX
      REAL(8)               :: ERFCX
      INTEGER(4)            :: NTASKNUM,ITASK,ICOUNT
      REAL(8)   ,PARAMETER  :: TR=15.D0 !ESTIMATED EFFORT PER TERM IN REAL SPACE
      REAL(8)   ,PARAMETER  :: TF=1.D0 !ESTIMATED EFFORT PER TERM IN REC. SPACE
      INTEGER(4)            :: IBI
      REAL(8)               :: X0,Y0,XM,YM,X2
      REAL(8)               :: ALPHA
      COMPLEX(8)            :: RHOK,CSVAR
      REAL(8)               :: B,C  ! FACTORS FOR CLOSE CONTACT POTENTIAL
!     ******************************************************************
      PI=4.D0*DATAN(1.D0)
      FOURPI=4.D0*PI
      ROOT2=SQRT(2.D0)
      B=2.D0/DISMIN
      C=-1.D0/DISMIN**2
!
!     ==================================================================
!     == CHECKS FOR PARALLEIZATION                                    ==
!     ==================================================================
!     CALL MPE$QUERY('NONE',NTASKNUM,ITASK)
      NTASKNUM=1
      ITASK=1
!
!     ==================================================================
!     == CALCULATE RECIPROCAL TRANSLATION VECTORS                     ==
!     ==================================================================
      CALL GBASS(RBAS,GBAS,VOL)
!
!     ==================================================================
!     == CALCULATE RANGE OF R- AND G-SPACE SUMMATIONS                 ==
!     ==================================================================
      ALPHA=(TR*PI**3*REAL(NBAS)/(TF*VOL**2))**(1.D0/6.D0)
      X0=10.D0
      DX=1.D0
      IBI=1
      CALL BISEC(-1,IBI,X0,Y0,DX,XM,YM)
      DO I=1,1000
        X2=X0**2
        Y0=EXP(-X2)/X2-TOL
        CALL BISEC(0,IBI,X0,Y0,DX,XM,YM)
        X0=MAX(X0,1.D-5)
        IF(ABS(Y0).LT.1.D-3*TOL) EXIT
      ENDDO
      RMAX=X0/ALPHA
      GMAX=2.D0*X0*ALPHA
      RC=1.D0/ALPHA
!
!     ==================================================================
!     == DETERMINE CUTOFF RADII FOR REAL AND RECIPROCAL SPACE         ==
!     ==================================================================
      VMAD(:)=0.D0
      FMAD(:,:)=0.D0
!
!     ==================================================================
!     == G-SPACE SUM                                                  ==
!     ==================================================================
!     == G(R)=(SQRT(PI)*RC)**3 EXP(-(R/RC)**2)
!     == G(G)=1/V INT[V;DR:SUM G(R-T) EXP(-G*R)]=
!     ==     =INT[INFTY;DR:G(R)EXP(-G*R)]
!     ==     =1/V EXP(-(G*RC/2)**2) 
!     == E=Q1*Q2*SUM{G|G(G)**2*4*PI/G**2*COS(G*(R1-R2))               ==
!
!     == THE RESULT FOR GVEC AND -GVEC IS IDENTICAL. THEREFORE ONLY   == 
!     == HALF OF THE LOOP IS EXECUTED. THE GAMMA POINT DOES NOT       ==
!     == CONTRIBUTE AND IS EXCLUDED                                   ==
!
      CALL BOXSPH(GBAS,0.D0,0.D0,0.D0,GMAX &
     &           ,IG1MIN,IG1MAX,IG2MIN,IG2MAX,IG3MIN,IG3MAX)
      G2MAX=GMAX**2
      FAC=2.D0*FOURPI/VOL
      ICOUNT=0
      DO IG1=0,IG1MAX
        T1=DBLE(IG1)
        IF(IG1.EQ.0) THEN
          IG2MIN=0
        ELSE
          IG2MIN=-IG2MAX
        END IF  
        DO IG2=IG2MIN,IG2MAX
          T2=DBLE(IG2)
          IF(IG1.EQ.0.AND.IG2.EQ.0) THEN
            IG3MIN=1
          ELSE
            IG3MIN=-IG3MAX
          END IF  
          DO IG3=IG3MIN,IG3MAX
            ICOUNT=ICOUNT+1
!           __ SELECTION FOR PARALLEL PROCESSING________________________
            IF(MOD(ICOUNT-1,NTASKNUM).NE.ITASK-1) CYCLE
!
            T3=DBLE(IG3)
            GVEC(:)=GBAS(:,1)*T1+GBAS(:,2)*T2+GBAS(:,3)*T3  
            GSQUARE=DOT_PRODUCT(GVEC,GVEC)
            IF(GSQUARE.LE.G2MAX.AND.GSQUARE.GT.1.D-7) THEN 
!             ========================================================
!             == THIS IS THE FIRST TIME-CRITICAL PART 
!             == CAN BE STREAMLINED:
!             ========================================================
              RHOK=(0.D0,0D0)
              DO IR=1,NBAS
                GR=DOT_PRODUCT(GVEC(:),BAS(:,IR))
                EIGR(IR)=EXP(-CI*GR)
                RHOK=RHOK+EIGR(IR)*Q(IR)
              ENDDO  
              SVAR=-0.5D0*GSQUARE*RC**2
              RHOK=RHOK*FAC*DEXP(SVAR)/GSQUARE
              DO IR1=1,NBAS
                CSVAR=CONJG(EIGR(IR1))*RHOK
                SINFAC=-AIMAG(CSVAR)
                COSFAC=REAL(CSVAR,KIND=8)
                VMAD(IR1)  =VMAD(IR1)+COSFAC
                SVAR       =Q(IR1)*SINFAC           
                FMAD(:,IR1)=FMAD(:,IR1)+SVAR*GVEC(:)
              ENDDO
            END IF
          ENDDO
        ENDDO
      ENDDO
!
!     ==================================================================
!     == R-SPACE SUM                                                  ==
!     ==================================================================
      ICOUNT=0
      FAC=1.D0/(ROOT2*RC)
      RCG1SQUARE=DOT_PRODUCT(GBAS(:,1),GBAS(:,1))*RMAX
      RCG2SQUARE=DOT_PRODUCT(GBAS(:,2),GBAS(:,2))*RMAX
      RCG3SQUARE=DOT_PRODUCT(GBAS(:,3),GBAS(:,3))*RMAX
      DO IR1=1,NBAS
        ICOUNT=ICOUNT+1
!       __ SELECTION FOR PARALLEL PROCESSING__________________________
        IF(MOD(ICOUNT-1,NTASKNUM).NE.ITASK-1) CYCLE
        DO IR2=1,NBAS
          Q12=Q(IR1)*Q(IR2)
          DR12(:)=BAS(:,IR2)-BAS(:,IR1)
          SVAR=-DOT_PRODUCT(DR12,GBAS(:,1))
          IT1MIN=INT(SVAR-RCG1SQUARE+1001.D0)-1000
          IT1MAX=INT(SVAR+RCG1SQUARE+1000.D0)-1000
          IF(IT1MAX-IT1MIN.LT.0) CYCLE
          SVAR=-DOT_PRODUCT(DR12,GBAS(:,2))
          IT2MIN=INT(SVAR-RCG2SQUARE+1001.D0)-1000
          IT2MAX=INT(SVAR+RCG2SQUARE+1000.D0)-1000
          IF(IT2MAX-IT2MIN.LT.0) CYCLE
          SVAR=-DOT_PRODUCT(DR12,GBAS(:,3))
          IT3MIN=INT(SVAR-RCG3SQUARE+1001.D0)-1000
          IT3MAX=INT(SVAR+RCG3SQUARE+1000.D0)-1000
          DO IT1=IT1MIN,IT1MAX
            T1=DBLE(IT1)
            DO IT2=IT2MIN,IT2MAX
              T2=DBLE(IT2)
              DO IT3=IT3MIN,IT3MAX
                T3=DBLE(IT3)
                DR(:)=DR12(:)+RBAS(:,1)*T1+RBAS(:,2)*T2+RBAS(:,3)*T3  
                DLEN=DSQRT(DOT_PRODUCT(DR,DR))
                IF(DLEN.LT.RMAX) THEN
!                 == THIS IS TIME CRITICAL
                  IF(IR1.EQ.IR2 &
     &                   .AND.IT1.EQ.0.AND.IT2.EQ.0.AND.IT3.EQ.0) THEN
                    RFAC1=-SQRT(2.D0/PI)/RC
                    RFAC2=0.D0
                  ELSE
                    IF(DLEN.GT.DISMIN) THEN
!                   == TABLE LOOKUP MAY BE FASTER
!                   == STORE SPLINE OF P(X):=ERFC(X)/X
!                   ==    RFAC1=FAC*P(DLEN*FAC)
!                   ==    RFAC2=[AC**2*DP(DLEN*FAC)/D(DLEN*FAC)]/DLEN
                      CALL LIB$ERFCR8(DLEN*FAC,ERFCX)
                      RFAC1=ERFCX/DLEN
                      RFAC2=-(RFAC1+FAC*2.D0/DSQRT(PI) &
     &                               *DEXP(-(FAC*DLEN)**2))/DLEN**2
                    ELSE
                      RFAC1=B+C*DLEN
                      RFAC2=C/DLEN
                    END IF
                  END IF
                  RFAC1=0.5D0*RFAC1
                  RFAC2=0.5D0*RFAC2*Q12
                  VMAD(IR1)=VMAD(IR1)+RFAC1*Q(IR2)
                  VMAD(IR2)=VMAD(IR2)+RFAC1*Q(IR1)
                  FMAD(:,IR1)=FMAD(:,IR1)-RFAC2*DR(:)     
                  FMAD(:,IR2)=FMAD(:,IR2)+RFAC2*DR(:)
                END IF              
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO
!     CALL MPE$COMBINE('NONE','+',FMAD)
!     CALL MPE$COMBINE('NONE','+',VMAD)
!
!     ==================================================================
!     == FROM YIN/COHEN                                               ==
!     ==================================================================
      QTOT=0.D0
      DO IR=1,NBAS
        QTOT=QTOT+Q(IR)
      ENDDO
      DV=-2.D0*RC**2*PI/VOL*QTOT
      DO IR=1,NBAS
        VMAD(IR)=VMAD(IR)+DV
      ENDDO
!
!     ==================================================================
!     == MADELUNG ENERGY                                              ==
!     ==================================================================
      EMAD=0.D0
      DO IR=1,NBAS
        EMAD=EMAD+0.5D0*Q(IR)*VMAD(IR)
      ENDDO
!
!     ==================================================================
!     == TURN DERIVATIVES INTO FORCES                                 ==
!     ==================================================================
      DO IR=1,NBAS
        DO I=1,3
          FMAD(I,IR)=-FMAD(I,IR)
        ENDDO
      ENDDO
      RETURN
      END
!      
!     ...................................................................
      SUBROUTINE COSMO_MAPTOEFF(NAT,NQ,NQEFF,ZEROTHETA &
     &                         ,QSURF,RSURF,QAT,RAT,QEFF,REFF)
!     **                                                               **
!     **  COLLECTS ALL NONZERO SURFACE CHARGES AND ATOMIC CHARGES      **
!     **  INTO ONE ARRAY                                               **
!     **                                                               **
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: NAT
      INTEGER,INTENT(IN) :: NQ
      INTEGER,INTENT(IN) :: NQEFF
      LOGICAL,INTENT(IN) :: ZEROTHETA(NQ)
      REAL(8),INTENT(IN) :: QSURF(NQ)
      REAL(8),INTENT(IN) :: RSURF(3,NQ)
      REAL(8),INTENT(IN) :: QAT(NAT)
      REAL(8),INTENT(IN) :: RAT(3,NAT)
      REAL(8),INTENT(OUT):: QEFF(NQEFF)
      REAL(8),INTENT(OUT):: REFF(3,NQEFF)
      INTEGER            :: IQ,IQEFF,IAT
!     *******************************************************************
      IQEFF=0
      DO IQ=1,NQ
        IF(ZEROTHETA(IQ)) CYCLE
        IQEFF=IQEFF+1
        QEFF(IQEFF)=QSURF(IQ)
        REFF(:,IQEFF)=RSURF(:,IQ)
      ENDDO
      DO IAT=1,NAT
        IQEFF=IQEFF+1
        QEFF(IQEFF)=QAT(IAT)
        REFF(:,IQEFF)=RAT(:,IAT)
      ENDDO
      IF(IQEFF.GT.NQEFF) THEN
        CALL ERROR$MSG('INDEX CONFUSION')
        CALL ERROR$STOP('COSMO_MAPTOQEFF')
      END IF
      RETURN
      END
!      
!     ...................................................................
      SUBROUTINE COSMO_MAPFROMEFF(NAT,NQ,NQEFF,ZEROTHETA &
     &                           ,VSURF,FSURF,VAT,FAT,VEFF,FEFF)
!     **                                                               **
!     **  COLLECTS ALL NONZERO SURFACE CHARGES AND ATOMIC CHARGES      **
!     **  INTO ONE ARRAY                                               **
!     **                                                               **
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: NAT
      INTEGER,INTENT(IN) :: NQ
      INTEGER,INTENT(IN) :: NQEFF
      LOGICAL,INTENT(IN) :: ZEROTHETA(NQ)
      REAL(8),INTENT(OUT):: VSURF(NQ)
      REAL(8),INTENT(OUT):: FSURF(3,NQ)
      REAL(8),INTENT(OUT):: VAT(NAT)
      REAL(8),INTENT(OUT):: FAT(3,NAT)
      REAL(8),INTENT(IN) :: VEFF(NQEFF)
      REAL(8),INTENT(IN) :: FEFF(3,NQEFF)
      INTEGER            :: IQ,IQEFF,IAT
!     *******************************************************************
      IQEFF=0
      DO IQ=1,NQ
        VSURF(IQ)=0.D0
        FSURF(:,IQ)=0.D0
        IF(ZEROTHETA(IQ)) CYCLE
        IQEFF=IQEFF+1
        VSURF(IQ)=VEFF(IQEFF)
        FSURF(:,IQ)=FEFF(:,IQEFF)
      ENDDO
      DO IAT=1,NAT
        IQEFF=IQEFF+1
        VAT(IAT)=VEFF(IQEFF)
        FAT(:,IAT)=FEFF(:,IQEFF)
      ENDDO
      IF(IQEFF.GT.NQEFF) THEN
        CALL ERROR$MSG('INDEX CONFUSION')
        CALL ERROR$STOP('COSMO_MAPTOQEFF')
      END IF
      RETURN
      END
!      
!     ...................................................................
      SUBROUTINE COSMO_ISOLATEDHARTREE(N,R,Q,ETOT,V,F,DISMIN)
!     **                                                               **
!     **  COLLECTS ALL NONZERO SURFACE CHARGES AND ATOMIC CHARGES      **
!     **  INTO ONE ARRAY                                               **
!     **                                                               **
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: N
      REAL(8),INTENT(IN) :: R(3,N)  ! POSITIONS
      REAL(8),INTENT(IN) :: Q(N)    ! CHARGES
      REAL(8),INTENT(OUT):: ETOT    ! TOTAL ENERGY
      REAL(8),INTENT(OUT):: V(N)    ! POTENTIALS
      REAL(8),INTENT(OUT):: F(3,N)  ! FORCES
      REAL(8),INTENT(IN) :: DISMIN  ! POSITIONS
      REAL(8)            :: DR(3)
      REAL(8)            :: DIS
      REAL(8)            :: SVAR
      INTEGER            :: I,J
      REAL(8)            :: B,C
      REAL(8)            :: VINT,DVINTDRBYR
!     *******************************************************************
      B=2.D0/DISMIN
      C=-1.D0/DISMIN**2
!
      V(:)=0.D0
      F(:,:)=0.D0
      DO I=1,N
        DO J=1,I-1
          DR(:)=R(:,J)-R(:,I)
          DIS=SQRT(SUM(DR**2))
          IF(DIS.GT.DISMIN) THEN
            VINT=1.D0/DIS
            DVINTDRBYR=-1.D0/DIS**3
          ELSE
!           == TAKE CARE OF VERY CLOSE NEIGHBORS
            VINT=B+C*DIS
            DVINTDRBYR=C/DIS
          END IF
          V(I)=V(I)+Q(J)*VINT
          V(J)=V(J)+Q(I)*VINT
          SVAR=-Q(I)*Q(J)*DVINTDRBYR
          F(:,J)=F(:,J)+SVAR*DR(:)
          F(:,I)=F(:,I)-SVAR*DR(:)
        ENDDO
      ENDDO
      ETOT=0.5D0*DOT_PRODUCT(Q,V)
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO_SELFENERGY(NAT,NQ,ZEROTHETA,THETA,FDIEL,KSELF,BETA,GAMMA &
     &                     ,NQAT,RSOLV,QSURF,ETOT,VSURF,VTHETA)
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: NAT
      INTEGER,INTENT(IN) :: NQ
      LOGICAL,INTENT(IN) :: ZEROTHETA(NQ)
      REAL(8),INTENT(IN) :: THETA(NQ)
      REAL(8),INTENT(IN) :: FDIEL
      REAL(8),INTENT(IN) :: KSELF
      REAL(8),INTENT(IN) :: BETA
      REAL(8),INTENT(IN) :: GAMMA
      INTEGER,INTENT(IN) :: NQAT(NAT)
      REAL(8),INTENT(IN) :: RSOLV(NAT)
      REAL(8),INTENT(IN) :: QSURF(NQ)
      REAL(8),INTENT(OUT):: ETOT
      REAL(8),INTENT(OUT):: VSURF(NQ)
      REAL(8),INTENT(OUT):: VTHETA(NQ)
      REAL(8)            :: PI
      REAL(8)            :: FACEAREA
      REAL(8)            :: CSELF,CAREA
      INTEGER(4)         :: IAT,IQ1,IQ
      REAL(8)            :: EBLANK
      REAL(8)            :: ENONPOLAR
      REAL(8)            :: ESELF
!     **********************************************************************
      PI=4.D0*DATAN(1.D0)
!
!     ======================================================================
!     ==  ZERO OUT ARRAYS                                                 ==
!     ======================================================================
      VTHETA(:)=0.D0
      VSURF(:)=0.D0
      EBLANK=0.D0
      ENONPOLAR=BETA
      ESELF=0.D0
!
!     ======================================================================
!     ==  ADD UP ENERGIES AND POTENTIALS                                  ==
!     ======================================================================
      IQ=0
      DO IAT=1,NAT
        FACEAREA=4.D0*PI*RSOLV(IAT)**2/REAL(NQAT(IAT),KIND=8)
        CAREA=GAMMA*FACEAREA
        CSELF=1.07D0*SQRT(PI/FACEAREA)/FDIEL
        DO IQ1=1,NQAT(IAT)
          IQ=IQ+1
!         ==================================================================
!         == ENERGY OF CHARGES IN THE OVERLAP                             ==
!         ==================================================================
          EBLANK    =EBLANK    +KSELF*(1.D0-THETA(IQ))*QSURF(IQ)**2
          VTHETA(IQ)=VTHETA(IQ)-KSELF*QSURF(IQ)**2
          VSURF(IQ) =VSURF(IQ) +KSELF*(1.D0-THETA(IQ))*2.D0*QSURF(IQ)
!
!         ==================================================================
!         == STEP OUT IF THETA=0                                          ==
!         ==================================================================
          IF(ZEROTHETA(IQ)) CYCLE
!
!         ==================================================================
!         == NONPOLAR ENERGY                                              ==
!         ==================================================================
          ENONPOLAR=ENONPOLAR+THETA(IQ)*CAREA
          VTHETA(IQ)=VTHETA(IQ)+CAREA
!
!         ==================================================================
!         == SELF ENERGY                                                  ==
!         ==================================================================
          ESELF=ESELF+CSELF*(THETA(IQ)*QSURF(IQ))**2
          VSURF(IQ)=VSURF(IQ)+2.D0*CSELF*THETA(IQ)**2*QSURF(IQ)
          VTHETA(IQ)=VTHETA(IQ)+2.D0*CSELF*THETA(IQ)*QSURF(IQ)**2
        ENDDO
      ENDDO
!
!     ======================================================================
!     ==  COMPOSE ENERGY                                                  ==
!     ======================================================================
      ENONPOLAR=ENONPOLAR+GAMMA
      ETOT=EBLANK+ENONPOLAR+ESELF
!PRINT*,'COSMO EBLANK',EBLANK
!PRINT*,'COSMO ENONPOLAR',ENONPOLAR
!PRINT*,'COSMO ESELF',ESELF
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO_SHORTRANGED(NQ,NAT,NG,NNX,NNN,NNLIST,RBAS &
     &                            ,IQFIRST,NQAT,ZEROTHETA &
     &                            ,EPOT,Q,VQ,QRELPOS,RAT,FAT,RC,QMAD,VMAD)
!     **                                                                   **
!     ** 1) EXCLUDE CHARGES THAT ARE BLANKED OUT                           **
!     ** 2) USE THAT THE POTENTIAL ON THE SAME ATOM DOES NOT CONTRIBUTE    **
!     **    TO FORCES AND THAT THE POTENTIAL IS THE SAME ON ALL CHARGES    **
!     ** 3) WHY DO WE NEED THIS TERM ANYWAY? THIS COULD BE ANY RADIAL      **
!     **    FUNCTION OR NONE AT ALL. (IT COMPRESSES OR EXPANDS THE WAVE    C*
!     **    FUNCTIONS)                                                     **
!     ** 4) EXPLOIT FINITE RANGE                                           **
!     ** 5) CONSIDER PERIODIC IMAGES                                       **
!     **                                                                   **
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: NQ
      INTEGER,INTENT(IN) :: NAT
      INTEGER,INTENT(IN) :: NG
      INTEGER,INTENT(IN) :: NNX
      INTEGER,INTENT(IN) :: NNN(NAT)
      INTEGER,INTENT(IN) :: NNLIST(4,NNX,NAT)
      REAL(8),INTENT(IN) :: RBAS(3,3)
      INTEGER,INTENT(IN) :: IQFIRST(NAT)
      INTEGER,INTENT(IN) :: NQAT(NAT)
      LOGICAL,INTENT(IN) :: ZEROTHETA(NQ)
      REAL(8),INTENT(OUT):: EPOT
      REAL(8),INTENT(IN) :: Q(NQ)   ! Q*THETA
      REAL(8),INTENT(OUT):: VQ(NQ)   ! DE/D(Q*THETA)
      REAL(8),INTENT(IN) :: QRELPOS(3,NQ)
      REAL(8),INTENT(IN) :: RC(NG,nat)
      REAL(8),INTENT(IN) :: RAT(3,NAT)
      REAL(8),INTENT(IN) :: QMAD(NG,NAT)
      REAL(8),INTENT(OUT):: VMAD(NG,NAT)
      REAL(8),INTENT(OUT):: FAT(3,NAT)
      REAL(8)            :: PI
      REAL(8)            :: TWOBYSQPI
      INTEGER            :: IAT1,IAT2,IG,IQ,IQ1,IQ2,IN,IT1,IT2,IT3
      REAL(8)            :: DIS,DIS2,DR(3)
      REAL(8)            :: QSUM,VSUM
      REAL(8)            :: RC1
      REAL(8)            :: R2(3)
      REAL(8)            :: R21(3)
      REAL(8)            :: GAUSSINT,DGAUSSINTDR
      REAL(8)            :: VINT,DVINTDR
      REAL(8)            :: SVAR
!     ***********************************************************************
      PI=4.D0*DATAN(1.D0)
      TWOBYSQPI=2.D0/SQRT(PI)
      EPOT=0.D0
      VMAD(:,:)=0.D0
      FAT(:,:)=0.D0
      VQ(:)=0.D0
      DO IAT1=1,NAT
        IQ1=IQFIRST(IAT1)
        IQ2=IQFIRST(IAT1)+NQAT(IAT1)-1
!
!       =====================================================================
!       == ONSITE TERM: NO FORCES, SAME DISTANCE FOR ALL CHARGES           ==
!       =====================================================================
        DIS2=SUM(QRELPOS(:,IQ1)**2)
        DIS=SQRT(DIS2)
        QSUM=SUM(Q(IQ1:IQ2))
        VSUM=0.D0
        DO IG=1,NG
          RC1=RC(IG,iat1)
          CALL LIB$ERFR8(DIS/RC1,GAUSSINT)
          VINT=(GAUSSINT-1.D0)/DIS
!         == FOR THE PURE ATOM-SURF ENERGY USE VINT=GAUSSINT/DIS  ======
!VINT=GAUSSINT/DIS  
          VSUM=VSUM+QMAD(IG,IAT1)*VINT
          VMAD(IG,IAT1)=VMAD(IG,IAT1)+VINT*QSUM
        ENDDO
        EPOT=EPOT+VSUM*QSUM
        VQ(IQ1:IQ2)=VQ(IQ1:IQ2)+VSUM
!
!       =====================================================================
!       == OFFSITE TERM,CONTRIBUTION ONLY BETWEEN RSOLV AND RANGE          ==
!       == CONSIDER PERIODIC IMAGES                                        ==
!       =====================================================================
        DO IN=1,NNN(IAT1)
          IAT2=NNLIST(1,IN,IAT1)
          IT1 =NNLIST(2,IN,IAT1)
          IT2 =NNLIST(3,IN,IAT1)
          IT3 =NNLIST(4,IN,IAT1)
          R2(:)=RAT(:,IAT2) &
      &        +RBAS(:,1)*REAL(IT1)+RBAS(:,2)*REAL(IT2)+RBAS(:,3)*REAL(IT3)
          R21(:)=R2(:)-RAT(:,IAT1)
          DO IQ=IQ1,IQ2
            IF(ZEROTHETA(IQ)) CYCLE
            DR=QRELPOS(:,IQ)-R21(:)
            DIS2=SUM(DR(:)**2)
            DIS=SQRT(DIS2)
            DO IG=1,NG
              RC1=RC(IG,iat2)
! IF(DIS/RC1.GT.5.D0*RC1) CYCLE
              CALL LIB$ERFR8(DIS/RC1,GAUSSINT)
              DGAUSSINTDR=TWOBYSQPI*EXP(-DIS2/RC1**2)/RC1
              VINT=(GAUSSINT-1.D0)/DIS
!             == FOR THE PURE ATOM-SURF ENERGY USE VINT=GAUSSINT/DIS  ======
!VINT=GAUSSINT/DIS  
              DVINTDR=DGAUSSINTDR/DIS-(GAUSSINT-1.D0)/DIS2
              EPOT=EPOT+QMAD(IG,IAT2)*Q(IQ)*VINT
              VQ(IQ)=VQ(IQ)+QMAD(IG,IAT2)*VINT
              VMAD(IG,IAT2)=VMAD(IG,IAT2)+Q(IQ)*VINT
              SVAR=QMAD(IG,IAT2)*Q(IQ)*DVINTDR/DIS
              FAT(:,IAT1)=FAT(:,IAT1)-SVAR*DR(:)
              FAT(:,IAT2)=FAT(:,IAT2)+SVAR*DR(:)
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO_PROPAGATE(DT,ANNE,MQ,NQ,Q0,QM,FQ,QP)
!     **                                                                   **
!     ** PROPAGATE                                                         **
!     **                                                                   **
      IMPLICIT NONE
      REAL(8),INTENT(IN) :: DT ! TIMESTEP
      REAL(8),INTENT(IN) :: ANNE ! FRICTION FACTOR
      INTEGER,INTENT(IN) :: NQ
      REAL(8),INTENT(IN) :: MQ
      REAL(8),INTENT(IN) :: Q0(NQ)
      REAL(8),INTENT(IN) :: QM(NQ)
      REAL(8),INTENT(IN) :: FQ(NQ)
      REAL(8),INTENT(OUT):: QP(NQ)
      REAL(8)            :: SVAR1,SVAR2,SVAR3
!     ***********************************************************************
      SVAR1=2.D0/(1.D0+ANNE)
      SVAR2=1.D0-SVAR1
      SVAR3=DT**2/MQ/(1.D0+ANNE)
      QP(:)=SVAR1*Q0(:)+SVAR2*QM(:)+SVAR3*FQ(:)
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO_EKIN(DT,MQ,NQ,QP,Q0,QM,EKIN)
!     **                                                                   **
!     **  CALCULATES THE KINETIC ENERGY OF THE CHARGES                     **
!     **                                                                   **
      IMPLICIT NONE
      REAL(8),INTENT(IN) :: DT ! TIMESTEP
      INTEGER,INTENT(IN) :: NQ
      REAL(8),INTENT(IN) :: MQ
      REAL(8),INTENT(IN) :: QM(NQ)
      REAL(8),INTENT(IN) :: Q0(NQ)
      REAL(8),INTENT(IN) :: QP(NQ)
      REAL(8),INTENT(OUT):: EKIN
!     ***********************************************************************
!      EKIN=0.5D0*MQ*SUM((QP-QM)**2)/(2.D0*DT)**2
!     == THE FOLLOWING FORM GIVES A BETTER ENERGY CONSERVATION THAN THE
!     == VERLET FORM ABOVE
      EKIN=0.25D0*MQ*(SUM((QP-Q0)**2)+SUM((Q0-QM)**2))/DT**2
      RETURN
      END
!
!     .......................................................................
      SUBROUTINE COSMO_SWITCH(NQ,QP,Q0,QM)
!     **                                                                   **
!     ** SWITCH CHARGES                                                    **
!     **                                                                   **
      IMPLICIT NONE
      INTEGER,INTENT(IN)    :: NQ
      REAL(8),INTENT(INOUT) :: QM(NQ)
      REAL(8),INTENT(INOUT) :: Q0(NQ)
      REAL(8),INTENT(INOUT) :: QP(NQ)
!     ***********************************************************************
      QM=Q0
      Q0=QP
      QP=0.D0
      RETURN
      END

!     .......................................................................
      SUBROUTINE COSMO$INTERFACE(NAT_,R0,NG,RC,QMAD,EPOT,EKIN,VMAD,FORCE)
!     **                                                                   **
!     ** PROPAGATE                                                         **
!     **                                                                   **
!     ** THIS IS THE HEART OF THE CODE: IT GETS CALLED FROM PAW_ISOLATE    **
!     ** AND DOES ALL THE RUNTIME CALCULATIONS!  (IT IS PUBLIC)            **
!     **                                                                   **
!     ** WE TAKE IT FROM THE ORIGINAL CODE BUT MODIFY IT SIGNIFICANTLY     **
!     ** ESPECIALLY WE BREAK UP THE CHARGE PROPAGATION AND THE CALCULATION **
!     ** OF THE FORCES ON THE NUCLEI AND THE GAUSSIAN CHARGES              **
!     ** ONLY MULTIPLE TIMESTEP IS POSSIBLE                                **
!     ** -> THIS IS ENSURED BY THE FACT THAT MULTIPLE MUST BE AN EVEN      **
!     ** NUMBER (AT LEAST 2 :-)                                            **
!     **                                                                   **
      USE COSMO_MODULE
      IMPLICIT NONE
      INTEGER(4),INTENT(IN)    :: NAT_
      INTEGER(4),INTENT(IN)    :: NG
      REAL(8)   ,INTENT(OUT)   :: EPOT
      REAL(8)   ,INTENT(OUT)   :: EKIN
      REAL(8)   ,INTENT(OUT)   :: FORCE(3,NAT_)
      REAL(8)   ,INTENT(OUT)   :: VMAD(NG,NAT_)    !PCONT
      REAL(8)   ,INTENT(IN)    :: R0(3,NAT_)
      REAL(8)   ,INTENT(INOUT) :: QMAD(NG,NAT_)
      REAL(8)   ,INTENT(IN)    :: RC(NG,NAT_)
      INTEGER   ,PARAMETER     :: NNX=20  ! MAX. #(NEIGBORS)/ATOM
      REAL(8)                  :: RMAX    ! MAX. DISTANCE FOR NEIGHBORLIST
      INTEGER                  :: NNN(NAT_)
      INTEGER                  :: NNLIST(4,NNX,NAT_)
      LOGICAL   ,ALLOCATABLE   :: ZEROTHETA(:)
!     == ATOMS
      REAL(8)                  :: QATBAR(NAT_)
      REAL(8)                  :: RAT(3,NAT_)
      REAL(8)                  :: VAT(NAT_)
      REAL(8)                  :: VAT1(NAT_)
      REAL(8)                  :: FAT(3,NAT_)
      REAL(8)                  :: FAT1(3,NAT_)
      REAL(8)                  :: VMAD1(NG,NAT_)
!     == CHARGES
      REAL(8)   ,ALLOCATABLE   :: QBAR(:)
      REAL(8)   ,ALLOCATABLE   :: RQ(:,:)
      REAL(8)   ,ALLOCATABLE   :: FQ1(:,:)
      REAL(8)   ,ALLOCATABLE   :: VQ(:)
      REAL(8)   ,ALLOCATABLE   :: VQ1(:)
      REAL(8)   ,ALLOCATABLE   :: Q2M(:)
!     == THETA
      REAL(8)   ,ALLOCATABLE   :: THETA(:)
      REAL(8)   ,ALLOCATABLE   :: VTHETA(:)
      REAL(8)   ,ALLOCATABLE   :: VTHETA1(:)
!
      REAL(8)                  :: EPOT1,EPOTSUM
      REAL(8)                  :: EKIN1,EKINSUM
      INTEGER                  :: I,IAT,ITER,IQ1,IQ2
      REAL(8)                  :: RBAS(3,3)
      LOGICAL                  :: TCONVG
      REAL(8)                  :: SVAR
      INTEGER                  :: NITER

      REAL(8)                  :: annem  ! previous friction factor
      real(8)                  :: dq,de
      REAL(8)                  :: R01(3,NAT_)
      REAL(8)                  :: QMAD1(NG,NAT_)
      LOGICAL(4)               :: Tchk
INTEGER(4) :: NFILINFO
!     ***********************************************************************
      EPOT=0.D0
      EKIN=0.D0
      VMAD(:,:)=0.D0
      FORCE(:,:)=0.D0
      IF (.NOT.TON) RETURN
                               CALL TRACE$PUSH('COSMO$INTERFACE')
                               CALL TIMING$CLOCKON('COSMO')
!PRINT*,'=============================== COSMO ============================='
      IF(TADIABATIC.AND.TMULTIPLE) THEN
        CALL ERROR$MSG('OPTIONS ADIABATIC AND MULTIPLE ARE MUTUALLY EXCLUSIVE')
        CALL ERROR$STOP('COSMO$INTERFACE')
      END IF
!
!     =======================================================================
!     == READ RESTART FILE, TESSELATION FILES ETC                          ==
!     =======================================================================
      CALL CELL$GETR8A('T0',9,RBAS)
      CALL COSMO_INIT()
!
      IF(NAT_.NE.NAT) THEN
        CALL ERROR$MSG("INCONSISTENT NUMBER OF ATOMS")
        CALL ERROR$STOP("COsmo$interface")
      END IF
!
!     =======================================================================
!     == SETUP DATA THAT DEPEND ONLY ON POSITIONS BUT NOT THE CHARGES      ==
!     =======================================================================
      IF(TADIABATIC) THEN
        NITER=1000
        CALL OPTFRIC$SELECT('COSMO')
        CALL OPTFRIC$GETL4('ON',TCHK) 
        ALLOCATE(Q2M(NQ))
        q2m(:)=qm(:)
      END IF
      IF(TMULTIPLE) THEN
        NITER=NMULTIPLE
      END IF
!
      IF(TSTOP) THEN
        QM(:)=Q0(:)
        TSTOP=.FALSE.
      END IF
!
      IF(START) THEN
        Q0(:)=0.D0
        QM(:)=0.D0
        START=.FALSE.
      END IF

!     == KEEP INPUT VARIABLES IN SEPARATE ARRAYS TO ENABLE TESTING
      QMAD1=QMAD
      R01=R0
!
!     =======================================================================
!     == SETUP DATA THAT DEPEND ONLY ON POSITIONS BUT NOT THE CHARGES      ==
!     =======================================================================
      ALLOCATE(RQ(3,NQ))
      RAT(:,:)=R01(:,:)
      DO IAT=1,NAT
        DO I=IQFIRST(IAT),IQFIRST(IAT)+NQAT(IAT)-1
          RQ(:,I)=QRELPOS(:,I)+RAT(:,IAT)
        ENDDO
      ENDDO 
!
      RMAX=MAX(2.D0*MAXVAL(RSOLV),5.D0*MAXVAL(RC))
      CALL COSMO_NEIGHBORLIST(TISO,NAT,RAT,RBAS,RMAX,NNX,NNN,NNLIST) 
      ALLOCATE(ZEROTHETA(NQ))
      ALLOCATE(THETA(NQ))
      CALL COSMO_CUTOFF(NAT,NQ,RAT,RBAS,RSOLV,NNX,NNN,NNLIST,IQFIRST,NQAT,RQ &
     &                       ,ZEROTHETA,THETA)
      if(.not.allocated(cutofftheta))allocate(cutofftheta(nq))
      cutofftheta(:)=theta(:)
!
!     =======================================================================
!     == NOW CALCULATE TOTAL ENERGY                                        ==
!     =======================================================================
      ALLOCATE(QBAR(NQ))
      ALLOCATE(VQ(NQ))
      ALLOCATE(VQ1(NQ))
      ALLOCATE(FQ1(3,NQ))
      ALLOCATE(VTHETA(NQ))
      ALLOCATE(VTHETA1(NQ))
      EKINSUM=0.D0
      EPOTSUM=0.D0
      DO ITER=1,NITER
        VQ(:)=0.D0
        EPOT=0.D0
        EKIN=0.D0
        IF(.NOT.TMULTIPLE.OR.ITER.EQ.1) THEN
          VAT(:)=0.D0
          VTHETA(:)=0.D0
          FAT(:,:)=0.D0
          VMAD(:,:)=0.D0
        END IF
!
!       =======================================================================
!       == BIG ELECTROSTATIC DOUBLE SUM                                      ==
!       =======================================================================
        QBAR(:)=Q0(:)*THETA(:)
        DO IAT=1,NAT
          QATBAR(IAT)=FDIEL*SUM(QMAD1(:,IAT))
        ENDDO 
        CALL COSMO_LONGRANGE(TISO,RBAS,NQ,NAT,DISMIN,ZEROTHETA,QBAR,RQ,QATBAR,RAT &
     &                      ,EPOT1,VQ1,FQ1,VAT1,FAT1)
        DO IAT=1,NAT
          IQ1=IQFIRST(IAT)
          IQ2=IQ1-1+NQAT(IAT)
          DO I=1,3
            FAT1(I,IAT)=FAT1(I,IAT)+SUM(FQ1(I,IQ1:IQ2))
          ENDDO
        ENDDO
!       == DIVIDE BY FDIEL 
        EPOT1=EPOT1/FDIEL
        VQ1(:)=VQ1(:)/FDIEL
        FAT1(:,:)=FAT1(:,:)/FDIEL
        VAT1(:)=VAT1(:)/FDIEL
!     
        EPOT=EPOT+EPOT1
        VQ(:)=VQ(:)+VQ1(:)*THETA(:)
        VTHETA(:)=VTHETA(:)+VQ1(:)*Q0(:)
        VAT(:)=VAT(:)+VAT1(:)*FDIEL
        FAT(:,:)=FAT(:,:)+FAT1(:,:)
!PRINT*,'LONGRANGE ',EPOT1,'SUM(QBAR)',SUM(QBAR),'SUM(QATBAR)',SUM(QATBAR)
!
!       =======================================================================
!       == SELF-ENERGY                                                       ==
!       =======================================================================
        CALL COSMO_SELFENERGY(NAT,NQ,ZEROTHETA,THETA,FDIEL,KSELF,BETA,GAMMA &
     &                     ,NQAT,RSOLV,Q0,EPOT1,VQ1,VTHETA1)
        EPOT=EPOT+EPOT1
        VQ(:)=VQ(:)+VQ1(:)
        VTHETA(:)=VTHETA(:)+VTHETA1(:)
!PRINT*,'SELF ENERGY ',EPOT1
!
!       =======================================================================
!       == DENSITY SURFACE INTERACTION                                       ==
!       =======================================================================
        CALL COSMO_SHORTRANGED(NQ,NAT,NG,NNX,NNN,NNLIST,RBAS,IQFIRST,NQAT,ZEROTHETA &
     &                      ,EPOT1,QBAR,VQ1,QRELPOS,RAT,FAT1,RC,QMAD1,VMAD1)
        EPOT=EPOT+EPOT1
        VQ(:)=VQ(:)+VQ1(:)*THETA(:)
        VTHETA=VTHETA(:)+VQ1(:)*Q0(:)
        FAT(:,:)=FAT(:,:)+FAT1(:,:)
        VMAD(:,:)=VMAD(:,:)+VMAD1(:,:)
!PRINT*,'SHORT RANGED ',EPOT1
!
!       =======================================================================
!       == TRANSFORM BACK TO FUNDAMENTAL VARIABLES                           ==
!       =======================================================================
        CALL COSMO_THETAFORCE(NAT,NQ,RAT,RBAS,RSOLV,NNX,NNN,NNLIST,IQFIRST,NQAT,RQ &
     &                       ,ZEROTHETA,THETA,VTHETA,FAT1)
        FAT(:,:)=FAT(:,:)+FAT1(:,:)
        VTHETA(:)=0.D0
        DO IAT=1,NAT
          VMAD(:,IAT)=VMAD(:,IAT)+VAT(IAT)
        ENDDO
        VAT(:)=0.D0
!
!       =====================================================================
!       ==  PROPAGATE,KINETIC ENERGY AND SWITCH                            ==
!       =====================================================================
!       == MAGIC NUMBER DT**2/M=1/1000                                     ==
        IF(TADIABATIC) THEN
          CALL OPTFRIC$GETL4('ON',TCHK)
          annem=anne
          IF(TCHK) CALL OPTFRIC$GETR8('FRIC',ANNE)
        END IF
        CALL COSMO_PROPAGATE(DT,ANNE,QMASS,NQ,Q0,QM,-VQ,QP)
        CALL COSMO_EKIN(DT,QMASS,NQ,QP,Q0,QM,EKIN1)
        EKIN=EKIN+EKIN1
WRITE(*,FMT='(I5,10F20.10)')ITER,EKIN,EPOT,EKIN+EPOT,ekin+epot-de,anne,de,dq
!
        IF(TADIABATIC) THEN
!         == check convergence ==============================================
          CALL OPTFRIC$TESTCONV(NQ,QP,Q0,QM,Q2M,ANNE,ANNEM,DT,(/(QMASS,I=1,NQ)/),DQ,DE)
          IF(ETOL.GT.0.AND.QTOL.GT.0) THEN 
            TCONVG=(DQ.LT.QTOL.AND.DE.LT.ETOL)
          ELSE IF(ETOL.GT.0.AND.QTOL.LT.0) THEN 
            TCONVG=DE.LT.ETOL
          ELSE IF(ETOL.LT.0.AND.QTOL.GT.0) THEN 
            TCONVG=DQ.LT.QTOL
          ELSE
            CALL ERROR$MSG('QTOL OR ETOL MUST BE SET FOR ADIABATIC OPTION') 
            CALL ERROR$STOP('COSMO$INTERFACE')
          END IF
          tconvg=tconvg.and.(iter.ge.3)
          IF(TCONVG)EXIT
!         == update optimized friction
          CALL OPTFRIC$GETL4('ON',TCHK)
          IF(TCHK) THEN
            CALL OPTFRIC$UPDATER8('COSMO',NQ,QP,Q0,QM,Q2M,(/(QMASS,i=1,nq)/))
            Q2M=QM
          END IF
        END IF
        CALL COSMO_SWITCH(NQ,QP,Q0,QM)
        IF(TMULTIPLE) THEN
          EKINSUM=EKINSUM+EKIN
          EPOTSUM=EPOTSUM+EPOT
          EKIN=0.D0
          EPOT=0.D0
         END IF
      ENDDO
      IF(TADIABATIC.AND.(.NOT.TCONVG)) THEN
        CALL ERROR$MSG('LOOP NOT CONVERGED')
        CALL ERROR$STOP('COSMO_INTERFACE')
      END IF
!
!     =======================================================================
!     ==  RENORMALIZE RESULT FOR MULTIPLE TIME STEPS                       ==
!     =======================================================================
      IF(TMULTIPLE) THEN
        SVAR=1.D0/REAL(NMULTIPLE)
        EKIN=EKINSUM*SVAR
        EPOT=EPOTSUM*SVAR
        FORCE(:,:)=FAT(:,:)*SVAR
        VMAD(:,:)=VMAD(:,:)*SVAR
      END IF
      IF(TADIABATIC) THEN
        FORCE(:,:)=FAT(:,:)
      END IF
!
!     =======================================================================
!     ==  add pauli repulsion                                              ==
!     =======================================================================
      call COSMO_Pauli(NAT,ng,rc,qmad,NQ,RAT,RBAS,NNX,NNN,NNLIST,IQFIRST,NQAT,RQ &
     &                       ,ZEROTHETA,vpauli,epot1,FAT1,vmad1)
      epot=epot+epot1
      force(:,:)=force(:,:)+fat1(:,:)
      vmad(:,:)=vmad(:,:)+vmad1(:,:)
print*,'epauli ',epot1
!
!     =======================================================================
!     ==  CLOSE DOWN                                                       ==
!     =======================================================================
IF(TCHARGES) THEN
  CALL FILEHANDLER$UNIT('INFO',NFILINFO)
  REWIND(NFILINFO)
  WRITE(NFILINFO,FMT='(I12)') NQ
  WRITE(NFILINFO,FMT='(A10,I10)') "    NONAME",101010
  DO I=1,NQ
    IF(ZEROTHETA(I)) CYCLE
    IF(QBAR(I).LE.0) THEN
      WRITE(NFILINFO,FMT='(A2,3F11.5,F15.10)')'O',RQ(:,I), QBAR(I)
    ELSE 
      WRITE(NFILINFO,FMT='(A2,3F11.5,F15.10)')'CL',RQ(:,I), QBAR(I)
    END IF
  ENDDO
END IF


      IF(ALLOCATED(Q2M)) DEALLOCATE(Q2M)
      DEALLOCATE(QBAR)
      DEALLOCATE(VQ)
      DEALLOCATE(VQ1)
      DEALLOCATE(FQ1)
      DEALLOCATE(VTHETA)
      DEALLOCATE(VTHETA1)
      DEALLOCATE(ZEROTHETA)
      DEALLOCATE(THETA)
      DEALLOCATE(RQ)
                               CALL TIMING$CLOCKOFF('COSMO')
                               CALL TRACE$POP
      RETURN 
      END SUBROUTINE COSMO$INTERFACE
!
!     ..................................................................
      SUBROUTINE COSMO$PRINTOUT()
!attention !!!!! The area is printed in angstrom **2!!!!
      USE COSMO_MODULE
      USE PERIODICTABLE_MODULE
      IMPLICIT NONE
      INTEGER(4)            :: NFIL
      CHARACTER(10)         :: ID
      CHARACTER(2)          :: SYMBOL(nat)
      REAL(8)               :: RAT(3,nat)
      integer(4)            :: iz(nat)
      integer(4)            :: itiedto(nq)
      REAL(8)               :: qpos(3,nq)
      REAL(8)               :: segmentAREA(nq)
      REAL(8)               :: qi(nq)
      REAL(8)               :: vi(nq)
      REAL(8)               :: PI
      REAL(8)               :: facearea
      INTEGER(4)            :: Iq,IAT,iq1,iq2,isp
      integer(4)            :: nqcount
      REAL(8)               :: AEZ
      real(8)               :: ediel
      real(8)               :: etot
      real(8)               :: rsolv1
!     ******************************************************************
      IF (.NOT.TON) RETURN
      PI=4.D0*DATAN(1.D0)

!      
!     ==================================================================
!     == declare file to file handler                                 ==
!     ==================================================================
      ID='COSMO_OUT'
      CALL FILEHANDLER$SETFILE(ID,.TRUE.,'.COSMO_OUT')
      CALL FILEHANDLER$SETSPECIFICATION(ID,'STATUS','REPLACE')
      CALL FILEHANDLER$SETSPECIFICATION(ID,'POSITION','REWIND')
      CALL FILEHANDLER$SETSPECIFICATION(ID,'ACTION','WRITE')
      CALL FILEHANDLER$SETSPECIFICATION(ID,'FORM','FORMATTED')
!      
!     ==================================================================
!     == declare file to file handler                                 ==
!     ==================================================================
      NQCOUNT=0
      DO IAT=1,NAT
        CALL ATOMLIST$GETR8A('R(0)',IAT,3,RAT(:,IAT))
        CALL ATOMLIST$GETI4('ISPECIES',IAT,ISP)
        CALL SETUP$AEZ(ISP,AEZ)
        IZ(IAT)=NINT(AEZ)
        CALL PERIODICTABLE$GET(IZ(IAT),'SYMBOL',SYMBOL(IAT))
        IF(SYMBOL(IAT)(2:2).EQ.'_') SYMBOL(IAT)(2:2)=' '
        FACEAREA=4.D0*PI*RSOLV(IAT)/REAL(NQAT(IAT),KIND=8)
        IQ1=IQFIRST(IAT)
        IQ2=IQ1-1+NQAT(IAT)
        DO IQ=IQ1,IQ2
          IF(CUTOFFTHETA(IQ).LT.1.D-5) CYCLE
          NQCOUNT=NQCOUNT+1
          IF(NQCOUNT.GT.NQ) THEN
            CALL ERROR$MSG('NQCOUNT OUT OF RANGE')
            CALL ERROR$STOP('COSMO$PRINTOUT')
          END IF
          ITIEDTO(NQCOUNT)=IAT
          SEGMENTAREA(NQCOUNT)=FACEAREA*CUTOFFTHETA(IQ)
          QPOS(:,NQCOUNT)=QRELPOS(:,IQ)+RAT(:,IAT)
          QI(NQCOUNT)=Q0(IQ)*CUTOFFTHETA(IQ)
          VI(NQCOUNT)=0.D0
        ENDDO
      ENDDO
!
!     ==================================================================
!     == WRITE INFORMATION TO FILE                                    ==
!     ==================================================================
      CALL FILEHANDLER$UNIT('COSMO_OUT',NFIL)
      CALL COSMO_WRITEOUT(NFIL,NAT,SYMBOL,IZ,RAT,RSOLV &
     &                    ,NQCOUNT,ITIEDTO(1:NQCOUNT),QI(1:NQCOUNT),vi(1:nqcount) &
     &                    ,SEGMENTAREA(1:NQCOUNT),QPOS(:,1:NQCOUNT) &
     &                    ,RSOLV1,ETOT,EDIEL)
      CALL FILEHANDLER$CLOSE('COSMO_OUT')
      RETURN
      END
!
!     ..................................................................
      subroutine cosmo_writeout(nfil,natoms,mtype,nuc,xyz,srad &
     &                    ,nps,iatsp,qcosc,phic,ar,cosurf &
     &                    ,rsolv,etot,ediel)
!     **                                                              **
!     **   WRITE COSMO FILE                                           **
!     **                                                              **
!     **   The a-matrix describes the surface potentials              **
!     **   as v(i)=sum_j A_{i,j} q(j)                                 **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      INTEGER(4)  ,INTENT(IN) :: NFIL          ! FILE UNIT OF THE COSMO FILE
      INTEGER(4)  ,INTENT(IN) :: NATOMS        ! #(ATOMS)
      CHARACTER(*),INTENT(IN) :: MTYPE(NATOMS) ! ELEMENT SYMBOLS FOR EACH ATOM
      REAL(8)     ,INTENT(IN) :: XYZ(3,NATOMS) ! ATOM COORDINATES
      INTEGER(4)  ,INTENT(IN) :: NUC(NATOMS)   ! ATOMIC NUMBERS
      REAL(8)     ,INTENT(IN) :: SRAD(NATOMS)  ! RADIUS OF SURFACE CHARGES 
      INTEGER(4)  ,INTENT(IN) :: NPS           ! #(SURFACE CHARGES)
      INTEGER(4)  ,INTENT(IN) :: IATSP(NPS)    ! ATOM TO WHICH A SEGMENT IS TIED
      REAL(8)     ,INTENT(IN) :: QCOSC(NPS)    ! CORRECTED SCREENING CHARGES
      REAL(8)     ,INTENT(IN) :: PHIC(NPS)     ! CORRECTED POTENTIAL ON THE SEQMENT
      REAL(8)     ,INTENT(IN) :: AR(NPS)       ! SEGMENT AREA
      REAL(8)     ,INTENT(IN) :: COSURF(3,NPS) ! POSITION OF SURFACE SEGMENT
      REAL(8)     ,INTENT(IN) :: RSOLV         ! SOLVENT RADIUS
      REAL(8)     ,INTENT(IN) :: ETOT          ! TOTAL ENERGY
      REAL(8)                 :: EDIEL         ! DIELECTRIC ENERGY
      REAL(8)                 :: DE            ! outlying charge energy correction
!     ****************************************************************************
      IF(LEN(MTYPE(1)).LT.2) THEN
        STOP 'ERROR 1 IN COSMO$WRITEOUT'
      END IF
!
!     *****************************************************************************
!     **  print input (not used by cosmotherm)                                   **
!     *****************************************************************************
      call cosmo_writeheader(nfil)
!
!     *****************************************************************************
!     **  print final geometry (coordinates and their radii) (used by cosmotherm)**
!     *****************************************************************************
      CALL COSMO_WRITECOSMORAD(NFIL,NATOMS,XYZ,MTYPE,SRAD,RSOLV)
!
!     *****************************************************************************
!     **  coordinates in .car format for cosmo-rs  (used by cosmotherm)          **
!     *****************************************************************************
      CALL COSMO_WRITECOORDCAR(NFIL,NATOMS,NUC,MTYPE,XYZ)
!
!     *****************************************************************************
!     **  charges                             (not used by cosmotherm)           **
!     *****************************************************************************
      call cosmo_writescreeningcharge(nfil)

!     *****************************************************************************
!     **  energies                             (used by cosmotherm)              **
!     *****************************************************************************
      de=0.d0
      ediel=0.d0   ! not used by cosmotherm
      call cosmo_writeenergies(nfil,etot,de,ediel)
!
!     *****************************************************************************
!     **  segment information            use by cosmotherm)                      **
!     *****************************************************************************
      CALL cosmo_WRITESEGMENT(NFIL,NPS,IATSP,COSURF,QCOSC,AR,PHIC)
      return
      END
!
!     .............................................................................
      subroutine cosmo_writeheader(nfil)
      implicit none
      INTEGER(4)  ,INTENT(IN) :: NFIL          ! FILE UNIT OF THE COSMO FILE
      REAL(8)                 :: EPS=0.d0      ! DIELECTRIC CONSTANT
      REAL(8)                 :: FEPSI=1.d0    ! (EPS-1)/(EPS+0.5)
      REAL(8)                 :: RSOLV=0.d0    ! SOLVENT RADIUS
      INTEGER(4)              :: NPPA=0        !#(BASIS GRID POINTS PER ATOM)
      INTEGER(4)              :: NSPA=0        !#(SURFACE CHARGES FOR ATOMS OTHER THAN HYDROGEN)
      INTEGER(4)              :: NSPH=0        !#(SURFACE CHARGES FOR HYDROGEN)
      REAL(8)                 :: DISEX=0.d0    !DISTANCE THRESHOLD FOR A MATRIX ELEMENTS
                                               ! 
      REAL(8)                 :: ROUTF=0.d0    ! FACTOR FOR OUTER CAVITY CONSTRUCTION IN THE OUTLYING CHARGE CORRECTION
      INTEGER(4)              :: LCAVITY=0     ! O=OPEN CAVITY 1=CLOSED CAVITY
      REAL(8)                 :: PHSRAN=0.d0   ! AMPLITUDE OF THE CAVITY DE-SYMMETRIZATION
      REAL(8)                 :: AMPRAN=0.d0   ! PHASE OF THE CAVITY DE-SYMMETRIZATION
      REAL(8)                 :: DISEX2=0.d0   ! MEAN SQUARE DISTANCE OF TWO SEGMENTS TIMES DISEX
      INTEGER(4)              :: NPS=0        ! =NPS+NPSHER
      INTEGER(4)              :: NPSD=0        ! =NPS+NPSHER
      INTEGER(4)              :: NPSPHER=0     ! #(SEGMENTS ON THE SURFACE FOR THE OUTLYING CHARGE CORRECTION)
      REAL(8)                 :: VOLUME=0.d0   !??
      REAL(8)                 :: area=0.d0     ! sum of all segment areas
      REAL(8)      ,PARAMETER :: ANGSTROM = 1.D0/0.529177249D0 
!     *****************************************************************************
      LCAVITY=0    ! 0 FOR OPEN CAVITY ; 1 FOR CLOSED CAVITY
!     =============================================================================
!     == write version info                                                      ==
!     =============================================================================
      write(nfil,fmt="('CURRENT PROG: CP-PAW, CLAUSTHAL UNIVERSITY OF TECHNOLOGY')") 
      write(nfil,"('$cosmo')") 
      if(fepsi.eq.1.d0) then
         write(nfil,'(a)') '  epsilon=infinity'
      else
         write(nfil,'(a,f9.3)') '  epsilon=', eps
      endif

      write(nfil,'("  nppa=",i5)')     nppa
      write(nfil,'("  nspa=",i5)')     nspa
      write(nfil,'("  disex=",g12.6)') disex/angstrom
      write(nfil,'("  rsolv=",f5.2)')  rsolv/angstrom
      write(nfil,'("  routf=",f5.2)')  routf
      if (lcavity .eq. 0) then
        write(nfil,'("  cavity open")')
      else
        write(nfil,'("  cavity closed")')
      endif
      write(nfil,'("  amat file=",a)') 'amat.out'  ! amat-file name
      write(nfil,'("  phsran=",g9.2)') phsran
      write(nfil,'("  ampran=",g9.2)') ampran
!
!     *****************************************************************************
!     **  calculated parameters and variables in (not used by cosmotherm)        **
!     *****************************************************************************
      write(nfil,fmt="('$cosmo_data')")
      write(nfil,fmt="('  fepsi=',f14.7)")  fepsi
      write(nfil,fmt="('  disex2=',g12.6)") disex2
      write(nfil,fmt="('  nsph=',i5)")      nsph
      write(nfil,fmt="('  nps=',i5)")       nps
      write(nfil,fmt="('  npsd=',i5)")      npsd
      write(nfil,fmt="('  npspher=',i5)")   npspher
      write(nfil,fmt="('  area=',f8.2)")    area
      write(nfil,fmt="('  volume=',f8.2)")  volume
      return
      end
!
!     ............................................................................
      subroutine cosmo_writescreeningcharge(nfil)
      implicit none
      integer(4),intent(in) :: nfil
      real(8)               :: qsumo=0.d0
      real(8)               :: qsum=0.d0
!     ****************************************************************************
      write(nfil,fmt='("$screening_charge")')
      write(nfil,fmt='("  cosmo      = ",f10.6)')qsum
      write(nfil,fmt='("  correction = ",f10.6)')qsumo
      write(nfil,fmt='("  total      = ",f10.6)')qsum+qsumo
      return
      end
!!
!     .............................................................................
      SUBROUTINE COSMO_WRITECOSMORAD(NFIL,NATOMS,XYZ,MTYPE,SRAD,RSOLV)
!     **
      INTEGER(4)  ,INTENT(IN) :: NFIL          ! FILE UNIT OF THE COSMO FILE
      INTEGER(4)  ,INTENT(IN) :: NATOMS        ! #(ATOMS)
      REAL(8)     ,INTENT(IN) :: XYZ(3,NATOMS) ! ATOM COORDINATES
      CHARACTER(*),INTENT(IN) :: MTYPE(NATOMS) ! ELEMENT SYMBOLS FOR EACH ATOM
      REAL(8)     ,INTENT(IN) :: SRAD(NATOMS)  ! RADIUS OF SURFACE CHARGES 
      REAL(8)     ,INTENT(IN) :: RSOLV         ! SOLVENT RADIUS
      REAL(8)     ,PARAMETER  :: ANGSTROM = 1.D0/0.529177249D0 
      integer(4)              :: i,j
!     *****************************************************************************
      write(nfil,"('$coord_rad')") 
      write(nfil,"('#atom',t9,'x',t28,'y',t47,'z',t61,'element  radius [A]')")
      do i=1,natoms
          write(nfil,fmt="(1x,i3,3(1x,f18.14),2x,a2,2x,f10.5)") &
     &                 i,(xyz(j,i),j=1,3),mtype(i)(1:2),(srad(i)-rsolv)/angstrom
      enddo
      RETURN
      END
!
!     .............................................................................
      SUBROUTINE COSMO_WRITECOORDCAR(NFIL,NATOMS,NUC,MTYPE,XYZ)
      IMPLICIT NONE
      INTEGER(4)  ,INTENT(IN) :: NFIL          ! FILE UNIT OF THE COSMO FILE
      INTEGER(4)  ,INTENT(IN) :: NATOMS        ! #(ATOMS)
      INTEGER(4)  ,INTENT(IN) :: NUC(NATOMS)   ! ATOMIC NUMBERS
      REAL(8)     ,INTENT(IN) :: XYZ(3,NATOMS) ! ATOM COORDINATES
      CHARACTER(*),INTENT(IN) :: MTYPE(NATOMS) ! ELEMENT SYMBOLS FOR EACH ATOM
      REAL(8)     ,PARAMETER  :: ANGSTROM = 1.D0/0.529177249D0 
      INTEGER(4)  ,ALLOCATABLE:: ICNT(:)
      INTEGER(4)              :: I
      CHARACTER(5)            :: LAB1
      CHARACTER(2)            :: LAB2
!     *****************************************************************************
      write(nfil,fmt='("$coord_car")')
      write(nfil,fmt='("!BIOSYM archive 3")')
      write(nfil,fmt='("PBC=OFF")')
      write(nfil,fmt='("coordinates from COSMO calculation")')
      write(nfil,fmt='("!DATE ")')
print*,' 0',nuc

      allocate(icnt(maxval(nuc)))
      icnt(:)=0
      do i=1,natoms
print*,' a',i
        icnt(nuc(i))=icnt(nuc(i))+1  ! atom counter for each element
        write(lab1,*)icnt(nuc(i))
print*,mtype(i)
print*,uc(mtype(i))
print*,lab1
        lab1=trim(uc(mtype(i)(1:2)))//adjustl(lab1)
print*,'--',lab1
        lab2=mtype(i)(1:2)
        lab2(1:1)=uc(lab2(1:1))
        write(nfil,fmt="(a5,3f15.9,' COSM 1      ',a2,6x,a2,f7.3)") &
     &                 lab1,xyz(:,i)/angstrom,mtype(i),lab2,0.d0
      enddo
      deallocate(icnt)
      write(nfil,'("end")')
      write(nfil,'("end")')
      RETURN
      contains
!       .......................................................................
        FUNCTION LC(IN) RESULT(OUT)
!       **  makes a string lowercase                                         **
        CHARACTER(*) ,INTENT(IN) :: IN
        CHARACTER(82)            :: OUT
        INTEGER(4)               :: LENgth
        INTEGER(4)               :: I
        INTEGER(4)               :: ICH
!       *******************************************************************
        OUT=IN
        LENgth=LEN_TRIM(IN)
        DO I=1,LENgth
          ICH=ICHAR(OUT(I:I))
          IF(ICH.GE.65.AND.ICH.LE.90) THEN
            OUT(I:I)=ACHAR(ICH-65+97)
          END IF
        ENDDO
        RETURN
        END FUNCTION LC
!
!       .......................................................................
        FUNCTION uC(IN) RESULT(OUT)
!       **  makes a string uppercase                                         **
        CHARACTER(*),INTENT(IN) :: IN
        CHARACTER(82)           :: OUT
        INTEGER(4)              :: LENgth
        INTEGER(4)              :: I
        INTEGER(4)              :: ICH
!       *******************************************************************
        OUT=IN
        LENgth=LEN_TRIM(IN)
        DO I=1,LENgth
          ICH=ICHAR(OUT(I:I))
          IF(ICH.GE.97.AND.ICH.LE.122) THEN
            OUT(I:I)=ACHAR(ICH+97+65)
          END IF
        ENDDO
        RETURN
        END FUNCTION uC
      END
!
!     .............................................................................
      subroutine cosmo_writeenergies(nfil,etot,de,ediel)
      implicit none
      INTEGER(4)  ,INTENT(IN) :: NFIL          ! FILE UNIT OF THE COSMO FILE
      REAL(8)     ,INTENT(IN) :: ETOT          ! TOTAL ENERGY
      REAL(8)     ,INTENT(IN) :: EDIEL         ! DIELECTRIC ENERGY
      REAL(8)     ,INTENT(IN) :: DE            ! ENERGY CORRECTION
!     *****************************************************************************
      write(nfil,fmt='("# Correlated (C) cosmo calculation:")')
      write(nfil,fmt='("# Total energy: E(SCF)-Ediel(SCF)+E(C)+Ediel(C)")')
      write(nfil,fmt='("# OC corr.:     outlying charge correction using the")')
      write(nfil,fmt='("#               correlated density")')
      write(nfil,fmt='("# ediel:        Ediel(C) using the correlated density")')

      write(nfil,fmt='("$cosmo_energy")')
      write(nfil,fmt='("  Total energy [a.u.]            =   ", f17.10)')etot
      write(nfil,fmt='("  Total energy + OC corr. [a.u.] =   ", f17.10)')etot+de
      write(nfil,fmt='("  Total energy corrected [a.u.]  =   ", f17.10 &
     &                ," Note: incorrect value contained for downward compatibility")')etot+0.5d0*de
      write(nfil,fmt='("  Dielectric energy [a.u.]       =   ", f17.10)')ediel
      write(nfil,fmt='("  Diel. energy + OC corr. [a.u.] =   ", f17.10)')ediel+de
      return
      end
!
!     .............................................................................
      subroutine cosmo_WRITESEGMENT(NFIL,NPS,IATSP,COSURF,QCOSC,AR,PHIC)
      IMPLICIT NONE
      INTEGER(4)  ,INTENT(IN) :: NFIL          ! FILE UNIT OF THE COSMO FILE
      INTEGER(4)  ,INTENT(IN) :: NPS           ! #(SURFACE CHARGES)
      INTEGER(4)  ,INTENT(IN) :: IATSP(NPS)    ! ATOM TO WHICH A SEGMENT IS TIED
      REAL(8)     ,INTENT(IN) :: QCOSC(NPS)    ! CORRECTED SCREENING CHARGES
      REAL(8)     ,INTENT(IN) :: PHIC(NPS)     ! CORRECTED POTENTIAL ON THE SEQMENT
      REAL(8)     ,INTENT(IN) :: AR(NPS)       ! SEGMENT AREA
      REAL(8)     ,INTENT(IN) :: COSURF(3,NPS) ! POSITION OF SURFACE SEGMENT
      REAL(8)      ,PARAMETER :: ANGSTROM = 1.D0/0.529177249D0 
      INTEGER(4)              :: I,J
!     *****************************************************************************
      write(nfil,fmt='("$segment_information")')
      write(nfil,fmt='("# n             - segment number")')
      write(nfil,fmt='("# atom          - atom associated with segment n")')
      write(nfil,fmt='("# position      - segment coordinates [a.u.]")')
      write(nfil,fmt='("# charge        - segment charge (corrected)")')
      write(nfil,fmt='("# area          - segment area [A**2]")')
      write(nfil,fmt='("# potential     - solute potential on segment (A length scale)")')
      write(nfil,fmt='("#")')
      write(nfil,fmt='("#  n   atom",14x,"position (X, Y, Z)",19x &
     &                ,"charge         area        charge/area     potential")')
      write(nfil,fmt='("#")')
      write(nfil,fmt='("#")')
      do i=1,nps
        write(nfil,'(i5,i5,7f15.9)') i,iatsp(i),(cosurf(j,i),j=1,3) &
     &            ,qcosc(i),ar(i)/angstrom**2,qcosc(i)/(ar(i)/angstrom**2),phic(i)*angstrom
      enddo
      RETURN
      END
