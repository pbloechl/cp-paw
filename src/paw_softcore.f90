MODULE CORE_MODULE
!**                                                                           **
!**  THE COMPLEX POINTER LIST THIS IS NEEDED FOR THE PARALLELIZATION          **
!**  BECAUSE EVERY NODE WILL CALCULATE THE CORE LEVELS ONLY FOR ITS           **
!**  OWN SET OF ATOMS AND ONLY THE FIRST NODE CAN WRITE                       **
!**                                                                           **
!**  COMMUNIATION IN CORE$REPORT NOT DONE YET.                                **
!**                                                                           **
TYPE CORESHIFT_TYPE
INTEGER(4)           :: IAT        !ATOM INDEX
INTEGER(4)           :: N          !#(CORE WAVE FUNCTIONS PER SPIN (L,M,N))
CHARACTER(8),POINTER :: TYPE(:)    !ONE OF 'S','P','D','F','?'
REAL(8)     ,POINTER :: E(:)       !CRYSTAL CORE ENERGY LEVELS
REAL(8)     ,POINTER :: EATOM(:)   !ATOMIC ENERGY EIGENVALUE
TYPE(CORESHIFT_TYPE),POINTER :: NEXT
END TYPE CORESHIFT_TYPE
LOGICAL(4)                  :: TCORESHIFTS=.TRUE.
LOGICAL(4)                  :: DEFAULT=.FALSE.
INTEGER(4)                  :: NATOMS=0
CHARACTER(32),ALLOCATABLE   :: ATOMS(:)
TYPE(CORESHIFT_TYPE),TARGET :: FIRST
TYPE(CORESHIFT_TYPE),POINTER:: THIS
LOGICAL(4),SAVE             :: TINI=.FALSE.
END MODULE CORE_MODULE
!
!     ...1.........2.........3.........4.........5.........6.........7.........8
      SUBROUTINE CORE$SETL4(ID,VAL)
      USE CORE_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      LOGICAL(4)  ,INTENT(IN) :: VAL
!     **************************************************************************
      IF(ID.EQ.'ON') THEN
        TCORESHIFTS=VAL
      ELSE IF(ID.EQ.'DEFAULT') THEN
        DEFAULT=VAL
      ELSE
        CALL ERROR$MSG('ID NOT RECOGNIZED')
        CALL ERROR$CHVAL('ID',ID)
        CALL ERROR$STOP('CORE$SETL4')
      END IF
      RETURN
      END
!
!     ...1.........2.........3.........4.........5.........6.........7.........8
      SUBROUTINE CORE$SETCHA(ID,LEN,VAL)
      USE CORE_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      INTEGER(4)  ,INTENT(IN) :: LEN
      CHARACTER(*),INTENT(IN) :: VAL(LEN)
      INTEGER(4)              :: I
!     **************************************************************************
      IF(ID.EQ.'ATOMS') THEN
!       == LIST OF ATOMS FOR WHICH THE CORE STATES SHALL BE EVALUATED ==========
        IF(ALLOCATED(ATOMS))DEALLOCATE(ATOMS)
        NATOMS=LEN
        ALLOCATE(ATOMS(NATOMS))
        DO I=1,LEN
          ATOMS(I)=VAL(I)
        ENDDO
      ELSE
        CALL ERROR$MSG('ID NOT RECOGNIZED')
        CALL ERROR$CHVAL('ID',ID)
        CALL ERROR$STOP('CORE$SETCHA')
      END IF
      RETURN
      END
!
!     ...1.........2.........3.........4.........5.........6.........7.........8
      SUBROUTINE CORE$REPORT(NFIL)
      USE CORE_MODULE
      USE MPE_MODULE
      IMPLICIT NONE
      INTEGER(4)  ,INTENT(IN)  :: NFIL
      REAL(8)                  :: EV
      CHARACTER(32)            :: NAME
      INTEGER(4)               :: I
      INTEGER(4)               :: N
      REAL(8)                  :: SVAR
      INTEGER(4)               :: THISTASK,NTASKS
      CHARACTER(128)           :: STRING
      INTEGER(4)               :: NAT
      INTEGER(4)               :: IAT
      INTEGER(4)  ,ALLOCATABLE :: TASKARR(:)
      INTEGER(4)  ,ALLOCATABLE :: TASKARR1(:)
      REAL(8)     ,ALLOCATABLE :: E(:)
      REAL(8)     ,ALLOCATABLE :: EATOM(:)
      CHARACTER(8),ALLOCATABLE :: XTYPE(:)
      INTEGER(4)               :: SNDTASK,RCVTASK
!     **************************************************************************
      IF(.NOT.TINI) RETURN
                             CALL TRACE$PUSH('CORE$REPORT')
      CALL MPE$QUERY('MONOMER',NTASKS,THISTASK)
      CALL CONSTANTS('EV',EV)

!     == CREATE A MAPPING ARRAY FROM ATOMS TO TASKS FOR EACH NODE.            ==
!     == ATOMS NOT PRESENT ON THIS TASK OBTAIN 0 FOR THE ATOM NUMBER          ==
      CALL ATOMLIST$NATOM(NAT)
      ALLOCATE(TASKARR(NAT))
      TASKARR(:)=0   ! TASK ID FOR EACH ATOM
      IF(FIRST%IAT.NE.0) THEN 
        THIS=>FIRST
        DO 
          IAT=THIS%IAT
          TASKARR(IAT)=THISTASK
          IF(.NOT.ASSOCIATED(THIS%NEXT)) EXIT
          THIS=>THIS%NEXT
        ENDDO
      ENDIF
!
!     == COMMUNICATE A UNIQUE MAPPING KNOWN TO ALL TASKS =======================
! THIS COMMUNICATION IS UNNECCESARILY COMPLICATED.
      ALLOCATE(TASKARR1(NAT))
      TASKARR1(:)=0
      DO I=1,NTASKS
        SNDTASK=THISTASK+1
        SNDTASK=MODULO(SNDTASK-1,NTASKS)+1
        RCVTASK=THISTASK-1
        RCVTASK=MODULO(RCVTASK-1,NTASKS)+1
        !== MPE$COMBINE WITH THE MAX FUNCTION MAY HAVE BEEN BETTER
        TASKARR1=TASKARR
        CALL MPE$SENDRECEIVE('MONOMER',THISTASK,SNDTASK,TASKARR1)
        CALL MPE$SENDRECEIVE('MONOMER',RCVTASK,THISTASK,TASKARR1)
        DO IAT=1,NAT
          TASKARR(IAT)=MAX(TASKARR(IAT),TASKARR1(IAT))
        ENDDO
      ENDDO       
      DEALLOCATE(TASKARR1)
!     == TASKARRAY IS NOW COMMON TO ALL TASKS        

      DO IAT=1,NAT
        IF(TASKARR(IAT).EQ.0) CYCLE ! NO INFORMATION FOR THIS ATOM AVAILABLE

        IF(THISTASK.EQ.TASKARR(IAT)) THEN
          THIS=>FIRST
          DO 
            IF(THIS%IAT.EQ.IAT) EXIT
            IF(.NOT.ASSOCIATED(THIS%NEXT)) EXIT
            THIS=>THIS%NEXT
          ENDDO
          N=THIS%N
        END IF
        CALL MPE$SENDRECEIVE('MONOMER',TASKARR(IAT),1,N)

!
        ALLOCATE(E(N))
        ALLOCATE(EATOM(N))
        ALLOCATE(XTYPE(N))
        IF(THISTASK.EQ.TASKARR(IAT)) THEN
          E=THIS%E
          EATOM=THIS%EATOM
          XTYPE=THIS%TYPE
        END IF
        CALL MPE$SENDRECEIVE('MONOMER',TASKARR(IAT),1,E)
        CALL MPE$SENDRECEIVE('MONOMER',TASKARR(IAT),1,EATOM)
        CALL MPE$SENDRECEIVE('MONOMER',TASKARR(IAT),1,XTYPE)
!       == TASK 1 RECEIVED DATA ================================================
!
!       == PRINT INFORMATION ON THE FIRST TASK =================================
        IF(THISTASK.EQ.1) THEN
          CALL ATOMLIST$GETCH('NAME',IAT,NAME)
          CALL REPORT$TITLE(NFIL,'EIGENVALUES OF CORE STATES FROM ATOM '& 
     &                     //TRIM(NAME))
          WRITE(NFIL,*)'     CURRENT SYSTEM          ISOLATED ATOM' 
          STRING='(T3,"ENERGY[H]",T14,"ENERGY[EV]"'
          STRING=TRIM(ADJUSTL(STRING))//',T28,"ENERGY[H]",T39,"ENERGY[EV]"'
          STRING=TRIM(ADJUSTL(STRING))//',T53,"SHIFT[H]",T64,"SHIFT[EV]"'
          STRING=TRIM(ADJUSTL(STRING))//')'
          WRITE(NFIL,STRING)
          DO I=1,N
            WRITE(NFIL,FMT='(A5,6F12.5)')TRIM(XTYPE(I)) &
     &                           ,E(I),E(I)/EV &
     &                           ,EATOM(I),EATOM(I)/EV &
     &                           ,E(I)-EATOM(I),(E(I)-EATOM(I))/EV
          ENDDO
        END IF
        DEALLOCATE(E)
        DEALLOCATE(EATOM)
        DEALLOCATE(XTYPE)
      ENDDO
                             CALL TRACE$POP()
      RETURN
      END
!
! SANTOS040617 BEGIN
!!$!     ...1.........2.........3.........4.........5.........6.........7.........8
!!$      SUBROUTINE CORE_CORESHIFTS(IAT,ISP,GID,NR,LMRXX,NDIMD,AEPOT)
!!$!     **************************************************************************
!!$!     **  CALCULATES THE EIGENVALUES OF CORE HAMILTONIAN                      **
!!$!     **                                                                      **
!!$!     **  IS CALLED FROM PAW_AUGMENTATION, WHICH EXECUTES ONLY ON ONE TASK    **
!!$!     **                                                                      **
!!$!     **                                                                      **
!!$!     **************************************************************************
!!$      USE CORE_MODULE, ONLY : CORESHIFT_TYPE &
!!$     &                       ,TCORESHIFTS &
!!$     &                       ,DEFAULT &
!!$     &                       ,TINI &
!!$     &                       ,THIS,FIRST &
!!$     &                       ,NATOMS &
!!$     &                       ,ATOMS 
!!$      IMPLICIT NONE
!!$      INTEGER(4),INTENT(IN) :: IAT     ! ATOM INDEX
!!$      INTEGER(4),INTENT(IN) :: ISP     ! ATOM TYPE
!!$      INTEGER(4),INTENT(IN) :: GID     ! GRID ID
!!$      INTEGER(4),INTENT(IN) :: NR      ! #(GRID POINTS)
!!$      INTEGER(4),INTENT(IN) :: NDIMD   ! #(DENSITY SPIN COMPONENTS)
!!$      INTEGER(4),INTENT(IN) :: LMRXX
!!$      REAL(8)   ,INTENT(IN) :: AEPOT(NR,LMRXX,NDIMD) ! 1C-AE POTENTIAL
!!$      REAL(8)   ,PARAMETER  :: PI=4.D0*ATAN(1.D0)
!!$      REAL(8)               :: ATPOT(NR)  !RADIAL ATOM POTENTIAL 
!!$      REAL(8)   ,ALLOCATABLE:: AEPOT1(:,:,:)!(NR,LMRX,NDIMD)
!!$      INTEGER(4)            :: NB         !#(ATOMIC CORE AND VALENCE STATES)
!!$      INTEGER(4)            :: NC         !#(ATOMIC CORE STATES)
!!$      INTEGER(4),ALLOCATABLE:: LB(:)      !MAIN ANGULAR MOMENTUM
!!$!     REAL(8)   ,ALLOCATABLE:: FB(:)  
!!$      REAL(8)   ,ALLOCATABLE:: EB(:)      !ENERGY LEVEL
!!$      REAL(8)   ,ALLOCATABLE:: AEPSI(:,:) !AE WAVE FUNCTION
!!$      REAL(8)               :: R(NR)      !RADIAL GRID
!!$      CHARACTER(32)         :: NAME
!!$      INTEGER(4)            :: LMNX
!!$      INTEGER(4)            :: LMRX
!!$
!!$      INTEGER(4)            :: I
!!$      REAL(8)               :: AUX2
!!$
!!$      INTEGER(4)            :: LMN1,LMN2
!!$      INTEGER(4)            :: LN1,LN2
!!$      INTEGER(4)            :: LM1,LM2,LM3
!!$      INTEGER(4)            :: L1,L2,IM1,IM2
!!$      REAL(8)               :: AEDMU(NR)
!!$      REAL(8)               :: DWORK1(NR)
!!$      REAL(8)               :: CG          !GAUNT COEFFICIENT
!!$      REAL(8)               :: SVAR
!!$
!!$      REAL(8)   ,ALLOCATABLE:: HAMIL(:,:)
!!$      REAL(8)   ,ALLOCATABLE:: EIGENVAL(:)
!!$      REAL(8)   ,ALLOCATABLE:: EIGENVEC(:,:)
!!$      INTEGER(4)            :: NFILO
!!$      REAL(8)               :: EV             ! ELECTRON VOLT
!!$      LOGICAL(4)            :: TCHK
!!$!     **************************************************************************
!!$      IF(.NOT.TCORESHIFTS) RETURN
!!$      CALL RADIAL$R(GID,NR,R)
!!$      CALL ATOMLIST$GETCH('NAME',IAT,NAME)
!!$!     == CHECK WHETHER CORE LEVELS SHALL BE CALCULATED FOR THIS ATOM (IAT)  ====
!!$      TCHK=DEFAULT
!!$      DO I=1,NATOMS
!!$        IF(NAME.EQ.ATOMS(I)) THEN
!!$          TCHK=.NOT.DEFAULT
!!$        END IF
!!$      ENDDO
!!$      IF(.NOT.TCHK) RETURN
!!$!
!!$!     ==========================================================================
!!$!     ==  SELECT THE PROPER ENTRY IN THE TABLE                                ==
!!$!     ==========================================================================
!!$      THIS=>FIRST
!!$      IF(.NOT.TINI) THEN
!!$        TINI=.TRUE.
!!$        THIS%IAT=IAT
!!$        THIS%N=0
!!$        NULLIFY(THIS%E)
!!$        NULLIFY(THIS%EATOM)
!!$        NULLIFY(THIS%TYPE)
!!$        NULLIFY(THIS%NEXT)
!!$      ELSE
!!$        DO WHILE (THIS%IAT.NE.IAT) 
!!$          IF(ASSOCIATED(THIS%NEXT)) THEN
!!$            THIS=>THIS%NEXT
!!$          ELSE
!!$            ALLOCATE(THIS%NEXT)
!!$            THIS=>THIS%NEXT
!!$            NULLIFY(THIS%NEXT)
!!$            THIS%IAT=IAT
!!$            THIS%N=0
!!$            NULLIFY(THIS%E)
!!$            NULLIFY(THIS%EATOM)
!!$            NULLIFY(THIS%TYPE)
!!$          END IF
!!$        ENDDO
!!$      END IF
!!$!     == THE POINTER "THIS" REFERS NOW TO THE ATOM AT HAND =====================
!!$!
!!$!     ==========================================================================
!!$!     ==  COLLECT ATOMIC CORE WAVE FUNCTIONS AEPSI FROM SETUP OBJECT          ==
!!$!     ==========================================================================
!!$      CALL SETUP$ISELECT(ISP)
!!$      CALL SETUP$GETI4('NB',NB)
!!$      CALL SETUP$GETI4('NC',NC)
!!$      IF (NC.EQ.0) THEN
!!$        CALL FILEHANDLER$UNIT('PROT',NFILO)
!!$        CALL REPORT$TITLE(NFILO,'NO CORE STATES FOR ATOM '//TRIM(NAME))
!!$        RETURN
!!$      END IF
!!$      ALLOCATE(LB(NB))
!!$      ALLOCATE(EB(NB))
!!$      ALLOCATE(AEPSI(NR,NB))
!!$      CALL SETUP$GETR8A('AEPOT',NR,ATPOT)    !POTENTIAL OF THE ATOM
!!$      CALL SETUP$GETI4A('LB',NB,LB)          !MAIN ANGULAR MOMENTUM
!!$      CALL SETUP$GETR8A('EB',NB,EB)          !ATOMIC ENERGY LEVEL
!!$      CALL SETUP$GETR8A('AEPSI',NR*NB,AEPSI) !ATOMIC WAVE FUNCTION
!!$      CALL SETUP$UNSELECT()
!!$!
!!$!     ==========================================================================
!!$!     ==  CONSTANTS                                                           ==
!!$!     ==========================================================================
!!$      LMNX=0
!!$      LMRX=0
!!$      DO I=1,NC
!!$        LMNX=LMNX+2*LB(I)+1
!!$        LMRX=MAX(LMRX,(2*LB(I)+1)**2)
!!$      ENDDO
!!$      LMRX=MIN(LMRX,LMRXX)
!!$      IF(THIS%N.NE.LMNX) THEN
!!$        THIS%N=LMNX
!!$        ALLOCATE(THIS%TYPE(LMNX))
!!$        ALLOCATE(THIS%E(LMNX))
!!$        ALLOCATE(THIS%EATOM(LMNX))
!!$      END IF
!!$
!!$!     ==========================================================================
!!$!     == SUBTRACTS ATOMIC AE POTENTIAL FROM AE TOTAL POTENTIAL                ==
!!$!     ==========================================================================
!!$      ALLOCATE(AEPOT1(NR,LMRXX,NDIMD))
!!$      AEPOT1(:,LMRX,:)=AEPOT(:,LMRX,:)
!!$      AEPOT1(:,1)=AEPOT(:,1)-ATPOT(:)
!!$!
!!$!     ==========================================================================
!!$!     ==   CALCULATE HAMILTONIAN                                              ==
!!$!     ==========================================================================
!!$      ALLOCATE(HAMIL(LMNX,LMNX))      
!!$      HAMIL(:,:)=0.D0
!!$!
!!$      LMN1=0
!!$      DO LN1=1,NC
!!$        L1=LB(LN1)
!!$        DO IM1=1,2*L1+1
!!$          LMN1=LMN1+1
!!$          LM1=L1**2+IM1
!!$!
!!$          LMN2=0
!!$          DO LN2=1,NC
!!$            L2=LB(LN2)
!!$            DO IM2=1,2*L2+1
!!$              LMN2=LMN2+1
!!$              LM2=L2**2+IM2
!!$!
!!$              IF(LMN1.EQ.LMN2) THEN
!!$                HAMIL(LMN1,LMN2)=EB(LN1)
!!$              END IF
!!$!
!!$              AEDMU(:)=0.D0
!!$              DO LM3=1,LMRX
!!$                CALL CLEBSCH(LM1,LM2,LM3,CG)
!!$                IF(CG.NE.0.D0) THEN
!!$                  AEDMU(:)=AEDMU(:)+CG*AEPOT1(:,LM3)
!!$                END IF
!!$              ENDDO
!!$!
!!$              DWORK1(:)=(AEDMU(:)*AEPSI(:,LN1)*AEPSI(:,LN2))*R(:)**2
!!$              CALL RADIAL$INTEGRAL(GID,NR,DWORK1,SVAR)
!!$              HAMIL(LMN1,LMN2)=HAMIL(LMN1,LMN2)+SVAR
!!$!
!!$            ENDDO
!!$          ENDDO
!!$        ENDDO
!!$      ENDDO
!!$      DEALLOCATE(AEPOT1)
!!$!
!!$!     ==========================================================================
!!$!     ==  DIAGONALIZATION OF THE HAMILTONIAN                                  ==
!!$!     ==========================================================================
!!$      ALLOCATE(EIGENVAL(LMNX))
!!$      ALLOCATE(EIGENVEC(LMNX,LMNX))
!!$      CALL LIB$DIAGR8(LMNX,HAMIL,EIGENVAL,EIGENVEC)
!!$      DEALLOCATE(EIGENVEC)
!!$!
!!$!     ==========================================================================
!!$!     ==  WRITE INTO TABLE                                                    ==
!!$!     ==========================================================================
!!$      LMN1=0
!!$      DO LN1=1,NC
!!$        L1=LB(LN1)
!!$        DO IM1=1,2*L1+1
!!$          LMN1=LMN1+1
!!$          THIS%E(LMN1)=EIGENVAL(LMN1)
!!$          THIS%EATOM(LMN1)=EB(LN1)
!!$!
!!$!         == MAIN QUANTUM NUMBER I =============================================
!!$          I=L1
!!$          DO LN2=1,LN1
!!$            IF(LB(LN2).NE.L1) CYCLE
!!$            I=I+1
!!$          ENDDO
!!$!
!!$!         == COMPOSE STRING 1S,2S,2P,3S,3P,3D,... ==============================
!!$          WRITE(THIS%TYPE(LMN1),FMT='(I2)')I
!!$          IF(L1.EQ.0) THEN
!!$            THIS%TYPE(LMN1)=TRIM(THIS%TYPE(LMN1))//'S'
!!$          ELSE IF(L1.EQ.1) THEN
!!$            THIS%TYPE(LMN1)=TRIM(THIS%TYPE(LMN1))//'P'
!!$          ELSE IF(L1.EQ.2) THEN
!!$            THIS%TYPE(LMN1)=TRIM(THIS%TYPE(LMN1))//'D'
!!$          ELSE IF(L1.EQ.3) THEN
!!$            THIS%TYPE(LMN1)=TRIM(THIS%TYPE(LMN1))//'F'
!!$          ELSE
!!$            THIS%TYPE(LMN1)=TRIM(THIS%TYPE(LMN1))//'?'
!!$          END IF
!!$        ENDDO
!!$      ENDDO
!!$      
!!$
!!$!     ==========================================================================
!!$!     ==  CLOSE DOWN                                                          ==
!!$!     ==========================================================================
!!$      DEALLOCATE(LB)
!!$!     DEALLOCATE(FB)
!!$      DEALLOCATE(EB)
!!$      DEALLOCATE(AEPSI)
!!$      DEALLOCATE(HAMIL)
!!$      DEALLOCATE(EIGENVAL)
!!$
!!$      RETURN
!!$      END
!
! SANTOS040617 END
!     ...1.........2.........3.........4.........5.........6.........7.........8
      SUBROUTINE CORE_BEYONDFROZENCORE(IAT,ISP,GID,NR,LMRXX,NDIMD,AEPOT)
!     **************************************************************************
!     **  WORK OUT HAMILTONIAN BETWEEN FROZEN CORE STATES AND ALL-ELECTRON    **
!     **  PARTIAL WAVES
!     **                                                                      **
!     **  IS CALLED FROM PAW_AUGMENTATION, WHICH EXECUTES ONLY ON ONE TASK    **
!     **                                                                      **
! TODO: THE HVV LACKS THE ATOMIC POTENTIAL AND THE KINETIC ENERGY
! TODO: INCLUDE SPIN-ORBIT COUPLING

!     **                                                                      **
!     **************************************************************************
      IMPLICIT NONE
      INTEGER(4),INTENT(IN) :: IAT     ! ATOM INDEX
      INTEGER(4),INTENT(IN) :: ISP     ! ATOM TYPE
      INTEGER(4),INTENT(IN) :: GID     ! GRID ID
      INTEGER(4),INTENT(IN) :: NR      ! #(GRID POINTS)
      INTEGER(4),INTENT(IN) :: LMRXX
      INTEGER(4),INTENT(IN) :: NDIMD   ! #(DENSITY SPIN COMPONENTS)
      REAL(8)   ,INTENT(IN) :: AEPOT(NR,LMRXX,NDIMD) ! 1C-AE POTENTIAL
      COMPLEX(8),PARAMETER  :: CI=(0.D0,1.D0)  !COMPLEX SQRT(-1)
      COMPLEX(8),PARAMETER  :: CZERO=(0.D0,0.D0)  !COMPLEX ZERO
      REAL(8)   ,PARAMETER  :: PI=4.D0*ATAN(1.D0)
      REAL(8)   ,PARAMETER  :: EPSILONNU=0.D0
      REAL(8)               :: ATPOT(NR)  !RADIAL ATOM POTENTIAL 
      REAL(8)   ,ALLOCATABLE:: AEPOT1(:,:,:)!(NR,LMRXX,NDIMD)
      INTEGER(4)            :: LNX        !
      INTEGER(4)            :: LMNX
      INTEGER(4),ALLOCATABLE:: LOX(:)     !MAIN ANGULAR MOMENTUM OF PARTIAL W.
      REAL(8)   ,ALLOCATABLE:: AEPHI(:,:) !(NR,LNX) AE PARTIAL WAVES
      REAL(8)   ,ALLOCATABLE:: PSPHI(:,:) !(NR,LNX) PS PARTIAL WAVES
      INTEGER(4)            :: NB         !#(ATOMIC CORE AND VALENCE STATES)
      INTEGER(4)            :: NC         !#(ATOMIC CORE STATES)
      INTEGER(4)            :: LMNCX
      INTEGER(4),ALLOCATABLE:: LB(:)      !MAIN ANGULAR MOMENTUM
      REAL(8)   ,ALLOCATABLE:: EB(:)      !ENERGY LEVEL
      REAL(8)   ,ALLOCATABLE:: AEPSI(:,:) !(NR,NB) AE WAVE FUNCTION
      REAL(8)               :: R(NR)      !RADIAL GRID
      CHARACTER(32)         :: NAME
      INTEGER(4)            :: LMRX

      INTEGER(4)            :: IC
      REAL(8)               :: AUX2

      INTEGER(4)            :: LN
      INTEGER(4)            :: LMN1,LMN2
      INTEGER(4)            :: LN1,LN2
      INTEGER(4)            :: LM1,LM2,LM3
      INTEGER(4)            :: L1,L2,IM1,IM2
      INTEGER(4)            :: I1A,I1B,I2A,I2B
      INTEGER(4)            :: IDIMD
      REAL(8)               :: AEDMU(NR,NDIMD)
      REAL(8)               :: DWORK1(NR)
      REAL(8)               :: CG          !GAUNT COEFFICIENT
      REAL(8)               :: SVAR
      REAL(8)               :: X(NDIMD)
      COMPLEX(8)            :: CMAT22(2,2)
      COMPLEX(8),ALLOCATABLE:: HCC(:,:)
      COMPLEX(8),ALLOCATABLE:: HCV(:,:)
      COMPLEX(8),ALLOCATABLE:: HVV(:,:)
      COMPLEX(8),ALLOCATABLE:: OCC(:,:)
      COMPLEX(8),ALLOCATABLE:: OCV(:,:)
      COMPLEX(8),ALLOCATABLE:: OVV(:,:)
      REAL(8)   ,ALLOCATABLE:: EIGENVAL(:)
      COMPLEX(8),ALLOCATABLE:: EIGENVEC(:,:)
      COMPLEX(8),ALLOCATABLE:: SELFENERGY(:,:)
      COMPLEX(8),ALLOCATABLE:: GREEN(:,:)
      INTEGER(4)            :: NFILO
!     **************************************************************************
      CALL RADIAL$R(GID,NR,R)
      CALL ATOMLIST$GETCH('NAME',IAT,NAME)
WRITE(*,FMT='(80("="),T20,A)')'CORE_BEYONDFROZENCORE FOR ATOM '//TRIM(NAME)
!
!     ==========================================================================
!     ==  COLLECT ATOMIC CORE WAVE FUNCTIONS AEPSI FROM SETUP OBJECT          ==
!     ==========================================================================
      CALL SETUP$ISELECT(ISP)
      CALL SETUP$GETI4('LNX',LNX)
      ALLOCATE(LOX(LNX))
      CALL SETUP$GETI4A('LOX',LNX,LOX)
      ALLOCATE(AEPHI(NR,LNX))
      ALLOCATE(PSPHI(NR,LNX))
      CALL SETUP$GETR8A('AEPHI',NR*LNX,AEPHI)
      CALL SETUP$GETR8A('PSPHI',NR*LNX,PSPHI)

      CALL SETUP$GETI4('NB',NB)
      CALL SETUP$GETI4('NC',NC)
      IF (NC.EQ.0) THEN
!       == NO CORE STATES, NOTHING TO DO, RETURN ===============================
        CALL FILEHANDLER$UNIT('PROT',NFILO)
!        CALL REPORT$TITLE(NFILO,'NO CORE STATES FOR ATOM '//TRIM(NAME))
        CALL SETUP$UNSELECT()
        RETURN
      END IF
      ALLOCATE(LB(NB))
      ALLOCATE(EB(NB))
      ALLOCATE(AEPSI(NR,NB))
      CALL SETUP$GETR8A('AEPOT',NR,ATPOT)    !POTENTIAL OF THE ATOM
      CALL SETUP$GETI4A('LB',NB,LB)          !MAIN ANGULAR MOMENTUM
      CALL SETUP$GETR8A('EB',NB,EB)          !ATOMIC ENERGY LEVEL
      CALL SETUP$GETR8A('AEPSI',NR*NB,AEPSI) !ATOMIC WAVE FUNCTION
      CALL SETUP$UNSELECT()

WRITE(*,FMT='("LNX=",10I5)')LNX
WRITE(*,FMT='("LOX=",10I5)')LOX
WRITE(*,FMT='("NB=",10I5)')NB
WRITE(*,FMT='("NC=",10I5)')NC
WRITE(*,FMT='("LB=",10I5)')LB(:NC)
WRITE(*,FMT='("EB=",5F12.3)')EB(:NC)
!
!     ==========================================================================
!     ==  CONSTANTS                                                           ==
!     ==========================================================================
      LMRX=0
      LMNCX=0
      DO IC=1,NC
        LMNCX=LMNCX+2*LB(IC)+1
        LMRX=MAX(LMRX,(2*LB(IC)+1)**2)
      ENDDO
      LMNX=0
      DO LN=1,LNX
        LMNX=LMNX+2*LOX(LN)+1
        LMRX=MAX(LMRX,(2*LOX(LN)+1)**2)
      ENDDO
      LMRX=MIN(LMRX,LMRXX)
WRITE(*,FMT='("NDIMD=",10I5)')NDIMD
WRITE(*,FMT='("LMNX=",10I5)')LMNX
WRITE(*,FMT='("LMNCX=",10I5)')LMNCX
WRITE(*,FMT='("LMRX=",10I5)')LMRX
!     ==========================================================================
!     == SUBTRACTS ATOMIC AE POTENTIAL FROM AE TOTAL POTENTIAL                ==
!     ==========================================================================
      ALLOCATE(AEPOT1(NR,LMRX,NDIMD))
      AEPOT1(:,:,:)=AEPOT(:,:LMRX,:)
      AEPOT1(:,1,1)=AEPOT1(:,1,1)-ATPOT(:)
!
!     ==========================================================================
!     ==   CALCULATE CORE-CORE HAMILTONIAN                                    ==
!     ==========================================================================
      ALLOCATE(HCC(2*LMNCX,2*LMNCX))      
      ALLOCATE(OCC(2*LMNCX,2*LMNCX))      
      HCC(:,:)=CZERO
      OCC(:,:)=CZERO
!
      LMN1=0
      DO LN1=1,NC
        L1=LB(LN1)
        DO IM1=1,2*L1+1
          LMN1=LMN1+1
          LM1=L1**2+IM1
          I1A=2*(LMN1-1)+1
          I1B=I1A+1
!
          LMN2=0
          DO LN2=1,NC
            L2=LB(LN2)
            DO IM2=1,2*L2+1
              LMN2=LMN2+1
              LM2=L2**2+IM2
              I2A=2*(LMN2-1)+1
              I2B=I1A+1
!
              AEDMU(:,:)=0.D0
              DO LM3=1,LMRX
                CALL CLEBSCH(LM1,LM2,LM3,CG)
                IF(CG.NE.0.D0) THEN
                  AEDMU(:,:)=AEDMU(:,:)+CG*AEPOT1(:,LM3,:)
                END IF
              ENDDO
!
!             == X(IDIMD)=<PSI(LMN1)|AEPOT(IDIMD)|PSI(LMN2)>
              DO IDIMD=1,NDIMD
                DWORK1(:)=AEDMU(:,IDIMD)*AEPSI(:,LN1)*AEPSI(:,LN2)*R(:)**2
                CALL RADIAL$INTEGRAL(GID,NR,DWORK1,X(IDIMD))
              ENDDO
              CALL CORE_RHOMAG2UPDN(NDIMD,X,CMAT22)
              HCC(I1A:I1B,I2A:I2B)=HCC(I1A:I1B,I2A:I2B)+CMAT22(:,:)

!OCC IS THE UNIT MATRIX BY CONSTRUCTION
              IF(LM1.EQ.LM2) THEN
                DWORK1(:)=AEPSI(:,LN1)*AEPSI(:,LN2)*R(:)**2
                CALL RADIAL$INTEGRAL(GID,NR,DWORK1,X(1))
                CALL CORE_RHOMAG2UPDN(1,X,CMAT22)
                OCC(I1A:I1B,I2A:I2B)=OCC(I1A:I1B,I2A:I2B)+CMAT22
                IF(LMN1.EQ.LMN2) THEN
                  HCC(I1A:I1B,I2A:I2B)=HCC(I1A:I1B,I2A:I2B)+CMAT22*EB(LN1)
                END IF
              END IF

            ENDDO
          ENDDO
        ENDDO
      ENDDO

WRITE(*,FMT='(80("="),T20,A)')' CORE OVERLAP '
WRITE(*,FMT='(I5,"     ",100I10)')0,(LMN2,LMN2=1,LMNCX)
DO LMN1=1,2*LMNCX
  WRITE(*,FMT='(I5,"OCC: ",100F10.5)')LMN1,REAL(OCC(LMN1,:))
ENDDO
WRITE(*,FMT='(80("="),T20,A)')' CORE HAMILTONIAN '
WRITE(*,FMT='(I5,"     ",100I10)')0,(LMN2,LMN2=1,LMNCX)
DO LMN1=1,2*LMNCX
  WRITE(*,FMT='(I5,"HCC: ",100F10.2)')LMN1,REAL(HCC(LMN1,:))
ENDDO


!     ==========================================================================
!     ==   CALCULATE CORE-VALENCE HAMILTONIAN                                 ==
!     ==========================================================================
      ALLOCATE(HCV(2*LMNCX,2*LMNX))      
      ALLOCATE(OCV(2*LMNCX,2*LMNX))      
      HCV(:,:)=CZERO
      OCV(:,:)=CZERO
      LMN1=0
      DO LN1=1,NC
        L1=LB(LN1)
        DO IM1=1,2*L1+1
          LMN1=LMN1+1
          LM1=L1**2+IM1
          I1A=2*(LMN1-1)+1
          I1B=I1A+1
!
          LMN2=0
          DO LN2=1,LNX
            L2=LOX(LN2)
            DO IM2=1,2*L2+1
              LMN2=LMN2+1
              LM2=L2**2+IM2
              I2A=2*(LMN2-1)+1
              I2B=I2A+1
!
              AEDMU(:,:)=0.D0
              DO LM3=1,LMRX
                CALL CLEBSCH(LM1,LM2,LM3,CG)
                IF(CG.NE.0.D0) THEN
                  AEDMU(:,:)=AEDMU(:,:)+CG*AEPOT1(:,LM3,:)
                END IF
              ENDDO
!
!             == X(IDIMD)=<PSI(LMN1)|AEPOT(IDIMD)|PSI(LMN2)>
              DO IDIMD=1,NDIMD
                DWORK1(:)=AEDMU(:,IDIMD)*AEPSI(:,LN1)*AEPHI(:,LN2)*R(:)**2
                CALL RADIAL$INTEGRAL(GID,NR,DWORK1,X(IDIMD))
              ENDDO
              CALL CORE_RHOMAG2UPDN(NDIMD,X,CMAT22)
              HCV(I1A:I1B,I2A:I2B)=HCV(I1A:I1B,I2A:I2B)+CMAT22(:,:)
! 
!OCV IS ZERO BY CONSTRUCTION
              IF(LM1.EQ.LM2) THEN
                DWORK1(:)=AEPSI(:,LN1)*AEPHI(:,LN2)*R(:)**2
                CALL RADIAL$INTEGRAL(GID,NR,DWORK1,X(1))
                CALL CORE_RHOMAG2UPDN(1,X,CMAT22)
                OCV(I1A:I1B,I2A:I2B)=OCV(I1A:I1B,I2A:I2B)+CMAT22(:,:)
                HCV(I1A:I1B,I2A:I2B)=HCV(I1A:I1B,I2A:I2B)+CMAT22(:,:)*EB(LN1)
              END IF

            ENDDO
          ENDDO
        ENDDO
      ENDDO
WRITE(*,FMT='(80("="),T20,A)')' CORE-VALENCE OVERLAP '
WRITE(*,FMT='(I5,"     ",100I10)')0,(LMN2,LMN2=1,2*LMNX)
DO LMN1=1,2*LMNCX
  WRITE(*,FMT='(I5,"OCV: ",100F10.5)')LMN1,REAL(OCV(LMN1,:))
ENDDO
WRITE(*,FMT='(80("="),T20,A)')' CORE-VALENCE HAMILTONIAN '
WRITE(*,FMT='(I5,"     ",100I10)')0,(LMN2,LMN2=1,2*LMNX)
DO LMN1=1,2*LMNCX
  WRITE(*,FMT='(I5,"HCV: ",100F10.2)')LMN1,REAL(HCV(LMN1,:))
ENDDO

!     ==========================================================================
!     ==  VALENCE HAMILTONIAN AND OVERLAP
!     ==========================================================================
      ALLOCATE(HVV(2*LMNX,2*LMNX))      
      ALLOCATE(OVV(2*LMNX,2*LMNX))      
      HVV(:,:)=CZERO
      OVV(:,:)=CZERO
      LMN1=0
      DO LN1=1,LNX
        L1=LOX(LN1)
        DO IM1=1,2*L1+1
          LMN1=LMN1+1
          LM1=L1**2+IM1
          I1A=2*(LMN1-1)+1
          I1B=I1A+1
!
          LMN2=0
          DO LN2=1,LNX
            L2=LOX(LN2)
            DO IM2=1,2*L2+1
              LMN2=LMN2+1
              LM2=L2**2+IM2
              I2A=2*(LMN2-1)+1
              I2B=I2A+1
!
!
              AEDMU(:,:)=0.D0
              DO LM3=1,LMRX
                CALL CLEBSCH(LM1,LM2,LM3,CG)
                IF(CG.NE.0.D0) THEN
                  AEDMU(:,:)=AEDMU(:,:)+CG*AEPOT1(:,LM3,:)
                END IF
              ENDDO
!
              DO IDIMD=1,NDIMD
                DWORK1(:)=AEDMU(:,IDIMD)*AEPHI(:,LN1)*AEPHI(:,LN2)*R(:)**2
                CALL RADIAL$INTEGRAL(GID,NR,DWORK1,X(IDIMD))
              ENDDO
              CALL CORE_RHOMAG2UPDN(NDIMD,X,CMAT22)
              HVV(I1A:I1B,I2A:I2B)=HVV(I1A:I1B,I2A:I2B)+CMAT22(:,:)

!OCV IS ZERO BY CONSTRUCTION
              IF(LM1.EQ.LM2) THEN
                DWORK1(:)=AEPHI(:,LN1)*AEPHI(:,LN2)*R(:)**2
                CALL RADIAL$INTEGRAL(GID,NR,DWORK1,X(1))
                CALL CORE_RHOMAG2UPDN(1,X,CMAT22)
                OVV(I1A:I1B,I2A:I2B)=OVV(I1A:I1B,I2A:I2B)+CMAT22(:,:)
              END IF
! 
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE(AEPOT1)

WRITE(*,FMT='(80("="),T20,A)')' VALENCE OVERLAP '
WRITE(*,FMT='(I5,"     ",100I10)')0,(LMN2,LMN2=1,2*LMNX)
DO LMN1=1,2*LMNX
  WRITE(*,FMT='(I5,"OVV: ",100F10.5)')LMN1,REAL(OVV(LMN1,:))
ENDDO
WRITE(*,FMT='(80("="),T20,A)')' VALENCE HAMILTONIAN '
WRITE(*,FMT='(I5,"     ",100I10)')0,(LMN2,LMN2=1,2*LMNX)
DO LMN1=1,2*LMNX
  WRITE(*,FMT='(I5,"HVV: ",100F10.5)')LMN1,REAL(HVV(LMN1,:))
ENDDO

!!$WRITE(*,*)'CORE DENSITY AT NUCLEUS    ', AEPSI(1,1)**2
!!$WRITE(*,*)'VALENCE DENSITY AT NUCLEUS ', AEPHI(1,1)**2


!
!     ==========================================================================
!     ==  DIAGONALIZATION OF THE HAMILTONIAN                                  ==
!     ==========================================================================
      ALLOCATE(EIGENVAL(2*LMNCX))
      ALLOCATE(EIGENVEC(2*LMNCX,2*LMNCX))
      CALL LIB$DIAGC8(2*LMNCX,HCC,EIGENVAL,EIGENVEC)
      DEALLOCATE(EIGENVEC)

WRITE(*,FMT='("EIGENVALUES ",5F12.3)')EIGENVAL
!     ==========================================================================
!     ==  SELF ENERGY                                                         ==
!     ==========================================================================
      ALLOCATE(SELFENERGY(2*LMNX,2*LMNX))
      ALLOCATE(GREEN(2*LMNCX,2*LMNCX))

      CALL LIB$INVERTC8(2*LMNCX,EPSILONNU*OCC-HCC,GREEN)
WRITE(*,FMT='(80("="),T20,A)')' CORE GREENS FUNCTION '
WRITE(*,FMT='(I5,"     ",100I10)')0,(LMN2,LMN2=1,2*LMNCX)
DO LMN1=1,2*LMNCX
  WRITE(*,FMT='(I5,"GREEN: ",100F10.5)')LMN1,REAL(GREEN(LMN1,:))
ENDDO
      
      SELFENERGY=MATMUL(TRANSPOSE(HCV-EPSILONNU*OCV) &
     &                 ,MATMUL(GREEN,HCV-EPSILONNU*OCV))

WRITE(*,FMT='(80("="),T20,A)')' SELF ENERGY '
WRITE(*,FMT='(I5,"     ",100I10)')0,(LMN2,LMN2=1,2*LMNX)
DO LMN1=1,2*LMNX
  WRITE(*,FMT='(I5,"SIGMA: ",100F8.3)')LMN1,REAL(SELFENERGY(LMN1,:))
ENDDO

!
!     ==========================================================================
!     ==  ENERGY GAIN FROM CORE-VALENCE INTERACTION                           ==
!     ==========================================================================
!!$      ALLOCATE(OVVIN(LMNX,LMNX))      
!!$      CALL LIB$INVERTR8(LMNX,OVV,OVVIN)
!!$         MATMUL(MATMUL(HCV,OVVIN),TRANSPOSE(HCV))

!
!     ==========================================================================
!     ==  CLOSE DOWN                                                          ==
!     ==========================================================================
      DEALLOCATE(LOX)
      DEALLOCATE(AEPHI)
      DEALLOCATE(PSPHI)
      DEALLOCATE(LB)
      DEALLOCATE(EB)
      DEALLOCATE(AEPSI)
      DEALLOCATE(HCC)
      DEALLOCATE(HCV)

      RETURN
      END
!
!     ...1.........2.........3.........4.........5.........6.........7.........8
      SUBROUTINE CORE_RHOMAG2UPDN(NDIMD,X,H)
!     **************************************************************************
!     ** TRANSFORMS MATRIX ELEMENTS INTO THE UPDN REPRESENTATION              **
!     **   NDIMD=1 H=SIGMA_0*X(1)                                             **
!     **   NDIMD=2 H=SIGMA_0*X(1)+SIGMAZ*X(2)                                 **
!     **   NDIMD=4 H=SIGMA_0*X(1)+SIGMAX*X(2)+SIGMAY*X(3)+SIGMAZ*X(4)         **
!     ** WHERE THE SIGMA ARE PAULI MATRICES                                   **
!     **************************************************************************
      IMPLICIT NONE
      INTEGER(4),INTENT(IN) :: NDIMD
      REAL(8)   ,INTENT(IN) :: X(NDIMD)
      COMPLEX(8),INTENT(OUT):: H(2,2)
      COMPLEX(8),PARAMETER  :: CI=(0.D0,1.D0)
      COMPLEX(8),PARAMETER  :: CZERO=(0.D0,0.D0)
!     **************************************************************************
      H(1,1)=X(1)
      H(2,1)=CZERO
      H(1,2)=CZERO
      H(2,2)=X(1)
      IF(NDIMD.EQ.2) THEN
        H(1,1)=H(1,1)+X(2)
        H(2,2)=H(2,2)-X(2)
      ELSE IF(NDIMD.EQ.4) THEN
        H(1,1)=H(1,1)+X(4)
        H(2,2)=H(2,2)-X(4)
        H(1,2)=H(1,2)+X(2)+CI*X(3)
        H(2,1)=H(2,1)+X(2)-CI*X(3)
      END IF
      RETURN
      END
