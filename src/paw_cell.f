!***********************************************************************
!**  OBJECT CELL : PARRINELLO RAHMAN                                  **
!**  USAGE                                                            **
!**    ==  INITIALIZE: ================================               **
!**    SET MANDATORY DT,TREF,TMASS                                    **
!**    SET OPTINALLY: STRESS,PRESSURE,FRIC                            **
!**    GET('T0',9,T0)                                                 **
!**    ==  LOOP: ======================================               **
!**    --  CALCULATE STRESS                                           **
!**    SETR8A('STRESS_I',9,STRESS)                                    **
!**    PROPAGATE                                                      **
!**    GET('FRICMAT',9,U)                                             **
!**    -- ADD OTHERFRICTION TERMS TO U                                **
!**    -- U=U*DT/2                                                    **
!**    -- PROPAGATE POSITIONS                                         **
!**       R(+)=(1+U)**(-1)[2R(0)-(1-U)R(-)+F*DT^2/M                   **
!**    GET('MAPTOCELL',3,U)                                           **
!**    -- RP=U*RP ; R0=U*R0 ; RM=U*RM                                 **
!**    -- RBAS=T0                                                     **
!**    GET('EKIN',EKIN)                                               **
!**    GET('EPOT',EPOT)                                               **
!**    SWITCH                                                         **
!**    GET('T0',9,T0)                                                 **
!**                                                                   **
!***********************************************************************
MODULE CELL_MODULE
IMPLICIT NONE
LOGICAL(4) :: TINIT=.FALSE.
LOGICAL(4) :: TON  =.FALSE.      ! USED TO REQUEST INTERNAL STRESS
LOGICAL(4) :: TMOVE=.FALSE.      ! PROPAGATE UNIT CELL
REAL(8)    :: TREF(3,3)          ! REFERENCE CELL CONSTANTS
REAL(8)    :: STRESS(3,3)=0.D0   ! EXTERNAL STRESS TENSOR
REAL(8)    :: PRESSURE=0.D0      ! EXTERNAL PRESSURE
REAL(8)    :: DELTAT=0.D0
REAL(8)    :: FRIC=0.D0
REAL(8)    :: STRESS_I(3,3)=0.D0 ! INTERNAL STRESS TENSOR
REAL(8)    :: KINSTRESS(3,3)=0.D0 ! INTERNAL STRESS TENSOR
REAL(8)    :: TP(3,3)
REAL(8)    :: T0(3,3)            ! ACTUAL CELL 
REAL(8)    :: TM(3,3)
REAL(8)    :: TMM(3,3)
REAL(8)    :: TMASS              ! MASS FOR THE UNIT CELL DYNAMICS
LOGICAL(4) :: TPROPAGATED=.FALSE.
REAL(8)    :: EPOT=0.D0          ! POTENTIAL ENERGY
REAL(8)    :: EKIN=0.D0          ! KINETIC ENERGY
REAL(8)    :: VREF=0.D0          ! VOLUME OF THE REFERENCE UNIT CELL
REAL(8)    :: V0=0.D0            ! VOLUME OF THE ACTUAL UNIT CELL
LOGICAL(4) :: TSTOP=.FALSE.      ! RESET VELOCITIES TO ZERO
!=====================================
REAL(8)    :: SIGMA(3,3)      ! 
REAL(8)    :: TREFINV(3,3)    ! 
CONTAINS
!     ..................................................................
      SUBROUTINE CELL_INITIALIZE()
      IMPLICIT NONE
      INTEGER(4) :: I
      REAL(8)    :: SVAR
!     ******************************************************************
      IF(TINIT) RETURN
      TINIT=.TRUE.
!
!     ==================================================================
!     ==  CALCULATE TREFINV                                           ==
!     ==================================================================
      VREF=TREF(1,1) * (TREF(2,2)*TREF(3,3)-TREF(3,2)*TREF(2,3)) &
     &    +TREF(2,1) * (TREF(3,2)*TREF(1,3)-TREF(1,2)*TREF(3,3)) &
     &    +TREF(3,1) * (TREF(1,2)*TREF(2,3)-TREF(2,2)*TREF(1,3))
      CALL LIB$INVERTR8(3,TREF,TREFINV)
!
!     ==================================================================
!     ==  SET DEFAULT CELL IF NOT DEFINED OTHERWISE                ==
!     ==================================================================
      SVAR=T0(1,1) * (T0(2,2)*T0(3,3)-T0(3,2)*T0(2,3)) &
     &    +T0(2,1) * (T0(3,2)*T0(1,3)-T0(1,2)*T0(3,3)) &
     &    +T0(3,1) * (T0(1,2)*T0(2,3)-T0(2,2)*T0(1,3))
      IF(SVAR.EQ.0.D0) THEN
        T0=TREF
        TM=T0
        TMM=TM
      ELSE
        SVAR=TM(1,1) * (TM(2,2)*TM(3,3)-TM(3,2)*TM(2,3)) &
     &      +TM(2,1) * (TM(3,2)*TM(1,3)-TM(1,2)*TM(3,3)) &
     &      +TM(3,1) * (TM(1,2)*TM(2,3)-TM(2,2)*TM(1,3))
        IF(SVAR.EQ.0.D0) THEN
          TM=T0
          TMM=TM
        ELSE 
          SVAR=TMM(1,1) * (TMM(2,2)*TMM(3,3)-TMM(3,2)*TMM(2,3)) &
     &        +TMM(2,1) * (TMM(3,2)*TMM(1,3)-TMM(1,2)*TMM(3,3)) &
     &        +TMM(3,1) * (TMM(1,2)*TMM(2,3)-TMM(2,2)*TMM(1,3))
          IF(SVAR.EQ.0) TMM=TM
        END IF
      END IF
      TP=2.D0*T0-TM
!
!     ===================================================================
!     ==  CALCULATE SIGMA MATRIX EQ 2.24                               ==
!     ===================================================================
      SIGMA=STRESS
      SVAR=0.D0
      DO I=1,3
        SVAR=SVAR+SIGMA(I,I)
      ENDDO
      SVAR=SVAR/3.D0
      DO I=1,3
        SIGMA(I,I)=SIGMA(I,I)-SVAR
      END DO
      RETURN
      END SUBROUTINE CELL_INITIALIZE
END MODULE CELL_MODULE
!
!     .........................................................
      SUBROUTINE CELL$REPORT(NFIL)
      USE CELL_MODULE
      IMPLICIT NONE
      INTEGER(4),INTENT(IN) :: NFIL
!     *****************************************************************
      IF(.NOT.TON) RETURN
      CALL REPORT$TITLE(NFIL,'UNIT CELL')
      CALL REPORT$L4VAL(NFIL,'DYNAMICAL UNIT CELL',TMOVE) 
      CALL REPORT$R8VAL(NFIL,'MASS',TMASS,'A.U.') 
      CALL REPORT$R8VAL(NFIL,'FRICTION',FRIC,'DT/2') 
      CALL REPORT$R8VAL(NFIL,'EXTERNAL PRESSURE',PRESSURE,'A.U.') 
      CALL REPORT$R8VAL(NFIL,'REFERENCE VOLUME',VREF,'A.U.') 
      WRITE(NFIL,FMT='("REFERENCE UNIT CELL",T35," STRESS ")')
      WRITE(NFIL,FMT='(3F10.5,T35,3F10.5)')TREF(1,:),STRESS(1,:)
      WRITE(NFIL,FMT='(3F10.5,T35,3F10.5)')TREF(2,:),STRESS(2,:)
      WRITE(NFIL,FMT='(3F10.5,T35,3F10.5)')TREF(3,:),STRESS(3,:)
      END SUBROUTINE CELL$REPORT
!
!     .........................................................
      SUBROUTINE CELL$CONVERT(DT,B,TREF,PERIOD,MASS,FRICTION)
      IMPLICIT NONE
      REAL(8),INTENT(IN) :: DT ! TIME STEP
      REAL(8),INTENT(IN) :: B  ! BULK MODULUS
      REAL(8),INTENT(IN) :: TREF(3,3)  ! REFERENCE UNIT CELL
      REAL(8),INTENT(IN) :: PERIOD ! PERIOD OF CELL OSCILLATION
      REAL(8),INTENT(OUT):: MASS  ! MASS OF THE UNIT CELL
      REAL(8),INTENT(OUT):: FRICTION ! FRICTION PARAMETER FOR CRITICAL DAMPING
      REAL(8)            :: PI
      REAL(8)            :: OMEGA
      REAL(8)            :: VOL
      REAL(8)            :: AMAT(3,3)
!     *****************************************************************
      CALL ERROR$MSG('NOT FULLY IMPLEMENTED: DO NOT USE')
      CALL ERROR$STOP('CELL$CONVERT')
      PI=4.D0*DATAN(1.D0)
      CALL GBASS(TREF,AMAT,VOL)
      OMEGA=2.D0*PI/PERIOD
      MASS=9.D0*B/(OMEGA*VOL)**2
      FRICTION=OMEGA*DT
      RETURN
      END
!
!     .........................................................
      SUBROUTINE CELL$SETL4(ID,VAL)
      USE CELL_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      LOGICAL(4)  ,INTENT(IN) :: VAL
!     *****************************************************************
!
!     =================================================================
!     ==  CALCULATE PRESSURE                                         ==
!     =================================================================
      IF(ID.EQ.'ON') THEN
        TON=VAL
!
!     =================================================================
!     ==  PROPAGATE                                                  ==
!     =================================================================
      ELSE IF(ID.EQ.'MOVE') THEN
        TMOVE=VAL
        IF(TMOVE) THEN
          TON=.TRUE.
        END IF
!
!     =================================================================
!     ==  SET VELOCITY TO ZERO IN THE NEXT TIME STEP                 ==
!     =================================================================
      ELSE IF(ID.EQ.'STOP') THEN
        TSTOP=VAL
!
!     =================================================================
!     ==  DONE                                                       ==
!     =================================================================
      ELSE
        CALL ERROR$MSG('ID NOT RECOGNIZED')
        CALL ERROR$CHVAL('ID',ID)
        CALL ERROR$STOP('CELL$SETL4')
      END IF
      RETURN
      END SUBROUTINE CELL$SETL4
!
!     ..................................................................
      SUBROUTINE CELL$GETL4(ID,VAL)
      USE CELL_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      LOGICAL(4)  ,INTENT(OUT):: VAL
!     *****************************************************************
!
!     =================================================================
!     ==  CALCULATE PRESSURE AND STRESS                              ==
!     =================================================================
      IF(ID.EQ.'ON') THEN
        VAL=TON
!
!     =================================================================
!     ==  PROPAGATE                                                  ==
!     =================================================================
      ELSE IF(ID.EQ.'MOVE') THEN
        VAL=TMOVE
!
!     =================================================================
!     ==  DONE                                                       ==
!     =================================================================
      ELSE
        CALL ERROR$MSG('ID NOT RECOGNIZED')
        CALL ERROR$CHVAL('ID',ID)
        CALL ERROR$STOP('CELL$GETL4')
      END IF
      RETURN
      END SUBROUTINE CELL$GETL4
!
!     ..................................................................
      SUBROUTINE CELL$SETR8(ID,VAL)
      USE CELL_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      REAL(8)     ,INTENT(IN) :: VAL
!     ******************************************************************
!
!     ==================================================================
!     ==  PRESSURE                                                    ==
!     ==================================================================
      IF(ID.EQ.'P') THEN
        PRESSURE=VAL
!
!     ==================================================================
!     ==  TIME STEP                                                   ==
!     ==================================================================
      ELSE IF(ID.EQ.'DT') THEN
        DELTAT=VAL
!
!     ==================================================================
!     ==  FRICTION                                                    ==
!     ==================================================================
      ELSE IF(ID.EQ.'FRICTION') THEN
        FRIC=VAL
!
!     ==================================================================
!     ==  MASS                                                        ==
!     ==================================================================
      ELSE IF(ID.EQ.'MASS') THEN
        TMASS=VAL
!
!     ==================================================================
!     ==  DONE                                                        ==
!     ==================================================================
      ELSE
        CALL ERROR$MSG('ID NOT RECOGNIZED')
        CALL ERROR$CHVAL('ID',ID)
        CALL ERROR$STOP('CELL$SETR8')
      END IF
      RETURN
      END SUBROUTINE CELL$SETR8
!
!     ..................................................................
      SUBROUTINE CELL$GETR8(ID,VAL)
      USE CELL_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      REAL(8)     ,INTENT(OUT):: VAL
!     ******************************************************************
!
!     =================================================================
!     ==  POTENTIAL ENERGY                                           ==
!     =================================================================
      IF(ID.EQ.'EPOT') THEN
        IF(TMOVE.AND.(.NOT.TPROPAGATED)) THEN
          CALL ERROR$MSG('DATA AVALAIBLE ONLY AFTER PROPAGATION')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$STOP('CELL$GETR8')
        END IF
        VAL=EPOT
!
!     =================================================================
!     ==  KINETIC ENERGY                                             ==
!     =================================================================
      ELSE IF(ID.EQ.'EKIN') THEN
        IF(TMOVE.AND.(.NOT.TPROPAGATED)) THEN
          CALL ERROR$MSG('DATA AVALAIBLE ONLY AFTER PROPAGATION')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$STOP('CELL$GETR8')
        END IF
        VAL=EKIN
!
!     =================================================================
!     ==  MASS FOR CELL DYNAMICS                                     ==
!     =================================================================
      ELSE IF(ID.EQ.'MASS') THEN
        VAL=TMASS
!
!     =================================================================
!     ==  DONE                                                       ==
!     =================================================================
      ELSE
        CALL ERROR$MSG('ID NOT RECOGNIZED')
        CALL ERROR$CHVAL('ID',ID)
        CALL ERROR$STOP('CELL$GETR8')
      END IF
      RETURN
      END SUBROUTINE CELL$GETR8
!
!     .........................................................
      SUBROUTINE CELL$SETR8A(ID,LEN,VAL)
      USE CELL_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      INTEGER(4)  ,INTENT(IN) :: LEN
      REAL(8)     ,INTENT(IN) :: VAL(LEN)
!     ******************************************************************
!
!     ==================================================================
!     ==  REFERENCE CELL VECTORS                                   ==
!     ==================================================================
      IF(ID.EQ.'TREF') THEN
        IF(LEN.NE.9) THEN
          CALL ERROR$MSG('SIZE MISMATCH')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$I4VAL('LEN',LEN)
          CALL ERROR$STOP('CELL$SETR8A')
        END IF
        TREF=RESHAPE(VAL,(/3,3/))
!
!     ==================================================================
!     ==  EXTERNAL STRESS TENSOR                                      ==
!     ==================================================================
      ELSE IF(ID.EQ.'STRESS') THEN
        IF(LEN.NE.9) THEN
          CALL ERROR$MSG('SIZE MISMATCH')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$I4VAL('LEN',LEN)
          CALL ERROR$STOP('CELL$SETR8A')
        END IF
        STRESS=RESHAPE(VAL,(/3,3/))
!
!     ==================================================================
!     ==  INTERNAL STRESS TENSOR                                      ==
!     ==================================================================
      ELSE IF(ID.EQ.'STRESS_I') THEN
        IF(LEN.NE.9) THEN
          CALL ERROR$MSG('SIZE MISMATCH')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$I4VAL('LEN',LEN)
          CALL ERROR$STOP('CELL$SETR8A')
        END IF
        STRESS_I=RESHAPE(VAL,(/3,3/))
!
!     ==================================================================
!     ==  INTERNAL STRESS TENSOR                                      ==
!     ==================================================================
      ELSE IF(ID.EQ.'KINSTRESS') THEN
        IF(LEN.NE.9) THEN
          CALL ERROR$MSG('SIZE MISMATCH')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$I4VAL('LEN',LEN)
          CALL ERROR$STOP('CELL$SETR8A')
        END IF
        KINSTRESS=RESHAPE(VAL,(/3,3/))
!
!     ==================================================================
!     ==  CELL VECTORS                                             ==
!     ==================================================================
      ELSE IF(ID.EQ.'T0'.OR.ID.EQ.'T(0)') THEN
        IF(LEN.NE.9) THEN
          CALL ERROR$MSG('SIZE MISMATCH')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$I4VAL('LEN',LEN)
          CALL ERROR$STOP('CELL$SETR8A')
        END IF
        T0=RESHAPE(VAL,(/3,3/))
!
!     ==================================================================
!     ==  CELL VECTORS OF PREVIOUS TIME STEP                       ==
!     ==================================================================
      ELSE IF(ID.EQ.'TM'.OR.ID.EQ.'T(-)') THEN
        IF(LEN.NE.9) THEN
          CALL ERROR$MSG('SIZE MISMATCH')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$I4VAL('LEN',LEN)
          CALL ERROR$STOP('CELL$SETR8A')
        END IF
        TM=RESHAPE(VAL,(/3,3/))
!
!     ==================================================================
!     ==  DONE                                                        ==
!     ==================================================================
      ELSE
        CALL ERROR$MSG('ID NOT RECOGNIZED')
        CALL ERROR$CHVAL('ID',ID)
        CALL ERROR$STOP('CELL$SETR8A')
      END IF
      RETURN
      END SUBROUTINE CELL$SETR8A
!
!     ..................................................................
      SUBROUTINE CELL$GETR8A(ID,LEN,VAL)
      USE CELL_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      INTEGER(4)  ,INTENT(IN) :: LEN
      REAL(8)     ,INTENT(OUT):: VAL(LEN)
      REAL(8)                 :: AMAT(3,3)
!     ******************************************************************
!
!     ==================================================================
!     ==  CELL VECTORS                                             ==
!     ==================================================================
      IF(ID.EQ.'T0'.OR.ID.EQ.'T(0)') THEN
        IF(LEN.NE.9) THEN
          CALL ERROR$MSG('SIZE MISMATCH')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$I4VAL('LEN',LEN)
          CALL ERROR$STOP('CELL$GETR8A')
        END IF
        VAL=RESHAPE(T0,(/9/))
!
!     ==================================================================
!     ==  CELL VECTORS FOR THE NEXT TIME STEP                         ==
!     ==================================================================
      ELSE IF(ID.EQ.'TP'.OR.ID.EQ.'T(+)') THEN
        CALL CELL_INITIALIZE()
        IF(LEN.NE.9) THEN
          CALL ERROR$MSG('SIZE MISMATCH')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$I4VAL('LEN',LEN)
          CALL ERROR$STOP('CELL$GETR8A')
        END IF
        IF(TINIT) THEN
          VAL=RESHAPE(TP,(/9/))
        ELSE
          VAL=RESHAPE(TREF,(/9/))
        END IF
!
!     ==================================================================
!     ==  REFERENCE CELL VECTORS                                   ==
!     ==================================================================
      ELSE IF(ID.EQ.'TREF') THEN
        IF(LEN.NE.9) THEN
          CALL ERROR$MSG('SIZE MISMATCH')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$I4VAL('LEN',LEN)
          CALL ERROR$STOP('CELL$GETR8A')
        END IF
        VAL=RESHAPE(TREF,(/9/))
!
!     ==================================================================
!     ==  STRESS                                                      ==
!     ==================================================================
      ELSE IF(ID.EQ.'STRESS_I') THEN
        IF(LEN.NE.9) THEN
          CALL ERROR$MSG('SIZE MISMATCH')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$I4VAL('LEN',LEN)
          CALL ERROR$STOP('CELL$GETR8A')
        END IF
        VAL=RESHAPE(STRESS_I,(/9/))
!
!     ==================================================================
!     ==  FRICTION MATRIX FOR ATOM DYNAMICS                           ==
!     ==================================================================
      ELSE IF(ID.EQ.'FRICMAT') THEN
        CALL CELL_INITIALIZE()
        IF(LEN.NE.9) THEN
          CALL ERROR$MSG('SIZE MISMATCH')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$I4VAL('LEN',LEN)
          CALL ERROR$STOP('CELL$GETR8A')
        END IF
        IF(TON) THEN
          CALL LIB$INVERTR8(3,T0,AMAT)
          AMAT=MATMUL(TP-TM,AMAT)/(2.D0*DELTAT)
          AMAT=AMAT+TRANSPOSE(AMAT)
          AMAT=AMAT*0.5D0*DELTAT
          VAL=RESHAPE(AMAT,(/9/))
        ELSE
          VAL=0.D0
        ENDIF
!
!     ==================================================================
!     ==  MATRIX TO MAP POSITIONS INTO NEW UNIT CELL                  ==
!     ==================================================================
      ELSE IF(ID.EQ.'MAPTOCELL') THEN
        CALL CELL_INITIALIZE()
        IF(LEN.NE.9) THEN
          CALL ERROR$MSG('SIZE MISMATCH')
          CALL ERROR$CHVAL('ID',ID)
          CALL ERROR$I4VAL('LEN',LEN)
          CALL ERROR$STOP('CELL$GETR8A')
        END IF
        IF(TON) THEN
          CALL LIB$INVERTR8(3,T0,AMAT)
          AMAT=MATMUL(TP,AMAT)
          VAL=RESHAPE(AMAT,(/9/))
        ELSE
          VAL=(/1.D0,0.D0,0.D0,0.D0,1.D0,0.D0,0.D0,0.D0,1.D0/)
        END IF
!
!     ==================================================================
!     ==  DONE                                                        ==
!     ==================================================================
      ELSE
        CALL ERROR$MSG('ID NOT RECOGNIZED')
        CALL ERROR$CHVAL('ID',ID)
        CALL ERROR$STOP('CELL$GETR8A')
      END IF
      RETURN
      END SUBROUTINE CELL$GETR8A
!
!     ..................................................................
      SUBROUTINE CELL$STOP()
      USE CELL_MODULE
      IMPLICIT NONE
!     ******************************************************************
      TSTOP=.TRUE.
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE CELL$PROPAGATE()
      USE CELL_MODULE
      IMPLICIT NONE
      REAL(8)    :: AMAT(3,3)
      REAL(8)    :: BMAT(3,3)
      REAL(8)    :: CMAT(3,3)
      REAL(8)    :: T0INV(3,3)
      REAL(8)    :: ALPHAP(3,3),ALPHA0(3,3),ALPHAM(3,3)
      REAL(8)    :: ALPHADOT(3,3)
      REAL(8)    :: XPMAT(3,3),XMMAT(3,3)
      REAL(8)    :: ONE(3,3)
      REAL(8)    :: SVAR1,SVAR2,SVAR3
      REAL(8)    :: SVAR
      INTEGER(4) :: I,J,ITER
!     ******************************************************************
      IF(.NOT.TON) RETURN
WRITE(*,FMT='("STRESS_I ",3F10.5)')STRESS_I(1,:)
WRITE(*,FMT='("STRESS_I ",3F10.5)')STRESS_I(2,:)
WRITE(*,FMT='("STRESS_I ",3F10.5)')STRESS_I(3,:)
      IF(.NOT.TMOVE) RETURN
      CALL CELL_INITIALIZE()
      IF(TSTOP) THEN
        TM=T0
        TSTOP=.FALSE.
      END IF
      ONE=0.D0
      DO I=1,3
        ONE(I,I)=1.D0
      ENDDO
      V0=T0(1,1)*(T0(2,2)*T0(3,3)-T0(2,3)*T0(3,2)) &
     &  +T0(2,1)*(T0(3,2)*T0(1,3)-T0(3,3)*T0(1,2)) &
     &  +T0(3,1)*(T0(1,2)*T0(2,3)-T0(1,3)*T0(2,2)) 
      IF(ABS(V0).GT.1.D+10*ABS(VREF)) THEN
        CALL ERROR$MSG('CELL DYNAMICS UNSTABLE')
!       CALL ERROR$R8AVAL('CELL',T0)
        CALL ERROR$R8VAL('VOLUME ',V0)
        CALL ERROR$R8VAL('REFERENCE VOLUME ',VREF)
        CALL ERROR$STOP('CELL_PROPAGATE')
      END IF
!     == EXTERNAL STRESS ===============================================
      AMAT=MATMUL(SIGMA,TRANSPOSE(TREFINV))
      AMAT=MATMUL(TREFINV,AMAT)
      AMAT=MATMUL(SIGMA,TRANSPOSE(T0))
      AMAT=MATMUL(T0,AMAT)
      AMAT=AMAT*VREF       
      EPOT=0.5D0*(AMAT(1,1)+AMAT(2,2)+AMAT(3,3))
      AMAT=-AMAT
!     == EXTERNAL PRESSURE =============================================
      AMAT=AMAT-ONE*PRESSURE*V0  ! -PV - V_0 T0*SIGMA*T0^T
      EPOT=EPOT+PRESSURE*V0
!     == STRESS_I ======================================================
      AMAT=AMAT+STRESS_I+KINSTRESS  ! -DE/DALPHA -PV +V T0*SIGMA*T0^T
!     == STRESS PER VOLUME**2 ==========================================
      AMAT=AMAT/V0**2
!  
!     ==PROPAGATE ======================================================
      AMAT=AMAT/TMASS  ! ACCELERATION
      CALL LIB$INVERTR8(3,T0,T0INV)
      ALPHAM=MATMUL(TM,T0INV)-ONE
      ALPHA0=0.D0
      ALPHAP=-ALPHAM
      DO ITER=1,100
        ALPHADOT=(ALPHAP-ALPHAM)/(2.D0*DELTAT)
        BMAT=ALPHADOT-ONE*(ALPHADOT(1,1)+ALPHADOT(2,2)+ALPHADOT(3,3))
        BMAT=BMAT+TRANSPOSE(BMAT)
        BMAT=-BMAT*0.5D0*DELTAT  ! FRICTION CONSTANT
        XPMAT=(1.D0+FRIC)*ONE+BMAT         
        XMMAT=(1.D0-FRIC)*ONE-BMAT         
        CMAT=MATMUL(TRANSPOSE(ALPHADOT),ALPHADOT)
        CMAT=-(CMAT-ONE*(CMAT(1,1)+CMAT(2,2)+CMAT(3,3)))
        CALL LIB$INVERTR8(3,XPMAT,XPMAT)
        ALPHAP=MATMUL(2.D0*ALPHA0-MATMUL(ALPHAM,XMMAT) &
     &               +DELTAT**2*(AMAT+CMAT),XPMAT)
      END DO
      TP=MATMUL(ONE+ALPHAP,T0)
!
!     ==  KINETIC ENERGY  ==============================================
      ALPHADOT=(ALPHAP-ALPHAM)/(2.D0*DELTAT)
      AMAT=MATMUL(ALPHADOT,TRANSPOSE(ALPHADOT))
      EKIN=0.5D0*TMASS*V0**2*(AMAT(1,1)+AMAT(2,2)+AMAT(3,3))
PRINT*,'CELL-EKIN ',EKIN
PRINT*,'CELL-EPOT ',EPOT
AMAT=-0.5D0*MATMUL(MATMUL(STRESS_I,TP-TM),TREFINV)
SVAR=0.D0
DO I=1,3
  SVAR=SVAR+AMAT(I,I)
ENDDO
PRINT*,'CELL-SYS  ',SVAR
PRINT*,'CELL-ETOT ',EKIN+EPOT
WRITE(*,FMT='("TM ",3F10.5)')TM(1,:)
WRITE(*,FMT='("TM ",3F10.5)')TM(2,:)
WRITE(*,FMT='("TM ",3F10.5)')TM(3,:)
WRITE(*,FMT='("T0 ",3F10.5)')T0(1,:)
WRITE(*,FMT='("T0 ",3F10.5)')T0(2,:)
WRITE(*,FMT='("T0 ",3F10.5)')T0(3,:)
WRITE(*,FMT='("TP ",3F10.5)')TP(1,:)
WRITE(*,FMT='("TP ",3F10.5)')TP(2,:)
WRITE(*,FMT='("TP ",3F10.5)')TP(3,:)
      TPROPAGATED=.TRUE.
      RETURN
      END SUBROUTINE CELL$PROPAGATE
!
!     ..................................................................
      SUBROUTINE CELL$SWITCH()
      USE CELL_MODULE
      IMPLICIT NONE
      REAL(8)  :: AMAT(3,3)
!     ******************************************************************
      STRESS_I=0.D0
      EKIN=0.D0
      EPOT=0.D0
      IF(.NOT.TMOVE) RETURN
      TMM=TM
      TM=T0
      T0=TP
      TP=2.D0*T0-TM
      TP=3.D0*T0-3.D0*TM+TMM
!
!     == UPDATE VOUME OF THE UNIT CELL =================================
      V0=T0(1,1) * (T0(2,2)*T0(3,3)-T0(3,2)*T0(2,3)) &
     &  +T0(2,1) * (T0(3,2)*T0(1,3)-T0(1,2)*T0(3,3)) &
     &  +T0(3,1) * (T0(1,2)*T0(2,3)-T0(2,2)*T0(1,3))
      TPROPAGATED=.FALSE.
      RETURN
      END SUBROUTINE CELL$SWITCH
!
!     ..................................................................
      SUBROUTINE CELL$READ(NFIL,NFILO,TCHK)
      USE RESTART_INTERFACE
      USE CELL_MODULE
      USE MPE_MODULE
      IMPLICIT NONE
      INTEGER(4)           ,INTENT(IN) :: NFIL
      INTEGER(4)           ,INTENT(IN) :: NFILO
      LOGICAL(4)           ,INTENT(OUT):: TCHK
      TYPE (SEPARATOR_TYPE),PARAMETER  :: MYSEPARATOR &
                 =SEPARATOR_TYPE(1,'CELL','NONE','MAY2000','NONE')
      TYPE (SEPARATOR_TYPE)            :: SEPARATOR
      INTEGER(4)                       :: NTASKS,THISTASK
!     ******************************************************************
      TCHK=.TRUE.
      SEPARATOR=MYSEPARATOR
      CALL READSEPARATOR(SEPARATOR,NFIL,NFILO,TCHK)
      IF(.NOT.TCHK) RETURN
!
      CALL MPE$QUERY(NTASKS,THISTASK)
!
!     ==================================================================
!     ==  READ DATA                                                   ==
!     ==================================================================
      IF(THISTASK.EQ.1) THEN
        IF(SEPARATOR%VERSION.NE.MYSEPARATOR%VERSION) THEN
          CALL ERROR$MSG('VERSION INCONSISTENCY')
          CALL ERROR$STOP('CELL$READ')
        END IF        
        READ(NFIL)T0,TM
      END IF
      CALL MPE$BROADCAST(1,T0)
      CALL MPE$BROADCAST(1,TM)
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE CELL$WRITE(NFIL,NFILO,TCHK)
!     ******************************************************************
!     **                                                              **
!     ******************************************************************
      USE CELL_MODULE
      USE RESTART_INTERFACE
      IMPLICIT NONE
      INTEGER(4)            ,INTENT(IN) :: NFIL
      INTEGER(4)            ,INTENT(IN) :: NFILO
      LOGICAL(4)            ,INTENT(OUT):: TCHK
      TYPE (SEPARATOR_TYPE),PARAMETER  :: MYSEPARATOR &
               =SEPARATOR_TYPE(1,'CELL','NONE','MAY2000','NONE')
      INTEGER(4)                        :: NTASKS,THISTASK
!     ******************************************************************
      CALL WRITESEPARATOR(MYSEPARATOR,NFIL,NFILO,TCHK)
!
!     ==================================================================
!     ==  WRITE DATA                                                  ==
!     ==================================================================
      CALL MPE$QUERY(NTASKS,THISTASK)
      IF(THISTASK.EQ.1) THEN
        WRITE(NFIL)T0(:,:),TM(:,:),TMM(:,:)
      END IF
      RETURN
      END





