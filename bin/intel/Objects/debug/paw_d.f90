!#if defined(IBMLICENSE)
      PROGRAM MAIN
!     ******************************************************************
!     **     CP-PAW                                                   **
!     **     (C) COPYRIGHT   I B M   CORPORATION   1990-1997          **
!     **     LICENSED MATERIALS -  PROPERTY     OF     I B M          **
!     ******************************************************************
!     **                                                              **
!     **  THIS THE CAR-PARRINELLO FIRST PRINCIPLES MOLECULAR DYNAMICS **
!     **  PROGRAM BASED ON THE PROJECTOR-AUGMENTED PLANE WAVE METHOD  **
!     **  AND THE LOCAL DENSITY APPROXIMATION.                        **
!     **                                                              **
!     **  PLEASE READ THE PAW.README FILE CONTAINING DISCLAIMER       **
!     **  AND USER AGGREEMENT!                                        **
!     **                                                              **
!     **  AUTHOR: PETER E. BLOECHL                                    **
!     **                                                              **
!     ******************************************************************
      use clock_module
      IMPLICIT NONE
      CHARACTER(32) :: DATIME
      CHARACTER(256):: VERSIONTEXT
      CHARACTER(256):: VERSIONINFO
      INTEGER(4)    :: NFILO
      INTEGER(4)    :: NTASKS,THISTASK
      COMMON/VERSION/VERSIONTEXT
!     ******************************************************************
      VERSIONINFO = '@(#) PAW-VERSION %R% CREATED %U% %E%'
      VERSIONTEXT = 'PROGRAM VERSION %R% CREATED %U% %E%'
!
!     ==================================================================
!     == INITIALIZE MPE ROUTINE FOR PARALLEL PROCESSING               ==
!     ==================================================================
      CALL MPE__INIT
      CALL MPE__QUERY(NTASKS,THISTASK)
!
!     ==================================================================
!     ==  ENTER CAR-PARRINELLO SIMULATION                             ==
!     ==================================================================
      CALL TRACE__PUSH('MAIN')
                              CALL TIMING__START
      CALL PAW
!
!     ==================================================================
!     ==  END OF CARPARRINELLO CALCULATION                            ==
!     ==================================================================
!     ==PRINTING IS ALLOWED ONLY ON THE FIRST TASK =====================
      IF(THISTASK.EQ.1) THEN
        CALL FILEHANDLER__UNIT('PROT',NFILO)
        CALL FILEHANDLER__REPORT(NFILO,'USED')
      END IF
!     == TIMING MUST BE CALLED BY ALL NODES ===========================
      CALL TRACE__PASS('BEFORE TIMING')
      CALL TIMING__PRINT(NFILO,'ALL')
      CALL TRACE__PASS('AFTER TIMING')
      CALL USAGE__REPORT(NFILO)
      CALL TRACE__PASS('AFTER USAGE')
!
!     ==PRINTING IS ALLOWED ONLY ON THE FIRST TASK =====================
      IF(THISTASK.EQ.1) THEN
        CALL CLOCK__NOW(DATIME)
        WRITE(NFILO,FMT='(72("="))')
        WRITE(NFILO,FMT='(72("="),T15,"  PROGRAM FINISHED ",A32,"  ")')DATIME
        WRITE(NFILO,FMT='(72("="))')
      END IF
!
!     ==================================================================
!     ==  CLOSE DOWN                                                  ==
!     ==================================================================
      CALL TRACE__POP
      CALL ERROR__NORMALSTOP
      STOP
      END
!
!     .....................................................PAW..........
      SUBROUTINE PAW
!     ******************************************************************
!     **                                                              **
!     **  THIS IS THE MAIN PAW SUBROUTINE                             **
!     **    1) READ AND INITIALIZE                                    **
!     **    2) ITERATE                                                **
!     **    3) CLOSE DOWN                                             **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      LOGICAL(4)   :: TNWSTR
      LOGICAL(4)   :: TSTOPR
      LOGICAL(4)   :: TSTOP
      LOGICAL(4)   :: TRANP
      LOGICAL(4)   :: TFIRST
      LOGICAL(4)   :: TLAST
      LOGICAL(4)   :: TSTOPH
      LOGICAL(4)   :: TPRINT
      LOGICAL(4)   :: TOLATE,TMERMN
      CHARACTER(80):: TEXT
      INTEGER(4)   :: NFILO     ! FILE UNIT OF PROTOCOL
      INTEGER(4)   :: NFI0      ! TIME STEP COUNTER AT START
      INTEGER(4)   :: NFI       ! TIME STEP COUNTER
      INTEGER(4)   :: NOMORE    !
      INTEGER(4)   :: IPRINT    !
      INTEGER(4)   :: NBEG      ! DECIDES IF RESTART FILE IS READ
      REAL(8)      :: DELT
      logical(4)   :: tchk
!     ******************************************************************
                              CALL TRACE__PUSH('PAW')
                              CALL TIMING__CLOCKON('INITIALIZATION')
      call stopit__setstarttime
!
!     ==================================================================
!     ====  READ CONTROL INPUT DATA FILE "CNTL"                     ====
!     ==================================================================
      CALL READIN(NBEG,NOMORE,IPRINT,DELT,TMERMN,TNWSTR)
!
!     ==================================================================
!     ==  READ STRUCTURAL DATA FROM FILE "STRC"                       ==
!     ==================================================================
      CALL STRCIN
!
!     ==================================================================
!     ==  INITIALIZE ATOMIC SETUPS                                    ==
!     ==================================================================
      CALL SETUP__READ
!
!     ==================================================================
!     ==  INITIALIZE ATOMS OBJECT                                     ==
!     ==================================================================
      CALL ATOMS__INITIALIZE
!
!     ==================================================================
!     ==  GENERATE G-VECTORS                                          ==
!     ==================================================================
                              CALL TIMING__CLOCKON('WAVES__INITIALIZE')
      CALL WAVES__INITIALIZE
                              CALL TIMING__CLOCKOFF('WAVES__INITIALIZE')
!
!     ==================================================================
!     ==  READ RESTART FILE (WAVE FUNCTION COEFFICIENTS, ETC)         ==
!     ==================================================================
      IF(NBEG.GE.0) THEN
                              CALL TIMING__CLOCKON('RESTART I/O')
        CALL READRESTART
                              CALL TIMING__CLOCKOFF('RESTART I/O')
      END IF
!
!     ================================================================
!     ==  REPORT INPUT DATA                                         ==
!     ================================================================
      CALL IO__REPORT
!
!     ==================================================================
!     ==  RUN TIME STATISTICS FOR THE INITIALIZATION                  ==
!     ==================================================================
      CALL FILEHANDLER__UNIT('PROT',NFILO)
                              CALL TIMING__CLOCKOFF('INITIALIZATION')
                              CALL TIMING__PRINT(NFILO,'ALL')
                              CALL TIMING__START
!
!     ==================================================================
!     ==  RETURN IF NO ITERATIONS ARE REQUIRED                        ==
!     ==================================================================
      IF(NOMORE.EQ.0) THEN
        CALL TRACE__POP
        RETURN
      END IF
!
!     ==================================================================
!     ==================================================================
!     == THE BASIC LOOP FOR MOLECULAR DYNAMICS STARTS HERE            ==
!     ==================================================================
!     ==================================================================
      CALL TIMESTEP__GETI4('ISTEP',NFI)
      NFI0=NFI
      TFIRST=.TRUE.
      TLAST=.FALSE.
      TSTOP=.FALSE.
      CALL TRAJECTORYIO__INITIALIZE(IPRINT)
1000  CONTINUE
!
!     ==================================================================
!     ==  ITERATION CONTROL (PROPER STOP ETC. )                       ==
!     ==================================================================
      NFI=NFI+1
      IF(TSTOP) TLAST=.TRUE.
      CALL STOPIT__GETL4('STOP',TSTOP)
      IF(TSTOP) TLAST=.TRUE.
      IF(NFI.GE.NFI0+NOMORE) TLAST=.TRUE.
      TPRINT=(MOD(NFI,IPRINT).EQ.0.OR.TFIRST.OR.TLAST)

      CALL HYPERFINE__SETL4('WAKE',TPRINT)
      CALL GRAPHICS__SETL4('WAKE',TPRINT)
!
!     ==================================================================
!     ==   WRITE RESTART_OUT                                          ==
!     ==================================================================
      IF(TLAST.OR.(TPRINT.AND.(.NOT.TFIRST))) THEN
                              CALL TIMING__CLOCKON('RESTART I/O')
        CALL WRITERESTART
        CALL WAVES__GETL4('WRITERHO',TCHK)
        IF(TCHK) CALL WAVES__FIXRHOWRITE()
                              CALL TIMING__CLOCKOFF('RESTART I/O')
!       CALL MM_PAW_WRITE_RESTART (NFI) ! CALGARY QM/MM IMPLEMENTATION
      END IF
!
!     ==================================================================
!     ==   PERFORM ONE TIME STEP                                      ==
!     ==================================================================
                              CALL TIMING__CLOCKON('TIMESTEP')
!     ==use the line with "not tstop" to avoid an additional last time step
!     if(.not.tstop) CALL TIMESTEP(DELT,TPRINT,NFI,TSTOP,TMERMN)
      CALL DYNOCC__GETL4('DYN',TMERMN)
      CALL TIMESTEP(DELT,TPRINT,NFI,TSTOP,TMERMN)
                              CALL TIMING__CLOCKOFF('TIMESTEP')
!
!     ==================================================================
!     ==   WRITE INFORMATION AND TRAJECTORIES                         ==
!     ==================================================================
!     __ADD TO TRAJECTORIES (TEMPORARY BUFFER)__________________________
      IF(.NOT.TLAST) THEN
        CALL WRITETRAJECTORY(NFI,DELT)
      END IF
!     __ WRITE TRAJECTORY FROM TEMPORARY BUFFER TO FILE_________________
      IF(TPRINT.OR.TLAST) THEN
        CALL TRAJECTORYIO__FLUSH
      END IF
!
!     ==================================================================
!     ==   STOP OR CONTINUE LOOP                                      ==
!     ==================================================================
      TFIRST=.FALSE.
      IF(.NOT.TLAST) GO TO 1000
!     ==================================================================
!     ==================================================================
!     ====   END OF TIME-STEP ITERATION                               ==
!     ==================================================================
!     ==================================================================
      CALL TRACE__POP
      RETURN
      END
!
!     ..................................................................
      MODULE TIMESTEP_MODULE
      INTEGER(4) :: ISTEPNUMBER
      REAL(8)    :: DELTAT
      LOGICAL(4) :: TNEWTHERMOSTAT=.TRUE. !SWITCHES WAVES THERMOSTAT
      END MODULE TIMESTEP_MODULE
!
!     ..................................................................
      SUBROUTINE TIMESTEP__SETR8(ID_,VAL_)
      USE TIMESTEP_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID_
      REAL(8)     ,INTENT(IN) :: VAL_
!     ******************************************************************
      IF(ID_.EQ.'DELTAT') THEN
       DELTAT=VAL_
      ELSE
        CALL ERROR__MSG('ID NOT RECOGNIZED')
        CALL ERROR__STOP('TIMESTEP__SETR8')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE TIMESTEP__GETR8(ID_,VAL_)
      USE TIMESTEP_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID_
      REAL(8)     ,INTENT(OUT):: VAL_
!     ******************************************************************
      IF(ID_.EQ.'DELTAT') THEN
        VAL_=DELTAT
      ELSE
        CALL ERROR__MSG('ID NOT RECOGNIZED')
        CALL ERROR__STOP('TIMESTEP__GETR8')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE TIMESTEP__SETI4(ID_,VAL_)
      USE TIMESTEP_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID_
      INTEGER(4)  ,INTENT(IN) :: VAL_
!     ******************************************************************
      IF(ID_.EQ.'ISTEP') THEN
       ISTEPNUMBER=VAL_
      ELSE
        CALL ERROR__MSG('ID NOT RECOGNIZED')
        CALL ERROR__STOP('TIMESTEP__SETI4')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE TIMESTEP__GETI4(ID_,VAL_)
      USE TIMESTEP_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID_
      INTEGER(4)  ,INTENT(OUT):: VAL_
!     ******************************************************************
      IF(ID_.EQ.'ISTEP') THEN
       VAL_=ISTEPNUMBER
      ELSE
        CALL ERROR__MSG('ID NOT RECOGNIZED')
        CALL ERROR__STOP('TIMESTEP__GETI4')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE TIMESTEP__SETL4(ID_,VAL_)
      USE TIMESTEP_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID_
      LOGICAL(4)  ,INTENT(IN) :: VAL_
!     ******************************************************************
      IF(ID_.EQ.'NEWTHERMOSTAT') THEN
        TNEWTHERMOSTAT=VAL_
      ELSE
        CALL ERROR__MSG('ID NOT RECOGNIZED')
        CALL ERROR__STOP('TIMESTEP__SETL4')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE TIMESTEP__GETL4(ID_,VAL_)
      USE TIMESTEP_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID_
      LOGICAL(4)  ,INTENT(OUT):: VAL_
!     ******************************************************************
      IF(ID_.EQ.'NEWTHERMOSTAT') THEN
       VAL_=TNEWTHERMOSTAT
      ELSE
        CALL ERROR__MSG('ID NOT RECOGNIZED')
        CALL ERROR__STOP('TIMESTEP__GETL4')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE TIMESTEP(DELT,TPRINT,NFI,TSTOP,TMERMN)
!     ******************************************************************
!     ******************************************************************
      USE TIMESTEP_MODULE ,ONLY : DELTAT,ISTEPNUMBER,TNEWTHERMOSTAT
      IMPLICIT NONE
      REAL(8)   ,INTENT(IN)   :: DELT   ! TIME STEP
      LOGICAL(4),INTENT(IN)   :: TPRINT ! ON/OFF SWITCH FOR LONG PRINTOUT
      LOGICAL(4),INTENT(IN)   :: TSTOP  ! ON/OFF SWITCH FOR LAST TIME STEP
      LOGICAL(4),INTENT(IN)   :: TMERMN ! ON/OFF SWITCH FOR MERMIN FUNCTIONAL
      INTEGER(4),INTENT(INOUT):: NFI    ! TIME STEP COUNTER
      INTEGER(4)              :: NFILO
      LOGICAL(4)              :: TFOR   ! ON/OFF SWITCH FOR ATOMIC MOTION
      LOGICAL(4)              :: TGRA   ! ON/OFF SWITCH FOR GRADIENT CORRECTION
      LOGICAL(4)              :: TSPIN  ! ON/OFF SWITCH FOR SPIN POLARIZATION
      INTEGER(4)              :: NSPIN
      REAL(8)                 :: ANNEE  ! FRICTION COEFFICIENT FOR WAVE FUNCTIONS
      REAL(8)                 :: ANNER  ! FRICTION COEFFICIENT FOR NUCLEI
      REAL(8)   ,ALLOCATABLE  :: RHOE(:,:)  !(NNRX1,NSPIN)
      LOGICAL(4)              :: TDIAG=.FALSE. !ON/OFF SWITCH FOR WAVE
                                        ! FUNCTION DIAGONALIZATION
      INTEGER(4)              :: NR1,NR2,NR3,NNRX1
      LOGICAL(4)              :: TCHK
      REAL(8)                 :: TEMPINST
      REAL(8)                 :: ENOSE  ! GENERIC THERMOSTAT ENERGY
      REAL(8)                 :: EKIN   ! GENERIC KINETIC ENERGY
      REAL(8)                 :: SVAR
      LOGICAL(4)              :: TCHK1,TCHK2
      real(8)                 :: stress(3,3)
!     ******************************************************************
                              CALL TRACE__PUSH('TIMESTEP')
      CALL FILEHANDLER__UNIT('PROT',NFILO)
!
      DELTAT=DELT       !-> TIMESTEP_MODULE
      ISTEPNUMBER=NFI   !-> TIMESTEP_MODULE
      CALL ENERGYLIST__RESET
!
      CALL ATOMS__GETL4('MOVE',TFOR)
      CALL DFT__GETL4('GC',TGRA)
      CALL WAVES__SETL4('HAMILTON',TPRINT)
!
!     ==================================================================
!     ==  COMMUNICATE TIME STEP TO OBJECTS                            ==
!     ==================================================================
      CALL WAVES__SETR8('TIMESTEP',DELT)
      CALL ATOMS__SETR8('TIMESTEP',DELT)
      CALL CELL__SETR8('DT',DELT)
      CALL DYNOCC__SETR8('TIMESTEP',DELT)
      CALL QMMM__SETR8('TIMESTEP',DELT)
      CALL CONTINUUM__SETR8('TIMESTEP',DELT)
      CALL THERMOSTAT__SELECT('ATOMS')
      CALL THERMOSTAT__SETR8('TIMESTEP',DELT)
      CALL THERMOSTAT__SELECT('WAVES')
      CALL THERMOSTAT__SETR8('TIMESTEP',DELT)
      CALL DIALS__SETR8('TIMESTEP',DELT)
!
!     ==================================================================
!     ==================================================================
!     ==  CALCULATE TOTAL ENERGY AND FORCES                           ==
!     ==================================================================
!     ==================================================================
!     == LDA TOTAL ENERGY ==============================================
      CALL WAVES__ETOT
!     == EXTERNAL POTENTIAL ACTING ON ATOMS ============================
      CALL VEXT__APPLY
!     == occupations ===================================================
      CALL DYNOCC__GETR8('EPOT',SVAR)
      CALL ENERGYLIST__ADD('TOTAL ENERGY',SVAR)
      CALL ENERGYLIST__SET('ELECTRONIC HEAT',SVAR)
!
!     ==================================================================
!     ==================================================================
!     ==  SET FRICTION FOR ANNEALING SCHEDULE                         ==
!     ==================================================================
!     ==================================================================
      CALL AUTOPI(TCHK)
      IF(.NOT.TSTOP.AND.TCHK) THEN
        CALL STOPIT__SETL4('STOP',TCHK)
        CALL FILEHANDLER__UNIT('PROT',NFILO)
        WRITE(NFILO,*)'STOP SIGNAL FROM AUTOPILOT'
      END IF
!
!     ==================================================================
!     ==  OBTAIN INSTANTANEOUS FRICTION VALUES FROM THERMOSTATS       ==
!     ==  AND COMMUNICATE TO WAVES AND ATOMS OBJECTS                  ==
!     ==================================================================
!
!     == SET THERMOSTAT FRICTION FOR ATOMS =============================
      CALL THERMOSTAT__SELECT('ATOMS')
      CALL THERMOSTAT__GETL4('ON',TCHK1)
      IF(TCHK1) THEN
        CALL THERMOSTAT__GETR8('COOLING',ANNER)
        CALL ATOMS__SETR8('FRICTION',ANNER)
      END IF
!
!     == SET THERMOSTAT FRICTION FOR WAVE FUNCTIONS ====================
      CALL THERMOSTAT__SELECT('WAVES')
      CALL THERMOSTAT__GETL4('ON',TCHK2)
      IF(TCHK2) THEN
        CALL THERMOSTAT__SELECT('WAVES')
        CALL THERMOSTAT__GETR8('COOLING',ANNEE)
        CALL WAVES__SETR8('FRICTION',ANNEE)
      END IF
!
!     == DETERMINE IF CORRECTION FOR WAVE FUNCTION DRAG IS ON ==========
      IF(TCHK1) THEN          ! ATOM THERMOSTAT OR BOTH THERMOSTATS
        CALL WAVES__GETR8('FRICTION',ANNEE)
        CALL ATOMS__SETR8('ANNEE',ANNEE)
        IF(.NOT.TNEWTHERMOSTAT) THEN
          CALL ATOMS__SETR8('ANNEE',0.D0)
        END IF
      ELSE ! FRICTION DYNAMICS (NO THERMOSTAT)
        IF(TCHK2) THEN ! WAVE FUNCTION THERMOSTAT ALONE NOT ALLOWED
          CALL ERROR__MSG('WAVE FUNCTION THERMOSTAT ALONE IS NOT ALLOWED')
          CALL ERROR__STOP('TIMESTEP')
        ELSE
          CALL ATOMS__GETR8('FRICTION',ANNER)
          CALL ATOMS__SETR8('ANNEE',ANNER)
        END IF
      END IF
!
!     ==================================================================
!     ==================================================================
!     ==  PROPAGATE:                                                  ==
!     ==================================================================
!     ==================================================================
!
!     ==================================================================
!     ==  PROPAGATE NUCLEI                                            ==
!     ==================================================================
      CALL ATOMS__PROPAGATE()
      CALL ATOMS__CONSTRAINTS()
!
!     ==================================================================
!     ==  PROPAGATE UNIT CELL                                         ==
!     ==================================================================
      CALL CELL__PROPAGATE()
      CALL CELL__GETR8('EPOT',SVAR)
      CALL ENERGYLIST__set('CELLOSTAT POTENTIAL',SVAR)
      CALL ENERGYLIST__ADD('CONSTANT ENERGY',SVAR)
!
!     ==================================================================
!     ==  PROPAGATE WAVE FUNCTIONS                                    ==
!     ==================================================================
      CALL WAVES__PROPAGATE()
      CALL WAVES__ORTHOGONALIZE()
!
!     ==================================================================
!     ==  PROPAGATE OCCUPATIONS                                       ==
!     ==================================================================
      CALL DYNOCC__PROPAGATE()
!
!     ==================================================================
!     ==  PROPAGATE THERMOSTAT FOR THE NUCLEI                         ==
!     ==================================================================
      CALL THERMOSTAT__SELECT('ATOMS')
      CALL THERMOSTAT__GETL4('ON',TCHK)
      IF(TCHK) THEN
!       __EXTRACT KINETIC ENERGY (USING THE TRUE, EFFECTIVE MASSES)_____
        CALL ATOMS__EKIN(EKIN)
        CALL THERMOSTAT__SETR8('EKIN(SYSTEM)',EKIN)
!       __PROPAGATE THERMOSTAT AND RECORD ITS ENERGY____________________
        CALL THERMOSTAT__PROPAGATE()
      END IF
!
!     ==================================================================
!     ==  PROPAGATE THERMOSTAT FOR THE WAVE FUNCTIONS                 ==
!     ==================================================================
      CALL THERMOSTAT__SELECT('WAVES')
      CALL THERMOSTAT__GETL4('ON',TCHK)
      IF(TCHK) THEN
        IF(TNEWTHERMOSTAT) THEN
          CALL ATOMS__EFFEKIN(EKIN)
          CALL THERMOSTAT__SETR8('TARGET',EKIN)
        END IF
!       CALL ENERGYLIST__RETURN('WAVEFUNCTION KINETIC ENERGY',EKIN)
        CALL WAVES__GETR8('EKIN(PSI)',EKIN)
        CALL THERMOSTAT__SETR8('EKIN(SYSTEM)',EKIN)
        CALL THERMOSTAT__PROPAGATE()
      END IF
!
!     ==================================================================
!     ==================================================================
!     ==  COLLECT KINETIC ENERGIES                                    ==
!     ==================================================================
!     ==================================================================
!
!     ==================================================================
!     __WAVE FUNCTIONS__________________________________________________
!     ==================================================================
      CALL WAVES__GETR8('EKIN(PSI)',EKIN)
      CALL ENERGYLIST__SET('WAVEFUNCTION KINETIC ENERGY',EKIN)
      CALL ENERGYLIST__ADD('CONSTANT ENERGY',EKIN)
!
!     ==================================================================
!     __ NUCLEI__________________________________________________________
!     ==================================================================
!     __CALCULATE KINETIC ENERGY OF THE ATOMS_ _________________________
      CALL ATOMS__EKIN(EKIN)
      CALL ENERGYLIST__SET('IONIC KINETIC ENERGY',EKIN)
      CALL ENERGYLIST__ADD('CONSTANT ENERGY',EKIN)
!     __SUBTRACT THE EFFECTIVE WAVE FUNCTION KINETIC ENERGY_____________
      CALL ATOMS__EFFEKIN(EKIN)
      CALL ENERGYLIST__SET('BO-WAVEFUNCTION KINETIC ENERGY',EKIN)
      CALL ENERGYLIST__ADD('CONSTANT ENERGY',-EKIN)
!     __STORE INSTANTANEOUS ATOMIC TEMPERATURE__________________________
      CALL ATOMS__TEMPERATURE(TEMPINST)
      CALL ENERGYLIST__SET('IONIC TEMPERATURE',TEMPINST)
!
!     ==================================================================
!     == UNIT CELL                                                    ==
!     ==================================================================
      CALL CELL__GETR8('EKIN',EKIN)
      CALL ENERGYLIST__set('CELLOSTAT KINETIC',EKIN)
      CALL ENERGYLIST__ADD('CONSTANT ENERGY',EKIN)
!
!     ==================================================================
!     == OCCUPATIONS                                                  ==
!     ==================================================================
      CALL DYNOCC__GETR8('EKIN',SVAR)
      CALL ENERGYLIST__SET('OCCUPATION KINETIC ENERGY',SVAR)
      CALL ENERGYLIST__ADD('CONSTANT ENERGY',SVAR)
!
!     ==================================================================
!     ==  THERMOSTAT ACTING ON THE NUCLEI                             ==
!     ==================================================================
      CALL THERMOSTAT__SELECT('ATOMS')
      CALL THERMOSTAT__GETL4('ON',TCHK)
      IF(TCHK) THEN
        CALL THERMOSTAT__GETR8('ENERGY',ENOSE)
        CALL ENERGYLIST__SET('ATOM THERMOSTAT',ENOSE)
        CALL ENERGYLIST__ADD('CONSTANT ENERGY',ENOSE)
      END IF
!
!     ==================================================================
!     ==  collect energy of wave function thermostat                  ==
!     ==================================================================
      CALL THERMOSTAT__SELECT('WAVES')
      CALL THERMOSTAT__GETL4('ON',TCHK)
      IF(TCHK) THEN
        IF(TNEWTHERMOSTAT) THEN
          CALL THERMOSTAT__GETR8('EKIN',ENOSE)
        ELSE
          CALL THERMOSTAT__GETR8('ENERGY',ENOSE)
        END IF
        CALL ENERGYLIST__SET('ELECTRON THERMOSTAT',ENOSE)
        CALL ENERGYLIST__ADD('CONSTANT ENERGY',ENOSE)
      END IF
!     EFLUXE=-EKINC*(2.D0*ANNEE/DELT)/EMASS
!
!     ==================================================================
!     ==================================================================
!     == WRITE INFORMATION                                            ==
!     ==================================================================
!     ==================================================================
!     __WRITE PROTOCOLL_________________________________________________
      CALL PRINFO(TPRINT,NFI,DELT)
!
!     ==================================================================
!     ==================================================================
!     == UPDATE DYNAMICAL VARIABLES                                   ==
!     ==================================================================
!     ==================================================================
!     __ELECTRONIC WAVE FUNCTIONS_______________________________________
      CALL WAVES__SWITCH()
!     __ATOMIC POSITIONS________________________________________________
      CALL ATOMS__SWITCH()
!     __UNIT CELL_______________________________________________________
      CALL CELL__SWITCH()
!     __OCCUPATIONS_____________________________________________________
      CALL DYNOCC__SWITCH()
!     __WAVE FUNCTION THERMOSTAT________________________________________
      CALL THERMOSTAT__SELECT('WAVES')
      CALL THERMOSTAT__SWITCH()
!     __ATOM THERMOSTAT_________________________________________________
      CALL THERMOSTAT__SELECT('ATOMS')
      CALL THERMOSTAT__SWITCH()
!
!     ==================================================================
!     == TURN DIALS                                                   ==
!     ==================================================================
      CALL DIALS__APPLY
                              CALL TRACE__POP
      RETURN
      END
!
!     ...................................................STOPIT.........
MODULE STOPIT_MODULE
!**                                                                   **
!**  SET SWITCH TO INITIATE SOFT-KILL                                 **
!**  SWITCH CAN BE SET BY                                             **
!**   A) CREATE A PREDEFINED EXITfILE                                 **
!**   B) BY THE CODE BY SETTINg TSTOP EXPLICITELY                     **
!**                                                                   **
use clock_module
!use clock_module , only : date_time,(.later.)
LOGICAL(4)     :: TSTOP=.FALSE.   ! INITIATE SOFT-KILL
LOGICAL(4)     :: DISTRIBUTED=.FALSE. ! TSTOP=TRUE ON ALL NODES
LOGICAL(4)     :: TNOTIFY=.TRUE.  ! NOTIFICATION ABOUT STOP REQUIRED
LOGICAL(4)     :: EXITFILEREMOVED
integer(4)     :: runtime=-1         ! runtime in seconds
type(date_time) :: starttime
END MODULE STOPIT_MODULE
!
!     ..................................................................
      SUBROUTINE STOPIT__SETL4(ID_,VAL_)
!     ******************************************************************
!     **  STOPIT__SET                                                  **
!     ******************************************************************
      USE STOPIT_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN):: ID_
      LOGICAL(4)  ,INTENT(IN) :: VAL_
!     ******************************************************************
      IF(TRIM(ID_).EQ.'STOP') THEN
        TSTOP=VAL_
      ELSE
        CALL ERROR__MSG('IDENTIFIER NOT RECOGNIZED')
        CALL ERROR__CHVAL('ID_',ID_)
        CALL ERROR__STOP('STOPIT__SETL4')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE STOPIT__SETI4(ID_,VAL_)
!     ******************************************************************
!     **  STOPIT__SET                                                  **
!     ******************************************************************
      USE STOPIT_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN):: ID_
      integer(4)  ,INTENT(IN) :: VAL_
!     ******************************************************************
      IF(TRIM(ID_).EQ.'runtime') THEN
        runtime=val_    ! runtime in seconds
      ELSE
        CALL ERROR__MSG('IDENTIFIER NOT RECOGNIZED')
        CALL ERROR__CHVAL('ID_',ID_)
        CALL ERROR__STOP('STOPIT__SETL4')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE STOPIT__SET(IDENT_,NBYTE_,VAL_)
!     ******************************************************************
!     **  STOPIT__SET                                                  **
!     ******************************************************************
      USE STOPIT_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN):: IDENT_
      INTEGER(4)  ,INTENT(IN) :: NBYTE_
      REAL(8)     ,INTENT(IN) :: VAL_
!     ******************************************************************
      CALL ERROR__MSG('ROUTINE MARKED FOR DELETION')
      CALL ERROR__STOP('STOPIT__SET')
!     CALL LINKEDLIST__SET(__LIST,IDENT_,NBYTE_,VAL_)
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE STOPIT__GETL4(ID_,VAL_)
!     ******************************************************************
!     **  STOPIT__GET                                                  **
!     **  USE STOPIT__GET('TSTOP',4,TSTOP) TO SEE IF STOP IS SET       **
!     ******************************************************************
      USE STOPIT_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID_
      LOGICAL(4)  ,INTENT(OUT):: VAL_
!     ******************************************************************
      IF(TRIM(ID_).EQ.'STOP') THEN
        CALL  STOPIT_UPDATE
        VAL_=TSTOP
      ELSE
        CALL ERROR__MSG('IDENTIFIER NOT RECOGNIZED')
        CALL ERROR__CHVAL('ID_',ID_)
        CALL ERROR__STOP('STOPIT__GETL4')
      END IF
      RETURN
      END
!
!     ...................................................STOPIT.........
      SUBROUTINE STOPIT__setstarttime
!     ******************************************************************
!     **  STOPIT_UPDATE                                               **
!     **                                                              **
!     **  TESTS WHETHER TSTOP HAS BECOME TRUE IN THE MEANWHILE        **
!     **                                                              **
!     ******************************************************************
      USE STOPIT_MODULE
      USE MPE_MODULE
      IMPLICIT NONE
      call clock__now(starttime)
      return
      end

!
!     ...................................................STOPIT.........
      SUBROUTINE STOPIT_UPDATE
!     ******************************************************************
!     **  STOPIT_UPDATE                                               **
!     **                                                              **
!     **  TESTS WHETHER TSTOP HAS BECOME TRUE IN THE MEANWHILE        **
!     **                                                              **
!     ******************************************************************
      USE STOPIT_MODULE
      USE MPE_MODULE
      IMPLICIT NONE
      LOGICAL(4)     :: TCHK
      INTEGER(4)     :: NVAL
      INTEGER(4)     :: NFILO
      CHARACTER(256) :: EXITFILE=' '
      CHARACTER(264) :: CMD
      INTEGER(4)     :: NTASKS,THISTASK
      type(date_time) :: endtime,now
      integer(4)      :: isvar
!     ******************************************************************
      CALL MPE__QUERY(NTASKS,THISTASK)
!
!     ==================================================================
!     ==  CHECK EXITFILE ONLY FROM THE FIRST TASK                     ==
!     ==================================================================
      IF(THISTASK.EQ.1) THEN
!       ================================================================
!       ==  REMOVE EXIT FILE IN THE FIRST REQUEST                     ==
!       ================================================================
        IF(.NOT.EXITFILEREMOVED) THEN
          CALL FILEHANDLER__FILENAME('EXIT',EXITFILE)
          INQUIRE(FILE=EXITFILE,EXIST=TCHK)
          IF(TCHK) THEN
!           == CHAR(114)//CHAR(109)='RM'  (LOWERCASE)
            CMD=CHAR(114)//CHAR(109)//' '//EXITFILE
!           call error__msg('system call removed for absoft')
!           call error__stop('stopit__update')
            CALL SYSTEM(CMD)
          END IF
          EXITFILEREMOVED=.TRUE.
        END IF
!       ================================================================
!       ==  CHECK IF EXITFILE EXISTS                                  ==
!       ================================================================
        IF(.NOT.TSTOP) THEN
          CALL FILEHANDLER__FILENAME('EXIT',EXITFILE)
          INQUIRE(FILE=EXITFILE,EXIST=TSTOP)
          IF(TSTOP) THEN
            CALL FILEHANDLER__UNIT('PROT',NFILO)
            WRITE(NFILO,*)'EXITFILE EXISTS. PROGRAM WILL TERMINATE'
            CALL lib__FLUSHfile(NFILO)
            TNOTIFY=.FALSE.
          END IF
        END IF
      ELSE
        EXITFILEREMOVED=.TRUE.
      END IF

!     ==================================================================
!     ==  CHECK WHETHER TSTOP=T FOR ANY TASK                          ==
!     ==================================================================
      if(runtime.gt.0) then
        endtime=starttime
        isvar=runtime
        endtime%second=endtime%second+isvar
!
        isvar=int(endtime%second/60)
        endtime%second=endtime%second-isvar*60
        endtime%minute=endtime%minute+isvar
!
        isvar=int(endtime%minute/60)
        endtime%minute=endtime%minute-isvar*60
        endtime%hour=endtime%hour+isvar
!
        isvar=int(endtime%minute/24)
        endtime%hour=endtime%hour-isvar*24
        endtime%day=endtime%day+isvar
!
!       Warning! THis choice stops at the last second of the current month!!!!!!!
        call clock__now(now)
        if(now.later.endtime) tstop=.true.
      end if
!
!     ==================================================================
!     ==  CHECK WHETHER TSTOP=T FOR ANY TASK                          ==
!     ==================================================================
      IF(.NOT.DISTRIBUTED) THEN
        IF(TSTOP) THEN
          NVAL=1
        ELSE
          NVAL=0
        ENDIF
        CALL MPE__COMBINE('+',NVAL)
        TSTOP=(NVAL.NE.0)
        DISTRIBUTED=TSTOP
      END IF
!
!     ==================================================================
!     == WRITE MESSAGE IF NOT DONE ALREADY =============================
!     ==================================================================
      IF(TSTOP.AND.TNOTIFY) THEN
        CALL FILEHANDLER__UNIT('PROT',NFILO)
        WRITE(NFILO,*)'STOP SIGNAL RECEIVED'
        CALL lib__FLUSHfile(NFILO)
        TNOTIFY=.FALSE.
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE PRINFO(TPRINT,NFI,DELT)
!     ******************************************************************
!     **  reports on the process of the simulation and invokes        **
!     **  analysis routines                                           **
!     **                                                              **
!     **  prinfo is called once per timestep                          **
!     ******************************************************************
      USE CONTINUUM_CONTROL_MODULE
      IMPLICIT NONE
      LOGICAL(4),INTENT(IN) :: TPRINT
      INTEGER(4),INTENT(IN) :: NFI
      REAL(8)   ,INTENT(IN) :: DELT
      LOGICAL(4),SAVE       :: TFIRST=.TRUE.
      INTEGER(4)            :: NAT
      INTEGER(4)            :: NFILO
      INTEGER(4)            :: NFIL
      REAL(8)               :: GLIB
      INTEGER(4)            :: NTASKS,THISTASK
      REAL(8)               :: PICO
      REAL(8)               :: SECOND
      REAL(8)               :: TIME     ! ACTUAL TIME
      REAL(8)               :: TME1
      REAL(8)               :: ECONS
      REAL(8)               :: ETOT
      REAL(8)               :: EKINP
      REAL(8)               :: EKINC
      REAL(8)               :: ENOSEE
      REAL(8)               :: ENOSEP
      REAL(8)               :: EKINFC
      REAL(8)               :: HEAT
      REAL(8)               :: EEXT
      REAL(8)               :: OCCKIN
      REAL(8)               :: TEMPINST
      REAL(8)               :: CELVIN
      INTEGER(4)            :: ITEMP
      REAL(8)               :: ANNEE
      REAL(8)               :: ANNER
      REAL(8)               :: SVAR
      INTEGER(4)            :: ISVAR
      REAL(8)               :: EFFEKIN
      REAL(8)               :: Ecellpot
      REAL(8)               :: Ecellkin
      REAL(8)               :: ESOLV,EKINQ,QFRIC,QTOT
      LOGICAL(4)            :: TCHK,tchk1
      LOGICAL(4)            :: TCONTINUUM
      LOGICAL(4)            :: TQMMM=.FALSE.
      REAL(8)               :: QMMMKIN   ! EKIN OF QM-MM ENVIRONMENT
      REAL(8)               :: QMMMPOT   ! EPOT OF QM-MM ENVIRONMENT
      REAL(8)               :: QMMMTHERM ! ENERGY OF THE QM-MM THERMOSTAT
      REAL(8)               :: QMMMTEMP  ! TEMPERATURE OF THE QM-MM
      LOGICAL(4)            :: CALGARY_QMMM
      REAL(8)               :: MM_KINETIC_ENERGY
      REAL(8)               :: MM_POT_ENERGY
      REAL(8)               :: MM_TEMP
      INTEGER(4)            :: IMM_TEMP
      REAL(8)               :: MM_NOSE_ENERGY
      REAL(8)               :: MM_FRIC
      CHARACTER(126):: NAME
!     ******************************************************************
                              CALL TRACE__PUSH('PRINFO')
      TIME=DBLE(NFI)*DELT
      CALL ATOMLIST__NATOM(NAT)
      CALL FILEHANDLER__UNIT('PROT',NFILO)
      GLIB=3*NAT-3
      CALL MPE__QUERY(NTASKS,THISTASK)
!
!     ==================================================================
!     ==   HYPERFINE PARAMETERS                                       ==
!     ==================================================================
      IF(TPRINT) CALL HYPERFINE__PRINT
!
!     ==================================================================
!     ==   PLOT WAVE FUNCTIONS, DENSITIES                             ==
!     ==================================================================
      IF(TPRINT) THEN
        CALL GRAPHICS__PLOT
      END IF
                             CALL TRACE__PASS('AFTER GRAPHICS__PLOT')
!
!     ==================================================================
!     ==   THE FOLLOWING IS ONLY EXECUTED ON THE FIRST NODE           ==
!     ==================================================================
!     IF(THISTASK.GT.1) THEN
!       CALL TRACE__POP
!       RETURN
!     END IF

!     CALL MM_RUN(CALGARY_QMMM)
      CALGARY_QMMM = .FALSE.

!     ==================================================================
!     ==   WRITE HEADER FOR PROTOCOLL FOR EACH TIME STEP              ==
!     ==================================================================
                             CALL TRACE__PASS('WRITE HEADER')
      IF(THISTASK.EQ.1.AND.TFIRST) THEN
        IF(TQMMM) THEN
          WRITE(NFILO,FMT='()')
          WRITE(NFILO,FMT='(2X,A5,A9,1X,A4,1X,A9,2A11,2A6,3A10)') &
     &            'NFI','T[PSEC]','T[K]','EKIN(PSI)','E(RHO)','ECONS', &
     &            'ANNEE','ANNER','T(ENV)','EPOT(ENV)','ECNS(ENV)'
        ELSE IF (CALGARY_QMMM) THEN
          WRITE(NFILO,FMT='(/5X,"NFI",3X,"TIME",1X,"TEMP",3X,"EKINC"'   &
     &                  //',5X,"E(RHO)",6X,"ECONS",1X,"ANNEE",1X,"ANNER",' &
     &                  //'"   E_MM  TEMP FRIC")')
        ELSE
          WRITE(NFILO,FMT='()')
          WRITE(NFILO,FMT='(2X,A5,A9,1X,A4,1X,A9,2A11,2A8)') &
     &            'NFI','T[PSEC]','T[K]','EKIN(PSI)','E(RHO)','ECONS', &
     &            'ANNEE','ANNER'
        END IF
        TFIRST=.FALSE.
      END IF
      IF(TPRINT) TFIRST=.TRUE.
!
!     ==================================================================
!     ==   WRITE PROTOCOLL FOR EACH TIME STEP                         ==
!     ==================================================================
                             CALL TRACE__PASS('WRITE PROTOCOLL')
      IF(THISTASK.EQ.1) THEN

        CALL CONSTANTS('PICO',PICO)
        CALL CONSTANTS('SECOND',SECOND)
        CALL CONSTANTS('KB',CELVIN)
        TME1=TIME/(PICO*SECOND)
!       ================================================================
!       == ADD UP CONSERVED ENERGY                                    ==
!       ================================================================
        ECONS=0.D0
!
!       == BASIC LDA + ATOMIC AND FICTITIOUS ELECTRONIC KINETIC ENERGY =
        CALL ENERGYLIST__RETURN('TOTAL ENERGY',ETOT)
        CALL ENERGYLIST__RETURN('IONIC KINETIC ENERGY',EKINP)
        CALL ENERGYLIST__RETURN('WAVEFUNCTION KINETIC ENERGY',EKINC)
        CALL ENERGYLIST__RETURN('BO-WAVEFUNCTION KINETIC ENERGY',EFFEKIN)
        ECONS=ECONS+EKINC-EFFEKIN+EKINP+ETOT
!
!       == ELECTRON AND ATOM THERMOSTATS ===============================
        CALL ENERGYLIST__RETURN('CELLOSTAT KINETIC',ECELLKIN)
        CALL ENERGYLIST__RETURN('CELLOSTAT POTENTIAL',ECELLPOT)
        ECONS=ECONS+ECELLKIN+ECELLPOT
print*,'ecellkin/pot ',ecellkin,ecellpot,ECELLKIN+ECELLPOT
!
!       == ELECTRON AND ATOM THERMOSTATS ===============================
        CALL ENERGYLIST__RETURN('ATOM THERMOSTAT',ENOSEP)
        CALL ENERGYLIST__RETURN('ELECTRON THERMOSTAT',ENOSEE)
        ECONS=ECONS+ENOSEP+ENOSEE
!
        CALL ENERGYLIST__RETURN('CONSTRAINT KINETIC ENERGY',EKINFC)
        ECONS=ECONS+EKINFC
!
!       == OCCUPATIONS =================================================
        CALL ENERGYLIST__RETURN('EPOT',HEAT) ! -T*S_MERMIN-MU*N-B*S
        CALL ENERGYLIST__RETURN('OCCUPATION KINETIC ENERGY',OCCKIN)
        ECONS=ECONS+HEAT+OCCKIN
!
!       == QM-MM ENVIRONMENT ===========================================
        CALL QMMM__GETL4('ON',TQMMM)
        IF(TQMMM) THEN
          CALL QMMM__GETR8('EKIN',QMMMKIN)
          CALL QMMM__GETR8('EPOT',QMMMPOT)
          ECONS=ECONS+QMMMKIN   !POTENTIAL ENERGY ALREADY INCLUDED IN ETOT
!
!         == THERMOSTAT FOR THE ENVIRONMENT ============================
          CALL QMMM__GETR8('ETHERM',QMMMTHERM)
          ECONS=ECONS+QMMMTHERM
        END IF

!       == QMMM CALGARY IMPLEMENTATION  ===============================
        IF (CALGARY_QMMM) THEN
          CALL ENERGYLIST__RETURN('MM KINETIC ENERGY',MM_KINETIC_ENERGY)
          CALL ENERGYLIST__RETURN('MM POT ENERGY',MM_POT_ENERGY)
          CALL ENERGYLIST__RETURN('MM TEMPERATURE',MM_TEMP)
          CALL ENERGYLIST__RETURN('MM THERMOSTAT',MM_NOSE_ENERGY)
          CALL ENERGYLIST__RETURN('MM ATOM FRICTION',MM_FRIC)
          ECONS=ECONS + MM_KINETIC_ENERGY + MM_NOSE_ENERGY
        END IF

!
!       == CONTINUUM (COSMO) ===========================================
        CALL CONTINUUM__RETURNPROTOCOL(TCONTINUUM,ESOLV,EKINQ,QFRIC,QTOT)
        IF(TCONTINUUM) THEN
          CALL ENERGYLIST__RETURN('SURFACE Q EKIN',EKINQ)    ! REPLACE LATER
                                            !BY "CONTINUUM KINETIC ENERGY"
          ECONS=ECONS+EKINQ
        END IF
!
!       == EXTERNAL POTENTIAL============================================
        CALL ENERGYLIST__RETURN('EXTERNAL POTENTIAL',EEXT)
        ECONS=ECONS+EEXT
!
        CALL ENERGYLIST__RETURN('CONSTANT ENERGY',SVAR)
PRINT*,'CONSTANT ENERGY ',ECONS,SVAR
!
!       == SOME OTHER STUFF =============================================
        CALL ENERGYLIST__RETURN('IONIC TEMPERATURE',TEMPINST)
        ITEMP=NINT(TEMPINST/CELVIN)
        CALL WAVES__GETR8('FRICTION',ANNEE)
        CALL ATOMS__GETR8('FRICTION',ANNER)
!
        IF (TCONTINUUM) THEN
          WRITE(NFILO,FMT='("!>",I5,F9.5,1X,I5,F9.5,2F11.5,2F8.5' &
     &                     //',F11.5,1X,D7.2,1X,D7.2,2F6.3)') &
     &               NFI,TME1,ITEMP,EKINC-EFFEKIN,ETOT,ECONS,ANNEE,ANNER &
     &              ,ESOLV,EKINQ,QFRIC,QTOT
        ELSE IF(TQMMM) THEN
          CALL CONSTANTS('KB',CELVIN)
          CALL QMMM__GETI4('NAT:ENV',ISVAR)
          QMMMTEMP=2.D0*QMMMKIN/DBLE(ISVAR)/CELVIN
          WRITE(NFILO,FMT='("!>",I5,F9.5,1X,I5,F9.5,2F11.5,2F6.3' &
     &                //',I10,2F10.5)') &
     &                NFI,TME1,ITEMP,EKINC-EFFEKIN,ETOT,ECONS,ANNEE,ANNER &
     &               ,NINT(QMMMTEMP),QMMMPOT,QMMMKIN+QMMMPOT
        ELSE IF(CALGARY_QMMM) THEN
          IMM_TEMP=NINT(MM_TEMP)
          WRITE(NFILO,FMT='("!>",I5,F8.4,1X,I4,F8.5,2F11.5,2F6.3,1X,F6.3,1X' &
     &       //',I4,1X,F5.2 )') NFI,TME1,ITEMP,EKINC-EFFEKIN,ETOT,ECONS,ANNEE,ANNER   &
     &                     ,MM_POT_ENERGY, IMM_TEMP, MM_FRIC
        ELSE
          WRITE(NFILO,FMT='("!>",I5,F9.5,1X,I5,F9.5,2F11.5,2F8.5)') &
     &                NFI,TME1,ITEMP,EKINC-EFFEKIN,ETOT,ECONS,ANNEE,ANNER
        ENDIF
!
!       ================================================================
!       ==   WRITE ENERGIES TO PROTOCOLL                              ==
!       ================================================================
                             CALL TRACE__PASS('WRITE ENERGIES')
        IF(TPRINT) THEN
          CALL ENERGYLIST__PRINTHEADER(NFILO)
!         ==  BASIC LDA ================================================
          CALL ENERGYLIST__PRINTONE(NFILO,'TOTAL ENERGY')
          CALL ENERGYLIST__PRINTONE(NFILO,'AE  KINETIC')
          CALL ENERGYLIST__PRINTONE(NFILO,'AE  ELECTROSTATIC')
          CALL ENERGYLIST__PRINTONE(NFILO,'AE  EXCHANGE-CORRELATION')
          CALL ENERGYLIST__PRINTONE(NFILO,'BACKGROUND')
          CALL ENERGYLIST__PRINTONE(NFILO,'PS  KINETIC')
          CALL ENERGYLIST__PRINTONE(NFILO,'PS  ELECTROSTATIC')
          CALL ENERGYLIST__PRINTONE(NFILO,'PS  EXCHANGE-CORRELATION')
!         == ATOM KINETIC ENERGY =======================================
          CALL ENERGYLIST__PRINTONE(NFILO,'IONIC KINETIC ENERGY')
          CALL ENERGYLIST__PRINTONE(NFILO,'IONIC TEMPERATURE')
          CALL ENERGYLIST__PRINTONE(NFILO,'ATOM THERMOSTAT')
!
          CALL ENERGYLIST__PRINTONE(NFILO,'WAVEFUNCTION KINETIC ENERGY')
          CALL ENERGYLIST__PRINTONE(NFILO,'BO-WAVEFUNCTION KINETIC ENERGY')
          CALL ENERGYLIST__PRINTONE(NFILO,'ELECTRON THERMOSTAT')
!
!         == CELLOSTAT   ================================================
          CALL ENERGYLIST__PRINTONE(NFILO,'CELLOSTAT KINETIC')
          CALL ENERGYLIST__PRINTONE(NFILO,'CELLOSTAT POTENTIAL')

!         == OCCUPATIONS ================================================
          CALL ENERGYLIST__PRINTONE(NFILO,'ELECTRONIC HEAT')
          CALL ENERGYLIST__PRINTONE(NFILO,'OCCUPATION KINETIC ENERGY')
          CALL ENERGYLIST__PRINTONE(NFILO,'EXTERNAL POTENTIAL')
          IF(TCONTINUUM) THEN
            WRITE(NFILO,*)'-----ELECTROSTATIC SOLVATION CONTRIBUTIONS-----'
            CALL ENERGYLIST__PRINTONE(NFILO,'INTER-TRIANGLE-POTENTIAL')
            CALL ENERGYLIST__PRINTONE(NFILO,'INTRA-TRIANGLE-POTENTIAL')
            CALL ENERGYLIST__PRINTONE(NFILO,'ION-TRIANGLE-POTENTIAL')
            CALL ENERGYLIST__PRINTONE(NFILO,'CAVITY-HARDNESS-POTENTIAL')
            CALL ENERGYLIST__PRINTONE(NFILO,'SURFACETENSION-POTENTIAL')
            CALL ENERGYLIST__PRINTONE(NFILO,'TOTAL-SURFACE-POTENTIAL')
            CALL ENERGYLIST__PRINTONE(NFILO,'SURFACE Q EKIN')
          END IF
          IF (CALGARY_QMMM) THEN
            WRITE(NFILO,*)'    ----- QM/MM MM CONTRIBUTIONS -----'
            CALL ENERGYLIST__PRINTONE(NFILO,'MM KINETIC ENERGY')
            CALL ENERGYLIST__PRINTONE(NFILO,'MM POT ENERGY')
            CALL ENERGYLIST__PRINTONE(NFILO,'MM TEMPERATURE')
            CALL ENERGYLIST__PRINTONE(NFILO,'MM THERMOSTAT')
            WRITE(NFILO,*)
!           CALL MM_PRINT_ENERGY_TERMS (NFILO,1)
!           CALL MM_PRINT_XYZ (NFILO,1)
            WRITE(NFILO,*)
          END IF

        END IF
      END IF
!
!     ==================================================================
!     ==   PROJECTED DENSITY OF STATES                                ==
!     ==================================================================
                              CALL TRACE__PASS('BEFORE STATEANALYSIS')
      IF(TPRINT) THEN
        CALL WAVES__WRITEPDOS
        CALL ATOMLIST__REPORT(NFILO)
        CALL QMMM__REPORT(NFILO)
      ENDIF
                              CALL TRACE__PASS('AFTER STATEANALYSIS')
!
!     ==================================================================
!     ==   CALCULATE OPTICAL MATRIXELEMENTS                           ==
!     ==================================================================
!     CALL OPTICS__EVALUATE
!
!     ==================================================================
!     ==   WRITE OCCUPATIONS AND ENERGIES TO PROTOCOLL                ==
!     ==================================================================
                              CALL TRACE__PASS('BEFORE OCCUPATIONS')
      IF(TPRINT) THEN
        WRITE(NFILO,FMT='()')
        CALL DYNOCC__GETL4('DYN',TCHK)
        IF(TCHK) THEN
          CALL DYNOCC__REPORT(NFILO)
        ELSE
          CALL WAVES__REPORTEIG(NFILO)
        END IF
      END IF
!
!     ==================================================================
!     ==   WRITE CONSTRAINT INFORMATION TO PROTOCOLL                  ==
!     ==================================================================
                              CALL TRACE__PASS('BEFORE CONSTRAINTS')
      IF(TPRINT) THEN
        CALL CONSTRAINTS__REPORT(NFILO,'FULL')
      END IF
                              CALL TRACE__PASS('BEFORE FLUSH')
      CALL lib__FLUSHfile(NFILO)
!
!     ==================================================================
!     ==   WRITE CONSTRAINT INFORMATION                               ==
!     ==================================================================
      CALL FILEHANDLER__UNIT('CONSTRAINTS',NFIL)
      if(thistask.eq.1) then
        WRITE(NFIL,FMT='(20("="),2X,"TIMESTEP: ",I10,2X,20("="))')NFI
        CALL CONSTRAINTS__REPORT(NFIL,'SHORT')
        CALL lib__FLUSHfile(NFIL)
      end if
!
!     ==================================================================
!     ==   WRITE FILE STRC_OUT                                        ==
!     ==================================================================
      CALL STRCOUT
                              CALL TRACE__POP
      RETURN
      END
!
!     ...................................................................
      SUBROUTINE WRITETRAJECTORY(NFI,DELT)
      IMPLICIT NONE
      INTEGER(4),INTENT(IN)  :: NFI
      REAL(8)   ,INTENT(IN)  :: DELT
      REAL(8)                :: CELVIN
      REAL(8)                :: GLIB
      REAL(8)                :: PICO
      REAL(8)                :: SECOND
      INTEGER(4)             :: NTASKS,THISTASK
      REAL(8)   ,ALLOCATABLE :: DWORK(:)
      REAL(8)                :: TIME
      REAL(8)                :: SVAR
      INTEGER(4)             :: NAT
      REAL(8)                :: ECONS
      REAL(8)                :: ETOT
      REAL(8)                :: EKINP
      REAL(8)                :: EKINC
      REAL(8)                :: EFFEKIN
      REAL(8)                :: ENOSEP
      REAL(8)                :: ENOSEE
      REAL(8)                :: EKINFC
      REAL(8)                :: HEAT
      REAL(8)                :: OCCKIN
      LOGICAL(4)             :: TQMMM,TCALGARYQMMM
      REAL(8)                :: QMMMKIN,QMMMPOT,QMMMTHERM
      LOGICAL(4)             :: TCONTINUUM
      REAL(8)                :: ESOLV,EKINQ,QFRIC,QTOT
      REAL(8)                :: EEXT
!     ******************************************************************
      CALL MPE__QUERY(NTASKS,THISTASK)
      IF(THISTASK.NE.1) RETURN
                                 CALL TRACE__PUSH('WRITETRAJECTORY')
      CALL MPE__QUERY(NTASKS,THISTASK)
      CALL CONSTANTS('PICO',PICO)
      CALL CONSTANTS('SECOND',SECOND)
      CALL CONSTANTS('KB',CELVIN)
      TIME=DBLE(NFI)*DELT
!
!     ==================================================================
!     ==  COLLECT ENERGIES                                            ==
!     ==================================================================
!     ================================================================
!     == ADD UP CONSERVED ENERGY                                    ==
!     ================================================================
      ECONS=0.D0
!
!     == BASIC LDA + ATOMIC AND FICTITIOUS ELECTRONIC KINETIC ENERGY =
      CALL ENERGYLIST__RETURN('TOTAL ENERGY',ETOT)
      CALL ENERGYLIST__RETURN('IONIC KINETIC ENERGY',EKINP)
      CALL ENERGYLIST__RETURN('WAVEFUNCTION KINETIC ENERGY',EKINC)
      CALL ENERGYLIST__RETURN('BO-WAVEFUNCTION KINETIC ENERGY',EFFEKIN)
      ECONS=ECONS+EKINC-EFFEKIN+EKINP+ETOT
!
!     == ELECTRON AND ATOM THERMOSTATS ===============================
      CALL ENERGYLIST__RETURN('ATOM THERMOSTAT',ENOSEP)
      CALL ENERGYLIST__RETURN('ELECTRON THERMOSTAT',ENOSEE)
      ECONS=ECONS+ENOSEP+ENOSEE
!
      CALL ENERGYLIST__RETURN('CONSTRAINT KINETIC ENERGY',EKINFC)
      ECONS=ECONS+EKINFC
!
!     == OCCUPATIONS =================================================
      CALL ENERGYLIST__RETURN('ELECTRONIC HEAT',HEAT) ! -T*S_MERMIN
      CALL ENERGYLIST__RETURN('OCCUPATION KINETIC ENERGY',OCCKIN)
      ECONS=ECONS+HEAT+OCCKIN
!
!     == QM-MM ENVIRONMENT ===========================================
      CALL QMMM__GETL4('ON',TQMMM)
      IF(TQMMM) THEN
        CALL QMMM__GETR8('EKIN',QMMMKIN)
        CALL QMMM__GETR8('EPOT',QMMMPOT)
        ECONS=ECONS+QMMMKIN   !POTENTIAL ENERGY ALREADY INCLUDED IN ETOT
!
!       == THERMOSTAT FOR THE ENVIRONMENT ============================
        CALL QMMM__GETR8('ETHERM',QMMMTHERM)
        ECONS=ECONS+QMMMTHERM
      END IF
!
!     == QMMM CALGARY IMPLEMENTATION  ===============================
      TCALGARYQMMM = .FALSE.
      IF(TCALGARYQMMM) THEN
        CALL ENERGYLIST__RETURN('MM KINETIC ENERGY',QMMMKIN)
        CALL ENERGYLIST__RETURN('MM THERMOSTAT',QMMMTHERM)
        ECONS=ECONS+QMMMKIN+QMMMTHERM
      END IF
!
!     == CONTINUUM ===================================================
      CALL CONTINUUM__RETURNPROTOCOL(TCONTINUUM,ESOLV,EKINQ,QFRIC,QTOT)
      IF(TCONTINUUM) THEN
        CALL ENERGYLIST__RETURN('SURFACE Q EKIN',EKINQ)    ! REPLACE
        ECONS=ECONS+EKINQ        ! LATER BY "CONTINUUM KINETIC ENERGY"
      END IF
!
!     == EXTERNAL POTENTIAL============================================
      CALL ENERGYLIST__RETURN('EXTERNAL POTENTIAL',EEXT)
      ECONS=ECONS+EEXT
!
!     ==================================================================
!     ==   WRITE ENERGY TRAJECTORY                                    ==
!     ==================================================================
                              CALL TRACE__PASS('BEFORE E-TRAJECTORY')
      CALL ATOMLIST__NATOM(NAT)
      ALLOCATE(DWORK(MAX(4*NAT,8)))
      CALL CONSTANTS('KB',CELVIN)
      GLIB=3*NAT-3
      IF(GLIB.NE.0.D0) THEN
        SVAR=NINT(EKINP/(.5D0*GLIB*CELVIN))
      ELSE
        SVAR=0.D0
      END IF
      DWORK(1)=SVAR
      DWORK(2)=EKINC
      DWORK(3)=EKINP
      DWORK(4)=ETOT
      DWORK(5)=ECONS
      DWORK(6)=ENOSEE
      DWORK(7)=ENOSEP
      DWORK(8)=HEAT
      CALL TRAJECTORYIO__ADD('ENERGY-TRAJECTORY',NFI,TIME,8,DWORK)
      DEALLOCATE(DWORK)
!
!     ==================================================================
!     ==   WRITE POSITION TRAJECTORY                                  ==
!     ==================================================================
                              CALL TRACE__PASS('BEFORE R-TRAJECTORY')
      ALLOCATE(DWORK(9+4*NAT))
      CALL CELL__GETR8A('T(0)',9,DWORK(1:9))
      CALL ATOMLIST__GETR8A('R(0)',0,3*NAT,DWORK(10:9+3*NAT))
      CALL ATOMLIST__GETR8A('Q',0,NAT,DWORK(10+3*NAT:))
      CALL TRAJECTORYIO__ADD('POSITION-TRAJECTORY',NFI,TIME,9+4*NAT,DWORK)
      DEALLOCATE(DWORK)
!
!     ==================================================================
!     ==   WRITE FORCE TRAJECTORY                                     ==
!     ==================================================================
                              CALL TRACE__PASS('BEFORE F-TRAJECTORY')
      ALLOCATE(DWORK(4*NAT))
      CALL ATOMLIST__GETR8A('FORCE',0,3*NAT,DWORK)
      DWORK(3*NAT+1:4*NAT)=0.D0 ! SHALL CONTAIN IN FUTURE THE POTENTIALS
      CALL TRAJECTORYIO__ADD('FORCE-TRAJECTORY',NFI,TIME,4*NAT,DWORK)
      DEALLOCATE(DWORK)
!
                              CALL TRACE__POP
      RETURN
      END
!#end if


