!======================================================================
!======================================================================
!=====                                                            =====
!=====   OBJECT FOR WRITING AND READING THE RESTART FILE          =====
!=====   (ACTUALLY NOT A WELL DEFINED OBJECT,YET)                 =====
!=====                                                            =====
!======================================================================
!======================================================================
!======================================================================
!
!     ..................................................................
      SUBROUTINE READRESTART
!     ******************************************************************
!     ******************************************************************
      USE RESTART_INTERFACE
      USE MPE_MODULE
      IMPLICIT NONE
      LOGICAL(4)            :: TCHK
      INTEGER(4)            :: NFILO
      INTEGER(4)            :: NFIL
      TYPE(SEPARATOR_TYPE)  :: SEPARATOR
      LOGICAL(4)            :: TREAD
      INTEGER(4)            :: NSTEP
      REAL(8)               :: DELTAT
      INTEGER(4)            :: NTASKS,THISTASK
      INTEGER(4)            :: NFILSTDOUT=6
!     ******************************************************************
      CALL MPE__QUERY(NTASKS,THISTASK)
!     CALL FILEHANDLER__UNIT('PROT',NFILO)
      NFILO=NFILSTDOUT
      WRITE(NFILO,FMT='("READING RESTART FILE")')
!
!     =================================================================
!     == CHECK FILE FORMAT                                           ==
!     =================================================================
      IF(THISTASK.EQ.1) THEN
        CALL FILEHANDLER__UNIT('RESTART_IN',NFIL)
        TCHK=.FALSE.
        REWIND NFIL
        READ(NFIL,END=1111)SEPARATOR
        TCHK=.TRUE.
 1111   CONTINUE
        IF((.NOT.TCHK).OR.(SEPARATOR%ID.NE.HEADER%ID)) THEN
          CALL ERROR__MSG('FORMAT OF RESTART FILE NOT RECOGNIZED')
          CALL ERROR__STOP('READRESTART')
        END IF
        REWIND NFIL
      END IF
!
!     =================================================================
!     == READ FILE                                                   ==
!     =================================================================
      SEPARATOR=HEADER
      TCHK=.TRUE.
      CALL READSEPARATOR(SEPARATOR,NFIL,NFILO,TCHK)
      IF(TCHK) THEN
        IF(SEPARATOR%VERSION.NE.HEADER%VERSION) THEN
          CALL ERROR__STOP('READRESTART')
        END IF
      ELSE
        RETURN
      END IF
 100  CONTINUE
      TREAD=.FALSE.
!
!     ==================================================================
!     ==  END OF FILE                                                 ==
!     ==================================================================
      TCHK=.TRUE.
      SEPARATOR=ENDOFFILE
      CALL READSEPARATOR(SEPARATOR,NFIL,NFILO,TCHK)
      IF(TCHK) THEN
        IF(THISTASK.EQ.1) THEN
          CALL FILEHANDLER__CLOSE('RESTART_IN')
        END IF
        CALL MPE__BROADCAST(1,TCHK)
        RETURN
      END IF
!
!     ==================================================================
!     ==  READ TIMESTEP                                               ==
!     ==================================================================
      CALL TIMESTEP__READ(NFIL,NFILO,TCHK)
      CALL TIMESTEP__GETR8('DELTAT',DELTAT)
      CALL TIMESTEP__GETI4('ISTEP',NSTEP)
      TREAD=TREAD.OR.TCHK
!
!     ==================================================================
!     ==  READ THERMOSTAT FOR THE ATOMS                               ==
!     ==================================================================
      CALL THERMOSTAT__SELECT('ATOMS')
      CALL THERMOSTAT__READ(NFIL,NFILO,TCHK)
      TREAD=TREAD.OR.TCHK
!
!     ==================================================================
!     ==  READ THERMOSTAT FOR THE WAVE FUNCTIONS                      ==
!     ==================================================================
      CALL THERMOSTAT__SELECT('WAVES')
      CALL THERMOSTAT__READ(NFIL,NFILO,TCHK)
      TREAD=TREAD.OR.TCHK
!
!     ==================================================================
!     ==  READ ATOMIC POSITIONS                                       ==
!     ==================================================================
      CALL ATOMS__READ(NFIL,NFILO,TCHK)
      TREAD=TREAD.OR.TCHK
!
!     ==================================================================
!     ==  READ LATTICE VECTORS                                        ==
!     ==================================================================
      CALL CELL__READ(NFIL,NFILO,TCHK)
      TREAD=TREAD.OR.TCHK
!
!     ==================================================================
!     ==  READ OCCUPATIONS                                            ==
!     ==================================================================
      CALL DYNOCC__READ(NFIL,NFILO,TCHK)
      TREAD=TREAD.OR.TCHK
!
!     ==================================================================
!     ==  READ WAVE FUNCTIONS                                         ==
!     ==================================================================
      CALL WAVES__READ(NFIL,NFILO,TCHK)
      TREAD=TREAD.OR.TCHK
!
!     ==================================================================
!     ==  READ MOLECULAR MECHANICS ENVIRONMENT                        ==
!     ==================================================================
      CALL QMMM__READ(NFIL,NFILO,TCHK)
      TREAD=TREAD.OR.TCHK
!
!     ==================================================================
!     ==  READ continuum ENVIRONMENT                                  ==
!     ==================================================================
      CALL continuum__READ(NFIL,NFILO,TCHK)
      TREAD=TREAD.OR.TCHK
!
!     ==================================================================
!     ==  READ CONTINUUM ENVIRONMENT                                  ==
!     ==================================================================
!
!     ==================================================================
!     ==  UNIDENTIFIED OPTION                                         ==
!     ==================================================================
      IF(.NOT.TREAD) THEN
        CALL RESTART__SKIP(NFIL,NFILO)
      END IF
      GOTO 100
      END
!
!     ..................................................................
      SUBROUTINE WRITERESTART
!     ******************************************************************
!     ******************************************************************
      USE RESTART_INTERFACE
      USE MPE_MODULE
      use strings_module
      IMPLICIT NONE
      INTEGER(4)          :: NFIL
      INTEGER(4)          :: NFILO
      INTEGER(4)          :: NSTEP=1
      REAL(8)             :: DELTAT=1.D0
      LOGICAL(4)          :: TCHK
      INTEGER(4)          :: NTASKS,THISTASK
      INTEGER(4),PARAMETER:: NFILSTDOUT=6
      CHARACTER(128)      :: FILENAME
!     ******************************************************************
!     CALL FILEHANDLER__UNIT('PROT',NFILO)
!
!     ==================================================================
!     == RETURN IF WRITING TO A SCRATCH FILE                          ==
!     ==================================================================
      CALL FILEHANDLER__FILENAME('RESTART_OUT',FILENAME)
      TCHK=(FILENAME.EQ.-'/DEV/NULL')
      CALL MPE__BROADCAST(1,TCHK)
      IF(TCHK) RETURN
!
!     ==================================================================
!     == get file unit                                                ==
!     ==================================================================
      NFILO=NFILSTDOUT
      CALL MPE__QUERY(NTASKS,THISTASK)
      IF(THISTASK.EQ.1) THEN
        CALL FILEHANDLER__UNIT('RESTART_OUT',NFIL)
        REWIND NFIL
      ELSE
        NFIL=-1
      END IF
!
!     ==================================================================
!     == WRITE HEADER                                                 ==
!     ==================================================================
      CALL WRITESEPARATOR(HEADER,NFIL,NFILO,TCHK)
!
!     ==================================================================
!     == WRITE THERMOSTAT FOR THE ATOMS                               ==
!     ==================================================================
      CALL TIMESTEP__WRITE(NFIL,NFILO,TCHK)
!
!     ==================================================================
!     == WRITE THERMOSTAT FOR THE ATOMS                               ==
!     ==================================================================
      CALL THERMOSTAT__SELECT('ATOMS')
      CALL THERMOSTAT__WRITE(NFIL,NFILO,TCHK)
!
!     ==================================================================
!     == WRITE THERMOSTAT FOR THE WAVE FUNCTIONS                      ==
!     ==================================================================
      CALL THERMOSTAT__SELECT('WAVES')
      CALL THERMOSTAT__WRITE(NFIL,NFILO,TCHK)
!
!     ==================================================================
!     == WRITE ATOMIC POSITIONS                                       ==
!     ==================================================================
      CALL ATOMS__WRITE(NFIL,NFILO,TCHK)
!
!     ==================================================================
!     == WRITE ATOMIC POSITIONS                                       ==
!     ==================================================================
      CALL CELL__WRITE(NFIL,NFILO,TCHK)
!
!     ==================================================================
!     == WRITE OCCUPATIONS                                            ==
!     ==================================================================
      CALL DYNOCC__WRITE(NFIL,NFILO,TCHK)
!
!     ==================================================================
!     == WRITE WAVE FUNCTIONS                                         ==
!     ==================================================================
      CALL WAVES__WRITE(NFIL,NFILO,TCHK)
!
!     ==================================================================
!     == WRITE MOLECULAR MECHANICS ENVIRONMENT                        ==
!     ==================================================================
      CALL QMMM__WRITE(NFIL,NFILO,TCHK)
!
!     ==================================================================
!     == WRITE continuum ENVIRONMENT                                  ==
!     ==================================================================
      CALL continuum__WRITE(NFIL,NFILO,TCHK)
!
!     ==================================================================
!     == WRITE CONTINUUM           ENVIRONMENT                        ==
!     ==================================================================
!
!     ==================================================================
!     == END OF FILE                                                  ==
!     ==================================================================
      TCHK=.TRUE.
      CALL WRITESEPARATOR(ENDOFFILE,NFIL,NFILO,TCHK)
      IF(THISTASK.EQ.1) CALL FILEHANDLER__CLOSE('RESTART_OUT')
      CALL MPE__BROADCAST(1,TCHK)
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE TIMESTEP__READ(NFIL,NFILO,TCHK)
!     ******************************************************************
!     ******************************************************************
      USE RESTART_INTERFACE
      USE TIMESTEP_MODULE
      USE MPE_MODULE
      IMPLICIT NONE
      INTEGER(4)            ,INTENT(IN)  :: NFIL
      INTEGER(4)            ,INTENT(IN)  :: NFILO
      LOGICAL(4)            ,INTENT(OUT) :: TCHK
      TYPE (SEPARATOR_TYPE) ,PARAMETER   :: MYSEPARATOR &
             =SEPARATOR_TYPE(1,'TIMESTEP','NONE','AUG1996','NONE')
      TYPE (SEPARATOR_TYPE)              :: SEPARATOR
      INTEGER(4)                         :: NTASKS,THISTASK
!     ******************************************************************
      TCHK=.TRUE.
      SEPARATOR=MYSEPARATOR
      CALL READSEPARATOR(SEPARATOR,NFIL,NFILO,TCHK)
      IF(.NOT.TCHK) RETURN
!
      IF(SEPARATOR%VERSION.NE.MYSEPARATOR%VERSION) THEN
        CALL ERROR__MSG('VERSION NOT RECOGNIZED')
        CALL ERROR__CHVAL('ACTUALVERSION ',MYSEPARATOR%VERSION)
        CALL ERROR__CHVAL('VERSION ',SEPARATOR%VERSION)
        CALL ERROR__STOP('READTIMESTEP')
      END IF
      CALL MPE__QUERY(NTASKS,THISTASK)
      IF(THISTASK.EQ.1) THEN
        READ(NFIL)DELTAT,ISTEPNUMBER
      END IF
      CALL MPE__BROADCAST(1,ISTEPNUMBER)
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE TIMESTEP__WRITE(NFIL,NFILO,TCHK)
!     ******************************************************************
      USE RESTART_INTERFACE
      USE TIMESTEP_MODULE
      IMPLICIT NONE
      INTEGER(4)       ,INTENT(IN)  :: NFIL
      INTEGER(4)       ,INTENT(IN)  :: NFILO
      LOGICAL(4)       ,INTENT(OUT) :: TCHK
      TYPE (SEPARATOR_TYPE)         :: MYSEPARATOR &
           =SEPARATOR_TYPE(1,'TIMESTEP','NONE','AUG1996','NONE')
      INTEGER(4)                    :: NTASKS,THISTASK
!     ******************************************************************
      TCHK=.TRUE.
      CALL WRITESEPARATOR(MYSEPARATOR,NFIL,NFILO,TCHK)
      CALL MPE__QUERY(NTASKS,THISTASK)
      IF(THISTASK.EQ.1) THEN
        WRITE(NFIL)DELTAT,ISTEPNUMBER
      END IF
      RETURN
      END
