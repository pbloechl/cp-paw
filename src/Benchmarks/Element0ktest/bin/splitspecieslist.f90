!
!     ...1.........2.........3.........4.........5.........6.........7.........8
      PROGRAM MAIN
      IMPLICIT NONE
      INTEGER(4),PARAMETER :: NLINE=20
      CHARACTER(128)       :: LINE(NLINE)
      INTEGER(4)           :: NFIL=10
      INTEGER(4)           :: IL,I1
      CHARACTER(64)        :: ID
      CHARACTER(64)        :: NAME
      LOGICAL(4)           :: TSEMICORE
      CHARACTER(64)        :: FILE
      CHARACTER(64)        :: DIRECTORY
      LOGICAL(4)           :: TON
      DIRECTORY='S'//LOWER_CASE('PECIESFILES/FROM') &
     &        //'S'//LOWER_CASE('PECIESLIST/')

      TON=.FALSE.
      IL=0
      DO 
        IL=IL+1
        IF(IL.GT.NLINE) EXIT
        READ(5,FMT='(A)',END=1000)LINE(IL)
!
!       == PUT BEGINNING OF FILE TO THE TOP
        IF(INDEX(LINE(IL),'!SPECIES').NE.0) THEN
          TON=.TRUE.
          LINE(1)=LINE(IL)
          IL=1
          LINE(2:)=''
        END IF
!
!       == COLLECT ELEMENT NAME ================================================
        IF(INDEX(LINE(IL),'NAME=').NE.0) THEN
          I1=INDEX(LINE(IL),'NAME=')+5
          READ(LINE(IL)(I1:),*)NAME
          IF(NAME(2:2).EQ.'_')NAME(2:2)=' '
        END IF
!
!       == COLLECT ID OF THE SETUP CONSTRUCTION ================================
        IF(INDEX(LINE(IL),'ID=').NE.0) THEN
          I1=INDEX(LINE(IL),'ID=')+3
          READ(LINE(IL)(I1:),*)ID
        END IF
!
!       == WRITE SPECIES FILE ==================================================
        IF(LEN_TRIM(LINE(IL)).EQ.0) THEN
          TSEMICORE=(INDEX(ID,'_SC_').NE.0)
          FILE=TRIM(NAME)//LOWER_CASE('.SPECIES')
          IF(TSEMICORE) FILE=TRIM(FILE)//LOWER_CASE('_SC')
          FILE=TRIM(DIRECTORY)//TRIM(FILE)
          PRINT*,'DOING FILE=',TRIM(FILE),' TSC=',TSEMICORE 
          OPEN(NFIL,FILE=FILE)
          DO I1=1,IL-1
            WRITE(NFIL,*)TRIM(LINE(I1))
          ENDDO
          CLOSE (NFIL)
          TON=.FALSE.
        END IF
      ENDDO
1000  CONTINUE
      IF(TON) STOP 'ERROR IN SPLITSPECIESLIST.F90: LAST LINE MUST BE EMPTY'
      STOP
      CONTAINS
!
!       .1.........2.........3.........4.........5.........6.........7.........8
        FUNCTION LOWER_CASE(OLD) RESULT(NEW)
          IMPLICIT NONE
          CHARACTER(*), INTENT(IN):: OLD
          CHARACTER(LEN(OLD))     :: NEW
          INTEGER(4)              :: I,ISVAR
!         ***************************************************************
          NEW=OLD
          DO I=1,LEN(TRIM(OLD))
            ISVAR=IACHAR(OLD(I:I))
            IF(ISVAR.GE.65.AND.ISVAR.LE.90) NEW(I:I)=ACHAR(ISVAR+32)
          ENDDO
        RETURN
        END FUNCTION LOWER_CASE
!       .1.........2.........3.........4.........5.........6.........7.........8
        FUNCTION UPPER_CASE(OLD) RESULT(NEW)
          IMPLICIT NONE
          CHARACTER(*), INTENT(IN):: OLD
          CHARACTER(LEN(OLD))     :: NEW
          INTEGER(4)              :: I,ISVAR
!         **********************************************************************
          NEW=OLD 
          DO I=1,LEN(TRIM(OLD))
            ISVAR=IACHAR(OLD(I:I))
            IF(ISVAR.GE.97.AND.ISVAR.LE.122) NEW(I:I)=ACHAR(ISVAR-32)
          ENDDO
        END FUNCTION UPPER_CASE
      END
