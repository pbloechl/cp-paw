!
!............................................ENERGYLIST.................
MODULE ENERGYLIST_MODULE
!**                                                                   **
!** KEEPS TRACK OF ENERGIES                                           **
!**                                                                   **
!** FUNCTIONS:                                                        **
!**   SETUNIT                                                         **
!**   RESET                                                           **
!**   SET                                                             **
!**   ADD                                                             **
!**   RETURN                                                          **
!**   PRINT                                                           **
!**                                                                   **
!**                                                                   **
IMPLICIT NONE
INTEGER(4),PARAMETER :: NEX=100
INTEGER(4)           :: NE=0
CHARACTER(40)        :: IDENTIFIER(NEX)
REAL(8)              :: ENERGY(NEX)
CHARACTER(10)        :: UNITNAME='H'
REAL(8)              :: UNITVALUE=1.D0
END MODULE ENERGYLIST_MODULE
!
!     ..................................................................
      SUBROUTINE ENERGYLIST__SETUNIT(UNITNAME_,UNITVALUE_)
!     ==================================================================
!     == SPECIFIES THE ENERGY UNIT                                    ==
!     == (EXPRESSED IN STANDARD UNITS(HARTREE))                       ==
!     == DEFAULT IS HARTREE ATOMIC UNITS                              ==
!     ==================================================================
      USE ENERGYLIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: UNITNAME_
      REAL(8)     ,INTENT(IN) :: UNITVALUE_
!     ******************************************************************
      UNITNAME=UNITNAME_
      UNITVALUE=UNITVALUE_
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ENERGYLIST__RESET
!     ==================================================================
!     == SETS ALL ENERGIES TO ZERO                                    ==
!     ==================================================================
      USE ENERGYLIST_MODULE
      IMPLICIT NONE
      INTEGER(4)             :: I
!     ******************************************************************
      NE=0
      DO I=1,NEX
        IDENTIFIER(I)=' '
        ENERGY(I)=0.D0
      ENDDO
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ENERGYLIST__SET(STRING_,VALUE_)
!     ==================================================================
!     == SETS ENERGY                                                  ==
!     == STRING  IDENTIFIES THE ENERGY                                ==
!     == VALUE   IS THE VALUE OF THE ENERGY                           ==
!     ==================================================================
      USE ENERGYLIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: STRING_
      REAL(8)     ,INTENT(IN) :: VALUE_
      CHARACTER(40)           :: STRING
      LOGICAL(4)              :: TSET
      INTEGER(4)              :: I
!     ******************************************************************
      TSET=.FALSE.
      STRING=STRING_
      DO I=1,NE
        IF(STRING.EQ.IDENTIFIER(I)) THEN
          ENERGY(I)=VALUE_
          TSET=.TRUE.
          EXIT
        END IF
      ENDDO
      IF(.NOT.TSET) THEN
        IF(NE.EQ.NEX) THEN
          CALL ERROR__MSG('ATTEMPT TO STORE MORE ENERGIES THAN ALLOWED')
          CALL ERROR__MSG('INCREASE PARAMETER NEX IN ROUTINE ENERGIES')
          CALL ERROR__STOP('ENERGYLIST__SET')
        END IF
        NE=NE+1
        IDENTIFIER(NE)=STRING
        ENERGY(NE)=VALUE_
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ENERGYLIST__ADD(STRING_,VALUE_)
!     ==================================================================
!     == SETS ENERGY                                                  ==
!     == STRING  IDENTIFIES THE ENERGY                                ==
!     == VALUE   IS THE VALUE OF THE ENERGY                           ==
!     ==================================================================
      USE ENERGYLIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: STRING_
      REAL(8)     ,INTENT(IN) :: VALUE_
      CHARACTER(40)           :: STRING
      INTEGER(4)              :: I
      LOGICAL(4)              :: TSET
!     ******************************************************************
      TSET=.FALSE.
      STRING=STRING_
      DO I=1,NE
        IF(STRING.EQ.IDENTIFIER(I)) THEN
          ENERGY(I)=ENERGY(I)+VALUE_
          TSET=.TRUE.
          EXIT
        END IF
      ENDDO
      IF(.NOT.TSET) THEN
        IF(NE.EQ.NEX) THEN
          CALL ERROR__MSG('ATTEMPT TO STORE MORE ENERGIES THAN ALLOWED')
          CALL ERROR__MSG('INCREASE PARAMETER NEX IN ROUTINE ENERGIES')
          CALL ERROR__STOP('ENERGYLIST__ADD')
        END IF
        NE=NE+1
        IDENTIFIER(NE)=STRING
        ENERGY(NE)=VALUE_
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ENERGYLIST__RETURN(STRING_,VALUE_)
!     ==================================================================
!     == RETURNS ENERGY VALUE                                         ==
!     ==================================================================
      USE ENERGYLIST_MODULE
      IMPLICIT NONE
      CHARACTER(*) ,INTENT(in)  :: STRING_
      REAL(8)      ,INTENT(OUT) :: VALUE_
      CHARACTER(40)             :: STRING
      INTEGER(4)                :: I
!     ******************************************************************
      VALUE_=0.D0
      STRING=STRING_
      DO I=1,NE
        IF(STRING.EQ.IDENTIFIER(I)) THEN
          VALUE_=ENERGY(I)
          EXIT
        END IF
      ENDDO
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ENERGYLIST__PRINT(NFIL)
!     ==================================================================
!     == PRINTS ALL STORED ENERGIES                                   ==
!     ==================================================================
      USE ENERGYLIST_MODULE
      IMPLICIT NONE
      INTEGER(4),INTENT(IN) :: NFIL
      INTEGER(4)            :: I
!     ******************************************************************
      WRITE(NFIL,FMT='("ENERGY REPORT"/"=============")')
      DO I=1,NE
        WRITE(NFIL,FMT='(A40,":",F15.7," ",A10)') &
     &        IDENTIFIER(I),ENERGY(I)/UNITVALUE,UNITNAME
      ENDDO
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE  ENERGYLIST__PRINTHEADER(NFIL)
!     ==================================================================
!     == PRINTS ALL STORED ENERGIES                                   ==
!     ==================================================================
      USE ENERGYLIST_MODULE
      IMPLICIT NONE
      INTEGER(4),INTENT(IN) :: NFIL
!     ******************************************************************
      WRITE(NFIL,FMT='("ENERGY REPORT"/"=============")')
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ENERGYLIST__PRINTONE(NFIL,STRING_)
!     ==================================================================
!     == PRINTS ALL STORED ENERGIES                                   ==
!     ==================================================================
      USE ENERGYLIST_MODULE
      IMPLICIT NONE
      INTEGER(4)   ,INTENT(IN) :: NFIL
      CHARACTER(*) ,INTENT(in) :: STRING_
      CHARACTER(40)            :: STRING
      INTEGER(4)               :: I
!     ******************************************************************
      STRING=STRING_
      DO I=1,NE
        IF(IDENTIFIER(I).EQ.STRING) THEN
          WRITE(NFIL,FMT='(A40,":",F15.7," ",A10)') &
     &        IDENTIFIER(I),ENERGY(I)/UNITVALUE,UNITNAME
        END IF
      ENDDO
      RETURN
      END
!
!     ..................................................................
      MODULE GROUPLIST_MODULE
      LOGICAL(4)                :: TINI=.FALSE.
      INTEGER(4)                :: NATOM=0
      INTEGER(4)                :: NGROUPX=0
      INTEGER(4)                :: NGROUP=0
      CHARACTER(32),ALLOCATABLE :: IDENTIFIER(:) ! NGROUPX
      LOGICAL(4)   ,ALLOCATABLE :: TMEMBER(:,:)
      END MODULE GROUPLIST_MODULE
!
!     ..................................................................
      SUBROUTINE GROUPLIST__INITIALIZE(NATOM1,NGROUPX1)
!     ******************************************************************
!     **  INITIALIZE                                                  **
!     ******************************************************************
      USE GROUPLIST_MODULE
      IMPLICIT NONE
      INTEGER(4),INTENT(IN)  :: NATOM1
      INTEGER(4),INTENT(IN)  :: NGROUPX1
      INTEGER(4)             :: IGROUP,IAT
!     ******************************************************************
      IF(TINI) THEN
        CALL ERROR__MSG('GROUPLIST  INITIALIZED')
        CALL ERROR__STOP('GROUPLIST')
      END IF
      TINI=.TRUE.
      NATOM=NATOM1
      NGROUPX=NGROUPX1
!     CALL MEMORY__ALLOCATE(32,__IDENTIFIER,NGROUPX)
!     CALL MEMORY__ALLOCATE(4,__TMEMBER,NATOM*NGROUPX)
      ALLOCATE(IDENTIFIER(NGROUPX))
      ALLOCATE(TMEMBER(NATOM,NGROUPX))
      NGROUP=0
      DO IGROUP=1,NGROUPX
        IDENTIFIER(IGROUP)=' '
        DO IAT=1,NATOM
          TMEMBER(IAT,IGROUP)=.FALSE.
        ENDDO
      ENDDO
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE GROUPLIST__ADD(IDENT1,IAT1)
!     ******************************************************************
!     **  ADD ATOM TO THE GROUP                                       **
!     ******************************************************************
      USE GROUPLIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN):: IDENT1
      INTEGER(4),INTENT(IN)  :: IAT1
      INTEGER(4)             :: IGROUP,I
!     ******************************************************************
      IF(.NOT.TINI) THEN
        CALL ERROR__MSG('GROUPLIST NOT INITIALIZED')
        CALL ERROR__STOP('GROUPLIST__ADD')
      END IF
!
!     ==================================================================
!     ==  DETERMINE GROUP INDEX                                       ==
!     ==================================================================
      IGROUP=0
      DO I=1,NGROUP
        IF(TRIM(IDENT1).EQ.TRIM(IDENTIFIER(I))) THEN
          IGROUP=I
          EXIT
        END IF
      ENDDO
      IF(IGROUP.EQ.0) THEN
        NGROUP=NGROUP+1
        IF(NGROUP.GT.NGROUPX) THEN
          CALL ERROR__MSG('MORE GROUPS THAN ALLOWED BY INITIALIZATION')
          CALL ERROR__MSG('CURRENT MAX. NUMBER OF GROUPS=NGROUPX')
          CALL ERROR__I4VAL('NGROUPX',NGROUPX)
          CALL ERROR__STOP('GROUPLIST__ADD')
        ENDIF
        IGROUP=NGROUP
        IDENTIFIER(NGROUP)=IDENT1
      END IF
      IF(IAT1.GT.NATOM) THEN
        CALL ERROR__MSG('ATOMINDEX LARGER THAN MAXIMUM')
        CALL ERROR__STOP('GROUPLIST__ADD')
      END IF
      TMEMBER(IAT1,IGROUP)=.TRUE.
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE GROUPLIST__MEMBERS(IDENT1,NATOM1,TMEMBER1)
!     ******************************************************************
!     **  REQUEST FOR MEMBERS OF A GIVEN GROUP                        **
!     ******************************************************************
      USE GROUPLIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: IDENT1
      INTEGER(4)  ,INTENT(IN) :: NATOM1
      LOGICAL(4)  ,INTENT(OUT):: TMEMBER1(NATOM1)
      INTEGER(4)              :: IGROUP,IAT,I
!     ******************************************************************
      IF(.NOT.TINI) THEN
        CALL ERROR__MSG('GROUPLIST NOT INITIALIZED')
        CALL ERROR__STOP('GROUPLIST__MEMBERS')
      END IF
      IF(NATOM1.NE.NATOM) THEN
        CALL ERROR__MSG(' NUMBER OF ATOMS INCONSISTENT')
        CALL ERROR__STOP('GROUPLIST__MEMBERS')
      END IF
!
!     ==================================================================
!     ==  DETERMINE GROUP INDEX                                       ==
!     ==================================================================
      IGROUP=0
      DO I=1,NGROUP
        IF(TRIM(IDENT1).EQ.TRIM(IDENTIFIER(I))) THEN
          IGROUP=I
          EXIT
        END IF
      ENDDO
      IF(IGROUP.EQ.0) THEN
        CALL ERROR__MSG('NO GROUP NAMED BY IDENT IN THE LIST')
        CALL ERROR__CHVAL('IDENT',IDENT1)
        CALL ERROR__STOP('GROUPLIST')
      END IF
      DO IAT=1,NATOM
        TMEMBER1(IAT)=TMEMBER(IAT,IGROUP)
      ENDDO
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE GROUPLIST__LENGTH(NGROUP1)
!     ******************************************************************
!     **  LENGTH                                                      **
!     ******************************************************************
      USE GROUPLIST_MODULE
      IMPLICIT NONE
      INTEGER(4),INTENT(OUT) :: NGROUP1
!     ******************************************************************
      NGROUP1=NGROUP
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE GROUPLIST__MEMBERNUMBER(IDENT1,NMEMBER)
!     ******************************************************************
!     **  NUMBER OF MEMBERS                                           **
!     ******************************************************************
      USE GROUPLIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN)  :: IDENT1
      INTEGER(4)  ,INTENT(OUT) :: NMEMBER
      INTEGER(4)               :: IGROUP,IAT,I
!     ******************************************************************
      IF(.NOT.TINI) THEN
        CALL ERROR__MSG('GROUPLIST NOT INITIALIZED')
        CALL ERROR__STOP('GROUPLIST')
      END IF
!
!     ==================================================================
!     ==  DETERMINE GROUP INDEX                                       ==
!     ==================================================================
      IGROUP=0
      DO I=1,NGROUP
        IF(TRIM(IDENT1).EQ.TRIM(IDENTIFIER(I))) THEN
          IGROUP=I
          EXIT
        END IF
      ENDDO
      IF(IGROUP.EQ.0) THEN
        CALL ERROR__MSG('NO GROUP NAMED BY IDENT IN THE LIST')
        CALL ERROR__CHVAL('IDENT',IDENT1)
        CALL ERROR__STOP('GROUPLIST')
      END IF
!
!     ==================================================================
!     ==  COUNT MEMBERS IN THE GROUP                                  ==
!     ==================================================================
      NMEMBER=0
      DO IAT=1,NATOM
        IF(TMEMBER(IAT,IGROUP)) NMEMBER=NMEMBER+1
      ENDDO
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE GROUPLIST__REPORT(NFIL)
!     ******************************************************************
!     **  REPORT                                                      **
!     ******************************************************************
      USE GROUPLIST_MODULE
      IMPLICIT NONE
      INTEGER(4)  ,INTENT(IN) :: NFIL
      INTEGER(4)              :: IGROUP,IAT,NMEM,I
      CHARACTER(32)           :: NAMES(NATOM)
!     ******************************************************************
      WRITE(NFIL,FMT='("GROUPLIST REPORT"/"================")')
      DO IGROUP=1,NGROUP
        NMEM=0
        DO IAT=1,NATOM
          IF(TMEMBER(IAT,IGROUP)) THEN
            NMEM=NMEM+1
            CALL ATOMLIST__GETCH('NAME',IAT,NAMES(NMEM))
          END IF
        ENDDO
        WRITE(NFIL,FMT='("GROUP:",A16,"N(MEMBERS)",I5)') &
     &         IDENTIFIER(IGROUP),NMEM
        WRITE(NFIL,FMT='(10(A,"; "))')(TRIM(NAMES(I)),I=1,NMEM)
      ENDDO
      RETURN
      END
!
!.......................................................................
MODULE ATOMTYPELIST_MODULE
!***********************************************************************
!**                                                                   **
!**                                                                   **
!**  FUNCTIONS:                                                       **
!**    INITIALIZE(NTYPEX)                                             **
!**    ADD(NAME)                                                      **
!**    SELECT(NAME)                                                   **
!**    UNSELECT                                                       **
!**    SETR8/I4/CH(ID,VAL)                                            **
!**    GETR8/I4/CH(ID,VAL)                                            **
!**    REPORT(NFIL)                                                   **
!**                                                                   **
!***********************************************************************
TYPE XXX_TYPE
  CHARACTER(16):: NAME       ! ATOM TYPE NAME
  CHARACTER(16):: FILE       ! SETUP FILE
  REAL(8)      :: Z          ! ATOMIC NUMBER
  REAL(8)      :: VALENCE    ! #(VALENCE ELECTRONS)
  REAL(8)      :: RMASS      ! ATOMIC MASS
  REAL(8)      :: PSG2       ! PARAMETER REQUIRED FOR MASS RENORMALIZATION
  REAL(8)      :: PSG4       ! PARAMETER REQUIRED FOR MASS RENORMALIZATION
  INTEGER(4)   :: LRHOX      ! MAX ANGULAR MOMENTUM FOR ONECENTER DENSITY
  INTEGER(4),POINTER   :: NPRO(:)    ! MAX #(PROJECTORS PER ANGULAR MOMENTUM)
END TYPE XXX_TYPE
LOGICAL(4)            :: TINI=.FALSE.
INTEGER(4)            :: NTYPEX=0
INTEGER(4)            :: NTYPE=0
INTEGER(4)            :: THISTYPE=0 ! POINTER TO THE ACTUAL TYPE OR ZERO
TYPE(XXX_TYPE),ALLOCATABLE :: XXX(:)
END MODULE ATOMTYPELIST_MODULE
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__INITIALIZE(NTYPEX1)
!     ******************************************************************
!     **  INITIALIZE                                                  **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      INTEGER(4),INTENT(IN) :: NTYPEX1
      INTEGER(4)            :: ITYPE
!     ******************************************************************
      IF(TINI) THEN
        CALL ERROR__MSG('ATOMTYPELIST ALREADY INITIALIZED')
        CALL ERROR__STOP('ATOMTYPELIST__INITIALIZE')
      END IF
      TINI=.TRUE.
      NTYPEX=NTYPEX1
      ALLOCATE(XXX(NTYPEX))
      NTYPE=0
      THISTYPE=0
      DO ITYPE=1,NTYPEX
        XXX(ITYPE)%NAME=' '
        XXX(ITYPE)%Z=0.D0
        XXX(ITYPE)%PSg2=0.D0
        XXX(ITYPE)%PSg4=0.D0
        XXX(ITYPE)%VALENCE=0.D0
        XXX(ITYPE)%RMASS=0.D0
! 1  patch    NULLIFY(XXX(ITYPE)%NPRO)
allocate(xxx(itype)%npro(1))
      ENDDO
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__CLEAR
!     ******************************************************************
!     **  CLEAR                                                       **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
!     ******************************************************************
      CALL LIST_TESTINI(TINI,'ATOMTYPELIST')
      NTYPEX=0
      NTYPE=0
      THISTYPE=0
      TINI=.FALSE.
      DEALLOCATE(XXX)
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__LENGTH(LENG)
!     ******************************************************************
!     **  RETURN LENGTH OF THE LIST                                   **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      INTEGER(4),INTENT(OUT) :: LENG
!     ******************************************************************
      LENG=NTYPE
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__ADD(NAME)
!     ******************************************************************
!     **  ADD ATOM                                                    **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: NAME
      INTEGER(4)              :: ITYPE
!     ******************************************************************
      CALL LIST_TESTINI(TINI,'ATOMTYPELIST')
      DO ITYPE=1,NTYPE
        IF(NAME.EQ.TRIM(XXX(ITYPE)%NAME)) THEN
          CALL ERROR__MSG('ATOM TYPE NAME ALREADY USED')
          CALL ERROR__CHVAL('TYPE',NAME)
          CALL ERROR__STOP('ATOMTYPELIST__ADD')
        END IF
      ENDDO
      NTYPE=NTYPE+1
      IF(NTYPE.GT.NTYPEX) THEN
        CALL ERROR__MSG('NR. OF ATOM TYPES LARGER THAN MAXIMUM')
        CALL ERROR__OVERFLOW('NTYPE',NTYPE,NTYPEX)
        CALL ERROR__STOP('ATOMTYPELIST__ADD')
      ENDIF
      XXX(NTYPE)%NAME=NAME
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__SELECT(NAME)
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: NAME
      INTEGER(4)              :: ITYPE
!     ******************************************************************
      IF(.NOT.TINI) THEN
        CALL ERROR__MSG('ATOMTYPELIST NOT INITIALIZED')
        CALL ERROR__STOP('ATOMTYPELIST__SELECT')
      END IF
      THISTYPE=0
      DO ITYPE=1,NTYPE
        IF(TRIM(XXX(ITYPE)%NAME).EQ.TRIM(NAME)) THEN
          THISTYPE=ITYPE
          EXIT
        END IF
      ENDDO
      IF(THISTYPE.EQ.0) THEN
        CALL ERROR__MSG('UNKNOWN ATOM TYPE')
        CALL ERROR__STOP('ATOMTYPELIST__SELECT')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__SELECTI4(ITYPE)
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      INTEGER(4)  ,INTENT(IN) :: ITYPE
!     ******************************************************************
      IF(ITYPE.GT.NTYPE.OR.ITYPE.LE.0) THEN
        CALL ERROR__MSG('TYPE INDEX INDEX OUT OF RANGE')
        CALL ERROR__I4VAL('TYPE SELECTED',ITYPE)
        CALL ERROR__I4VAL('MAX #(TYPES)',NTYPE)
        CALL ERROR__STOP('ATOMTYPELIST__SELECTI4')
      END IF
      THISTYPE=ITYPE
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__UNSELECT
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
!     ******************************************************************
      THISTYPE=0
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__SETR8(ID,VAL)
!     ******************************************************************
!     **  SET ATOMIC NUMBER                                           **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      REAL(8)     ,INTENT(IN) :: VAL
!     ******************************************************************
      IF(.NOT.TINI) THEN
        CALL ERROR__MSG('ATOMTYPELIST NOT INITIALIZED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__SETR8')
      END IF
      IF(THISTYPE.EQ.0) THEN
        CALL ERROR__MSG('NO ATOM TYPE SELECTED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__SETR8')
      END IF
!
      IF(ID.EQ.'Z') THEN
        XXX(THISTYPE)%Z=VAL
      ELSE IF(ID.EQ.'ZV') THEN
        XXX(THISTYPE)%VALENCE=VAL
      ELSE IF(ID.EQ.'M') THEN
        XXX(THISTYPE)%RMASS=VAL
      ELSE IF(ID.EQ.'PS<G2>') THEN
        XXX(THISTYPE)%PSG2=VAL
      ELSE IF(ID.EQ.'PS<G4>') THEN
        XXX(THISTYPE)%PSG4=VAL
      ELSE
        CALL ERROR__MSG('UNKNOWN ID')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__SETR8')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__GETR8(ID,VAL)
!     ******************************************************************
!     **  SET ATOMIC NUMBER                                           **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN)  :: ID
      REAL(8)     ,INTENT(OUT) :: VAL
!     ******************************************************************
      IF(.NOT.TINI) THEN
        CALL ERROR__MSG('ATOMTYPELIST NOT INITIALIZED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__GETR8')
      END IF
      IF(THISTYPE.EQ.0) THEN
        CALL ERROR__MSG('NO ATOM TYPE SELECTED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__GETR8')
      END IF
!
      IF(ID.EQ.'Z') THEN
        VAL=XXX(THISTYPE)%Z
      ELSE IF(ID.EQ.'ZV') THEN
        VAL=XXX(THISTYPE)%VALENCE
      ELSE IF(ID.EQ.'M') THEN
        VAL=XXX(THISTYPE)%RMASS
      ELSE IF(ID.EQ.'PS<G2>') THEN
        VAL=XXX(THISTYPE)%PSG2
      ELSE IF(ID.EQ.'PS<G4>') THEN
        VAL=XXX(THISTYPE)%PSG4
      ELSE
        CALL ERROR__MSG('UNKNOWN ID')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__GETR8')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__SETI4(ID,VAL)
!     ******************************************************************
!     **  SET ATOMIC NUMBER                                           **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      INTEGER(4)  ,INTENT(IN) :: VAL
!     ******************************************************************
      IF(.NOT.TINI) THEN
        CALL ERROR__MSG('ATOMTYPELIST NOT INITIALIZED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__SETI4')
      END IF
      IF(THISTYPE.EQ.0) THEN
        CALL ERROR__MSG('NO ATOM TYPE SELECTED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__SETI4')
      END IF
!
      IF(ID.EQ.'LRHOX') THEN
        XXX(THISTYPE)%LRHOX=VAL
      ELSE
        CALL ERROR__MSG('UNKNOWN ID')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__SETI4')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__GETI4(ID,VAL)
!     ******************************************************************
!     **  SET ATOMIC NUMBER                                           **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN)  :: ID
      INTEGER(4)  ,INTENT(OUT) :: VAL
!     ******************************************************************
      IF(.NOT.TINI) THEN
        CALL ERROR__MSG('ATOMTYPELIST NOT INITIALIZED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__GETI4')
      END IF
      IF(THISTYPE.EQ.0) THEN
        CALL ERROR__MSG('NO ATOM TYPE SELECTED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__GETI4')
      END IF
!
      IF(ID.EQ.'LRHOX') THEN
        VAL=XXX(THISTYPE)%LRHOX
      ELSE IF(ID.EQ.'IZ') THEN
        VAL=NINT(XXX(THISTYPE)%Z)
      ELSE
        CALL ERROR__MSG('UNKNOWN ID')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__GETI4')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__SETI4A(ID,LEN,VAL)
!     ******************************************************************
!     **  SET ATOMIC NUMBER                                           **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      INTEGER(4)  ,INTENT(IN) :: LEN
      INTEGER(4)  ,INTENT(IN) :: VAL(LEN)
!     ******************************************************************
      IF(.NOT.TINI) THEN
        CALL ERROR__MSG('ATOMTYPELIST NOT INITIALIZED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__SETI4')
      END IF
      IF(THISTYPE.EQ.0) THEN
        CALL ERROR__MSG('NO ATOM TYPE SELECTED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__SETI4')
      END IF
!
      IF(ID.EQ.'NPRO') THEN
        IF(ASSOCIATED(XXX(THISTYPE)%NPRO)) then  ! 1  patch
          DEALLOCATE(XXX(THISTYPE)%NPRO)
        end if    ! 1  patch (see nullify in atomtypelist__initialize)
        ALLOCATE(XXX(THISTYPE)%NPRO(LEN))
        XXX(THISTYPE)%NPRO(:)=VAL(:)
      ELSE
        CALL ERROR__MSG('UNKNOWN ID')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__SETI4')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__GETI4A(ID,LEN,VAL)
!     ******************************************************************
!     **  SET ATOMIC NUMBER                                           **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN)  :: ID
      INTEGER(4)  ,INTENT(IN)  :: LEN
      INTEGER(4)  ,INTENT(OUT) :: VAL(LEN)
      INTEGER(4)               :: LEN1
!     ******************************************************************
      IF(.NOT.TINI) THEN
        CALL ERROR__MSG('ATOMTYPELIST NOT INITIALIZED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__GETI4')
      END IF
      IF(THISTYPE.EQ.0) THEN
        CALL ERROR__MSG('NO ATOM TYPE SELECTED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__GETI4')
      END IF
!
      IF(ID.EQ.'NPRO') THEN
        IF(.NOT.ASSOCIATED(XXX(THISTYPE)%NPRO)) THEN
          VAL(:)=10000
        ELSE
          LEN1=SIZE(XXX(THISTYPE)%NPRO)
          IF(LEN.GE.LEN1) THEN
            VAL(:)=0
            VAL(1:LEN1)=XXX(THISTYPE)%NPRO
          ELSE
            VAL(:)=XXX(THISTYPE)%NPRO(1:LEN)
          END IF
        END IF
      ELSE
        CALL ERROR__MSG('UNKNOWN ID')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__GETI4')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__SETCH(ID,VAL)
!     ******************************************************************
!     **  SET ATOMIC NUMBER                                           **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID
      CHARACTER(*),INTENT(IN) :: VAL
!     ******************************************************************
      IF(.NOT.TINI) THEN
        CALL ERROR__MSG('ATOMTYPELIST NOT INITIALIZED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__SETCH')
      END IF
      IF(THISTYPE.EQ.0) THEN
        CALL ERROR__MSG('NO ATOM TYPE SELECTED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__SETCH')
      END IF
!
      IF(ID.EQ.'FILE') THEN
        XXX(THISTYPE)%FILE=VAL
      ELSE
        CALL ERROR__MSG('UNKNOWN ID')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__SETCH')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__GETCH(ID,VAL)
!     ******************************************************************
!     **  SET ATOMIC NUMBER                                           **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN)  :: ID
      CHARACTER(*),INTENT(OUT) :: VAL
!     ******************************************************************
      IF(.NOT.TINI) THEN
        CALL ERROR__MSG('ATOMTYPELIST NOT INITIALIZED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__GETCH')
      END IF
      IF(THISTYPE.EQ.0) THEN
        CALL ERROR__MSG('NO ATOM TYPE SELECTED')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__GETCH')
      END IF
!
      IF(ID.EQ.'FILE') THEN
        VAL=XXX(THISTYPE)%FILE
      ELSE IF(ID.EQ.'NAME') THEN
        VAL=XXX(THISTYPE)%NAME
      ELSE
        CALL ERROR__MSG('UNKNOWN ID')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__STOP('ATOMTYPELIST__GETR8')
      END IF
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__SETZ(ID_,Z)
!     ******************************************************************
!     **  SET ATOMIC NUMBER                                           **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID_
      REAL(8)     ,INTENT(IN) :: Z
!     ******************************************************************
      CALL ATOMTYPELIST__SELECT(ID_)
      CALL ATOMTYPELIST__SETR8('Z',Z)
      CALL ATOMTYPELIST__UNSELECT
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__Z(IDENT1_,Z1)
!     ******************************************************************
!     **  RETURN ATOMIC NUMBER                                        **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: IDENT1_
      REAL(8)     ,INTENT(OUT):: Z1
!     ******************************************************************
      CALL ATOMTYPELIST__SELECT(IDENT1_)
      CALL ATOMTYPELIST__GETR8('Z',Z1)
      CALL ATOMTYPELIST__UNSELECT
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__SETVALENCE(IDENT1_,Z1)
!     ******************************************************************
!     **  SET ATOMIC NUMBER                                           **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: IDENT1_
      REAL(8)     ,INTENT(IN) :: Z1
!     ******************************************************************
      CALL ATOMTYPELIST__SELECT(IDENT1_)
      CALL ATOMTYPELIST__SETR8('ZV',Z1)
      CALL ATOMTYPELIST__UNSELECT
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__VALENCE(IDENT1_,Z1)
!     ******************************************************************
!     **  RETURN ATOMIC NUMBER                                        **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: IDENT1_
      REAL(8)     ,INTENT(OUT):: Z1
!     ******************************************************************
      CALL ATOMTYPELIST__SELECT(IDENT1_)
      CALL ATOMTYPELIST__GETR8('ZV',Z1)
      CALL ATOMTYPELIST__UNSELECT
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__SETMASS(IDENT1_,RMASS1)
!     ******************************************************************
!     **  SET MASS                                                    **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: IDENT1_
      REAL(8)     ,INTENT(IN) :: RMASS1
      CHARACTER(16)           :: IDENT
      INTEGER(4)              :: ITYPE
!     ******************************************************************
      CALL ATOMTYPELIST__SELECT(IDENT1_)
      CALL ATOMTYPELIST__SETR8('M',RMASS1)
      CALL ATOMTYPELIST__UNSELECT
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__MASS(IDENT1_,RMASS1)
!     ******************************************************************
!     **  RETURN MASS                                                 **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: IDENT1_
      REAL(8)     ,INTENT(OUT):: RMASS1
!     ******************************************************************
      CALL ATOMTYPELIST__SELECT(IDENT1_)
      CALL ATOMTYPELIST__GETR8('M',RMASS1)
      CALL ATOMTYPELIST__UNSELECT
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__SETFILE(IDENT1_,IDENT2)
!     ******************************************************************
!     **  SET ID FOR THE SETUP FILE                           **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: IDENT1_
      CHARACTER(*),INTENT(IN) :: IDENT2
!     ******************************************************************
      CALL ATOMTYPELIST__SELECT(IDENT1_)
      CALL ATOMTYPELIST__SETCH('FILE',IDENT2)
      CALL ATOMTYPELIST__UNSELECT
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__FILE(IDENT1_,IDENT2)
!     ******************************************************************
!     **  RETURN ID FOR THE SETUP FILE                        **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: IDENT1_
      CHARACTER(*),INTENT(OUT):: IDENT2
!     ******************************************************************
      CALL ATOMTYPELIST__SELECT(IDENT1_)
      CALL ATOMTYPELIST__GETCH('FILE',IDENT2)
      CALL ATOMTYPELIST__UNSELECT
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__INDEX(IDENT1_,ITYPE1)
!     ******************************************************************
!     **  REQUEST INDEX BY NAME                                       **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: IDENT1_
      INTEGER(4)  ,INTENT(OUT):: ITYPE1
!     ******************************************************************
      CALL ATOMTYPELIST__SELECT(IDENT1_)
      ITYPE1=THISTYPE
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__NAME(ITYPE1,IDENT1_)
!     ******************************************************************
!     **  REQUEST NAME BY INDEX                                       **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      INTEGER(4)  ,INTENT(IN) :: ITYPE1
      CHARACTER(*),INTENT(OUT):: IDENT1_
!     ******************************************************************
      IF(ITYPE1.GT.NTYPE) THEN
        CALL ERROR__MSG('ATOM TYPE NUMBER OUT OF RANGE')
        CALL ERROR__STOP('ATOMTYPELIST__NAME')
      END IF
      IDENT1_=XXX(ITYPE1)%NAME
      RETURN
      END
!
!     ..................................................................
      SUBROUTINE ATOMTYPELIST__REPORT(NFIL)
!     ******************************************************************
!     **  REPORT NAME BY INDEX                                        **
!     ******************************************************************
      USE ATOMTYPELIST_MODULE
      IMPLICIT NONE
      INTEGER(4)  ,INTENT(IN) :: NFIL
      REAL(8)                 :: U
      INTEGER(4)              :: ITYPE
!     ******************************************************************
      CALL CONSTANTS('U',U)
      WRITE(NFIL,FMT='("ATOMTYPELIST"/"============")')
      WRITE(NFIL,FMT='("NAME",T10,"Z ",T15,"N_VAL",T25,"M[U]" &
     &      ,T33,"PS<G2>",T43,"PS<G4>",T52,1X,"FILE_ID")')
      DO ITYPE=1,NTYPE
        WRITE(NFIL,FMT='(A10,T10,F4.1,T15,F4.1,T20,F9.5 &
     &      ,T30,F9.5,T40,F9.5,T52,1X,A)') &
     &       XXX(ITYPE)%NAME,XXX(ITYPE)%Z,XXX(ITYPE)%VALENCE &
     &      ,XXX(ITYPE)%RMASS/U,XXX(ITYPE)%PSG2,XXX(ITYPE)%PSG4 &
     &      ,TRIM(XXX(ITYPE)%FILE)
      ENDDO
      RETURN
      END
!
!     .................................................................
      SUBROUTINE LIST_LOOKUP(LENGTH,LIST,ITEM,INDEX)
!     **                                                             **
!     **  DETERMNIES THE POSITION OF AN ITEM IN A LIST               **
!     **  SUCH THAT                                                  **
!     **            ITEM=LIST(INDEX)                                 **
!     **                                                             **
      INTEGER(4)  ,INTENT(IN) :: LENGTH
      INTEGER(4)  ,INTENT(OUT):: INDEX
      CHARACTER(*),INTENT(IN) :: ITEM
      CHARACTER(*),INTENT(IN) :: LIST(LENGTH)
      INTEGER(4)              :: I
!     ******************************************************************
      INDEX=0
      DO I=1,LENGTH
        IF(TRIM(ITEM).EQ.TRIM(LIST(I))) THEN
          INDEX=I
          RETURN
        END IF
      ENDDO
      RETURN
      END
!
!     .................................................................
      SUBROUTINE LIST_TESTINI(TINI,NAME)
!     ******************************************************************
!     ******************************************************************
      LOGICAL(4)  ,INTENT(IN) :: TINI
      CHARACTER(*),INTENT(IN) :: NAME
!     ******************************************************************
      IF(TINI) RETURN
!
      CALL ERROR__MSG('OBJECT NAME IS NOT INITIALIZED')
      CALL ERROR__CHVAL('NAME',NAME)
      CALL ERROR__STOP('LIST_TESTINI')
      END

