!
!***********************************************************************
!**                                                                   **
!**  NAME: LLIST                                                      **
!**                                                                   **
!**  PURPOSE: PROVIDES BASIC ROUTINES TO MAINTAIN A                   **
!**    LINKEDLIST/TREE DATA STRUCTURE AND TO STORE,RETRIEVE           **
!**    AND REMOVE CHARACTER(1) ARRAYS                                 **
!**                                                                   **
!**  FUNCTIONS:                                                       **
!**    LLIST_NEW(LIST)                                                **
!**    INQUIRE/SELECT:                                                **
!**    LLIST_NDATA(LIST,ID,NUM)                                       **
!**    LLIST_NLISTS(LIST,ID,NUM)                                      **
!**    LLIST_FINDDATA(LIST,ID,ITH,DATA)                               **
!**    LLIST_FINDLIST(LIST,ID,ITH,NEWLIST)                            **
!**    MODIFY LIST:                                                   **
!**    LLIST_APPENDLIST(LIST,ID,NEWLIST)                              **
!**    LLIST_APPENDDATA(LIST,ID,DATA)       PRIVATE                   **
!**    LLIST_REMOVELIST(LIST,ID,ITH)                                  **
!**    LLIST_REMOVEDATA(LIST,ID,ITH)                                  **
!**    LLIST_DIEDATA(DATA)                   PRIVATE                  **
!**    LLIST_DIELISTS(LIST)                  PRIVATE                  **
!**    MODIFY DATA:                                                   **
!**    LLIST_GETPTR(LIST,ID,ITH,TYPE,NBYTE,VAL)                       **
!**    LLIST_SETPTR(LIST,ID,ITH,TYPE,NBYTE,VAL)                       **
!**    LLIST_ADDPTR(LIST,ID,TYPE,NBYTE_,VAL_)                         **
!**                                                                   **
!**  REMARKS:                                                         **
!**    1) EACH LIST KNOWS HIS PARENT, HIS ELDEST CHILD                **
!**           AND HIS ELDER AND YOUNGER BROTHER                       **
!**       EXCEPT.                                                     **
!**      A) THE ELDEST BROTHER HAS NO ELDER BROTHER                   **
!**      B) THE YOUNGEST BROTHER HAS NO YOUNGER BROTHER               **
!**      C) THE HOME LIST HAS NEITHER PARENT BROTHERS                 **
!**    2) EACH DATA HAS AN ELDER AND A YOUNGER BROTHER                **
!**      A) THE ELDEST BROTHER HAS NO ELDER BROTHER                   **
!**      B) THE YOUNGEST BROTHER HAS NO YOUNGER BROTHER               **
!**    3) SPECIAL SYMBOLS:                                            **
!**       A) '..' STANDS FOR PARENT                                   **
!**       B) '~'  STANDS FOR HOME                                     **
!**       C) '*'  STANDS FOR ANY ID                                   **
!**    4) SPECIAL NUMBERING                                           **
!**     ? A) ITH<0 MEANS THE LARGEST POSSIBLE NUMBER (EG: APPEND)     **
!**    5) THE OBJECT ORIENTS ITSELF ACCORDING TO LISTS NOT DATA       **
!**                                                                   **
!***********************************************************************
MODULE LLIST_MODULE
TYPE LLIST_TYPE
  CHARACTER(32)             :: ID     ! KEY WORD
  TYPE (LLIST_TYPE),POINTER :: UP     ! PARENT
  TYPE (LLIST_TYPE),POINTER :: PREV   ! ELDER BROTHER
  TYPE (LLIST_TYPE),POINTER :: NEXT   ! YOUNGER BROTHER
  TYPE (LLIST_TYPE),POINTER :: LISTS  ! ELDEST CHILD
  TYPE (LDATA_TYPE),POINTER :: DATA   ! FIRST DATA
END TYPE LLIST_TYPE
TYPE LDATA_TYPE
  CHARACTER(32)             :: ID     ! KEY WORD
  TYPE (LDATA_TYPE),POINTER :: PREV   ! PREV DATA
  TYPE (LDATA_TYPE),POINTER :: NEXT   ! NEXT DATA
  CHARACTER(8)              :: TYPE   ! DATA TYPE
  INTEGER(4)                :: SIZE   ! SIZE
  CHARACTER(1)     ,POINTER :: VAL(:) ! DATA
END TYPE LDATA_TYPE
TYPE TYPE_TYPE
 CHARACTER(8)   :: NAME
 INTEGER(4)     :: NBYTE
END TYPE TYPE_TYPE
CHARACTER(32),PARAMETER :: CH32HOME='~'
CHARACTER(32),PARAMETER :: CH32BACK='..'
CHARACTER(32),PARAMETER :: CH32ANY='*'
TYPE(TYPE_TYPE) ,PARAMETER :: C8TYPE=TYPE_TYPE('C(8)',16)
TYPE(TYPE_TYPE) ,PARAMETER :: C4TYPE=TYPE_TYPE('C(4)',8)
TYPE(TYPE_TYPE) ,PARAMETER :: R8TYPE=TYPE_TYPE('R(8)',8)
TYPE(TYPE_TYPE) ,PARAMETER :: R4TYPE=TYPE_TYPE('R(4)',4)
TYPE(TYPE_TYPE) ,PARAMETER :: I4TYPE=TYPE_TYPE('I(4)',4)
TYPE(TYPE_TYPE) ,PARAMETER :: L4TYPE=TYPE_TYPE('L(4)',4)
TYPE(TYPE_TYPE) ,PARAMETER :: CHTYPE=TYPE_TYPE('CH',1)
CONTAINS
!
!     ................................................................
      SUBROUTINE LLIST_NEW(LIST)
!     ******************************************************************
!     **                                                              **
!     **  CREATES A HOME DIRECTORY, IF LIST IS UNDEFINED              **
!     **  ON RETURN NEWLIST MAY OR MAY NOT BE ASSOCIATED              **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE)  ,POINTER    :: LIST
!     ******************************************************************
      ALLOCATE(LIST)
      LIST%ID=CH32HOME
      NULLIFY(LIST%UP)
      NULLIFY(LIST%PREV)
      NULLIFY(LIST%NEXT)
      NULLIFY(LIST%LISTS)
      NULLIFY(LIST%DATA)
      RETURN
      END SUBROUTINE LLIST_NEW
!
!     ................................................................
      SUBROUTINE LLIST_FINDDATA(LIST,ID,NTH,DATA)
!     ******************************************************************
!     **                                                              **
!     **  NAME: LLIST_FINDDATA                                   **
!     **                                                              **
!     **  FINDS A LDATA_TYPE IN THE CURRENT LIST.                      **
!     **  IF NO DATA WITH THE CORRECT SPECIFICATIONS IS FOUND         **
!     **  THE POINTER DATA IS NULLIFIED                               **
!     **                                                              **
!     **  WILDCARDS:                                                  **
!     **    ID='*'  APPLIES TO ANY ID                                 **
!     **    ITH<0   FINDS THE LAST DATA CONSISTENT WITH ID            **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    1) DATA IS ALWAYS NULLIEFIED IF NTH=0                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE),INTENT(IN) :: LIST
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      TYPE(LDATA_TYPE),POINTER    :: DATA    ! OUTPUT
      TYPE(LDATA_TYPE),POINTER    :: MARK
      INTEGER(4)                  :: ITH
      CHARACTER(32)               :: ID1
!     ****************************************************************
      ID1=ID
      NULLIFY(MARK)
      ITH=0
      DATA=>LIST%DATA
      DO WHILE(ASSOCIATED(DATA))
        IF(ID1.EQ.DATA%ID.OR.ID1.EQ.CH32ANY) THEN
          ITH=ITH+1
          IF(ITH.EQ.NTH) RETURN
          IF(ITH.LT.0) MARK=>DATA
        END IF
        DATA=>DATA%NEXT
      ENDDO
      IF(ITH.LT.0) THEN
        DATA=>MARK
        RETURN
      END IF
      NULLIFY(DATA)
      END SUBROUTINE LLIST_FINDDATA
!
!     ..................................................................
      SUBROUTINE LLIST_FINDLIST(LIST,ID,NTH,NEWLIST)
!     ******************************************************************
!     **                                                              **
!     **  NAME: LLIST_FINDLIST                                   **
!     **                                                              **
!     **  FINDS A LLIST_TYPE IN THE CURRENT LIST.                      **
!     **  IF NO LIST WITH THE CORRECT SPECIFICATIONS IS FOUND         **
!     **  THE POINTER NEWLIST IS NULLIFIED                            **
!     **                                                              **
!     **  WILDCARDS:                                                  **
!     **    ID='*'  APPLIES TO ANY ID                                 **
!     **    ITH<0   FINDS THE LAST LIST CONSISTENT WITH ID            **
!     **    ID='~'  FINDS THE HOMELIST IRRESPECTIVE OF NTH            **
!     **    ID='..' NTH=1 : SELECTS THE PARENT LIST                   **
!     **    ID='..' NTH>1 : SELECTS THE NTH ANCESTOR                  **
!     **    ID='..' NTH<0 : IS IDENTICAL TO ID='~'                    **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    1) NEWLIST IS ALWAYS NULLIEFIED IF NTH=0                  **
!     **                                                              **
!     **                                                            **
!     ****************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE),TARGET,INTENT(IN) :: LIST
      CHARACTER(*)    ,INTENT(IN)        :: ID
      INTEGER(4)      ,INTENT(IN)        :: NTH
      TYPE(LLIST_TYPE),POINTER           :: NEWLIST
      TYPE(LLIST_TYPE),POINTER           :: MARK
      INTEGER(4)                         :: ITH
      CHARACTER(32)                      :: ID1
!     ****************************************************************
      ID1=ID
      IF(NTH.LT.0.AND.ID1.EQ.CH32BACK)ID1=CH32HOME
      IF(NTH.EQ.0) THEN
        NULLIFY(NEWLIST)
        RETURN
      END IF
!
!     ================================================================
!     == SELECT HOME                                                ==
!     ================================================================
      IF(ID1.EQ.CH32HOME) THEN
        NEWLIST=>LIST
        DO WHILE(ASSOCIATED(NEWLIST%UP))
          NEWLIST=>NEWLIST%UP
        ENDDO
!
!     ================================================================
!     == SELECT PARENTS OR EARLIER ANCESTORS                        ==
!     == THE CASE (ITH<0,ID='..') IS ALREADY TAKEN CARE OF          ==
!     ================================================================
      ELSE IF(ID1.EQ.CH32BACK) THEN
        NEWLIST=>LIST
        DO ITH=1,NTH
          NEWLIST=>NEWLIST%UP
          IF(.NOT.ASSOCIATED(NEWLIST)) RETURN
        ENDDO
!
!     ================================================================
!     == SEARCH FOR ANOTHER KEYWORD                                 ==
!     ================================================================
      ELSE
        NEWLIST=>LIST%LISTS
        ITH=0
        NULLIFY(MARK)
        DO WHILE(ASSOCIATED(NEWLIST))
          IF(ID1.EQ.NEWLIST%ID.OR.ID1.EQ.CH32ANY) THEN
            ITH=ITH+1
            IF(ITH.EQ.NTH) RETURN
            IF(ITH.LT.0) MARK=>NEWLIST
          END IF
          NEWLIST=>NEWLIST%NEXT
        ENDDO
        IF(ITH.LT.0) THEN
          NEWLIST=>MARK
          RETURN
        END IF
        NULLIFY(NEWLIST)
      END IF
      RETURN
      END SUBROUTINE LLIST_FINDLIST
!
!     ..................................................................
      SUBROUTINE LLIST_APPENDLIST(LIST,ID,NEWLIST)
!     ******************************************************************
!     **                                                              **
!     **  APPENDS A NEW LIST (NEWLIST) TO THE CURRENT LIST            **
!     **  (NEWLIST HAS THE NAME AND IS PROPERLY CONNECTED             **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE),POINTER    :: LIST
      CHARACTER(*)    ,INTENT(IN) :: ID
      TYPE(LLIST_TYPE),POINTER    :: NEWLIST,PREV
!     ******************************************************************
      ALLOCATE(NEWLIST)
      NEWLIST%ID=ID
      NEWLIST%UP=>LIST
      NULLIFY(NEWLIST%PREV)
      NULLIFY(NEWLIST%NEXT)
      NULLIFY(NEWLIST%DATA)
      NULLIFY(NEWLIST%LISTS)
      IF(.NOT.ASSOCIATED(LIST%LISTS)) THEN
        LIST%LISTS=>NEWLIST
      ELSE
        PREV=>LIST%LISTS
        DO WHILE(ASSOCIATED(PREV%NEXT))
          PREV=>PREV%NEXT
        ENDDO
        PREV%NEXT=>NEWLIST
        NEWLIST%PREV=>PREV
      END IF
      RETURN
      END SUBROUTINE LLIST_APPENDLIST
!
!     ..................................................................
      SUBROUTINE LLIST_APPENDDATA(LIST,ID,DATA)
!     ******************************************************************
!     **                                                              **
!     **  APPENDS A NEW DATA (DATA) TO THE CURRENT LIST               **
!     **  (DATA HAS THE NAME AND IS PROPERLY CONNECTED)               **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE),INTENT(INout) :: LIST
      CHARACTER(*)    ,INTENT(IN)    :: ID
      TYPE(LDATA_TYPE),POINTER       :: DATA
!     ******************************************************************
      IF(.NOT.ASSOCIATED(LIST%DATA)) THEN
        ALLOCATE(LIST%DATA)
        DATA=>LIST%DATA
        NULLIFY(DATA%PREV)
      ELSE
        DATA=>LIST%DATA
        DO WHILE(ASSOCIATED(DATA%NEXT))
          DATA=>DATA%NEXT
        ENDDO
        ALLOCATE(DATA%NEXT)
        DATA%NEXT%PREV=>DATA
        DATA=>DATA%NEXT
      END IF
      NULLIFY(DATA%NEXT)     ! AN APPENDED ITEM HAS NO YOUNGER BROTHER YET
      DATA%ID=ID
      DATA%TYPE=' '
      DATA%SIZE=0
      NULLIFY(DATA%VAL)
      RETURN
      END SUBROUTINE LLIST_APPENDDATA
!
!     ................................................................
      SUBROUTINE LLIST_NDATA(LIST,ID,NUM)
!     ******************************************************************
!     **                                                              **
!     **  COUNTS THE NUMBER OF LISTS IN THE CURRENT LIST              **
!     **  CONSISTENT WITH ID                                          **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE),INTENT(IN) :: LIST
      CHARACTER(*)   ,INTENT(IN) :: ID
      INTEGER(4)     ,INTENT(OUT):: NUM
      TYPE(LDATA_TYPE),POINTER    :: DATA
      CHARACTER(32)              :: ID1
!     ******************************************************************
      ID1=ID
      DATA=>LIST%DATA
      NUM=0
      DO WHILE(ASSOCIATED(DATA))
        IF(ID1.EQ.DATA%ID.OR.ID1.EQ.CH32ANY)NUM=NUM+1
        DATA=>DATA%NEXT
      ENDDO
      RETURN
      END SUBROUTINE LLIST_NDATA
!
!     ..................................................................
      SUBROUTINE LLIST_NLISTS(LIST,ID,NUM)
!     ******************************************************************
!     **                                                              **
!     **  COUNTS THE NUMBER OF DATA  IN THE CURRENT LIST              **
!     **  CONSISTENT WITH ID                                          **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE),INTENT(IN) :: LIST
      CHARACTER(*)   ,INTENT(IN) :: ID
      INTEGER(4)     ,INTENT(OUT):: NUM
      TYPE(LLIST_TYPE),POINTER    :: LISTI
      CHARACTER(32)              :: ID1
!     ******************************************************************
      ID1=ID
      LISTI=>LIST%LISTS
      NUM=0
      DO WHILE(ASSOCIATED(LISTI))
        IF(ID1.EQ.LISTI%ID.OR.ID1.EQ.CH32ANY)NUM=NUM+1
        LISTI=>LISTI%NEXT
      ENDDO
      RETURN
      END SUBROUTINE LLIST_NLISTS
!
!     ..................................................................
      SUBROUTINE LLIST_REMOVEDATA(LIST,ID,NTH)
!     ******************************************************************
!     **                                                              **
!     **  REMOVE A GIVEN DATA FROM THE CURRENT LIST                   **
!     **  NO ACTION, IF NO DATA WITH THE CORRECT SPECIFICATIONS EXISTS**
!     **                                                              **
!     **  WILD CARDS:                                                 **
!     **    ID='*'  REMOVES THE NTH DATA IRRESPECTIVE OF ID           **
!     **    ITH<0   REMOVES THE LAST DATA CONSISTENT WITH ID          **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE),POINTER    :: LIST
      CHARACTER(*)   ,INTENT(IN) :: ID
      INTEGER(4)     ,INTENT(IN) :: NTH
      TYPE(LDATA_TYPE),POINTER    :: DATA,NEXT,PREV
!     ******************************************************************
      CALL LLIST_FINDDATA(LIST,ID,NTH,DATA)
      IF(ASSOCIATED(DATA)) THEN
        DEALLOCATE(DATA%VAL)
        PREV=>DATA%PREV
        NEXT=>DATA%NEXT
        IF(ASSOCIATED(PREV))THEN
          PREV%NEXT=>NEXT
        ELSE
          LIST%DATA=>NEXT
        END IF
        IF(ASSOCIATED(NEXT))NEXT%PREV=>PREV
        !PRINT*,'KILLING DATA ',DATA%ID
        DEALLOCATE(DATA)
      END IF
      RETURN
      END SUBROUTINE LLIST_REMOVEDATA
!
!     ..................................................................
      SUBROUTINE LLIST_REMOVELIST(LIST,ID,NTH)
!     ******************************************************************
!     **                                                              **
!     **  REMOVE A GIVEN LIST FROM THE CURRENT LIST                   **
!     **  NO ACTION, IF NO DATA WITH THE CORRECT SPECIFICATIONS EXISTS**
!     **  NO ACTION IF ID='~' OR ID='..'                              **
!     **                                                              **
!     **  WILD CARDS:                                                 **
!     **    ID='*'  REMOVES THE NTH DATA IRRESPECTIVE OF ID           **
!     **    ITH<0   REMOVES THE LAST DATA CONSISTENT WITH ID          **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE),POINTER    :: LIST
      CHARACTER(*)   ,INTENT(IN) :: ID
      INTEGER(4)     ,INTENT(IN) :: NTH
      TYPE(LLIST_TYPE),POINTER    :: DIELIST,PREV,NEXT
!     ******************************************************************
      IF(TRIM(ID).EQ.TRIM(CH32HOME)) THEN
        RETURN
      ELSE IF(TRIM(ID).EQ.TRIM(CH32BACK)) THEN
        RETURN
      END IF
      CALL LLIST_FINDLIST(LIST,ID,NTH,DIELIST)
      IF(.NOT.ASSOCIATED(DIELIST)) RETURN
!
!     ================================================================
!     == RECONNECT POINTERS TO DIELIST                              ==
!     ================================================================
      PREV=>DIELIST%PREV
      NEXT=>DIELIST%NEXT
      IF(ASSOCIATED(PREV)) THEN
        PREV%NEXT=>NEXT
      ELSE
        LIST%LISTS=>NEXT
      END IF
      IF(ASSOCIATED(NEXT))NEXT%PREV=>PREV
!
!     ================================================================
!     == REMOVE DATA AND SUBLISTS                                   ==
!     ================================================================
      CALL LLIST_DIEDATA(DIELIST%DATA)
      CALL LLIST_DIELIST(DIELIST%LISTS)
      !PRINT*,'KILLING LIST ',DIELIST%ID
      DEALLOCATE(DIELIST)
      RETURN
      END SUBROUTINE LLIST_REMOVELIST
!
!     ..................................................................
      RECURSIVE SUBROUTINE LLIST_DIELIST(LISTS)
!     ******************************************************************
!     **                                                              **
!     **  REMOVE LISTS AND ALL DATA AND LISTS CONNECTED TO IT         **
!     **  APPLY ONLY TO LLIST_TYPE%LISTS, BECAUSE NO CONNECTIONS       **
!     **  ARE RECOVERD EXCEPT THAT LISTS IS NULLIFIED.                **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE),POINTER    :: LISTS
      TYPE(LLIST_TYPE),POINTER    :: DIELIST
!     ******************************************************************
      DO WHILE (ASSOCIATED(LISTS))
        CALL LLIST_DIEDATA(LISTS%DATA)
        CALL LLIST_DIELIST(LISTS%LISTS)
        DIELIST=>LISTS
        LISTS=>LISTS%NEXT
        !PRINT*,'KILLING LIST ',DIELIST%ID
        DEALLOCATE(DIELIST)
      ENDDO
      NULLIFY(LISTS)
      RETURN
      END SUBROUTINE LLIST_DIELIST
!
!     ..................................................................
      SUBROUTINE LLIST_DIEDATA(DATA)
!     ******************************************************************
!     **                                                              **
!     **  REMOVE ALL DATA EQUAL AND YOUNGER TO DATA                   **
!     **  APPLY ONLY TO LLIST_TYPE%DATA,  BECAUSE NO CONNECTIONS       **
!     **  ARE RECOVERD EXCEPT THAT LISTS IS NULLIFIED.                **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LDATA_TYPE),POINTER    :: DATA
      TYPE(LDATA_TYPE),POINTER    :: OLD
!     ******************************************************************
      DO WHILE (ASSOCIATED(DATA))
        DEALLOCATE(DATA%VAL)
        OLD=>DATA
        DATA=>DATA%NEXT
        !PRINT*,'KILLING DATA ',OLD%ID
        DEALLOCATE(OLD)
      ENDDO
      NULLIFY(DATA)
      RETURN
      END SUBROUTINE LLIST_DIEDATA
!
!     ..................................................................
      SUBROUTINE LLIST_REPORTDATA(DATA,NFIL,LEVEL)
!     ******************************************************************
!     **                                                              **
!     **  MAKES A REPORT OF ALL DATA EQUAL OR YOUNGER THAN DATA       **
!     **  AND WRITES THE REPORT TO FILE NFIL                          **
!     **  APPLY ONLY TO LLIST_TYPE%DATA,  BECAUSE NO CONNECTIONS       **
!     **  ARE RECOVERD EXCEPT THAT LISTS IS NULLIFIED.                **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LDATA_TYPE),POINTER   :: DATA
      INTEGER(4)      ,INTENT(IN):: NFIL
      INTEGER(4)      ,INTENT(IN):: LEVEL
      TYPE(LDATA_TYPE),POINTER   :: IDATA
      CHARACTER(256)             :: EMPTY=' '
      CHARACTER(32)              :: SIZESTRING
!     ******************************************************************
      IF(.NOT.ASSOCIATED(DATA)) RETURN
      EMPTY=' '
      IDATA=>DATA
      DO
        WRITE(SIZESTRING,fmt='(i32)')IDATA%SIZE
        SIZESTRING=TRIM(ADJUSTL(SIZESTRING))//'*'//TRIM(IDATA%TYPE)
        WRITE(NFIL,FMT='(A,A,"[",A,"]")') &
     &          EMPTY(1:LEVEL),TRIM(IDATA%ID),TRIM(SIZESTRING)
        IF(.NOT.ASSOCIATED(IDATA%NEXT)) EXIT
        IDATA=>IDATA%NEXT
      ENDDO
      NULLIFY(IDATA)
      RETURN
      END SUBROUTINE LLIST_REPORTDATA
!
!     ..................................................................
      RECURSIVE SUBROUTINE LLIST_REPORTLISTS(LIST,NFIL,LEVEL)
!     ******************************************************************
!     **                                                              **
!     **  MAKES A REPORT OF ALL DATA AND LISTS INCLUDING ALL          **
!     **  CHILDREN AND THEIR DATA ETC. ATTACHED TO THE CURRENT LIST   **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE),INTENT(IN)   :: LIST
      INTEGER(4)      ,INTENT(IN)   :: NFIL
      INTEGER(4)      ,INTENT(INOUT):: LEVEL
      TYPE(LLIST_TYPE)              :: ILIST
      CHARACTER(256)                :: EMPTY=' '
!     ******************************************************************
!     IF(.NOT.ASSOCIATED(LIST)) RETURN
      ILIST=LIST
      DO
        WRITE(NFIL,FMT='(A,A,"[LIST]")')EMPTY(1:LEVEL),TRIM(ILIST%ID)
        LEVEL=LEVEL+2
        IF(ASSOCIATED(ILIST%DATA)) THEN
          CALL LLIST_REPORTDATA(ILIST%DATA,NFIL,LEVEL)
        END IF
        IF(ASSOCIATED(ILIST%LISTS)) THEN
          CALL LLIST_REPORTLISTS(ILIST%LISTS,NFIL,LEVEL)
        END IF
        LEVEL=LEVEL-2
        IF(.NOT.ASSOCIATED(ILIST%NEXT)) EXIT
        ILIST=ILIST%NEXT
      ENDDO
      RETURN
      END SUBROUTINE LLIST_REPORTLISTS
!
!     ..................................................................
      SUBROUTINE LLIST_GETPTR(LIST,ID,NTH,TYPE,NBYTE,VAL)
!     ******************************************************************
!     **                                                              **
!     **  RETURNS A DATA                                              **
!     **  IF NO LDATA_TYPE WITH THE GIVEN SPECIFICATIONS EXIST         **
!     **  THE POINTER VAL IS NULLIFIED, NBYTE=0 AND TYPE=' '          **
!     **                                                              **
!     **  WILDCARDS: SEE LLIST_FINDLIST                          **
!     **                                                              **
!     **  ERROR CONDITION: NONE                                       **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE) ,POINTER    :: LIST
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)      ,INTENT(OUT):: NBYTE
      CHARACTER(*)    ,INTENT(OUT):: TYPE
      CHARACTER(1)    ,POINTER    :: VAL(:)
      TYPE(LDATA_TYPE),POINTER    :: DATA
!     ******************************************************************
      CALL LLIST_FINDDATA(LIST,ID,NTH,DATA)
      IF(.NOT.ASSOCIATED(DATA)) THEN
        NULLIFY(DATA)
        NBYTE=0
        TYPE=' '
        RETURN
      END IF
      NBYTE=DATA%SIZE
      TYPE =DATA%TYPE
      VAL  =>DATA%VAL
      RETURN
      END SUBROUTINE LLIST_GETPTR
!
!     ..................................................................
      SUBROUTINE LLIST_SETPTR(LIST,ID,NTH,TYPE,NBYTE,VAL,IERR)
!     ******************************************************************
!     **                                                              **
!     **  CHANGES THE VALUE OF OF A DATA ITEM                         **
!     **                                                              **
!     **  WILDCARDS: SEE LLIST_FINDLIST                               **
!     **                                                              **
!     **  ERROR CONDITION:                                            **
!     **    1) DATA ENTRY NOT FOUND                                   **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    ON OUTPUT THE POINTER VAL IS NULLIFIED                    **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE),POINTER    :: LIST
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      CHARACTER(*)    ,INTENT(IN) :: TYPE
      INTEGER(4)      ,INTENT(IN) :: NBYTE
      INTEGER(4)      ,INTENT(OUT):: IERR
      CHARACTER(1)    ,POINTER    :: VAL(:)
      TYPE(LDATA_TYPE),POINTER    :: DATA
!     ******************************************************************
      CALL LLIST_FINDDATA(LIST,ID,NTH,DATA)
      IF(.NOT.ASSOCIATED(DATA)) THEN
        IERR=1
        RETURN
      END IF
!     ==================================================================
!     == IF THE SIZE IS CONSISTENT, KEEP THE OLD TARGET AND DROP THE  ==
!     == NEW ONE TO AVOID FRAGMENTATION OF THE MEMORY                 ==
!     ==================================================================
      IF(SIZE(DATA%VAL).EQ.SIZE(VAL)) THEN
        DATA%VAL=VAL
        DEALLOCATE(VAL)
      ELSE
        DEALLOCATE(DATA%VAL)
        DATA%VAL=>VAL
        NULLIFY(VAL)
      END IF
      DATA%SIZE=NBYTE
      DATA%TYPE=TYPE
      IERR=0
      RETURN
      END SUBROUTINE LLIST_SETPTR
!
!     ..................................................................
      SUBROUTINE LLIST_ADDPTR(LIST,ID,TYPE,NBYTE,VAL)
!     ******************************************************************
!     **                                                              **
!     **  ATTACHES A NEW DATA ITEM TO THE CURRENT LIST                **
!     **                                                              **
!     **  ERROR CONDITION: NONE                                       **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE),POINTER    :: LIST
      CHARACTER(*)   ,INTENT(IN) :: ID
      CHARACTER(*)   ,INTENT(IN) :: TYPE
      INTEGER(4)     ,INTENT(IN) :: NBYTE
      CHARACTER(1)   ,POINTER    :: VAL(:)
      TYPE(LDATA_TYPE),POINTER    :: DATA
!     ******************************************************************
      CALL LLIST_APPENDDATA(LIST,ID,DATA)
      DATA%ID=ID
      DATA%SIZE=NBYTE
      DATA%TYPE=TYPE
      DATA%VAL=>VAL
      RETURN
      END SUBROUTINE LLIST_ADDPTR
END MODULE LLIST_MODULE
!!__!
!!__!...............................................................
!!__!module linkedlist_transferch_module
!!__!public LINKEDLIST_TRANSFERCHTO1
!!__!public LINKEDLIST_TRANSFERCHfrom1
!!__!INTERFACE LINKEDLIST_TRANSFERCHTO1
!!__!  MODULE PROCEDURE LINKEDLIST_TRANSFERCHSCALTO1
!!__!  MODULE PROCEDURE LINKEDLIST_TRANSFERCHARRTO1
!!__!END INTERFACE
!!__!contains
!!__!
!!__!     ..................................................................
!!__      SUBROUTINE linkedlist_TRANSFERCHscalTO1(FROMSIZE,FROM,NTO,TO)
!!__      INTEGER(4)  ,INTENT(IN)   :: FROMSIZE ! not used
!!__      CHARACTER(*),INTENT(IN)   :: FROM
!!__      INTEGER(4)  ,INTENT(IN)   :: NTO
!!__      CHARACTER(1),INTENT(OUT)  :: TO(NTO)
!!__      INTEGER(4)                :: I,J,IJ
!!__      INTEGER(4)                :: FROMLEN
!!__!     ******************************************************************
!!__      if(fromsize.ne.1) then
!!__        call error__stop('linkedlist_TRANSFERCHscalTO1')
!!__      end if
!!__      FROMLEN=LEN(FROM)
!!__      IJ=0
!!__      DO J=1,FROMLEN
!!__        IJ=IJ+1
!!__        TO(IJ)=FROM(J:J)
!!__      ENDDO
!!__      DO I=IJ+1,NTO
!!__        TO(I)=' '
!!__      ENDDO
!!__      RETURN
!!__      END SUBROUTINE linkedlist_TRANSFERCHscalTO1
!!__!
!!__!     ..................................................................
!!__      SUBROUTINE linkedlist_TRANSFERCHarrTO1(FROMSIZE,FROM,NTO,TO)
!!__      INTEGER(4)  ,INTENT(IN)   :: NTO
!!__      INTEGER(4)  ,INTENT(IN)   :: FROMSIZE
!!__      CHARACTER(*),INTENT(IN)   :: FROM(FROMSIZE)
!!__      CHARACTER(1),INTENT(OUT)  :: TO(NTO)
!!__      INTEGER(4)                :: I,J,IJ
!!__      INTEGER(4)                :: FROMLEN
!!__!     ******************************************************************
!!__      FROMLEN=LEN(FROM)
!!__      IJ=0
!!__      DO I=1,FROMSIZE
!!__        DO J=1,FROMLEN
!!__          IJ=IJ+1
!!__          TO(IJ)=FROM(I)(J:J)
!!__        ENDDO
!!__      ENDDO
!!__      DO I=IJ+1,NTO
!!__        TO(I)=' '
!!__      ENDDO
!!__      RETURN
!!__      END SUBROUTINE linkedlist_TRANSFERCHarrTO1
!!__!end module linkedlist_transferch_module
!
!     ..................................................................
      SUBROUTINE linkedlist_TRANSFERCHTO1(FROMSIZE,FROM,NTO,TO)
      INTEGER(4)  ,INTENT(IN)   :: NTO
      INTEGER(4)  ,INTENT(IN)   :: FROMSIZE
      CHARACTER(*),INTENT(IN)   :: FROM(FROMSIZE)
      CHARACTER(1),INTENT(OUT)  :: TO(NTO)
      INTEGER(4)                :: I,J,IJ
      INTEGER(4)                :: FROMLEN
!     ******************************************************************
      FROMLEN=LEN(FROM)
      IJ=0
      DO I=1,FROMSIZE
        DO J=1,FROMLEN
          IJ=IJ+1
          TO(IJ)=FROM(I)(J:J)
        ENDDO
      ENDDO
      DO I=IJ+1,NTO
        TO(I)=' '
      ENDDO
      RETURN
      END SUBROUTINE linkedlist_TRANSFERCHTO1
!
!     ..................................................................
      SUBROUTINE linkedlist_TRANSFERCHfrom1(nfrom,from,tosize,to)
      INTEGER(4)  ,INTENT(IN)   :: Nfrom
      INTEGER(4)  ,INTENT(IN)   :: toSIZE
      CHARACTER(*),INTENT(out)  :: to(toSIZE)
      CHARACTER(1),INTENT(in)   :: from(Nfrom)
      INTEGER(4)                :: I,J,IJ
      INTEGER(4)                :: toLEN
!     ******************************************************************
      TOLEN=LEN(TO)
      IJ=0
      DO I=1,TOSIZE
        TO(I)=' '
        DO J=1,TOLEN
          IJ=IJ+1
          TO(I)(J:J)=FROM(IJ)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE linkedlist_TRANSFERCHfrom1
!
!.......................................................................
!***********************************************************************
!**                                                                   **
!**  NAME: LINKEDLIST_MODULE                                          **
!**                                                                   **
!**  PURPOSE: ALLOWS TO STORE DATA IN A BRANCHED KEY-WORD DRIVEN      **
!**    LIST/TREE TRUCTURE.                                            **
!**   -THERE ARE SUBLISTS VERY SIMILAR TO DIRECTORIES IN UNIX         **
!**    AND DATA VERY SIMILAR TO FILES. HOWEVER ALSO MULTIPLE LISTS    **
!**    AND DATA ARE ALLOWED.                                          **
!**   -THIS OBJECT IS STRONGLY TYPED, THAT IS WHAT YOU RETRIEVE       **
!**    MUST HAVE THE SAME TYPE,SIZE AND KIND PARAMETERS AS WHAT       **
!**    HAS BEEN STORED.                                               **
!**   -A CERTAIN NUMBER OF WILD CARDS ARE SUPPORTED.                  **
!**                                                                   **
!**  USAGE:                                                           **
!**    TYPE(LLIST_TYPE) :: LIST                                       **
!**    CALL LINKEDLIST__NEW(LIST)                                      **
!**    CALL LINKEDLIST__SET(LIST,'FIRSTDATA',0,DATA)                   **
!**    CALL LINKEDLIST__GET(LIST,'FIRSTDATA',0,DATA)                   **
!**    CALL LINKEDLIST__DELETE(LIST,'D','FIRSTDATA',0)                 **
!**    CALL LINKEDLIST__SELECT(LIST,'SUBLIST',0)                       **
!**    CALL LINKEDLIST__SELECT(LIST,'~',0)                             **
!**                                                                   **
!**  FUNCTIONS:                                                       **
!**    LINKEDLIST__NEW(LIST)                                           **
!**    LINKEDLIST__READ(LIST,NFIL)                                     **
!**    LINKEDLIST__WRITE(LIST,NFIL)                                    **
!**    LINKEDLIST__SELECT(LIST,ID,[NTH])                               **
!**    LINKEDLIST__SET(LIST,ID,NTH,VAL)                                **
!**    LINKEDLIST__GET(LIST,ID,NTH,VAL)                                **
!**    LINKEDLIST__RMLIST(LIST,ID,[NTH])                               **
!**    LINKEDLIST__RMDATA(LIST,ID,[NTH])                               **
!**    LINKEDLIST__NLISTS(LIST,ID,NUM)                                 **
!**    LINKEDLIST__NDATA(LIST,ID,NUM)                                  **
!**    LINKEDLIST__EXISTL(LIST,ID,ITH,TCHK)                            **
!**    LINKEDLIST__EXISTD(LIST,ID,ITH,TCHK)                            **
!**    LINKEDLIST__CONVERT(LIST,ID,NTH,TYPE)                           **
!**    LINKEDLIST__ID(LIST,ITH,ID,NTH)                                 **
!**    LINKEDLIST__TYPE(LIST,ID,ITH,TYPE)                              **
!**    LINKEDLIST__SIZE(LIST,ID,ITH,SIZE)                              **
!**    LINKEDLIST__LEN(LIST,ID,ITH,LEN)                                **
!**    LINKEDLIST__REPORT(LIST,NFIL)                                   **
!**                                                                   **
!**  REMARKS:                                                         **
!**  1) THE FOLLOWING TYPES ARE SUPPORTED                             **
!**       C8,C4,R8,R4,I4,L4,CH                                        **
!**  2) SCALAR AND OBJECTS WITH RANK 1-6 ARE SUPPORTED                **
!**       ONLY THE SIZE IS PROTECTED, NOT RANK AND SHAPE              **
!**  3)SINCE IT DISTRIBUTES ITS CONTENTS ALL OVER THE                 **
!**    MEMORY AND SINCE KEY WORDS NEAD TO BE SEARCHED                 **
!**    THE LINKEDLIST SHOULD NOT BE USED ALL TOO FREQUENTLY           **
!**  4) THE FUNCTIONS LINKEDLIST__READ AND LINKEDLIST__WRITE ARE        **
!**    APPENDED BEHIND THE BUFFER_MODULE                              **
!**                                                                   **
!**                                                                   **
!***********************************************************************
MODULE LINKEDLIST_MODULE
USE LLIST_MODULE
TYPE LL_TYPE
 TYPE(LLIST_TYPE),POINTER :: PTR
END TYPE LL_TYPE
PUBLIC LINKEDLIST__NEW
PUBLIC LINKEDLIST__SELECT
PUBLIC LINKEDLIST__RMLIST
PUBLIC LINKEDLIST__RMDATA
PUBLIC LINKEDLIST__NLISTS
PUBLIC LINKEDLIST__NDATA
PUBLIC LINKEDLIST__EXISTL
PUBLIC LINKEDLIST__EXISTD
PUBLIC LINKEDLIST__LISTID
PUBLIC LINKEDLIST__DATAID
PUBLIC LINKEDLIST__TYPE
PUBLIC LINKEDLIST__SIZE
PUBLIC LINKEDLIST__CONVERT
PUBLIC LINKEDLIST__SET
PUBLIC LINKEDLIST__GET
!PUBLIC LINKEDLIST__READ
!PUBLIC LINKEDLIST__WRITE
PUBLIC LINKEDLIST__REPORT
INTERFACE LINKEDLIST__SET
  MODULE PROCEDURE LINKEDLIST__SETR8R0
  MODULE PROCEDURE LINKEDLIST__SETR4R0
  MODULE PROCEDURE LINKEDLIST__SETC8R0
  MODULE PROCEDURE LINKEDLIST__SETC4R0
  MODULE PROCEDURE LINKEDLIST__SETI4R0
  MODULE PROCEDURE LINKEDLIST__SETL4R0
  MODULE PROCEDURE LINKEDLIST__SETR8R1
  MODULE PROCEDURE LINKEDLIST__SETR4R1
  MODULE PROCEDURE LINKEDLIST__SETC8R1
  MODULE PROCEDURE LINKEDLIST__SETC4R1
  MODULE PROCEDURE LINKEDLIST__SETI4R1
  MODULE PROCEDURE LINKEDLIST__SETL4R1
  MODULE PROCEDURE LINKEDLIST__SETR8R2
  MODULE PROCEDURE LINKEDLIST__SETR4R2
  MODULE PROCEDURE LINKEDLIST__SETC8R2
  MODULE PROCEDURE LINKEDLIST__SETC4R2
  MODULE PROCEDURE LINKEDLIST__SETI4R2
  MODULE PROCEDURE LINKEDLIST__SETL4R2
  MODULE PROCEDURE LINKEDLIST__SETR8R3
  MODULE PROCEDURE LINKEDLIST__SETR4R3
  MODULE PROCEDURE LINKEDLIST__SETC8R3
  MODULE PROCEDURE LINKEDLIST__SETC4R3
  MODULE PROCEDURE LINKEDLIST__SETI4R3
  MODULE PROCEDURE LINKEDLIST__SETL4R3
  MODULE PROCEDURE LINKEDLIST__SETR8R4
  MODULE PROCEDURE LINKEDLIST__SETR4R4
  MODULE PROCEDURE LINKEDLIST__SETC8R4
  MODULE PROCEDURE LINKEDLIST__SETC4R4
  MODULE PROCEDURE LINKEDLIST__SETI4R4
  MODULE PROCEDURE LINKEDLIST__SETL4R4
  MODULE PROCEDURE LINKEDLIST__SETR8R5
  MODULE PROCEDURE LINKEDLIST__SETR4R5
  MODULE PROCEDURE LINKEDLIST__SETC8R5
  MODULE PROCEDURE LINKEDLIST__SETC4R5
  MODULE PROCEDURE LINKEDLIST__SETI4R5
  MODULE PROCEDURE LINKEDLIST__SETL4R5
  MODULE PROCEDURE LINKEDLIST__SETR8R6
  MODULE PROCEDURE LINKEDLIST__SETR4R6
  MODULE PROCEDURE LINKEDLIST__SETC8R6
  MODULE PROCEDURE LINKEDLIST__SETC4R6
  MODULE PROCEDURE LINKEDLIST__SETI4R6
  MODULE PROCEDURE LINKEDLIST__SETL4R6
  MODULE PROCEDURE LINKEDLIST__SETCHR0
  MODULE PROCEDURE LINKEDLIST__SETCHR1
  MODULE PROCEDURE LINKEDLIST__SETCHR2
  MODULE PROCEDURE LINKEDLIST__SETCHR3
  MODULE PROCEDURE LINKEDLIST__SETCHR4
  MODULE PROCEDURE LINKEDLIST__SETCHR5
  MODULE PROCEDURE LINKEDLIST__SETCHR6
END INTERFACE
INTERFACE LINKEDLIST__GET
  MODULE PROCEDURE LINKEDLIST__GETR8R0
  MODULE PROCEDURE LINKEDLIST__GETR4R0
  MODULE PROCEDURE LINKEDLIST__GETC8R0
  MODULE PROCEDURE LINKEDLIST__GETC4R0
  MODULE PROCEDURE LINKEDLIST__GETI4R0
  MODULE PROCEDURE LINKEDLIST__GETL4R0
  MODULE PROCEDURE LINKEDLIST__GETR8R1
  MODULE PROCEDURE LINKEDLIST__GETR4R1
  MODULE PROCEDURE LINKEDLIST__GETC8R1
  MODULE PROCEDURE LINKEDLIST__GETC4R1
  MODULE PROCEDURE LINKEDLIST__GETI4R1
  MODULE PROCEDURE LINKEDLIST__GETL4R1
  MODULE PROCEDURE LINKEDLIST__GETR8R2
  MODULE PROCEDURE LINKEDLIST__GETR4R2
  MODULE PROCEDURE LINKEDLIST__GETC8R2
  MODULE PROCEDURE LINKEDLIST__GETC4R2
  MODULE PROCEDURE LINKEDLIST__GETI4R2
  MODULE PROCEDURE LINKEDLIST__GETL4R2
  MODULE PROCEDURE LINKEDLIST__GETR8R3
  MODULE PROCEDURE LINKEDLIST__GETR4R3
  MODULE PROCEDURE LINKEDLIST__GETC8R3
  MODULE PROCEDURE LINKEDLIST__GETC4R3
  MODULE PROCEDURE LINKEDLIST__GETI4R3
  MODULE PROCEDURE LINKEDLIST__GETL4R3
  MODULE PROCEDURE LINKEDLIST__GETR8R4
  MODULE PROCEDURE LINKEDLIST__GETR4R4
  MODULE PROCEDURE LINKEDLIST__GETC8R4
  MODULE PROCEDURE LINKEDLIST__GETC4R4
  MODULE PROCEDURE LINKEDLIST__GETI4R4
  MODULE PROCEDURE LINKEDLIST__GETL4R4
  MODULE PROCEDURE LINKEDLIST__GETR8R5
  MODULE PROCEDURE LINKEDLIST__GETR4R5
  MODULE PROCEDURE LINKEDLIST__GETC8R5
  MODULE PROCEDURE LINKEDLIST__GETC4R5
  MODULE PROCEDURE LINKEDLIST__GETI4R5
  MODULE PROCEDURE LINKEDLIST__GETL4R5
  MODULE PROCEDURE LINKEDLIST__GETR8R6
  MODULE PROCEDURE LINKEDLIST__GETR4R6
  MODULE PROCEDURE LINKEDLIST__GETC8R6
  MODULE PROCEDURE LINKEDLIST__GETC4R6
  MODULE PROCEDURE LINKEDLIST__GETI4R6
  MODULE PROCEDURE LINKEDLIST__GETL4R6
  MODULE PROCEDURE LINKEDLIST__GETCHR0
  MODULE PROCEDURE LINKEDLIST__GETCHR1
  MODULE PROCEDURE LINKEDLIST__GETCHR2
  MODULE PROCEDURE LINKEDLIST__GETCHR3
  MODULE PROCEDURE LINKEDLIST__GETCHR4
  MODULE PROCEDURE LINKEDLIST__GETCHR5
  MODULE PROCEDURE LINKEDLIST__GETCHR6
END INTERFACE
CONTAINS
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__NEW(LL)
!     ******************************************************************
!     **                                                              **
!     **  CREATES A HOME DIRECTORY FOR THE LIST                       **
!     **  IF LIST IS ASSOCIATED ALREADY, ITS CONNECTION IS BROKEN     **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(OUT) :: LL
      TYPE(LLIST_TYPE),POINTER     :: LIST
!     ******************************************************************
      NULLIFY(LIST)
      CALL LLIST_NEW(LIST)
      LL%PTR=>LIST
      RETURN
      END SUBROUTINE LINKEDLIST__NEW
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SELECT(LL,ID,NTH_)
!     ******************************************************************
!     **                                                              **
!     **  SELECT OR (ADD AND SELECT) A LIST                           **
!     **                                                              **
!     **  WILDCARDS:                                                  **
!     **    1) '~' SELECTS THE HOME LIST IRRESPECTIVE OF NTH          **
!     **    2) '..' SELECTS THE PARENT LIST IRRESPECTIVE OF NTH       **
!     **    3) NTH=0 LIKE NTH=1, BUT CREATES A NEW LIST IF NECCESARY  **
!     **    4) NTH<0 APPENDS AND SELECTS A LIST                       **
!     **    5) '*' ELECTS THE NTH LIST IRRESPECTIVE OF ID             **
!     **                                                              **
!     **  ERROR__CONDITIONS:                                           **
!     **    1) ATTEMPT TO SELECT PARENT OF HOME LIST                  **
!     **    2) LIST DOES NOT EXIST                                    **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(INOUT) :: LL
      CHARACTER(*)    ,INTENT(IN)    :: ID
      INTEGER(4)      ,INTENT(IN),OPTIONAL :: NTH_
      TYPE(LLIST_TYPE),POINTER       :: LIST
      TYPE(LLIST_TYPE),POINTER       :: NEWLIST
      INTEGER(4)                     :: NTH
!     ******************************************************************
      LIST=>LL%PTR
      NTH=0
      IF(PRESENT(NTH_)) NTH=NTH_
!
!     =================================================================
!     == SELECT HOME LIST                                            ==
!     =================================================================
      IF(TRIM(ID).EQ.TRIM(CH32HOME)) THEN
        CALL LLIST_FINDLIST(LIST,ID,1,NEWLIST)
        LL%PTR=>NEWLIST
!
!     =================================================================
!     == SELECT PARENT LIST                                          ==
!     =================================================================
      ELSE IF(TRIM(ID).EQ.TRIM(CH32BACK)) THEN
        IF(LIST%ID.EQ.CH32HOME) THEN
          CALL ERROR__MSG('CANNOT SELECT PARENT OF HOME LIST')
          CALL ERROR__STOP('LINKEDLIST__SELECT')
        END IF
        CALL LLIST_FINDLIST(LIST,ID,1,NEWLIST)
        LL%PTR=>NEWLIST
!
!     =================================================================
!     == SELECT FIRST LIST AND CREATE ONE IF NECCESARY              ==
!     =================================================================
      ELSE IF(NTH.EQ.0) THEN
        CALL LLIST_FINDLIST(LIST,ID,1,NEWLIST)
        IF(.NOT.ASSOCIATED(NEWLIST)) THEN
          CALL LLIST_APPENDLIST(LIST,ID,NEWLIST)
        END IF
        LL%PTR=>NEWLIST
        RETURN
!
!     =================================================================
!     == APPEND LIST IF NTH<0                                        ==
!     =================================================================
      ELSE IF(NTH.LT.0) THEN
        IF(TRIM(ID).EQ.TRIM(CH32ANY)) THEN
          CALL ERROR__MSG('ID MUST NO BE A PROTECTED STRING')
          CALL ERROR__STOP('LINKEDLIST__SELECT')
        END IF
        CALL LLIST_APPENDLIST(LIST,ID,NEWLIST)
        LL%PTR=>NEWLIST
        RETURN
!
!     =================================================================
!     ==  OTHERWISE IF N.NEQ.0 SELECT AN EXISTING LIST               ==
!     =================================================================
      ELSE
        CALL LLIST_FINDLIST(LIST,ID,NTH,NEWLIST)
        IF(.NOT.ASSOCIATED(NEWLIST)) THEN
          CALL ERROR__MSG('LIST NOT FOUND')
          CALL ERROR__CHVAL('ID ',ID)
          CALL ERROR__STOP('LINKEDLIST__SELECT')
        END IF
        LL%PTR=>NEWLIST
        RETURN
      END IF
      RETURN
      END SUBROUTINE LINKEDLIST__SELECT
!
!     ..................................................................
      SUBROUTINE LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE,NBYTE,CHARVAL)
!     ******************************************************************
!     **                                                              **
!     **  STORES A DATA                                               **
!     **                                                              **
!     **  WILDCARDS:                                                  **
!     **    1) NTH=0 ATTACH DATA TO THE FIST ITEM AND CREATE ONE      **
!     **       IF NECCESARY                                           **
!     **    2) NTH<0 APPENDS DATA ENTRY AND ATTACHES DATA THERE       **
!     **    3) '*' APPLIES TO ANY ID                                  **
!     **                                                              **
!     **  ERROR__CONDITIONS:                                           **
!     **    1) CANNOT FIND DATA ENTRY                                 **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE),POINTER   :: LIST
      CHARACTER(*)   ,INTENT(IN) :: ID
      INTEGER(4)     ,INTENT(IN) :: NTH
      CHARACTER(*)   ,INTENT(IN) :: TYPE
      INTEGER(4)     ,INTENT(IN) :: NBYTE
      CHARACTER(1)   ,POINTER    :: CHARVAL(:)
      INTEGER(4)                 :: NUM,IERR
!     ******************************************************************
      IERR=0
      IF(NTH.EQ.0) THEN
        CALL LLIST_NDATA(LIST,ID,NUM)
        IF(NUM.GT.0) THEN
          CALL LLIST_SETPTR(LIST,ID,1,TYPE,NBYTE,CHARVAL,IERR)
        ELSE
          CALL LLIST_ADDPTR(LIST,ID,TYPE,NBYTE,CHARVAL)
        END IF
        RETURN
      ELSE IF(NTH.LT.0) THEN
        CALL LLIST_ADDPTR(LIST,ID,TYPE,NBYTE,CHARVAL)
      ELSE
        CALL LLIST_SETPTR(LIST,ID,NTH,TYPE,NBYTE,CHARVAL,IERR)
      END IF
      IF(IERR.NE.0) THEN
        CALL ERROR__MSG('CANNOT FIND DATA ENTRY')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__I4VAL('NTH',NTH)
        CALL ERROR__STOP('LINKEDLIST_SETGENERIC')
      END IF
      RETURN
      END SUBROUTINE LINKEDLIST_SETGENERIC
!
!     ..................................................................
      SUBROUTINE LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE,NBYTE,CHARVAL)
!     ******************************************************************
!     **                                                              **
!     **  RETRIEVES A DATA FROM A DATA ENTRY                          **
!     **                                                              **
!     **  WILDCARDS:                                                  **
!     **    1) NTH=0 LIKE NTH=1                                       **
!     **    3) '*' APPLIES TO ANY ID                                  **
!     **                                                              **
!     **  ERROR__CONDITIONS:                                           **
!     **    1) NTH<0                                                  **
!     **    2) DATA NOT FOUND                                         **
!     **    3) SIZE INCONSISTENT                                      **
!     **    4) TYPE INCONSISTENT                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LLIST_TYPE),POINTER   :: LIST
      CHARACTER(*)   ,INTENT(IN) :: ID
      INTEGER(4)     ,INTENT(IN) :: NTH
      INTEGER(4)     ,INTENT(IN) :: NBYTE  ! size of array
      CHARACTER(*)   ,INTENT(IN) :: TYPE
      CHARACTER(1)   ,POINTER    :: CHARVAL(:)
      INTEGER(4)                 :: NBYTE1
      CHARACTER(8)               :: TYPE1
!     ******************************************************************
      IF(NTH.EQ.0) THEN
        CALL LLIST_GETPTR(LIST,ID,1,TYPE1,NBYTE1,CHARVAL)
      ELSE IF (NTH.LT.0) THEN
        CALL ERROR__MSG('NTH<0 IS NOT ALLOWED')
        CALL ERROR__STOP('LINKEDLIST_GETGENERIC')
      ELSE
        CALL LLIST_GETPTR(LIST,ID,NTH,TYPE1,NBYTE1,CHARVAL)
      END IF
!
!     ==================================================================
!     ==  FINAL CHECKS                                                ==
!     ==================================================================
      IF(.NOT.ASSOCIATED(CHARVAL)) THEN
        CALL ERROR__MSG('DATA ENTRY NOT FOUND')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__I4VAL('NTH',NTH)
        CALL ERROR__STOP('LINKEDLIST_GETGENERIC')
      END IF
      IF(NBYTE1.NE.NBYTE) THEN
        CALL ERROR__MSG('SIZE INCONSISTENT')
        CALL ERROR__CHVAL('ID',ID)
        CALL ERROR__I4VAL('NTH',NTH)
        CALL ERROR__I4VAL('SIZE EXPECTED',NBYTE)
        CALL ERROR__I4VAL('ACTUAL SIZE  ',NBYTE1)
        CALL ERROR__STOP('LINKEDLIST_GETGENERIC')
      END IF
      IF(TRIM(TYPE1).NE.TRIM(TYPE)) THEN
        IF(TYPE1(1:2).NE.'CH'.OR.TYPE(1:2).NE.'CH') THEN
          CALL ERROR__MSG('TYPE INCONSISTENT')
          CALL ERROR__CHVAL('ID',ID)
          CALL ERROR__I4VAL('NTH',NTH)
          CALL ERROR__CHVAL('TYPE EXPECTED',TRIM(TYPE))
          CALL ERROR__CHVAL('ACTUAL TYPE  ',TRIM(TYPE1))
          CALL ERROR__STOP('LINKEDLIST_GETGENERIC')
        END IF
      END IF
      RETURN
      END SUBROUTINE LINKEDLIST_GETGENERIC
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__RMLIST(LL,ID,NTH_)
!     ******************************************************************
!     **                                                              **
!     **  DELETES A LIST                                              **
!     **                                                              **
!     **  WILD CARDS:                                                 **
!     **    1) '*' THE NTH ENTRY IS RMDATA IRRESPECTIVE OF ITS ID    **
!     **       (CAN BE COMBINED WITH NTH<0)                           **
!     **    2) NTH=0 LIKE NTH=1 BUT NO ERROR CONDITION OCCURS         **
!     **       IF THE DATA ITEM IS NOT FOUND                          **
!     **    3) IF NTH IS NOT PRESENT ALL ITEMS CONSISTENT WITH ID     **
!     **       ARE REMOVED (EQUIVALENT TO NTH<0)                      **
!     **                                                              **
!     **  ERROR CONDITIONS:                                           **
!     **    2) SPECIFIED DATA OR LIST ITEM DOES NOT EXIST             **
!     **       (ONLY FOR NTH>0)                                       **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN),OPTIONAL :: NTH_
      TYPE(LLIST_TYPE),POINTER    :: LIST
      INTEGER(4)                  :: NTH
      INTEGER(4)                  :: NUM
      INTEGER(4)                  :: ITH
!     ******************************************************************
      LIST=>LL%PTR
      NTH=-1
      IF(PRESENT(NTH_))NTH=NTH_
!
!     ==================================================================
!     == REMOVE ALL ITEMS FOR NTH<0                                   ==
!     ==================================================================
      IF(NTH.LT.0) THEN
        CALL LLIST_NLISTS(LIST,ID,NUM)
        DO ITH=1,NUM
          CALL LLIST_REMOVELIST(LIST,ID,ITH)
        ENDDO
!
!     ==================================================================
!     == REMOVE SPECIFIC ITEM                                         ==
!     ==================================================================
      ELSE
        CALL LLIST_NLISTS(LIST,ID,NUM)
        IF(NTH.EQ.0) THEN
          IF(NUM.EQ.0) RETURN
          ITH=1
        ELSE
          IF(NUM.LT.NTH) THEN
            CALL ERROR__MSG('ITEM DOES NOT EXIST')
            CALL ERROR__CHVAL('ID',ID)
            CALL ERROR__I4VAL('NTH',NTH)
            CALL ERROR__I4VAL('NUMBER',NUM)
            CALL ERROR__STOP('LINKEDLIST__DELETE')
          END IF
          ITH=NTH
        END IF
        CALL LLIST_REMOVELIST(LIST,ID,ITH)
      END IF
      RETURN
      END SUBROUTINE LINKEDLIST__RMLIST
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__RMDATA(LL,ID,NTH_)
!     ******************************************************************
!     **                                                              **
!     **  DELETES A DATA                                              **
!     **                                                              **
!     **  WILD CARDS:                                                 **
!     **    1) IF NTH_ IS NOT PRESENT ALL DATA CONSISTENT WITH ID    **
!     **       ARE REMOVED (EQUIVALENT TO NTH_<0)                     **
!     **    2) '*' THE NTH ENTRY IS RMDATA IRRESPECTIVE OF ITS ID    **
!     **       (CAN BE COMBINED WITH NTH<0)                           **
!     **    3) NTH=0 LIKE NTH=1 BUT NO ERROR CONDITION OCCURS         **
!     **       IF THE DATA ITEM IS NOT FOUND                          **
!     **                                                              **
!     **                                                              **
!     **  ERROR CONDITIONS:                                           **
!     **    2) SPECIFIED DATA OR LIST ITEM DOES NOT EXIST             **
!     **       (ONLY FOR NTH>0)                                       **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN),OPTIONAL :: NTH_
      TYPE(LLIST_TYPE),POINTER    :: LIST
      INTEGER(4)                  :: NTH
      INTEGER(4)                  :: NUM
      INTEGER(4)                  :: ITH
!     ******************************************************************
      LIST=>LL%PTR
      NTH=-1
      IF(PRESENT(NTH_)) NTH=NTH_
!
!     ==================================================================
!     == REMOVE ALL ITEMS FOR NTH<0                                   ==
!     ==================================================================
      IF(NTH.LT.0) THEN
        CALL LLIST_NDATA(LIST,ID,NUM)
        DO ITH=1,NUM
          CALL LLIST_REMOVEDATA(LIST,ID,ITH)
        ENDDO
!
!     ==================================================================
!     == REMOVE SPECIFIC ITEM                                         ==
!     ==================================================================
      ELSE
        CALL LLIST_NDATA(LIST,ID,NUM)
        IF(NTH.EQ.0) THEN
          IF(NUM.EQ.0) RETURN
          ITH=1
        ELSE
          IF(NUM.LT.NTH) THEN
            CALL ERROR__MSG('ITEM DOES NOT EXIST')
            CALL ERROR__CHVAL('ID',ID)
            CALL ERROR__I4VAL('NTH',NTH)
            CALL ERROR__I4VAL('NUMBER',NUM)
            CALL ERROR__STOP('LINKEDLIST__DELETE')
          END IF
          ITH=NTH
        END IF
        CALL LLIST_REMOVEDATA(LIST,ID,ITH)
      END IF
      RETURN
      END SUBROUTINE LINKEDLIST__RMDATA
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__NLISTS(LL,ID,NUM)
!     ******************************************************************
!     **                                                              **
!     **  COUNTS THE NUMBER OF LIST                                   **
!     **                                                              **
!     **  WILD CARDS:                                                 **
!     **    1) '*' COUNTS ALL ITMES                                   **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      TYPE(LLIST_TYPE),POINTER    :: LIST
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(OUT):: NUM
!     ******************************************************************
      LIST=>LL%PTR
      CALL LLIST_NLISTS(LIST,ID,NUM)
      RETURN
      END SUBROUTINE LINKEDLIST__NLISTS
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__NDATA(LL,ID,NUM)
!     ******************************************************************
!     **                                                              **
!     **  COUNTS THE NUMBER OF LIST OR DATA ITEMS                     **
!     **                                                              **
!     **  WILD CARDS:                                                 **
!     **    1) '*' COUNTS ALL ITMES                                   **
!     **                                                              **
!     **  ERROR CONDITIONS:                                           **
!     **    1) SWITCH IS NEITHER 'D' OR 'L'                           **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      TYPE(LLIST_TYPE),POINTER    :: LIST
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(OUT):: NUM
!     ******************************************************************
      LIST=>LL%PTR
      CALL LLIST_NDATA(LIST,ID,NUM)
      RETURN
      END SUBROUTINE LINKEDLIST__NDATA
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__EXISTL(LL,ID,NTH,TCHK)
!     ******************************************************************
!     **                                                              **
!     **  TESTS WHETHER A LIST OR DATA ITEM EXISTS                    **
!     **                                                              **
!     **  WILD CARDS:                                                 **
!     **    1) '*' TEST ITEMS IRRESPECTIVE OF THEIR ID                **
!     **    2) NTH=0 LIKE NTH=1                                       **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)      ,INTENT(OUT):: TCHK
      TYPE(LLIST_TYPE),POINTER    :: LIST
      INTEGER(4)                  :: NUM
!     ******************************************************************
      LIST=>LL%PTR
      CALL LLIST_NLISTS(LIST,ID,NUM)
      IF(NTH.GT.0) THEN
        TCHK=(NUM.EQ.NTH)
      ELSE IF(NTH.EQ.0) THEN
        TCHK=(NUM.GT.0)
      ELSE
        CALL ERROR__MSG('NTH<0 NOT ALLOWED')
        CALL ERROR__STOP('LINKEDLIST__EXIST')
      END IF
      END SUBROUTINE LINKEDLIST__EXISTL
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__EXISTD(LL,ID,NTH,TCHK)
!     ******************************************************************
!     **                                                              **
!     **  TESTS WHETHER A LIST OR DATA ITEM EXISTS                    **
!     **                                                              **
!     **  WILD CARDS:                                                 **
!     **    1) '*' TEST ITEMS IRRESPECTIVE OF THEIR ID                **
!     **    2) NTH=0 LIKE NTH=1                                       **
!     **                                                              **
!     **  ERROR CONDITIONS:                                           **
!     **    1) SWITCH IS NEITHER 'D' OR 'L'                           **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)      ,INTENT(OUT):: TCHK
      TYPE(LLIST_TYPE),POINTER    :: LIST
      INTEGER(4)                  :: NUM
!     ******************************************************************
      LIST=>LL%PTR
      CALL LLIST_NDATA(LIST,ID,NUM)
      IF(NTH.GT.0) THEN
        TCHK=(NUM.EQ.NTH)
      ELSE IF(NTH.EQ.0) THEN
        TCHK=(NUM.GT.0)
      ELSE
        CALL ERROR__MSG('NTH<0 NOT ALLOWED')
        CALL ERROR__STOP('LINKEDLIST__EXIST')
      END IF
      END SUBROUTINE LINKEDLIST__EXISTD
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__LISTID(LL,ITH,ID,NTH)
!     ******************************************************************
!     **                                                              **
!     **  RETURNS THE KEY WORD OF A LIST                              **
!     **                                                              **
!     **  ERROR CONDITIONS:                                           **
!     **    2) NTH<0                                                  **
!     **    2) DATA ENTRY NOT FOUND                                   **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      INTEGER(4)      ,INTENT(IN) :: ITH
      CHARACTER(*)    ,INTENT(OUT):: ID
      INTEGER(4)      ,INTENT(OUT):: NTH
      TYPE(LLIST_TYPE),POINTER    :: LIST1
      TYPE(LLIST_TYPE),POINTER    :: LIST
      INTEGER(4)                  :: I
!     ******************************************************************
      LIST=>LL%PTR
      CALL LLIST_FINDLIST(LIST,'*',ITH,LIST1)
      IF(LEN_TRIM(LIST1%ID).GT.LEN(ID)) THEN
        CALL ERROR__MSG('STRING PROVIDED FOR ID TOO SMALL')
        CALL ERROR__STOP('LINKEDLIST__LISTID')
      END IF
      ID=LIST1%ID
      NTH=0
      DO I=1,ITH
        CALL LLIST_FINDLIST(LIST,'*',ITH,LIST1)
        IF(TRIM(LIST1%ID).EQ.TRIM(ID)) NTH=NTH+1
      ENDDO
      END SUBROUTINE LINKEDLIST__LISTID
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__DATAID(LL,ITH,ID,NTH)
!     ******************************************************************
!     **                                                              **
!     **  RETURNS THE KEY WORD OF A DATA                              **
!     **                                                              **
!     **  ERROR CONDITIONS:                                           **
!     **    2) NTH<0                                                  **
!     **    2) DATA ENTRY NOT FOUND                                   **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)  ,INTENT(IN) :: LL
      INTEGER(4)     ,INTENT(IN) :: ITH
      CHARACTER(*)   ,INTENT(OUT):: ID
      INTEGER(4)     ,INTENT(OUT):: NTH
      TYPE(LLIST_TYPE),POINTER   :: LIST
      TYPE(LDATA_TYPE),POINTER   :: DATA
      INTEGER(4)                 :: I
!     ******************************************************************
      LIST=>LL%PTR
      CALL LLIST_FINDDATA(LIST,'*',ITH,DATA)
      IF(LEN_TRIM(DATA%ID).GT.LEN(ID)) THEN
        CALL ERROR__MSG('STRING PROVIDED FOR ID TOOO SMALL')
        CALL ERROR__STOP('LINKEDLIST__DATAID')
      END IF
      ID=DATA%ID
      NTH=0
      DO I=1,ITH
        CALL LLIST_FINDDATA(LIST,'*',I,DATA)
        IF(TRIM(DATA%ID).EQ.TRIM(ID)) NTH=NTH+1
      ENDDO
      END SUBROUTINE LINKEDLIST__DATAID
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__TYPE(LL,ID,NTH,TYPE)
!     ******************************************************************
!     **                                                              **
!     **  RETURNS THE TYPE OF A DATA                                  **
!     **                                                              **
!     **  WILD CARDS:                                                 **
!     **    1) '*' TEST ITEMS IRRESPECTIVE OF THEIR ID                **
!     **    2) NTH=0 LIKE NTH=1                                       **
!     **                                                              **
!     **  ERROR CONDITIONS:                                           **
!     **    2) NTH<0                                                  **
!     **    2) DATA ENTRY NOT FOUND                                   **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)  ,INTENT(IN) :: LL
      CHARACTER(*)   ,INTENT(IN) :: ID
      INTEGER(4)     ,INTENT(IN) :: NTH
      CHARACTER(*)   ,INTENT(OUT):: TYPE
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(LDATA_TYPE),POINTER    :: DATA
!     ******************************************************************
      LIST=>LL%PTR
      IF(NTH.GT.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,NTH,DATA)
      ELSE IF(NTH.EQ.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,1,DATA)
      ELSE
        CALL ERROR__MSG('NTH MUST NOT BE NEGATIVE')
        CALL ERROR__STOP('LINKEDLIST__TYPE')
      END IF
      IF(.NOT.ASSOCIATED(DATA)) THEN
        CALL ERROR__MSG('DATA DOES NOT EXIST')
        CALL ERROR__STOP('LINKEDLIST__TYPE')
      END IF
      TYPE=DATA%TYPE
      END SUBROUTINE LINKEDLIST__TYPE
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SIZE(LL,ID,NTH,SIZE)
!     ******************************************************************
!     **                                                              **
!     **  RETURNS THE SIZE OF A DATA                                  **
!     **                                                              **
!     **  WILD CARDS:                                                 **
!     **    1) '*' TEST ITEMS IRRESPECTIVE OF THEIR ID                **
!     **    2) NTH=0 LIKE NTH=1                                       **
!     **                                                              **
!     **  ERROR CONDITIONS:                                           **
!     **    2) NTH<0                                                  **
!     **    2) DATA ENTRY NOT FOUND                                   **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      TYPE(LLIST_TYPE),POINTER    :: LIST
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)      ,INTENT(OUT):: SIZE
      TYPE(LDATA_TYPE),POINTER    :: DATA
!     ******************************************************************
      LIST=>LL%PTR
      IF(NTH.GT.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,NTH,DATA)
      ELSE IF(NTH.EQ.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,1,DATA)
      ELSE
        CALL ERROR__MSG('NTH MUST NOT BE NEGATIVE')
        CALL ERROR__STOP('LINKEDLIST__SIZE')
      END IF
      IF(.NOT.ASSOCIATED(DATA)) THEN
        CALL ERROR__MSG('DATA DOES NOT EXIST')
        CALL ERROR__STOP('LINKEDLIST__SIZE')
      END IF
      SIZE=DATA%SIZE
      END SUBROUTINE LINKEDLIST__SIZE
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__LEN(LL,ID,NTH,LEN)
!     ******************************************************************
!     **                                                              **
!     **  RETURNS THE LENGTH OF A DATA                                **
!     **                                                              **
!     **  WILD CARDS:                                                 **
!     **    1) '*' TEST ITEMS IRRESPECTIVE OF THEIR ID                **
!     **    2) NTH=0 LIKE NTH=1                                       **
!     **                                                              **
!     **  ERROR CONDITIONS:                                           **
!     **    2) NTH<0                                                  **
!     **    2) DATA ENTRY NOT FOUND                                   **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)      ,INTENT(OUT):: LEN
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(LDATA_TYPE),POINTER    :: DATA
      CHARACTER(8)                :: STRING
      INTEGER(4)                  :: I1,I2
!     ******************************************************************
      LIST=>LL%PTR
      IF(NTH.GT.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,NTH,DATA)
      ELSE IF(NTH.EQ.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,1,DATA)
      ELSE
        CALL ERROR__MSG('NTH MUST NOT BE NEGATIVE')
        CALL ERROR__STOP('LINKEDLIST__SIZE')
      END IF
      IF(.NOT.ASSOCIATED(DATA)) THEN
        CALL ERROR__MSG('DATA DOES NOT EXIST')
        CALL ERROR__STOP('LINKEDLIST__SIZE')
      END IF
      STRING=DATA%TYPE
      I1=INDEX(STRING,'(')
      I2=INDEX(STRING,')',.TRUE.)
      STRING=STRING(I1+1:I2-1)
      READ(STRING,*)LEN
      END SUBROUTINE LINKEDLIST__LEN
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__REPORT(LL,NFIL)
!     ******************************************************************
!     **                                                              **
!     **  WRITES THE CHARACTERISTICS OF THE LIST,SUBLISTS AND         **
!     **  THEIR DATA TO FILE                                          **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      TYPE(LLIST_TYPE),POINTER    :: LIST
      INTEGER(4)      ,INTENT(IN) :: NFIL
      INTEGER(4)                  :: I,LEVEL
!     ******************************************************************
      LIST=>LL%PTR
      WRITE(NFIL,FMT='(72("="),T5," REPORT FOR LINKED LIST: ",A," ")') &
     &     TRIM(LIST%ID)
      LEVEL=0
      IF(ASSOCIATED(LIST%DATA)) THEN
        CALL LLIST_REPORTDATA(LIST%DATA,NFIL,LEVEL)
      END IF
      IF(ASSOCIATED(LIST%LISTS)) THEN
        CALL LLIST_REPORTLISTS(LIST%LISTS,NFIL,LEVEL)
      END IF
      WRITE(NFIL,FMT='(72("="))')
      END SUBROUTINE LINKEDLIST__REPORT
!     ....................................................................
!#TEMPLATE LINKEDLIST__SETNUM
!(<TYPEID>,<TYPE>,<TYPEDEF>)=([R8],[REAL(8)],['R(8)',8])
!                            ([R4],[REAL(4)],['R(4)',4])
!                            ([C8],[COMPLEX(8)],['C(8)',16])
!                            ([C4],[COMPLEX(4)],['C(4)',8])
!                            ([I4],[INTEGER(4)],['I(4)',4])
!                            ([L4],[LOGICAL(4)],['L(4)',4])
!(<RANKID>,<RANK>,<SIZE>)=([R0],[],[1])
!                         ([R1],[(:)],[SIZE(VAL)])
!                         ([R2],[(:,:)],[SIZE(VAL)])
!                         ([R3],[(:,:,:)],[SIZE(VAL)])
!                         ([R4],[(:,:,:,:)],[SIZE(VAL)])
!                         ([R5],[(:,:,:,:,:)],[SIZE(VAL)])
!                         ([R6],[(:,:,:,:,:,:)],[SIZE(VAL)])
!#BODY
!!
!!     ..................................................................
!      SUBROUTINE LINKEDLIST__SET<TYPEID><RANKID>(LL,ID,NTH,VAL)
!!     ******************************************************************
!!     **                                                              **
!!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!!     **                                                              **
!!     **  REMARKS:                                                    **
!!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!!     **                                                              **
!!     ******************************************************************
!      IMPLICIT NONE
!      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
!      CHARACTER(*)    ,INTENT(IN) :: ID
!      INTEGER(4)      ,INTENT(IN) :: NTH
!      <TYPE>          ,INTENT(IN) :: VAL<RANK>
!      TYPE(LLIST_TYPE),POINTER    :: LIST
!      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE(<TYPEDEF>)
!      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
!      INTEGER(4)                  :: LENG
!!     ******************************************************************
!      LIST=>LL%PTR
!      LENG=<SIZE>
!      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
!      CHARVAL=TRANSFER(VAL,CHARVAL)
!      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
!      RETURN
!      END SUBROUTINE LINKEDLIST__SET<TYPEID><RANKID>
!#INSTANCES
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETR8R0(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(8)          ,INTENT(IN) :: VAL
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(8)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=1
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETR8R0
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETR4R0(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(4)          ,INTENT(IN) :: VAL
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=1
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETR4R0
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETC8R0(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(8)          ,INTENT(IN) :: VAL
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(8)',16)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=1
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETC8R0
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETC4R0(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(4)          ,INTENT(IN) :: VAL
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(4)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=1
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETC4R0
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETI4R0(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)          ,INTENT(IN) :: VAL
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('I(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=1
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETI4R0
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETL4R0(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)          ,INTENT(IN) :: VAL
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('L(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=1
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETL4R0
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETR8R1(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(8)          ,INTENT(IN) :: VAL(:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(8)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETR8R1
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETR4R1(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(4)          ,INTENT(IN) :: VAL(:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETR4R1
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETC8R1(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(8)          ,INTENT(IN) :: VAL(:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(8)',16)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETC8R1
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETC4R1(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(4)          ,INTENT(IN) :: VAL(:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(4)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETC4R1
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETI4R1(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)          ,INTENT(IN) :: VAL(:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('I(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETI4R1
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETL4R1(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)          ,INTENT(IN) :: VAL(:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('L(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETL4R1
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETR8R2(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(8)          ,INTENT(IN) :: VAL(:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(8)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETR8R2
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETR4R2(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(4)          ,INTENT(IN) :: VAL(:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETR4R2
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETC8R2(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(8)          ,INTENT(IN) :: VAL(:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(8)',16)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETC8R2
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETC4R2(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(4)          ,INTENT(IN) :: VAL(:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(4)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETC4R2
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETI4R2(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)          ,INTENT(IN) :: VAL(:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('I(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETI4R2
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETL4R2(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)          ,INTENT(IN) :: VAL(:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('L(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETL4R2
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETR8R3(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(8)          ,INTENT(IN) :: VAL(:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(8)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETR8R3
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETR4R3(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(4)          ,INTENT(IN) :: VAL(:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETR4R3
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETC8R3(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(8)          ,INTENT(IN) :: VAL(:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(8)',16)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETC8R3
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETC4R3(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(4)          ,INTENT(IN) :: VAL(:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(4)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETC4R3
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETI4R3(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)          ,INTENT(IN) :: VAL(:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('I(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETI4R3
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETL4R3(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)          ,INTENT(IN) :: VAL(:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('L(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETL4R3
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETR8R4(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(8)          ,INTENT(IN) :: VAL(:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(8)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETR8R4
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETR4R4(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(4)          ,INTENT(IN) :: VAL(:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETR4R4
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETC8R4(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(8)          ,INTENT(IN) :: VAL(:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(8)',16)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETC8R4
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETC4R4(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(4)          ,INTENT(IN) :: VAL(:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(4)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETC4R4
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETI4R4(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)          ,INTENT(IN) :: VAL(:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('I(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETI4R4
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETL4R4(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)          ,INTENT(IN) :: VAL(:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('L(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETL4R4
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETR8R5(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(8)          ,INTENT(IN) :: VAL(:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(8)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETR8R5
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETR4R5(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(4)          ,INTENT(IN) :: VAL(:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETR4R5
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETC8R5(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(8)          ,INTENT(IN) :: VAL(:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(8)',16)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETC8R5
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETC4R5(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(4)          ,INTENT(IN) :: VAL(:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(4)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETC4R5
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETI4R5(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)          ,INTENT(IN) :: VAL(:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('I(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETI4R5
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETL4R5(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)          ,INTENT(IN) :: VAL(:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('L(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETL4R5
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETR8R6(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(8)          ,INTENT(IN) :: VAL(:,:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(8)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETR8R6
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETR4R6(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(4)          ,INTENT(IN) :: VAL(:,:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETR4R6
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETC8R6(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(8)          ,INTENT(IN) :: VAL(:,:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(8)',16)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETC8R6
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETC4R6(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(4)          ,INTENT(IN) :: VAL(:,:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(4)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETC4R6
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETI4R6(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)          ,INTENT(IN) :: VAL(:,:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('I(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETI4R6
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETL4R6(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)          ,INTENT(IN) :: VAL(:,:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('L(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      ALLOCATE(CHARVAL(LENG*TYPE%NBYTE))
      CHARVAL=TRANSFER(VAL,CHARVAL)
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETL4R6
!#END TEMPLATE LINKEDLIST__SETNUM
!#TEMPLATE LINKEDLIST__GETNUM
!(<TYPEID>,<TYPE>,<TYPEDEF>)=([R8],[REAL(8)],['R(8)',8])
!                            ([R4],[REAL(4)],['R(4)',4])
!                            ([C8],[COMPLEX(8)],['C(8)',16])
!                            ([C4],[COMPLEX(4)],['C(4)',8])
!                            ([I4],[INTEGER(4)],['I(4)',4])
!                            ([L4],[LOGICAL(4)],['L(4)',4])
!(<RANKID>,<SIZE>,<RESHAPE(><RESHAPE)><RANK>)
!              =([R0],[1],[],[],[])
!               ([R1],[SIZE(VAL)][RESHAPE(][,SHAPE(VAL))],[(:)])
!               ([R2],[SIZE(VAL)][RESHAPE(][,SHAPE(VAL))],[(:,:)])
!               ([R3],[SIZE(VAL)][RESHAPE(][,SHAPE(VAL))],[(:,:,:)])
!               ([R4],[SIZE(VAL)][RESHAPE(][,SHAPE(VAL))],[(:,:,:,:)])
!               ([R5],[SIZE(VAL)][RESHAPE(][,SHAPE(VAL))],[(:,:,:,:,:)])
!               ([R6],[SIZE(VAL)][RESHAPE(][,SHAPE(VAL))],[(:,:,:,:,:,:)])
!#BODY
!!
!!     ..................................................................
!      SUBROUTINE LINKEDLIST__GET<TYPEID><RANKID>(LL,ID,NTH,VAL)
!!     ******************************************************************
!!     **                                                              **
!!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!!     **                                                              **
!!     **  REMARKS:                                                    **
!!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!!     **                                                              **
!!     ******************************************************************
!      IMPLICIT NONE
!      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
!      CHARACTER(*)    ,INTENT(IN) :: ID
!      INTEGER(4)      ,INTENT(IN) :: NTH
!      <TYPE>          ,INTENT(OUT):: VAL<RANK>
!      TYPE(LLIST_TYPE),POINTER    :: LIST
!!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=<TYPEID>TYPE
!      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE(<TYPEDEF>)
!      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
!      INTEGER(4)                  :: LENG
!!     ******************************************************************
!      LIST=>LL%PTR
!      LENG=<SIZE>
!      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
!      VAL=<RESHAPE(>TRANSFER(CHARVAL,VAL)<RESHAPE)>
!      RETURN
!      END SUBROUTINE LINKEDLIST__GET<TYPEID><RANKID>
!#INSTANCES
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETR8R0(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(8)          ,INTENT(OUT):: VAL
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=R8TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(8)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=1
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=TRANSFER(CHARVAL,VAL)
      RETURN
      END SUBROUTINE LINKEDLIST__GETR8R0
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETR4R0(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(4)          ,INTENT(OUT):: VAL
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=R4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=1
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=TRANSFER(CHARVAL,VAL)
      RETURN
      END SUBROUTINE LINKEDLIST__GETR4R0
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETC8R0(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(8)          ,INTENT(OUT):: VAL
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=C8TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(8)',16)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=1
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=TRANSFER(CHARVAL,VAL)
      RETURN
      END SUBROUTINE LINKEDLIST__GETC8R0
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETC4R0(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(4)          ,INTENT(OUT):: VAL
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=C4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(4)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=1
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=TRANSFER(CHARVAL,VAL)
      RETURN
      END SUBROUTINE LINKEDLIST__GETC4R0
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETI4R0(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)          ,INTENT(OUT):: VAL
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=I4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('I(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=1
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=TRANSFER(CHARVAL,VAL)
      RETURN
      END SUBROUTINE LINKEDLIST__GETI4R0
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETL4R0(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)          ,INTENT(OUT):: VAL
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=L4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('L(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=1
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=TRANSFER(CHARVAL,VAL)
      RETURN
      END SUBROUTINE LINKEDLIST__GETL4R0
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETR8R1(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(8)          ,INTENT(OUT):: VAL(:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=R8TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(8)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETR8R1
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETR4R1(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(4)          ,INTENT(OUT):: VAL(:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=R4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETR4R1
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETC8R1(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(8)          ,INTENT(OUT):: VAL(:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=C8TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(8)',16)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETC8R1
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETC4R1(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(4)          ,INTENT(OUT):: VAL(:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=C4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(4)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETC4R1
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETI4R1(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)          ,INTENT(OUT):: VAL(:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=I4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('I(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETI4R1
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETL4R1(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)          ,INTENT(OUT):: VAL(:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=L4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('L(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETL4R1
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETR8R2(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(8)          ,INTENT(OUT):: VAL(:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=R8TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(8)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETR8R2
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETR4R2(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(4)          ,INTENT(OUT):: VAL(:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=R4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETR4R2
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETC8R2(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(8)          ,INTENT(OUT):: VAL(:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=C8TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(8)',16)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETC8R2
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETC4R2(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(4)          ,INTENT(OUT):: VAL(:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=C4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(4)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETC4R2
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETI4R2(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)          ,INTENT(OUT):: VAL(:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=I4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('I(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETI4R2
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETL4R2(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)          ,INTENT(OUT):: VAL(:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=L4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('L(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETL4R2
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETR8R3(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(8)          ,INTENT(OUT):: VAL(:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=R8TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(8)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETR8R3
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETR4R3(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(4)          ,INTENT(OUT):: VAL(:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=R4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETR4R3
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETC8R3(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(8)          ,INTENT(OUT):: VAL(:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=C8TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(8)',16)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETC8R3
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETC4R3(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(4)          ,INTENT(OUT):: VAL(:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=C4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(4)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETC4R3
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETI4R3(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)          ,INTENT(OUT):: VAL(:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=I4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('I(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETI4R3
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETL4R3(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)          ,INTENT(OUT):: VAL(:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=L4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('L(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETL4R3
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETR8R4(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(8)          ,INTENT(OUT):: VAL(:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=R8TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(8)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETR8R4
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETR4R4(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(4)          ,INTENT(OUT):: VAL(:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=R4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETR4R4
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETC8R4(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(8)          ,INTENT(OUT):: VAL(:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=C8TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(8)',16)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETC8R4
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETC4R4(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(4)          ,INTENT(OUT):: VAL(:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=C4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(4)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETC4R4
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETI4R4(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)          ,INTENT(OUT):: VAL(:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=I4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('I(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETI4R4
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETL4R4(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)          ,INTENT(OUT):: VAL(:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=L4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('L(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETL4R4
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETR8R5(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(8)          ,INTENT(OUT):: VAL(:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=R8TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(8)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETR8R5
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETR4R5(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(4)          ,INTENT(OUT):: VAL(:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=R4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETR4R5
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETC8R5(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(8)          ,INTENT(OUT):: VAL(:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=C8TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(8)',16)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETC8R5
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETC4R5(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(4)          ,INTENT(OUT):: VAL(:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=C4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(4)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETC4R5
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETI4R5(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)          ,INTENT(OUT):: VAL(:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=I4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('I(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETI4R5
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETL4R5(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)          ,INTENT(OUT):: VAL(:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=L4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('L(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETL4R5
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETR8R6(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(8)          ,INTENT(OUT):: VAL(:,:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=R8TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(8)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETR8R6
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETR4R6(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      REAL(4)          ,INTENT(OUT):: VAL(:,:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=R4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('R(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETR4R6
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETC8R6(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(8)          ,INTENT(OUT):: VAL(:,:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=C8TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(8)',16)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETC8R6
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETC4R6(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      COMPLEX(4)          ,INTENT(OUT):: VAL(:,:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=C4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('C(4)',8)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETC4R6
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETI4R6(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      INTEGER(4)          ,INTENT(OUT):: VAL(:,:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=I4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('I(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETI4R6
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETL4R6(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      LOGICAL(4)          ,INTENT(OUT):: VAL(:,:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
!     TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=L4TYPE
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('L(4)',4)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,TYPE%NAME,LENG,CHARVAL)
      VAL=RESHAPE(TRANSFER(CHARVAL,VAL),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETL4R6
!#END TEMPLATE LINKEDLIST__GETNUM
!
!#TEMPLATE LINKEDLIST__SETCHAR
!(<RANKID>,<SIZE>,<RESHAPE(><RESHAPE)><RANK>)
!              =([R0],[1],[],[],[])
!               ([R1],[SIZE(VAL)][RESHAPE(][,SHAPE(VAL))],[(:)])
!               ([R2],[SIZE(VAL)][RESHAPE(][,SHAPE(VAL))],[(:,:)])
!               ([R3],[SIZE(VAL)][RESHAPE(][,SHAPE(VAL))],[(:,:,:)])
!               ([R4],[SIZE(VAL)][RESHAPE(][,SHAPE(VAL))],[(:,:,:,:)])
!               ([R5],[SIZE(VAL)][RESHAPE(][,SHAPE(VAL))],[(:,:,:,:,:)])
!               ([R6],[SIZE(VAL)][RESHAPE(][,SHAPE(VAL))],[(:,:,:,:,:,:)])
!#BODY
!!
!!     ..................................................................
!      SUBROUTINE LINKEDLIST__SETCH<RANKID>(LL,ID,NTH,VAL)
!!     ******************************************************************
!!     **                                                              **
!!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!!     **                                                              **
!!     **  REMARKS:                                                    **
!!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!!     **                                                              **
!!     ******************************************************************
!      IMPLICIT NONE
!      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
!      CHARACTER(*)    ,INTENT(IN) :: ID
!      INTEGER(4)      ,INTENT(IN) :: NTH
!      CHARACTER(*)    ,INTENT(IN) :: VAL<RANK>
!      TYPE(LLIST_TYPE),POINTER    :: LIST
!      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
!      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
!      INTEGER(4)                  :: LENG
!      INTEGER(4)                  :: LENVAL
!      CHARACTER(8)                :: STRING
!!     ******************************************************************
!      LIST=>LL%PTR
!      LENG=<SIZE>
!      lenval=len(val)
!      WRITE(STRING,fmt='(i8)')LENVAL
!      STRING=ADJUSTL(STRING)
!      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
!      ALLOCATE(CHARVAL(LENG*LENVAL))
!!          == IN THE FOLLOWING LINE THE <RANK>(1:LENVAL) WAS REQUIRED
!!          == TO AVOID THAT A BLANK WAS INSERTED AFTER EACH ELEMENT OF VAL
!!     CHARVAL=TRANSFER(VAL<RANK>(1:LENVAL),CHARVAL)
!!LINUX PATCH IF
!!      CALL linkedlist_TRANSFERCHTO1(LENG,VAL,LENG*LENVAL,CHARVAL)
!!LINUX PATCH ELSE
!      CHARVAL=TRANSFER(VAL,CHARVAL)
!!LINUX PATCH END
!      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
!      RETURN
!      END SUBROUTINE LINKEDLIST__SETCH<RANKID>
!#INSTANCES
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETCHR0(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      CHARACTER(*)    ,INTENT(IN) :: VAL
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
      INTEGER(4)                  :: LENVAL
      CHARACTER(8)                :: STRING
!     ******************************************************************
      LIST=>LL%PTR
      LENG=1
      LENVAL=LEN(VAL)
      WRITE(STRING,FMT='(I8)')LENVAL
      STRING=ADJUSTL(STRING)
      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
      ALLOCATE(CHARVAL(LENG*LENVAL))
!          == IN THE FOLLOWING LINE THE (1:LENVAL) WAS REQUIRED
!          == TO AVOID THAT A BLANK WAS INSERTED AFTER EACH ELEMENT OF VAL
!     CHARVAL=TRANSFER(VAL(1:LENVAL),CHARVAL)
!LINUX PATCH IF
!      CALL LINKEDLIST_TRANSFERCHTO1(LENG,VAL,LENG*LENVAL,CHARVAL)
!LINUX PATCH ELSE
      CHARVAL=TRANSFER(VAL,CHARVAL)
!LINUX PATCH END
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETCHR0
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETCHR1(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      CHARACTER(*)    ,INTENT(IN) :: VAL(:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
      INTEGER(4)                  :: LENVAL
      CHARACTER(8)                :: STRING
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      LENVAL=LEN(VAL)
      WRITE(STRING,FMT='(I8)')LENVAL
      STRING=ADJUSTL(STRING)
      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
      ALLOCATE(CHARVAL(LENG*LENVAL))
!          == IN THE FOLLOWING LINE THE (:)(1:LENVAL) WAS REQUIRED
!          == TO AVOID THAT A BLANK WAS INSERTED AFTER EACH ELEMENT OF VAL
!     CHARVAL=TRANSFER(VAL(:)(1:LENVAL),CHARVAL)
!LINUX PATCH IF
!      CALL LINKEDLIST_TRANSFERCHTO1(LENG,VAL,LENG*LENVAL,CHARVAL)
!LINUX PATCH ELSE
      CHARVAL=TRANSFER(VAL,CHARVAL)
!LINUX PATCH END
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETCHR1
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETCHR2(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      CHARACTER(*)    ,INTENT(IN) :: VAL(:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
      INTEGER(4)                  :: LENVAL
      CHARACTER(8)                :: STRING
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      LENVAL=LEN(VAL)
      WRITE(STRING,FMT='(I8)')LENVAL
      STRING=ADJUSTL(STRING)
      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
      ALLOCATE(CHARVAL(LENG*LENVAL))
!          == IN THE FOLLOWING LINE THE (:,:)(1:LENVAL) WAS REQUIRED
!          == TO AVOID THAT A BLANK WAS INSERTED AFTER EACH ELEMENT OF VAL
!     CHARVAL=TRANSFER(VAL(:,:)(1:LENVAL),CHARVAL)
!LINUX PATCH IF
!      CALL LINKEDLIST_TRANSFERCHTO1(LENG,VAL,LENG*LENVAL,CHARVAL)
!LINUX PATCH ELSE
      CHARVAL=TRANSFER(VAL,CHARVAL)
!LINUX PATCH END
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETCHR2
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETCHR3(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      CHARACTER(*)    ,INTENT(IN) :: VAL(:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
      INTEGER(4)                  :: LENVAL
      CHARACTER(8)                :: STRING
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      LENVAL=LEN(VAL)
      WRITE(STRING,FMT='(I8)')LENVAL
      STRING=ADJUSTL(STRING)
      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
      ALLOCATE(CHARVAL(LENG*LENVAL))
!          == IN THE FOLLOWING LINE THE (:,:,:)(1:LENVAL) WAS REQUIRED
!          == TO AVOID THAT A BLANK WAS INSERTED AFTER EACH ELEMENT OF VAL
!     CHARVAL=TRANSFER(VAL(:,:,:)(1:LENVAL),CHARVAL)
!LINUX PATCH IF
!      CALL LINKEDLIST_TRANSFERCHTO1(LENG,VAL,LENG*LENVAL,CHARVAL)
!LINUX PATCH ELSE
      CHARVAL=TRANSFER(VAL,CHARVAL)
!LINUX PATCH END
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETCHR3
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETCHR4(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      CHARACTER(*)    ,INTENT(IN) :: VAL(:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
      INTEGER(4)                  :: LENVAL
      CHARACTER(8)                :: STRING
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      LENVAL=LEN(VAL)
      WRITE(STRING,FMT='(I8)')LENVAL
      STRING=ADJUSTL(STRING)
      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
      ALLOCATE(CHARVAL(LENG*LENVAL))
!          == IN THE FOLLOWING LINE THE (:,:,:,:)(1:LENVAL) WAS REQUIRED
!          == TO AVOID THAT A BLANK WAS INSERTED AFTER EACH ELEMENT OF VAL
!     CHARVAL=TRANSFER(VAL(:,:,:,:)(1:LENVAL),CHARVAL)
!LINUX PATCH IF
!      CALL LINKEDLIST_TRANSFERCHTO1(LENG,VAL,LENG*LENVAL,CHARVAL)
!LINUX PATCH ELSE
      CHARVAL=TRANSFER(VAL,CHARVAL)
!LINUX PATCH END
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETCHR4
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETCHR5(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      CHARACTER(*)    ,INTENT(IN) :: VAL(:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
      INTEGER(4)                  :: LENVAL
      CHARACTER(8)                :: STRING
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      LENVAL=LEN(VAL)
      WRITE(STRING,FMT='(I8)')LENVAL
      STRING=ADJUSTL(STRING)
      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
      ALLOCATE(CHARVAL(LENG*LENVAL))
!          == IN THE FOLLOWING LINE THE (:,:,:,:,:)(1:LENVAL) WAS REQUIRED
!          == TO AVOID THAT A BLANK WAS INSERTED AFTER EACH ELEMENT OF VAL
!     CHARVAL=TRANSFER(VAL(:,:,:,:,:)(1:LENVAL),CHARVAL)
!LINUX PATCH IF
!      CALL LINKEDLIST_TRANSFERCHTO1(LENG,VAL,LENG*LENVAL,CHARVAL)
!LINUX PATCH ELSE
      CHARVAL=TRANSFER(VAL,CHARVAL)
!LINUX PATCH END
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETCHR5
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__SETCHR6(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_SET.                      **
!     **  SEE LINKEDLIST_SETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      CHARACTER(*)    ,INTENT(IN) :: VAL(:,:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      INTEGER(4)                  :: LENG
      INTEGER(4)                  :: LENVAL
      CHARACTER(8)                :: STRING
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      LENVAL=LEN(VAL)
      WRITE(STRING,FMT='(I8)')LENVAL
      STRING=ADJUSTL(STRING)
      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
      ALLOCATE(CHARVAL(LENG*LENVAL))
!          == IN THE FOLLOWING LINE THE (:,:,:,:,:,:)(1:LENVAL) WAS REQUIRED
!          == TO AVOID THAT A BLANK WAS INSERTED AFTER EACH ELEMENT OF VAL
!     CHARVAL=TRANSFER(VAL(:,:,:,:,:,:)(1:LENVAL),CHARVAL)
!LINUX PATCH IF
!      CALL LINKEDLIST_TRANSFERCHTO1(LENG,VAL,LENG*LENVAL,CHARVAL)
!LINUX PATCH ELSE
      CHARVAL=TRANSFER(VAL,CHARVAL)
!LINUX PATCH END
      CALL LINKEDLIST_SETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
      RETURN
      END SUBROUTINE LINKEDLIST__SETCHR6
!#END TEMPLATE LINKEDLIST__SETCHAR
!
!#TEMPLATE LINKEDLIST__GETCHAR
!(<RANKID>,<SIZE>,<,SIZE>,<RESHAPE(><RESHAPE)><RANK>)
!    =([R0][1]        []          []        []            [])
!     ([R1][SIZE(VAL)][,SIZE(VAL)][RESHAPE(][,SHAPE(VAL))][(:)])
!     ([R2][SIZE(VAL)][,SIZE(VAL)][RESHAPE(][,SHAPE(VAL))][(:,:)])
!     ([R3][SIZE(VAL)][,SIZE(VAL)][RESHAPE(][,SHAPE(VAL))][(:,:,:)])
!     ([R4][SIZE(VAL)][,SIZE(VAL)][RESHAPE(][,SHAPE(VAL))][(:,:,:,:)])
!     ([R5][SIZE(VAL)][,SIZE(VAL)][RESHAPE(][,SHAPE(VAL))][(:,:,:,:,:)])
!     ([R6][SIZE(VAL)][,SIZE(VAL)][RESHAPE(][,SHAPE(VAL))][(:,:,:,:,:,:)])
!#BODY
!!
!!     ..................................................................
!      SUBROUTINE LINKEDLIST__GETCH<RANKID>(LL,ID,NTH,VAL)
!!     ******************************************************************
!!     **                                                              **
!!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!!     **                                                              **
!!     **  REMARKS:                                                    **
!!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!!     **                                                              **
!!     ******************************************************************
!      IMPLICIT NONE
!      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
!      CHARACTER(*)    ,INTENT(IN) :: ID
!      INTEGER(4)      ,INTENT(IN) :: NTH
!      CHARACTER(*)    ,INTENT(OUT):: VAL<RANK>
!      TYPE(LLIST_TYPE),POINTER    :: LIST
!      TYPE(LDATA_TYPE),POINTER    :: DATA
!      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
!      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
!      CHARACTER(8)                :: STRING
!      CHARACTER(500)              :: MOLD
!      INTEGER(4)                  :: LENG
!      INTEGER(4)                  :: I1,I2,KIND
!!     ******************************************************************
!      LIST=>LL%PTR
!      LENG=<SIZE>
!      WRITE(STRING,fmt='(i8)')LEN(VAL)
!      STRING=ADJUSTL(STRING)
!      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
!      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
!!     == CALCULATE LENGTH OF DATA RECEIVED ===========================
!      IF(NTH.EQ.0) THEN
!        CALL LLIST_FINDDATA(LIST,ID,1,DATA)
!      ELSE IF(NTH.GT.0) THEN
!        CALL LLIST_FINDDATA(LIST,ID,NTH,DATA)
!      ELSE
!        CALL ERROR__STOP('LINKEDLIST__GETCH<RANKID>')
!      END IF
!      STRING=DATA%TYPE
!      I1=INDEX(STRING,'(')
!      I2=INDEX(STRING,')')
!      READ(STRING(I1+1:I2-1),*)KIND
!!     == MAP STORED DATA ONTO VAL ====================================
!      VAL=<RESHAPE(>TRANSFER(CHARVAL,MOLD(1:KIND)<,SIZE>)<RESHAPE)>
!      RETURN
!      END SUBROUTINE LINKEDLIST__GETCH<RANKID>
!#INSTANCES
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETCHR0(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      CHARACTER(*)    ,INTENT(OUT):: VAL
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(LDATA_TYPE),POINTER    :: DATA
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      CHARACTER(8)                :: STRING
      CHARACTER(500)              :: MOLD
      INTEGER(4)                  :: LENG
      INTEGER(4)                  :: I1,I2,KIND
!     ******************************************************************
      LIST=>LL%PTR
      LENG=1
      WRITE(STRING,FMT='(I8)')LEN(VAL)
      STRING=ADJUSTL(STRING)
      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
!     == CALCULATE LENGTH OF DATA RECEIVED ===========================
      IF(NTH.EQ.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,1,DATA)
      ELSE IF(NTH.GT.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,NTH,DATA)
      ELSE
        CALL ERROR__STOP('LINKEDLIST__GETCHR0')
      END IF
      STRING=DATA%TYPE
      I1=INDEX(STRING,'(')
      I2=INDEX(STRING,')')
      READ(STRING(I1+1:I2-1),*)KIND
!     == MAP STORED DATA ONTO VAL ====================================
      VAL=TRANSFER(CHARVAL,MOLD(1:KIND))
      RETURN
      END SUBROUTINE LINKEDLIST__GETCHR0
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETCHR1(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      CHARACTER(*)    ,INTENT(OUT):: VAL(:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(LDATA_TYPE),POINTER    :: DATA
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      CHARACTER(8)                :: STRING
      CHARACTER(500)              :: MOLD
      INTEGER(4)                  :: LENG
      INTEGER(4)                  :: I1,I2,KIND
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      WRITE(STRING,FMT='(I8)')LEN(VAL)
      STRING=ADJUSTL(STRING)
      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
!     == CALCULATE LENGTH OF DATA RECEIVED ===========================
      IF(NTH.EQ.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,1,DATA)
      ELSE IF(NTH.GT.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,NTH,DATA)
      ELSE
        CALL ERROR__STOP('LINKEDLIST__GETCHR1')
      END IF
      STRING=DATA%TYPE
      I1=INDEX(STRING,'(')
      I2=INDEX(STRING,')')
      READ(STRING(I1+1:I2-1),*)KIND
!     == MAP STORED DATA ONTO VAL ====================================
      VAL=RESHAPE(TRANSFER(CHARVAL,MOLD(1:KIND),SIZE(VAL)),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETCHR1
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETCHR2(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      CHARACTER(*)    ,INTENT(OUT):: VAL(:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(LDATA_TYPE),POINTER    :: DATA
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      CHARACTER(8)                :: STRING
      CHARACTER(500)              :: MOLD
      INTEGER(4)                  :: LENG
      INTEGER(4)                  :: I1,I2,KIND
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      WRITE(STRING,FMT='(I8)')LEN(VAL)
      STRING=ADJUSTL(STRING)
      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
!     == CALCULATE LENGTH OF DATA RECEIVED ===========================
      IF(NTH.EQ.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,1,DATA)
      ELSE IF(NTH.GT.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,NTH,DATA)
      ELSE
        CALL ERROR__STOP('LINKEDLIST__GETCHR2')
      END IF
      STRING=DATA%TYPE
      I1=INDEX(STRING,'(')
      I2=INDEX(STRING,')')
      READ(STRING(I1+1:I2-1),*)KIND
!     == MAP STORED DATA ONTO VAL ====================================
      VAL=RESHAPE(TRANSFER(CHARVAL,MOLD(1:KIND),SIZE(VAL)),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETCHR2
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETCHR3(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      CHARACTER(*)    ,INTENT(OUT):: VAL(:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(LDATA_TYPE),POINTER    :: DATA
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      CHARACTER(8)                :: STRING
      CHARACTER(500)              :: MOLD
      INTEGER(4)                  :: LENG
      INTEGER(4)                  :: I1,I2,KIND
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      WRITE(STRING,FMT='(I8)')LEN(VAL)
      STRING=ADJUSTL(STRING)
      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
!     == CALCULATE LENGTH OF DATA RECEIVED ===========================
      IF(NTH.EQ.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,1,DATA)
      ELSE IF(NTH.GT.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,NTH,DATA)
      ELSE
        CALL ERROR__STOP('LINKEDLIST__GETCHR3')
      END IF
      STRING=DATA%TYPE
      I1=INDEX(STRING,'(')
      I2=INDEX(STRING,')')
      READ(STRING(I1+1:I2-1),*)KIND
!     == MAP STORED DATA ONTO VAL ====================================
      VAL=RESHAPE(TRANSFER(CHARVAL,MOLD(1:KIND),SIZE(VAL)),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETCHR3
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETCHR4(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      CHARACTER(*)    ,INTENT(OUT):: VAL(:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(LDATA_TYPE),POINTER    :: DATA
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      CHARACTER(8)                :: STRING
      CHARACTER(500)              :: MOLD
      INTEGER(4)                  :: LENG
      INTEGER(4)                  :: I1,I2,KIND
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      WRITE(STRING,FMT='(I8)')LEN(VAL)
      STRING=ADJUSTL(STRING)
      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
!     == CALCULATE LENGTH OF DATA RECEIVED ===========================
      IF(NTH.EQ.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,1,DATA)
      ELSE IF(NTH.GT.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,NTH,DATA)
      ELSE
        CALL ERROR__STOP('LINKEDLIST__GETCHR4')
      END IF
      STRING=DATA%TYPE
      I1=INDEX(STRING,'(')
      I2=INDEX(STRING,')')
      READ(STRING(I1+1:I2-1),*)KIND
!     == MAP STORED DATA ONTO VAL ====================================
      VAL=RESHAPE(TRANSFER(CHARVAL,MOLD(1:KIND),SIZE(VAL)),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETCHR4
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETCHR5(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      CHARACTER(*)    ,INTENT(OUT):: VAL(:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(LDATA_TYPE),POINTER    :: DATA
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      CHARACTER(8)                :: STRING
      CHARACTER(500)              :: MOLD
      INTEGER(4)                  :: LENG
      INTEGER(4)                  :: I1,I2,KIND
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      WRITE(STRING,FMT='(I8)')LEN(VAL)
      STRING=ADJUSTL(STRING)
      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
!     == CALCULATE LENGTH OF DATA RECEIVED ===========================
      IF(NTH.EQ.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,1,DATA)
      ELSE IF(NTH.GT.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,NTH,DATA)
      ELSE
        CALL ERROR__STOP('LINKEDLIST__GETCHR5')
      END IF
      STRING=DATA%TYPE
      I1=INDEX(STRING,'(')
      I2=INDEX(STRING,')')
      READ(STRING(I1+1:I2-1),*)KIND
!     == MAP STORED DATA ONTO VAL ====================================
      VAL=RESHAPE(TRANSFER(CHARVAL,MOLD(1:KIND),SIZE(VAL)),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETCHR5
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__GETCHR6(LL,ID,NTH,VAL)
!     ******************************************************************
!     **                                                              **
!     **  SPECIFIC INTERFACE FOR LINKEDLIST_GET.                      **
!     **  SEE LINKEDLIST_GETGENERIC FOR FURTHER INFO.                 **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    THIS SPECIFIC INTERFACE DIFFERS FROM OTHER SPECIFIC       **
!     **    INTERFACES BY THE TYPE OF VAL AND THE VALUE OF TYPE       **
!     **    AND VAL MAY BE ARRAY VALUED OR SCALAR                     **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH
      CHARACTER(*)    ,INTENT(OUT):: VAL(:,:,:,:,:,:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      TYPE(LDATA_TYPE),POINTER    :: DATA
      TYPE(TYPE_TYPE) ,PARAMETER  :: TYPE=TYPE_TYPE('CH',1)
      CHARACTER(1)    ,POINTER    :: CHARVAL(:)
      CHARACTER(8)                :: STRING
      CHARACTER(500)              :: MOLD
      INTEGER(4)                  :: LENG
      INTEGER(4)                  :: I1,I2,KIND
!     ******************************************************************
      LIST=>LL%PTR
      LENG=SIZE(VAL)
      WRITE(STRING,FMT='(I8)')LEN(VAL)
      STRING=ADJUSTL(STRING)
      STRING=TRIM(TYPE%NAME)//'('//TRIM(STRING)//')'
      CALL LINKEDLIST_GETGENERIC(LIST,ID,NTH,STRING,LENG,CHARVAL)
!     == CALCULATE LENGTH OF DATA RECEIVED ===========================
      IF(NTH.EQ.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,1,DATA)
      ELSE IF(NTH.GT.0) THEN
        CALL LLIST_FINDDATA(LIST,ID,NTH,DATA)
      ELSE
        CALL ERROR__STOP('LINKEDLIST__GETCHR6')
      END IF
      STRING=DATA%TYPE
      I1=INDEX(STRING,'(')
      I2=INDEX(STRING,')')
      READ(STRING(I1+1:I2-1),*)KIND
!     == MAP STORED DATA ONTO VAL ====================================
      VAL=RESHAPE(TRANSFER(CHARVAL,MOLD(1:KIND),SIZE(VAL)),SHAPE(VAL))
      RETURN
      END SUBROUTINE LINKEDLIST__GETCHR6
!#END TEMPLATE LINKEDLIST__GETCHAR
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__CONVERT(LL,ID,NTH_,TYPE)
!     ******************************************************************
!     **                                                              **
!     **  CREATES A HOME DIRECTORY FOR THE LIST                       **
!     **  IF LIST IS ASSOCIATED ALREADY, ITS CONNECTION IS BROKEN     **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      CHARACTER(*)    ,INTENT(IN) :: ID
      INTEGER(4)      ,INTENT(IN) :: NTH_
      CHARACTER(*)    ,INTENT(IN) :: TYPE
      CHARACTER(8)                :: OLDTYPE
      CHARACTER(1)    ,POINTER    :: OLDVAL(:)
      CHARACTER(1)    ,POINTER    :: NEWVAL(:)
      TYPE(LLIST_TYPE),POINTER    :: LIST
      INTEGER(4)                  :: LENG
      REAL(8)         ,ALLOCATABLE:: R8ARRAY(:)
      REAL(4)         ,ALLOCATABLE:: R4ARRAY(:)
      INTEGER(4)      ,ALLOCATABLE:: I4ARRAY(:)
      INTEGER(4)                  :: IERR
      INTEGER(4)                  :: NTH
      LOGICAL(4)                  :: UNDEF
!     ******************************************************************
      NULLIFY(LIST)
      LIST=>LL%PTR
      NTH=NTH_
      IF(NTH.EQ.0)NTH=1
      NULLIFY(OLDVAL)
      NULLIFY(NEWVAL)
      CALL LLIST_GETPTR(LIST,ID,NTH,OLDTYPE,LENG,OLDVAL)
      IF(TRIM(OLDTYPE).EQ.TRIM(TYPE)) RETURN
      UNDEF=.FALSE.
!
!     ==================================================================
!     ==  CONVERSION TO  REAL(8)                                      ==
!     ==================================================================
      IF(TYPE.EQ.'R(8)') THEN
        ALLOCATE(NEWVAL(R8TYPE%NBYTE*LENG))
        ALLOCATE(R8ARRAY(LENG))
        IF(OLDTYPE.EQ.R4TYPE%NAME) THEN
          R8ARRAY=REAL(TRANSFER(OLDVAL,R4ARRAY),KIND=8)
        ELSE IF(OLDTYPE.EQ.I4TYPE%NAME) THEN
          R8ARRAY=REAL(TRANSFER(OLDVAL,I4ARRAY),KIND=8)
        ELSE
          UNDEF=.TRUE.
        END IF
        NEWVAL=TRANSFER(R8ARRAY,NEWVAL)
        DEALLOCATE(R8ARRAY)
!
!     ==================================================================
!     ==  CONVERSION TO INTEGER(4)                                    ==
!     ==================================================================
      ELSE IF(TYPE.EQ.'I(4)') THEN
        ALLOCATE(NEWVAL(I4TYPE%NBYTE*LENG))
        ALLOCATE(I4ARRAY(LENG))
        IF(OLDTYPE.EQ.R8TYPE%NAME) THEN
          I4ARRAY=NINT(TRANSFER(OLDVAL,R8ARRAY))
        ELSE IF(OLDTYPE.EQ.R4TYPE%NAME) THEN
          I4ARRAY=NINT(TRANSFER(OLDVAL,R4ARRAY))
        ELSE
          UNDEF=.TRUE.
        END IF
        NEWVAL=TRANSFER(I4ARRAY,NEWVAL)
        DEALLOCATE(I4ARRAY)
      ELSE
        UNDEF=.TRUE.
      END IF
!
      IF(UNDEF) THEN
        CALL ERROR__MSG('THIS TYPE CONVERSION NOT ALLOWED')
        CALL ERROR__STOP('LINKEDLIST__CONVERT')
      END IF
!
!     ==================================================================
!     ==  REPLACE THE OLD DATA BY THE NEW ONE                         ==
!     ==================================================================
      CALL LLIST_SETPTR(LIST,ID,NTH,TYPE,LENG,NEWVAL,IERR)
      RETURN
      END SUBROUTINE LINKEDLIST__CONVERT
END MODULE LINKEDLIST_MODULE
!
!.......................................................................
MODULE BUFFER_MODULE
!***********************************************************************
!**                                                                   **
!**  NAME: BUFFER                                                     **
!**                                                                   **
!**  PURPOSE: READS A BLOCK STRUCTURE INTO A LINKED LIST              **
!**                                                                   **
!**  FUNCTIONS:                                                       **
!**    BUFFER__READ                                                    **
!**    BUFFER__WRITE                                                   **
!**                                                                   **
!**  DEPENDENCIES:                                                    **
!**    ERROR                                                          **
!**    LINKEDLIST                                                     **
!**    MPE                                                            **
!**                                                                   **
!***********************************************************************
TYPE BUFF_TYPE
  INTEGER(4)           :: FIRST
  INTEGER(4)           :: LAST
  INTEGER(4)           :: LENG
  CHARACTER(1),POINTER :: CH(:)
END TYPE BUFF_TYPE
!***********************************************************************
CONTAINS
!     ..................................................................
      SUBROUTINE BUFFER_NEW(BUFFER,LENG)
!     ******************************************************************
!     **                                                              **
!     **  ALLOCATE THE BUFFER ARRAY BUFFER%CH                         **
!     **                                                              **
!     ******************************************************************
      TYPE(BUFF_TYPE),INTENT(INOUT) :: BUFFER
      INTEGER(4)     ,INTENT(IN)    :: LENG
!     ******************************************************************
      ALLOCATE(BUFFER%CH(LENG))
      BUFFER%CH(:)=' '
      BUFFER%LENG=LENG
      BUFFER%FIRST=1
      BUFFER%LAST=0
      END SUBROUTINE BUFFER_NEW
!     ..................................................................
      SUBROUTINE BUFFER_DELETE(BUFFER)
!     ******************************************************************
!     **                                                              **
!     **  DEALLOCATE THE BUFFER ARRAY BUFFER%CH                       **
!     **                                                              **
!     ******************************************************************
      TYPE(BUFF_TYPE),INTENT(INOUT) :: BUFFER
!     ******************************************************************
      DEALLOCATE(BUFFER%CH)
      BUFFER%LENG=0
      BUFFER%FIRST=0
      BUFFER%LAST=0
      END SUBROUTINE BUFFER_DELETE
!
!     ..................................................................
      RECURSIVE SUBROUTINE BUFFER__WRITE(LIST_,NFIL)
!     ******************************************************************
!     **                                                              **
!     **  NAME: BUFFER__WRITE(LIST,NFIL)                               **
!     **                                                              **
!     ******************************************************************
      USE LINKEDLIST_MODULE
      IMPLICIT NONE
      INTEGER(4)   ,INTENT(IN) :: NFIL
      TYPE(LL_TYPE),INTENT(IN) :: LIST_
      TYPE(LL_TYPE)            :: LIST
      INTEGER(4)               :: NUM
      CHARACTER(32)            :: KEY
      CHARACTER(32)            :: TYPE
      INTEGER(4)               :: NTH,I,ISVAR,J
      INTEGER(4)               :: LENG
      REAL(8)      ,ALLOCATABLE:: R8ARRAY(:)
      REAL(4)      ,ALLOCATABLE:: R4ARRAY(:)
      COMPLEX(8)   ,ALLOCATABLE:: C8ARRAY(:)
      COMPLEX(4)   ,ALLOCATABLE:: C4ARRAY(:)
      INTEGER(4)   ,ALLOCATABLE:: I4ARRAY(:)
      LOGICAL(4)   ,ALLOCATABLE:: L4ARRAY(:)
      CHARACTER(256),ALLOCATABLE:: CHARRAY(:)
!     ******************************************************************
      LIST=LIST_
!
!     ==================================================================
!     == WRITE DATA TO FILE                                           ==
!     == REMARK: THE RECORDS MAY BECOME TOO LARGE FOR THE 256 CHAR    ==
!     ==  LIMIT OF BUFFER__READ                                        ==
!     == CHARACTERS AT THE END OF THE RECORD ARE BROKEN, WHICH IS     ==
!     == NOT HANDLED PROPERLY BY BUFFER__READ                          ==
!     == THE RECORDS SHOULD BE WRITTEN DENSER, AND TRAILING ZEROS     ==
!     == AND REDUNDANT EXPONENTS SHALL BE DROPPED                     ==
!     ==================================================================
      CALL LINKEDLIST__NDATA(LIST,'*',NUM)
      DO I=1,NUM
        CALL LINKEDLIST__DATAID(LIST,I,KEY,NTH)
        KEY=TRIM(KEY)//'='
        WRITE(NFIL,FMT='(A)')TRIM(KEY)
        CALL LINKEDLIST__TYPE(LIST,'*',I,TYPE)
        CALL LINKEDLIST__SIZE(LIST,'*',I,LENG)
        IF(TRIM(TYPE).EQ.'R(8)') THEN
          ALLOCATE(R8ARRAY(LENG))
          CALL LINKEDLIST__GET(LIST,'*',I,R8ARRAY)
          do j=1,leng
            WRITE(NFIL,*)R8ARRAY(j)
          enddo
          DEALLOCATE(R8ARRAY)
        ELSE IF(TRIM(TYPE).EQ.'R(4)') THEN
          ALLOCATE(R4ARRAY(LENG))
          CALL LINKEDLIST__GET(LIST,'*',I,R4ARRAY)
          do j=1,leng
            WRITE(NFIL,*)R4ARRAY(j)
          enddo
          DEALLOCATE(R4ARRAY)
        ELSE IF(TRIM(TYPE).EQ.'C(8)') THEN
          ALLOCATE(C8ARRAY(LENG))
          CALL LINKEDLIST__GET(LIST,'*',I,C8ARRAY)
          do j=1,leng
            WRITE(NFIL,*)C8ARRAY(j)
          enddo
          DEALLOCATE(C8ARRAY)
        ELSE IF(TRIM(TYPE).EQ.'C(4)') THEN
          ALLOCATE(C4ARRAY(LENG))
          CALL LINKEDLIST__GET(LIST,'*',I,C4ARRAY)
          do j=1,leng
            WRITE(NFIL,*)C4ARRAY(j)
          enddo
          DEALLOCATE(C4ARRAY)
        ELSE IF(TRIM(TYPE).EQ.'I(4)') THEN
          ALLOCATE(I4ARRAY(LENG))
          CALL LINKEDLIST__GET(LIST,'*',I,I4ARRAY)
          do j=1,leng
            WRITE(NFIL,*)I4ARRAY(j)
          enddo
          DEALLOCATE(I4ARRAY)
        ELSE IF(TRIM(TYPE).EQ.'L(4)') THEN
          ALLOCATE(L4ARRAY(LENG))
          CALL LINKEDLIST__GET(LIST,'*',I,L4ARRAY)
          do j=1,leng
            WRITE(NFIL,*)L4ARRAY(j)
          enddo
          DEALLOCATE(L4ARRAY)
        ELSE IF(TYPE(1:3).EQ.'CH(') THEN
          READ(TYPE(4:INDEX(TYPE,')')-1),*)ISVAR
          ALLOCATE(CHARRAY(LENG))
          CHARRAY(:)=' '
          CALL LINKEDLIST__GET(LIST,'*',I,CHARRAY(:)(1:ISVAR))
          DO J=1,LENG
            WRITE(NFIL,*)"'"//TRIM(CHARRAY(J))//"'"
          ENDDO
          DEALLOCATE(CHARRAY)
        ELSE
          CALL ERROR__MSG('TYPE NOT RECOGNIZED')
          CALL ERROR__CHVAL('TYPE',TYPE)
          CALL ERROR__STOP('BUFFER__WRITE')
        END IF
      ENDDO
!
!     ==================================================================
!     == WRITE LISTS TO FILE                                          ==
!     ==================================================================
      CALL LINKEDLIST__NLISTS(LIST,'*',NUM)
      DO I=1,NUM
        CALL LINKEDLIST__LISTID(LIST,I,KEY,NTH)
        KEY='!'//TRIM(KEY)
        CALL LINKEDLIST__SELECT(LIST,'*',I)
        WRITE(NFIL,FMT='(A)')TRIM(KEY)
        CALL BUFFER__WRITE(LIST,NFIL)
        WRITE(NFIL,FMT='("!END")')
        CALL LINKEDLIST__SELECT(LIST,'..',0)
      ENDDO
      RETURN
      END SUBROUTINE BUFFER__WRITE
!
!     ..................................................................
      SUBROUTINE BUFFER__READ(LIST_,NFIL)
!     ******************************************************************
!     **                                                              **
!     **  READS DATA IN A BLOCK STRUCTURE INTO THE LINKED LIST        **
!     **                                                              **
!     **  SYNTAX FOR BLOCK STRUCTURE                                  **
!     **  1) CHARACTER VALUES MUST NOT BE SPLIT ACCROSS LINES         **
!     **        (OTHERWISE ALL BLANKS WILL BE REMOVED                 **
!     **         AND ONE BLANK WILL BE INSERTED AT THE LINE BREAK)    **
!     **                                                              **
!     **                                                              **
!     ******************************************************************
      USE LINKEDLIST_MODULE
      USE STRINGS_MODULE
      IMPLICIT NONE
      INTEGER(4)    ,INTENT(IN) :: NFIL
      TYPE(LL_TYPE) ,INTENT(IN) :: LIST_
      TYPE(LL_TYPE)             :: LIST
      TYPE(BUFF_TYPE)           :: BUFFER
      INTEGER(4)    ,PARAMETER  :: BUFFERSIZE=100000
      LOGICAL(4)                :: TDATA
      CHARACTER(32)             :: KEY
      INTEGER(4)                :: KEYL
      INTEGER(4)                :: I1,I2
      CHARACTER(32)             :: TYPE        ! TYPE SPECIFIER OF DATA
      INTEGER(4)                :: LENG        !SIZE OF TEMPARRAY
      INTEGER(4)                :: ISVAR,I
      INTEGER(4)                :: LEVEL
      COMPLEX(8)    ,ALLOCATABLE:: C8ARRAY(:)
      REAL(8)       ,ALLOCATABLE:: R8ARRAY(:)
      INTEGER(4)    ,ALLOCATABLE:: I4ARRAY(:)
      LOGICAL(4)    ,ALLOCATABLE:: L4ARRAY(:)
      CHARACTER(256),ALLOCATABLE:: CHARRAY(:)
!     ******************************************************************
      LIST=LIST_
      KEY=' '
      KEYL=0
!
!     ==================================================================
!     == INITIALIZE AND FILL BUFFER                                   ==
!     ==================================================================
      CALL BUFFER_NEW(BUFFER,BUFFERSIZE)
      CALL BUFFER_READTOBUFFER(NFIL,BUFFER)
!
!     ==================================================================
!     ==  RESOLVE BUFFER AND MAP TO LINKEDLIST                        ==
!     ==  LOOP IS TERMINATED BY !EOB                                  ==
!     ==================================================================
      LEVEL=0
      TDATA=.FALSE.
      DO
!       ================================================================
!       ==  HANDLE KEY WORDS                                          ==
!       ================================================================
        IF(.NOT.TDATA) THEN
!         == COLLECT KEY WORD
          I1=BUFFER%FIRST
          CALL BUFFER_NEXTWORD(BUFFER,I1,I2)
!         == REFRESH BUFFER
          IF(I1.EQ.0) THEN
            CALL BUFFER_READTOBUFFER(NFIL,BUFFER)
            I1=BUFFER%FIRST
            CALL BUFFER_NEXTWORD(BUFFER,I1,I2)
            IF(I1.EQ.0) THEN
              CALL ERROR__MSG('DATA DOES NOT FIT BUFFER')
              CALL ERROR__MSG('OR BUFFER STRUCTURE CORRUPTED')
              CALL ERROR__STOP('BUFFER__READ')
            ENDIF
          END IF
!         == COPY TO STRING
          CALL BUFFER_TOSTRING(BUFFER,I1,I2,KEY)
          KEY=+KEY
!         == REMOVE FROM BUFFER
          BUFFER%FIRST=I2+1
!
!         ==============================================================
!         == NOW CHANGE LINKEDLIST =====================================
!         ==============================================================
          KEYL=LEN_TRIM(KEY)
          IF(+KEY(1:KEYL).EQ.'!EOB') THEN
            IF(LEVEL.NE.0) THEN
              CALL ERROR__MSG('BLOCK STRUCTURE INCORRECT')
              CALL ERROR__STOP('BUFFER__READ')
            END IF
            EXIT
          ELSE IF(+KEY(1:KEYL).EQ.'!END') THEN
            LEVEL=LEVEL-1
            IF(LEVEL.LT.0) THEN
              CALL ERROR__MSG('BLOCK STRUCTURE INCORRECT')
              CALL ERROR__STOP('BUFFER__READ')
            END IF
            CALL LINKEDLIST__SELECT(LIST,'..')
          ELSE IF(KEY(1:1).EQ.'!') THEN
            CALL LINKEDLIST__SELECT(LIST,TRIM(KEY(2:)),-1)
            LEVEL=LEVEL+1
          ELSE IF(KEY(KEYL:KEYL).EQ.'=') THEN
            TDATA=.TRUE.
            KEY(KEYL:KEYL)=' '
          ELSE
            CALL ERROR__MSG('KEYWORD MUST EITHER BEGIN WITH ! OR END WITH =')
            CALL ERROR__STOP('BUFFER__READ')
          END IF
!
!       ================================================================
!       ==  READ OUT DATA                                             ==
!       ================================================================
        ELSE
          CALL BUFFER_TYPESIZE(BUFFER,TYPE,LENG)
          IF(LENG.EQ.0) THEN
            CALL BUFFER_READTOBUFFER(NFIL,BUFFER)
            CALL BUFFER_TYPESIZE(BUFFER,TYPE,LENG)
            IF(LENG.EQ.0) THEN
              CALL ERROR__MSG('DATA DOES NOT FIT BUFFER')
              CALL ERROR__MSG('OR BUFFER STRUCTURE CORRUPTED')
              CALL ERROR__STOP('BUFFER__READ')
            ENDIF
          END IF
          CALL BUFFER_TYPESIZE(BUFFER,TYPE,LENG)
!         ==============================================================
          IF(TRIM(TYPE).EQ.'REAL') THEN
            ALLOCATE(R8ARRAY(LENG))
            CALL BUFFER_READR8(BUFFER,R8ARRAY)
            CALL LINKEDLIST__SET(LIST,TRIM(KEY),-1,R8ARRAY)
            DEALLOCATE(R8ARRAY)
          ELSE IF(TRIM(TYPE).EQ.'COMPLEX') THEN
            ALLOCATE(C8ARRAY(LENG))
            CALL BUFFER_READC8(BUFFER,C8ARRAY)
            CALL LINKEDLIST__SET(LIST,TRIM(KEY),-1,C8ARRAY)
            DEALLOCATE(C8ARRAY)
          ELSE IF(TRIM(TYPE).EQ.'INTEGER') THEN
            ALLOCATE(I4ARRAY(LENG))
            CALL BUFFER_READI4(BUFFER,I4ARRAY)
            CALL LINKEDLIST__SET(LIST,TRIM(KEY),-1,I4ARRAY)
            DEALLOCATE(I4ARRAY)
          ELSE IF(TRIM(TYPE).EQ.'LOGICAL') THEN
            ALLOCATE(L4ARRAY(LENG))
            CALL BUFFER_READL4(BUFFER,L4ARRAY)
            CALL LINKEDLIST__SET(LIST,TRIM(KEY),-1,L4ARRAY)
            DEALLOCATE(L4ARRAY)
          ELSE IF(TRIM(TYPE).EQ.'CHARACTER') THEN
            ALLOCATE(CHARRAY(LENG))
            CALL BUFFER_READCH(BUFFER,CHARRAY)
            ISVAR=0
            DO I=1,LENG
              ISVAR=MAX(ISVAR,LEN_TRIM(CHARRAY(I)))
            ENDDO
            CALL LINKEDLIST__SET(LIST,TRIM(KEY),-1,CHARRAY(:)(1:ISVAR))
            DEALLOCATE(CHARRAY)
          ELSE
            CALL ERROR__MSG('TYPE NOT RECOGNIZED')
            CALL ERROR__STOP('BUFFER__READ')
          END IF
          TDATA=.FALSE.
        END IF
      ENDDO
      CALL BUFFER_DELETE(BUFFER)
      RETURN
      END SUBROUTINE BUFFER__READ
!
!     ..................................................................
      SUBROUTINE BUFFER_READTOBUFFER(NFIL,BUFFER)
!     ******************************************************************
!     **                                                              **
!     **  NAME: BUFFER_READTOBUFFER(NFIL,BUFFER)                      **
!     **                                                              **
!     **  PURPOSE: READS FROM FILE INTO BUFFER UNTIL THE BUFFER       **
!     **    IS EITHER FULL OR A '!EOB' IS ENCOUNTERED                 **
!     **                                                              **
!     ******************************************************************
      USE MPE_MODULE
      USE STRINGS_MODULE
      IMPLICIT NONE
      INTEGER(4)     ,INTENT(IN)    :: NFIL
      TYPE(BUFF_TYPE),INTENT(INOUT) :: BUFFER
      INTEGER(4)     ,PARAMETER     :: LENGX=1024*32
      LOGICAL(4)                    :: TEOB
      LOGICAL(4)                    :: PACKED  !INSIGNIFICANT BLANKS REMOVED
      CHARACTER(LENGX)              :: LINE
      INTEGER(4)                    :: LENG
      INTEGER(4)                    :: FREE
      INTEGER(4)                    :: THISTASK,NTASKS
      CHARACTER                     :: STCHAR
      INTEGER(4)                    :: IOS
      INTEGER(4)                    :: ISVAR,I
!     ******************************************************************
      CALL MPE__QUERY(NTASKS,THISTASK)
      CALL BUFFER_PACK(BUFFER)
      FREE=BUFFER%LENG-BUFFER%LAST+1
!
!     ==================================================================
!     ==                                                              ==
!     ==================================================================
      I=MAX(1,BUFFER%LAST-LENGX)
      LINE=' '
      CALL BUFFER_TOSTRING(BUFFER,I,BUFFER%LAST,LINE)
      TEOB=(INDEX(+LINE,'!EOB').NE.0)
!
!     ==================================================================
!     ==                                                              ==
!     ==================================================================
      DO WHILE((FREE.GT.LENGX).AND.(.NOT.TEOB))
!
!       ================================================================
!       ==  READ A LINE AND COMMUNICATE                               ==
!       ================================================================
        LINE=' '
        IF(THISTASK.EQ.1) THEN
          READ(NFIL,FMT='(A)',ERR=9999,IOSTAT=IOS,END=200)LINE
        END IF
 200    CONTINUE
        CALL MPE__BROADCAST(1,LINE)
        LINE=ADJUSTL(LINE)
        LENG=LEN_TRIM(LINE)
!       CALL MPE__BCAST(LINE,LENG,1)
!
!       ================================================================
!       ==  CHECK HERE FOR THE PRESENCE OF TABS ETC                   ==
!       ================================================================
        DO I=1,LENG
          STCHAR=LINE(I:I)
          ISVAR=IACHAR(STCHAR)
          IF(ISVAR.LT.32) THEN
            CALL ERROR__MSG('TABS AND OTHER SPECIAL LETTERS ARE NOT ALLOWED')
            CALL ERROR__MSG('IN BUFFER STRUCTURED INPUT FILES')
            CALL ERROR__CHVAL('LINE',TRIM(LINE))
            CALL ERROR__CHVAL('POSITION',TRIM(LINE))
            CALL ERROR__I4VAL('ASCII VALUE OF DISALLOWED CHARACTER',ISVAR)
            CALL ERROR__STOP('BUFFER_READ')
          END IF
        ENDDO
!
!       ================================================================
!       ==  INTRODUCE A BLANK FOR NEWLINE AND ADD LINE TO BUFFER      ==
!       ================================================================
!       __COPY LINE TO BUFFER
        DO I=1,LENG
          BUFFER%LAST=BUFFER%LAST+1
          BUFFER%CH(BUFFER%LAST)=LINE(I:I)
        ENDDO
        PACKED=.FALSE.
!       __INTRODUCE ONE BLANK FOR EACH LINE BREAK
        IF(BUFFER%LAST.NE.0) THEN
          BUFFER%LAST=BUFFER%LAST+1
          BUFFER%CH(BUFFER%LAST)=' '
        END IF
!
!       ================================================================
!       ==  RECALCULATE FREE                                          ==
!       ================================================================
        TEOB=(INDEX(+LINE(1:LENG),'!EOB').NE.0)
        FREE=BUFFER%LENG-BUFFER%LAST+1
        IF(FREE.LT.LENGX) THEN
          CALL BUFFER_PACK(BUFFER)
          FREE=BUFFER%LENG-BUFFER%LAST+1
          PACKED=.TRUE.
        END IF
      ENDDO
      IF(.NOT.PACKED) CALL BUFFER_PACK(BUFFER)
      RETURN
 9999 CONTINUE
      CALL ERROR__I4VAL('IOS',IOS)
      CALL ERROR__STOP('BUFFER_READ')
      RETURN
      END SUBROUTINE BUFFER_READTOBUFFER
!
!     ..................................................................
      SUBROUTINE BUFFER_PACK(BUFFER)
!     ******************************************************************
!     **                                                              **
!     **  BUFFER_PACK(BUFFER)                                         **
!     **                                                              **
!     **  REMOVES INSIGNIFICANT PORTIONS OF THE BUFFER ARRAY          **
!     **                                                              **
!     **  REMARKS:                                                    **
!     **    1) EVERYTHING BEFORE BUFFER%FIRST IS REMOVED AND          **
!     **       BUFFER%FIRST IS SET TO ONE                             **
!     **    2) MULTIPLE BLANKS OUTSIDE CHARACTER CONTEXT ARE          **
!     **       COLLAPSED TO SINGLE BLANKS                             **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(BUFF_TYPE),INTENT(INOUT) :: BUFFER
      INTEGER(4)                    :: ICHAR
      INTEGER(4)                    :: IBLANK
      INTEGER(4)                    :: I1,I2
      CHARACTER(1)                  :: CHAR
!     ******************************************************************
!
!     ==================================================================
!     ==  CONTRACT MULTIPLE BLANKS SEPARATING KEY WORDS AND DATA      ==
!     ==================================================================
      ICHAR=0
      IBLANK=2
      I2=0
      DO I1=BUFFER%FIRST,BUFFER%LAST
        CHAR=BUFFER%CH(I1)
!       ================================================================
!       ==  COUNT SIGNIFICANT BLANKS                                  ==
!       ================================================================
        IF(CHAR.EQ.' '.AND.ICHAR.EQ.0)THEN
          IBLANK=IBLANK+1
        ELSE
          IBLANK=0
        END IF
!       ================================================================
!       ==  SWITCH FOR CHARACTER CONTEXT                              ==
!       ================================================================
        IF(ICHAR.EQ.0) THEN
          IF(CHAR.EQ."'") THEN
            ICHAR=1
          ELSE IF(CHAR.EQ.'"') THEN
            ICHAR=2
          END IF
        ELSE
          IF(ICHAR.EQ.1.AND.CHAR.EQ."'") THEN
            ICHAR=0
          ELSE IF(ICHAR.EQ.2.AND.CHAR.EQ.'"') THEN
            ICHAR=0
          END IF
        END IF
!       ================================================================
!       ==  REMOVE INSIGNIFICANT BLANKS                               ==
!       ================================================================
        IF(IBLANK.LE.1) THEN
          I2=I2+1
          BUFFER%CH(I2)=CHAR
        END IF
      ENDDO
      BUFFER%CH(I2+1:BUFFER%LAST)=' '
      BUFFER%FIRST=1
      BUFFER%LAST=I2
      RETURN
      END SUBROUTINE BUFFER_PACK
!
!     ..................................................................
      SUBROUTINE BUFFER_TYPESIZE(BUFFER,TYPE,NUM)
!     ******************************************************************
!     **                                                              **
!     **  NAME: BUFFER_TYPESIZE(BUFFER,TYPE,NUM)                      **
!     **                                                              **
!     **  PURPOSE: DETERMINES TYPE AND SIZE OF A DATA ARRAY           **
!     **    ON THE BUFFER ARRAY.                                      **
!     **                                                              **
!     **  REMARK: TYPE CAN BE 'COMPLEX','REAL','INTEGER','LOGICAL'    **
!     **    AND 'CHARACTER'                                           **
!     **                                                              **
!     ******************************************************************
      USE STRINGS_MODULE
      TYPE(BUFF_TYPE),INTENT(INOUT):: BUFFER
      CHARACTER(*)   ,INTENT(OUT)  :: TYPE
      INTEGER(4)     ,INTENT(OUT)  :: NUM
      CHARACTER(128)               :: DATUM
      INTEGER(4)                   :: I1,I2
      INTEGER(4)                   :: ISVAR,II
!     ******************************************************************
      I1=BUFFER%FIRST
      TYPE='NONE'
      NUM=0
      DO
        CALL BUFFER_NEXTWORD(BUFFER,I1,I2)
!
!       ================================================================
!       == TERMINATE LOOP IF KEY WORD ENCOUNTERED OR NO NEXT WORD     ==
!       ================================================================
        IF(I1.EQ.0) THEN
          TYPE='NONE'
          NUM=0
          EXIT
        END IF
        IF((BUFFER%CH(I2).EQ.'=').OR.BUFFER%CH(I1).EQ.'!') EXIT
        DATUM=' '
        CALL BUFFER_TOSTRING(BUFFER,I1,I2,DATUM)
        DATUM=+DATUM
        I1=I2+1
!
!       ================================================================
!       == DETERMINE NUMBER OF ELEMENTS                               ==
!       ================================================================
        ISVAR=INDEX(DATUM,'*')
        IF(ISVAR.NE.0) THEN
          IF(INDEX(DATUM(1:ISVAR),'"').NE.0) ISVAR=0
        END IF
        IF(ISVAR.NE.0) THEN
          IF(INDEX(DATUM(1:ISVAR),"'").NE.0) ISVAR=0
        END IF
        IF(ISVAR.NE.0) THEN
          READ(DATUM(1:ISVAR-1),*)II
          DATUM=DATUM(ISVAR+1:)
        ELSE
          II=1
        END IF
        NUM=NUM+II
!
!       ================================================================
!       == DETERMINE CHARACTER TYPE                                   ==
!       ================================================================
        IF(INDEX(DATUM,'"')+INDEX(DATUM,"'").NE.0) THEN
          IF((TRIM(TYPE).EQ.'NONE').OR.(TRIM(TYPE).EQ.'CHARACTER')) THEN
            TYPE='CHARACTER'
          ELSE
            CALL ERROR__MSG('AN ARRAY DATA ITEM MUST BE OF ONE TYPE')
            CALL ERROR__CHVAL('PREVIOUS TYPE',TRIM(TYPE))
            CALL ERROR__CHVAL('TYPE ENCOUNTERED','CHARACTER')
            CALL ERROR__CHVAL('DATA',DATUM)
            CALL ERROR__STOP('BUFFER_TYPESIZE')
          END IF
!
!       ================================================================
!       == DETERMINE LOGICAL TYPE                                     ==
!       ================================================================
        ELSE IF((TRIM(DATUM).EQ.'T').OR.(TRIM(DATUM).EQ.'F') &
       &  .OR.(TRIM(DATUM).EQ.'.TRUE.').OR.(TRIM(DATUM).EQ.'.FALSE.')) THEN
          IF((TRIM(TYPE).EQ.'NONE').OR.(TRIM(TYPE).EQ.'LOGICAL')) THEN
            TYPE='LOGICAL'
          ELSE
            CALL ERROR__MSG('AN ARRAY DATA ITEM MUST BE OF ONE TYPE')
            CALL ERROR__CHVAL('PREVIOUS TYPE',TRIM(TYPE))
            CALL ERROR__CHVAL('TYPE ENCOUNTERED','LOGICAL')
            CALL ERROR__CHVAL('DATA',DATUM)
            CALL ERROR__STOP('BUFFER_TYPESIZE')
          END IF
!
!       ================================================================
!       == DETERMINE COMPLEX TYPE                                     ==
!       ================================================================
        ELSE IF(INDEX(DATUM,'(').NE.0) THEN
          IF((TRIM(TYPE).EQ.'NONE').OR.(TRIM(TYPE).EQ.'COMPLEX')) THEN
            TYPE='COMPLEX'
          ELSE
            CALL ERROR__MSG('AN ARRAY DATA ITEM MUST BE OF ONE TYPE')
            CALL ERROR__CHVAL('PREVIOUS TYPE',TRIM(TYPE))
            CALL ERROR__CHVAL('TYPE ENCOUNTERED','COMPLEX')
            CALL ERROR__CHVAL('DATA',DATUM)
            CALL ERROR__STOP('BUFFER_TYPESIZE')
          END IF
!
!       ================================================================
!       == DETERMINE REAL TYPE                                        ==
!       ================================================================
        ELSE IF(INDEX(DATUM,'.').NE.0) THEN
          IF((TRIM(TYPE).EQ.'NONE').OR.(TRIM(TYPE).EQ.'REAL') &
       &                           .OR.(TRIM(TYPE).EQ.'INTEGER')) THEN
            TYPE='REAL'
          ELSE
            CALL ERROR__MSG('AN ARRAY DATA ITEM MUST BE OF ONE TYPE')
            CALL ERROR__CHVAL('PREVIOUS TYPE',TRIM(TYPE))
            CALL ERROR__CHVAL('TYPE ENCOUNTERED','REAL')
            CALL ERROR__CHVAL('DATA',DATUM)
            CALL ERROR__STOP('BUFFER_TYPESIZE')
          END IF
!
!       ================================================================
!       == DETERMINE INTEGER TYPE                                     ==
!       ================================================================
        ELSE
          IF((TRIM(TYPE).EQ.'NONE').OR.(TRIM(TYPE).EQ.'INTEGER')) THEN
            TYPE='INTEGER'
          ELSE
            CALL ERROR__MSG('AN ARRAY DATA ITEM MUST BE OF ONE TYPE')
            CALL ERROR__CHVAL('PREVIOUS TYPE',TRIM(TYPE))
            CALL ERROR__CHVAL('TYPE ENCOUNTERED','INTEGER')
            CALL ERROR__CHVAL('DATA',DATUM)
            CALL ERROR__STOP('BUFFER_TYPESIZE')
          END IF
        END IF
      ENDDO
      RETURN
      END SUBROUTINE BUFFER_TYPESIZE
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!#TEMPLATE BUFFER_READXX
!(<TYPEID><TYPE>)=([R8][REAL(8)])
!                 ([C8][COMPLEX(8)])
!                 ([I4][INTEGER(4)])
!                 ([L4][LOGICAL(4)])
!                 ([CH][CHARACTER(*)])
!#BODY
!!
!!     ..................................................................
!      SUBROUTINE BUFFER_READ<TYPEID>(BUFFER,VAL)
!!     ******************************************************************
!!     **                                                              **
!!     **  NAME: BUFFER_READ<TYPEID>(BUFFER,VAL)                       **
!!     **                                                              **
!!     **  PURPOSE: READS AN ARRAY OF TYPE <TYPEID> FROM THE           **
!!     **    BUFFER ARRAY.                                             **
!!     **                                                              **
!!     **    THE DATA ARE READ BEGINNING WITH BUFFER%FIRST             **
!!     **    AND READING IS ENDED BY THE NEXT KEY WORD !XXX OR XXX=    **
!!     **                                                              **
!!     **  REMARK: THIS ROUTINE IS IMPLEMENTED AS A TEMPLATE FOR       **
!!     **    TYPES C8,R8,I4,L4,CH                                      **
!!     **                                                              **
!!     ******************************************************************
!      IMPLICIT NONE
!      TYPE(BUFF_TYPE),INTENT(INOUT):: BUFFER
!      <TYPE>         ,INTENT(OUT)  :: VAL(:)
!      CHARACTER(128)               :: DATUM
!      INTEGER(4)                   :: I1,I2
!      INTEGER(4)                   :: ISVAR,II,IDATA
!      INTEGER(4)                   :: LENG
!      INTEGER(4)                   :: I
!!     ******************************************************************
!      I1=BUFFER%FIRST
!      LENG=SIZE(VAL)
!      IDATA=0
!      DO
!        CALL BUFFER_NEXTWORD(BUFFER,I1,I2)
!!
!!       ================================================================
!!       == TERMINATE LOOP IF KEY WORD ENCOUNTERED OR NO NEXT WORD     ==
!!       ================================================================
!        IF(I1.EQ.0) THEN
!          CALL ERROR__STOP('BUFFER_READ<TYPEID>')
!        END IF
!        IF((BUFFER%CH(I2).EQ.'=').OR.(BUFFER%CH(I1).EQ.'!')) THEN
!          BUFFER%FIRST=I1
!          EXIT
!        END IF
!!
!        DATUM=' '
!        CALL BUFFER_TOSTRING(BUFFER,I1,I2,DATUM)
!        I1=I2+1
!!
!!       ================================================================
!!       == DETERMINE NUMBER OF ELEMENTS                               ==
!!       ================================================================
!        ISVAR=INDEX(DATUM,'*')
!        IF(ISVAR.NE.0) THEN
!          IF(INDEX(DATUM(1:ISVAR),'"').NE.0) ISVAR=0
!        END IF
!        IF(ISVAR.NE.0) THEN
!          IF(INDEX(DATUM(1:ISVAR),"'").NE.0) ISVAR=0
!        END IF
!        IF(ISVAR.NE.0) THEN
!          READ(DATUM(1:ISVAR-1),*)II
!          DATUM=DATUM(ISVAR+1:)
!        ELSE
!          II=1
!        END IF
!        IF(IDATA+II.GT.LENG) THEN
!          CALL ERROR__MSG('MORE DATA THAN ARRAY CAN HOLD')
!          CALL ERROR__STOP('BUFFER_READ<TYPEID>')
!        END IF
!!
!!       ================================================================
!!       == READ                                                       ==
!!       ================================================================
!        DO I=1,II
!          IDATA=IDATA+1
!          READ(DATUM,*)VAL(IDATA)
!        ENDDO
!      ENDDO
!      RETURN
!      END SUBROUTINE BUFFER_READ<TYPEID>
!#INSTANCES
!
!     ..................................................................
      SUBROUTINE BUFFER_READR8(BUFFER,VAL)
!     ******************************************************************
!     **                                                              **
!     **  NAME: BUFFER_READR8(BUFFER,VAL)                       **
!     **                                                              **
!     **  PURPOSE: READS AN ARRAY OF TYPE R8 FROM THE           **
!     **    BUFFER ARRAY.                                             **
!     **                                                              **
!     **    THE DATA ARE READ BEGINNING WITH BUFFER%FIRST             **
!     **    AND READING IS ENDED BY THE NEXT KEY WORD !XXX OR XXX=    **
!     **                                                              **
!     **  REMARK: THIS ROUTINE IS IMPLEMENTED AS A TEMPLATE FOR       **
!     **    TYPES C8,R8,I4,L4,CH                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(BUFF_TYPE),INTENT(INOUT):: BUFFER
      REAL(8)         ,INTENT(OUT)  :: VAL(:)
      CHARACTER(128)               :: DATUM
      INTEGER(4)                   :: I1,I2
      INTEGER(4)                   :: ISVAR,II,IDATA
      INTEGER(4)                   :: LENG
      INTEGER(4)                   :: I
!     ******************************************************************
      I1=BUFFER%FIRST
      LENG=SIZE(VAL)
      IDATA=0
      DO
        CALL BUFFER_NEXTWORD(BUFFER,I1,I2)
!
!       ================================================================
!       == TERMINATE LOOP IF KEY WORD ENCOUNTERED OR NO NEXT WORD     ==
!       ================================================================
        IF(I1.EQ.0) THEN
          CALL ERROR__STOP('BUFFER_READR8')
        END IF
        IF((BUFFER%CH(I2).EQ.'=').OR.(BUFFER%CH(I1).EQ.'!')) THEN
          BUFFER%FIRST=I1
          EXIT
        END IF
!
        DATUM=' '
        CALL BUFFER_TOSTRING(BUFFER,I1,I2,DATUM)
        I1=I2+1
!
!       ================================================================
!       == DETERMINE NUMBER OF ELEMENTS                               ==
!       ================================================================
        ISVAR=INDEX(DATUM,'*')
        IF(ISVAR.NE.0) THEN
          IF(INDEX(DATUM(1:ISVAR),'"').NE.0) ISVAR=0
        END IF
        IF(ISVAR.NE.0) THEN
          IF(INDEX(DATUM(1:ISVAR),"'").NE.0) ISVAR=0
        END IF
        IF(ISVAR.NE.0) THEN
          READ(DATUM(1:ISVAR-1),*)II
          DATUM=DATUM(ISVAR+1:)
        ELSE
          II=1
        END IF
        IF(IDATA+II.GT.LENG) THEN
          CALL ERROR__MSG('MORE DATA THAN ARRAY CAN HOLD')
          CALL ERROR__STOP('BUFFER_READR8')
        END IF
!
!       ================================================================
!       == READ                                                       ==
!       ================================================================
        DO I=1,II
          IDATA=IDATA+1
          READ(DATUM,*)VAL(IDATA)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE BUFFER_READR8
!
!     ..................................................................
      SUBROUTINE BUFFER_READC8(BUFFER,VAL)
!     ******************************************************************
!     **                                                              **
!     **  NAME: BUFFER_READC8(BUFFER,VAL)                       **
!     **                                                              **
!     **  PURPOSE: READS AN ARRAY OF TYPE C8 FROM THE           **
!     **    BUFFER ARRAY.                                             **
!     **                                                              **
!     **    THE DATA ARE READ BEGINNING WITH BUFFER%FIRST             **
!     **    AND READING IS ENDED BY THE NEXT KEY WORD !XXX OR XXX=    **
!     **                                                              **
!     **  REMARK: THIS ROUTINE IS IMPLEMENTED AS A TEMPLATE FOR       **
!     **    TYPES C8,R8,I4,L4,CH                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(BUFF_TYPE),INTENT(INOUT):: BUFFER
      COMPLEX(8)         ,INTENT(OUT)  :: VAL(:)
      CHARACTER(128)               :: DATUM
      INTEGER(4)                   :: I1,I2
      INTEGER(4)                   :: ISVAR,II,IDATA
      INTEGER(4)                   :: LENG
      INTEGER(4)                   :: I
!     ******************************************************************
      I1=BUFFER%FIRST
      LENG=SIZE(VAL)
      IDATA=0
      DO
        CALL BUFFER_NEXTWORD(BUFFER,I1,I2)
!
!       ================================================================
!       == TERMINATE LOOP IF KEY WORD ENCOUNTERED OR NO NEXT WORD     ==
!       ================================================================
        IF(I1.EQ.0) THEN
          CALL ERROR__STOP('BUFFER_READC8')
        END IF
        IF((BUFFER%CH(I2).EQ.'=').OR.(BUFFER%CH(I1).EQ.'!')) THEN
          BUFFER%FIRST=I1
          EXIT
        END IF
!
        DATUM=' '
        CALL BUFFER_TOSTRING(BUFFER,I1,I2,DATUM)
        I1=I2+1
!
!       ================================================================
!       == DETERMINE NUMBER OF ELEMENTS                               ==
!       ================================================================
        ISVAR=INDEX(DATUM,'*')
        IF(ISVAR.NE.0) THEN
          IF(INDEX(DATUM(1:ISVAR),'"').NE.0) ISVAR=0
        END IF
        IF(ISVAR.NE.0) THEN
          IF(INDEX(DATUM(1:ISVAR),"'").NE.0) ISVAR=0
        END IF
        IF(ISVAR.NE.0) THEN
          READ(DATUM(1:ISVAR-1),*)II
          DATUM=DATUM(ISVAR+1:)
        ELSE
          II=1
        END IF
        IF(IDATA+II.GT.LENG) THEN
          CALL ERROR__MSG('MORE DATA THAN ARRAY CAN HOLD')
          CALL ERROR__STOP('BUFFER_READC8')
        END IF
!
!       ================================================================
!       == READ                                                       ==
!       ================================================================
        DO I=1,II
          IDATA=IDATA+1
          READ(DATUM,*)VAL(IDATA)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE BUFFER_READC8
!
!     ..................................................................
      SUBROUTINE BUFFER_READI4(BUFFER,VAL)
!     ******************************************************************
!     **                                                              **
!     **  NAME: BUFFER_READI4(BUFFER,VAL)                       **
!     **                                                              **
!     **  PURPOSE: READS AN ARRAY OF TYPE I4 FROM THE           **
!     **    BUFFER ARRAY.                                             **
!     **                                                              **
!     **    THE DATA ARE READ BEGINNING WITH BUFFER%FIRST             **
!     **    AND READING IS ENDED BY THE NEXT KEY WORD !XXX OR XXX=    **
!     **                                                              **
!     **  REMARK: THIS ROUTINE IS IMPLEMENTED AS A TEMPLATE FOR       **
!     **    TYPES C8,R8,I4,L4,CH                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(BUFF_TYPE),INTENT(INOUT):: BUFFER
      INTEGER(4)         ,INTENT(OUT)  :: VAL(:)
      CHARACTER(128)               :: DATUM
      INTEGER(4)                   :: I1,I2
      INTEGER(4)                   :: ISVAR,II,IDATA
      INTEGER(4)                   :: LENG
      INTEGER(4)                   :: I
!     ******************************************************************
      I1=BUFFER%FIRST
      LENG=SIZE(VAL)
      IDATA=0
      DO
        CALL BUFFER_NEXTWORD(BUFFER,I1,I2)
!
!       ================================================================
!       == TERMINATE LOOP IF KEY WORD ENCOUNTERED OR NO NEXT WORD     ==
!       ================================================================
        IF(I1.EQ.0) THEN
          CALL ERROR__STOP('BUFFER_READI4')
        END IF
        IF((BUFFER%CH(I2).EQ.'=').OR.(BUFFER%CH(I1).EQ.'!')) THEN
          BUFFER%FIRST=I1
          EXIT
        END IF
!
        DATUM=' '
        CALL BUFFER_TOSTRING(BUFFER,I1,I2,DATUM)
        I1=I2+1
!
!       ================================================================
!       == DETERMINE NUMBER OF ELEMENTS                               ==
!       ================================================================
        ISVAR=INDEX(DATUM,'*')
        IF(ISVAR.NE.0) THEN
          IF(INDEX(DATUM(1:ISVAR),'"').NE.0) ISVAR=0
        END IF
        IF(ISVAR.NE.0) THEN
          IF(INDEX(DATUM(1:ISVAR),"'").NE.0) ISVAR=0
        END IF
        IF(ISVAR.NE.0) THEN
          READ(DATUM(1:ISVAR-1),*)II
          DATUM=DATUM(ISVAR+1:)
        ELSE
          II=1
        END IF
        IF(IDATA+II.GT.LENG) THEN
          CALL ERROR__MSG('MORE DATA THAN ARRAY CAN HOLD')
          CALL ERROR__STOP('BUFFER_READI4')
        END IF
!
!       ================================================================
!       == READ                                                       ==
!       ================================================================
        DO I=1,II
          IDATA=IDATA+1
          READ(DATUM,*)VAL(IDATA)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE BUFFER_READI4
!
!     ..................................................................
      SUBROUTINE BUFFER_READL4(BUFFER,VAL)
!     ******************************************************************
!     **                                                              **
!     **  NAME: BUFFER_READL4(BUFFER,VAL)                       **
!     **                                                              **
!     **  PURPOSE: READS AN ARRAY OF TYPE L4 FROM THE           **
!     **    BUFFER ARRAY.                                             **
!     **                                                              **
!     **    THE DATA ARE READ BEGINNING WITH BUFFER%FIRST             **
!     **    AND READING IS ENDED BY THE NEXT KEY WORD !XXX OR XXX=    **
!     **                                                              **
!     **  REMARK: THIS ROUTINE IS IMPLEMENTED AS A TEMPLATE FOR       **
!     **    TYPES C8,R8,I4,L4,CH                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(BUFF_TYPE),INTENT(INOUT):: BUFFER
      LOGICAL(4)         ,INTENT(OUT)  :: VAL(:)
      CHARACTER(128)               :: DATUM
      INTEGER(4)                   :: I1,I2
      INTEGER(4)                   :: ISVAR,II,IDATA
      INTEGER(4)                   :: LENG
      INTEGER(4)                   :: I
!     ******************************************************************
      I1=BUFFER%FIRST
      LENG=SIZE(VAL)
      IDATA=0
      DO
        CALL BUFFER_NEXTWORD(BUFFER,I1,I2)
!
!       ================================================================
!       == TERMINATE LOOP IF KEY WORD ENCOUNTERED OR NO NEXT WORD     ==
!       ================================================================
        IF(I1.EQ.0) THEN
          CALL ERROR__STOP('BUFFER_READL4')
        END IF
        IF((BUFFER%CH(I2).EQ.'=').OR.(BUFFER%CH(I1).EQ.'!')) THEN
          BUFFER%FIRST=I1
          EXIT
        END IF
!
        DATUM=' '
        CALL BUFFER_TOSTRING(BUFFER,I1,I2,DATUM)
        I1=I2+1
!
!       ================================================================
!       == DETERMINE NUMBER OF ELEMENTS                               ==
!       ================================================================
        ISVAR=INDEX(DATUM,'*')
        IF(ISVAR.NE.0) THEN
          IF(INDEX(DATUM(1:ISVAR),'"').NE.0) ISVAR=0
        END IF
        IF(ISVAR.NE.0) THEN
          IF(INDEX(DATUM(1:ISVAR),"'").NE.0) ISVAR=0
        END IF
        IF(ISVAR.NE.0) THEN
          READ(DATUM(1:ISVAR-1),*)II
          DATUM=DATUM(ISVAR+1:)
        ELSE
          II=1
        END IF
        IF(IDATA+II.GT.LENG) THEN
          CALL ERROR__MSG('MORE DATA THAN ARRAY CAN HOLD')
          CALL ERROR__STOP('BUFFER_READL4')
        END IF
!
!       ================================================================
!       == READ                                                       ==
!       ================================================================
        DO I=1,II
          IDATA=IDATA+1
          READ(DATUM,*)VAL(IDATA)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE BUFFER_READL4
!
!     ..................................................................
      SUBROUTINE BUFFER_READCH(BUFFER,VAL)
!     ******************************************************************
!     **                                                              **
!     **  NAME: BUFFER_READCH(BUFFER,VAL)                       **
!     **                                                              **
!     **  PURPOSE: READS AN ARRAY OF TYPE CH FROM THE           **
!     **    BUFFER ARRAY.                                             **
!     **                                                              **
!     **    THE DATA ARE READ BEGINNING WITH BUFFER%FIRST             **
!     **    AND READING IS ENDED BY THE NEXT KEY WORD !XXX OR XXX=    **
!     **                                                              **
!     **  REMARK: THIS ROUTINE IS IMPLEMENTED AS A TEMPLATE FOR       **
!     **    TYPES C8,R8,I4,L4,CH                                      **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(BUFF_TYPE),INTENT(INOUT):: BUFFER
      CHARACTER(*)         ,INTENT(OUT)  :: VAL(:)
      CHARACTER(128)               :: DATUM
      INTEGER(4)                   :: I1,I2
      INTEGER(4)                   :: ISVAR,II,IDATA
      INTEGER(4)                   :: LENG
      INTEGER(4)                   :: I
!     ******************************************************************
      I1=BUFFER%FIRST
      LENG=SIZE(VAL)
      IDATA=0
      DO
        CALL BUFFER_NEXTWORD(BUFFER,I1,I2)
!
!       ================================================================
!       == TERMINATE LOOP IF KEY WORD ENCOUNTERED OR NO NEXT WORD     ==
!       ================================================================
        IF(I1.EQ.0) THEN
          CALL ERROR__STOP('BUFFER_READCH')
        END IF
        IF((BUFFER%CH(I2).EQ.'=').OR.(BUFFER%CH(I1).EQ.'!')) THEN
          BUFFER%FIRST=I1
          EXIT
        END IF
!
        DATUM=' '
        CALL BUFFER_TOSTRING(BUFFER,I1,I2,DATUM)
        I1=I2+1
!
!       ================================================================
!       == DETERMINE NUMBER OF ELEMENTS                               ==
!       ================================================================
        ISVAR=INDEX(DATUM,'*')
        IF(ISVAR.NE.0) THEN
          IF(INDEX(DATUM(1:ISVAR),'"').NE.0) ISVAR=0
        END IF
        IF(ISVAR.NE.0) THEN
          IF(INDEX(DATUM(1:ISVAR),"'").NE.0) ISVAR=0
        END IF
        IF(ISVAR.NE.0) THEN
          READ(DATUM(1:ISVAR-1),*)II
          DATUM=DATUM(ISVAR+1:)
        ELSE
          II=1
        END IF
        IF(IDATA+II.GT.LENG) THEN
          CALL ERROR__MSG('MORE DATA THAN ARRAY CAN HOLD')
          CALL ERROR__STOP('BUFFER_READCH')
        END IF
!
!       ================================================================
!       == READ                                                       ==
!       ================================================================
        DO I=1,II
          IDATA=IDATA+1
          READ(DATUM,*)VAL(IDATA)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE BUFFER_READCH
!#END TEMPLATE BUFFER_READXX
!
!     ..................................................................
      SUBROUTINE BUFFER_TOSTRING(BUFFER,I1,I2,STRING)
!     ******************************************************************
!     **                                                              **
!     **  NAME: BUFFER_TOSTRING(BUFFER,I1,I2,STRING)                  **
!     **                                                              **
!     **  PURPOSE: MAPS A PORTION OF THE BUFFER ARRAY BUFFER%CHA      **
!     **    ONTO A STRING.                                            **
!     **                                                              **
!     **                                                              **
!     ******************************************************************
      implicit none
      TYPE(BUFF_TYPE),INTENT(INOUT):: BUFFER
      INTEGER(4)     ,INTENT(IN)   :: I1,I2
      CHARACTER(*)   ,INTENT(OUT)  :: STRING
      INTEGER(4)                   :: I
      INTEGER(4)                   :: Ii
      INTEGER(4)                   :: LENG
!     ******************************************************************
      STRING=' '
      LENG=LEN(STRING)
      IF(I2-I1+1.GT.LENG) THEN
        II=0
        DO I=I1,I1+LENG-1
          II=II+1
          STRING(II:II)=BUFFER%CH(I)
        ENDDO
        CALL ERROR__MSG('WORD DOES NOT FIT INTO STRING')
        CALL ERROR__CHVAL('WORD(TRUNCATED)',STRING)
        CALL ERROR__I4VAL('LENG',LENG)
        CALL ERROR__STOP('BUFFER_tostring')
      END IF
!     == COPY INTO STRING
      II=0
      DO I=I1,I2
        II=II+1
        STRING(II:II)=BUFFER%CH(I)
      ENDDO
      RETURN
      END SUBROUTINE BUFFER_TOSTRING
!
!     ..................................................................
      SUBROUTINE BUFFER_NEXTWORD(BUFFER,I1,I2)
!     ******************************************************************
!     **                                                              **
!     **  NAME: BUFFER_NEXTWORD(BUFFER,I1,I2)                         **
!     **                                                              **
!     **  PURPOSE: SEARCHES FOR THE NEXT SYNTAXTIC WORD FOLLOWING     **
!     **    POSITION I1 ON BUFFER ARRAY "BUFFER%CH". A SYNTACTIC      **
!     **    WORD IS EITHER A BLOCK OR DATA KEY WORD OR A SCALAR DATA. **
!     **                                                              **
!     **    ON OUTPUT, I1 IS SET TO THE FIRST NON-BLANK CHARACTER     **
!     **    FOLLOWING THE PROVIDED I1 AND I2 POINTS TO THE LAST       **
!     **    NON-BLANK CHARACTER OR THE FIRST '=' SIGN FOLLOWING       **
!     **    THE NEW I1.                                               **
!     **                                                              **
!     ******************************************************************
      IMPLICIT NONE
      TYPE(BUFF_TYPE),INTENT(INOUT):: BUFFER
      INTEGER(4)     ,INTENT(INOUT):: I1
      INTEGER(4)     ,INTENT(OUT)  :: I2
      INTEGER(4)                   :: I,I1NEW
      INTEGER(4)                   :: IBLANK,ICHAR
      CHARACTER(1)                 :: CHAR
!     ******************************************************************
!
!     ==================================================================
!     ==                                                              ==
!     ==================================================================
      ICHAR=0
      IBLANK=2
      I1NEW=0
      I2=0
      DO I=I1,BUFFER%LAST
        CHAR=BUFFER%CH(I)
!       ================================================================
!       ==  COUNT SIGNIFICANT BLANKS
!       ================================================================
        IF(CHAR.EQ.' '.AND.ICHAR.EQ.0)THEN
          IBLANK=IBLANK+1
        ELSE
          IBLANK=0
        END IF
!       ================================================================
!       ==  SWITCH FOR CHARACTER CONTEXT                              ==
!       ================================================================
        IF(ICHAR.EQ.0) THEN
          IF(CHAR.EQ."'") THEN
            ICHAR=1
          ELSE IF(CHAR.EQ.'"') THEN
            ICHAR=2
          END IF
        ELSE
          IF(ICHAR.EQ.1.AND.CHAR.EQ."'") THEN
            ICHAR=0
          ELSE IF(ICHAR.EQ.2.AND.CHAR.EQ.'"') THEN
            ICHAR=0
          END IF
        END IF
!       ================================================================
!       ==                                                            ==
!       ================================================================
        IF(I1NEW.EQ.0) THEN
          IF(IBLANK.EQ.0) I1NEW=I
        ELSE
          IF(ICHAR.EQ.0) THEN
            IF(IBLANK.GE.1) THEN
              I2=I-1
              EXIT
            ELSE IF(CHAR.EQ.'=') THEN
              I2=I
              EXIT
            END IF
          END IF
        END IF
      ENDDO
      I1=I1NEW
      IF(I2.EQ.0) I1=0
      RETURN
      END SUBROUTINE BUFFER_NEXTWORD
END MODULE BUFFER_MODULE
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__READ(LL_,NFIL)
!     ******************************************************************
!     **                                                              **
!     **  CREATES A HOME DIRECTORY FOR THE LIST                       **
!     **  IF LIST IS ASSOCIATED ALREADY, ITS CONNECTION IS BROKEN     **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      USE BUFFER_MODULE,      ONLY: BUFFER__READ
      USE LINKEDLIST_MODULE, ONLY: LL_TYPE &
     &          ,LINKEDLIST__SELECT,LINKEDLIST__REPORT
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL_
      INTEGER(4)      ,INTENT(IN) :: NFIL
      TYPE(LL_TYPE)               :: LL
!     ******************************************************************
      LL=LL_
      CALL BUFFER__READ(LL,NFIL)
      RETURN
      END SUBROUTINE LINKEDLIST__READ
!
!     ..................................................................
      SUBROUTINE LINKEDLIST__WRITE(LL,NFIL)
!     ******************************************************************
!     **                                                              **
!     **  CREATES A HOME DIRECTORY FOR THE LIST                       **
!     **  IF LIST IS ASSOCIATED ALREADY, ITS CONNECTION IS BROKEN     **
!     **                                                              **
!     **  ERROR CONDITIONS: NONE                                      **
!     **                                                              **
!     ******************************************************************
      USE BUFFER_MODULE,      ONLY: BUFFER__WRITE
      USE LINKEDLIST_MODULE, ONLY: LL_TYPE
      IMPLICIT NONE
      TYPE(LL_TYPE)   ,INTENT(IN) :: LL
      INTEGER(4)      ,INTENT(IN) :: NFIL
!     ******************************************************************
      CALL BUFFER__WRITE(LL,NFIL)
      WRITE(NFIL,FMT='(A)')'!EOB'
      RETURN
      END SUBROUTINE LINKEDLIST__WRITE
