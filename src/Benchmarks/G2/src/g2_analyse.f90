!
!........1.........2.........3.........4.........5.........6.........7.........8
MODULE STRINGS_MODULE
!*******************************************************************************
!**                                                                           **
!**  NAME: STRINGS                                                            **
!**                                                                           **
!**  PURPOSE: DEFINE CASING OPERATORS FOR STRING MANIPULATIONS                **
!**                                                                           **
!**  FUNCTIONS:                                                               **
!**    '+'   UPPERCASE A STRING                                               **
!**    '-'   LOWERCASE A STRING                                               **
!**                                                                           **
!*******************************************************************************
PUBLIC
INTERFACE OPERATOR (-)
  MODULE PROCEDURE LOWER_CASE
END INTERFACE
INTERFACE OPERATOR (+) 
  MODULE PROCEDURE UPPER_CASE
END INTERFACE
CONTAINS
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
        return
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
end MODULE STRINGS_MODULE
!     ...1.........2.........3.........4.........5.........6.........7.........8
      PROGRAM MAIN
!     **************************************************************************
!     **  reads a file readpaw.in from standard input, which contains a list  **
!     **     of (1) name of the substance (2) energy of the substance         **
!     **                                                                      **
!     **   writes a set of files files with the atomization energies relative **
!     **   to the actual paw calculation for plotting                         **
!     **     g2devinev_expt.dat                                               **
!     **     g2devinev_gpawpbe.dat                                            **
!     **     g2devinev_vasppbe.dat                                            **
!     **     g2devinev_gausspbe.dat                                           **
!     **     g2devinev_scuseriapbe.dat                                        **
!     **     g2devinev_beckebpw91.dat                                         **
!     **                                                                      **
!     **  writes a file g2results.dat                                         **
!     **     (1) molecule number
!     **     (2) energy in ev of scuseria minus cp-paw                        **
!     **     (3) energy in ev of paierVASP/PBE minus cp-paw                   **
!     **     (4) energy in ev of paierGAUSS/PBE minus cp-paw                  **
!     **     (5) energy in ev of BECKE/PW91 minus cp-paw                      **
!     **                                                                      **
!     **  writes a file readpaw.out to standard out                           **
!     **                                                                      **
!     **  caution:                                                            **
!     **    set tspherical                                                    **
!     **                                                                      **
!     **************************************************************************
      use strings_module
      IMPLICIT NONE
      INTEGER(4),PARAMETER :: NFILO=6
      INTEGER(4),PARAMETER :: NFILdat=100
      INTEGER(4),PARAMETER :: NFILdat1=101
      character(32),parameter ::fildat='g2results.dat'
      REAL(8),PARAMETER    :: EV=1.D0/27.21138505D0
      LOGICAL(4),PARAMETER :: TSPHERICAL=.false.
      LOGICAL(4),PARAMETER :: Tnotfound=.true.
      INTEGER(4),PARAMETER :: NDATAX=200
      INTEGER(4)           :: NDATA
      LOGICAL(4)           :: TCHK,tchk1
      CHARACTER(26)        :: MOLID(NDATAX)
      REAL(8)              :: E(NDATAX)
      REAL(8)              :: VAL,val0,val1,val2,val3,val4,valref,val5,val6
      INTEGER(4),PARAMETER :: LEN=10
      INTEGER(4)           :: NUM(LEN)
      CHARACTER(2)         :: ATOM(LEN)
      INTEGER(4),PARAMETER :: NATX=20
      CHARACTER(2)         :: ATID(NATX)
      REAL(8)              :: EAT(NATX)
      INTEGER(4)           :: NAT
      INTEGER(4)           :: I,J,K,IAT
      INTEGER(4)           :: IASCII,ISVAR
      REAL(8)              :: SVAR
      CHARACTER(32)        :: STRING
!     **************************************************************************
!
!     ==========================================================================
!     == READ DATA MOLECULE ID AND ENERGY IN HARTREE                          ==
!     ==========================================================================
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(NFILO,FMT='(80("="),T20,A)')'  LIST OF MOLECULES ON DATA FILE  '
      WRITE(NFILO,FMT='(80("="),T20,A)')
      DO I=1,NDATAX
        READ(5,*,END=1000)MOLID(I),E(I)
        NDATA=I
        DO J=1,32
          IASCII=ICHAR(MOLID(I)(J:J))
          IF(IASCII.GE.97.AND.IASCII.LE.122) THEN
            IASCII=IASCII-97+65  ! MAKE UPPERCASE
            MOLID(I)(J:J)=ACHAR(IASCII)
          END IF
        ENDDO
      ENDDO
1000  CONTINUE
      WRITE(NFILO,FMT='(I5," MOLECULES AND ATOMS READ")')NDATA
      WRITE(NFILO,FMT='(A,T28,A,T55,A)')MOLID(1:NDATA)
!
!     ==========================================================================
!     ==   SELECT SPHERICAL OR NONSPHERICAL ATOMS                             ==
!     ==========================================================================
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(NFILO,FMT='(80("="),T20,A)') &
     &                                 '  SELECT SPHERICAL/NONSPHERICAL ATOMS  '
      WRITE(NFILO,FMT='(80("="),T20,A)')
      IF(TSPHERICAL) THEN
!       ==  FOR EACH SPHERICAL ATOM, REMOVE THE CORRESPONDING NONSPHERICAL ATOM
        WRITE(NFILO,FMT='("SPHERICAL ATOMS ARE USED")')
        I=1
        DO WHILE(I.LE.NDATA)
          STRING=MOLID(I)
          ISVAR=INDEX(STRING,'-SPH')
          IF(ISVAR.EQ.0) THEN
             I=I+1
             CYCLE
          END IF
          STRING=STRING(1:ISVAR-1)
          TCHK=.FALSE.
          DO J=1,NDATA
            IF(MOLID(J).EQ.STRING) THEN
              MOLID(J:NDATA-1)=MOLID(J+1:NDATA)
              E(J:NDATA-1)=E(J+1:NDATA)
              MOLID(NDATA)=' '
              E(NDATA)=0.D0
              NDATA=NDATA-1
              IF(J.LT.I) I=I-1
              MOLID(I)=STRING
              TCHK=.TRUE.
              EXIT
            END IF
          ENDDO
          IF(.NOT.TCHK) THEN
            PRINT*,'===============ERROR================'
            PRINT*,'DID NOT FIND NONSPHERICAL ATOM ',STRING
            STOP 'IN MAIN'
          END IF
          I=I+1
        ENDDO
      ELSE
!       ==  REMOVE THE SPHERICAL ATOMS (ENDING -SPH)
        WRITE(NFILO,FMT='("NONSPHERICAL ATOMS ARE USED")')
        I=1
        DO WHILE(I.LE.NDATA)
          STRING=MOLID(I)
          ISVAR=INDEX(STRING,'-SPH')
          IF(ISVAR.EQ.0) THEN
            I=I+1
            CYCLE
          ELSE
            MOLID(I:NDATA-1)=MOLID(I+1:NDATA)
            E(I:NDATA-1)=E(I+1:NDATA)
            NDATA=NDATA-1
          END IF
        ENDDO
      END IF
!
!     ==========================================================================
!     ==  COLLECT ATOM ENERGIES OF THE ATOMS                                  ==
!     ==========================================================================
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(NFILO,FMT='(80("="),T20,A)')'  ENERGIES OF THE ATOMS  '
      WRITE(NFILO,FMT='(80("="),T20,A)')
      NAT=0
      DO I=1,NDATA
!       == LOOK UP THE COMPOSITION OF THE MOLECULE =============================
        CALL ATOMIZE__GET(MOLID(I),LEN,NUM,ATOM)
!       == SKIP ALL MOLECULES AND ONLY LOOK UP ATOMS ===========================
!       == AND EXTRACT ATOMIC ENERGY ===========================================
        IF(NUM(1).EQ.1.AND.NUM(2).EQ.0) THEN
          NAT=NAT+1
          ATID(NAT)=ATOM(1)
          EAT(NAT)=E(I)
!         ==  CORRECTION FOR USING SPIN=0.499 ==================================
          IF(ATOM(1).EQ.'H') THEN
            EAT(NAT)=EAT(NAT)-2.544D-3   
            WRITE(NFILO,FMT='("CORRECTION OF -2.544 MH ADDED FOR HYDROGEN")')
            WRITE(NFILO,FMT='("TO EXTRAPOLATE FROM S=0.499 HBAR TO S=0.5 HBAR")')
          END IF
        END IF
      ENDDO
!
!     == REPORT RESULT =========================================================
      DO IAT=1,NAT
        WRITE(NFILO,FMT='(A10,F20.5)')ATID(IAT),EAT(IAT)
      ENDDO
!
!     ==========================================================================
!     ==  CONVERT TOTAL ENERGIES IN ATOMIZATION ENERGIES                      ==
!     ==========================================================================
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(NFILO,FMT='(80("="),T20,A)') &
     &                    '  CONVERT TOTAL ENERGIES INTO ATOMIZATION ENERGIES  '
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(NFILO,FMT='(A,t30,3A15)')'MOLECULE','ETOT','SUM:EAT','etot-sum:eat'
      DO I=1,NDATA
!       == LOOK UP THE COMPOSITION OF THE MOLECULE =============================
        CALL ATOMIZE__GET(MOLID(I),LEN,NUM,ATOM)

        SVAR=0.D0
        DO J=1,LEN
          IF(NUM(J).EQ.0) EXIT
          TCHK=.FALSE.
          DO K=1,NAT
            IF(ATID(K).EQ.ATOM(J)) THEN
              SVAR=SVAR+REAL(NUM(J))*EAT(K)
              TCHK=.TRUE.
              EXIT
            END IF
          ENDDO
          IF(.NOT.TCHK) THEN 
            PRINT*,'=====ERROR====='
            PRINT*,'MOLID ',MOLID(I)
            PRINT*,'ATOM ',ATOM(J),' NOT FOUND'
            PRINT*,'ATOM(J)',ATOM(J),J
            PRINT*,'ATOM',ATOM
            PRINT*,'NUM',NUM
            STOP 'IN MAIN'
          END IF
        ENDDO
        WRITE(NFILO,FMT='(i4,t6,A,t36,3F15.6)')i,MOLID(I),E(I),SVAR,E(I)-SVAR
        E(I)=E(I)-SVAR 
      ENDDO
!
!     ==  from here on E(i) is the CP-PAW energy of the molecule              ==
!     ==  relative to the sum of the energies of the isolated atoms           ==

!
!     ==========================================================================
!     ==  COMPARE ATOMIZATION ENERGIES TO EXPERIMENT                          ==
!     ==========================================================================
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(NFILO,FMT='(80("="),T20,A)') &
     &                          '  COMPARE ATOMIZATION ENERGIES TO EXPERIMENT  '
      WRITE(NFILO,FMT='(80("="),T20,A)') &
     &              '  EXP. DATA FROM ERNZERHOF,SCUSERIA, JCP110, 5029 (1999)  '
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(NFILO,FMT='(A,T30,3A15)')'MOLECULE ',' CPPAW[EV] ','EXP[EV] ' &
     &                                          ,' CPPAW-EXP[EV]'
      WRITE(NFILO,FMT='(80("-"))')
!
      OPEN(NFILDAT,FILE=-'G2DEVINEV_EXPT.DAT')
      DO I=1,NDATA
        CALL SCUSERIA__GET(MOLID(I),'EXP',VAL,TCHK)
        IF(TCHK) THEN
           WRITE(*,FMT='(i4,t6,A,T36,3F15.6)')i,TRIM(MOLID(I)) &
     &                                        ,-E(I)/EV,VAL/EV,(-E(I)-VAL)/EV
           write(nfildat,*)i,(-E(I)-VAL)/EV
        ELSE
          if(tnotfound)WRITE(nfilo,FMT='(i4,t6,A,t30," NOT FOUND")') &
     &               i,TRIM(MOLID(I))
        END IF
      ENDDO
      close(nfildat)
!
!     ==========================================================================
!     ==  COMPARE DATA TO OTHER CALCULATIONS                                  ==
!     ==========================================================================
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(NFILO,FMT='(80("="),T20,A)') &
     &                        '  COMPARE ATOMIZATION ENERGIES TO SCUSERIA PBE  '
      WRITE(NFILO,FMT='(80("="),T20,A)') &
     &                              '  ERNZERHOF,SCUSERIA, JCP110,5029 (1999)  '
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(nfilo,FMT='(A,t30,3A15)') &
     &                     'MOLECULE ',' CPPAW[EV] ','REF[EV] ',' CPPAW-REF[EV]'
      OPEN(NFILDAT,FILE=-'G2DEVINEV_SCUSERIAPBE.DAT')
      DO I=1,NDATA
        CALL SCUSERIA__GET(MOLID(I),'PBE',VAL,TCHK)
        IF(TCHK) THEN
           WRITE(nfilo,FMT='(i4,t6,A,T36,3F15.6)')i,TRIM(MOLID(I)) &
    &                                  ,-E(I)/EV,VAL/EV,(-E(I)-VAL)/EV
           write(nfildat,*)i,(-E(I)-VAL)/EV
        ELSE
         if(tnotfound)WRITE(nfilo,FMT='(i4,t6,A,T30,"NOT FOUND")')i,TRIM(MOLID(I))
        END IF
      ENDDO
      close(nfildat)
!
!     ==========================================================================
!     ==  COMPARE DATA TO Paier PBE                                           ==
!     ==========================================================================
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(NFILO,FMT='(80("="),T20,A)')'  COMPARE RESULTS TO DATABASES  '
      WRITE(NFILO,FMT='(80("="),T20,A)')'  PAIER ET AL. JCP122, 234102 (2005) '
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(*,FMT='(80("-"),t30,a)')' all energies in electron volt '
      WRITE(*,FMT='(A,t33,4A12)') &
     &              'MOLECULE',' CPPAW','VASP-CPPAW',' GAUSS-CPPAW','GAUSS-vasp'
      OPEN(NFILDAT,FILE=-'G2DEVINEV_VASPPBE.DAT')
      OPEN(NFILDAT1,FILE=-'G2DEVINEV_GAUSSPBE.DAT')
      DO I=1,NDATA
        CALL PAIER__GET(MOLID(I),'PBE_VASP',VAL,TCHK)
        CALL PAIER__GET(MOLID(I),'PBE_GAUSS',VAL1,TCHK1)
        if(.not.tchk1)val1=0.d0
        IF(TCHK) THEN
           WRITE(nfilo,FMT='(i4,t6,A,t33,4F12.3)')i,MOLID(I) &
     &                      ,-E(I)/EV,(E(I)+VAL)/EV,(E(I)+VAL1)/EV,(VAL1-VAL)/EV
           write(nfildat,*)i,(-E(I)-VAL)/EV
           write(nfildat1,*)i,(-E(I)-VAL1)/EV
       ELSE
         if(tnotfound)WRITE(nfilo,FMT='(i4,t6,A,t30," NOT FOUND")')i,TRIM(MOLID(I))
        END IF
      ENDDO
      close(nfildat)
      close(nfildat1)
!
!     ==========================================================================
!     ==  COMPARE DATA TO becke                                               ==
!     ==========================================================================
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(NFILO,FMT='(80("="),T20,A)') &
     &                           '  COMPARE ATOMIZATION ENERGIES TO DATABASES  '
      WRITE(NFILO,FMT='(80("="),T20,A)')'  BECKE '
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(NFILO,FMT='(A,T30,3A15)') &
     &                'MOLECULE',' CPPAW[EV]','BECKE/BP91[EV]','BECKE-CPPAW[EV]'
      OPEN(NFILDAT,FILE=-'G2DEVINEV_beckeBPW91.DAT')
      DO I=1,NDATA
        CALL BECKE__GET(MOLID(I),'BPW91',VAL,TCHK)
        IF(TCHK)then
          WRITE(*,FMT='(i4,t6,A,t36,4F15.3)')i,MOLID(I) &
     &                                          ,-E(I)/EV,VAL/EV,(-E(I)-VAL)/EV
          write(nfildat,*)i,(-E(I)-VAL)/EV
        endif
      ENDDO
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(NFILO,FMT='(80("="),T20,A)')'  DONE  '
      WRITE(NFILO,FMT='(80("="),T20,A)')
      close(nfildat)
!
!     ==========================================================================
!     ==  COMPARE DATA TO gpaw                                               ==
!     ==========================================================================
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(NFILO,FMT='(80("="),T20,A)') &
     &                           '  COMPARE ATOMIZATION ENERGIES TO DATABASES  '
      WRITE(NFILO,FMT='(80("="),T20,A)')'  gpaw '
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(NFILO,FMT='(A,T30,3A15)') &
     &                'MOLECULE',' CPPAW[EV]','gpaw/pbe[EV]','gpaw-CPPAW[EV]'
      OPEN(NFILDAT,FILE=-'G2DEVINEV_gpawpbe.DAT')
      DO I=1,NDATA
        CALL GPAW__GET(MOLID(I),'GPAW',VAL,TCHK)
        IF(TCHK)THEN
          WRITE(*,FMT='(I4,T6,A,T36,4F15.3)')I,MOLID(I) &
     &                                          ,-E(I)/EV,VAL/EV,(-E(I)-VAL)/EV
          WRITE(NFILDAT,*)I,(-E(I)-VAL)/EV
        ENDIF
      ENDDO
      WRITE(NFILO,FMT='(80("="),T20,A)')
      WRITE(NFILO,FMT='(80("="),T20,A)')'  DONE  '
      WRITE(NFILO,FMT='(80("="),T20,A)')
      close(nfildat)
!
!     ==========================================================================
!     ==  write data to file for graphic output                               ==
!     ==========================================================================
      open(nfildat,file=fildat)
      WRITE(NFILDAT,FMT='("# ATOMIZATION ENERGIES IN EV RELATIVE TO CP-PAW")')
      WRITE(NFILDAT,FMT='("# 1) PBE-VASP(PAIER) - MINE")')
      WRITE(NFILDAT,FMT='("# 2) PBE-GAUSS(PAIER)- MINE")')
      WRITE(NFILDAT,FMT='("# 3) PBE-GPAW - MINE")')
      WRITE(NFILDAT,FMT='("# 4) PBE-SCUSERIA - MINE")')
      WRITE(NFILDAT,FMT='("# 5) PW91-BECKE - MINE")')
      WRITE(NFILDAT,FMT='("# 6) EXPERIMENT - MINE")')
      DO I=1,NDATA
        VALREF=-E(I)
!       == DEFINE REFERENCE ENERGY
        CALL SCUSERIA__GET(MOLID(I),'EXP',VAL,TCHK)
        IF(.NOT.TCHK) CYCLE
        VAL6=VAL-valref   ! experiment - mine
!
        CALL SCUSERIA__GET(MOLID(I),'PBE',VAL,TCHK)
        VAL1=0.D0
        IF(TCHK)VAL1=VAL-VALREF  ! SCUSERIA-EXP
!
        CALL PAIER__GET(MOLID(I),'PBE_VASP',VAL,TCHK)
        VAL2=0.D0
        IF(TCHK)VAL2=VAL-VALREF  ! VASP-EXP
if(.not.tchk) cycle
!
        CALL PAIER__GET(MOLID(I),'PBE_GAUSS',VAL,TCHK)
        VAL3=0.D0
        IF(TCHK)VAL3=VAL-VALREF  ! GAUSS-EXP
!
        CALL BECKE__GET(MOLID(I),'BPW91',VAL,TCHK)
        VAL4=0.D0
        IF(TCHK)VAL4=VAL-VALREF  ! BECKEBPW91-EXP
!
        CALL GPAW__GET(MOLID(I),'GPAW',VAL,TCHK)
        VAL5=0.D0
        IF(TCHK)VAL5=VAL-VALREF  ! BECKEBPW91-EXP
        WRITE(NFILDAT,fmt='(i5,10f20.5)')I,VAL2/EV &  !pbe-vasp - mine
     &                                    ,VAL3/EV &  !pbe-gauss - mine
     &                                    ,VAL5/EV &  !pbe-gpaw -mine
     &                                    ,VAL1/EV &  !pbe-scuseria - mine
     &                                    ,VAL4/EV &   !pw91-becke - mine  
     &                                    ,VAL6/EV    !experiment - mine  
      ENDDO
      STOP
      END
!
!........1.........2.........3.........4.........5.........6.........7.........8
MODULE BECKE_MODULE
IMPLICIT NONE
LOGICAL(4)            :: TINI=.FALSE.
INTEGER(4),PARAMETER  :: NDATA=57
CHARACTER(20)         :: ID(NDATA)
REAL(8)               :: DATA(5,NDATA)
END MODULE BECKE_MODULE
!     ...1.........2.........3.........4.........5.........6.........7.........8
      SUBROUTINE BECKE_INI
!     **                                                                      **
!     ** CONTAINS  THE REACTION ENERGIES FROM                                 **
!     **    DENSITY FUNCTIONAL THERMOCHEMISTRY II                             **
!     **    A.D. BECKE J. CHEM. PHYS.97,9173(1992)                            **
!     ** AND THE ZERO POINT VIBRATIONAL ENERGIES FROM                         **
!     **    GAUSSIAN-1 THEORY: A GENERAL PROCEDURE FOR PREDICTION...          **
!     **    J.A.POPLE ET AL., J. CHEM PHYS. 90,5622(1989                      **
!     **                                                                      **
!     **  DATA(1): EXPT.    ATOMIZATION ENERGY IN KCAL/MOL                    **
!     **  DATA(2): LSDA     ATOMIZATION ENERGY IN KCAL/MOL                    **
!     **  DATA(3): BX       ATOMIZATION ENERGY IN KCAL/MOL                    **
!     **  DATA(4): BX-PW91C ATOMIZATION ENERGY IN KCAL/MOL                    **
!     **  DATA(5): ZERO POINT ENERGY IN MILLIHARTREE                          **
!     **                                                                      **  
      USE BECKE_MODULE
      IMPLICIT NONE
      INTEGER(4)           :: I
      REAL(8)   ,PARAMETER :: KCALBYMOL=1.D0/627.5095563399D0
      REAL(8)   ,PARAMETER :: MILLIHARTREE=1.D-3
!     **************************************************************************
      IF(TINI) RETURN
      TINI=.TRUE.
      I=0
 I=I+1; ID(I)='H2       '; DATA(:,I)=(/103.5D0,107.2D0,111.3D0,100.1D0, 9.45D0/) 
 I=I+1; ID(I)='LIH      '; DATA(:,I)=(/ 56.0D0, 58.8D0, 61.6D0, 51.7D0, 2.88D0/) 
 I=I+1; ID(I)='BEH      '; DATA(:,I)=(/ 46.9D0, 57.5D0, 57.7D0, 53.6D0, 4.37D0/) 
 I=I+1; ID(I)='CH       '; DATA(:,I)=(/ 79.9D0, 88.6D0, 84.4D0, 80.0D0, 6.22D0/) 
 I=I+1; ID(I)='CH2_3B1  '; DATA(:,I)=(/179.6D0,202.4D0,184.4D0,182.8D0,16.45D0/) 
 I=I+1; ID(I)='CH2_1A1  '; DATA(:,I)=(/170.6D0,188.9D0,174.9D0,167.3D0,16.04D0/) 
 I=I+1;ID(I)='CH2_1A1_AF'; DATA(:,I)=(/170.6D0,188.9D0,174.9D0,167.3D0,16.04D0/) 
 I=I+1; ID(I)='CH3      '; DATA(:,I)=(/189.2D0,321.7D0,285.7D0,190.2D0,27.65D0/) 
 I=I+1; ID(I)='CH4      '; DATA(:,I)=(/392.5D0,435.5D0,397.4D0,389.9D0,42.66D0/) 
 I=I+1; ID(I)='NH       '; DATA(:,I)=(/ 79.0D0, 91.0D0, 86.7D0, 82.8D0, 7.18D0/) 
 I=I+1; ID(I)='NH2      '; DATA(:,I)=(/170.0D0,196.5D0,181.1D0,174.7D0,18.36D0/) 
 I=I+1; ID(I)='NH3      '; DATA(:,I)=(/276.7D0,316.8D0,285.0D0,277.7D0,33.04D0/) 
 I=I+1; ID(I)='OH       '; DATA(:,I)=(/101.3D0,119.1D0,106.1D0,103.7D0, 8.13D0/) 
 I=I+1; ID(I)='H2O      '; DATA(:,I)=(/219.3D0,253.7D0,222.3D0,219.1D0,20.51D0/) 
 I=I+1; ID(I)='HF       '; DATA(:,I)=(/135.2D0,156.6D0,136.6D0,135.4D0, 8.86D0/) 
 I=I+1; ID(I)='LI2      '; DATA(:,I)=(/ 24.0D0, 23.0D0, 21.0D0, 17.1D0, 0.69D0/) 
 I=I+1; ID(I)='LIF      '; DATA(:,I)=(/137.6D0,154.2D0,138.8D0,135.0D0, 2.10D0/) 
 I=I+1; ID(I)='C2H2     '; DATA(:,I)=(/388.9D0,443.6D0,387.6D0,392.4D0,26.29D0/) 
 I=I+1; ID(I)='C2H4     '; DATA(:,I)=(/531.9D0,601.7D0,532.3D0,533.4D0,48.90D0/) 
 I=I+1; ID(I)='C2H6     '; DATA(:,I)=(/666.3D0,749.4D0,665.5D0,663.4D0,71.21D0/) 
 I=I+1; ID(I)='CN       '; DATA(:,I)=(/176.6D0,217.4D0,183.0D0,189.6D0, 4.03D0/) 
 I=I+1; ID(I)='HCN      '; DATA(:,I)=(/301.8D0,351.1D0,307.0D0,310.4D0,16.06D0/) 
 I=I+1; ID(I)='CO       '; DATA(:,I)=(/256.2D0,296.1D0,253.6D0,261.1D0, 4.96D0/) 
 I=I+1; ID(I)='HCO      '; DATA(:,I)=(/270.3D0,325.1D0,274.7D0,281.7D0,12.84D0/) 
 I=I+1; ID(I)='H2CO     '; DATA(:,I)=(/357.2D0,417.6D0,359.8D0,363.7D0,26.07D0/) 
 I=I+1; ID(I)='CH3OH    '; DATA(:,I)=(/480.8D0,555.4D0,480.6D0,481.6D0,49.41D0/) 
 I=I+1; ID(I)='N2       '; DATA(:,I)=(/225.1D0,264.6D0,231.1D0,234.3D0, 5.61D0/)
 I=I+1; ID(I)='N2H4     '; DATA(:,I)=(/405.4D0,483.8D0,413.4D0,411.3D0,51.84D0/)
 I=I+1; ID(I)='NO       '; DATA(:,I)=(/150.1D0,196.3D0,156.7D0,163.8D0, 4.52D0/)
 I=I+1; ID(I)='O2       '; DATA(:,I)=(/118.0D0,172.5D0,124.9D0,136.2D0, 4.06D0/)
 I=I+1; ID(I)='H2O2     '; DATA(:,I)=(/252.3D0,318.9D0,255.0D0,259.4D0,26.16D0/)
 I=I+1; ID(I)='F2       '; DATA(:,I)=(/ 36.9D0, 76.7D0, 40.2D0, 47.5D0, 2.53D0/)
 I=I+1; ID(I)='CO2      '; DATA(:,I)=(/381.9D0,466.2D0,381.6D0,400.3D0,11.38D0/)
 I=I+1; ID(I)='SIH2_1A1 '; DATA(:,I)=(/144.4D0,159.2D0,150.1D0,140.6D0,00.00D0/) 
 I=I+1; ID(I)='SIH2_3B1 '; DATA(:,I)=(/123.4D0,139.5D0,128.2D0,124.2D0,00.00D0/) 
 I=I+1; ID(I)='SIH3     '; DATA(:,I)=(/214.0D0,233.6D0,218.6D0,209.0D0,00.00D0/) 
 I=I+1; ID(I)='SIH4     '; DATA(:,I)=(/302.8D0,327.9D0,308.3D0,293.6D0,00.00D0/) 
 I=I+1; ID(I)='PH2      '; DATA(:,I)=(/144.7D0,165.7D0,152.6D0,145.5D0,00.00D0/) 
 I=I+1; ID(I)='PH3      '; DATA(:,I)=(/227.4D0,255.3D0,232.2D0,223.1D0,00.00D0/) 
 I=I+1; ID(I)='H2S      '; DATA(:,I)=(/173.2D0,197.3D0,174.2D0,171.6D0,00.00D0/) 
 I=I+1; ID(I)='HCL      '; DATA(:,I)=(/102.2D0,116.3D0,101.6D0,101.7D0,00.00D0/) 
 I=I+1; ID(I)='NA2      '; DATA(:,I)=(/ 16.6D0, 20.0D0, 16.3D0, 13.0D0,00.00D0/) 
 I=I+1; ID(I)='SI2      '; DATA(:,I)=(/ 74.0D0, 92.8D0, 73.6D0, 78.8D0,00.00D0/) 
 I=I+1; ID(I)='P2       '; DATA(:,I)=(/116.1D0,143.1D0,115.4D0,118.1D0,00.00D0/) 
 I=I+1; ID(I)='S2       '; DATA(:,I)=(/100.7D0,134.6D0,100.6D0,111.3D0, 0.00D0/)
 I=I+1; ID(I)='CL2      '; DATA(:,I)=(/ 57.2D0, 82.8D0, 51.8D0, 62.2D0,00.00D0/) 
 I=I+1; ID(I)='NACL     '; DATA(:,I)=(/ 97.5D0,102.8D0, 91.1D0, 91.9D0,00.00D0/) 
 I=I+1; ID(I)='SIO      '; DATA(:,I)=(/190.5D0,222.7D0,189.4D0,191.9D0,00.00D0/) 
 I=I+1; ID(I)='SC       '; DATA(:,I)=(/169.5D0,200.6D0,165.7D0,174.1D0,00.00D0/) 
 I=I+1; ID(I)='SO       '; DATA(:,I)=(/123.5D0,166.4D0,126.7D0,136.0D0,00.00D0/)  
 I=I+1; ID(I)='CLO      '; DATA(:,I)=(/ 63.3D0,104.0D0, 66.9D0, 76.5D0,00.00D0/) 
 I=I+1; ID(I)='FCL      '; DATA(:,I)=(/ 60.3D0, 94.3D0, 59.9D0, 67.9D0,00.00D0/) 
 I=I+1; ID(I)='SI2H6    '; DATA(:,I)=(/500.1D0,549.2D0,501.1D0,486.6D0,00.00D0/) 
 I=I+1; ID(I)='CH3CL    '; DATA(:,I)=(/371.0D0,424.7D0,367.4D0,371.7D0,00.00D0/) 
 I=I+1; ID(I)='CH3SH    '; DATA(:,I)=(/445.1D0,508.5D0,441.5D0,443.9D0,00.00D0/) 
 I=I+1; ID(I)='HOCL     '; DATA(:,I)=(/156.3D0,203.2D0,155.6D0,162.8D0,00.00D0/) 
 I=I+1; ID(I)='SO2      '; DATA(:,I)=(/254.0D0,332.8D0,250.3D0,269.0D0, 0.00D0/)
      IF(I.NE.NDATA) THEN
        PRINT*,'===============ERROR=============='
        PRINT*,'TARGET LENGTH OF THE LIST DIFFERS FROM ACTUAL LENGTH'
        PRINT*,'LIST: PAIER; PAW VERSUS GAUSSIAN'
        PRINT*,'TARGET LENGTH: (NDATA) :',NDATA
        PRINT*,'ACTUAL LENGTH: (I)     :',I
        STOP 'IN becke_INI'
      END IF
      DATA(1:4,:)=DATA(1:4,:)*KCALBYMOL
      DATA(5,:)=DATA(5,:)*MILLIHARTREE
!     == CORRECT FOR ZERO POINT VIBRATIONS
      DATA(1,:)=DATA(1,:)+DATA(5,:)
      DATA(2,:)=DATA(2,:)+DATA(5,:)
      DATA(3,:)=DATA(3,:)+DATA(5,:)
      DATA(4,:)=DATA(4,:)+DATA(5,:)
      RETURN
      END SUBROUTINE BECKE_INI
!
!     .....................................................................
      SUBROUTINE BECKE__GET(MOLID,DFTID,VAL,TCHK)
      USE BECKE_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: MOLID
      CHARACTER(*),INTENT(IN) :: DFTID
      REAL(8)     ,INTENT(OUT):: VAL
      LOGICAL(4)  ,INTENT(OUT):: TCHK
      INTEGER(4)              :: I
!     *********************************************************************
      CALL BECKE_INI
      DO I=1,NDATA
        TCHK=(TRIM(ID(I)).EQ.TRIM(MOLID))
        IF(TCHK) THEN
          IF(DFTID.EQ.'EXP') THEN
            VAL=DATA(1,I)
            RETURN
          ELSE IF(DFTID.EQ.'PZ') THEN
            VAL=DATA(2,I)
            RETURN
          ELSE IF(DFTID.EQ.'B') THEN
            VAL=DATA(3,I)
            RETURN
          ELSE IF(DFTID.EQ.'BPW91') THEN
            VAL=DATA(4,I)
            RETURN
          ELSE
            PRINT*,'===== ERROR======='
            PRINT*,'DFTID NOT RECOGNIZED'
            PRINT*,'DFTID:',DFTID
            STOP 'BECKE$GET'
          END IF
        END IF
      ENDDO
      VAL=0.D0
      RETURN
      END
!
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!...............................................................................
MODULE PAIER_MODULE
IMPLICIT NONE
LOGICAL(4)            :: TINI=.FALSE.
INTEGER(4),PARAMETER  :: NDATA=55
CHARACTER(20)         :: ID(NDATA)
REAL(8)               :: DATA(3,NDATA)
END MODULE PAIER_MODULE
!     ...1.........2.........3.........4.........5.........6.........7.........8
      SUBROUTINE PAIER_INI
!     **************************************************************************
!     ** CONTAINS  THE REACTION ENERGIES FROM                                 **
!     ** "THE PERDEW-BURKE-ERNZERHOF EXCHANGE CORRELATION FUNCTIONAL          **
!     ** APPLIED TO THE G2-1 TEST SET USING A PLANE-WAVE BASIS SET"           **
!     ** J. PAIER, R. HIRSCHL, M. MARSMAN AND G. KRESSE                       **
!     ** J. CHEM PHYS. 122,234102 (2005)                                      **
!     **************************************************************************
      USE PAIER_MODULE
      IMPLICIT NONE
      INTEGER(4)           :: I
      REAL(8)   ,PARAMETER :: KCALBYMOL=1.D0/627.5095563399D0
!     **************************************************************************
      IF(TINI) RETURN
      TINI=.TRUE.
      I=0
!     ==========================================================================
!     == ENERGIES IN KCAL/MOL                                                 ==
!     ==================================== EXP, PAW_VASP, GAUSSIAN==============
      I=I+1; ID(I)='LIH      '; DATA(:,I)=(/ 58.0D0, 53.5D0, 53.5D0/)
      I=I+1; ID(I)='BEH      '; DATA(:,I)=(/ 48.0D0, 55.5D0, 55.6D0/)
      I=I+1; ID(I)='CH       '; DATA(:,I)=(/ 84.0D0, 84.7D0, 84.8D0/)
      I=I+1; ID(I)='CH2_3B1  '; DATA(:,I)=(/189.0D0,194.4D0,194.6D0/)
      I=I+1; ID(I)='CH2_1A1  '; DATA(:,I)=(/182.0D0,178.8D0,179.1D0/)
      I=I+1; ID(I)='CH3      '; DATA(:,I)=(/306.0D0,309.7D0,310.1D0/)
      I=I+1; ID(I)='CH4      '; DATA(:,I)=(/420.0D0,419.6D0,420.2D0/)
      I=I+1; ID(I)='NH       '; DATA(:,I)=(/ 82.0D0, 88.6D0, 88.6D0/)
      I=I+1; ID(I)='NH2      '; DATA(:,I)=(/182.0D0,188.7D0,188.9D0/)
      I=I+1; ID(I)='NH3      '; DATA(:,I)=(/297.0D0,301.7D0,302.3D0/)
      I=I+1; ID(I)='OH       '; DATA(:,I)=(/107.0D0,109.7D0,110.1D0/)
      I=I+1; ID(I)='H2O      '; DATA(:,I)=(/233.0D0,233.7D0,234.5D0/)
      I=I+1; ID(I)='HF       '; DATA(:,I)=(/142.0D0,141.5D0,142.2D0/)
      I=I+1; ID(I)='SIH2_1A1 '; DATA(:,I)=(/154.0D0,147.9D0,148.0D0/)
      I=I+1; ID(I)='SIH2_3B1 '; DATA(:,I)=(/131.0D0,131.3D0,131.8D0/)
      I=I+1; ID(I)='SIH3     '; DATA(:,I)=(/226.0D0,222.2D0,222.6D0/)
      I=I+1; ID(I)='SIH4     '; DATA(:,I)=(/324.0D0,313.3D0,313.7D0/)
      I=I+1; ID(I)='PH2      '; DATA(:,I)=(/153.0D0,154.5D0,154.6D0/)
      I=I+1; ID(I)='PH3      '; DATA(:,I)=(/241.0D0,238.0D0,239.3D0/)
      I=I+1; ID(I)='SH2      '; DATA(:,I)=(/182.0D0,182.0D0,182.2D0/)
      I=I+1; ID(I)='HCL      '; DATA(:,I)=(/107.0D0,106.3D0,106.5D0/)
      I=I+1; ID(I)='LI2      '; DATA(:,I)=(/ 26.0D0, 19.9D0, 20.1D0/)
      I=I+1; ID(I)='LIF      '; DATA(:,I)=(/139.0D0,138.4D0,139.0D0/)
      I=I+1; ID(I)='C2H2     '; DATA(:,I)=(/404.0D0,414.5D0,415.1D0/)
      I=I+1; ID(I)='C2H4     '; DATA(:,I)=(/562.0D0,571.0D0,571.9D0/)
      I=I+1; ID(I)='C2H6     '; DATA(:,I)=(/711.0D0,716.0D0,717.1D0/)
      I=I+1; ID(I)='CN       '; DATA(:,I)=(/179.0D0,197.5D0,197.7D0/)
      I=I+1; ID(I)='HCN      '; DATA(:,I)=(/313.0D0,326.3D0,326.5D0/)
      I=I+1; ID(I)='CO       '; DATA(:,I)=(/261.0D0,268.6D0,269.1D0/)
      I=I+1; ID(I)='HCO      '; DATA(:,I)=(/279.0D0,294.9D0,295.5D0/)
      I=I+1; ID(I)='H2CO     '; DATA(:,I)=(/376.0D0,385.5D0,386.3D0/)
      I=I+1; ID(I)='CH3OH    '; DATA(:,I)=(/513.0D0,519.3D0,520.4D0/)
      I=I+1; ID(I)='N2       '; DATA(:,I)=(/227.0D0,243.7D0,243.9D0/)
      I=I+1; ID(I)='N2H4     '; DATA(:,I)=(/437.0D0,452.7D0,453.7D0/)
      I=I+1; ID(I)='NO       '; DATA(:,I)=(/153.0D0,172.0D0,172.5D0/)
      I=I+1; ID(I)='O2       '; DATA(:,I)=(/118.0D0,143.3D0,144.0D0/)
      I=I+1; ID(I)='H2O2     '; DATA(:,I)=(/268.0D0,281.6D0,282.6D0/)
      I=I+1; ID(I)='F2       '; DATA(:,I)=(/ 38.0D0, 52.6D0, 53.0D0/)
      I=I+1; ID(I)='CO2      '; DATA(:,I)=(/392.0D0,415.4D0,416.5D0/)
      I=I+1; ID(I)='NA2      '; DATA(:,I)=(/ 19.0D0, 17.7D0, 18.1D0/)
      I=I+1; ID(I)='SI2      '; DATA(:,I)=(/ 74.0D0, 81.3D0, 81.4D0/)
      I=I+1; ID(I)='P2       '; DATA(:,I)=(/116.0D0,121.5D0,121.7D0/)
      I=I+1; ID(I)='S2       '; DATA(:,I)=(/ 98.0D0,115.4D0,115.2D0/)
      I=I+1; ID(I)='CL2      '; DATA(:,I)=(/ 57.0D0, 65.8D0, 65.8D0/)
      I=I+1; ID(I)='NACL     '; DATA(:,I)=(/ 99.0D0, 93.6D0, 94.5D0/)
      I=I+1; ID(I)='SIO      '; DATA(:,I)=(/191.0D0,195.6D0,196.6D0/)
      I=I+1; ID(I)='SC       '; DATA(:,I)=(/172.0D0,179.5D0,179.6D0/)
      I=I+1; ID(I)='SO       '; DATA(:,I)=(/122.0D0,141.5D0,141.3D0/)
      I=I+1; ID(I)='CLO      '; DATA(:,I)=(/ 62.0D0, 81.6D0, 81.5D0/)
      I=I+1; ID(I)='FCL      '; DATA(:,I)=(/ 62.0D0, 72.3D0, 72.5D0/)
      I=I+1; ID(I)='SI2H6    '; DATA(:,I)=(/533.0D0,519.5D0,520.4D0/)
      I=I+1; ID(I)='CH3CL    '; DATA(:,I)=(/395.0D0,399.4D0,400.2D0/)
      I=I+1; ID(I)='CH3SH    '; DATA(:,I)=(/473.0D0,477.8D0,478.6D0/)
      I=I+1; ID(I)='HOCL     '; DATA(:,I)=(/165.0D0,175.2D0,175.7D0/)
      I=I+1; ID(I)='SO2      '; DATA(:,I)=(/253.0D0,281.1D0,280.7D0/)
      IF(I.NE.NDATA) THEN
        PRINT*,'===============ERROR=============='
        PRINT*,'TARGET LENGTH OF THE LIST DIFFERS FROM ACTUAL LENGTH'
        PRINT*,'LIST: PAIER; PAW VERSUS GAUSSIAN'
        PRINT*,'TARGET LENGTH: (NDATA) :',NDATA
        PRINT*,'ACTUAL LENGTH: (I)     :',I
        STOP 'IN PAIER_INI'
      END IF
      DATA(1:3,:)=DATA(1:3,:)*KCALBYMOL
      RETURN
      END SUBROUTINE PAIER_INI
!
!     ...1.........2.........3.........4.........5.........6.........7.........8
      SUBROUTINE PAIER__GET(MOLID,DFTID,VAL,TCHK)
      USE PAIER_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: MOLID
      CHARACTER(*),INTENT(IN) :: DFTID
      REAL(8)     ,INTENT(OUT):: VAL
      LOGICAL(4)  ,INTENT(OUT):: TCHK
      INTEGER(4)              :: I
!     *********************************************************************
      CALL PAIER_INI
      VAL=0.D0
      DO I=1,NDATA
        TCHK=(TRIM(ID(I)).EQ.TRIM(MOLID))
        IF(TCHK) THEN
          IF(DFTID.EQ.'EXP') THEN
            VAL=DATA(1,I)
            RETURN
          ELSE IF(DFTID.EQ.'PBE_VASP') THEN
            VAL=DATA(2,I)
            RETURN
          ELSE IF(DFTID.EQ.'PBE_GAUSS') THEN
            VAL=DATA(3,I)
            RETURN
          ELSE
            PRINT*,'===== ERROR======='
            PRINT*,'DFTID NOT RECOGNIZED'
            PRINT*,'DFTID:',DFTID
            STOP 'PAIER$GET'
          END IF
        END IF
      ENDDO
      VAL=0.D0
      RETURN
      END
!
!********************************************************************************
!********************************************************************************
!********************************************************************************
!........................................................................
MODULE SCUSERIA_MODULE
IMPLICIT NONE
LOGICAL(4)            :: TINI=.FALSE.
INTEGER(4),PARAMETER  :: NDATA=149
CHARACTER(26)         :: ID(NDATA)
REAL(8)               :: DATA(4,NDATA)
END MODULE SCUSERIA_MODULE
!     ..........................................................................
      SUBROUTINE SCUSERIA_INI
!     **                                                                      **
!     ** CONTAINS  THE REACTION ENERGIES FROM                                 **
!     **   M.ERNZERHOF, G.E. SCUSERIA, JCP110,5029(1999); TABLE 1             **
!     **                                                                      **
      USE SCUSERIA_MODULE
      IMPLICIT NONE
      INTEGER(4)           :: I
      REAL(8)   ,PARAMETER :: KCALBYMOL=1.D0/627.5095563399D0
!     ******************************************************************
      IF(TINI) RETURN
      TINI=.TRUE.
      I=0
!     ==========================================================================
!     == ATOMIZATION ENERGIES IN KCAL/MOL
!     ==  DATA(1,I)= EXPERIMENT: FROM CURTISS ET AL. JCP 106, 1063 (1977)
!     ==  DATA(2,I)=LSD (SVWN5)
!     ==  DATA(3,I)=PBE  (GGA)
!     ==  DATA(4,I)=PBE1PBE (HYBRID FUNCTIONAL)
!     ==========================================================================
      I=I+1; ID(I)='LIH                       '; DATA(:,I)=(/   58.0,   60.0,   53.0,   52.0/)
      I=I+1; ID(I)='BEH                       '; DATA(:,I)=(/   48.0,   60.0,   55.0,   56.0/)
      I=I+1; ID(I)='CH                        '; DATA(:,I)=(/   84.0,   92.0,   84.0,   83.0/)
      I=I+1; ID(I)='CH2_3B1                   '; DATA(:,I)=(/  189.0,  213.0,  184.0,  184.0/)
      I=I+1; ID(I)='CH2_1A1                   '; DATA(:,I)=(/  182.0,  203.0,  186.0,  184.0/)
      I=I+1; ID(I)='CH2_1A1_AF                '; DATA(:,I)=(/  182.0,  203.0,  186.0,  184.0/)
      I=I+1; ID(I)='CH3                       '; DATA(:,I)=(/  306.0,  339.0,  310.0,  308.0/)
      I=I+1; ID(I)='CH4                       '; DATA(:,I)=(/  420.0,  463.0,  420.0,  418.0/)
      I=I+1; ID(I)='NH                        '; DATA(:,I)=(/   82.0,   95.0,   88.0,   85.0/)
      I=I+1; ID(I)='NH2                       '; DATA(:,I)=(/  182.0,  208.0,  189.0,  184.0/)
      I=I+1; ID(I)='NH3                       '; DATA(:,I)=(/  297.0,  337.0,  302.0,  295.0/)
      I=I+1; ID(I)='OH                        '; DATA(:,I)=(/  107.0,  124.0,  110.0,  106.0/)
      I=I+1; ID(I)='H2O                       '; DATA(:,I)=(/  233.0,  267.0,  235.0,  227.0/)
      I=I+1; ID(I)='HF                        '; DATA(:,I)=(/  142.0,  163.0,  142.0,  137.0/)
      I=I+1; ID(I)='SIH2_1A1                  '; DATA(:,I)=(/  154.0,  166.0,  148.0,  147.0/)
      I=I+1; ID(I)='SIH2_3B1                  '; DATA(:,I)=(/  131.0,  147.0,  132.0,  132.0/)
      I=I+1; ID(I)='SIH3                      '; DATA(:,I)=(/  226.0,  247.0,  223.0,  223.0/)
      I=I+1; ID(I)='SIH4                      '; DATA(:,I)=(/  324.0,  348.0,  315.0,  316.0/)
      I=I+1; ID(I)='PH2                       '; DATA(:,I)=(/  153.0,  174.0,  155.0,  153.0/)
      I=I+1; ID(I)='PH3                       '; DATA(:,I)=(/  241.0,  270.0,  239.0,  238.0/)
      I=I+1; ID(I)='SH2                       '; DATA(:,I)=(/  182.0,  207.0,  182.0,  180.0/)
      I=I+1; ID(I)='HCL                       '; DATA(:,I)=(/  107.0,  120.0,  108.0,  105.0/)
      I=I+1; ID(I)='LI2                       '; DATA(:,I)=(/   26.0,   24.0,   20.0,   19.0/)
      I=I+1; ID(I)='LIF                       '; DATA(:,I)=(/  139.0,  152.0,  139.0,  132.0/)
      I=I+1; ID(I)='C2H2                      '; DATA(:,I)=(/  404.0,  461.0,  415.0,  404.0/)
      I=I+1; ID(I)='C2H4                      '; DATA(:,I)=(/  562.0,  633.0,  571.0,  564.0/)
      I=I+1; ID(I)='C2H6                      '; DATA(:,I)=(/  711.0,  795.0,  717.0,  712.0/)
      I=I+1; ID(I)='CN                        '; DATA(:,I)=(/  179.0,  219.0,  196.0,  179.0/)
      I=I+1; ID(I)='HCN                       '; DATA(:,I)=(/  313.0,  361.0,  326.0,  311.0/)
      I=I+1; ID(I)='CO                        '; DATA(:,I)=(/  261.0,  300.0,  270.0,  255.0/)
      I=I+1; ID(I)='HCO                       '; DATA(:,I)=(/  279.0,  334.0,  296.0,  281.0/)
      I=I+1; ID(I)='H2CO                      '; DATA(:,I)=(/  376.0,  435.0,  387.0,  373.0/)
      I=I+1; ID(I)='CH3OH                     '; DATA(:,I)=(/  513.0,  587.0,  520.0,  510.0/)
      I=I+1; ID(I)='N2                        '; DATA(:,I)=(/  227.0,  265.0,  242.0,  223.0/)
      I=I+1; ID(I)='N2H4                      '; DATA(:,I)=(/  437.0,  516.0,  453.0,  438.0/)
      I=I+1; ID(I)='NO                        '; DATA(:,I)=(/  153.0,  199.0,  172.0,  154.0/)
      I=I+1; ID(I)='O2                        '; DATA(:,I)=(/  118.0,  173.0,  143.0,  122.0/)
      I=I+1; ID(I)='H2O2                      '; DATA(:,I)=(/  268.0,  334.0,  282.0,  262.0/)
      I=I+1; ID(I)='F2                        '; DATA(:,I)=(/   38.0,   77.0,   52.0,   33.0/)
      I=I+1; ID(I)='CO2                       '; DATA(:,I)=(/  392.0,  475.0,  418.0,  393.0/)
      I=I+1; ID(I)='NA2                       '; DATA(:,I)=(/   19.0,   20.0,   18.0,   16.0/)
      I=I+1; ID(I)='SI2                       '; DATA(:,I)=(/   74.0,   93.0,   82.0,   71.0/)
      I=I+1; ID(I)='P2                        '; DATA(:,I)=(/  116.0,  144.0,  122.0,  111.0/)
      I=I+1; ID(I)='S2                        '; DATA(:,I)=(/   98.0,  137.0,  114.0,  106.0/)
      I=I+1; ID(I)='CL2                       '; DATA(:,I)=(/   57.0,   81.0,   67.0,   59.0/)
      I=I+1; ID(I)='NACL                      '; DATA(:,I)=(/   99.0,  104.0,   96.0,   94.0/)
      I=I+1; ID(I)='SIO                       '; DATA(:,I)=(/  191.0,  224.0,  197.0,  182.0/)
      I=I+1; ID(I)='SC                        '; DATA(:,I)=(/  172.0,  204.0,  179.0,  168.0/)
      I=I+1; ID(I)='SO                        '; DATA(:,I)=(/  122.0,  167.0,  140.0,  125.0/)
      I=I+1; ID(I)='CLO                       '; DATA(:,I)=(/   62.0,  102.0,   81.0,   66.0/)
      I=I+1; ID(I)='FCL                       '; DATA(:,I)=(/   62.0,   94.0,   73.0,   60.0/)
      I=I+1; ID(I)='SI2H6                     '; DATA(:,I)=(/  533.0,  581.0,  522.0,  523.0/)
      I=I+1; ID(I)='CH3CL                     '; DATA(:,I)=(/  395.0,  448.0,  401.0,  396.0/)
      I=I+1; ID(I)='CH3SH                     '; DATA(:,I)=(/  473.0,  538.0,  478.0,  473.0/)
      I=I+1; ID(I)='HOCL                      '; DATA(:,I)=(/  165.0,  210.0,  176.0,  163.0/)
      I=I+1; ID(I)='SO2                       '; DATA(:,I)=(/  253.0,  334.0,  279.0,  249.0/)
      I=I+1; ID(I)='BF3                       '; DATA(:,I)=(/  470.0,  545.0,  482.0,  467.0/)
      I=I+1; ID(I)='BCL3                      '; DATA(:,I)=(/  325.0,  383.0,  341.0,  332.0/)
      I=I+1; ID(I)='ALF3                      '; DATA(:,I)=(/  425.0,  481.0,  425.0,  411.0/)
      I=I+1; ID(I)='ALCL3                     '; DATA(:,I)=(/  309.0,  344.0,  311.0,  307.0/)
      I=I+1; ID(I)='CF4                       '; DATA(:,I)=(/  482.0,  604.0,  507.0,  482.0/)
      I=I+1; ID(I)='CCL4                      '; DATA(:,I)=(/  316.0,  402.0,  339.0,  322.0/)
      I=I+1; ID(I)='COS                       '; DATA(:,I)=(/  336.0,  412.0,  361.0,  339.0/)
      I=I+1; ID(I)='CS2                       '; DATA(:,I)=(/  280.0,  348.0,  302.0,  285.0/)
      I=I+1; ID(I)='CF2O                      '; DATA(:,I)=(/  423.0,  528.0,  451.0,  425.0/)
      I=I+1; ID(I)='SIF4                      '; DATA(:,I)=(/  566.0,  654.0,  569.0,  551.0/)
      I=I+1; ID(I)='SICL4                     '; DATA(:,I)=(/  387.0,  443.0,  391.0,  384.0/)
      I=I+1; ID(I)='N2O                       '; DATA(:,I)=(/  270.0,  360.0,  310.0,  273.0/)
      I=I+1; ID(I)='CLNO                      '; DATA(:,I)=(/  192.0,  265.0,  226.0,  192.0/)
      I=I+1; ID(I)='NF3                       '; DATA(:,I)=(/  209.0,  315.0,  247.0,  211.0/)
      I=I+1; ID(I)='PF3                       '; DATA(:,I)=(/  359.0,  443.0,  375.0,  354.0/)
      I=I+1; ID(I)='O3                        '; DATA(:,I)=(/  148.0,  240.0,  184.0,  141.0/)
      I=I+1; ID(I)='F2O                       '; DATA(:,I)=(/   94.0,  171.0,  123.0,   90.0/)
      I=I+1; ID(I)='CLF3                      '; DATA(:,I)=(/  126.0,  229.0,  166.0,  129.0/)
      I=I+1; ID(I)='C2F4                      '; DATA(:,I)=(/  592.0,  751.0,  634.0,  600.0/)
      I=I+1; ID(I)='C2CL4                     '; DATA(:,I)=(/  471.0,  591.0,  507.0,  483.0/)
      I=I+1; ID(I)='CF3CN                     '; DATA(:,I)=(/  645.0,  795.0,  685.0,  647.0/)
      I=I+1; ID(I)='CH3CCH_PROPYNE            '; DATA(:,I)=(/  703.0,  804.0,  722.0,  708.0/)
      I=I+1; ID(I)='CH2CCH2_ALLENE            '; DATA(:,I)=(/  702.0,  808.0,  725.0,  710.0/)
      I=I+1; ID(I)='C3H4                      '; DATA(:,I)=(/  679.0,  787.0,  702.0,  689.0/)
      I=I+1; ID(I)='CH3CHCH2_PROPYLENE        '; DATA(:,I)=(/  859.0,  973.0,  874.0,  864.0/)
      I=I+1; ID(I)='C3H6                      '; DATA(:,I)=(/  851.0,  971.0,  869.0,  860.0/)
      I=I+1; ID(I)='C3H8                      '; DATA(:,I)=(/ 1005.0, 1130.0, 1015.0, 1008.0/)
      I=I+1; ID(I)='C4H6_BUTADIENE            '; DATA(:,I)=(/ 1009.0, 1153.0, 1035.0, 1018.0/)
      I=I+1; ID(I)='C4H6_2BUTYNE              '; DATA(:,I)=(/ 1001.0, 1145.0, 1026.0, 1010.0/)
      I=I+1; ID(I)='C4H6_METHYLENECYCLOPROPANE'; DATA(:,I)=(/  990.0, 1141.0, 1019.0, 1004.0/)
      I=I+1; ID(I)='C4H6_BICYCLOBUTANE        '; DATA(:,I)=(/  983.0, 1138.0, 1013.0,  999.0/)
      I=I+1; ID(I)='C4H6_CYCLOBUTENE          '; DATA(:,I)=(/  998.0, 1148.0, 1025.0, 1010.0/)
      I=I+1; ID(I)='C4H8_CYCLOBUTANE          '; DATA(:,I)=(/ 1147.0, 1307.0, 1168.0, 1157.0/)
      I=I+1; ID(I)='C4H8_ISOBUTENE            '; DATA(:,I)=(/ 1156.0, 1312.0, 1176.0, 1163.0/)
      I=I+1; ID(I)='C4H10                     '; DATA(:,I)=(/ 1299.0, 1464.0, 1313.0, 1304.0/)
      I=I+1; ID(I)='C4H10_ISOBUTANE           '; DATA(:,I)=(/ 1301.0, 1466.0, 1314.0, 1305.0/)
      I=I+1; ID(I)='C5H8_SPIROPENTANE         '; DATA(:,I)=(/ 1281.0, 1478.0, 1317.0, 1300.0/)
      I=I+1; ID(I)='C6H6_BENZENE              '; DATA(:,I)=(/ 1362.0, 1578.0, 1410.0, 1386.0/)
      I=I+1; ID(I)='CH2F2                     '; DATA(:,I)=(/  439.0,  519.0,  452.0,  438.0/)
      I=I+1; ID(I)='CHF3                      '; DATA(:,I)=(/  462.0,  563.0,  481.0,  461.0/)
      I=I+1; ID(I)='CH2CL2                    '; DATA(:,I)=(/  370.0,  434.0,  383.0,  374.0/)
      I=I+1; ID(I)='CHCL3                     '; DATA(:,I)=(/  344.0,  419.0,  363.0,  350.0/)
      I=I+1; ID(I)='CH3NH2_METHYLAMINE        '; DATA(:,I)=(/  581.0,  662.0,  591.0,  581.0/)
      I=I+1; ID(I)='CH3CN_METHYLCYANIDE       '; DATA(:,I)=(/  615.0,  707.0,  636.0,  617.0/)
      I=I+1; ID(I)='CH3NO2_NITROMETHANE       '; DATA(:,I)=(/  603.0,  745.0,  643.0,  605.0/)
      I=I+1; ID(I)='CH3ONO_METHYLNITRITE      '; DATA(:,I)=(/  601.0,  738.0,  638.0,  601.0/)
      I=I+1; ID(I)='CH3SIH3_METHYLSILANE      '; DATA(:,I)=(/  627.0,  692.0,  623.0,  621.0/)
      I=I+1; ID(I)='CHOOH_FORMICACID          '; DATA(:,I)=(/  503.0,  600.0,  524.0,  502.0/)
      I=I+1; ID(I)='HCOOCH3_METHYLFORMATE     '; DATA(:,I)=(/  788.0,  925.0,  814.0,  788.0/)
      I=I+1; ID(I)='CH3CONH2_ACETAMIDE        '; DATA(:,I)=(/  867.0, 1013.0,  897.0,  873.0/)
      I=I+1; ID(I)='C2H4NH_AZIRIDINE          '; DATA(:,I)=(/  719.0,  834.0,  741.0,  727.0/)
      I=I+1; ID(I)='NCCN_CYANOGEN             '; DATA(:,I)=(/  501.0,  601.0,  539.0,  504.0/)
      I=I+1; ID(I)='CH3_2NH_DIMETHYLAMINE     '; DATA(:,I)=(/  869.0,  991.0,  884.0,  871.0/)
      I=I+1; ID(I)='CH3CH2NH2_TRANSETHYLAMINE '; DATA(:,I)=(/  877.0,  999.0,  892.0,  880.0/)
      I=I+1; ID(I)='CH2CO_KETENE              '; DATA(:,I)=(/  533.0,  630.0,  560.0,  539.0/)
      I=I+1; ID(I)='C2H4O_OXIRANE             '; DATA(:,I)=(/  651.0,  759.0,  671.0,  655.0/)
      I=I+1; ID(I)='CH3CHO_ACETALDEHYDE       '; DATA(:,I)=(/  677.0,  780.0,  695.0,  678.0/)
      I=I+1; ID(I)='C2H2O2_GLYOXAL            '; DATA(:,I)=(/  636.0,  755.0,  665.0,  636.0/)
      I=I+1; ID(I)='CH3CH2OH_ETHANOL          '; DATA(:,I)=(/  810.0,  926.0,  822.0,  810.0/)
      I=I+1; ID(I)='CH3OCH3_DIMETHYLETHER     '; DATA(:,I)=(/  799.0,  914.0,  811.0,  798.0/)
      I=I+1; ID(I)='C2H4S_THIOOXIRANE         '; DATA(:,I)=(/  624.0,  723.0,  641.0,  631.0/)
      I=I+1; ID(I)='CH3_2SO_DIMETHYLSULFOXIDE '; DATA(:,I)=(/  853.0,  995.0,  874.0,  855.0/)
      I=I+1; ID(I)='C2H5SH_ETHANTHIOL         '; DATA(:,I)=(/  767.0,  873.0,  777.0,  769.0/)
      I=I+1; ID(I)='CH3SCH3_DIMETHYLSULFIDE   '; DATA(:,I)=(/  766.0,  872.0,  777.0,  768.0/)
      I=I+1; ID(I)='CH2CHF                    '; DATA(:,I)=(/  573.0,  666.0,  591.0,  576.0/)
      I=I+1; ID(I)='CH3CH2CL_ETHYLCHLORIDE    '; DATA(:,I)=(/  691.0,  785.0,  702.0,  694.0/)
      I=I+1; ID(I)='CH2CHCL_VINYLCHLORIDE     '; DATA(:,I)=(/  542.0,  626.0,  559.0,  547.0/)
      I=I+1; ID(I)='CH2CHCN_ACRYLONITRILE     '; DATA(:,I)=(/  761.0,  882.0,  792.0,  767.0/)
      I=I+1; ID(I)='CH3COCH3_ACETONE          '; DATA(:,I)=(/  978.0, 1123.0, 1001.0,  982.0/)
      I=I+1; ID(I)='CH3COOH_ACETICACID        '; DATA(:,I)=(/  804.0,  943.0,  830.0,  806.0/)
      I=I+1; ID(I)='CH3COF_ACETYLFLUORIDE     '; DATA(:,I)=(/  707.0,  832.0,  733.0,  710.0/)
      I=I+1; ID(I)='CH3COCL_ACETYLCHLORIDE    '; DATA(:,I)=(/  669.0,  786.0,  695.0,  674.0/)
      I=I+1; ID(I)='CH3CH2CH2CL_PROPYLCHLORIDE'; DATA(:,I)=(/  985.0, 1119.0, 1000.0,  990.0/)
      I=I+1; ID(I)='CH3_2CHOH_ISOPROPANOL     '; DATA(:,I)=(/ 1108.0, 1265.0, 1124.0, 1109.0/)
      I=I+1; ID(I)='C2H5OCH3_METHYLETHYLETHER '; DATA(:,I)=(/ 1096.0, 1252.0, 1113.0, 1097.0/)
      I=I+1; ID(I)='CH3_3N_TRIMETHYLAMINE     '; DATA(:,I)=(/ 1160.0, 1323.0, 1178.0, 1163.0/)
      I=I+1; ID(I)='C4H4O_FURAN               '; DATA(:,I)=(/  992.0, 1169.0, 1033.0, 1007.0/)
      I=I+1; ID(I)='C4H4S_THIOPHENE           '; DATA(:,I)=(/  960.0, 1126.0,  997.0,  977.0/)
      I=I+1; ID(I)='C4H5N_PYRROLE             '; DATA(:,I)=(/ 1068.0, 1253.0, 1112.0, 1087.0/)
      I=I+1; ID(I)='C5H5N_PYRIDINE            '; DATA(:,I)=(/ 1234.0, 1445.0, 1286.0, 1256.0/)
      I=I+1; ID(I)='H2                        '; DATA(:,I)=(/  110.0,  113.0,  104.0,  104.0/)
      I=I+1; ID(I)='HS                        '; DATA(:,I)=(/   87.0,  100.0,   88.0,   87.0/)
      I=I+1; ID(I)='CCH                       '; DATA(:,I)=(/  262.0,  310.0,  276.0,  266.0/)
      I=I+1; ID(I)='C2H3-2APRIME              '; DATA(:,I)=(/  443.0,  509.0,  458.0,  450.0/)
      I=I+1; ID(I)='CH3CO-2APRIME             '; DATA(:,I)=(/  581.0,  679.0,  605.0,  587.0/)
      I=I+1; ID(I)='H2COH-2A                  '; DATA(:,I)=(/  409.0,  479.0,  422.0,  412.0/)
      I=I+1; ID(I)='CH3O-2APRIME              '; DATA(:,I)=(/  400.0,  465.0,  413.0,  403.0/)
      I=I+1; ID(I)='CH3CH2O-2ADOUBLEPRIME     '; DATA(:,I)=(/  696.0,  802.0,  714.0,  701.0/)
      I=I+1; ID(I)='CH3S-2APRIME              '; DATA(:,I)=(/  381.0,  437.0,  390.0,  384.0/)
      I=I+1; ID(I)='C2H5-2APRIME              '; DATA(:,I)=(/  601.0,  678.0,  612.0,  607.0/)
      I=I+1; ID(I)='CH3_2CH-2APRIME           '; DATA(:,I)=(/  898.0, 1018.0,  915.0,  907.0/)
      I=I+1; ID(I)='CH3_3C_TBUTYLRADICAL      '; DATA(:,I)=(/ 1195.0, 1357.0, 1217.0, 1207.0/)
      I=I+1; ID(I)='NO2                       '; DATA(:,I)=(/  228.0,  323.0,  271.0,  233.0/)
      IF(I.NE.NDATA) THEN
        PRINT*,'===============ERROR=============='
        PRINT*,I,NDATA
        STOP 'IN SCUSERIA_INI'
      END IF
      DATA(1:3,:)=DATA(1:3,:)*KCALBYMOL
      RETURN
    END SUBROUTINE SCUSERIA_INI
!
!     .....................................................................
      SUBROUTINE SCUSERIA__GET(MOLID,DFTID,VAL,TCHK)
      USE SCUSERIA_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: MOLID
      CHARACTER(*),INTENT(IN) :: DFTID
      REAL(8)     ,INTENT(OUT):: VAL
      LOGICAL(4)  ,INTENT(OUT):: TCHK
      INTEGER(4)              :: I
!     *********************************************************************
      CALL SCUSERIA_INI
      VAL=0.D0
      DO I=1,NDATA
        TCHK=(TRIM(ID(I)).EQ.TRIM(MOLID))
        IF(TCHK) THEN
          IF(DFTID.EQ.'EXP') THEN
            VAL=DATA(1,I)
            RETURN
          ELSE IF(DFTID.EQ.'SVWN') THEN
            VAL=DATA(2,I)
            RETURN
          ELSE IF(DFTID.EQ.'PBE') THEN
            VAL=DATA(3,I)
            RETURN
          ELSE IF(DFTID.EQ.'PBE1PBE') THEN
            VAL=DATA(4,I)
            RETURN
          ELSE
            PRINT*,'===== ERROR======='
            PRINT*,'DFTID NOT RECOGNIZED'
            PRINT*,'DFTID:',DFTID
            STOP 'SCUSERIA$GET'
          END IF
        END IF
      ENDDO
      VAL=0.D0
      RETURN
      END SUBROUTINE SCUSERIA__GET
!
!
!........1.........2.........3.........4.........5.........6.........7.........8
MODULE ATOMIZE_MODULE
!*******************************************************************************
!***                                                                         ***
!***  COMPOSITION OF EACH MOLECULE                                           ***
!***    THE MOLECULE WITH ID(IDATA) CONSISTS OF NUM(IAT,IDATA) ATOMS         ***
!***    OF TYPE ATOM(IAT,IDATA)                                              ***
!***                                                                         ***
!***                                                                         ***
!*******************************************************************************
INTEGER(4),PARAMETER :: NDATA=167
INTEGER(4),PARAMETER :: NATOMX=10
CHARACTER(32)        :: ID(NDATA)
INTEGER(4)           :: NUM(NATOMX,NDATA)
CHARACTER(2)         :: ATOM(NATOMX,NDATA)
LOGICAL(4)           :: TINI=.FALSE.
END MODULE ATOMIZE_MODULE
!
!     ...1.........2.........3.........4.........5.........6.........7.........8
      SUBROUTINE ATOMIZE_INI()
!     ************************************************************************** 
!     **  COMPOSITION FOR EACH OF THE MOLECULES IN THE SET                    ** 
!     ************************************************************************** 
      USE ATOMIZE_MODULE
      IMPLICIT NONE
      INTEGER(4)           :: I,IATOM,J,IASCII
      CHARACTER(64)        :: ATOMIZATION(NDATA)
      CHARACTER(64)        :: STRING
      INTEGER(4)           :: NUMBER
!     ************************************************************************** 
      IF(TINI) RETURN
      TINI=.TRUE.
!
      I=0
      I=I+1; ID(I)='H       '; ATOMIZATION(I)='H   '
      I=I+1; ID(I)='LI      '; ATOMIZATION(I)='LI   '
      I=I+1; ID(I)='BE      '; ATOMIZATION(I)='BE   '
      I=I+1; ID(I)='B       '; ATOMIZATION(I)='B   '
      I=I+1; ID(I)='C       '; ATOMIZATION(I)='C   '
      I=I+1; ID(I)='N       '; ATOMIZATION(I)='N   '
      I=I+1; ID(I)='O       '; ATOMIZATION(I)='O   '
      I=I+1; ID(I)='F       '; ATOMIZATION(I)='F   '
      I=I+1; ID(I)='NA      '; ATOMIZATION(I)='NA   '
      I=I+1; ID(I)='AL      '; ATOMIZATION(I)='AL   '
      I=I+1; ID(I)='SI      '; ATOMIZATION(I)='SI   '
      I=I+1; ID(I)='P       '; ATOMIZATION(I)='P   '
      I=I+1; ID(I)='S       '; ATOMIZATION(I)='S   '
      I=I+1; ID(I)='CL      '; ATOMIZATION(I)='CL   '
      I=I+1; ID(I)='BR      '; ATOMIZATION(I)='BR       '
      I=I+1; ID(I)='ALCL3    '; ATOMIZATION(I)='AL+3*CL  '
      I=I+1; ID(I)='ALF3     '; ATOMIZATION(I)='AL+3*F   '
      I=I+1; ID(I)='BCL3     '; ATOMIZATION(I)='B+3*CL   '
      I=I+1; ID(I)='BE2      '; ATOMIZATION(I)='2*BE     '
      I=I+1; ID(I)='BEH      '; ATOMIZATION(I)='BE+H     '
      I=I+1; ID(I)='BE       '; ATOMIZATION(I)='BE       '
      I=I+1; ID(I)='BF3      '; ATOMIZATION(I)='B+3*F    '
      I=I+1; ID(I)='C2CL4    '; ATOMIZATION(I)='2*C+4*CL '
      I=I+1; ID(I)='C2F4     '; ATOMIZATION(I)='2*C+4*F '
      I=I+1; ID(I)='C2H2     '; ATOMIZATION(I)='2*C+2*H  '
      I=I+1; ID(I)='C2H4NH_AZIRIDINE          '; ATOMIZATION(I)='2*C+5*H+N  '
      I=I+1; ID(I)='C2H4O_OXIRANE             '; ATOMIZATION(I)='2*C+4*H+O  '
      I=I+1; ID(I)='C2H4S_THIOOXIRANE         '; ATOMIZATION(I)='2*C+4*H+S  '
      I=I+1; ID(I)='C2H4     '; ATOMIZATION(I)='2*C+4*H  '
      I=I+1; ID(I)='CH3CH2CL_ETHYLCHLORIDE    '; ATOMIZATION(I)='2*C+5*H+CL  '
      I=I+1; ID(I)='C2H5OCH3_METHYLETHYLETHER '; ATOMIZATION(I)='3*C+8*H+O  '
      I=I+1; ID(I)='C2H5SH_ETHANTHIOL         '; ATOMIZATION(I)='2*C+6*H+S  '
      I=I+1; ID(I)='C2H6     '; ATOMIZATION(I)='2*C+6*H  '
      I=I+1; ID(I)='C2       '; ATOMIZATION(I)='2*C     '
      I=I+1; ID(I)='C3H4     '; ATOMIZATION(I)='3*C+4*H  '
      I=I+1; ID(I)='C3H6     '; ATOMIZATION(I)='3*C+6*H  '
      I=I+1; ID(I)='C3H8     '; ATOMIZATION(I)='3*C+8*H  '
      I=I+1; ID(I)='C4H10    '; ATOMIZATION(I)='4*C+10*H  '
      I=I+1; ID(I)='C4H10_ISOBUTANE           '; ATOMIZATION(I)='4*C+10*H  '
      I=I+1; ID(I)='C4H4O_FURAN               '; ATOMIZATION(I)='4*C+4*H+O  '
      I=I+1; ID(I)='C4H5N_PYRROLE             '; ATOMIZATION(I)='4*C+5*H+N  '
      I=I+1; ID(I)='C4H6_BUTADIENE            '; ATOMIZATION(I)='4*C+6*H  '
      I=I+1; ID(I)='C4H6_METHYLENECYCLOPROPANE'; ATOMIZATION(I)='4*C+6*H  '
      I=I+1; ID(I)='C4H6_CYCLOBUTENE          '; ATOMIZATION(I)='4*C+6*H  '
      I=I+1; ID(I)='C4H6_2BUTYNE              '; ATOMIZATION(I)='4*C+6*H  '
      I=I+1; ID(I)='C4H6_BICYCLOBUTANE        '; ATOMIZATION(I)='4*C+6*H  '
      I=I+1; ID(I)='C4H8_CYCLOBUTANE          '; ATOMIZATION(I)='4*C+8*H  '
      I=I+1; ID(I)='C4H8_ISOBUTENE            '; ATOMIZATION(I)='4*C+8*H  '
      I=I+1; ID(I)='C5H8_SPIROPENTANE         '; ATOMIZATION(I)='5*C+8*H  '
      I=I+1; ID(I)='C6H6_BENZENE              '; ATOMIZATION(I)='6*C+6*H  '
      I=I+1; ID(I)='CCL4                      '; ATOMIZATION(I)='C+4*CL  '
      I=I+1; ID(I)='CF2O                      '; ATOMIZATION(I)='C+2*F+O  '
      I=I+1; ID(I)='CF3CN                     '; ATOMIZATION(I)='2*C+3*F+N  '
      I=I+1; ID(I)='CF4                       '; ATOMIZATION(I)='C+4*F  '
      I=I+1; ID(I)='CH2CCH2_ALLENE            '; ATOMIZATION(I)='3*C+4*H  '
      I=I+1; ID(I)='CH2CHCL_VINYLCHLORIDE     '; ATOMIZATION(I)='2*C+3*H+CL  '
      I=I+1; ID(I)='CH2CHCN_ACRYLONITRILE     '; ATOMIZATION(I)='3*C+3*H+N  '
      I=I+1; ID(I)='CH2CHF                    '; ATOMIZATION(I)='2*C+3*H+F  '
      I=I+1; ID(I)='CH2CL2                    '; ATOMIZATION(I)='C+2*H+2*CL  '
      I=I+1; ID(I)='CH2CO_KETENE              '; ATOMIZATION(I)='2*C+2*H+O  '
      I=I+1; ID(I)='CH2F2                     '; ATOMIZATION(I)='C+2*H+2*F  '
      I=I+1; ID(I)='CH3_2CHOH_ISOPROPANOL     '; ATOMIZATION(I)='3*C+8*H+O  '
      I=I+1; ID(I)='CH3_2NH_DIMETHYLAMINE     '; ATOMIZATION(I)='2*C+7*H+N  '
      I=I+1; ID(I)='CH3_2SO_DIMETHYLSULFOXIDE '; ATOMIZATION(I)='2*C+6*H+S+O  '
      I=I+1; ID(I)='CH3_3C_TBUTYLRADICAL      '; ATOMIZATION(I)='4*C+9*H '
      I=I+1; ID(I)='CH3_3N_TRIMETHYLAMINE     '; ATOMIZATION(I)='3*C+9*H+N '
      I=I+1; ID(I)='CH3CCH_PROPYNE            '; ATOMIZATION(I)='3*C+4*H '
      I=I+1; ID(I)='CH3CH2CH2CL_PROPYLCHLORIDE'; ATOMIZATION(I)='3*C+7*H+CL '
      I=I+1; ID(I)='CH3CH2NH2_TRANSETHYLAMINE '; ATOMIZATION(I)='2*C+N+7*H '
      I=I+1; ID(I)='CH3CH2OH_ETHANOL          '; ATOMIZATION(I)='2*C+6*H+O '
      I=I+1; ID(I)='CH3CHCH2_PROPYLENE        '; ATOMIZATION(I)='3*C+6*H '
      I=I+1; ID(I)='CH3CHO_ACETALDEHYDE       '; ATOMIZATION(I)='2*C+4*H+O '
      I=I+1; ID(I)='CH3CL                     '; ATOMIZATION(I)='C+3*H+CL '
      I=I+1; ID(I)='CH3CN_METHYLCYANIDE       '; ATOMIZATION(I)='2*C+3*H+N '
      I=I+1; ID(I)='CH3COCH3_ACETONE          '; ATOMIZATION(I)='3*C+6*H+O '
      I=I+1; ID(I)='CH3COCL_ACETYLCHLORIDE    '; ATOMIZATION(I)='2*C+3*H+O+CL '
      I=I+1; ID(I)='CH3COF_ACETYLFLUORIDE     '; ATOMIZATION(I)='2*C+3*H+O+F '
      I=I+1; ID(I)='CH3NH2_METHYLAMINE        '; ATOMIZATION(I)='C+5*H+N '
      I=I+1; ID(I)='CH3NO2_NITROMETHANE       '; ATOMIZATION(I)='C+3*H+N+2*O '
      I=I+1; ID(I)='CH3OCH3_DIMETHYLETHER     '; ATOMIZATION(I)='2*C+6*H+O '
      I=I+1; ID(I)='CH3ONO_METHYLNITRITE      '; ATOMIZATION(I)='C+3*H+2*O+N '
      I=I+1; ID(I)='CH3SCH3_DIMETHYLSULFIDE   '; ATOMIZATION(I)='2*C+6*H+S '
      I=I+1; ID(I)='CH3SIH3_METHYLSILANE      '; ATOMIZATION(I)='C+6*H+SI '
      I=I+1; ID(I)='CH3      '; ATOMIZATION(I)='C+3*H    '
      I=I+1; ID(I)='CH3+      '; ATOMIZATION(I)='C+3*H    '
      I=I+1; ID(I)='CH4      '; ATOMIZATION(I)='C+4*H    '
      I=I+1; ID(I)='CHCL3      '; ATOMIZATION(I)='C+H+3*CL    '
      I=I+1; ID(I)='CHF3      '; ATOMIZATION(I)='C+H+3*F    '
      I=I+1; ID(I)='CH      '; ATOMIZATION(I)='C+H '
      I=I+1; ID(I)='CL2      '; ATOMIZATION(I)='2*CL     '
      I=I+1; ID(I)='CLF3      '; ATOMIZATION(I)='CL+3*F     '
      I=I+1; ID(I)='HCL      '; ATOMIZATION(I)='CL+H     '
      I=I+1; ID(I)='CLNO      '; ATOMIZATION(I)='CL+N+O     '
      I=I+1; ID(I)='CN       '; ATOMIZATION(I)='N+C      '
      I=I+1; ID(I)='CO2      '; ATOMIZATION(I)='2*O+C    '
      I=I+1; ID(I)='COS      '; ATOMIZATION(I)='C+O+S    '
      I=I+1; ID(I)='CO       '; ATOMIZATION(I)='C+O      '
      I=I+1; ID(I)='CS2       '; ATOMIZATION(I)='C+2*S      '
      I=I+1; ID(I)='F2O      '; ATOMIZATION(I)='2*F+O   '
      I=I+1; ID(I)='F2      '; ATOMIZATION(I)='2*F   '
      I=I+1; ID(I)='FCL      '; ATOMIZATION(I)='F+CL   '
      I=I+1; ID(I)='H2CO     '; ATOMIZATION(I)='C+O+2*H  '
      I=I+1; ID(I)='N2H4    '; ATOMIZATION(I)='2*N+4*H '
      I=I+1; ID(I)='H2O      '; ATOMIZATION(I)='O+2*H    '
      I=I+1; ID(I)='H2       '; ATOMIZATION(I)='2*H      '
      I=I+1; ID(I)='CH3OH     '; ATOMIZATION(I)='4H+C+O   '
      I=I+1; ID(I)='CH3SH     '; ATOMIZATION(I)='4H+C+S   '
      I=I+1; ID(I)='HCN      '; ATOMIZATION(I)='C+N+H    '
      I=I+1; ID(I)='C2H2O2_GLYOXAL      '; ATOMIZATION(I)='2*C+2*O+2*H    '
      I=I+1; ID(I)='C2H2O2_CISGLYOXAL   '; ATOMIZATION(I)='2*C+2*O+2*H    '
      I=I+1; ID(I)='HCOOCH3_METHYLFORMATE   '; ATOMIZATION(I)='2*C+2*O+4*H    '
      I=I+1; ID(I)='HCO      '; ATOMIZATION(I)='C+O+H    '
      I=I+1; ID(I)='HF       '; ATOMIZATION(I)='F+H      '
      I=I+1; ID(I)='HOCL       '; ATOMIZATION(I)='O+H+CL      '
      I=I+1; ID(I)='H2O2     '; ATOMIZATION(I)='2*O+2*H   '
      I=I+1; ID(I)='LI2      '; ATOMIZATION(I)='2*LI     '
      I=I+1; ID(I)='LIF      '; ATOMIZATION(I)='LI+F     '
      I=I+1; ID(I)='LIH      '; ATOMIZATION(I)='LI+H     '
      I=I+1; ID(I)='N2O      '; ATOMIZATION(I)='2*N+O    '
      I=I+1; ID(I)='N2       '; ATOMIZATION(I)='2*N      '
      I=I+1; ID(I)='NA2      '; ATOMIZATION(I)='2*NA     '
      I=I+1; ID(I)='NACL     '; ATOMIZATION(I)='CL+NA'
      I=I+1; ID(I)='NCCN_CYANOGEN  '; ATOMIZATION(I)='2*C+2*N'
      I=I+1; ID(I)='NF3     '; ATOMIZATION(I)='N+3*F   '
      I=I+1; ID(I)='NH2      '; ATOMIZATION(I)='N+2*H    '
      I=I+1; ID(I)='NH3      '; ATOMIZATION(I)='N+3*H    '
      I=I+1; ID(I)='NH       '; ATOMIZATION(I)='N+H      '
      I=I+1; ID(I)='NO2       '; ATOMIZATION(I)='N+2*O      '
      I=I+1; ID(I)='NO       '; ATOMIZATION(I)='N+O      '
      I=I+1; ID(I)='O2       '; ATOMIZATION(I)='2*O      '
      I=I+1; ID(I)='O3       '; ATOMIZATION(I)='3*O      '
      I=I+1; ID(I)='O3_CYCLE '; ATOMIZATION(I)='3*O      '
      I=I+1; ID(I)='OH       '; ATOMIZATION(I)='O+H      '
      I=I+1; ID(I)='P2       '; ATOMIZATION(I)='2*P      '
      I=I+1; ID(I)='PF3      '; ATOMIZATION(I)='P+3*F    '
      I=I+1; ID(I)='PH2      '; ATOMIZATION(I)='P+2*H    '
      I=I+1; ID(I)='PH3      '; ATOMIZATION(I)='P+3*H    '
      I=I+1; ID(I)='S2       '; ATOMIZATION(I)='2*S      '
      I=I+1; ID(I)='SC       '; ATOMIZATION(I)='C+S      '
      I=I+1; ID(I)='H2S      '; ATOMIZATION(I)='S+2*H    '
      I=I+1; ID(I)='SH2      '; ATOMIZATION(I)='S+2*H    '
      I=I+1; ID(I)='SI2H6    '; ATOMIZATION(I)='2*SI+6*H '
      I=I+1; ID(I)='SI2      '; ATOMIZATION(I)='2*SI     '
      I=I+1; ID(I)='SI2_SINGLET      '; ATOMIZATION(I)='2*SI     '
      I=I+1; ID(I)='SICL4     '; ATOMIZATION(I)='SI+4*CL     '
      I=I+1; ID(I)='SIC     '; ATOMIZATION(I)='SI+C     '
      I=I+1; ID(I)='SIF4     '; ATOMIZATION(I)='SI+4*F     '
      I=I+1; ID(I)='SIH3     '; ATOMIZATION(I)='SI+3*H   '
      I=I+1; ID(I)='SIO      '; ATOMIZATION(I)='SI+O     '
      I=I+1; ID(I)='SO2      '; ATOMIZATION(I)='S+2*O    '
      I=I+1; ID(I)='SO       '; ATOMIZATION(I)='S+O      '
!     ==========================================================================
!     == MULECULES UP TO HERE BELONG TO THE G2-1 DATASET                      ==
!     ==========================================================================
      I=I+1; ID(I)='CH       '; ATOMIZATION(I)='C+H      '
      I=I+1; ID(I)='CH2_3B1  '; ATOMIZATION(I)='C+2*H    '
      I=I+1; ID(I)='CH2_1A1  '; ATOMIZATION(I)='C+2*H    '
      I=I+1; ID(I)='CH2_1A1_AF'; ATOMIZATION(I)='C+2*H    '
      I=I+1; ID(I)='CH3OH    '; ATOMIZATION(I)='C+O+4*H  '
      I=I+1; ID(I)='N2H4     '; ATOMIZATION(I)='2*N+4*H  '
      I=I+1; ID(I)='F2       '; ATOMIZATION(I)='2*F      '
      I=I+1; ID(I)='SIH2_1A1 '; ATOMIZATION(I)='SI+2*H   '
      I=I+1; ID(I)='SIH2_3B1 '; ATOMIZATION(I)='SI+2*H   '
      I=I+1; ID(I)='SIH4     '; ATOMIZATION(I)='SI+4*H   '
      I=I+1; ID(I)='SC       '; ATOMIZATION(I)='S+C      '  !MUST BE SC TO AVOID CONFUSION WITH CESIUM
      I=I+1; ID(I)='CLO      '; ATOMIZATION(I)='CL+O     '
      I=I+1; ID(I)='FCL      '; ATOMIZATION(I)='CL+F     '
      I=I+1; ID(I)='CH3CL    '; ATOMIZATION(I)='CL+C+3*H '
      I=I+1; ID(I)='CH3SH    '; ATOMIZATION(I)='S+C+4*H  '
      I=I+1; ID(I)='HOCL     '; ATOMIZATION(I)='CL+O+H   '
      IF(I.NE.NDATA) THEN
        PRINT*,'====ERROR IN ATOMIZE INI====='
        PRINT*,'NDATA ',I,NDATA
        STOP 'IN ATOMIZE_INI'
      END IF
!
!     ========================================================================== 
!     ========================================================================== 
!     ========================================================================== 
      DO I=1,NDATA
        STRING=ATOMIZATION(I)
        NUM(:,I)=0
        ATOM(:,I)=' '
        IATOM=1
        J=0
!       == STEP LETTER BY LETTER THROUGH THE STRING
        DO 
          J=J+1
          IASCII=ICHAR(STRING(J:J))
          IF(IASCII.GE.97.AND.IASCII.LE.122) IASCII=IASCII-97+65  ! MAKE UPPERCASE
          IF(IASCII.EQ.43) THEN    !'+'
            IATOM=IATOM+1
          ELSE IF(IASCII.EQ.42) THEN    !'*'
          ELSE IF(IASCII.GE.48.AND.IASCII.LE.57) THEN    ! NUMBER
            NUMBER=IASCII-48
!           == COMPLICATED BECAUSE IT SHOULD WORK FOR ARBITRARY-DIGIT NUMBERS
            NUM(IATOM,I)=10*NUM(IATOM,I)+NUMBER
          ELSE IF(IASCII.GE.65.AND.IASCII.LE.90) THEN    ! UPPERCASE LETTER
            IF(NUM(IATOM,I).EQ.0) NUM(IATOM,I)=1
            ATOM(IATOM,I)(1:2)=STRING(J:J)
            IASCII=ICHAR(STRING(J+1:J+1))
            IF(IASCII.GE.97.AND.IASCII.LE.122) IASCII=IASCII-97+65  ! MAKE UPPERCASE
            IF(IASCII.GE.65.AND.IASCII.LE.90) THEN    ! UPPERCASE LETTER
              J=J+1
              ATOM(IATOM,I)(2:2)=STRING(J:J)
            END IF
          ELSE IF(IASCII.EQ.32) THEN   ! SPACE
            EXIT
          ELSE
            PRINT*,'PARSING STRING FAILED'
            PRINT*,'STRING=',STRING
            STOP 'ERROR IN ATOMIZE_INI'
          END IF
        ENDDO
!        PRINT*,ID(I),NUM(:,I),ATOM(:,I)
      ENDDO
      RETURN
      END 
!     
!     ..........................................................................
      SUBROUTINE ATOMIZE__GET(ID_,LEN,NUM_,ATOM_)
      USE ATOMIZE_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ID_
      INTEGER(4)  ,INTENT(IN) :: LEN
      INTEGER(4)  ,INTENT(OUT):: NUM_(LEN)
      CHARACTER(2),INTENT(OUT):: ATOM_(LEN)
      INTEGER(4)              :: I,J
!     **************************************************************************
!     ==========================================================================
!     == LOOK UP THE COMPOSITION OF ALL MOLECULES IN THE DATA BASE            ==
!     ==========================================================================
      CALL ATOMIZE_INI()
!
!     ==========================================================================
!     == LOOK UP THE COMPOSITION OF ALL MOLECULES IN THE DATA BASE            ==
!     ==========================================================================
      NUM_(:)=0
      ATOM_(:)='  '
      DO I=1,NDATA
        IF(ID_.EQ.ID(I)) THEN
          DO J=1,NATOMX
            IF(NUM(J,I).EQ.0) EXIT
            NUM_(J) =NUM(J,I)
            ATOM_(J)=ATOM(J,I)
          ENDDO
          RETURN
        END IF
      ENDDO
      PRINT*,'=====ERROR========'
      PRINT*,'ID NOT RECOGNIZED: ',ID_
      STOP 'IN ATOMIZE__GET'
      END
!
!........1.........2.........3.........4.........5.........6.........7.........8
MODULE gpaw_MODULE
!*******************************************************************************
!***                                                                         ***
!***                                                                         ***
!***                                                                         ***
!*******************************************************************************
INTEGER(4),PARAMETER :: NDATA=55
CHARACTER(32)        :: ID(NDATA)   ! molecule id
CHARACTER(32)        :: name(NDATA) ! latex string with the formula
real(8)              :: egpaw(NDATA)
real(8)              :: evasp(NDATA)
LOGICAL(4)           :: TINI=.FALSE.
END MODULE GPAW_MODULE
!
!     ...1.........2.........3.........4.........5.........6.........7.........8
      SUBROUTINE gpaw_INI()
!     **************************************************************************
!     **  COMPOSITION FOR EACH OF THE MOLECULES IN THE SET                    **
!     ** 
!     ** Atomization with functional PBE energies                             **
!     ** retrieved from                                                       **
!     ** https://wiki.fysik.dtu.dk/gpaw/setups/molecule_tests.html            **
!     ** on Oct.10, 2015                                                      **
!     **                                                                      **
!     ** VASP= J. Paier, R. Hirschl, M. Marsman and G. Kresse,                **
!     ** J. Chem. Phys. 122, 234102 (2005)                                    **
!     **************************************************************************
      USE gpaw_MODULE
      IMPLICIT NONE
      REAL(8),PARAMETER    :: EV=1.D0/27.21138505D0
!     **************************************************************************
      IF(TINI) RETURN
      TINI=.TRUE.
      name (  1)='\rm{BeH}'
      id   (  1)='BEH'
      egpaw(  1)=   2.39900d0*ev
      evasp(  1)=   2.40700d0*ev
      name (  2)='\rm{C}_2\rm{H}_2'
      id   (  2)='C2H2'
      egpaw(  2)=  18.04600d0*ev
      evasp(  2)=  17.97400d0*ev
      name (  3)='\rm{C}_2\rm{H}_4'
      id   (  3)='C2H4'
      egpaw(  3)=  24.83700d0*ev
      evasp(  3)=  24.76100d0*ev
      name (  4)='\rm{C}_2\rm{H}_6'
      id   (  4)='C2H6'
      egpaw(  4)=  31.12900d0*ev
      evasp(  4)=  31.04900d0*ev
      name (  5)='\rm{CH}'
      id   (  5)='CH'
      egpaw(  5)=   3.67500d0*ev
      evasp(  5)=   3.67300d0*ev
      name (  6)='\rm{CH}_2(^1\rm{A}_1)'
      id   (  6)='CH2_1A1'
      egpaw(  6)=   7.76700d0*ev
      evasp(  6)=   7.75400d0*ev
      name (  7)='\rm{CH}_2(^3\rm{B}_1)'
      id   (  7)='CH2_3B1'
      egpaw(  7)=   8.44100d0*ev
      evasp(  7)=   8.43000d0*ev
      name (  8)='\rm{CH}_3'
      id   (  8)='CH3'
      egpaw(  8)=  13.45800d0*ev
      evasp(  8)=  13.43000d0*ev
      name (  9)='\rm{CH}_3\rm{Cl}'
      id   (  9)='CH3CL'
      egpaw(  9)=  17.35600d0*ev
      evasp(  9)=  17.32000d0*ev
      name ( 10)='\rm{H}_3\rm{COH}'
      id   ( 10)='CH3OH'
      egpaw( 10)=  22.55200d0*ev
      evasp( 10)=  22.51900d0*ev
      name ( 11)='\rm{H}_3\rm{CSH}'
      id   ( 11)='CH3SH'
      egpaw( 11)=  20.74500d0*ev
      evasp( 11)=  20.71900d0*ev
      name ( 12)='\rm{CH}_4'
      id   ( 12)='CH4'
      egpaw( 12)=  18.23300d0*ev
      evasp( 12)=  18.19600d0*ev
      name ( 13)='\rm{CN}'
      id   ( 13)='CN'
      egpaw( 13)=   8.53500d0*ev
      evasp( 13)=   8.56400d0*ev
      name ( 14)='\rm{CO}'
      id   ( 14)='CO'
      egpaw( 14)=  11.61700d0*ev
      evasp( 14)=  11.64800d0*ev
      name ( 15)='\rm{CO}_2'
      id   ( 15)='CO2'
      egpaw( 15)=  18.00200d0*ev
      evasp( 15)=  18.01300d0*ev
      name ( 16)='\rm{SC}'
      id   ( 16)='SC'
      egpaw( 16)=   7.79000d0*ev
      evasp( 16)=   7.78400d0*ev
      name ( 17)='\rm{Cl}_2'
      id   ( 17)='CL2'
      egpaw( 17)=   2.84100d0*ev
      evasp( 17)=   2.85300d0*ev
      name ( 18)='\rm{FCl}'
      id   ( 18)='FCL'
      egpaw( 18)=   3.13400d0*ev
      evasp( 18)=   3.13500d0*ev
      name ( 19)='\rm{ClO}'
      id   ( 19)='CLO'
      egpaw( 19)=   3.51500d0*ev
      evasp( 19)=   3.53900d0*ev
      name ( 20)='\rm{F}_2'
      id   ( 20)='F2'
      egpaw( 20)=   2.31200d0*ev
      evasp( 20)=   2.28100d0*ev
      name ( 21)='\rm{H}_2\rm{CO}'
      id   ( 21)='H2CO'
      egpaw( 21)=  16.73400d0*ev
      evasp( 21)=  16.71700d0*ev
      name ( 22)='\rm{H}_2\rm{O}'
      id   ( 22)='H2O'
      egpaw( 22)=  10.11900d0*ev
      evasp( 22)=  10.13400d0*ev
      name ( 23)='\rm{HOOH}'
      id   ( 23)='H2O2'
      egpaw( 23)=  12.20400d0*ev
      evasp( 23)=  12.21100d0*ev
      name ( 24)='\rm{HCN}'
      id   ( 24)='HCN'
      egpaw( 24)=  14.19000d0*ev
      evasp( 24)=  14.15000d0*ev
      name ( 25)='\rm{HCO}'
      id   ( 25)='HCO'
      egpaw( 25)=  12.79000d0*ev
      evasp( 25)=  12.78800d0*ev
      name ( 26)='\rm{HCl}'
      id   ( 26)='HCL'
      egpaw( 26)=   4.60600d0*ev
      evasp( 26)=   4.61000d0*ev
      name ( 27)='\rm{HF}'
      id   ( 27)='HF'
      egpaw( 27)=   6.15600d0*ev
      evasp( 27)=   6.13600d0*ev
      name ( 28)='\rm{HOCl}'
      id   ( 28)='HOCL'
      egpaw( 28)=   7.59000d0*ev
      evasp( 28)=   7.59700d0*ev
      name ( 29)='\rm{Li}_2'
      id   ( 29)='LI2'
      egpaw( 29)=   0.86500d0*ev
      evasp( 29)=   0.86300d0*ev
      name ( 30)='\rm{LiF}'
      id   ( 30)='LIF'
      egpaw( 30)=   5.98100d0*ev
      evasp( 30)=   6.00200d0*ev
      name ( 31)='\rm{LiH}'
      id   ( 31)='LIH'
      egpaw( 31)=   2.33100d0*ev
      evasp( 31)=   2.32000d0*ev
      name ( 32)='\rm{N}_2'
      id   ( 32)='N2'
      egpaw( 32)=  10.57200d0*ev
      evasp( 32)=  10.56800d0*ev
      name ( 33)='\rm{H}_2\rm{NNH}_2'
      id   ( 33)='N2H4'
      egpaw( 33)=  19.71300d0*ev
      evasp( 33)=  19.63100d0*ev
      name ( 34)='\rm{NH}'
      id   ( 34)='NH'
      egpaw( 34)=   3.83600d0*ev
      evasp( 34)=   3.84200d0*ev
      name ( 35)='\rm{NH}_2'
      id   ( 35)='NH2'
      egpaw( 35)=   8.18600d0*ev
      evasp( 35)=   8.18300d0*ev
      name ( 36)='\rm{NH}_3'
      id   ( 36)='NH3'
      egpaw( 36)=  13.12400d0*ev
      evasp( 36)=  13.08300d0*ev
      name ( 37)='\rm{NO}'
      id   ( 37)='NO'
      egpaw( 37)=   7.41700d0*ev
      evasp( 37)=   7.45900d0*ev
      name ( 38)='\rm{Na}_2'
      id   ( 38)='NA2'
      egpaw( 38)=   0.76600d0*ev
      evasp( 38)=   0.76800d0*ev
      name ( 39)='\rm{NaCl}'
      id   ( 39)='NACL'
      egpaw( 39)=   4.10100d0*ev
      evasp( 39)=   4.05900d0*ev
      name ( 40)='\rm{O}_2'
      id   ( 40)='O2'
      egpaw( 40)=   6.15800d0*ev
      evasp( 40)=   6.21400d0*ev
      name ( 41)='\rm{OH}'
      id   ( 41)='OH'
      egpaw( 41)=   4.74900d0*ev
      evasp( 41)=   4.75700d0*ev
      name ( 42)='\rm{P}_2'
      id   ( 42)='P2'
      egpaw( 42)=   5.23200d0*ev
      evasp( 42)=   5.26900d0*ev
      name ( 43)='\rm{PH}_2'
      id   ( 43)='PH2'
      egpaw( 43)=   6.67400d0*ev
      evasp( 43)=   6.70000d0*ev
      name ( 44)='\rm{PH}_3'
      id   ( 44)='PH3'
      egpaw( 44)=  10.32900d0*ev
      evasp( 44)=  10.36400d0*ev
      name ( 45)='\rm{S}_2'
      id   ( 45)='S2'
      egpaw( 45)=   4.98000d0*ev
      evasp( 45)=   5.00400d0*ev
      name ( 46)='\rm{SH}_2'
      id   ( 46)='SH2'
      egpaw( 46)=   7.87500d0*ev
      evasp( 46)=   7.89200d0*ev
      name ( 47)='\rm{SO}'
      id   ( 47)='SO'
      egpaw( 47)=   6.08500d0*ev
      evasp( 47)=   6.13600d0*ev
      name ( 48)='\rm{SO}_2'
      id   ( 48)='SO2'
      egpaw( 48)=  12.05700d0*ev
      evasp( 48)=  12.19000d0*ev
      name ( 49)='\rm{Si}_2'
      id   ( 49)='SI2'
      egpaw( 49)=   3.50700d0*ev
      evasp( 49)=   3.52600d0*ev
      name ( 50)='\rm{Si}_2\rm{H}_6'
      id   ( 50)='SI2H6'
      egpaw( 50)=  22.45800d0*ev
      evasp( 50)=  22.52800d0*ev
      name ( 51)='\rm{SiH}_2(^1\rm{A}_1)'
      id   ( 51)='SIH2_1A1'
      egpaw( 51)=   6.38800d0*ev
      evasp( 51)=   6.41400d0*ev
      name ( 52)='\rm{SiH}_2(^3\rm{B}_1)'
      id   ( 52)='SIH2_3B1'
      egpaw( 52)=   5.67000d0*ev
      evasp( 52)=   5.69400d0*ev
      name ( 53)='\rm{SiH}_3'
      id   ( 53)='SIH3'
      egpaw( 53)=   9.60100d0*ev
      evasp( 53)=   9.63600d0*ev
      name ( 54)='\rm{SiH}_4'
      id   ( 54)='SIH4'
      egpaw( 54)=  13.54200d0*ev
      evasp( 54)=  13.58600d0*ev
      name ( 55)='\rm{SiO}'
      id   ( 55)='SIO'
      egpaw( 55)=   8.42800d0*ev
      evasp( 55)=   8.48200d0*ev
      RETURN
      END 
!     
!     ..........................................................................
      SUBROUTINE gpaw__GET(MOLID,DFTID,VAL,TCHK)
      USE gpaw_MODULE
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: MOLID
      CHARACTER(*),INTENT(IN) :: DFTID
      REAL(8)     ,INTENT(OUT):: VAL
      LOGICAL(4)  ,INTENT(OUT):: TCHK
      INTEGER(4)              :: I
!     *********************************************************************
      CALL GPAW_INI
      VAL=0.D0
      DO I=1,NDATA
        TCHK=(TRIM(ID(I)).EQ.TRIM(MOLID))
        IF(TCHK) THEN
          IF(DFTID.EQ.'VASP') THEN
            VAL=EVASP(I)
            RETURN
          ELSE IF(DFTID.EQ.'GPAW') THEN
            VAL=EGPAW(i)
            RETURN
          ELSE
            PRINT*,'===== ERROR======='
            PRINT*,'DFTID NOT RECOGNIZED'
            PRINT*,'DFTID:',DFTID
            PRINT*,'MOLID:',MOLID
            STOP 'GPAW__GET'
          END IF
        END IF
      ENDDO
      VAL=0.D0
      RETURN
      END SUBROUTINE GPAW__GET
