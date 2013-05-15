*DECK DBESI
      SUBROUTINE DBESI (X, ALPHA, KODE, N, Y, NZ)
C***BEGIN PROLOGUE  DBESI
C***PURPOSE  Compute an N member sequence of I Bessel functions
C            I/SUB(ALPHA+K-1)/(X), K=1,...,N or scaled Bessel functions
C            EXP(-X)*I/SUB(ALPHA+K-1)/(X), K=1,...,N for nonnegative
C            ALPHA and X.
C***LIBRARY   SLATEC
C***CATEGORY  C10B3
C***TYPE      DOUBLE PRECISION (BESI-S, DBESI-D)
C***KEYWORDS  I BESSEL FUNCTION, SPECIAL FUNCTIONS
C***AUTHOR  Amos, D. E., (SNLA)
C           Daniel, S. L., (SNLA)
C***DESCRIPTION
C
C     Abstract  **** a double precision routine ****
C         DBESI computes an N member sequence of I Bessel functions
C         I/sub(ALPHA+K-1)/(X), K=1,...,N or scaled Bessel functions
C         EXP(-X)*I/sub(ALPHA+K-1)/(X), K=1,...,N for nonnegative ALPHA
C         and X.  A combination of the power series, the asymptotic
C         expansion for X to infinity, and the uniform asymptotic
C         expansion for NU to infinity are applied over subdivisions of
C         the (NU,X) plane.  For values not covered by one of these
C         formulae, the order is incremented by an integer so that one
C         of these formulae apply.  Backward recursion is used to reduce
C         orders by integer values.  The asymptotic expansion for X to
C         infinity is used only when the entire sequence (specifically
C         the last member) lies within the region covered by the
C         expansion.  Leading terms of these expansions are used to test
C         for over or underflow where appropriate.  If a sequence is
C         requested and the last member would underflow, the result is
C         set to zero and the next lower order tried, etc., until a
C         member comes on scale or all are set to zero.  An overflow
C         cannot occur with scaling.
C
C         The maximum number of significant digits obtainable
C         is the smaller of 14 and the number of digits carried in
C         double precision arithmetic.
C
C     Description of Arguments
C
C         Input      X,ALPHA are double precision
C           X      - X .GE. 0.0D0
C           ALPHA  - order of first member of the sequence,
C                    ALPHA .GE. 0.0D0
C           KODE   - a parameter to indicate the scaling option
C                    KODE=1 returns
C                           Y(K)=        I/sub(ALPHA+K-1)/(X),
C                                K=1,...,N
C                    KODE=2 returns
C                           Y(K)=EXP(-X)*I/sub(ALPHA+K-1)/(X),
C                                K=1,...,N
C           N      - number of members in the sequence, N .GE. 1
C
C         Output     Y is double precision
C           Y      - a vector whose first N components contain
C                    values for I/sub(ALPHA+K-1)/(X) or scaled
C                    values for EXP(-X)*I/sub(ALPHA+K-1)/(X),
C                    K=1,...,N depending on KODE
C           NZ     - number of components of Y set to zero due to
C                    underflow,
C                    NZ=0   , normal return, computation completed
C                    NZ .NE. 0, last NZ components of Y set to zero,
C                             Y(K)=0.0D0, K=N-NZ+1,...,N.
C
C     Error Conditions
C         Improper input arguments - a fatal error
C         Overflow with KODE=1 - a fatal error
C         Underflow - a non-fatal error(NZ .NE. 0)
C
C***REFERENCES  D. E. Amos, S. L. Daniel and M. K. Weston, CDC 6600
C                 subroutines IBESS and JBESS for Bessel functions
C                 I(NU,X) and J(NU,X), X .GE. 0, NU .GE. 0, ACM
C                 Transactions on Mathematical Software 3, (1977),
C                 pp. 76-92.
C               F. W. J. Olver, Tables of Bessel Functions of Moderate
C                 or Large Orders, NPL Mathematical Tables 6, Her
C                 Majesty's Stationery Office, London, 1962.
C***ROUTINES CALLED  D1MACH, DASYIK, DLNGAM, I1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   750101  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DBESI
C
      INTEGER I, IALP, IN, INLIM, IS, I1, K, KK, KM, KODE, KT,
     1 N, NN, NS, NZ
      INTEGER I1MACH
      DOUBLE PRECISION AIN,AK,AKM,ALPHA,ANS,AP,ARG,ATOL,TOLLN,DFN,
     1 DTM, DX, EARG, ELIM, ETX, FLGIK,FN, FNF, FNI,FNP1,FNU,GLN,RA,
     2 RTTPI, S, SX, SXO2, S1, S2, T, TA, TB, TEMP, TFN, TM, TOL,
     3 TRX, T2, X, XO2, XO2L, Y, Z
      DOUBLE PRECISION D1MACH, DLNGAM
      DIMENSION Y(*), TEMP(3)
      SAVE RTTPI, INLIM
      DATA RTTPI           / 3.98942280401433D-01/
      DATA INLIM           /          80         /
C***FIRST EXECUTABLE STATEMENT  DBESI
      NZ = 0
      KT = 1
C     I1MACH(15) REPLACES I1MACH(12) IN A DOUBLE PRECISION CODE
C     I1MACH(14) REPLACES I1MACH(11) IN A DOUBLE PRECISION CODE
      RA = D1MACH(3)
      TOL = MAX(RA,1.0D-15)
      I1 = -I1MACH(15)
      GLN = D1MACH(5)
      ELIM = 2.303D0*(I1*GLN-3.0D0)
C     TOLLN = -LN(TOL)
      I1 = I1MACH(14)+1
      TOLLN = 2.303D0*GLN*I1
      TOLLN = MIN(TOLLN,34.5388D0)
      IF (N-1) 590, 10, 20
   10 KT = 2
   20 NN = N
      IF (KODE.LT.1 .OR. KODE.GT.2) GO TO 570
      IF (X) 600, 30, 80
   30 IF (ALPHA) 580, 40, 50
   40 Y(1) = 1.0D0
      IF (N.EQ.1) RETURN
      I1 = 2
      GO TO 60
   50 I1 = 1
   60 DO 70 I=I1,N
        Y(I) = 0.0D0
   70 CONTINUE
      RETURN
   80 CONTINUE
      IF (ALPHA.LT.0.0D0) GO TO 580
C
      IALP = INT(ALPHA)
      FNI = IALP + N - 1
      FNF = ALPHA - IALP
      DFN = FNI + FNF
      FNU = DFN
      IN = 0
      XO2 = X*0.5D0
      SXO2 = XO2*XO2
      ETX = KODE - 1
      SX = ETX*X
C
C     DECISION TREE FOR REGION WHERE SERIES, ASYMPTOTIC EXPANSION FOR X
C     TO INFINITY AND ASYMPTOTIC EXPANSION FOR NU TO INFINITY ARE
C     APPLIED.
C
      IF (SXO2.LE.(FNU+1.0D0)) GO TO 90
      IF (X.LE.12.0D0) GO TO 110
      FN = 0.55D0*FNU*FNU
      FN = MAX(17.0D0,FN)
      IF (X.GE.FN) GO TO 430
      ANS = MAX(36.0D0-FNU,0.0D0)
      NS = INT(ANS)
      FNI = FNI + NS
      DFN = FNI + FNF
      FN = DFN
      IS = KT
      KM = N - 1 + NS
      IF (KM.GT.0) IS = 3
      GO TO 120
   90 FN = FNU
      FNP1 = FN + 1.0D0
      XO2L = LOG(XO2)
      IS = KT
      IF (X.LE.0.5D0) GO TO 230
      NS = 0
  100 FNI = FNI + NS
      DFN = FNI + FNF
      FN = DFN
      FNP1 = FN + 1.0D0
      IS = KT
      IF (N-1+NS.GT.0) IS = 3
      GO TO 230
  110 XO2L = LOG(XO2)
      NS = INT(SXO2-FNU)
      GO TO 100
  120 CONTINUE
C
C     OVERFLOW TEST ON UNIFORM ASYMPTOTIC EXPANSION
C
      IF (KODE.EQ.2) GO TO 130
      IF (ALPHA.LT.1.0D0) GO TO 150
      Z = X/ALPHA
      RA = SQRT(1.0D0+Z*Z)
      GLN = LOG((1.0D0+RA)/Z)
      T = RA*(1.0D0-ETX) + ETX/(Z+RA)
      ARG = ALPHA*(T-GLN)
      IF (ARG.GT.ELIM) GO TO 610
      IF (KM.EQ.0) GO TO 140
  130 CONTINUE
C
C     UNDERFLOW TEST ON UNIFORM ASYMPTOTIC EXPANSION
C
      Z = X/FN
      RA = SQRT(1.0D0+Z*Z)
      GLN = LOG((1.0D0+RA)/Z)
      T = RA*(1.0D0-ETX) + ETX/(Z+RA)
      ARG = FN*(T-GLN)
  140 IF (ARG.LT.(-ELIM)) GO TO 280
      GO TO 190
  150 IF (X.GT.ELIM) GO TO 610
      GO TO 130
C
C     UNIFORM ASYMPTOTIC EXPANSION FOR NU TO INFINITY
C
  160 IF (KM.NE.0) GO TO 170
      Y(1) = TEMP(3)
      RETURN
  170 TEMP(1) = TEMP(3)
      IN = NS
      KT = 1
      I1 = 0
  180 CONTINUE
      IS = 2
      FNI = FNI - 1.0D0
      DFN = FNI + FNF
      FN = DFN
      IF(I1.EQ.2) GO TO 350
      Z = X/FN
      RA = SQRT(1.0D0+Z*Z)
      GLN = LOG((1.0D0+RA)/Z)
      T = RA*(1.0D0-ETX) + ETX/(Z+RA)
      ARG = FN*(T-GLN)
  190 CONTINUE
      I1 = ABS(3-IS)
      I1 = MAX(I1,1)
      FLGIK = 1.0D0
      CALL DASYIK(X,FN,KODE,FLGIK,RA,ARG,I1,TEMP(IS))
      GO TO (180, 350, 510), IS
C
C     SERIES FOR (X/2)**2.LE.NU+1
C
  230 CONTINUE
      GLN = DLNGAM(FNP1)
      ARG = FN*XO2L - GLN - SX
      IF (ARG.LT.(-ELIM)) GO TO 300
      EARG = EXP(ARG)
  240 CONTINUE
      S = 1.0D0
      IF (X.LT.TOL) GO TO 260
      AK = 3.0D0
      T2 = 1.0D0
      T = 1.0D0
      S1 = FN
      DO 250 K=1,17
        S2 = T2 + S1
        T = T*SXO2/S2
        S = S + T
        IF (ABS(T).LT.TOL) GO TO 260
        T2 = T2 + AK
        AK = AK + 2.0D0
        S1 = S1 + FN
  250 CONTINUE
  260 CONTINUE
      TEMP(IS) = S*EARG
      GO TO (270, 350, 500), IS
  270 EARG = EARG*FN/XO2
      FNI = FNI - 1.0D0
      DFN = FNI + FNF
      FN = DFN
      IS = 2
      GO TO 240
C
C     SET UNDERFLOW VALUE AND UPDATE PARAMETERS
C
  280 Y(NN) = 0.0D0
      NN = NN - 1
      FNI = FNI - 1.0D0
      DFN = FNI + FNF
      FN = DFN
      IF (NN-1) 340, 290, 130
  290 KT = 2
      IS = 2
      GO TO 130
  300 Y(NN) = 0.0D0
      NN = NN - 1
      FNP1 = FN
      FNI = FNI - 1.0D0
      DFN = FNI + FNF
      FN = DFN
      IF (NN-1) 340, 310, 320
  310 KT = 2
      IS = 2
  320 IF (SXO2.LE.FNP1) GO TO 330
      GO TO 130
  330 ARG = ARG - XO2L + LOG(FNP1)
      IF (ARG.LT.(-ELIM)) GO TO 300
      GO TO 230
  340 NZ = N - NN
      RETURN
C
C     BACKWARD RECURSION SECTION
C
  350 CONTINUE
      NZ = N - NN
  360 CONTINUE
      IF(KT.EQ.2) GO TO 420
      S1 = TEMP(1)
      S2 = TEMP(2)
      TRX = 2.0D0/X
      DTM = FNI
      TM = (DTM+FNF)*TRX
      IF (IN.EQ.0) GO TO 390
C     BACKWARD RECUR TO INDEX ALPHA+NN-1
      DO 380 I=1,IN
        S = S2
        S2 = TM*S2 + S1
        S1 = S
        DTM = DTM - 1.0D0
        TM = (DTM+FNF)*TRX
  380 CONTINUE
      Y(NN) = S1
      IF (NN.EQ.1) RETURN
      Y(NN-1) = S2
      IF (NN.EQ.2) RETURN
      GO TO 400
  390 CONTINUE
C     BACKWARD RECUR FROM INDEX ALPHA+NN-1 TO ALPHA
      Y(NN) = S1
      Y(NN-1) = S2
      IF (NN.EQ.2) RETURN
  400 K = NN + 1
      DO 410 I=3,NN
        K = K - 1
        Y(K-2) = TM*Y(K-1) + Y(K)
        DTM = DTM - 1.0D0
        TM = (DTM+FNF)*TRX
  410 CONTINUE
      RETURN
  420 Y(1) = TEMP(2)
      RETURN
C
C     ASYMPTOTIC EXPANSION FOR X TO INFINITY
C
  430 CONTINUE
      EARG = RTTPI/SQRT(X)
      IF (KODE.EQ.2) GO TO 440
      IF (X.GT.ELIM) GO TO 610
      EARG = EARG*EXP(X)
  440 ETX = 8.0D0*X
      IS = KT
      IN = 0
      FN = FNU
  450 DX = FNI + FNI
      TM = 0.0D0
      IF (FNI.EQ.0.0D0 .AND. ABS(FNF).LT.TOL) GO TO 460
      TM = 4.0D0*FNF*(FNI+FNI+FNF)
  460 CONTINUE
      DTM = DX*DX
      S1 = ETX
      TRX = DTM - 1.0D0
      DX = -(TRX+TM)/ETX
      T = DX
      S = 1.0D0 + DX
      ATOL = TOL*ABS(S)
      S2 = 1.0D0
      AK = 8.0D0
      DO 470 K=1,25
        S1 = S1 + ETX
        S2 = S2 + AK
        DX = DTM - S2
        AP = DX + TM
        T = -T*AP/S1
        S = S + T
        IF (ABS(T).LE.ATOL) GO TO 480
        AK = AK + 8.0D0
  470 CONTINUE
  480 TEMP(IS) = S*EARG
      IF(IS.EQ.2) GO TO 360
      IS = 2
      FNI = FNI - 1.0D0
      DFN = FNI + FNF
      FN = DFN
      GO TO 450
C
C     BACKWARD RECURSION WITH NORMALIZATION BY
C     ASYMPTOTIC EXPANSION FOR NU TO INFINITY OR POWER SERIES.
C
  500 CONTINUE
C     COMPUTATION OF LAST ORDER FOR SERIES NORMALIZATION
      AKM = MAX(3.0D0-FN,0.0D0)
      KM = INT(AKM)
      TFN = FN + KM
      TA = (GLN+TFN-0.9189385332D0-0.0833333333D0/TFN)/(TFN+0.5D0)
      TA = XO2L - TA
      TB = -(1.0D0-1.0D0/TFN)/TFN
      AIN = TOLLN/(-TA+SQRT(TA*TA-TOLLN*TB)) + 1.5D0
      IN = INT(AIN)
      IN = IN + KM
      GO TO 520
  510 CONTINUE
C     COMPUTATION OF LAST ORDER FOR ASYMPTOTIC EXPANSION NORMALIZATION
      T = 1.0D0/(FN*RA)
      AIN = TOLLN/(GLN+SQRT(GLN*GLN+T*TOLLN)) + 1.5D0
      IN = INT(AIN)
      IF (IN.GT.INLIM) GO TO 160
  520 CONTINUE
      TRX = 2.0D0/X
      DTM = FNI + IN
      TM = (DTM+FNF)*TRX
      TA = 0.0D0
      TB = TOL
      KK = 1
  530 CONTINUE
C
C     BACKWARD RECUR UNINDEXED
C
      DO 540 I=1,IN
        S = TB
        TB = TM*TB + TA
        TA = S
        DTM = DTM - 1.0D0
        TM = (DTM+FNF)*TRX
  540 CONTINUE
C     NORMALIZATION
      IF (KK.NE.1) GO TO 550
      TA = (TA/TB)*TEMP(3)
      TB = TEMP(3)
      KK = 2
      IN = NS
      IF (NS.NE.0) GO TO 530
  550 Y(NN) = TB
      NZ = N - NN
      IF (NN.EQ.1) RETURN
      TB = TM*TB + TA
      K = NN - 1
      Y(K) = TB
      IF (NN.EQ.2) RETURN
      DTM = DTM - 1.0D0
      TM = (DTM+FNF)*TRX
      KM = K - 1
C
C     BACKWARD RECUR INDEXED
C
      DO 560 I=1,KM
        Y(K-1) = TM*Y(K) + Y(K+1)
        DTM = DTM - 1.0D0
        TM = (DTM+FNF)*TRX
        K = K - 1
  560 CONTINUE
      RETURN
C
C
C
  570 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESI',
     +   'SCALING OPTION, KODE, NOT 1 OR 2.', 2, 1)
      RETURN
  580 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESI', 'ORDER, ALPHA, LESS THAN ZERO.',
     +   2, 1)
      RETURN
  590 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESI', 'N LESS THAN ONE.', 2, 1)
      RETURN
  600 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESI', 'X LESS THAN ZERO.', 2, 1)
      RETURN
  610 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESI',
     +   'OVERFLOW, X TOO LARGE FOR KODE = 1.', 6, 1)
      RETURN
      END
*DECK DBESJ
      SUBROUTINE DBESJ (X, ALPHA, N, Y, NZ)
C***BEGIN PROLOGUE  DBESJ
C***PURPOSE  Compute an N member sequence of J Bessel functions
C            J/SUB(ALPHA+K-1)/(X), K=1,...,N for non-negative ALPHA
C            and X.
C***LIBRARY   SLATEC
C***CATEGORY  C10A3
C***TYPE      DOUBLE PRECISION (BESJ-S, DBESJ-D)
C***KEYWORDS  J BESSEL FUNCTION, SPECIAL FUNCTIONS
C***AUTHOR  Amos, D. E., (SNLA)
C           Daniel, S. L., (SNLA)
C           Weston, M. K., (SNLA)
C***DESCRIPTION
C
C     Abstract  **** a double precision routine ****
C         DBESJ computes an N member sequence of J Bessel functions
C         J/sub(ALPHA+K-1)/(X), K=1,...,N for non-negative ALPHA and X.
C         A combination of the power series, the asymptotic expansion
C         for X to infinity and the uniform asymptotic expansion for
C         NU to infinity are applied over subdivisions of the (NU,X)
C         plane.  For values of (NU,X) not covered by one of these
C         formulae, the order is incremented or decremented by integer
C         values into a region where one of the formulae apply. Backward
C         recursion is applied to reduce orders by integer values except
C         where the entire sequence lies in the oscillatory region.  In
C         this case forward recursion is stable and values from the
C         asymptotic expansion for X to infinity start the recursion
C         when it is efficient to do so. Leading terms of the series and
C         uniform expansion are tested for underflow.  If a sequence is
C         requested and the last member would underflow, the result is
C         set to zero and the next lower order tried, etc., until a
C         member comes on scale or all members are set to zero.
C         Overflow cannot occur.
C
C         The maximum number of significant digits obtainable
C         is the smaller of 14 and the number of digits carried in
C         double precision arithmetic.
C
C     Description of Arguments
C
C         Input      X,ALPHA are double precision
C           X      - X .GE. 0.0D0
C           ALPHA  - order of first member of the sequence,
C                    ALPHA .GE. 0.0D0
C           N      - number of members in the sequence, N .GE. 1
C
C         Output     Y is double precision
C           Y      - a vector whose first N components contain
C                    values for J/sub(ALPHA+K-1)/(X), K=1,...,N
C           NZ     - number of components of Y set to zero due to
C                    underflow,
C                    NZ=0   , normal return, computation completed
C                    NZ .NE. 0, last NZ components of Y set to zero,
C                             Y(K)=0.0D0, K=N-NZ+1,...,N.
C
C     Error Conditions
C         Improper input arguments - a fatal error
C         Underflow  - a non-fatal error (NZ .NE. 0)
C
C***REFERENCES  D. E. Amos, S. L. Daniel and M. K. Weston, CDC 6600
C                 subroutines IBESS and JBESS for Bessel functions
C                 I(NU,X) and J(NU,X), X .GE. 0, NU .GE. 0, ACM
C                 Transactions on Mathematical Software 3, (1977),
C                 pp. 76-92.
C               F. W. J. Olver, Tables of Bessel Functions of Moderate
C                 or Large Orders, NPL Mathematical Tables 6, Her
C                 Majesty's Stationery Office, London, 1962.
C***ROUTINES CALLED  D1MACH, DASYJY, DJAIRY, DLNGAM, I1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   750101  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DBESJ
      EXTERNAL DJAIRY
      INTEGER I,IALP,IDALP,IFLW,IN,INLIM,IS,I1,I2,K,KK,KM,KT,N,NN,
     1        NS,NZ
      INTEGER I1MACH
      DOUBLE PRECISION AK,AKM,ALPHA,ANS,AP,ARG,COEF,DALPHA,DFN,DTM,
     1           EARG,ELIM1,ETX,FIDAL,FLGJY,FN,FNF,FNI,FNP1,FNU,
     2           FNULIM,GLN,PDF,PIDT,PP,RDEN,RELB,RTTP,RTWO,RTX,RZDEN,
     3           S,SA,SB,SXO2,S1,S2,T,TA,TAU,TB,TEMP,TFN,TM,TOL,
     4           TOLLN,TRX,TX,T1,T2,WK,X,XO2,XO2L,Y,SLIM,RTOL
      SAVE RTWO, PDF, RTTP, PIDT, PP, INLIM, FNULIM
      DOUBLE PRECISION D1MACH, DLNGAM
      DIMENSION Y(*), TEMP(3), FNULIM(2), PP(4), WK(7)
      DATA RTWO,PDF,RTTP,PIDT                    / 1.34839972492648D+00,
     1 7.85398163397448D-01, 7.97884560802865D-01, 1.57079632679490D+00/
      DATA  PP(1),  PP(2),  PP(3),  PP(4)        / 8.72909153935547D+00,
     1 2.65693932265030D-01, 1.24578576865586D-01, 7.70133747430388D-04/
      DATA INLIM           /      150            /
      DATA FNULIM(1), FNULIM(2) /      100.0D0,     60.0D0     /
C***FIRST EXECUTABLE STATEMENT  DBESJ
      NZ = 0
      KT = 1
      NS=0
C     I1MACH(14) REPLACES I1MACH(11) IN A DOUBLE PRECISION CODE
C     I1MACH(15) REPLACES I1MACH(12) IN A DOUBLE PRECISION CODE
      TA = D1MACH(3)
      TOL = MAX(TA,1.0D-15)
      I1 = I1MACH(14) + 1
      I2 = I1MACH(15)
      TB = D1MACH(5)
      ELIM1 = -2.303D0*(I2*TB+3.0D0)
      RTOL=1.0D0/TOL
      SLIM=D1MACH(1)*RTOL*1.0D+3
C     TOLLN = -LN(TOL)
      TOLLN = 2.303D0*TB*I1
      TOLLN = MIN(TOLLN,34.5388D0)
      IF (N-1) 720, 10, 20
   10 KT = 2
   20 NN = N
      IF (X) 730, 30, 80
   30 IF (ALPHA) 710, 40, 50
   40 Y(1) = 1.0D0
      IF (N.EQ.1) RETURN
      I1 = 2
      GO TO 60
   50 I1 = 1
   60 DO 70 I=I1,N
        Y(I) = 0.0D0
   70 CONTINUE
      RETURN
   80 CONTINUE
      IF (ALPHA.LT.0.0D0) GO TO 710
C
      IALP = INT(ALPHA)
      FNI = IALP + N - 1
      FNF = ALPHA - IALP
      DFN = FNI + FNF
      FNU = DFN
      XO2 = X*0.5D0
      SXO2 = XO2*XO2
C
C     DECISION TREE FOR REGION WHERE SERIES, ASYMPTOTIC EXPANSION FOR X
C     TO INFINITY AND ASYMPTOTIC EXPANSION FOR NU TO INFINITY ARE
C     APPLIED.
C
      IF (SXO2.LE.(FNU+1.0D0)) GO TO 90
      TA = MAX(20.0D0,FNU)
      IF (X.GT.TA) GO TO 120
      IF (X.GT.12.0D0) GO TO 110
      XO2L = LOG(XO2)
      NS = INT(SXO2-FNU) + 1
      GO TO 100
   90 FN = FNU
      FNP1 = FN + 1.0D0
      XO2L = LOG(XO2)
      IS = KT
      IF (X.LE.0.50D0) GO TO 330
      NS = 0
  100 FNI = FNI + NS
      DFN = FNI + FNF
      FN = DFN
      FNP1 = FN + 1.0D0
      IS = KT
      IF (N-1+NS.GT.0) IS = 3
      GO TO 330
  110 ANS = MAX(36.0D0-FNU,0.0D0)
      NS = INT(ANS)
      FNI = FNI + NS
      DFN = FNI + FNF
      FN = DFN
      IS = KT
      IF (N-1+NS.GT.0) IS = 3
      GO TO 130
  120 CONTINUE
      RTX = SQRT(X)
      TAU = RTWO*RTX
      TA = TAU + FNULIM(KT)
      IF (FNU.LE.TA) GO TO 480
      FN = FNU
      IS = KT
C
C     UNIFORM ASYMPTOTIC EXPANSION FOR NU TO INFINITY
C
  130 CONTINUE
      I1 = ABS(3-IS)
      I1 = MAX(I1,1)
      FLGJY = 1.0D0
      CALL DASYJY(DJAIRY,X,FN,FLGJY,I1,TEMP(IS),WK,IFLW)
      IF(IFLW.NE.0) GO TO 380
      GO TO (320, 450, 620), IS
  310 TEMP(1) = TEMP(3)
      KT = 1
  320 IS = 2
      FNI = FNI - 1.0D0
      DFN = FNI + FNF
      FN = DFN
      IF(I1.EQ.2) GO TO 450
      GO TO 130
C
C     SERIES FOR (X/2)**2.LE.NU+1
C
  330 CONTINUE
      GLN = DLNGAM(FNP1)
      ARG = FN*XO2L - GLN
      IF (ARG.LT.(-ELIM1)) GO TO 400
      EARG = EXP(ARG)
  340 CONTINUE
      S = 1.0D0
      IF (X.LT.TOL) GO TO 360
      AK = 3.0D0
      T2 = 1.0D0
      T = 1.0D0
      S1 = FN
      DO 350 K=1,17
        S2 = T2 + S1
        T = -T*SXO2/S2
        S = S + T
        IF (ABS(T).LT.TOL) GO TO 360
        T2 = T2 + AK
        AK = AK + 2.0D0
        S1 = S1 + FN
  350 CONTINUE
  360 CONTINUE
      TEMP(IS) = S*EARG
      GO TO (370, 450, 610), IS
  370 EARG = EARG*FN/XO2
      FNI = FNI - 1.0D0
      DFN = FNI + FNF
      FN = DFN
      IS = 2
      GO TO 340
C
C     SET UNDERFLOW VALUE AND UPDATE PARAMETERS
C     UNDERFLOW CAN ONLY OCCUR FOR NS=0 SINCE THE ORDER MUST BE LARGER
C     THAN 36. THEREFORE, NS NEE NOT BE TESTED.
C
  380 Y(NN) = 0.0D0
      NN = NN - 1
      FNI = FNI - 1.0D0
      DFN = FNI + FNF
      FN = DFN
      IF (NN-1) 440, 390, 130
  390 KT = 2
      IS = 2
      GO TO 130
  400 Y(NN) = 0.0D0
      NN = NN - 1
      FNP1 = FN
      FNI = FNI - 1.0D0
      DFN = FNI + FNF
      FN = DFN
      IF (NN-1) 440, 410, 420
  410 KT = 2
      IS = 2
  420 IF (SXO2.LE.FNP1) GO TO 430
      GO TO 130
  430 ARG = ARG - XO2L + LOG(FNP1)
      IF (ARG.LT.(-ELIM1)) GO TO 400
      GO TO 330
  440 NZ = N - NN
      RETURN
C
C     BACKWARD RECURSION SECTION
C
  450 CONTINUE
      IF(NS.NE.0) GO TO 451
      NZ = N - NN
      IF (KT.EQ.2) GO TO 470
C     BACKWARD RECUR FROM INDEX ALPHA+NN-1 TO ALPHA
      Y(NN) = TEMP(1)
      Y(NN-1) = TEMP(2)
      IF (NN.EQ.2) RETURN
  451 CONTINUE
      TRX = 2.0D0/X
      DTM = FNI
      TM = (DTM+FNF)*TRX
      AK=1.0D0
      TA=TEMP(1)
      TB=TEMP(2)
      IF(ABS(TA).GT.SLIM) GO TO 455
      TA=TA*RTOL
      TB=TB*RTOL
      AK=TOL
  455 CONTINUE
      KK=2
      IN=NS-1
      IF(IN.EQ.0) GO TO 690
      IF(NS.NE.0) GO TO 670
      K=NN-2
      DO 460 I=3,NN
        S=TB
        TB = TM*TB - TA
        TA=S
        Y(K)=TB*AK
        DTM = DTM - 1.0D0
        TM = (DTM+FNF)*TRX
        K = K - 1
  460 CONTINUE
      RETURN
  470 Y(1) = TEMP(2)
      RETURN
C
C     ASYMPTOTIC EXPANSION FOR X TO INFINITY WITH FORWARD RECURSION IN
C     OSCILLATORY REGION X.GT.MAX(20, NU), PROVIDED THE LAST MEMBER
C     OF THE SEQUENCE IS ALSO IN THE REGION.
C
  480 CONTINUE
      IN = INT(ALPHA-TAU+2.0D0)
      IF (IN.LE.0) GO TO 490
      IDALP = IALP - IN - 1
      KT = 1
      GO TO 500
  490 CONTINUE
      IDALP = IALP
      IN = 0
  500 IS = KT
      FIDAL = IDALP
      DALPHA = FIDAL + FNF
      ARG = X - PIDT*DALPHA - PDF
      SA = SIN(ARG)
      SB = COS(ARG)
      COEF = RTTP/RTX
      ETX = 8.0D0*X
  510 CONTINUE
      DTM = FIDAL + FIDAL
      DTM = DTM*DTM
      TM = 0.0D0
      IF (FIDAL.EQ.0.0D0 .AND. ABS(FNF).LT.TOL) GO TO 520
      TM = 4.0D0*FNF*(FIDAL+FIDAL+FNF)
  520 CONTINUE
      TRX = DTM - 1.0D0
      T2 = (TRX+TM)/ETX
      S2 = T2
      RELB = TOL*ABS(T2)
      T1 = ETX
      S1 = 1.0D0
      FN = 1.0D0
      AK = 8.0D0
      DO 530 K=1,13
        T1 = T1 + ETX
        FN = FN + AK
        TRX = DTM - FN
        AP = TRX + TM
        T2 = -T2*AP/T1
        S1 = S1 + T2
        T1 = T1 + ETX
        AK = AK + 8.0D0
        FN = FN + AK
        TRX = DTM - FN
        AP = TRX + TM
        T2 = T2*AP/T1
        S2 = S2 + T2
        IF (ABS(T2).LE.RELB) GO TO 540
        AK = AK + 8.0D0
  530 CONTINUE
  540 TEMP(IS) = COEF*(S1*SB-S2*SA)
      IF(IS.EQ.2) GO TO 560
      FIDAL = FIDAL + 1.0D0
      DALPHA = FIDAL + FNF
      IS = 2
      TB = SA
      SA = -SB
      SB = TB
      GO TO 510
C
C     FORWARD RECURSION SECTION
C
  560 IF (KT.EQ.2) GO TO 470
      S1 = TEMP(1)
      S2 = TEMP(2)
      TX = 2.0D0/X
      TM = DALPHA*TX
      IF (IN.EQ.0) GO TO 580
C
C     FORWARD RECUR TO INDEX ALPHA
C
      DO 570 I=1,IN
        S = S2
        S2 = TM*S2 - S1
        TM = TM + TX
        S1 = S
  570 CONTINUE
      IF (NN.EQ.1) GO TO 600
      S = S2
      S2 = TM*S2 - S1
      TM = TM + TX
      S1 = S
  580 CONTINUE
C
C     FORWARD RECUR FROM INDEX ALPHA TO ALPHA+N-1
C
      Y(1) = S1
      Y(2) = S2
      IF (NN.EQ.2) RETURN
      DO 590 I=3,NN
        Y(I) = TM*Y(I-1) - Y(I-2)
        TM = TM + TX
  590 CONTINUE
      RETURN
  600 Y(1) = S2
      RETURN
C
C     BACKWARD RECURSION WITH NORMALIZATION BY
C     ASYMPTOTIC EXPANSION FOR NU TO INFINITY OR POWER SERIES.
C
  610 CONTINUE
C     COMPUTATION OF LAST ORDER FOR SERIES NORMALIZATION
      AKM = MAX(3.0D0-FN,0.0D0)
      KM = INT(AKM)
      TFN = FN + KM
      TA = (GLN+TFN-0.9189385332D0-0.0833333333D0/TFN)/(TFN+0.5D0)
      TA = XO2L - TA
      TB = -(1.0D0-1.5D0/TFN)/TFN
      AKM = TOLLN/(-TA+SQRT(TA*TA-TOLLN*TB)) + 1.5D0
      IN = KM + INT(AKM)
      GO TO 660
  620 CONTINUE
C     COMPUTATION OF LAST ORDER FOR ASYMPTOTIC EXPANSION NORMALIZATION
      GLN = WK(3) + WK(2)
      IF (WK(6).GT.30.0D0) GO TO 640
      RDEN = (PP(4)*WK(6)+PP(3))*WK(6) + 1.0D0
      RZDEN = PP(1) + PP(2)*WK(6)
      TA = RZDEN/RDEN
      IF (WK(1).LT.0.10D0) GO TO 630
      TB = GLN/WK(5)
      GO TO 650
  630 TB=(1.259921049D0+(0.1679894730D0+0.0887944358D0*WK(1))*WK(1))
     1 /WK(7)
      GO TO 650
  640 CONTINUE
      TA = 0.5D0*TOLLN/WK(4)
      TA=((0.0493827160D0*TA-0.1111111111D0)*TA+0.6666666667D0)*TA*WK(6)
      IF (WK(1).LT.0.10D0) GO TO 630
      TB = GLN/WK(5)
  650 IN = INT(TA/TB+1.5D0)
      IF (IN.GT.INLIM) GO TO 310
  660 CONTINUE
      DTM = FNI + IN
      TRX = 2.0D0/X
      TM = (DTM+FNF)*TRX
      TA = 0.0D0
      TB = TOL
      KK = 1
      AK=1.0D0
  670 CONTINUE
C
C     BACKWARD RECUR UNINDEXED
C
      DO 680 I=1,IN
        S = TB
        TB = TM*TB - TA
        TA = S
        DTM = DTM - 1.0D0
        TM = (DTM+FNF)*TRX
  680 CONTINUE
C     NORMALIZATION
      IF (KK.NE.1) GO TO 690
      S=TEMP(3)
      SA=TA/TB
      TA=S
      TB=S
      IF(ABS(S).GT.SLIM) GO TO 685
      TA=TA*RTOL
      TB=TB*RTOL
      AK=TOL
  685 CONTINUE
      TA=TA*SA
      KK = 2
      IN = NS
      IF (NS.NE.0) GO TO 670
  690 Y(NN) = TB*AK
      NZ = N - NN
      IF (NN.EQ.1) RETURN
      K = NN - 1
      S=TB
      TB = TM*TB - TA
      TA=S
      Y(K)=TB*AK
      IF (NN.EQ.2) RETURN
      DTM = DTM - 1.0D0
      TM = (DTM+FNF)*TRX
      K=NN-2
C
C     BACKWARD RECUR INDEXED
C
      DO 700 I=3,NN
        S=TB
        TB = TM*TB - TA
        TA=S
        Y(K)=TB*AK
        DTM = DTM - 1.0D0
        TM = (DTM+FNF)*TRX
        K = K - 1
  700 CONTINUE
      RETURN
C
C
C
  710 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESJ', 'ORDER, ALPHA, LESS THAN ZERO.',
     +   2, 1)
      RETURN
  720 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESJ', 'N LESS THAN ONE.', 2, 1)
      RETURN
  730 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESJ', 'X LESS THAN ZERO.', 2, 1)
      RETURN
      END
*DECK DBESK
      SUBROUTINE DBESK (X, FNU, KODE, N, Y, NZ)
C***BEGIN PROLOGUE  DBESK
C***PURPOSE  Implement forward recursion on the three term recursion
C            relation for a sequence of non-negative order Bessel
C            functions K/SUB(FNU+I-1)/(X), or scaled Bessel functions
C            EXP(X)*K/SUB(FNU+I-1)/(X), I=1,...,N for real, positive
C            X and non-negative orders FNU.
C***LIBRARY   SLATEC
C***CATEGORY  C10B3
C***TYPE      DOUBLE PRECISION (BESK-S, DBESK-D)
C***KEYWORDS  K BESSEL FUNCTION, SPECIAL FUNCTIONS
C***AUTHOR  Amos, D. E., (SNLA)
C***DESCRIPTION
C
C     Abstract  **** a double precision routine ****
C         DBESK implements forward recursion on the three term
C         recursion relation for a sequence of non-negative order Bessel
C         functions K/sub(FNU+I-1)/(X), or scaled Bessel functions
C         EXP(X)*K/sub(FNU+I-1)/(X), I=1,..,N for real X .GT. 0.0D0 and
C         non-negative orders FNU.  If FNU .LT. NULIM, orders FNU and
C         FNU+1 are obtained from DBSKNU to start the recursion.  If
C         FNU .GE. NULIM, the uniform asymptotic expansion is used for
C         orders FNU and FNU+1 to start the recursion.  NULIM is 35 or
C         70 depending on whether N=1 or N .GE. 2.  Under and overflow
C         tests are made on the leading term of the asymptotic expansion
C         before any extensive computation is done.
C
C         The maximum number of significant digits obtainable
C         is the smaller of 14 and the number of digits carried in
C         double precision arithmetic.
C
C     Description of Arguments
C
C         Input      X,FNU are double precision
C           X      - X .GT. 0.0D0
C           FNU    - order of the initial K function, FNU .GE. 0.0D0
C           KODE   - a parameter to indicate the scaling option
C                    KODE=1 returns Y(I)=       K/sub(FNU+I-1)/(X),
C                                        I=1,...,N
C                    KODE=2 returns Y(I)=EXP(X)*K/sub(FNU+I-1)/(X),
C                                        I=1,...,N
C           N      - number of members in the sequence, N .GE. 1
C
C         Output     Y is double precision
C           Y      - a vector whose first N components contain values
C                    for the sequence
C                    Y(I)=       k/sub(FNU+I-1)/(X), I=1,...,N  or
C                    Y(I)=EXP(X)*K/sub(FNU+I-1)/(X), I=1,...,N
C                    depending on KODE
C           NZ     - number of components of Y set to zero due to
C                    underflow with KODE=1,
C                    NZ=0   , normal return, computation completed
C                    NZ .NE. 0, first NZ components of Y set to zero
C                             due to underflow, Y(I)=0.0D0, I=1,...,NZ
C
C     Error Conditions
C         Improper input arguments - a fatal error
C         Overflow - a fatal error
C         Underflow with KODE=1 -  a non-fatal error (NZ .NE. 0)
C
C***REFERENCES  F. W. J. Olver, Tables of Bessel Functions of Moderate
C                 or Large Orders, NPL Mathematical Tables 6, Her
C                 Majesty's Stationery Office, London, 1962.
C               N. M. Temme, On the numerical evaluation of the modified
C                 Bessel function of the third kind, Journal of
C                 Computational Physics 19, (1975), pp. 324-337.
C***ROUTINES CALLED  D1MACH, DASYIK, DBESK0, DBESK1, DBSK0E, DBSK1E,
C                    DBSKNU, I1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   790201  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DBESK
C
      INTEGER I, J, K, KODE, MZ, N, NB, ND, NN, NUD, NULIM, NZ
      INTEGER I1MACH
      DOUBLE PRECISION CN,DNU,ELIM,ETX,FLGIK,FN,FNN,FNU,GLN,GNU,RTZ,
     1 S, S1, S2, T, TM, TRX, W, X, XLIM, Y, ZN
      DOUBLE PRECISION DBESK0, DBESK1, DBSK1E, DBSK0E, D1MACH
      DIMENSION W(2), NULIM(2), Y(*)
      SAVE NULIM
      DATA NULIM(1),NULIM(2) / 35 , 70 /
C***FIRST EXECUTABLE STATEMENT  DBESK
      NN = -I1MACH(15)
      ELIM = 2.303D0*(NN*D1MACH(5)-3.0D0)
      XLIM = D1MACH(1)*1.0D+3
      IF (KODE.LT.1 .OR. KODE.GT.2) GO TO 280
      IF (FNU.LT.0.0D0) GO TO 290
      IF (X.LE.0.0D0) GO TO 300
      IF (X.LT.XLIM) GO TO 320
      IF (N.LT.1) GO TO 310
      ETX = KODE - 1
C
C     ND IS A DUMMY VARIABLE FOR N
C     GNU IS A DUMMY VARIABLE FOR FNU
C     NZ = NUMBER OF UNDERFLOWS ON KODE=1
C
      ND = N
      NZ = 0
      NUD = INT(FNU)
      DNU = FNU - NUD
      GNU = FNU
      NN = MIN(2,ND)
      FN = FNU + N - 1
      FNN = FN
      IF (FN.LT.2.0D0) GO TO 150
C
C     OVERFLOW TEST  (LEADING EXPONENTIAL OF ASYMPTOTIC EXPANSION)
C     FOR THE LAST ORDER, FNU+N-1.GE.NULIM
C
      ZN = X/FN
      IF (ZN.EQ.0.0D0) GO TO 320
      RTZ = SQRT(1.0D0+ZN*ZN)
      GLN = LOG((1.0D0+RTZ)/ZN)
      T = RTZ*(1.0D0-ETX) + ETX/(ZN+RTZ)
      CN = -FN*(T-GLN)
      IF (CN.GT.ELIM) GO TO 320
      IF (NUD.LT.NULIM(NN)) GO TO 30
      IF (NN.EQ.1) GO TO 20
   10 CONTINUE
C
C     UNDERFLOW TEST (LEADING EXPONENTIAL OF ASYMPTOTIC EXPANSION)
C     FOR THE FIRST ORDER, FNU.GE.NULIM
C
      FN = GNU
      ZN = X/FN
      RTZ = SQRT(1.0D0+ZN*ZN)
      GLN = LOG((1.0D0+RTZ)/ZN)
      T = RTZ*(1.0D0-ETX) + ETX/(ZN+RTZ)
      CN = -FN*(T-GLN)
   20 CONTINUE
      IF (CN.LT.-ELIM) GO TO 230
C
C     ASYMPTOTIC EXPANSION FOR ORDERS FNU AND FNU+1.GE.NULIM
C
      FLGIK = -1.0D0
      CALL DASYIK(X,GNU,KODE,FLGIK,RTZ,CN,NN,Y)
      IF (NN.EQ.1) GO TO 240
      TRX = 2.0D0/X
      TM = (GNU+GNU+2.0D0)/X
      GO TO 130
C
   30 CONTINUE
      IF (KODE.EQ.2) GO TO 40
C
C     UNDERFLOW TEST (LEADING EXPONENTIAL OF ASYMPTOTIC EXPANSION IN X)
C     FOR ORDER DNU
C
      IF (X.GT.ELIM) GO TO 230
   40 CONTINUE
      IF (DNU.NE.0.0D0) GO TO 80
      IF (KODE.EQ.2) GO TO 50
      S1 = DBESK0(X)
      GO TO 60
   50 S1 = DBSK0E(X)
   60 CONTINUE
      IF (NUD.EQ.0 .AND. ND.EQ.1) GO TO 120
      IF (KODE.EQ.2) GO TO 70
      S2 = DBESK1(X)
      GO TO 90
   70 S2 = DBSK1E(X)
      GO TO 90
   80 CONTINUE
      NB = 2
      IF (NUD.EQ.0 .AND. ND.EQ.1) NB = 1
      CALL DBSKNU(X, DNU, KODE, NB, W, NZ)
      S1 = W(1)
      IF (NB.EQ.1) GO TO 120
      S2 = W(2)
   90 CONTINUE
      TRX = 2.0D0/X
      TM = (DNU+DNU+2.0D0)/X
C     FORWARD RECUR FROM DNU TO FNU+1 TO GET Y(1) AND Y(2)
      IF (ND.EQ.1) NUD = NUD - 1
      IF (NUD.GT.0) GO TO 100
      IF (ND.GT.1) GO TO 120
      S1 = S2
      GO TO 120
  100 CONTINUE
      DO 110 I=1,NUD
        S = S2
        S2 = TM*S2 + S1
        S1 = S
        TM = TM + TRX
  110 CONTINUE
      IF (ND.EQ.1) S1 = S2
  120 CONTINUE
      Y(1) = S1
      IF (ND.EQ.1) GO TO 240
      Y(2) = S2
  130 CONTINUE
      IF (ND.EQ.2) GO TO 240
C     FORWARD RECUR FROM FNU+2 TO FNU+N-1
      DO 140 I=3,ND
        Y(I) = TM*Y(I-1) + Y(I-2)
        TM = TM + TRX
  140 CONTINUE
      GO TO 240
C
  150 CONTINUE
C     UNDERFLOW TEST FOR KODE=1
      IF (KODE.EQ.2) GO TO 160
      IF (X.GT.ELIM) GO TO 230
  160 CONTINUE
C     OVERFLOW TEST
      IF (FN.LE.1.0D0) GO TO 170
      IF (-FN*(LOG(X)-0.693D0).GT.ELIM) GO TO 320
  170 CONTINUE
      IF (DNU.EQ.0.0D0) GO TO 180
      CALL DBSKNU(X, FNU, KODE, ND, Y, MZ)
      GO TO 240
  180 CONTINUE
      J = NUD
      IF (J.EQ.1) GO TO 210
      J = J + 1
      IF (KODE.EQ.2) GO TO 190
      Y(J) = DBESK0(X)
      GO TO 200
  190 Y(J) = DBSK0E(X)
  200 IF (ND.EQ.1) GO TO 240
      J = J + 1
  210 IF (KODE.EQ.2) GO TO 220
      Y(J) = DBESK1(X)
      GO TO 240
  220 Y(J) = DBSK1E(X)
      GO TO 240
C
C     UPDATE PARAMETERS ON UNDERFLOW
C
  230 CONTINUE
      NUD = NUD + 1
      ND = ND - 1
      IF (ND.EQ.0) GO TO 240
      NN = MIN(2,ND)
      GNU = GNU + 1.0D0
      IF (FNN.LT.2.0D0) GO TO 230
      IF (NUD.LT.NULIM(NN)) GO TO 230
      GO TO 10
  240 CONTINUE
      NZ = N - ND
      IF (NZ.EQ.0) RETURN
      IF (ND.EQ.0) GO TO 260
      DO 250 I=1,ND
        J = N - I + 1
        K = ND - I + 1
        Y(J) = Y(K)
  250 CONTINUE
  260 CONTINUE
      DO 270 I=1,NZ
        Y(I) = 0.0D0
  270 CONTINUE
      RETURN
C
C
C
  280 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESK',
     +   'SCALING OPTION, KODE, NOT 1 OR 2', 2, 1)
      RETURN
  290 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESK', 'ORDER, FNU, LESS THAN ZERO', 2,
     +   1)
      RETURN
  300 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESK', 'X LESS THAN OR EQUAL TO ZERO',
     +   2, 1)
      RETURN
  310 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESK', 'N LESS THAN ONE', 2, 1)
      RETURN
  320 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESK',
     +   'OVERFLOW, FNU OR N TOO LARGE OR X TOO SMALL', 6, 1)
      RETURN
      END
*DECK DBESY
      SUBROUTINE DBESY (X, FNU, N, Y)
C***BEGIN PROLOGUE  DBESY
C***PURPOSE  Implement forward recursion on the three term recursion
C            relation for a sequence of non-negative order Bessel
C            functions Y/SUB(FNU+I-1)/(X), I=1,...,N for real, positive
C            X and non-negative orders FNU.
C***LIBRARY   SLATEC
C***CATEGORY  C10A3
C***TYPE      DOUBLE PRECISION (BESY-S, DBESY-D)
C***KEYWORDS  SPECIAL FUNCTIONS, Y BESSEL FUNCTION
C***AUTHOR  Amos, D. E., (SNLA)
C***DESCRIPTION
C
C     Abstract  **** a double precision routine ****
C         DBESY implements forward recursion on the three term
C         recursion relation for a sequence of non-negative order Bessel
C         functions Y/sub(FNU+I-1)/(X), I=1,N for real X .GT. 0.0D0 and
C         non-negative orders FNU.  If FNU .LT. NULIM, orders FNU and
C         FNU+1 are obtained from DBSYNU which computes by a power
C         series for X .LE. 2, the K Bessel function of an imaginary
C         argument for 2 .LT. X .LE. 20 and the asymptotic expansion for
C         X .GT. 20.
C
C         If FNU .GE. NULIM, the uniform asymptotic expansion is coded
C         in DASYJY for orders FNU and FNU+1 to start the recursion.
C         NULIM is 70 or 100 depending on whether N=1 or N .GE. 2.  An
C         overflow test is made on the leading term of the asymptotic
C         expansion before any extensive computation is done.
C
C         The maximum number of significant digits obtainable
C         is the smaller of 14 and the number of digits carried in
C         double precision arithmetic.
C
C     Description of Arguments
C
C         Input
C           X      - X .GT. 0.0D0
C           FNU    - order of the initial Y function, FNU .GE. 0.0D0
C           N      - number of members in the sequence, N .GE. 1
C
C         Output
C           Y      - a vector whose first N components contain values
C                    for the sequence Y(I)=Y/sub(FNU+I-1)/(X), I=1,N.
C
C     Error Conditions
C         Improper input arguments - a fatal error
C         Overflow - a fatal error
C
C***REFERENCES  F. W. J. Olver, Tables of Bessel Functions of Moderate
C                 or Large Orders, NPL Mathematical Tables 6, Her
C                 Majesty's Stationery Office, London, 1962.
C               N. M. Temme, On the numerical evaluation of the modified
C                 Bessel function of the third kind, Journal of
C                 Computational Physics 19, (1975), pp. 324-337.
C               N. M. Temme, On the numerical evaluation of the ordinary
C                 Bessel function of the second kind, Journal of
C                 Computational Physics 21, (1976), pp. 343-350.
C***ROUTINES CALLED  D1MACH, DASYJY, DBESY0, DBESY1, DBSYNU, DYAIRY,
C                    I1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   800501  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DBESY
C
      EXTERNAL DYAIRY
      INTEGER I, IFLW, J, N, NB, ND, NN, NUD, NULIM
      INTEGER I1MACH
      DOUBLE PRECISION AZN,CN,DNU,ELIM,FLGJY,FN,FNU,RAN,S,S1,S2,TM,TRX,
     1           W,WK,W2N,X,XLIM,XXN,Y
      DOUBLE PRECISION DBESY0, DBESY1, D1MACH
      DIMENSION W(2), NULIM(2), Y(*), WK(7)
      SAVE NULIM
      DATA NULIM(1),NULIM(2) / 70 , 100 /
C***FIRST EXECUTABLE STATEMENT  DBESY
      NN = -I1MACH(15)
      ELIM = 2.303D0*(NN*D1MACH(5)-3.0D0)
      XLIM = D1MACH(1)*1.0D+3
      IF (FNU.LT.0.0D0) GO TO 140
      IF (X.LE.0.0D0) GO TO 150
      IF (X.LT.XLIM) GO TO 170
      IF (N.LT.1) GO TO 160
C
C     ND IS A DUMMY VARIABLE FOR N
C
      ND = N
      NUD = INT(FNU)
      DNU = FNU - NUD
      NN = MIN(2,ND)
      FN = FNU + N - 1
      IF (FN.LT.2.0D0) GO TO 100
C
C     OVERFLOW TEST  (LEADING EXPONENTIAL OF ASYMPTOTIC EXPANSION)
C     FOR THE LAST ORDER, FNU+N-1.GE.NULIM
C
      XXN = X/FN
      W2N = 1.0D0-XXN*XXN
      IF(W2N.LE.0.0D0) GO TO 10
      RAN = SQRT(W2N)
      AZN = LOG((1.0D0+RAN)/XXN) - RAN
      CN = FN*AZN
      IF(CN.GT.ELIM) GO TO 170
   10 CONTINUE
      IF (NUD.LT.NULIM(NN)) GO TO 20
C
C     ASYMPTOTIC EXPANSION FOR ORDERS FNU AND FNU+1.GE.NULIM
C
      FLGJY = -1.0D0
      CALL DASYJY(DYAIRY,X,FNU,FLGJY,NN,Y,WK,IFLW)
      IF(IFLW.NE.0) GO TO 170
      IF (NN.EQ.1) RETURN
      TRX = 2.0D0/X
      TM = (FNU+FNU+2.0D0)/X
      GO TO 80
C
   20 CONTINUE
      IF (DNU.NE.0.0D0) GO TO 30
      S1 = DBESY0(X)
      IF (NUD.EQ.0 .AND. ND.EQ.1) GO TO 70
      S2 = DBESY1(X)
      GO TO 40
   30 CONTINUE
      NB = 2
      IF (NUD.EQ.0 .AND. ND.EQ.1) NB = 1
      CALL DBSYNU(X, DNU, NB, W)
      S1 = W(1)
      IF (NB.EQ.1) GO TO 70
      S2 = W(2)
   40 CONTINUE
      TRX = 2.0D0/X
      TM = (DNU+DNU+2.0D0)/X
C     FORWARD RECUR FROM DNU TO FNU+1 TO GET Y(1) AND Y(2)
      IF (ND.EQ.1) NUD = NUD - 1
      IF (NUD.GT.0) GO TO 50
      IF (ND.GT.1) GO TO 70
      S1 = S2
      GO TO 70
   50 CONTINUE
      DO 60 I=1,NUD
        S = S2
        S2 = TM*S2 - S1
        S1 = S
        TM = TM + TRX
   60 CONTINUE
      IF (ND.EQ.1) S1 = S2
   70 CONTINUE
      Y(1) = S1
      IF (ND.EQ.1) RETURN
      Y(2) = S2
   80 CONTINUE
      IF (ND.EQ.2) RETURN
C     FORWARD RECUR FROM FNU+2 TO FNU+N-1
      DO 90 I=3,ND
        Y(I) = TM*Y(I-1) - Y(I-2)
        TM = TM + TRX
   90 CONTINUE
      RETURN
C
  100 CONTINUE
C     OVERFLOW TEST
      IF (FN.LE.1.0D0) GO TO 110
      IF (-FN*(LOG(X)-0.693D0).GT.ELIM) GO TO 170
  110 CONTINUE
      IF (DNU.EQ.0.0D0) GO TO 120
      CALL DBSYNU(X, FNU, ND, Y)
      RETURN
  120 CONTINUE
      J = NUD
      IF (J.EQ.1) GO TO 130
      J = J + 1
      Y(J) = DBESY0(X)
      IF (ND.EQ.1) RETURN
      J = J + 1
  130 CONTINUE
      Y(J) = DBESY1(X)
      IF (ND.EQ.1) RETURN
      TRX = 2.0D0/X
      TM = TRX
      GO TO 80
C
C
C
  140 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESY', 'ORDER, FNU, LESS THAN ZERO', 2,
     +   1)
      RETURN
  150 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESY', 'X LESS THAN OR EQUAL TO ZERO',
     +   2, 1)
      RETURN
  160 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESY', 'N LESS THAN ONE', 2, 1)
      RETURN
  170 CONTINUE
      CALL XERMSG ('SLATEC', 'DBESY',
     +   'OVERFLOW, FNU OR N TOO LARGE OR X TOO SMALL', 6, 1)
      RETURN
      END
*DECK DASYIK
      SUBROUTINE DASYIK (X, FNU, KODE, FLGIK, RA, ARG, IN, Y)
C***BEGIN PROLOGUE  DASYIK
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DBESI and DBESK
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (ASYIK-S, DASYIK-D)
C***AUTHOR  Amos, D. E., (SNLA)
C***DESCRIPTION
C
C                    DASYIK computes Bessel functions I and K
C                  for arguments X.GT.0.0 and orders FNU.GE.35
C                  on FLGIK = 1 and FLGIK = -1 respectively.
C
C                                    INPUT
C
C      X    - Argument, X.GT.0.0D0
C      FNU  - Order of first Bessel function
C      KODE - A parameter to indicate the scaling option
C             KODE=1 returns Y(I)=        I/SUB(FNU+I-1)/(X), I=1,IN
C                    or      Y(I)=        K/SUB(FNU+I-1)/(X), I=1,IN
C                    on FLGIK = 1.0D0 or FLGIK = -1.0D0
C             KODE=2 returns Y(I)=EXP(-X)*I/SUB(FNU+I-1)/(X), I=1,IN
C                    or      Y(I)=EXP( X)*K/SUB(FNU+I-1)/(X), I=1,IN
C                    on FLGIK = 1.0D0 or FLGIK = -1.0D0
C     FLGIK - Selection parameter for I or K FUNCTION
C             FLGIK =  1.0D0 gives the I function
C             FLGIK = -1.0D0 gives the K function
C        RA - SQRT(1.+Z*Z), Z=X/FNU
C       ARG - Argument of the leading exponential
C        IN - Number of functions desired, IN=1 or 2
C
C                                    OUTPUT
C
C         Y - A vector whose first IN components contain the sequence
C
C     Abstract  **** A double precision routine ****
C         DASYIK implements the uniform asymptotic expansion of
C         the I and K Bessel functions for FNU.GE.35 and real
C         X.GT.0.0D0. The forms are identical except for a change
C         in sign of some of the terms. This change in sign is
C         accomplished by means of the FLAG FLGIK = 1 or -1.
C
C***SEE ALSO  DBESI, DBESK
C***ROUTINES CALLED  D1MACH
C***REVISION HISTORY  (YYMMDD)
C   750101  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated the AUTHOR section.  (WRB)
C***END PROLOGUE  DASYIK
C
      INTEGER IN, J, JN, K, KK, KODE, L
      DOUBLE PRECISION AK,AP,ARG,C,COEF,CON,ETX,FLGIK,FN,FNU,GLN,RA,
     1 S1, S2, T, TOL, T2, X, Y, Z
      DOUBLE PRECISION D1MACH
      DIMENSION Y(*), C(65), CON(2)
      SAVE CON, C
      DATA CON(1), CON(2)  /
     1        3.98942280401432678D-01,    1.25331413731550025D+00/
      DATA C(1), C(2), C(3), C(4), C(5), C(6), C(7), C(8), C(9), C(10),
     1     C(11), C(12), C(13), C(14), C(15), C(16), C(17), C(18),
     2     C(19), C(20), C(21), C(22), C(23), C(24)/
     3       -2.08333333333333D-01,        1.25000000000000D-01,
     4        3.34201388888889D-01,       -4.01041666666667D-01,
     5        7.03125000000000D-02,       -1.02581259645062D+00,
     6        1.84646267361111D+00,       -8.91210937500000D-01,
     7        7.32421875000000D-02,        4.66958442342625D+00,
     8       -1.12070026162230D+01,        8.78912353515625D+00,
     9       -2.36408691406250D+00,        1.12152099609375D-01,
     1       -2.82120725582002D+01,        8.46362176746007D+01,
     2       -9.18182415432400D+01,        4.25349987453885D+01,
     3       -7.36879435947963D+00,        2.27108001708984D-01,
     4        2.12570130039217D+02,       -7.65252468141182D+02,
     5        1.05999045252800D+03,       -6.99579627376133D+02/
      DATA C(25), C(26), C(27), C(28), C(29), C(30), C(31), C(32),
     1     C(33), C(34), C(35), C(36), C(37), C(38), C(39), C(40),
     2     C(41), C(42), C(43), C(44), C(45), C(46), C(47), C(48)/
     3        2.18190511744212D+02,       -2.64914304869516D+01,
     4        5.72501420974731D-01,       -1.91945766231841D+03,
     5        8.06172218173731D+03,       -1.35865500064341D+04,
     6        1.16553933368645D+04,       -5.30564697861340D+03,
     7        1.20090291321635D+03,       -1.08090919788395D+02,
     8        1.72772750258446D+00,        2.02042913309661D+04,
     9       -9.69805983886375D+04,        1.92547001232532D+05,
     1       -2.03400177280416D+05,        1.22200464983017D+05,
     2       -4.11926549688976D+04,        7.10951430248936D+03,
     3       -4.93915304773088D+02,        6.07404200127348D+00,
     4       -2.42919187900551D+05,        1.31176361466298D+06,
     5       -2.99801591853811D+06,        3.76327129765640D+06/
      DATA C(49), C(50), C(51), C(52), C(53), C(54), C(55), C(56),
     1     C(57), C(58), C(59), C(60), C(61), C(62), C(63), C(64),
     2     C(65)/
     3       -2.81356322658653D+06,        1.26836527332162D+06,
     4       -3.31645172484564D+05,        4.52187689813627D+04,
     5       -2.49983048181121D+03,        2.43805296995561D+01,
     6        3.28446985307204D+06,       -1.97068191184322D+07,
     7        5.09526024926646D+07,       -7.41051482115327D+07,
     8        6.63445122747290D+07,       -3.75671766607634D+07,
     9        1.32887671664218D+07,       -2.78561812808645D+06,
     1        3.08186404612662D+05,       -1.38860897537170D+04,
     2        1.10017140269247D+02/
C***FIRST EXECUTABLE STATEMENT  DASYIK
      TOL = D1MACH(3)
      TOL = MAX(TOL,1.0D-15)
      FN = FNU
      Z  = (3.0D0-FLGIK)/2.0D0
      KK = INT(Z)
      DO 50 JN=1,IN
        IF (JN.EQ.1) GO TO 10
        FN = FN - FLGIK
        Z = X/FN
        RA = SQRT(1.0D0+Z*Z)
        GLN = LOG((1.0D0+RA)/Z)
        ETX = KODE - 1
        T = RA*(1.0D0-ETX) + ETX/(Z+RA)
        ARG = FN*(T-GLN)*FLGIK
   10   COEF = EXP(ARG)
        T = 1.0D0/RA
        T2 = T*T
        T = T/FN
        T = SIGN(T,FLGIK)
        S2 = 1.0D0
        AP = 1.0D0
        L = 0
        DO 30 K=2,11
          L = L + 1
          S1 = C(L)
          DO 20 J=2,K
            L = L + 1
            S1 = S1*T2 + C(L)
   20     CONTINUE
          AP = AP*T
          AK = AP*S1
          S2 = S2 + AK
          IF (MAX(ABS(AK),ABS(AP)) .LT.TOL) GO TO 40
   30   CONTINUE
   40   CONTINUE
      T = ABS(T)
      Y(JN) = S2*COEF*SQRT(T)*CON(KK)
   50 CONTINUE
      RETURN
      END
*DECK DASYJY
      SUBROUTINE DASYJY (FUNJY, X, FNU, FLGJY, IN, Y, WK, IFLW)
C***BEGIN PROLOGUE  DASYJY
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DBESJ and DBESY
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (ASYJY-S, DASYJY-D)
C***AUTHOR  Amos, D. E., (SNLA)
C***DESCRIPTION
C
C                 DASYJY computes Bessel functions J and Y
C               for arguments X.GT.0.0 and orders FNU .GE. 35.0
C               on FLGJY = 1 and FLGJY = -1 respectively
C
C                                  INPUT
C
C      FUNJY - External subroutine JAIRY or YAIRY
C          X - Argument, X.GT.0.0D0
C        FNU - Order of the first Bessel function
C      FLGJY - Selection flag
C              FLGJY =  1.0D0 gives the J function
C              FLGJY = -1.0D0 gives the Y function
C         IN - Number of functions desired, IN = 1 or 2
C
C                                  OUTPUT
C
C         Y  - A vector whose first IN components contain the sequence
C       IFLW - A flag indicating underflow or overflow
C                    return variables for BESJ only
C      WK(1) = 1 - (X/FNU)**2 = W**2
C      WK(2) = SQRT(ABS(WK(1)))
C      WK(3) = ABS(WK(2) - ATAN(WK(2)))  or
C              ABS(LN((1 + WK(2))/(X/FNU)) - WK(2))
C            = ABS((2/3)*ZETA**(3/2))
C      WK(4) = FNU*WK(3)
C      WK(5) = (1.5*WK(3)*FNU)**(1/3) = SQRT(ZETA)*FNU**(1/3)
C      WK(6) = SIGN(1.,W**2)*WK(5)**2 = SIGN(1.,W**2)*ZETA*FNU**(2/3)
C      WK(7) = FNU**(1/3)
C
C     Abstract   **** A Double Precision Routine ****
C         DASYJY implements the uniform asymptotic expansion of
C         the J and Y Bessel functions for FNU.GE.35 and real
C         X.GT.0.0D0. The forms are identical except for a change
C         in sign of some of the terms. This change in sign is
C         accomplished by means of the flag FLGJY = 1 or -1. On
C         FLGJY = 1 the Airy functions AI(X) and DAI(X) are
C         supplied by the external function JAIRY, and on
C         FLGJY = -1 the Airy functions BI(X) and DBI(X) are
C         supplied by the external function YAIRY.
C
C***SEE ALSO  DBESJ, DBESY
C***ROUTINES CALLED  D1MACH, I1MACH
C***REVISION HISTORY  (YYMMDD)
C   750101  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   891004  Correction computation of ELIM.  (WRB)
C   891009  Removed unreferenced variable.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated the AUTHOR section.  (WRB)
C***END PROLOGUE  DASYJY
      INTEGER I, IFLW, IN, J, JN,JR,JU,K, KB,KLAST,KMAX,KP1, KS, KSP1,
     * KSTEMP, L, LR, LRP1, ISETA, ISETB
      INTEGER I1MACH
      DOUBLE PRECISION ABW2, AKM, ALFA, ALFA1, ALFA2, AP, AR, ASUM, AZ,
     * BETA, BETA1, BETA2, BETA3, BR, BSUM, C, CON1, CON2,
     * CON548,CR,CRZ32, DFI,ELIM, DR,FI, FLGJY, FN, FNU,
     * FN2, GAMA, PHI,  RCZ, RDEN, RELB, RFN2,  RTZ, RZDEN,
     * SA, SB, SUMA, SUMB, S1, TA, TAU, TB, TFN, TOL, TOLS, T2, UPOL,
     *  WK, X, XX, Y, Z, Z32
      DOUBLE PRECISION D1MACH
      DIMENSION Y(*), WK(*), C(65)
      DIMENSION ALFA(26,4), BETA(26,5)
      DIMENSION ALFA1(26,2), ALFA2(26,2)
      DIMENSION BETA1(26,2), BETA2(26,2), BETA3(26,1)
      DIMENSION GAMA(26), KMAX(5), AR(8), BR(10), UPOL(10)
      DIMENSION CR(10), DR(10)
      EQUIVALENCE (ALFA(1,1),ALFA1(1,1))
      EQUIVALENCE (ALFA(1,3),ALFA2(1,1))
      EQUIVALENCE (BETA(1,1),BETA1(1,1))
      EQUIVALENCE (BETA(1,3),BETA2(1,1))
      EQUIVALENCE (BETA(1,5),BETA3(1,1))
      SAVE TOLS, CON1, CON2, CON548, AR, BR, C,
     1 ALFA1, ALFA2, BETA1, BETA2, BETA3, GAMA
      DATA TOLS            /-6.90775527898214D+00/
      DATA CON1,CON2,CON548/
     1 6.66666666666667D-01, 3.33333333333333D-01, 1.04166666666667D-01/
      DATA  AR(1),  AR(2),  AR(3),  AR(4),  AR(5),  AR(6),  AR(7),
     A      AR(8)          / 8.35503472222222D-02, 1.28226574556327D-01,
     1 2.91849026464140D-01, 8.81627267443758D-01, 3.32140828186277D+00,
     2 1.49957629868626D+01, 7.89230130115865D+01, 4.74451538868264D+02/
      DATA  BR(1), BR(2), BR(3), BR(4), BR(5), BR(6), BR(7), BR(8),
     A      BR(9), BR(10)  /-1.45833333333333D-01,-9.87413194444444D-02,
     1-1.43312053915895D-01,-3.17227202678414D-01,-9.42429147957120D-01,
     2-3.51120304082635D+00,-1.57272636203680D+01,-8.22814390971859D+01,
     3-4.92355370523671D+02,-3.31621856854797D+03/
      DATA C(1), C(2), C(3), C(4), C(5), C(6), C(7), C(8), C(9), C(10),
     1     C(11), C(12), C(13), C(14), C(15), C(16), C(17), C(18),
     2     C(19), C(20), C(21), C(22), C(23), C(24)/
     3       -2.08333333333333D-01,        1.25000000000000D-01,
     4        3.34201388888889D-01,       -4.01041666666667D-01,
     5        7.03125000000000D-02,       -1.02581259645062D+00,
     6        1.84646267361111D+00,       -8.91210937500000D-01,
     7        7.32421875000000D-02,        4.66958442342625D+00,
     8       -1.12070026162230D+01,        8.78912353515625D+00,
     9       -2.36408691406250D+00,        1.12152099609375D-01,
     A       -2.82120725582002D+01,        8.46362176746007D+01,
     B       -9.18182415432400D+01,        4.25349987453885D+01,
     C       -7.36879435947963D+00,        2.27108001708984D-01,
     D        2.12570130039217D+02,       -7.65252468141182D+02,
     E        1.05999045252800D+03,       -6.99579627376133D+02/
      DATA C(25), C(26), C(27), C(28), C(29), C(30), C(31), C(32),
     1     C(33), C(34), C(35), C(36), C(37), C(38), C(39), C(40),
     2     C(41), C(42), C(43), C(44), C(45), C(46), C(47), C(48)/
     3        2.18190511744212D+02,       -2.64914304869516D+01,
     4        5.72501420974731D-01,       -1.91945766231841D+03,
     5        8.06172218173731D+03,       -1.35865500064341D+04,
     6        1.16553933368645D+04,       -5.30564697861340D+03,
     7        1.20090291321635D+03,       -1.08090919788395D+02,
     8        1.72772750258446D+00,        2.02042913309661D+04,
     9       -9.69805983886375D+04,        1.92547001232532D+05,
     A       -2.03400177280416D+05,        1.22200464983017D+05,
     B       -4.11926549688976D+04,        7.10951430248936D+03,
     C       -4.93915304773088D+02,        6.07404200127348D+00,
     D       -2.42919187900551D+05,        1.31176361466298D+06,
     E       -2.99801591853811D+06,        3.76327129765640D+06/
      DATA C(49), C(50), C(51), C(52), C(53), C(54), C(55), C(56),
     1     C(57), C(58), C(59), C(60), C(61), C(62), C(63), C(64),
     2     C(65)/
     3       -2.81356322658653D+06,        1.26836527332162D+06,
     4       -3.31645172484564D+05,        4.52187689813627D+04,
     5       -2.49983048181121D+03,        2.43805296995561D+01,
     6        3.28446985307204D+06,       -1.97068191184322D+07,
     7        5.09526024926646D+07,       -7.41051482115327D+07,
     8        6.63445122747290D+07,       -3.75671766607634D+07,
     9        1.32887671664218D+07,       -2.78561812808645D+06,
     A        3.08186404612662D+05,       -1.38860897537170D+04,
     B        1.10017140269247D+02/
      DATA ALFA1(1,1), ALFA1(2,1), ALFA1(3,1), ALFA1(4,1), ALFA1(5,1),
     1     ALFA1(6,1), ALFA1(7,1), ALFA1(8,1), ALFA1(9,1), ALFA1(10,1),
     2     ALFA1(11,1),ALFA1(12,1),ALFA1(13,1),ALFA1(14,1),ALFA1(15,1),
     3     ALFA1(16,1),ALFA1(17,1),ALFA1(18,1),ALFA1(19,1),ALFA1(20,1),
     4     ALFA1(21,1),ALFA1(22,1),ALFA1(23,1),ALFA1(24,1),ALFA1(25,1),
     5     ALFA1(26,1)     /-4.44444444444444D-03,-9.22077922077922D-04,
     6-8.84892884892885D-05, 1.65927687832450D-04, 2.46691372741793D-04,
     7 2.65995589346255D-04, 2.61824297061501D-04, 2.48730437344656D-04,
     8 2.32721040083232D-04, 2.16362485712365D-04, 2.00738858762752D-04,
     9 1.86267636637545D-04, 1.73060775917876D-04, 1.61091705929016D-04,
     1 1.50274774160908D-04, 1.40503497391270D-04, 1.31668816545923D-04,
     2 1.23667445598253D-04, 1.16405271474738D-04, 1.09798298372713D-04,
     3 1.03772410422993D-04, 9.82626078369363D-05, 9.32120517249503D-05,
     4 8.85710852478712D-05, 8.42963105715700D-05, 8.03497548407791D-05/
      DATA ALFA1(1,2), ALFA1(2,2), ALFA1(3,2), ALFA1(4,2), ALFA1(5,2),
     1     ALFA1(6,2), ALFA1(7,2), ALFA1(8,2), ALFA1(9,2), ALFA1(10,2),
     2     ALFA1(11,2),ALFA1(12,2),ALFA1(13,2),ALFA1(14,2),ALFA1(15,2),
     3     ALFA1(16,2),ALFA1(17,2),ALFA1(18,2),ALFA1(19,2),ALFA1(20,2),
     4     ALFA1(21,2),ALFA1(22,2),ALFA1(23,2),ALFA1(24,2),ALFA1(25,2),
     5     ALFA1(26,2)     / 6.93735541354589D-04, 2.32241745182922D-04,
     6-1.41986273556691D-05,-1.16444931672049D-04,-1.50803558053049D-04,
     7-1.55121924918096D-04,-1.46809756646466D-04,-1.33815503867491D-04,
     8-1.19744975684254D-04,-1.06184319207974D-04,-9.37699549891194D-05,
     9-8.26923045588193D-05,-7.29374348155221D-05,-6.44042357721016D-05,
     1-5.69611566009369D-05,-5.04731044303562D-05,-4.48134868008883D-05,
     2-3.98688727717599D-05,-3.55400532972042D-05,-3.17414256609022D-05,
     3-2.83996793904175D-05,-2.54522720634871D-05,-2.28459297164725D-05,
     4-2.05352753106481D-05,-1.84816217627666D-05,-1.66519330021394D-05/
      DATA ALFA2(1,1), ALFA2(2,1), ALFA2(3,1), ALFA2(4,1), ALFA2(5,1),
     1     ALFA2(6,1), ALFA2(7,1), ALFA2(8,1), ALFA2(9,1), ALFA2(10,1),
     2     ALFA2(11,1),ALFA2(12,1),ALFA2(13,1),ALFA2(14,1),ALFA2(15,1),
     3     ALFA2(16,1),ALFA2(17,1),ALFA2(18,1),ALFA2(19,1),ALFA2(20,1),
     4     ALFA2(21,1),ALFA2(22,1),ALFA2(23,1),ALFA2(24,1),ALFA2(25,1),
     5     ALFA2(26,1)     /-3.54211971457744D-04,-1.56161263945159D-04,
     6 3.04465503594936D-05, 1.30198655773243D-04, 1.67471106699712D-04,
     7 1.70222587683593D-04, 1.56501427608595D-04, 1.36339170977445D-04,
     8 1.14886692029825D-04, 9.45869093034688D-05, 7.64498419250898D-05,
     9 6.07570334965197D-05, 4.74394299290509D-05, 3.62757512005344D-05,
     1 2.69939714979225D-05, 1.93210938247939D-05, 1.30056674793963D-05,
     2 7.82620866744497D-06, 3.59257485819352D-06, 1.44040049814252D-07,
     3-2.65396769697939D-06,-4.91346867098486D-06,-6.72739296091248D-06,
     4-8.17269379678658D-06,-9.31304715093561D-06,-1.02011418798016D-05/
      DATA ALFA2(1,2), ALFA2(2,2), ALFA2(3,2), ALFA2(4,2), ALFA2(5,2),
     1     ALFA2(6,2), ALFA2(7,2), ALFA2(8,2), ALFA2(9,2), ALFA2(10,2),
     2     ALFA2(11,2),ALFA2(12,2),ALFA2(13,2),ALFA2(14,2),ALFA2(15,2),
     3     ALFA2(16,2),ALFA2(17,2),ALFA2(18,2),ALFA2(19,2),ALFA2(20,2),
     4     ALFA2(21,2),ALFA2(22,2),ALFA2(23,2),ALFA2(24,2),ALFA2(25,2),
     5     ALFA2(26,2)     / 3.78194199201773D-04, 2.02471952761816D-04,
     6-6.37938506318862D-05,-2.38598230603006D-04,-3.10916256027362D-04,
     7-3.13680115247576D-04,-2.78950273791323D-04,-2.28564082619141D-04,
     8-1.75245280340847D-04,-1.25544063060690D-04,-8.22982872820208D-05,
     9-4.62860730588116D-05,-1.72334302366962D-05, 5.60690482304602D-06,
     1 2.31395443148287D-05, 3.62642745856794D-05, 4.58006124490189D-05,
     2 5.24595294959114D-05, 5.68396208545815D-05, 5.94349820393104D-05,
     3 6.06478527578422D-05, 6.08023907788436D-05, 6.01577894539460D-05,
     4 5.89199657344698D-05, 5.72515823777593D-05, 5.52804375585853D-05/
      DATA BETA1(1,1), BETA1(2,1), BETA1(3,1), BETA1(4,1), BETA1(5,1),
     1     BETA1(6,1), BETA1(7,1), BETA1(8,1), BETA1(9,1), BETA1(10,1),
     2     BETA1(11,1),BETA1(12,1),BETA1(13,1),BETA1(14,1),BETA1(15,1),
     3     BETA1(16,1),BETA1(17,1),BETA1(18,1),BETA1(19,1),BETA1(20,1),
     4     BETA1(21,1),BETA1(22,1),BETA1(23,1),BETA1(24,1),BETA1(25,1),
     5     BETA1(26,1)     / 1.79988721413553D-02, 5.59964911064388D-03,
     6 2.88501402231133D-03, 1.80096606761054D-03, 1.24753110589199D-03,
     7 9.22878876572938D-04, 7.14430421727287D-04, 5.71787281789705D-04,
     8 4.69431007606482D-04, 3.93232835462917D-04, 3.34818889318298D-04,
     9 2.88952148495752D-04, 2.52211615549573D-04, 2.22280580798883D-04,
     1 1.97541838033063D-04, 1.76836855019718D-04, 1.59316899661821D-04,
     2 1.44347930197334D-04, 1.31448068119965D-04, 1.20245444949303D-04,
     3 1.10449144504599D-04, 1.01828770740567D-04, 9.41998224204238D-05,
     4 8.74130545753834D-05, 8.13466262162801D-05, 7.59002269646219D-05/
      DATA BETA1(1,2), BETA1(2,2), BETA1(3,2), BETA1(4,2), BETA1(5,2),
     1     BETA1(6,2), BETA1(7,2), BETA1(8,2), BETA1(9,2), BETA1(10,2),
     2     BETA1(11,2),BETA1(12,2),BETA1(13,2),BETA1(14,2),BETA1(15,2),
     3     BETA1(16,2),BETA1(17,2),BETA1(18,2),BETA1(19,2),BETA1(20,2),
     4     BETA1(21,2),BETA1(22,2),BETA1(23,2),BETA1(24,2),BETA1(25,2),
     5     BETA1(26,2)     /-1.49282953213429D-03,-8.78204709546389D-04,
     6-5.02916549572035D-04,-2.94822138512746D-04,-1.75463996970783D-04,
     7-1.04008550460816D-04,-5.96141953046458D-05,-3.12038929076098D-05,
     8-1.26089735980230D-05,-2.42892608575730D-07, 8.05996165414274D-06,
     9 1.36507009262147D-05, 1.73964125472926D-05, 1.98672978842134D-05,
     1 2.14463263790823D-05, 2.23954659232457D-05, 2.28967783814713D-05,
     2 2.30785389811178D-05, 2.30321976080909D-05, 2.28236073720349D-05,
     3 2.25005881105292D-05, 2.20981015361991D-05, 2.16418427448104D-05,
     4 2.11507649256221D-05, 2.06388749782171D-05, 2.01165241997082D-05/
      DATA BETA2(1,1), BETA2(2,1), BETA2(3,1), BETA2(4,1), BETA2(5,1),
     1     BETA2(6,1), BETA2(7,1), BETA2(8,1), BETA2(9,1), BETA2(10,1),
     2     BETA2(11,1),BETA2(12,1),BETA2(13,1),BETA2(14,1),BETA2(15,1),
     3     BETA2(16,1),BETA2(17,1),BETA2(18,1),BETA2(19,1),BETA2(20,1),
     4     BETA2(21,1),BETA2(22,1),BETA2(23,1),BETA2(24,1),BETA2(25,1),
     5     BETA2(26,1)     / 5.52213076721293D-04, 4.47932581552385D-04,
     6 2.79520653992021D-04, 1.52468156198447D-04, 6.93271105657044D-05,
     7 1.76258683069991D-05,-1.35744996343269D-05,-3.17972413350427D-05,
     8-4.18861861696693D-05,-4.69004889379141D-05,-4.87665447413787D-05,
     9-4.87010031186735D-05,-4.74755620890087D-05,-4.55813058138628D-05,
     1-4.33309644511266D-05,-4.09230193157750D-05,-3.84822638603221D-05,
     2-3.60857167535411D-05,-3.37793306123367D-05,-3.15888560772110D-05,
     3-2.95269561750807D-05,-2.75978914828336D-05,-2.58006174666884D-05,
     4-2.41308356761280D-05,-2.25823509518346D-05,-2.11479656768913D-05/
      DATA BETA2(1,2), BETA2(2,2), BETA2(3,2), BETA2(4,2), BETA2(5,2),
     1     BETA2(6,2), BETA2(7,2), BETA2(8,2), BETA2(9,2), BETA2(10,2),
     2     BETA2(11,2),BETA2(12,2),BETA2(13,2),BETA2(14,2),BETA2(15,2),
     3     BETA2(16,2),BETA2(17,2),BETA2(18,2),BETA2(19,2),BETA2(20,2),
     4     BETA2(21,2),BETA2(22,2),BETA2(23,2),BETA2(24,2),BETA2(25,2),
     5     BETA2(26,2)     /-4.74617796559960D-04,-4.77864567147321D-04,
     6-3.20390228067038D-04,-1.61105016119962D-04,-4.25778101285435D-05,
     7 3.44571294294968D-05, 7.97092684075675D-05, 1.03138236708272D-04,
     8 1.12466775262204D-04, 1.13103642108481D-04, 1.08651634848774D-04,
     9 1.01437951597662D-04, 9.29298396593364D-05, 8.40293133016090D-05,
     1 7.52727991349134D-05, 6.69632521975731D-05, 5.92564547323195D-05,
     2 5.22169308826976D-05, 4.58539485165361D-05, 4.01445513891487D-05,
     3 3.50481730031328D-05, 3.05157995034347D-05, 2.64956119950516D-05,
     4 2.29363633690998D-05, 1.97893056664022D-05, 1.70091984636413D-05/
      DATA BETA3(1,1), BETA3(2,1), BETA3(3,1), BETA3(4,1), BETA3(5,1),
     1     BETA3(6,1), BETA3(7,1), BETA3(8,1), BETA3(9,1), BETA3(10,1),
     2     BETA3(11,1),BETA3(12,1),BETA3(13,1),BETA3(14,1),BETA3(15,1),
     3     BETA3(16,1),BETA3(17,1),BETA3(18,1),BETA3(19,1),BETA3(20,1),
     4     BETA3(21,1),BETA3(22,1),BETA3(23,1),BETA3(24,1),BETA3(25,1),
     5     BETA3(26,1)     / 7.36465810572578D-04, 8.72790805146194D-04,
     6 6.22614862573135D-04, 2.85998154194304D-04, 3.84737672879366D-06,
     7-1.87906003636972D-04,-2.97603646594555D-04,-3.45998126832656D-04,
     8-3.53382470916038D-04,-3.35715635775049D-04,-3.04321124789040D-04,
     9-2.66722723047613D-04,-2.27654214122820D-04,-1.89922611854562D-04,
     1-1.55058918599094D-04,-1.23778240761874D-04,-9.62926147717644D-05,
     2-7.25178327714425D-05,-5.22070028895634D-05,-3.50347750511901D-05,
     3-2.06489761035552D-05,-8.70106096849767D-06, 1.13698686675100D-06,
     4 9.16426474122779D-06, 1.56477785428873D-05, 2.08223629482467D-05/
      DATA GAMA(1),   GAMA(2),   GAMA(3),   GAMA(4),   GAMA(5),
     1     GAMA(6),   GAMA(7),   GAMA(8),   GAMA(9),   GAMA(10),
     2     GAMA(11),  GAMA(12),  GAMA(13),  GAMA(14),  GAMA(15),
     3     GAMA(16),  GAMA(17),  GAMA(18),  GAMA(19),  GAMA(20),
     4     GAMA(21),  GAMA(22),  GAMA(23),  GAMA(24),  GAMA(25),
     5     GAMA(26)        / 6.29960524947437D-01, 2.51984209978975D-01,
     6 1.54790300415656D-01, 1.10713062416159D-01, 8.57309395527395D-02,
     7 6.97161316958684D-02, 5.86085671893714D-02, 5.04698873536311D-02,
     8 4.42600580689155D-02, 3.93720661543510D-02, 3.54283195924455D-02,
     9 3.21818857502098D-02, 2.94646240791158D-02, 2.71581677112934D-02,
     1 2.51768272973862D-02, 2.34570755306079D-02, 2.19508390134907D-02,
     2 2.06210828235646D-02, 1.94388240897881D-02, 1.83810633800683D-02,
     3 1.74293213231963D-02, 1.65685837786612D-02, 1.57865285987918D-02,
     4 1.50729501494096D-02, 1.44193250839955D-02, 1.38184805735342D-02/
C***FIRST EXECUTABLE STATEMENT  DASYJY
      TA = D1MACH(3)
      TOL = MAX(TA,1.0D-15)
      TB = D1MACH(5)
      JU = I1MACH(15)
      IF(FLGJY.EQ.1.0D0) GO TO 6
      JR = I1MACH(14)
      ELIM = -2.303D0*TB*(JU+JR)
      GO TO 7
    6 CONTINUE
      ELIM = -2.303D0*(TB*JU+3.0D0)
    7 CONTINUE
      FN = FNU
      IFLW = 0
      DO 170 JN=1,IN
        XX = X/FN
        WK(1) = 1.0D0 - XX*XX
        ABW2 = ABS(WK(1))
        WK(2) = SQRT(ABW2)
        WK(7) = FN**CON2
        IF (ABW2.GT.0.27750D0) GO TO 80
C
C     ASYMPTOTIC EXPANSION
C     CASES NEAR X=FN, ABS(1.-(X/FN)**2).LE.0.2775
C     COEFFICIENTS OF ASYMPTOTIC EXPANSION BY SERIES
C
C     ZETA AND TRUNCATION FOR A(ZETA) AND B(ZETA) SERIES
C
C     KMAX IS TRUNCATION INDEX FOR A(ZETA) AND B(ZETA) SERIES=MAX(2,SA)
C
        SA = 0.0D0
        IF (ABW2.EQ.0.0D0) GO TO 10
        SA = TOLS/LOG(ABW2)
   10   SB = SA
        DO 20 I=1,5
          AKM = MAX(SA,2.0D0)
          KMAX(I) = INT(AKM)
          SA = SA + SB
   20   CONTINUE
        KB = KMAX(5)
        KLAST = KB - 1
        SA = GAMA(KB)
        DO 30 K=1,KLAST
          KB = KB - 1
          SA = SA*WK(1) + GAMA(KB)
   30   CONTINUE
        Z = WK(1)*SA
        AZ = ABS(Z)
        RTZ = SQRT(AZ)
        WK(3) = CON1*AZ*RTZ
        WK(4) = WK(3)*FN
        WK(5) = RTZ*WK(7)
        WK(6) = -WK(5)*WK(5)
        IF(Z.LE.0.0D0) GO TO 35
        IF(WK(4).GT.ELIM) GO TO 75
        WK(6) = -WK(6)
   35   CONTINUE
        PHI = SQRT(SQRT(SA+SA+SA+SA))
C
C     B(ZETA) FOR S=0
C
        KB = KMAX(5)
        KLAST = KB - 1
        SB = BETA(KB,1)
        DO 40 K=1,KLAST
          KB = KB - 1
          SB = SB*WK(1) + BETA(KB,1)
   40   CONTINUE
        KSP1 = 1
        FN2 = FN*FN
        RFN2 = 1.0D0/FN2
        RDEN = 1.0D0
        ASUM = 1.0D0
        RELB = TOL*ABS(SB)
        BSUM = SB
        DO 60 KS=1,4
          KSP1 = KSP1 + 1
          RDEN = RDEN*RFN2
C
C     A(ZETA) AND B(ZETA) FOR S=1,2,3,4
C
          KSTEMP = 5 - KS
          KB = KMAX(KSTEMP)
          KLAST = KB - 1
          SA = ALFA(KB,KS)
          SB = BETA(KB,KSP1)
          DO 50 K=1,KLAST
            KB = KB - 1
            SA = SA*WK(1) + ALFA(KB,KS)
            SB = SB*WK(1) + BETA(KB,KSP1)
   50     CONTINUE
          TA = SA*RDEN
          TB = SB*RDEN
          ASUM = ASUM + TA
          BSUM = BSUM + TB
          IF (ABS(TA).LE.TOL .AND. ABS(TB).LE.RELB) GO TO 70
   60   CONTINUE
   70   CONTINUE
        BSUM = BSUM/(FN*WK(7))
        GO TO 160
C
   75   CONTINUE
        IFLW = 1
        RETURN
C
   80   CONTINUE
        UPOL(1) = 1.0D0
        TAU = 1.0D0/WK(2)
        T2 = 1.0D0/WK(1)
        IF (WK(1).GE.0.0D0) GO TO 90
C
C     CASES FOR (X/FN).GT.SQRT(1.2775)
C
        WK(3) = ABS(WK(2)-ATAN(WK(2)))
        WK(4) = WK(3)*FN
        RCZ = -CON1/WK(4)
        Z32 = 1.5D0*WK(3)
        RTZ = Z32**CON2
        WK(5) = RTZ*WK(7)
        WK(6) = -WK(5)*WK(5)
        GO TO 100
   90   CONTINUE
C
C     CASES FOR (X/FN).LT.SQRT(0.7225)
C
        WK(3) = ABS(LOG((1.0D0+WK(2))/XX)-WK(2))
        WK(4) = WK(3)*FN
        RCZ = CON1/WK(4)
        IF(WK(4).GT.ELIM) GO TO 75
        Z32 = 1.5D0*WK(3)
        RTZ = Z32**CON2
        WK(7) = FN**CON2
        WK(5) = RTZ*WK(7)
        WK(6) = WK(5)*WK(5)
  100   CONTINUE
        PHI = SQRT((RTZ+RTZ)*TAU)
        TB = 1.0D0
        ASUM = 1.0D0
        TFN = TAU/FN
        RDEN=1.0D0/FN
        RFN2=RDEN*RDEN
        RDEN=1.0D0
        UPOL(2) = (C(1)*T2+C(2))*TFN
        CRZ32 = CON548*RCZ
        BSUM = UPOL(2) + CRZ32
        RELB = TOL*ABS(BSUM)
        AP = TFN
        KS = 0
        KP1 = 2
        RZDEN = RCZ
        L = 2
        ISETA=0
        ISETB=0
        DO 140 LR=2,8,2
C
C     COMPUTE TWO U POLYNOMIALS FOR NEXT A(ZETA) AND B(ZETA)
C
          LRP1 = LR + 1
          DO 120 K=LR,LRP1
            KS = KS + 1
            KP1 = KP1 + 1
            L = L + 1
            S1 = C(L)
            DO 110 J=2,KP1
              L = L + 1
              S1 = S1*T2 + C(L)
  110       CONTINUE
            AP = AP*TFN
            UPOL(KP1) = AP*S1
            CR(KS) = BR(KS)*RZDEN
            RZDEN = RZDEN*RCZ
            DR(KS) = AR(KS)*RZDEN
  120     CONTINUE
          SUMA = UPOL(LRP1)
          SUMB = UPOL(LR+2) + UPOL(LRP1)*CRZ32
          JU = LRP1
          DO 130 JR=1,LR
            JU = JU - 1
            SUMA = SUMA + CR(JR)*UPOL(JU)
            SUMB = SUMB + DR(JR)*UPOL(JU)
  130     CONTINUE
          RDEN=RDEN*RFN2
          TB = -TB
          IF (WK(1).GT.0.0D0) TB = ABS(TB)
          IF(RDEN.LT.TOL) GO TO 131
          ASUM = ASUM + SUMA*TB
          BSUM = BSUM + SUMB*TB
          GO TO 140
  131     IF(ISETA.EQ.1) GO TO 132
          IF(ABS(SUMA).LT.TOL) ISETA=1
          ASUM=ASUM+SUMA*TB
  132     IF(ISETB.EQ.1) GO TO 133
          IF(ABS(SUMB).LT.RELB) ISETB=1
          BSUM=BSUM+SUMB*TB
  133     IF(ISETA.EQ.1 .AND. ISETB.EQ.1) GO TO 150
  140   CONTINUE
  150   TB = WK(5)
        IF (WK(1).GT.0.0D0) TB = -TB
        BSUM = BSUM/TB
C
  160   CONTINUE
        CALL FUNJY(WK(6), WK(5), WK(4), FI, DFI)
        TA=1.0D0/TOL
        TB=D1MACH(1)*TA*1.0D+3
        IF(ABS(FI).GT.TB) GO TO 165
        FI=FI*TA
        DFI=DFI*TA
        PHI=PHI*TOL
  165   CONTINUE
        Y(JN) = FLGJY*PHI*(FI*ASUM+DFI*BSUM)/WK(7)
        FN = FN - FLGJY
  170 CONTINUE
      RETURN
      END
*DECK DBSKNU
      SUBROUTINE DBSKNU (X, FNU, KODE, N, Y, NZ)
C***BEGIN PROLOGUE  DBSKNU
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DBESK
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (BESKNU-S, DBSKNU-D)
C***AUTHOR  Amos, D. E., (SNLA)
C***DESCRIPTION
C
C     Abstract  **** A DOUBLE PRECISION routine ****
C         DBSKNU computes N member sequences of K Bessel functions
C         K/SUB(FNU+I-1)/(X), I=1,N for non-negative orders FNU and
C         positive X. Equations of the references are implemented on
C         small orders DNU for K/SUB(DNU)/(X) and K/SUB(DNU+1)/(X).
C         Forward recursion with the three term recursion relation
C         generates higher orders FNU+I-1, I=1,...,N. The parameter
C         KODE permits K/SUB(FNU+I-1)/(X) values or scaled values
C         EXP(X)*K/SUB(FNU+I-1)/(X), I=1,N to be returned.
C
C         To start the recursion FNU is normalized to the interval
C         -0.5.LE.DNU.LT.0.5. A special form of the power series is
C         implemented on 0.LT.X.LE.X1 while the Miller algorithm for the
C         K Bessel function in terms of the confluent hypergeometric
C         function U(FNU+0.5,2*FNU+1,X) is implemented on X1.LT.X.LE.X2.
C         For X.GT.X2, the asymptotic expansion for large X is used.
C         When FNU is a half odd integer, a special formula for
C         DNU=-0.5 and DNU+1.0=0.5 is used to start the recursion.
C
C         The maximum number of significant digits obtainable
C         is the smaller of 14 and the number of digits carried in
C         DOUBLE PRECISION arithmetic.
C
C         DBSKNU assumes that a significant digit SINH function is
C         available.
C
C     Description of Arguments
C
C         INPUT      X,FNU are DOUBLE PRECISION
C           X      - X.GT.0.0D0
C           FNU    - Order of initial K function, FNU.GE.0.0D0
C           N      - Number of members of the sequence, N.GE.1
C           KODE   - A parameter to indicate the scaling option
C                    KODE= 1  returns
C                             Y(I)=       K/SUB(FNU+I-1)/(X)
C                                  I=1,...,N
C                        = 2  returns
C                             Y(I)=EXP(X)*K/SUB(FNU+I-1)/(X)
C                                  I=1,...,N
C
C         OUTPUT     Y is DOUBLE PRECISION
C           Y      - A vector whose first N components contain values
C                    for the sequence
C                    Y(I)=       K/SUB(FNU+I-1)/(X), I=1,...,N or
C                    Y(I)=EXP(X)*K/SUB(FNU+I-1)/(X), I=1,...,N
C                    depending on KODE
C           NZ     - Number of components set to zero due to
C                    underflow,
C                    NZ= 0   , normal return
C                    NZ.NE.0 , first NZ components of Y set to zero
C                              due to underflow, Y(I)=0.0D0,I=1,...,NZ
C
C     Error Conditions
C         Improper input arguments - a fatal error
C         Overflow - a fatal error
C         Underflow with KODE=1 - a non-fatal error (NZ.NE.0)
C
C***SEE ALSO  DBESK
C***REFERENCES  N. M. Temme, On the numerical evaluation of the modified
C                 Bessel function of the third kind, Journal of
C                 Computational Physics 19, (1975), pp. 324-337.
C***ROUTINES CALLED  D1MACH, DGAMMA, I1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   790201  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   900328  Added TYPE section.  (WRB)
C   900727  Added EXTERNAL statement.  (WRB)
C   910408  Updated the AUTHOR and REFERENCES sections.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DBSKNU
C
      INTEGER I, IFLAG, INU, J, K, KK, KODE, KODED, N, NN, NZ
      INTEGER I1MACH
      DOUBLE PRECISION A,AK,A1,A2,B,BK,CC,CK,COEF,CX,DK,DNU,DNU2,ELIM,
     1 ETEST, EX, F, FC, FHS, FK, FKS, FLRX, FMU, FNU, G1, G2, P, PI,
     2 PT, P1, P2, Q, RTHPI, RX, S, SMU, SQK, ST, S1, S2, TM, TOL, T1,
     3 T2, X, X1, X2, Y
      DIMENSION A(160), B(160), Y(*), CC(8)
      DOUBLE PRECISION DGAMMA, D1MACH
      EXTERNAL DGAMMA
      SAVE X1, X2, PI, RTHPI, CC
      DATA X1, X2 / 2.0D0, 17.0D0 /
      DATA PI,RTHPI        / 3.14159265358979D+00, 1.25331413731550D+00/
      DATA CC(1), CC(2), CC(3), CC(4), CC(5), CC(6), CC(7), CC(8)
     1                     / 5.77215664901533D-01,-4.20026350340952D-02,
     2-4.21977345555443D-02, 7.21894324666300D-03,-2.15241674114900D-04,
     3-2.01348547807000D-05, 1.13302723200000D-06, 6.11609500000000D-09/
C***FIRST EXECUTABLE STATEMENT  DBSKNU
      KK = -I1MACH(15)
      ELIM = 2.303D0*(KK*D1MACH(5)-3.0D0)
      AK = D1MACH(3)
      TOL = MAX(AK,1.0D-15)
      IF (X.LE.0.0D0) GO TO 350
      IF (FNU.LT.0.0D0) GO TO 360
      IF (KODE.LT.1 .OR. KODE.GT.2) GO TO 370
      IF (N.LT.1) GO TO 380
      NZ = 0
      IFLAG = 0
      KODED = KODE
      RX = 2.0D0/X
      INU = INT(FNU+0.5D0)
      DNU = FNU - INU
      IF (ABS(DNU).EQ.0.5D0) GO TO 120
      DNU2 = 0.0D0
      IF (ABS(DNU).LT.TOL) GO TO 10
      DNU2 = DNU*DNU
   10 CONTINUE
      IF (X.GT.X1) GO TO 120
C
C     SERIES FOR X.LE.X1
C
      A1 = 1.0D0 - DNU
      A2 = 1.0D0 + DNU
      T1 = 1.0D0/DGAMMA(A1)
      T2 = 1.0D0/DGAMMA(A2)
      IF (ABS(DNU).GT.0.1D0) GO TO 40
C     SERIES FOR F0 TO RESOLVE INDETERMINACY FOR SMALL ABS(DNU)
      S = CC(1)
      AK = 1.0D0
      DO 20 K=2,8
        AK = AK*DNU2
        TM = CC(K)*AK
        S = S + TM
        IF (ABS(TM).LT.TOL) GO TO 30
   20 CONTINUE
   30 G1 = -S
      GO TO 50
   40 CONTINUE
      G1 = (T1-T2)/(DNU+DNU)
   50 CONTINUE
      G2 = (T1+T2)*0.5D0
      SMU = 1.0D0
      FC = 1.0D0
      FLRX = LOG(RX)
      FMU = DNU*FLRX
      IF (DNU.EQ.0.0D0) GO TO 60
      FC = DNU*PI
      FC = FC/SIN(FC)
      IF (FMU.NE.0.0D0) SMU = SINH(FMU)/FMU
   60 CONTINUE
      F = FC*(G1*COSH(FMU)+G2*FLRX*SMU)
      FC = EXP(FMU)
      P = 0.5D0*FC/T2
      Q = 0.5D0/(FC*T1)
      AK = 1.0D0
      CK = 1.0D0
      BK = 1.0D0
      S1 = F
      S2 = P
      IF (INU.GT.0 .OR. N.GT.1) GO TO 90
      IF (X.LT.TOL) GO TO 80
      CX = X*X*0.25D0
   70 CONTINUE
      F = (AK*F+P+Q)/(BK-DNU2)
      P = P/(AK-DNU)
      Q = Q/(AK+DNU)
      CK = CK*CX/AK
      T1 = CK*F
      S1 = S1 + T1
      BK = BK + AK + AK + 1.0D0
      AK = AK + 1.0D0
      S = ABS(T1)/(1.0D0+ABS(S1))
      IF (S.GT.TOL) GO TO 70
   80 CONTINUE
      Y(1) = S1
      IF (KODED.EQ.1) RETURN
      Y(1) = S1*EXP(X)
      RETURN
   90 CONTINUE
      IF (X.LT.TOL) GO TO 110
      CX = X*X*0.25D0
  100 CONTINUE
      F = (AK*F+P+Q)/(BK-DNU2)
      P = P/(AK-DNU)
      Q = Q/(AK+DNU)
      CK = CK*CX/AK
      T1 = CK*F
      S1 = S1 + T1
      T2 = CK*(P-AK*F)
      S2 = S2 + T2
      BK = BK + AK + AK + 1.0D0
      AK = AK + 1.0D0
      S = ABS(T1)/(1.0D0+ABS(S1)) + ABS(T2)/(1.0D0+ABS(S2))
      IF (S.GT.TOL) GO TO 100
  110 CONTINUE
      S2 = S2*RX
      IF (KODED.EQ.1) GO TO 170
      F = EXP(X)
      S1 = S1*F
      S2 = S2*F
      GO TO 170
  120 CONTINUE
      COEF = RTHPI/SQRT(X)
      IF (KODED.EQ.2) GO TO 130
      IF (X.GT.ELIM) GO TO 330
      COEF = COEF*EXP(-X)
  130 CONTINUE
      IF (ABS(DNU).EQ.0.5D0) GO TO 340
      IF (X.GT.X2) GO TO 280
C
C     MILLER ALGORITHM FOR X1.LT.X.LE.X2
C
      ETEST = COS(PI*DNU)/(PI*X*TOL)
      FKS = 1.0D0
      FHS = 0.25D0
      FK = 0.0D0
      CK = X + X + 2.0D0
      P1 = 0.0D0
      P2 = 1.0D0
      K = 0
  140 CONTINUE
      K = K + 1
      FK = FK + 1.0D0
      AK = (FHS-DNU2)/(FKS+FK)
      BK = CK/(FK+1.0D0)
      PT = P2
      P2 = BK*P2 - AK*P1
      P1 = PT
      A(K) = AK
      B(K) = BK
      CK = CK + 2.0D0
      FKS = FKS + FK + FK + 1.0D0
      FHS = FHS + FK + FK
      IF (ETEST.GT.FK*P1) GO TO 140
      KK = K
      S = 1.0D0
      P1 = 0.0D0
      P2 = 1.0D0
      DO 150 I=1,K
        PT = P2
        P2 = (B(KK)*P2-P1)/A(KK)
        P1 = PT
        S = S + P2
        KK = KK - 1
  150 CONTINUE
      S1 = COEF*(P2/S)
      IF (INU.GT.0 .OR. N.GT.1) GO TO 160
      GO TO 200
  160 CONTINUE
      S2 = S1*(X+DNU+0.5D0-P1/P2)/X
C
C     FORWARD RECURSION ON THE THREE TERM RECURSION RELATION
C
  170 CONTINUE
      CK = (DNU+DNU+2.0D0)/X
      IF (N.EQ.1) INU = INU - 1
      IF (INU.GT.0) GO TO 180
      IF (N.GT.1) GO TO 200
      S1 = S2
      GO TO 200
  180 CONTINUE
      DO 190 I=1,INU
        ST = S2
        S2 = CK*S2 + S1
        S1 = ST
        CK = CK + RX
  190 CONTINUE
      IF (N.EQ.1) S1 = S2
  200 CONTINUE
      IF (IFLAG.EQ.1) GO TO 220
      Y(1) = S1
      IF (N.EQ.1) RETURN
      Y(2) = S2
      IF (N.EQ.2) RETURN
      DO 210 I=3,N
        Y(I) = CK*Y(I-1) + Y(I-2)
        CK = CK + RX
  210 CONTINUE
      RETURN
C     IFLAG=1 CASES
  220 CONTINUE
      S = -X + LOG(S1)
      Y(1) = 0.0D0
      NZ = 1
      IF (S.LT.-ELIM) GO TO 230
      Y(1) = EXP(S)
      NZ = 0
  230 CONTINUE
      IF (N.EQ.1) RETURN
      S = -X + LOG(S2)
      Y(2) = 0.0D0
      NZ = NZ + 1
      IF (S.LT.-ELIM) GO TO 240
      NZ = NZ - 1
      Y(2) = EXP(S)
  240 CONTINUE
      IF (N.EQ.2) RETURN
      KK = 2
      IF (NZ.LT.2) GO TO 260
      DO 250 I=3,N
        KK = I
        ST = S2
        S2 = CK*S2 + S1
        S1 = ST
        CK = CK + RX
        S = -X + LOG(S2)
        NZ = NZ + 1
        Y(I) = 0.0D0
        IF (S.LT.-ELIM) GO TO 250
        Y(I) = EXP(S)
        NZ = NZ - 1
        GO TO 260
  250 CONTINUE
      RETURN
  260 CONTINUE
      IF (KK.EQ.N) RETURN
      S2 = S2*CK + S1
      CK = CK + RX
      KK = KK + 1
      Y(KK) = EXP(-X+LOG(S2))
      IF (KK.EQ.N) RETURN
      KK = KK + 1
      DO 270 I=KK,N
        Y(I) = CK*Y(I-1) + Y(I-2)
        CK = CK + RX
  270 CONTINUE
      RETURN
C
C     ASYMPTOTIC EXPANSION FOR LARGE X, X.GT.X2
C
C     IFLAG=0 MEANS NO UNDERFLOW OCCURRED
C     IFLAG=1 MEANS AN UNDERFLOW OCCURRED- COMPUTATION PROCEEDS WITH
C     KODED=2 AND A TEST FOR ON SCALE VALUES IS MADE DURING FORWARD
C     RECURSION
  280 CONTINUE
      NN = 2
      IF (INU.EQ.0 .AND. N.EQ.1) NN = 1
      DNU2 = DNU + DNU
      FMU = 0.0D0
      IF (ABS(DNU2).LT.TOL) GO TO 290
      FMU = DNU2*DNU2
  290 CONTINUE
      EX = X*8.0D0
      S2 = 0.0D0
      DO 320 K=1,NN
        S1 = S2
        S = 1.0D0
        AK = 0.0D0
        CK = 1.0D0
        SQK = 1.0D0
        DK = EX
        DO 300 J=1,30
          CK = CK*(FMU-SQK)/DK
          S = S + CK
          DK = DK + EX
          AK = AK + 8.0D0
          SQK = SQK + AK
          IF (ABS(CK).LT.TOL) GO TO 310
  300   CONTINUE
  310   S2 = S*COEF
        FMU = FMU + 8.0D0*DNU + 4.0D0
  320 CONTINUE
      IF (NN.GT.1) GO TO 170
      S1 = S2
      GO TO 200
  330 CONTINUE
      KODED = 2
      IFLAG = 1
      GO TO 120
C
C     FNU=HALF ODD INTEGER CASE
C
  340 CONTINUE
      S1 = COEF
      S2 = COEF
      GO TO 170
C
C
  350 CALL XERMSG ('SLATEC', 'DBSKNU', 'X NOT GREATER THAN ZERO', 2, 1)
      RETURN
  360 CALL XERMSG ('SLATEC', 'DBSKNU', 'FNU NOT ZERO OR POSITIVE', 2,
     +   1)
      RETURN
  370 CALL XERMSG ('SLATEC', 'DBSKNU', 'KODE NOT 1 OR 2', 2, 1)
      RETURN
  380 CALL XERMSG ('SLATEC', 'DBSKNU', 'N NOT GREATER THAN 0', 2, 1)
      RETURN
      END
*DECK DBSYNU
      SUBROUTINE DBSYNU (X, FNU, N, Y)
C***BEGIN PROLOGUE  DBSYNU
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DBESY
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (BESYNU-S, DBSYNU-D)
C***AUTHOR  Amos, D. E., (SNLA)
C***DESCRIPTION
C
C     Abstract  **** A DOUBLE PRECISION routine ****
C         DBSYNU computes N member sequences of Y Bessel functions
C         Y/SUB(FNU+I-1)/(X), I=1,N for non-negative orders FNU and
C         positive X. Equations of the references are implemented on
C         small orders DNU for Y/SUB(DNU)/(X) and Y/SUB(DNU+1)/(X).
C         Forward recursion with the three term recursion relation
C         generates higher orders FNU+I-1, I=1,...,N.
C
C         To start the recursion FNU is normalized to the interval
C         -0.5.LE.DNU.LT.0.5. A special form of the power series is
C         implemented on 0.LT.X.LE.X1 while the Miller algorithm for the
C         K Bessel function in terms of the confluent hypergeometric
C         function U(FNU+0.5,2*FNU+1,I*X) is implemented on X1.LT.X.LE.X
C         Here I is the complex number SQRT(-1.).
C         For X.GT.X2, the asymptotic expansion for large X is used.
C         When FNU is a half odd integer, a special formula for
C         DNU=-0.5 and DNU+1.0=0.5 is used to start the recursion.
C
C         The maximum number of significant digits obtainable
C         is the smaller of 14 and the number of digits carried in
C         DOUBLE PRECISION arithmetic.
C
C         DBSYNU assumes that a significant digit SINH function is
C         available.
C
C     Description of Arguments
C
C         INPUT
C           X      - X.GT.0.0D0
C           FNU    - Order of initial Y function, FNU.GE.0.0D0
C           N      - Number of members of the sequence, N.GE.1
C
C         OUTPUT
C           Y      - A vector whose first N components contain values
C                    for the sequence Y(I)=Y/SUB(FNU+I-1), I=1,N.
C
C     Error Conditions
C         Improper input arguments - a fatal error
C         Overflow - a fatal error
C
C***SEE ALSO  DBESY
C***REFERENCES  N. M. Temme, On the numerical evaluation of the ordinary
C                 Bessel function of the second kind, Journal of
C                 Computational Physics 21, (1976), pp. 343-350.
C               N. M. Temme, On the numerical evaluation of the modified
C                 Bessel function of the third kind, Journal of
C                 Computational Physics 19, (1975), pp. 324-337.
C***ROUTINES CALLED  D1MACH, DGAMMA, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   800501  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   900328  Added TYPE section.  (WRB)
C   900727  Added EXTERNAL statement.  (WRB)
C   910408  Updated the AUTHOR and REFERENCES sections.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DBSYNU
C
      INTEGER I, INU, J, K, KK, N, NN
      DOUBLE PRECISION A,AK,ARG,A1,A2,BK,CB,CBK,CC,CCK,CK,COEF,CPT,
     1 CP1, CP2, CS, CS1, CS2, CX, DNU, DNU2, ETEST, ETX, F, FC, FHS,
     2 FK, FKS, FLRX, FMU, FN, FNU, FX, G, G1, G2, HPI, P, PI, PT, Q,
     3 RB, RBK, RCK, RELB, RPT, RP1, RP2, RS, RS1, RS2, RTHPI, RX, S,
     4 SA, SB, SMU, SS, ST, S1, S2, TB, TM, TOL, T1, T2, X, X1, X2, Y
      DIMENSION A(120), RB(120), CB(120), Y(*), CC(8)
      DOUBLE PRECISION DGAMMA, D1MACH
      EXTERNAL DGAMMA
      SAVE X1, X2,PI, RTHPI, HPI, CC
      DATA X1, X2 / 3.0D0, 20.0D0 /
      DATA PI,RTHPI        / 3.14159265358979D+00, 7.97884560802865D-01/
      DATA HPI             / 1.57079632679490D+00/
      DATA CC(1), CC(2), CC(3), CC(4), CC(5), CC(6), CC(7), CC(8)
     1                     / 5.77215664901533D-01,-4.20026350340952D-02,
     2-4.21977345555443D-02, 7.21894324666300D-03,-2.15241674114900D-04,
     3-2.01348547807000D-05, 1.13302723200000D-06, 6.11609500000000D-09/
C***FIRST EXECUTABLE STATEMENT  DBSYNU
      AK = D1MACH(3)
      TOL = MAX(AK,1.0D-15)
      IF (X.LE.0.0D0) GO TO 270
      IF (FNU.LT.0.0D0) GO TO 280
      IF (N.LT.1) GO TO 290
      RX = 2.0D0/X
      INU = INT(FNU+0.5D0)
      DNU = FNU - INU
      IF (ABS(DNU).EQ.0.5D0) GO TO 260
      DNU2 = 0.0D0
      IF (ABS(DNU).LT.TOL) GO TO 10
      DNU2 = DNU*DNU
   10 CONTINUE
      IF (X.GT.X1) GO TO 120
C
C     SERIES FOR X.LE.X1
C
      A1 = 1.0D0 - DNU
      A2 = 1.0D0 + DNU
      T1 = 1.0D0/DGAMMA(A1)
      T2 = 1.0D0/DGAMMA(A2)
      IF (ABS(DNU).GT.0.1D0) GO TO 40
C     SERIES FOR F0 TO RESOLVE INDETERMINACY FOR SMALL ABS(DNU)
      S = CC(1)
      AK = 1.0D0
      DO 20 K=2,8
        AK = AK*DNU2
        TM = CC(K)*AK
        S = S + TM
        IF (ABS(TM).LT.TOL) GO TO 30
   20 CONTINUE
   30 G1 = -(S+S)
      GO TO 50
   40 CONTINUE
      G1 = (T1-T2)/DNU
   50 CONTINUE
      G2 = T1 + T2
      SMU = 1.0D0
      FC = 1.0D0/PI
      FLRX = LOG(RX)
      FMU = DNU*FLRX
      TM = 0.0D0
      IF (DNU.EQ.0.0D0) GO TO 60
      TM = SIN(DNU*HPI)/DNU
      TM = (DNU+DNU)*TM*TM
      FC = DNU/SIN(DNU*PI)
      IF (FMU.NE.0.0D0) SMU = SINH(FMU)/FMU
   60 CONTINUE
      F = FC*(G1*COSH(FMU)+G2*FLRX*SMU)
      FX = EXP(FMU)
      P = FC*T1*FX
      Q = FC*T2/FX
      G = F + TM*Q
      AK = 1.0D0
      CK = 1.0D0
      BK = 1.0D0
      S1 = G
      S2 = P
      IF (INU.GT.0 .OR. N.GT.1) GO TO 90
      IF (X.LT.TOL) GO TO 80
      CX = X*X*0.25D0
   70 CONTINUE
      F = (AK*F+P+Q)/(BK-DNU2)
      P = P/(AK-DNU)
      Q = Q/(AK+DNU)
      G = F + TM*Q
      CK = -CK*CX/AK
      T1 = CK*G
      S1 = S1 + T1
      BK = BK + AK + AK + 1.0D0
      AK = AK + 1.0D0
      S = ABS(T1)/(1.0D0+ABS(S1))
      IF (S.GT.TOL) GO TO 70
   80 CONTINUE
      Y(1) = -S1
      RETURN
   90 CONTINUE
      IF (X.LT.TOL) GO TO 110
      CX = X*X*0.25D0
  100 CONTINUE
      F = (AK*F+P+Q)/(BK-DNU2)
      P = P/(AK-DNU)
      Q = Q/(AK+DNU)
      G = F + TM*Q
      CK = -CK*CX/AK
      T1 = CK*G
      S1 = S1 + T1
      T2 = CK*(P-AK*G)
      S2 = S2 + T2
      BK = BK + AK + AK + 1.0D0
      AK = AK + 1.0D0
      S = ABS(T1)/(1.0D0+ABS(S1)) + ABS(T2)/(1.0D0+ABS(S2))
      IF (S.GT.TOL) GO TO 100
  110 CONTINUE
      S2 = -S2*RX
      S1 = -S1
      GO TO 160
  120 CONTINUE
      COEF = RTHPI/SQRT(X)
      IF (X.GT.X2) GO TO 210
C
C     MILLER ALGORITHM FOR X1.LT.X.LE.X2
C
      ETEST = COS(PI*DNU)/(PI*X*TOL)
      FKS = 1.0D0
      FHS = 0.25D0
      FK = 0.0D0
      RCK = 2.0D0
      CCK = X + X
      RP1 = 0.0D0
      CP1 = 0.0D0
      RP2 = 1.0D0
      CP2 = 0.0D0
      K = 0
  130 CONTINUE
      K = K + 1
      FK = FK + 1.0D0
      AK = (FHS-DNU2)/(FKS+FK)
      PT = FK + 1.0D0
      RBK = RCK/PT
      CBK = CCK/PT
      RPT = RP2
      CPT = CP2
      RP2 = RBK*RPT - CBK*CPT - AK*RP1
      CP2 = CBK*RPT + RBK*CPT - AK*CP1
      RP1 = RPT
      CP1 = CPT
      RB(K) = RBK
      CB(K) = CBK
      A(K) = AK
      RCK = RCK + 2.0D0
      FKS = FKS + FK + FK + 1.0D0
      FHS = FHS + FK + FK
      PT = MAX(ABS(RP1),ABS(CP1))
      FC = (RP1/PT)**2 + (CP1/PT)**2
      PT = PT*SQRT(FC)*FK
      IF (ETEST.GT.PT) GO TO 130
      KK = K
      RS = 1.0D0
      CS = 0.0D0
      RP1 = 0.0D0
      CP1 = 0.0D0
      RP2 = 1.0D0
      CP2 = 0.0D0
      DO 140 I=1,K
        RPT = RP2
        CPT = CP2
        RP2 = (RB(KK)*RPT-CB(KK)*CPT-RP1)/A(KK)
        CP2 = (CB(KK)*RPT+RB(KK)*CPT-CP1)/A(KK)
        RP1 = RPT
        CP1 = CPT
        RS = RS + RP2
        CS = CS + CP2
        KK = KK - 1
  140 CONTINUE
      PT = MAX(ABS(RS),ABS(CS))
      FC = (RS/PT)**2 + (CS/PT)**2
      PT = PT*SQRT(FC)
      RS1 = (RP2*(RS/PT)+CP2*(CS/PT))/PT
      CS1 = (CP2*(RS/PT)-RP2*(CS/PT))/PT
      FC = HPI*(DNU-0.5D0) - X
      P = COS(FC)
      Q = SIN(FC)
      S1 = (CS1*Q-RS1*P)*COEF
      IF (INU.GT.0 .OR. N.GT.1) GO TO 150
      Y(1) = S1
      RETURN
  150 CONTINUE
      PT = MAX(ABS(RP2),ABS(CP2))
      FC = (RP2/PT)**2 + (CP2/PT)**2
      PT = PT*SQRT(FC)
      RPT = DNU + 0.5D0 - (RP1*(RP2/PT)+CP1*(CP2/PT))/PT
      CPT = X - (CP1*(RP2/PT)-RP1*(CP2/PT))/PT
      CS2 = CS1*CPT - RS1*RPT
      RS2 = RPT*CS1 + RS1*CPT
      S2 = (RS2*Q+CS2*P)*COEF/X
C
C     FORWARD RECURSION ON THE THREE TERM RECURSION RELATION
C
  160 CONTINUE
      CK = (DNU+DNU+2.0D0)/X
      IF (N.EQ.1) INU = INU - 1
      IF (INU.GT.0) GO TO 170
      IF (N.GT.1) GO TO 190
      S1 = S2
      GO TO 190
  170 CONTINUE
      DO 180 I=1,INU
        ST = S2
        S2 = CK*S2 - S1
        S1 = ST
        CK = CK + RX
  180 CONTINUE
      IF (N.EQ.1) S1 = S2
  190 CONTINUE
      Y(1) = S1
      IF (N.EQ.1) RETURN
      Y(2) = S2
      IF (N.EQ.2) RETURN
      DO 200 I=3,N
        Y(I) = CK*Y(I-1) - Y(I-2)
        CK = CK + RX
  200 CONTINUE
      RETURN
C
C     ASYMPTOTIC EXPANSION FOR LARGE X, X.GT.X2
C
  210 CONTINUE
      NN = 2
      IF (INU.EQ.0 .AND. N.EQ.1) NN = 1
      DNU2 = DNU + DNU
      FMU = 0.0D0
      IF (ABS(DNU2).LT.TOL) GO TO 220
      FMU = DNU2*DNU2
  220 CONTINUE
      ARG = X - HPI*(DNU+0.5D0)
      SA = SIN(ARG)
      SB = COS(ARG)
      ETX = 8.0D0*X
      DO 250 K=1,NN
        S1 = S2
        T2 = (FMU-1.0D0)/ETX
        SS = T2
        RELB = TOL*ABS(T2)
        T1 = ETX
        S = 1.0D0
        FN = 1.0D0
        AK = 0.0D0
        DO 230 J=1,13
          T1 = T1 + ETX
          AK = AK + 8.0D0
          FN = FN + AK
          T2 = -T2*(FMU-FN)/T1
          S = S + T2
          T1 = T1 + ETX
          AK = AK + 8.0D0
          FN = FN + AK
          T2 = T2*(FMU-FN)/T1
          SS = SS + T2
          IF (ABS(T2).LE.RELB) GO TO 240
  230   CONTINUE
  240   S2 = COEF*(S*SA+SS*SB)
        FMU = FMU + 8.0D0*DNU + 4.0D0
        TB = SA
        SA = -SB
        SB = TB
  250 CONTINUE
      IF (NN.GT.1) GO TO 160
      S1 = S2
      GO TO 190
C
C     FNU=HALF ODD INTEGER CASE
C
  260 CONTINUE
      COEF = RTHPI/SQRT(X)
      S1 = COEF*SIN(X)
      S2 = -COEF*COS(X)
      GO TO 160
C
C
  270 CALL XERMSG ('SLATEC', 'DBSYNU', 'X NOT GREATER THAN ZERO', 2, 1)
      RETURN
  280 CALL XERMSG ('SLATEC', 'DBSYNU', 'FNU NOT ZERO OR POSITIVE', 2,
     +   1)
      RETURN
  290 CALL XERMSG ('SLATEC', 'DBSYNU', 'N NOT GREATER THAN 0', 2, 1)
      RETURN
      END
*DECK DJAIRY
      SUBROUTINE DJAIRY (X, RX, C, AI, DAI)
C***BEGIN PROLOGUE  DJAIRY
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DBESJ and DBESY
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (JAIRY-S, DJAIRY-D)
C***AUTHOR  Amos, D. E., (SNLA)
C           Daniel, S. L., (SNLA)
C           Weston, M. K., (SNLA)
C***DESCRIPTION
C
C                  DJAIRY computes the Airy function AI(X)
C                   and its derivative DAI(X) for DASYJY
C
C                                   INPUT
C
C         X - Argument, computed by DASYJY, X unrestricted
C        RX - RX=SQRT(ABS(X)), computed by DASYJY
C         C - C=2.*(ABS(X)**1.5)/3., computed by DASYJY
C
C                                  OUTPUT
C
C        AI - Value of function AI(X)
C       DAI - Value of the derivative DAI(X)
C
C***SEE ALSO  DBESJ, DBESY
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   750101  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   891009  Removed unreferenced variable.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated the AUTHOR section.  (WRB)
C***END PROLOGUE  DJAIRY
C
      INTEGER I, J, M1, M1D, M2, M2D, M3, M3D, M4, M4D, N1, N1D, N2,
     1 N2D, N3, N3D, N4, N4D
      DOUBLE PRECISION A,AI,AJN,AJP,AK1,AK2,AK3,B,C,CCV,CON2,
     1 CON3, CON4, CON5, CV, DA, DAI, DAJN, DAJP, DAK1, DAK2, DAK3,
     2 DB, EC, E1, E2, FPI12, F1, F2, RTRX, RX, SCV, T, TEMP1, TEMP2,
     3 TT, X
      DIMENSION AJP(19), AJN(19), A(15), B(15)
      DIMENSION AK1(14), AK2(23), AK3(14)
      DIMENSION DAJP(19), DAJN(19), DA(15), DB(15)
      DIMENSION DAK1(14), DAK2(24), DAK3(14)
      SAVE N1, N2, N3, N4, M1, M2, M3, M4, FPI12, CON2, CON3,
     1 CON4, CON5, AK1, AK2, AK3, AJP, AJN, A, B,
     2 N1D, N2D, N3D, N4D, M1D, M2D, M3D, M4D, DAK1, DAK2, DAK3,
     3 DAJP, DAJN, DA, DB
      DATA N1,N2,N3,N4/14,23,19,15/
      DATA M1,M2,M3,M4/12,21,17,13/
      DATA FPI12,CON2,CON3,CON4,CON5/
     1 1.30899693899575D+00, 5.03154716196777D+00, 3.80004589867293D-01,
     2 8.33333333333333D-01, 8.66025403784439D-01/
      DATA AK1(1), AK1(2), AK1(3), AK1(4), AK1(5), AK1(6), AK1(7),
     1     AK1(8), AK1(9), AK1(10),AK1(11),AK1(12),AK1(13),
     2     AK1(14)         / 2.20423090987793D-01,-1.25290242787700D-01,
     3 1.03881163359194D-02, 8.22844152006343D-04,-2.34614345891226D-04,
     4 1.63824280172116D-05, 3.06902589573189D-07,-1.29621999359332D-07,
     5 8.22908158823668D-09, 1.53963968623298D-11,-3.39165465615682D-11,
     6 2.03253257423626D-12,-1.10679546097884D-14,-5.16169497785080D-15/
      DATA AK2(1), AK2(2), AK2(3), AK2(4), AK2(5), AK2(6), AK2(7),
     1     AK2(8), AK2(9), AK2(10),AK2(11),AK2(12),AK2(13),AK2(14),
     2     AK2(15),AK2(16),AK2(17),AK2(18),AK2(19),AK2(20),AK2(21),
     3     AK2(22),AK2(23) / 2.74366150869598D-01, 5.39790969736903D-03,
     4-1.57339220621190D-03, 4.27427528248750D-04,-1.12124917399925D-04,
     5 2.88763171318904D-05,-7.36804225370554D-06, 1.87290209741024D-06,
     6-4.75892793962291D-07, 1.21130416955909D-07,-3.09245374270614D-08,
     7 7.92454705282654D-09,-2.03902447167914D-09, 5.26863056595742D-10,
     8-1.36704767639569D-10, 3.56141039013708D-11,-9.31388296548430D-12,
     9 2.44464450473635D-12,-6.43840261990955D-13, 1.70106030559349D-13,
     1-4.50760104503281D-14, 1.19774799164811D-14,-3.19077040865066D-15/
      DATA AK3(1), AK3(2), AK3(3), AK3(4), AK3(5), AK3(6), AK3(7),
     1     AK3(8), AK3(9), AK3(10),AK3(11),AK3(12),AK3(13),
     2     AK3(14)         / 2.80271447340791D-01,-1.78127042844379D-03,
     3 4.03422579628999D-05,-1.63249965269003D-06, 9.21181482476768D-08,
     4-6.52294330229155D-09, 5.47138404576546D-10,-5.24408251800260D-11,
     5 5.60477904117209D-12,-6.56375244639313D-13, 8.31285761966247D-14,
     6-1.12705134691063D-14, 1.62267976598129D-15,-2.46480324312426D-16/
      DATA AJP(1), AJP(2), AJP(3), AJP(4), AJP(5), AJP(6), AJP(7),
     1     AJP(8), AJP(9), AJP(10),AJP(11),AJP(12),AJP(13),AJP(14),
     2     AJP(15),AJP(16),AJP(17),AJP(18),
     3     AJP(19)         / 7.78952966437581D-02,-1.84356363456801D-01,
     4 3.01412605216174D-02, 3.05342724277608D-02,-4.95424702513079D-03,
     5-1.72749552563952D-03, 2.43137637839190D-04, 5.04564777517082D-05,
     6-6.16316582695208D-06,-9.03986745510768D-07, 9.70243778355884D-08,
     7 1.09639453305205D-08,-1.04716330588766D-09,-9.60359441344646D-11,
     8 8.25358789454134D-12, 6.36123439018768D-13,-4.96629614116015D-14,
     9-3.29810288929615D-15, 2.35798252031104D-16/
      DATA AJN(1), AJN(2), AJN(3), AJN(4), AJN(5), AJN(6), AJN(7),
     1     AJN(8), AJN(9), AJN(10),AJN(11),AJN(12),AJN(13),AJN(14),
     2     AJN(15),AJN(16),AJN(17),AJN(18),
     3     AJN(19)         / 3.80497887617242D-02,-2.45319541845546D-01,
     4 1.65820623702696D-01, 7.49330045818789D-02,-2.63476288106641D-02,
     5-5.92535597304981D-03, 1.44744409589804D-03, 2.18311831322215D-04,
     6-4.10662077680304D-05,-4.66874994171766D-06, 7.15218807277160D-07,
     7 6.52964770854633D-08,-8.44284027565946D-09,-6.44186158976978D-10,
     8 7.20802286505285D-11, 4.72465431717846D-12,-4.66022632547045D-13,
     9-2.67762710389189D-14, 2.36161316570019D-15/
      DATA A(1),   A(2),   A(3),   A(4),   A(5),   A(6),   A(7),
     1     A(8),   A(9),   A(10),  A(11),  A(12),  A(13),  A(14),
     2     A(15)           / 4.90275424742791D-01, 1.57647277946204D-03,
     3-9.66195963140306D-05, 1.35916080268815D-07, 2.98157342654859D-07,
     4-1.86824767559979D-08,-1.03685737667141D-09, 3.28660818434328D-10,
     5-2.57091410632780D-11,-2.32357655300677D-12, 9.57523279048255D-13,
     6-1.20340828049719D-13,-2.90907716770715D-15, 4.55656454580149D-15,
     7-9.99003874810259D-16/
      DATA B(1),   B(2),   B(3),   B(4),   B(5),   B(6),   B(7),
     1     B(8),   B(9),   B(10),  B(11),  B(12),  B(13),  B(14),
     2     B(15)           / 2.78593552803079D-01,-3.52915691882584D-03,
     3-2.31149677384994D-05, 4.71317842263560D-06,-1.12415907931333D-07,
     4-2.00100301184339D-08, 2.60948075302193D-09,-3.55098136101216D-11,
     5-3.50849978423875D-11, 5.83007187954202D-12,-2.04644828753326D-13,
     6-1.10529179476742D-13, 2.87724778038775D-14,-2.88205111009939D-15,
     7-3.32656311696166D-16/
      DATA N1D,N2D,N3D,N4D/14,24,19,15/
      DATA M1D,M2D,M3D,M4D/12,22,17,13/
      DATA DAK1(1), DAK1(2), DAK1(3), DAK1(4), DAK1(5), DAK1(6),
     1     DAK1(7), DAK1(8), DAK1(9), DAK1(10),DAK1(11),DAK1(12),
     2    DAK1(13),DAK1(14)/ 2.04567842307887D-01,-6.61322739905664D-02,
     3-8.49845800989287D-03, 3.12183491556289D-03,-2.70016489829432D-04,
     4-6.35636298679387D-06, 3.02397712409509D-06,-2.18311195330088D-07,
     5-5.36194289332826D-10, 1.13098035622310D-09,-7.43023834629073D-11,
     6 4.28804170826891D-13, 2.23810925754539D-13,-1.39140135641182D-14/
      DATA DAK2(1), DAK2(2), DAK2(3), DAK2(4), DAK2(5), DAK2(6),
     1     DAK2(7), DAK2(8), DAK2(9), DAK2(10),DAK2(11),DAK2(12),
     2     DAK2(13),DAK2(14),DAK2(15),DAK2(16),DAK2(17),DAK2(18),
     3     DAK2(19),DAK2(20),DAK2(21),DAK2(22),DAK2(23),
     4     DAK2(24)        / 2.93332343883230D-01,-8.06196784743112D-03,
     5 2.42540172333140D-03,-6.82297548850235D-04, 1.85786427751181D-04,
     6-4.97457447684059D-05, 1.32090681239497D-05,-3.49528240444943D-06,
     7 9.24362451078835D-07,-2.44732671521867D-07, 6.49307837648910D-08,
     8-1.72717621501538D-08, 4.60725763604656D-09,-1.23249055291550D-09,
     9 3.30620409488102D-10,-8.89252099772401D-11, 2.39773319878298D-11,
     1-6.48013921153450D-12, 1.75510132023731D-12,-4.76303829833637D-13,
     2 1.29498241100810D-13,-3.52679622210430D-14, 9.62005151585923D-15,
     3-2.62786914342292D-15/
      DATA DAK3(1), DAK3(2), DAK3(3), DAK3(4), DAK3(5), DAK3(6),
     1     DAK3(7), DAK3(8), DAK3(9), DAK3(10),DAK3(11),DAK3(12),
     2    DAK3(13),DAK3(14)/ 2.84675828811349D-01, 2.53073072619080D-03,
     3-4.83481130337976D-05, 1.84907283946343D-06,-1.01418491178576D-07,
     4 7.05925634457153D-09,-5.85325291400382D-10, 5.56357688831339D-11,
     5-5.90889094779500D-12, 6.88574353784436D-13,-8.68588256452194D-14,
     6 1.17374762617213D-14,-1.68523146510923D-15, 2.55374773097056D-16/
      DATA DAJP(1), DAJP(2), DAJP(3), DAJP(4), DAJP(5), DAJP(6),
     1     DAJP(7), DAJP(8), DAJP(9), DAJP(10),DAJP(11),DAJP(12),
     2     DAJP(13),DAJP(14),DAJP(15),DAJP(16),DAJP(17),DAJP(18),
     3     DAJP(19)        / 6.53219131311457D-02,-1.20262933688823D-01,
     4 9.78010236263823D-03, 1.67948429230505D-02,-1.97146140182132D-03,
     5-8.45560295098867D-04, 9.42889620701976D-05, 2.25827860945475D-05,
     6-2.29067870915987D-06,-3.76343991136919D-07, 3.45663933559565D-08,
     7 4.29611332003007D-09,-3.58673691214989D-10,-3.57245881361895D-11,
     8 2.72696091066336D-12, 2.26120653095771D-13,-1.58763205238303D-14,
     9-1.12604374485125D-15, 7.31327529515367D-17/
      DATA DAJN(1), DAJN(2), DAJN(3), DAJN(4), DAJN(5), DAJN(6),
     1     DAJN(7), DAJN(8), DAJN(9), DAJN(10),DAJN(11),DAJN(12),
     2     DAJN(13),DAJN(14),DAJN(15),DAJN(16),DAJN(17),DAJN(18),
     3     DAJN(19)        / 1.08594539632967D-02, 8.53313194857091D-02,
     4-3.15277068113058D-01,-8.78420725294257D-02, 5.53251906976048D-02,
     5 9.41674060503241D-03,-3.32187026018996D-03,-4.11157343156826D-04,
     6 1.01297326891346D-04, 9.87633682208396D-06,-1.87312969812393D-06,
     7-1.50798500131468D-07, 2.32687669525394D-08, 1.59599917419225D-09,
     8-2.07665922668385D-10,-1.24103350500302D-11, 1.39631765331043D-12,
     9 7.39400971155740D-14,-7.32887475627500D-15/
      DATA DA(1),  DA(2),  DA(3),  DA(4),  DA(5),  DA(6),  DA(7),
     1     DA(8),  DA(9),  DA(10), DA(11), DA(12), DA(13), DA(14),
     2     DA(15)          / 4.91627321104601D-01, 3.11164930427489D-03,
     3 8.23140762854081D-05,-4.61769776172142D-06,-6.13158880534626D-08,
     4 2.87295804656520D-08,-1.81959715372117D-09,-1.44752826642035D-10,
     5 4.53724043420422D-11,-3.99655065847223D-12,-3.24089119830323D-13,
     6 1.62098952568741D-13,-2.40765247974057D-14, 1.69384811284491D-16,
     7 8.17900786477396D-16/
      DATA DB(1),  DB(2),  DB(3),  DB(4),  DB(5),  DB(6),  DB(7),
     1     DB(8),  DB(9),  DB(10), DB(11), DB(12), DB(13), DB(14),
     2     DB(15)          /-2.77571356944231D-01, 4.44212833419920D-03,
     3-8.42328522190089D-05,-2.58040318418710D-06, 3.42389720217621D-07,
     4-6.24286894709776D-09,-2.36377836844577D-09, 3.16991042656673D-10,
     5-4.40995691658191D-12,-5.18674221093575D-12, 9.64874015137022D-13,
     6-4.90190576608710D-14,-1.77253430678112D-14, 5.55950610442662D-15,
     7-7.11793337579530D-16/
C***FIRST EXECUTABLE STATEMENT  DJAIRY
      IF (X.LT.0.0D0) GO TO 90
      IF (C.GT.5.0D0) GO TO 60
      IF (X.GT.1.20D0) GO TO 30
      T = (X+X-1.2D0)*CON4
      TT = T + T
      J = N1
      F1 = AK1(J)
      F2 = 0.0D0
      DO 10 I=1,M1
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + AK1(J)
        F2 = TEMP1
   10 CONTINUE
      AI = T*F1 - F2 + AK1(1)
C
      J = N1D
      F1 = DAK1(J)
      F2 = 0.0D0
      DO 20 I=1,M1D
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + DAK1(J)
        F2 = TEMP1
   20 CONTINUE
      DAI = -(T*F1-F2+DAK1(1))
      RETURN
C
   30 CONTINUE
      T = (X+X-CON2)*CON3
      TT = T + T
      J = N2
      F1 = AK2(J)
      F2 = 0.0D0
      DO 40 I=1,M2
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + AK2(J)
        F2 = TEMP1
   40 CONTINUE
      RTRX = SQRT(RX)
      EC = EXP(-C)
      AI = EC*(T*F1-F2+AK2(1))/RTRX
      J = N2D
      F1 = DAK2(J)
      F2 = 0.0D0
      DO 50 I=1,M2D
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + DAK2(J)
        F2 = TEMP1
   50 CONTINUE
      DAI = -EC*(T*F1-F2+DAK2(1))*RTRX
      RETURN
C
   60 CONTINUE
      T = 10.0D0/C - 1.0D0
      TT = T + T
      J = N1
      F1 = AK3(J)
      F2 = 0.0D0
      DO 70 I=1,M1
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + AK3(J)
        F2 = TEMP1
   70 CONTINUE
      RTRX = SQRT(RX)
      EC = EXP(-C)
      AI = EC*(T*F1-F2+AK3(1))/RTRX
      J = N1D
      F1 = DAK3(J)
      F2 = 0.0D0
      DO 80 I=1,M1D
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + DAK3(J)
        F2 = TEMP1
   80 CONTINUE
      DAI = -RTRX*EC*(T*F1-F2+DAK3(1))
      RETURN
C
   90 CONTINUE
      IF (C.GT.5.0D0) GO TO 120
      T = 0.4D0*C - 1.0D0
      TT = T + T
      J = N3
      F1 = AJP(J)
      E1 = AJN(J)
      F2 = 0.0D0
      E2 = 0.0D0
      DO 100 I=1,M3
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + AJP(J)
        E1 = TT*E1 - E2 + AJN(J)
        F2 = TEMP1
        E2 = TEMP2
  100 CONTINUE
      AI = (T*E1-E2+AJN(1)) - X*(T*F1-F2+AJP(1))
      J = N3D
      F1 = DAJP(J)
      E1 = DAJN(J)
      F2 = 0.0D0
      E2 = 0.0D0
      DO 110 I=1,M3D
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + DAJP(J)
        E1 = TT*E1 - E2 + DAJN(J)
        F2 = TEMP1
        E2 = TEMP2
  110 CONTINUE
      DAI = X*X*(T*F1-F2+DAJP(1)) + (T*E1-E2+DAJN(1))
      RETURN
C
  120 CONTINUE
      T = 10.0D0/C - 1.0D0
      TT = T + T
      J = N4
      F1 = A(J)
      E1 = B(J)
      F2 = 0.0D0
      E2 = 0.0D0
      DO 130 I=1,M4
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + A(J)
        E1 = TT*E1 - E2 + B(J)
        F2 = TEMP1
        E2 = TEMP2
  130 CONTINUE
      TEMP1 = T*F1 - F2 + A(1)
      TEMP2 = T*E1 - E2 + B(1)
      RTRX = SQRT(RX)
      CV = C - FPI12
      CCV = COS(CV)
      SCV = SIN(CV)
      AI = (TEMP1*CCV-TEMP2*SCV)/RTRX
      J = N4D
      F1 = DA(J)
      E1 = DB(J)
      F2 = 0.0D0
      E2 = 0.0D0
      DO 140 I=1,M4D
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + DA(J)
        E1 = TT*E1 - E2 + DB(J)
        F2 = TEMP1
        E2 = TEMP2
  140 CONTINUE
      TEMP1 = T*F1 - F2 + DA(1)
      TEMP2 = T*E1 - E2 + DB(1)
      E1 = CCV*CON5 + 0.5D0*SCV
      E2 = SCV*CON5 - 0.5D0*CCV
      DAI = (TEMP1*E1-TEMP2*E2)*RTRX
      RETURN
      END
*DECK DYAIRY
      SUBROUTINE DYAIRY (X, RX, C, BI, DBI)
C***BEGIN PROLOGUE  DYAIRY
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DBESJ and DBESY
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (YAIRY-S, DYAIRY-D)
C***AUTHOR  Amos, D. E., (SNLA)
C           Daniel, S. L., (SNLA)
C***DESCRIPTION
C
C                  DYAIRY computes the Airy function BI(X)
C                   and its derivative DBI(X) for DASYJY
C
C                                     INPUT
C
C         X  - Argument, computed by DASYJY, X unrestricted
C        RX  - RX=SQRT(ABS(X)), computed by DASYJY
C         C  - C=2.*(ABS(X)**1.5)/3., computed by DASYJY
C
C                                    OUTPUT
C        BI  - Value of function BI(X)
C       DBI  - Value of the derivative DBI(X)
C
C***SEE ALSO  DBESJ, DBESY
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   750101  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated the AUTHOR section.  (WRB)
C***END PROLOGUE  DYAIRY
C
      INTEGER I, J, M1, M1D, M2, M2D, M3, M3D, M4D, N1, N1D, N2, N2D,
     1 N3, N3D, N4D
      DOUBLE PRECISION AA,AX,BB,BI,BJN,BJP,BK1,BK2,BK3,BK4,C,CON1,CON2,
     1 CON3, CV, DAA, DBB, DBI, DBJN, DBJP, DBK1, DBK2, DBK3, DBK4, D1,
     2 D2, EX, E1, E2, FPI12, F1, F2, RTRX, RX, SPI12, S1, S2, T, TC,
     3 TEMP1, TEMP2, TT, X
      DIMENSION BK1(20), BK2(20), BK3(20), BK4(14)
      DIMENSION BJP(19), BJN(19), AA(14), BB(14)
      DIMENSION DBK1(21), DBK2(20), DBK3(20), DBK4(14)
      DIMENSION DBJP(19), DBJN(19), DAA(14), DBB(14)
      SAVE N1, N2, N3, M1, M2, M3, N1D, N2D, N3D, N4D,
     1 M1D, M2D, M3D, M4D, FPI12, SPI12, CON1, CON2, CON3,
     2 BK1, BK2, BK3, BK4, BJN, BJP, AA, BB, DBK1, DBK2, DBK3, DBK4,
     3 DBJP, DBJN, DAA, DBB
      DATA N1,N2,N3/20,19,14/
      DATA M1,M2,M3/18,17,12/
      DATA N1D,N2D,N3D,N4D/21,20,19,14/
      DATA M1D,M2D,M3D,M4D/19,18,17,12/
      DATA FPI12,SPI12,CON1,CON2,CON3/
     1 1.30899693899575D+00, 1.83259571459405D+00, 6.66666666666667D-01,
     2 7.74148278841779D+00, 3.64766105490356D-01/
      DATA BK1(1),  BK1(2),  BK1(3),  BK1(4),  BK1(5),  BK1(6),
     1     BK1(7),  BK1(8),  BK1(9),  BK1(10), BK1(11), BK1(12),
     2     BK1(13), BK1(14), BK1(15), BK1(16), BK1(17), BK1(18),
     3     BK1(19), BK1(20)/ 2.43202846447449D+00, 2.57132009754685D+00,
     4 1.02802341258616D+00, 3.41958178205872D-01, 8.41978629889284D-02,
     5 1.93877282587962D-02, 3.92687837130335D-03, 6.83302689948043D-04,
     6 1.14611403991141D-04, 1.74195138337086D-05, 2.41223620956355D-06,
     7 3.24525591983273D-07, 4.03509798540183D-08, 4.70875059642296D-09,
     8 5.35367432585889D-10, 5.70606721846334D-11, 5.80526363709933D-12,
     9 5.76338988616388D-13, 5.42103834518071D-14, 4.91857330301677D-15/
      DATA BK2(1),  BK2(2),  BK2(3),  BK2(4),  BK2(5),  BK2(6),
     1     BK2(7),  BK2(8),  BK2(9),  BK2(10), BK2(11), BK2(12),
     2     BK2(13), BK2(14), BK2(15), BK2(16), BK2(17), BK2(18),
     3     BK2(19), BK2(20)/ 5.74830555784088D-01,-6.91648648376891D-03,
     4 1.97460263052093D-03,-5.24043043868823D-04, 1.22965147239661D-04,
     5-2.27059514462173D-05, 2.23575555008526D-06, 4.15174955023899D-07,
     6-2.84985752198231D-07, 8.50187174775435D-08,-1.70400826891326D-08,
     7 2.25479746746889D-09,-1.09524166577443D-10,-3.41063845099711D-11,
     8 1.11262893886662D-11,-1.75542944241734D-12, 1.36298600401767D-13,
     9 8.76342105755664D-15,-4.64063099157041D-15, 7.78772758732960D-16/
      DATA BK3(1),  BK3(2),  BK3(3),  BK3(4),  BK3(5),  BK3(6),
     1     BK3(7),  BK3(8),  BK3(9),  BK3(10), BK3(11), BK3(12),
     2     BK3(13), BK3(14), BK3(15), BK3(16), BK3(17), BK3(18),
     3     BK3(19), BK3(20)/ 5.66777053506912D-01, 2.63672828349579D-03,
     4 5.12303351473130D-05, 2.10229231564492D-06, 1.42217095113890D-07,
     5 1.28534295891264D-08, 7.28556219407507D-10,-3.45236157301011D-10,
     6-2.11919115912724D-10,-6.56803892922376D-11,-8.14873160315074D-12,
     7 3.03177845632183D-12, 1.73447220554115D-12, 1.67935548701554D-13,
     8-1.49622868806719D-13,-5.15470458953407D-14, 8.75741841857830D-15,
     9 7.96735553525720D-15,-1.29566137861742D-16,-1.11878794417520D-15/
      DATA BK4(1),  BK4(2),  BK4(3),  BK4(4),  BK4(5),  BK4(6),
     1     BK4(7),  BK4(8),  BK4(9),  BK4(10), BK4(11), BK4(12),
     2     BK4(13), BK4(14)/ 4.85444386705114D-01,-3.08525088408463D-03,
     3 6.98748404837928D-05,-2.82757234179768D-06, 1.59553313064138D-07,
     4-1.12980692144601D-08, 9.47671515498754D-10,-9.08301736026423D-11,
     5 9.70776206450724D-12,-1.13687527254574D-12, 1.43982917533415D-13,
     6-1.95211019558815D-14, 2.81056379909357D-15,-4.26916444775176D-16/
      DATA BJP(1),  BJP(2),  BJP(3),  BJP(4),  BJP(5),  BJP(6),
     1     BJP(7),  BJP(8),  BJP(9),  BJP(10), BJP(11), BJP(12),
     2     BJP(13), BJP(14), BJP(15), BJP(16), BJP(17), BJP(18),
     3     BJP(19)         / 1.34918611457638D-01,-3.19314588205813D-01,
     4 5.22061946276114D-02, 5.28869112170312D-02,-8.58100756077350D-03,
     5-2.99211002025555D-03, 4.21126741969759D-04, 8.73931830369273D-05,
     6-1.06749163477533D-05,-1.56575097259349D-06, 1.68051151983999D-07,
     7 1.89901103638691D-08,-1.81374004961922D-09,-1.66339134593739D-10,
     8 1.42956335780810D-11, 1.10179811626595D-12,-8.60187724192263D-14,
     9-5.71248177285064D-15, 4.08414552853803D-16/
      DATA BJN(1),  BJN(2),  BJN(3),  BJN(4),  BJN(5),  BJN(6),
     1     BJN(7),  BJN(8),  BJN(9),  BJN(10), BJN(11), BJN(12),
     2     BJN(13), BJN(14), BJN(15), BJN(16), BJN(17), BJN(18),
     3     BJN(19)         / 6.59041673525697D-02,-4.24905910566004D-01,
     4 2.87209745195830D-01, 1.29787771099606D-01,-4.56354317590358D-02,
     5-1.02630175982540D-02, 2.50704671521101D-03, 3.78127183743483D-04,
     6-7.11287583284084D-05,-8.08651210688923D-06, 1.23879531273285D-06,
     7 1.13096815867279D-07,-1.46234283176310D-08,-1.11576315688077D-09,
     8 1.24846618243897D-10, 8.18334132555274D-12,-8.07174877048484D-13,
     9-4.63778618766425D-14, 4.09043399081631D-15/
      DATA AA(1),   AA(2),   AA(3),   AA(4),   AA(5),   AA(6),
     1     AA(7),   AA(8),   AA(9),   AA(10),  AA(11),  AA(12),
     2     AA(13),  AA(14) /-2.78593552803079D-01, 3.52915691882584D-03,
     3 2.31149677384994D-05,-4.71317842263560D-06, 1.12415907931333D-07,
     4 2.00100301184339D-08,-2.60948075302193D-09, 3.55098136101216D-11,
     5 3.50849978423875D-11,-5.83007187954202D-12, 2.04644828753326D-13,
     6 1.10529179476742D-13,-2.87724778038775D-14, 2.88205111009939D-15/
      DATA BB(1),   BB(2),   BB(3),   BB(4),   BB(5),   BB(6),
     1     BB(7),   BB(8),   BB(9),   BB(10),  BB(11),  BB(12),
     2     BB(13),  BB(14) /-4.90275424742791D-01,-1.57647277946204D-03,
     3 9.66195963140306D-05,-1.35916080268815D-07,-2.98157342654859D-07,
     4 1.86824767559979D-08, 1.03685737667141D-09,-3.28660818434328D-10,
     5 2.57091410632780D-11, 2.32357655300677D-12,-9.57523279048255D-13,
     6 1.20340828049719D-13, 2.90907716770715D-15,-4.55656454580149D-15/
      DATA DBK1(1), DBK1(2), DBK1(3), DBK1(4), DBK1(5), DBK1(6),
     1     DBK1(7), DBK1(8), DBK1(9), DBK1(10),DBK1(11),DBK1(12),
     2     DBK1(13),DBK1(14),DBK1(15),DBK1(16),DBK1(17),DBK1(18),
     3     DBK1(19),DBK1(20),
     4     DBK1(21)        / 2.95926143981893D+00, 3.86774568440103D+00,
     5 1.80441072356289D+00, 5.78070764125328D-01, 1.63011468174708D-01,
     6 3.92044409961855D-02, 7.90964210433812D-03, 1.50640863167338D-03,
     7 2.56651976920042D-04, 3.93826605867715D-05, 5.81097771463818D-06,
     8 7.86881233754659D-07, 9.93272957325739D-08, 1.21424205575107D-08,
     9 1.38528332697707D-09, 1.50190067586758D-10, 1.58271945457594D-11,
     1 1.57531847699042D-12, 1.50774055398181D-13, 1.40594335806564D-14,
     2 1.24942698777218D-15/
      DATA DBK2(1), DBK2(2), DBK2(3), DBK2(4), DBK2(5), DBK2(6),
     1     DBK2(7), DBK2(8), DBK2(9), DBK2(10),DBK2(11),DBK2(12),
     2     DBK2(13),DBK2(14),DBK2(15),DBK2(16),DBK2(17),DBK2(18),
     3    DBK2(19),DBK2(20)/ 5.49756809432471D-01, 9.13556983276901D-03,
     4-2.53635048605507D-03, 6.60423795342054D-04,-1.55217243135416D-04,
     5 3.00090325448633D-05,-3.76454339467348D-06,-1.33291331611616D-07,
     6 2.42587371049013D-07,-8.07861075240228D-08, 1.71092818861193D-08,
     7-2.41087357570599D-09, 1.53910848162371D-10, 2.56465373190630D-11,
     8-9.88581911653212D-12, 1.60877986412631D-12,-1.20952524741739D-13,
     9-1.06978278410820D-14, 5.02478557067561D-15,-8.68986130935886D-16/
      DATA DBK3(1), DBK3(2), DBK3(3), DBK3(4), DBK3(5), DBK3(6),
     1     DBK3(7), DBK3(8), DBK3(9), DBK3(10),DBK3(11),DBK3(12),
     2     DBK3(13),DBK3(14),DBK3(15),DBK3(16),DBK3(17),DBK3(18),
     3    DBK3(19),DBK3(20)/ 5.60598509354302D-01,-3.64870013248135D-03,
     4-5.98147152307417D-05,-2.33611595253625D-06,-1.64571516521436D-07,
     5-2.06333012920569D-08,-4.27745431573110D-09,-1.08494137799276D-09,
     6-2.37207188872763D-10,-2.22132920864966D-11, 1.07238008032138D-11,
     7 5.71954845245808D-12, 7.51102737777835D-13,-3.81912369483793D-13,
     8-1.75870057119257D-13, 6.69641694419084D-15, 2.26866724792055D-14,
     9 2.69898141356743D-15,-2.67133612397359D-15,-6.54121403165269D-16/
      DATA DBK4(1), DBK4(2), DBK4(3), DBK4(4), DBK4(5), DBK4(6),
     1     DBK4(7), DBK4(8), DBK4(9), DBK4(10),DBK4(11),DBK4(12),
     2    DBK4(13),DBK4(14)/ 4.93072999188036D-01, 4.38335419803815D-03,
     3-8.37413882246205D-05, 3.20268810484632D-06,-1.75661979548270D-07,
     4 1.22269906524508D-08,-1.01381314366052D-09, 9.63639784237475D-11,
     5-1.02344993379648D-11, 1.19264576554355D-12,-1.50443899103287D-13,
     6 2.03299052379349D-14,-2.91890652008292D-15, 4.42322081975475D-16/
      DATA DBJP(1), DBJP(2), DBJP(3), DBJP(4), DBJP(5), DBJP(6),
     1     DBJP(7), DBJP(8), DBJP(9), DBJP(10),DBJP(11),DBJP(12),
     2     DBJP(13),DBJP(14),DBJP(15),DBJP(16),DBJP(17),DBJP(18),
     3     DBJP(19)        / 1.13140872390745D-01,-2.08301511416328D-01,
     4 1.69396341953138D-02, 2.90895212478621D-02,-3.41467131311549D-03,
     5-1.46455339197417D-03, 1.63313272898517D-04, 3.91145328922162D-05,
     6-3.96757190808119D-06,-6.51846913772395D-07, 5.98707495269280D-08,
     7 7.44108654536549D-09,-6.21241056522632D-10,-6.18768017313526D-11,
     8 4.72323484752324D-12, 3.91652459802532D-13,-2.74985937845226D-14,
     9-1.95036497762750D-15, 1.26669643809444D-16/
      DATA DBJN(1), DBJN(2), DBJN(3), DBJN(4), DBJN(5), DBJN(6),
     1     DBJN(7), DBJN(8), DBJN(9), DBJN(10),DBJN(11),DBJN(12),
     2     DBJN(13),DBJN(14),DBJN(15),DBJN(16),DBJN(17),DBJN(18),
     3     DBJN(19)        /-1.88091260068850D-02,-1.47798180826140D-01,
     4 5.46075900433171D-01, 1.52146932663116D-01,-9.58260412266886D-02,
     5-1.63102731696130D-02, 5.75364806680105D-03, 7.12145408252655D-04,
     6-1.75452116846724D-04,-1.71063171685128D-05, 3.24435580631680D-06,
     7 2.61190663932884D-07,-4.03026865912779D-08,-2.76435165853895D-09,
     8 3.59687929062312D-10, 2.14953308456051D-11,-2.41849311903901D-12,
     9-1.28068004920751D-13, 1.26939834401773D-14/
      DATA DAA(1),  DAA(2),  DAA(3),  DAA(4),  DAA(5),  DAA(6),
     1     DAA(7),  DAA(8),  DAA(9),  DAA(10), DAA(11), DAA(12),
     2     DAA(13), DAA(14)/ 2.77571356944231D-01,-4.44212833419920D-03,
     3 8.42328522190089D-05, 2.58040318418710D-06,-3.42389720217621D-07,
     4 6.24286894709776D-09, 2.36377836844577D-09,-3.16991042656673D-10,
     5 4.40995691658191D-12, 5.18674221093575D-12,-9.64874015137022D-13,
     6 4.90190576608710D-14, 1.77253430678112D-14,-5.55950610442662D-15/
      DATA DBB(1),  DBB(2),  DBB(3),  DBB(4),  DBB(5),  DBB(6),
     1     DBB(7),  DBB(8),  DBB(9),  DBB(10), DBB(11), DBB(12),
     2     DBB(13), DBB(14)/ 4.91627321104601D-01, 3.11164930427489D-03,
     3 8.23140762854081D-05,-4.61769776172142D-06,-6.13158880534626D-08,
     4 2.87295804656520D-08,-1.81959715372117D-09,-1.44752826642035D-10,
     5 4.53724043420422D-11,-3.99655065847223D-12,-3.24089119830323D-13,
     6 1.62098952568741D-13,-2.40765247974057D-14, 1.69384811284491D-16/
C***FIRST EXECUTABLE STATEMENT  DYAIRY
      AX = ABS(X)
      RX = SQRT(AX)
      C = CON1*AX*RX
      IF (X.LT.0.0D0) GO TO 120
      IF (C.GT.8.0D0) GO TO 60
      IF (X.GT.2.5D0) GO TO 30
      T = (X+X-2.5D0)*0.4D0
      TT = T + T
      J = N1
      F1 = BK1(J)
      F2 = 0.0D0
      DO 10 I=1,M1
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + BK1(J)
        F2 = TEMP1
   10 CONTINUE
      BI = T*F1 - F2 + BK1(1)
      J = N1D
      F1 = DBK1(J)
      F2 = 0.0D0
      DO 20 I=1,M1D
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + DBK1(J)
        F2 = TEMP1
   20 CONTINUE
      DBI = T*F1 - F2 + DBK1(1)
      RETURN
   30 CONTINUE
      RTRX = SQRT(RX)
      T = (X+X-CON2)*CON3
      TT = T + T
      J = N1
      F1 = BK2(J)
      F2 = 0.0D0
      DO 40 I=1,M1
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + BK2(J)
        F2 = TEMP1
   40 CONTINUE
      BI = (T*F1-F2+BK2(1))/RTRX
      EX = EXP(C)
      BI = BI*EX
      J = N2D
      F1 = DBK2(J)
      F2 = 0.0D0
      DO 50 I=1,M2D
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + DBK2(J)
        F2 = TEMP1
   50 CONTINUE
      DBI = (T*F1-F2+DBK2(1))*RTRX
      DBI = DBI*EX
      RETURN
C
   60 CONTINUE
      RTRX = SQRT(RX)
      T = 16.0D0/C - 1.0D0
      TT = T + T
      J = N1
      F1 = BK3(J)
      F2 = 0.0D0
      DO 70 I=1,M1
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + BK3(J)
        F2 = TEMP1
   70 CONTINUE
      S1 = T*F1 - F2 + BK3(1)
      J = N2D
      F1 = DBK3(J)
      F2 = 0.0D0
      DO 80 I=1,M2D
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + DBK3(J)
        F2 = TEMP1
   80 CONTINUE
      D1 = T*F1 - F2 + DBK3(1)
      TC = C + C
      EX = EXP(C)
      IF (TC.GT.35.0D0) GO TO 110
      T = 10.0D0/C - 1.0D0
      TT = T + T
      J = N3
      F1 = BK4(J)
      F2 = 0.0D0
      DO 90 I=1,M3
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + BK4(J)
        F2 = TEMP1
   90 CONTINUE
      S2 = T*F1 - F2 + BK4(1)
      BI = (S1+EXP(-TC)*S2)/RTRX
      BI = BI*EX
      J = N4D
      F1 = DBK4(J)
      F2 = 0.0D0
      DO 100 I=1,M4D
        J = J - 1
        TEMP1 = F1
        F1 = TT*F1 - F2 + DBK4(J)
        F2 = TEMP1
  100 CONTINUE
      D2 = T*F1 - F2 + DBK4(1)
      DBI = RTRX*(D1+EXP(-TC)*D2)
      DBI = DBI*EX
      RETURN
  110 BI = EX*S1/RTRX
      DBI = EX*RTRX*D1
      RETURN
C
  120 CONTINUE
      IF (C.GT.5.0D0) GO TO 150
      T = 0.4D0*C - 1.0D0
      TT = T + T
      J = N2
      F1 = BJP(J)
      E1 = BJN(J)
      F2 = 0.0D0
      E2 = 0.0D0
      DO 130 I=1,M2
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + BJP(J)
        E1 = TT*E1 - E2 + BJN(J)
        F2 = TEMP1
        E2 = TEMP2
  130 CONTINUE
      BI = (T*E1-E2+BJN(1)) - AX*(T*F1-F2+BJP(1))
      J = N3D
      F1 = DBJP(J)
      E1 = DBJN(J)
      F2 = 0.0D0
      E2 = 0.0D0
      DO 140 I=1,M3D
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + DBJP(J)
        E1 = TT*E1 - E2 + DBJN(J)
        F2 = TEMP1
        E2 = TEMP2
  140 CONTINUE
      DBI = X*X*(T*F1-F2+DBJP(1)) + (T*E1-E2+DBJN(1))
      RETURN
C
  150 CONTINUE
      RTRX = SQRT(RX)
      T = 10.0D0/C - 1.0D0
      TT = T + T
      J = N3
      F1 = AA(J)
      E1 = BB(J)
      F2 = 0.0D0
      E2 = 0.0D0
      DO 160 I=1,M3
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + AA(J)
        E1 = TT*E1 - E2 + BB(J)
        F2 = TEMP1
        E2 = TEMP2
  160 CONTINUE
      TEMP1 = T*F1 - F2 + AA(1)
      TEMP2 = T*E1 - E2 + BB(1)
      CV = C - FPI12
      BI = (TEMP1*COS(CV)+TEMP2*SIN(CV))/RTRX
      J = N4D
      F1 = DAA(J)
      E1 = DBB(J)
      F2 = 0.0D0
      E2 = 0.0D0
      DO 170 I=1,M4D
        J = J - 1
        TEMP1 = F1
        TEMP2 = E1
        F1 = TT*F1 - F2 + DAA(J)
        E1 = TT*E1 - E2 + DBB(J)
        F2 = TEMP1
        E2 = TEMP2
  170 CONTINUE
      TEMP1 = T*F1 - F2 + DAA(1)
      TEMP2 = T*E1 - E2 + DBB(1)
      CV = C - SPI12
      DBI = (TEMP1*COS(CV)-TEMP2*SIN(CV))*RTRX
      RETURN
      END
*DECK XERMSG
      SUBROUTINE XERMSG (LIBRAR, SUBROU, MESSG, NERR, LEVEL)
C***BEGIN PROLOGUE  XERMSG
C***PURPOSE  Process error messages for SLATEC and other libraries.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XERMSG-A)
C***KEYWORDS  ERROR MESSAGE, XERROR
C***AUTHOR  Fong, Kirby, (NMFECC at LLNL)
C***DESCRIPTION
C
C   XERMSG processes a diagnostic message in a manner determined by the
C   value of LEVEL and the current value of the library error control
C   flag, KONTRL.  See subroutine XSETF for details.
C
C    LIBRAR   A character constant (or character variable) with the name
C             of the library.  This will be 'SLATEC' for the SLATEC
C             Common Math Library.  The error handling package is
C             general enough to be used by many libraries
C             simultaneously, so it is desirable for the routine that
C             detects and reports an error to identify the library name
C             as well as the routine name.
C
C    SUBROU   A character constant (or character variable) with the name
C             of the routine that detected the error.  Usually it is the
C             name of the routine that is calling XERMSG.  There are
C             some instances where a user callable library routine calls
C             lower level subsidiary routines where the error is
C             detected.  In such cases it may be more informative to
C             supply the name of the routine the user called rather than
C             the name of the subsidiary routine that detected the
C             error.
C
C    MESSG    A character constant (or character variable) with the text
C             of the error or warning message.  In the example below,
C             the message is a character constant that contains a
C             generic message.
C
C                   CALL XERMSG ('SLATEC', 'MMPY',
C                  *'THE ORDER OF THE MATRIX EXCEEDS THE ROW DIMENSION',
C                  *3, 1)
C
C             It is possible (and is sometimes desirable) to generate a
C             specific message--e.g., one that contains actual numeric
C             values.  Specific numeric values can be converted into
C             character strings using formatted WRITE statements into
C             character variables.  This is called standard Fortran
C             internal file I/O and is exemplified in the first three
C             lines of the following example.  You can also catenate
C             substrings of characters to construct the error message.
C             Here is an example showing the use of both writing to
C             an internal file and catenating character strings.
C
C                   CHARACTER*5 CHARN, CHARL
C                   WRITE (CHARN,10) N
C                   WRITE (CHARL,10) LDA
C                10 FORMAT(I5)
C                   CALL XERMSG ('SLATEC', 'MMPY', 'THE ORDER'//CHARN//
C                  *   ' OF THE MATRIX EXCEEDS ITS ROW DIMENSION OF'//
C                  *   CHARL, 3, 1)
C
C             There are two subtleties worth mentioning.  One is that
C             the // for character catenation is used to construct the
C             error message so that no single character constant is
C             continued to the next line.  This avoids confusion as to
C             whether there are trailing blanks at the end of the line.
C             The second is that by catenating the parts of the message
C             as an actual argument rather than encoding the entire
C             message into one large character variable, we avoid
C             having to know how long the message will be in order to
C             declare an adequate length for that large character
C             variable.  XERMSG calls XERPRN to print the message using
C             multiple lines if necessary.  If the message is very long,
C             XERPRN will break it into pieces of 72 characters (as
C             requested by XERMSG) for printing on multiple lines.
C             Also, XERMSG asks XERPRN to prefix each line with ' *  '
C             so that the total line length could be 76 characters.
C             Note also that XERPRN scans the error message backwards
C             to ignore trailing blanks.  Another feature is that
C             the substring '$$' is treated as a new line sentinel
C             by XERPRN.  If you want to construct a multiline
C             message without having to count out multiples of 72
C             characters, just use '$$' as a separator.  '$$'
C             obviously must occur within 72 characters of the
C             start of each line to have its intended effect since
C             XERPRN is asked to wrap around at 72 characters in
C             addition to looking for '$$'.
C
C    NERR     An integer value that is chosen by the library routine's
C             author.  It must be in the range -99 to 999 (three
C             printable digits).  Each distinct error should have its
C             own error number.  These error numbers should be described
C             in the machine readable documentation for the routine.
C             The error numbers need be unique only within each routine,
C             so it is reasonable for each routine to start enumerating
C             errors from 1 and proceeding to the next integer.
C
C    LEVEL    An integer value in the range 0 to 2 that indicates the
C             level (severity) of the error.  Their meanings are
C
C            -1  A warning message.  This is used if it is not clear
C                that there really is an error, but the user's attention
C                may be needed.  An attempt is made to only print this
C                message once.
C
C             0  A warning message.  This is used if it is not clear
C                that there really is an error, but the user's attention
C                may be needed.
C
C             1  A recoverable error.  This is used even if the error is
C                so serious that the routine cannot return any useful
C                answer.  If the user has told the error package to
C                return after recoverable errors, then XERMSG will
C                return to the Library routine which can then return to
C                the user's routine.  The user may also permit the error
C                package to terminate the program upon encountering a
C                recoverable error.
C
C             2  A fatal error.  XERMSG will not return to its caller
C                after it receives a fatal error.  This level should
C                hardly ever be used; it is much better to allow the
C                user a chance to recover.  An example of one of the few
C                cases in which it is permissible to declare a level 2
C                error is a reverse communication Library routine that
C                is likely to be called repeatedly until it integrates
C                across some interval.  If there is a serious error in
C                the input such that another step cannot be taken and
C                the Library routine is called again without the input
C                error having been corrected by the caller, the Library
C                routine will probably be called forever with improper
C                input.  In this case, it is reasonable to declare the
C                error to be fatal.
C
C    Each of the arguments to XERMSG is input; none will be modified by
C    XERMSG.  A routine may make multiple calls to XERMSG with warning
C    level messages; however, after a call to XERMSG with a recoverable
C    error, the routine should return to the user.  Do not try to call
C    XERMSG with a second recoverable error after the first recoverable
C    error because the error package saves the error number.  The user
C    can retrieve this error number by calling another entry point in
C    the error handling package and then clear the error number when
C    recovering from the error.  Calling XERMSG in succession causes the
C    old error number to be overwritten by the latest error number.
C    This is considered harmless for error numbers associated with
C    warning messages but must not be done for error numbers of serious
C    errors.  After a call to XERMSG with a recoverable error, the user
C    must be given a chance to call NUMXER or XERCLR to retrieve or
C    clear the error number.
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  FDUMP, J4SAVE, XERCNT, XERHLT, XERPRN, XERSVE
C***REVISION HISTORY  (YYMMDD)
C   880101  DATE WRITTEN
C   880621  REVISED AS DIRECTED AT SLATEC CML MEETING OF FEBRUARY 1988.
C           THERE ARE TWO BASIC CHANGES.
C           1.  A NEW ROUTINE, XERPRN, IS USED INSTEAD OF XERPRT TO
C               PRINT MESSAGES.  THIS ROUTINE WILL BREAK LONG MESSAGES
C               INTO PIECES FOR PRINTING ON MULTIPLE LINES.  '$$' IS
C               ACCEPTED AS A NEW LINE SENTINEL.  A PREFIX CAN BE
C               ADDED TO EACH LINE TO BE PRINTED.  XERMSG USES EITHER
C               ' ***' OR ' *  ' AND LONG MESSAGES ARE BROKEN EVERY
C               72 CHARACTERS (AT MOST) SO THAT THE MAXIMUM LINE
C               LENGTH OUTPUT CAN NOW BE AS GREAT AS 76.
C           2.  THE TEXT OF ALL MESSAGES IS NOW IN UPPER CASE SINCE THE
C               FORTRAN STANDARD DOCUMENT DOES NOT ADMIT THE EXISTENCE
C               OF LOWER CASE.
C   880708  REVISED AFTER THE SLATEC CML MEETING OF JUNE 29 AND 30.
C           THE PRINCIPAL CHANGES ARE
C           1.  CLARIFY COMMENTS IN THE PROLOGUES
C           2.  RENAME XRPRNT TO XERPRN
C           3.  REWORK HANDLING OF '$$' IN XERPRN TO HANDLE BLANK LINES
C               SIMILAR TO THE WAY FORMAT STATEMENTS HANDLE THE /
C               CHARACTER FOR NEW RECORDS.
C   890706  REVISED WITH THE HELP OF FRED FRITSCH AND REG CLEMENS TO
C           CLEAN UP THE CODING.
C   890721  REVISED TO USE NEW FEATURE IN XERPRN TO COUNT CHARACTERS IN
C           PREFIX.
C   891013  REVISED TO CORRECT COMMENTS.
C   891214  Prologue converted to Version 4.0 format.  (WRB)
C   900510  Changed test on NERR to be -9999999 < NERR < 99999999, but
C           NERR .ne. 0, and on LEVEL to be -2 < LEVEL < 3.  Added
C           LEVEL=-1 logic, changed calls to XERSAV to XERSVE, and
C           XERCTL to XERCNT.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XERMSG
      CHARACTER*(*) LIBRAR, SUBROU, MESSG
      CHARACTER*8 XLIBR, XSUBR
      CHARACTER*72  TEMP
      CHARACTER*20  LFIRST
C***FIRST EXECUTABLE STATEMENT  XERMSG
      LKNTRL = J4SAVE (2, 0, .FALSE.)
      MAXMES = J4SAVE (4, 0, .FALSE.)
C
C       LKNTRL IS A LOCAL COPY OF THE CONTROL FLAG KONTRL.
C       MAXMES IS THE MAXIMUM NUMBER OF TIMES ANY PARTICULAR MESSAGE
C          SHOULD BE PRINTED.
C
C       WE PRINT A FATAL ERROR MESSAGE AND TERMINATE FOR AN ERROR IN
C          CALLING XERMSG.  THE ERROR NUMBER SHOULD BE POSITIVE,
C          AND THE LEVEL SHOULD BE BETWEEN 0 AND 2.
C
      IF (NERR.LT.-9999999 .OR. NERR.GT.99999999 .OR. NERR.EQ.0 .OR.
     *   LEVEL.LT.-1 .OR. LEVEL.GT.2) THEN
         CALL XERPRN (' ***', -1, 'FATAL ERROR IN...$$ ' //
     *      'XERMSG -- INVALID ERROR NUMBER OR LEVEL$$ '//
     *      'JOB ABORT DUE TO FATAL ERROR.', 72)
         CALL XERSVE (' ', ' ', ' ', 0, 0, 0, KDUMMY)
         CALL XERHLT (' ***XERMSG -- INVALID INPUT')
         RETURN
      ENDIF
C
C       RECORD THE MESSAGE.
C
      I = J4SAVE (1, NERR, .TRUE.)
      CALL XERSVE (LIBRAR, SUBROU, MESSG, 1, NERR, LEVEL, KOUNT)
C
C       HANDLE PRINT-ONCE WARNING MESSAGES.
C
      IF (LEVEL.EQ.-1 .AND. KOUNT.GT.1) RETURN
C
C       ALLOW TEMPORARY USER OVERRIDE OF THE CONTROL FLAG.
C
      XLIBR  = LIBRAR
      XSUBR  = SUBROU
      LFIRST = MESSG
      LERR   = NERR
      LLEVEL = LEVEL
      CALL XERCNT (XLIBR, XSUBR, LFIRST, LERR, LLEVEL, LKNTRL)
C
      LKNTRL = MAX(-2, MIN(2,LKNTRL))
      MKNTRL = ABS(LKNTRL)
C
C       SKIP PRINTING IF THE CONTROL FLAG VALUE AS RESET IN XERCNT IS
C       ZERO AND THE ERROR IS NOT FATAL.
C
      IF (LEVEL.LT.2 .AND. LKNTRL.EQ.0) GO TO 30
      IF (LEVEL.EQ.0 .AND. KOUNT.GT.MAXMES) GO TO 30
      IF (LEVEL.EQ.1 .AND. KOUNT.GT.MAXMES .AND. MKNTRL.EQ.1) GO TO 30
      IF (LEVEL.EQ.2 .AND. KOUNT.GT.MAX(1,MAXMES)) GO TO 30
C
C       ANNOUNCE THE NAMES OF THE LIBRARY AND SUBROUTINE BY BUILDING A
C       MESSAGE IN CHARACTER VARIABLE TEMP (NOT EXCEEDING 66 CHARACTERS)
C       AND SENDING IT OUT VIA XERPRN.  PRINT ONLY IF CONTROL FLAG
C       IS NOT ZERO.
C
      IF (LKNTRL .NE. 0) THEN
         TEMP(1:21) = 'MESSAGE FROM ROUTINE '
         I = MIN(LEN(SUBROU), 16)
         TEMP(22:21+I) = SUBROU(1:I)
         TEMP(22+I:33+I) = ' IN LIBRARY '
         LTEMP = 33 + I
         I = MIN(LEN(LIBRAR), 16)
         TEMP(LTEMP+1:LTEMP+I) = LIBRAR (1:I)
         TEMP(LTEMP+I+1:LTEMP+I+1) = '.'
         LTEMP = LTEMP + I + 1
         CALL XERPRN (' ***', -1, TEMP(1:LTEMP), 72)
      ENDIF
C
C       IF LKNTRL IS POSITIVE, PRINT AN INTRODUCTORY LINE BEFORE
C       PRINTING THE MESSAGE.  THE INTRODUCTORY LINE TELLS THE CHOICE
C       FROM EACH OF THE FOLLOWING THREE OPTIONS.
C       1.  LEVEL OF THE MESSAGE
C              'INFORMATIVE MESSAGE'
C              'POTENTIALLY RECOVERABLE ERROR'
C              'FATAL ERROR'
C       2.  WHETHER CONTROL FLAG WILL ALLOW PROGRAM TO CONTINUE
C              'PROG CONTINUES'
C              'PROG ABORTED'
C       3.  WHETHER OR NOT A TRACEBACK WAS REQUESTED.  (THE TRACEBACK
C           MAY NOT BE IMPLEMENTED AT SOME SITES, SO THIS ONLY TELLS
C           WHAT WAS REQUESTED, NOT WHAT WAS DELIVERED.)
C              'TRACEBACK REQUESTED'
C              'TRACEBACK NOT REQUESTED'
C       NOTICE THAT THE LINE INCLUDING FOUR PREFIX CHARACTERS WILL NOT
C       EXCEED 74 CHARACTERS.
C       WE SKIP THE NEXT BLOCK IF THE INTRODUCTORY LINE IS NOT NEEDED.
C
      IF (LKNTRL .GT. 0) THEN
C
C       THE FIRST PART OF THE MESSAGE TELLS ABOUT THE LEVEL.
C
         IF (LEVEL .LE. 0) THEN
            TEMP(1:20) = 'INFORMATIVE MESSAGE,'
            LTEMP = 20
         ELSEIF (LEVEL .EQ. 1) THEN
            TEMP(1:30) = 'POTENTIALLY RECOVERABLE ERROR,'
            LTEMP = 30
         ELSE
            TEMP(1:12) = 'FATAL ERROR,'
            LTEMP = 12
         ENDIF
C
C       THEN WHETHER THE PROGRAM WILL CONTINUE.
C
         IF ((MKNTRL.EQ.2 .AND. LEVEL.GE.1) .OR.
     *       (MKNTRL.EQ.1 .AND. LEVEL.EQ.2)) THEN
            TEMP(LTEMP+1:LTEMP+14) = ' PROG ABORTED,'
            LTEMP = LTEMP + 14
         ELSE
            TEMP(LTEMP+1:LTEMP+16) = ' PROG CONTINUES,'
            LTEMP = LTEMP + 16
         ENDIF
C
C       FINALLY TELL WHETHER THERE SHOULD BE A TRACEBACK.
C
         IF (LKNTRL .GT. 0) THEN
            TEMP(LTEMP+1:LTEMP+20) = ' TRACEBACK REQUESTED'
            LTEMP = LTEMP + 20
         ELSE
            TEMP(LTEMP+1:LTEMP+24) = ' TRACEBACK NOT REQUESTED'
            LTEMP = LTEMP + 24
         ENDIF
         CALL XERPRN (' ***', -1, TEMP(1:LTEMP), 72)
      ENDIF
C
C       NOW SEND OUT THE MESSAGE.
C
      CALL XERPRN (' *  ', -1, MESSG, 72)
C
C       IF LKNTRL IS POSITIVE, WRITE THE ERROR NUMBER AND REQUEST A
C          TRACEBACK.
C
      IF (LKNTRL .GT. 0) THEN
         WRITE (TEMP, '(''ERROR NUMBER = '', I8)') NERR
         DO 10 I=16,22
            IF (TEMP(I:I) .NE. ' ') GO TO 20
   10    CONTINUE
C
   20    CALL XERPRN (' *  ', -1, TEMP(1:15) // TEMP(I:23), 72)
         CALL FDUMP
      ENDIF
C
C       IF LKNTRL IS NOT ZERO, PRINT A BLANK LINE AND AN END OF MESSAGE.
C
      IF (LKNTRL .NE. 0) THEN
         CALL XERPRN (' *  ', -1, ' ', 72)
         CALL XERPRN (' ***', -1, 'END OF MESSAGE', 72)
         CALL XERPRN ('    ',  0, ' ', 72)
      ENDIF
C
C       IF THE ERROR IS NOT FATAL OR THE ERROR IS RECOVERABLE AND THE
C       CONTROL FLAG IS SET FOR RECOVERY, THEN RETURN.
C
   30 IF (LEVEL.LE.0 .OR. (LEVEL.EQ.1 .AND. MKNTRL.LE.1)) RETURN
C
C       THE PROGRAM WILL BE STOPPED DUE TO AN UNRECOVERED ERROR OR A
C       FATAL ERROR.  PRINT THE REASON FOR THE ABORT AND THE ERROR
C       SUMMARY IF THE CONTROL FLAG AND THE MAXIMUM ERROR COUNT PERMIT.
C
      IF (LKNTRL.GT.0 .AND. KOUNT.LT.MAX(1,MAXMES)) THEN
         IF (LEVEL .EQ. 1) THEN
            CALL XERPRN
     *         (' ***', -1, 'JOB ABORT DUE TO UNRECOVERED ERROR.', 72)
         ELSE
            CALL XERPRN(' ***', -1, 'JOB ABORT DUE TO FATAL ERROR.', 72)
         ENDIF
         CALL XERSVE (' ', ' ', ' ', -1, 0, 0, KDUMMY)
         CALL XERHLT (' ')
      ELSE
         CALL XERHLT (MESSG)
      ENDIF
      RETURN
      END
*DECK FDUMP
      SUBROUTINE FDUMP
C***BEGIN PROLOGUE  FDUMP
C***PURPOSE  Symbolic dump (should be locally written).
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3
C***TYPE      ALL (FDUMP-A)
C***KEYWORDS  ERROR, XERMSG
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C        ***Note*** Machine Dependent Routine
C        FDUMP is intended to be replaced by a locally written
C        version which produces a symbolic dump.  Failing this,
C        it should be replaced by a version which prints the
C        subprogram nesting list.  Note that this dump must be
C        printed on each of up to five files, as indicated by the
C        XGETUA routine.  See XSETUA and XGETUA for details.
C
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C***END PROLOGUE  FDUMP
C***FIRST EXECUTABLE STATEMENT  FDUMP
      RETURN
      END
*DECK J4SAVE
      FUNCTION J4SAVE (IWHICH, IVALUE, ISET)
C***BEGIN PROLOGUE  J4SAVE
C***SUBSIDIARY
C***PURPOSE  Save or recall global variables needed by error
C            handling routines.
C***LIBRARY   SLATEC (XERROR)
C***TYPE      INTEGER (J4SAVE-I)
C***KEYWORDS  ERROR MESSAGES, ERROR NUMBER, RECALL, SAVE, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C     Abstract
C        J4SAVE saves and recalls several global variables needed
C        by the library error handling routines.
C
C     Description of Parameters
C      --Input--
C        IWHICH - Index of item desired.
C                = 1 Refers to current error number.
C                = 2 Refers to current error control flag.
C                = 3 Refers to current unit number to which error
C                    messages are to be sent.  (0 means use standard.)
C                = 4 Refers to the maximum number of times any
C                     message is to be printed (as set by XERMAX).
C                = 5 Refers to the total number of units to which
C                     each error message is to be written.
C                = 6 Refers to the 2nd unit for error messages
C                = 7 Refers to the 3rd unit for error messages
C                = 8 Refers to the 4th unit for error messages
C                = 9 Refers to the 5th unit for error messages
C        IVALUE - The value to be set for the IWHICH-th parameter,
C                 if ISET is .TRUE. .
C        ISET   - If ISET=.TRUE., the IWHICH-th parameter will BE
C                 given the value, IVALUE.  If ISET=.FALSE., the
C                 IWHICH-th parameter will be unchanged, and IVALUE
C                 is a dummy parameter.
C      --Output--
C        The (old) value of the IWHICH-th parameter will be returned
C        in the function value, J4SAVE.
C
C***SEE ALSO  XERMSG
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900205  Minor modifications to prologue.  (WRB)
C   900402  Added TYPE section.  (WRB)
C   910411  Added KEYWORDS section.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  J4SAVE
      LOGICAL ISET
      INTEGER IPARAM(9)
      SAVE IPARAM
      DATA IPARAM(1),IPARAM(2),IPARAM(3),IPARAM(4)/0,2,0,10/
      DATA IPARAM(5)/1/
      DATA IPARAM(6),IPARAM(7),IPARAM(8),IPARAM(9)/0,0,0,0/
C***FIRST EXECUTABLE STATEMENT  J4SAVE
      J4SAVE = IPARAM(IWHICH)
      IF (ISET) IPARAM(IWHICH) = IVALUE
      RETURN
      END
*DECK XERCNT
      SUBROUTINE XERCNT (LIBRAR, SUBROU, MESSG, NERR, LEVEL, KONTRL)
C***BEGIN PROLOGUE  XERCNT
C***SUBSIDIARY
C***PURPOSE  Allow user control over handling of errors.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XERCNT-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C     Abstract
C        Allows user control over handling of individual errors.
C        Just after each message is recorded, but before it is
C        processed any further (i.e., before it is printed or
C        a decision to abort is made), a call is made to XERCNT.
C        If the user has provided his own version of XERCNT, he
C        can then override the value of KONTROL used in processing
C        this message by redefining its value.
C        KONTRL may be set to any value from -2 to 2.
C        The meanings for KONTRL are the same as in XSETF, except
C        that the value of KONTRL changes only for this message.
C        If KONTRL is set to a value outside the range from -2 to 2,
C        it will be moved back into that range.
C
C     Description of Parameters
C
C      --Input--
C        LIBRAR - the library that the routine is in.
C        SUBROU - the subroutine that XERMSG is being called from
C        MESSG  - the first 20 characters of the error message.
C        NERR   - same as in the call to XERMSG.
C        LEVEL  - same as in the call to XERMSG.
C        KONTRL - the current value of the control flag as set
C                 by a call to XSETF.
C
C      --Output--
C        KONTRL - the new value of KONTRL.  If KONTRL is not
C                 defined, it will remain at its original value.
C                 This changed value of control affects only
C                 the current occurrence of the current message.
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900206  Routine changed from user-callable to subsidiary.  (WRB)
C   900510  Changed calling sequence to include LIBRARY and SUBROUTINE
C           names, changed routine name from XERCTL to XERCNT.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XERCNT
      CHARACTER*(*) LIBRAR, SUBROU, MESSG
C***FIRST EXECUTABLE STATEMENT  XERCNT
      RETURN
      END
*DECK XERHLT
      SUBROUTINE XERHLT (MESSG)
C***BEGIN PROLOGUE  XERHLT
C***SUBSIDIARY
C***PURPOSE  Abort program execution and print error message.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XERHLT-A)
C***KEYWORDS  ABORT PROGRAM EXECUTION, ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C     Abstract
C        ***Note*** machine dependent routine
C        XERHLT aborts the execution of the program.
C        The error message causing the abort is given in the calling
C        sequence, in case one needs it for printing on a dayfile,
C        for example.
C
C     Description of Parameters
C        MESSG is as in XERMSG.
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900206  Routine changed from user-callable to subsidiary.  (WRB)
C   900510  Changed calling sequence to delete length of character
C           and changed routine name from XERABT to XERHLT.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XERHLT
      CHARACTER*(*) MESSG
C***FIRST EXECUTABLE STATEMENT  XERHLT
      STOP
      END
*DECK XERPRN
      SUBROUTINE XERPRN (PREFIX, NPREF, MESSG, NWRAP)
C***BEGIN PROLOGUE  XERPRN
C***SUBSIDIARY
C***PURPOSE  Print error messages processed by XERMSG.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XERPRN-A)
C***KEYWORDS  ERROR MESSAGES, PRINTING, XERROR
C***AUTHOR  Fong, Kirby, (NMFECC at LLNL)
C***DESCRIPTION
C
C This routine sends one or more lines to each of the (up to five)
C logical units to which error messages are to be sent.  This routine
C is called several times by XERMSG, sometimes with a single line to
C print and sometimes with a (potentially very long) message that may
C wrap around into multiple lines.
C
C PREFIX  Input argument of type CHARACTER.  This argument contains
C         characters to be put at the beginning of each line before
C         the body of the message.  No more than 16 characters of
C         PREFIX will be used.
C
C NPREF   Input argument of type INTEGER.  This argument is the number
C         of characters to use from PREFIX.  If it is negative, the
C         intrinsic function LEN is used to determine its length.  If
C         it is zero, PREFIX is not used.  If it exceeds 16 or if
C         LEN(PREFIX) exceeds 16, only the first 16 characters will be
C         used.  If NPREF is positive and the length of PREFIX is less
C         than NPREF, a copy of PREFIX extended with blanks to length
C         NPREF will be used.
C
C MESSG   Input argument of type CHARACTER.  This is the text of a
C         message to be printed.  If it is a long message, it will be
C         broken into pieces for printing on multiple lines.  Each line
C         will start with the appropriate prefix and be followed by a
C         piece of the message.  NWRAP is the number of characters per
C         piece; that is, after each NWRAP characters, we break and
C         start a new line.  In addition the characters '$$' embedded
C         in MESSG are a sentinel for a new line.  The counting of
C         characters up to NWRAP starts over for each new line.  The
C         value of NWRAP typically used by XERMSG is 72 since many
C         older error messages in the SLATEC Library are laid out to
C         rely on wrap-around every 72 characters.
C
C NWRAP   Input argument of type INTEGER.  This gives the maximum size
C         piece into which to break MESSG for printing on multiple
C         lines.  An embedded '$$' ends a line, and the count restarts
C         at the following character.  If a line break does not occur
C         on a blank (it would split a word) that word is moved to the
C         next line.  Values of NWRAP less than 16 will be treated as
C         16.  Values of NWRAP greater than 132 will be treated as 132.
C         The actual line length will be NPREF + NWRAP after NPREF has
C         been adjusted to fall between 0 and 16 and NWRAP has been
C         adjusted to fall between 16 and 132.
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  I1MACH, XGETUA
C***REVISION HISTORY  (YYMMDD)
C   880621  DATE WRITTEN
C   880708  REVISED AFTER THE SLATEC CML SUBCOMMITTEE MEETING OF
C           JUNE 29 AND 30 TO CHANGE THE NAME TO XERPRN AND TO REWORK
C           THE HANDLING OF THE NEW LINE SENTINEL TO BEHAVE LIKE THE
C           SLASH CHARACTER IN FORMAT STATEMENTS.
C   890706  REVISED WITH THE HELP OF FRED FRITSCH AND REG CLEMENS TO
C           STREAMLINE THE CODING AND FIX A BUG THAT CAUSED EXTRA BLANK
C           LINES TO BE PRINTED.
C   890721  REVISED TO ADD A NEW FEATURE.  A NEGATIVE VALUE OF NPREF
C           CAUSES LEN(PREFIX) TO BE USED AS THE LENGTH.
C   891013  REVISED TO CORRECT ERROR IN CALCULATING PREFIX LENGTH.
C   891214  Prologue converted to Version 4.0 format.  (WRB)
C   900510  Added code to break messages between words.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XERPRN
      CHARACTER*(*) PREFIX, MESSG
      INTEGER NPREF, NWRAP
      CHARACTER*148 CBUFF
      INTEGER IU(5), NUNIT
      CHARACTER*2 NEWLIN
      PARAMETER (NEWLIN = '$$')
C***FIRST EXECUTABLE STATEMENT  XERPRN
      CALL XGETUA(IU,NUNIT)
C
C       A ZERO VALUE FOR A LOGICAL UNIT NUMBER MEANS TO USE THE STANDARD
C       ERROR MESSAGE UNIT INSTEAD.  I1MACH(4) RETRIEVES THE STANDARD
C       ERROR MESSAGE UNIT.
C
      N = I1MACH(4)
      DO 10 I=1,NUNIT
         IF (IU(I) .EQ. 0) IU(I) = N
   10 CONTINUE
C
C       LPREF IS THE LENGTH OF THE PREFIX.  THE PREFIX IS PLACED AT THE
C       BEGINNING OF CBUFF, THE CHARACTER BUFFER, AND KEPT THERE DURING
C       THE REST OF THIS ROUTINE.
C
      IF ( NPREF .LT. 0 ) THEN
         LPREF = LEN(PREFIX)
      ELSE
         LPREF = NPREF
      ENDIF
      LPREF = MIN(16, LPREF)
      IF (LPREF .NE. 0) CBUFF(1:LPREF) = PREFIX
C
C       LWRAP IS THE MAXIMUM NUMBER OF CHARACTERS WE WANT TO TAKE AT ONE
C       TIME FROM MESSG TO PRINT ON ONE LINE.
C
      LWRAP = MAX(16, MIN(132, NWRAP))
C
C       SET LENMSG TO THE LENGTH OF MESSG, IGNORE ANY TRAILING BLANKS.
C
      LENMSG = LEN(MESSG)
      N = LENMSG
      DO 20 I=1,N
         IF (MESSG(LENMSG:LENMSG) .NE. ' ') GO TO 30
         LENMSG = LENMSG - 1
   20 CONTINUE
   30 CONTINUE
C
C       IF THE MESSAGE IS ALL BLANKS, THEN PRINT ONE BLANK LINE.
C
      IF (LENMSG .EQ. 0) THEN
         CBUFF(LPREF+1:LPREF+1) = ' '
         DO 40 I=1,NUNIT
            WRITE(IU(I), '(A)') CBUFF(1:LPREF+1)
   40    CONTINUE
         RETURN
      ENDIF
C
C       SET NEXTC TO THE POSITION IN MESSG WHERE THE NEXT SUBSTRING
C       STARTS.  FROM THIS POSITION WE SCAN FOR THE NEW LINE SENTINEL.
C       WHEN NEXTC EXCEEDS LENMSG, THERE IS NO MORE TO PRINT.
C       WE LOOP BACK TO LABEL 50 UNTIL ALL PIECES HAVE BEEN PRINTED.
C
C       WE LOOK FOR THE NEXT OCCURRENCE OF THE NEW LINE SENTINEL.  THE
C       INDEX INTRINSIC FUNCTION RETURNS ZERO IF THERE IS NO OCCURRENCE
C       OR IF THE LENGTH OF THE FIRST ARGUMENT IS LESS THAN THE LENGTH
C       OF THE SECOND ARGUMENT.
C
C       THERE ARE SEVERAL CASES WHICH SHOULD BE CHECKED FOR IN THE
C       FOLLOWING ORDER.  WE ARE ATTEMPTING TO SET LPIECE TO THE NUMBER
C       OF CHARACTERS THAT SHOULD BE TAKEN FROM MESSG STARTING AT
C       POSITION NEXTC.
C
C       LPIECE .EQ. 0   THE NEW LINE SENTINEL DOES NOT OCCUR IN THE
C                       REMAINDER OF THE CHARACTER STRING.  LPIECE
C                       SHOULD BE SET TO LWRAP OR LENMSG+1-NEXTC,
C                       WHICHEVER IS LESS.
C
C       LPIECE .EQ. 1   THE NEW LINE SENTINEL STARTS AT MESSG(NEXTC:
C                       NEXTC).  LPIECE IS EFFECTIVELY ZERO, AND WE
C                       PRINT NOTHING TO AVOID PRODUCING UNNECESSARY
C                       BLANK LINES.  THIS TAKES CARE OF THE SITUATION
C                       WHERE THE LIBRARY ROUTINE HAS A MESSAGE OF
C                       EXACTLY 72 CHARACTERS FOLLOWED BY A NEW LINE
C                       SENTINEL FOLLOWED BY MORE CHARACTERS.  NEXTC
C                       SHOULD BE INCREMENTED BY 2.
C
C       LPIECE .GT. LWRAP+1  REDUCE LPIECE TO LWRAP.
C
C       ELSE            THIS LAST CASE MEANS 2 .LE. LPIECE .LE. LWRAP+1
C                       RESET LPIECE = LPIECE-1.  NOTE THAT THIS
C                       PROPERLY HANDLES THE END CASE WHERE LPIECE .EQ.
C                       LWRAP+1.  THAT IS, THE SENTINEL FALLS EXACTLY
C                       AT THE END OF A LINE.
C
      NEXTC = 1
   50 LPIECE = INDEX(MESSG(NEXTC:LENMSG), NEWLIN)
      IF (LPIECE .EQ. 0) THEN
C
C       THERE WAS NO NEW LINE SENTINEL FOUND.
C
         IDELTA = 0
         LPIECE = MIN(LWRAP, LENMSG+1-NEXTC)
         IF (LPIECE .LT. LENMSG+1-NEXTC) THEN
            DO 52 I=LPIECE+1,2,-1
               IF (MESSG(NEXTC+I-1:NEXTC+I-1) .EQ. ' ') THEN
                  LPIECE = I-1
                  IDELTA = 1
                  GOTO 54
               ENDIF
   52       CONTINUE
         ENDIF
   54    CBUFF(LPREF+1:LPREF+LPIECE) = MESSG(NEXTC:NEXTC+LPIECE-1)
         NEXTC = NEXTC + LPIECE + IDELTA
      ELSEIF (LPIECE .EQ. 1) THEN
C
C       WE HAVE A NEW LINE SENTINEL AT MESSG(NEXTC:NEXTC+1).
C       DON'T PRINT A BLANK LINE.
C
         NEXTC = NEXTC + 2
         GO TO 50
      ELSEIF (LPIECE .GT. LWRAP+1) THEN
C
C       LPIECE SHOULD BE SET DOWN TO LWRAP.
C
         IDELTA = 0
         LPIECE = LWRAP
         DO 56 I=LPIECE+1,2,-1
            IF (MESSG(NEXTC+I-1:NEXTC+I-1) .EQ. ' ') THEN
               LPIECE = I-1
               IDELTA = 1
               GOTO 58
            ENDIF
   56    CONTINUE
   58    CBUFF(LPREF+1:LPREF+LPIECE) = MESSG(NEXTC:NEXTC+LPIECE-1)
         NEXTC = NEXTC + LPIECE + IDELTA
      ELSE
C
C       IF WE ARRIVE HERE, IT MEANS 2 .LE. LPIECE .LE. LWRAP+1.
C       WE SHOULD DECREMENT LPIECE BY ONE.
C
         LPIECE = LPIECE - 1
         CBUFF(LPREF+1:LPREF+LPIECE) = MESSG(NEXTC:NEXTC+LPIECE-1)
         NEXTC  = NEXTC + LPIECE + 2
      ENDIF
C
C       PRINT
C
      DO 60 I=1,NUNIT
         WRITE(IU(I), '(A)') CBUFF(1:LPREF+LPIECE)
   60 CONTINUE
C
      IF (NEXTC .LE. LENMSG) GO TO 50
      RETURN
      END
*DECK XERSVE
      SUBROUTINE XERSVE (LIBRAR, SUBROU, MESSG, KFLAG, NERR, LEVEL,
     +   ICOUNT)
C***BEGIN PROLOGUE  XERSVE
C***SUBSIDIARY
C***PURPOSE  Record that an error has occurred.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3
C***TYPE      ALL (XERSVE-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C *Usage:
C
C        INTEGER  KFLAG, NERR, LEVEL, ICOUNT
C        CHARACTER * (len) LIBRAR, SUBROU, MESSG
C
C        CALL XERSVE (LIBRAR, SUBROU, MESSG, KFLAG, NERR, LEVEL, ICOUNT)
C
C *Arguments:
C
C        LIBRAR :IN    is the library that the message is from.
C        SUBROU :IN    is the subroutine that the message is from.
C        MESSG  :IN    is the message to be saved.
C        KFLAG  :IN    indicates the action to be performed.
C                      when KFLAG > 0, the message in MESSG is saved.
C                      when KFLAG=0 the tables will be dumped and
C                      cleared.
C                      when KFLAG < 0, the tables will be dumped and
C                      not cleared.
C        NERR   :IN    is the error number.
C        LEVEL  :IN    is the error severity.
C        ICOUNT :OUT   the number of times this message has been seen,
C                      or zero if the table has overflowed and does not
C                      contain this message specifically.  When KFLAG=0,
C                      ICOUNT will not be altered.
C
C *Description:
C
C   Record that this error occurred and possibly dump and clear the
C   tables.
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  I1MACH, XGETUA
C***REVISION HISTORY  (YYMMDD)
C   800319  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900413  Routine modified to remove reference to KFLAG.  (WRB)
C   900510  Changed to add LIBRARY NAME and SUBROUTINE to calling
C           sequence, use IF-THEN-ELSE, make number of saved entries
C           easily changeable, changed routine name from XERSAV to
C           XERSVE.  (RWC)
C   910626  Added LIBTAB and SUBTAB to SAVE statement.  (BKS)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XERSVE
      PARAMETER (LENTAB=10)
      INTEGER LUN(5)
      CHARACTER*(*) LIBRAR, SUBROU, MESSG
      CHARACTER*8  LIBTAB(LENTAB), SUBTAB(LENTAB), LIB, SUB
      CHARACTER*20 MESTAB(LENTAB), MES
      DIMENSION NERTAB(LENTAB), LEVTAB(LENTAB), KOUNT(LENTAB)
      SAVE LIBTAB, SUBTAB, MESTAB, NERTAB, LEVTAB, KOUNT, KOUNTX, NMSG
      DATA KOUNTX/0/, NMSG/0/
C***FIRST EXECUTABLE STATEMENT  XERSVE
C
      IF (KFLAG.LE.0) THEN
C
C        Dump the table.
C
         IF (NMSG.EQ.0) RETURN
C
C        Print to each unit.
C
         CALL XGETUA (LUN, NUNIT)
         DO 20 KUNIT = 1,NUNIT
            IUNIT = LUN(KUNIT)
            IF (IUNIT.EQ.0) IUNIT = I1MACH(4)
C
C           Print the table header.
C
            WRITE (IUNIT,9000)
C
C           Print body of table.
C
            DO 10 I = 1,NMSG
               WRITE (IUNIT,9010) LIBTAB(I), SUBTAB(I), MESTAB(I),
     *            NERTAB(I),LEVTAB(I),KOUNT(I)
   10       CONTINUE
C
C           Print number of other errors.
C
            IF (KOUNTX.NE.0) WRITE (IUNIT,9020) KOUNTX
            WRITE (IUNIT,9030)
   20    CONTINUE
C
C        Clear the error tables.
C
         IF (KFLAG.EQ.0) THEN
            NMSG = 0
            KOUNTX = 0
         ENDIF
      ELSE
C
C        PROCESS A MESSAGE...
C        SEARCH FOR THIS MESSG, OR ELSE AN EMPTY SLOT FOR THIS MESSG,
C        OR ELSE DETERMINE THAT THE ERROR TABLE IS FULL.
C
         LIB = LIBRAR
         SUB = SUBROU
         MES = MESSG
         DO 30 I = 1,NMSG
            IF (LIB.EQ.LIBTAB(I) .AND. SUB.EQ.SUBTAB(I) .AND.
     *         MES.EQ.MESTAB(I) .AND. NERR.EQ.NERTAB(I) .AND.
     *         LEVEL.EQ.LEVTAB(I)) THEN
                  KOUNT(I) = KOUNT(I) + 1
                  ICOUNT = KOUNT(I)
                  RETURN
            ENDIF
   30    CONTINUE
C
         IF (NMSG.LT.LENTAB) THEN
C
C           Empty slot found for new message.
C
            NMSG = NMSG + 1
            LIBTAB(I) = LIB
            SUBTAB(I) = SUB
            MESTAB(I) = MES
            NERTAB(I) = NERR
            LEVTAB(I) = LEVEL
            KOUNT (I) = 1
            ICOUNT    = 1
         ELSE
C
C           Table is full.
C
            KOUNTX = KOUNTX+1
            ICOUNT = 0
         ENDIF
      ENDIF
      RETURN
C
C     Formats.
C
 9000 FORMAT ('0          ERROR MESSAGE SUMMARY' /
     +   ' LIBRARY    SUBROUTINE MESSAGE START             NERR',
     +   '     LEVEL     COUNT')
 9010 FORMAT (1X,A,3X,A,3X,A,3I10)
 9020 FORMAT ('0OTHER ERRORS NOT INDIVIDUALLY TABULATED = ', I10)
 9030 FORMAT (1X)
      END
*DECK XGETUA
      SUBROUTINE XGETUA (IUNITA, N)
C***BEGIN PROLOGUE  XGETUA
C***PURPOSE  Return unit number(s) to which error messages are being
C            sent.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XGETUA-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C     Abstract
C        XGETUA may be called to determine the unit number or numbers
C        to which error messages are being sent.
C        These unit numbers may have been set by a call to XSETUN,
C        or a call to XSETUA, or may be a default value.
C
C     Description of Parameters
C      --Output--
C        IUNIT - an array of one to five unit numbers, depending
C                on the value of N.  A value of zero refers to the
C                default unit, as defined by the I1MACH machine
C                constant routine.  Only IUNIT(1),...,IUNIT(N) are
C                defined by XGETUA.  The values of IUNIT(N+1),...,
C                IUNIT(5) are not defined (for N .LT. 5) or altered
C                in any way by XGETUA.
C        N     - the number of units to which copies of the
C                error messages are being sent.  N will be in the
C                range from 1 to 5.
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  J4SAVE
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XGETUA
      DIMENSION IUNITA(5)
C***FIRST EXECUTABLE STATEMENT  XGETUA
      N = J4SAVE(5,0,.FALSE.)
      DO 30 I=1,N
         INDEX = I+4
         IF (I.EQ.1) INDEX = 3
         IUNITA(I) = J4SAVE(INDEX,0,.FALSE.)
   30 CONTINUE
      RETURN
      END

*DECK DBESK0
      DOUBLE PRECISION FUNCTION DBESK0 (X)
C***BEGIN PROLOGUE  DBESK0
C***PURPOSE  Compute the modified (hyperbolic) Bessel function of the
C            third kind of order zero.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESK0-S, DBESK0-D)
C***KEYWORDS  FNLIB, HYPERBOLIC BESSEL FUNCTION,
C             MODIFIED BESSEL FUNCTION, ORDER ZERO, SPECIAL FUNCTIONS,
C             THIRD KIND
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBESK0(X) calculates the double precision modified (hyperbolic)
C Bessel function of the third kind of order zero for double
C precision argument X.  The argument must be greater than zero
C but not so large that the result underflows.
C
C Series for BK0        on the interval  0.          to  4.00000E+00
C                                        with weighted error   3.08E-33
C                                         log weighted error  32.51
C                               significant figures required  32.05
C                                    decimal places required  33.11
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DBESI0, DBSK0E, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DBESK0
      DOUBLE PRECISION X, BK0CS(16), XMAX, XMAXT, XSML, Y,
     1  D1MACH, DCSEVL, DBESI0, DBSK0E
      LOGICAL FIRST
      SAVE BK0CS, NTK0, XSML, XMAX, FIRST
      DATA BK0CS(  1) / -.3532739323 3902768720 1140060063 153 D-1    /
      DATA BK0CS(  2) / +.3442898999 2462848688 6344927529 213 D+0    /
      DATA BK0CS(  3) / +.3597993651 5361501626 5721303687 231 D-1    /
      DATA BK0CS(  4) / +.1264615411 4469259233 8479508673 447 D-2    /
      DATA BK0CS(  5) / +.2286212103 1194517860 8269830297 585 D-4    /
      DATA BK0CS(  6) / +.2534791079 0261494573 0790013428 354 D-6    /
      DATA BK0CS(  7) / +.1904516377 2202088589 7214059381 366 D-8    /
      DATA BK0CS(  8) / +.1034969525 7633624585 1008317853 089 D-10   /
      DATA BK0CS(  9) / +.4259816142 7910825765 2445327170 133 D-13   /
      DATA BK0CS( 10) / +.1374465435 8807508969 4238325440 000 D-15   /
      DATA BK0CS( 11) / +.3570896528 5083735909 9688597333 333 D-18   /
      DATA BK0CS( 12) / +.7631643660 1164373766 7498666666 666 D-21   /
      DATA BK0CS( 13) / +.1365424988 4407818590 8053333333 333 D-23   /
      DATA BK0CS( 14) / +.2075275266 9066680831 9999999999 999 D-26   /
      DATA BK0CS( 15) / +.2712814218 0729856000 0000000000 000 D-29   /
      DATA BK0CS( 16) / +.3082593887 9146666666 6666666666 666 D-32   /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBESK0
      IF (FIRST) THEN
         NTK0 = INITDS (BK0CS, 16, 0.1*REAL(D1MACH(3)))
         XSML = SQRT(4.0D0*D1MACH(3))
         XMAXT = -LOG(D1MACH(1))
         XMAX = XMAXT - 0.5D0*XMAXT*LOG(XMAXT)/(XMAXT+0.5D0)
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LE. 0.D0) CALL XERMSG ('SLATEC', 'DBESK0',
     +   'X IS ZERO OR NEGATIVE', 2, 2)
      IF (X.GT.2.0D0) GO TO 20
C
      Y = 0.D0
      IF (X.GT.XSML) Y = X*X
      DBESK0 = -LOG(0.5D0*X)*DBESI0(X) - 0.25D0 + DCSEVL (.5D0*Y-1.D0,
     1  BK0CS, NTK0)
      RETURN
C
 20   DBESK0 = 0.D0
      IF (X .GT. XMAX) CALL XERMSG ('SLATEC', 'DBESK0',
     +   'X SO BIG K0 UNDERFLOWS', 1, 1)
      IF (X.GT.XMAX) RETURN
C
      DBESK0 = EXP(-X) * DBSK0E(X)
C
      RETURN
      END
*DECK DBESK1
      DOUBLE PRECISION FUNCTION DBESK1 (X)
C***BEGIN PROLOGUE  DBESK1
C***PURPOSE  Compute the modified (hyperbolic) Bessel function of the
C            third kind of order one.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESK1-S, DBESK1-D)
C***KEYWORDS  FNLIB, HYPERBOLIC BESSEL FUNCTION,
C             MODIFIED BESSEL FUNCTION, ORDER ONE, SPECIAL FUNCTIONS,
C             THIRD KIND
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBESK1(X) calculates the double precision modified (hyperbolic)
C Bessel function of the third kind of order one for double precision
C argument X.  The argument must be large enough that the result does
C not overflow and small enough that the result does not underflow.
C
C Series for BK1        on the interval  0.          to  4.00000E+00
C                                        with weighted error   9.16E-32
C                                         log weighted error  31.04
C                               significant figures required  30.61
C                                    decimal places required  31.64
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DBESI1, DBSK1E, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DBESK1
      DOUBLE PRECISION X, BK1CS(16), XMAX, XMAXT, XMIN, XSML, Y,
     1  D1MACH, DCSEVL, DBESI1, DBSK1E
      LOGICAL FIRST
      SAVE BK1CS, NTK1, XMIN, XSML, XMAX, FIRST
      DATA BK1CS(  1) / +.2530022733 8947770532 5311208685 33 D-1     /
      DATA BK1CS(  2) / -.3531559607 7654487566 7238316918 01 D+0     /
      DATA BK1CS(  3) / -.1226111808 2265714823 4790679300 42 D+0     /
      DATA BK1CS(  4) / -.6975723859 6398643501 8129202960 83 D-2     /
      DATA BK1CS(  5) / -.1730288957 5130520630 1765073689 79 D-3     /
      DATA BK1CS(  6) / -.2433406141 5659682349 6007350301 64 D-5     /
      DATA BK1CS(  7) / -.2213387630 7347258558 3152525451 26 D-7     /
      DATA BK1CS(  8) / -.1411488392 6335277610 9583302126 08 D-9     /
      DATA BK1CS(  9) / -.6666901694 1993290060 8537512643 73 D-12    /
      DATA BK1CS( 10) / -.2427449850 5193659339 2631968648 53 D-14    /
      DATA BK1CS( 11) / -.7023863479 3862875971 7837971200 00 D-17    /
      DATA BK1CS( 12) / -.1654327515 5100994675 4910293333 33 D-19    /
      DATA BK1CS( 13) / -.3233834745 9944491991 8933333333 33 D-22    /
      DATA BK1CS( 14) / -.5331275052 9265274999 4666666666 66 D-25    /
      DATA BK1CS( 15) / -.7513040716 2157226666 6666666666 66 D-28    /
      DATA BK1CS( 16) / -.9155085717 6541866666 6666666666 66 D-31    /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBESK1
      IF (FIRST) THEN
         NTK1 = INITDS (BK1CS, 16, 0.1*REAL(D1MACH(3)))
         XMIN = EXP(MAX(LOG(D1MACH(1)), -LOG(D1MACH(2))) + 0.01D0)
         XSML = SQRT(4.0D0*D1MACH(3))
         XMAXT = -LOG(D1MACH(1))
         XMAX = XMAXT - 0.5D0*XMAXT*LOG(XMAXT)/(XMAXT+0.5D0)
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LE. 0.D0) CALL XERMSG ('SLATEC', 'DBESK1',
     +   'X IS ZERO OR NEGATIVE', 2, 2)
      IF (X.GT.2.0D0) GO TO 20
C
      IF (X .LT. XMIN) CALL XERMSG ('SLATEC', 'DBESK1',
     +   'X SO SMALL K1 OVERFLOWS', 3, 2)
      Y = 0.D0
      IF (X.GT.XSML) Y = X*X
      DBESK1 = LOG(0.5D0*X)*DBESI1(X) + (0.75D0 + DCSEVL (.5D0*Y-1.D0,
     1  BK1CS, NTK1))/X
      RETURN
C
 20   DBESK1 = 0.D0
      IF (X .GT. XMAX) CALL XERMSG ('SLATEC', 'DBESK1',
     +   'X SO BIG K1 UNDERFLOWS', 1, 1)
      IF (X.GT.XMAX) RETURN
C
      DBESK1 = EXP(-X) * DBSK1E(X)
C
      RETURN
      END
*DECK DGAMMA
      DOUBLE PRECISION FUNCTION DGAMMA (X)
C***BEGIN PROLOGUE  DGAMMA
C***PURPOSE  Compute the complete Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7A
C***TYPE      DOUBLE PRECISION (GAMMA-S, DGAMMA-D, CGAMMA-C)
C***KEYWORDS  COMPLETE GAMMA FUNCTION, FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DGAMMA(X) calculates the double precision complete Gamma function
C for double precision argument X.
C
C Series for GAM        on the interval  0.          to  1.00000E+00
C                                        with weighted error   5.79E-32
C                                         log weighted error  31.24
C                               significant figures required  30.00
C                                    decimal places required  32.05
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, D9LGMC, DCSEVL, DGAMLM, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920618  Removed space from variable name.  (RWC, WRB)
C***END PROLOGUE  DGAMMA
      DOUBLE PRECISION X, GAMCS(42), DXREL, PI, SINPIY, SQ2PIL, XMAX,
     1  XMIN, Y, D9LGMC, DCSEVL, D1MACH
      LOGICAL FIRST
C
      SAVE GAMCS, PI, SQ2PIL, NGAM, XMIN, XMAX, DXREL, FIRST
      DATA GAMCS(  1) / +.8571195590 9893314219 2006239994 2 D-2      /
      DATA GAMCS(  2) / +.4415381324 8410067571 9131577165 2 D-2      /
      DATA GAMCS(  3) / +.5685043681 5993633786 3266458878 9 D-1      /
      DATA GAMCS(  4) / -.4219835396 4185605010 1250018662 4 D-2      /
      DATA GAMCS(  5) / +.1326808181 2124602205 8400679635 2 D-2      /
      DATA GAMCS(  6) / -.1893024529 7988804325 2394702388 6 D-3      /
      DATA GAMCS(  7) / +.3606925327 4412452565 7808221722 5 D-4      /
      DATA GAMCS(  8) / -.6056761904 4608642184 8554829036 5 D-5      /
      DATA GAMCS(  9) / +.1055829546 3022833447 3182350909 3 D-5      /
      DATA GAMCS( 10) / -.1811967365 5423840482 9185589116 6 D-6      /
      DATA GAMCS( 11) / +.3117724964 7153222777 9025459316 9 D-7      /
      DATA GAMCS( 12) / -.5354219639 0196871408 7408102434 7 D-8      /
      DATA GAMCS( 13) / +.9193275519 8595889468 8778682594 0 D-9      /
      DATA GAMCS( 14) / -.1577941280 2883397617 6742327395 3 D-9      /
      DATA GAMCS( 15) / +.2707980622 9349545432 6654043308 9 D-10     /
      DATA GAMCS( 16) / -.4646818653 8257301440 8166105893 3 D-11     /
      DATA GAMCS( 17) / +.7973350192 0074196564 6076717535 9 D-12     /
      DATA GAMCS( 18) / -.1368078209 8309160257 9949917230 9 D-12     /
      DATA GAMCS( 19) / +.2347319486 5638006572 3347177168 8 D-13     /
      DATA GAMCS( 20) / -.4027432614 9490669327 6657053469 9 D-14     /
      DATA GAMCS( 21) / +.6910051747 3721009121 3833697525 7 D-15     /
      DATA GAMCS( 22) / -.1185584500 2219929070 5238712619 2 D-15     /
      DATA GAMCS( 23) / +.2034148542 4963739552 0102605193 2 D-16     /
      DATA GAMCS( 24) / -.3490054341 7174058492 7401294910 8 D-17     /
      DATA GAMCS( 25) / +.5987993856 4853055671 3505106602 6 D-18     /
      DATA GAMCS( 26) / -.1027378057 8722280744 9006977843 1 D-18     /
      DATA GAMCS( 27) / +.1762702816 0605298249 4275966074 8 D-19     /
      DATA GAMCS( 28) / -.3024320653 7353062609 5877211204 2 D-20     /
      DATA GAMCS( 29) / +.5188914660 2183978397 1783355050 6 D-21     /
      DATA GAMCS( 30) / -.8902770842 4565766924 4925160106 6 D-22     /
      DATA GAMCS( 31) / +.1527474068 4933426022 7459689130 6 D-22     /
      DATA GAMCS( 32) / -.2620731256 1873629002 5732833279 9 D-23     /
      DATA GAMCS( 33) / +.4496464047 8305386703 3104657066 6 D-24     /
      DATA GAMCS( 34) / -.7714712731 3368779117 0390152533 3 D-25     /
      DATA GAMCS( 35) / +.1323635453 1260440364 8657271466 6 D-25     /
      DATA GAMCS( 36) / -.2270999412 9429288167 0231381333 3 D-26     /
      DATA GAMCS( 37) / +.3896418998 0039914493 2081663999 9 D-27     /
      DATA GAMCS( 38) / -.6685198115 1259533277 9212799999 9 D-28     /
      DATA GAMCS( 39) / +.1146998663 1400243843 4761386666 6 D-28     /
      DATA GAMCS( 40) / -.1967938586 3451346772 9510399999 9 D-29     /
      DATA GAMCS( 41) / +.3376448816 5853380903 3489066666 6 D-30     /
      DATA GAMCS( 42) / -.5793070335 7821357846 2549333333 3 D-31     /
      DATA PI / 3.1415926535 8979323846 2643383279 50 D0 /
      DATA SQ2PIL / 0.9189385332 0467274178 0329736405 62 D0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DGAMMA
      IF (FIRST) THEN
         NGAM = INITDS (GAMCS, 42, 0.1*REAL(D1MACH(3)) )
C
         CALL DGAMLM (XMIN, XMAX)
         DXREL = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y.GT.10.D0) GO TO 50
C
C COMPUTE GAMMA(X) FOR -XBND .LE. X .LE. XBND.  REDUCE INTERVAL AND FIND
C GAMMA(1+Y) FOR 0.0 .LE. Y .LT. 1.0 FIRST OF ALL.
C
      N = X
      IF (X.LT.0.D0) N = N - 1
      Y = X - N
      N = N - 1
      DGAMMA = 0.9375D0 + DCSEVL (2.D0*Y-1.D0, GAMCS, NGAM)
      IF (N.EQ.0) RETURN
C
      IF (N.GT.0) GO TO 30
C
C COMPUTE GAMMA(X) FOR X .LT. 1.0
C
      N = -N
      IF (X .EQ. 0.D0) CALL XERMSG ('SLATEC', 'DGAMMA', 'X IS 0', 4, 2)
      IF (X .LT. 0.0 .AND. X+N-2 .EQ. 0.D0) CALL XERMSG ('SLATEC',
     +   'DGAMMA', 'X IS A NEGATIVE INTEGER', 4, 2)
      IF (X .LT. (-0.5D0) .AND. ABS((X-AINT(X-0.5D0))/X) .LT. DXREL)
     +   CALL XERMSG ('SLATEC', 'DGAMMA',
     +   'ANSWER LT HALF PRECISION BECAUSE X TOO NEAR NEGATIVE INTEGER',
     +   1, 1)
C
      DO 20 I=1,N
        DGAMMA = DGAMMA/(X+I-1 )
 20   CONTINUE
      RETURN
C
C GAMMA(X) FOR X .GE. 2.0 AND X .LE. 10.0
C
 30   DO 40 I=1,N
        DGAMMA = (Y+I) * DGAMMA
 40   CONTINUE
      RETURN
C
C GAMMA(X) FOR ABS(X) .GT. 10.0.  RECALL Y = ABS(X).
C
 50   IF (X .GT. XMAX) CALL XERMSG ('SLATEC', 'DGAMMA',
     +   'X SO BIG GAMMA OVERFLOWS', 3, 2)
C
      DGAMMA = 0.D0
      IF (X .LT. XMIN) CALL XERMSG ('SLATEC', 'DGAMMA',
     +   'X SO SMALL GAMMA UNDERFLOWS', 2, 1)
      IF (X.LT.XMIN) RETURN
C
      DGAMMA = EXP ((Y-0.5D0)*LOG(Y) - Y + SQ2PIL + D9LGMC(Y) )
      IF (X.GT.0.D0) RETURN
C
      IF (ABS((X-AINT(X-0.5D0))/X) .LT. DXREL) CALL XERMSG ('SLATEC',
     +   'DGAMMA',
     +   'ANSWER LT HALF PRECISION, X TOO NEAR NEGATIVE INTEGER', 1, 1)
C
      SINPIY = SIN (PI*Y)
      IF (SINPIY .EQ. 0.D0) CALL XERMSG ('SLATEC', 'DGAMMA',
     +   'X IS A NEGATIVE INTEGER', 4, 2)
C
      DGAMMA = -PI/(Y*SINPIY*DGAMMA)
C
      RETURN
      END
*DECK DBESI0
      DOUBLE PRECISION FUNCTION DBESI0 (X)
C***BEGIN PROLOGUE  DBESI0
C***PURPOSE  Compute the hyperbolic Bessel function of the first kind
C            of order zero.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESI0-S, DBESI0-D)
C***KEYWORDS  FIRST KIND, FNLIB, HYPERBOLIC BESSEL FUNCTION,
C             MODIFIED BESSEL FUNCTION, ORDER ZERO, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBESI0(X) calculates the double precision modified (hyperbolic)
C Bessel function of the first kind of order zero and double
C precision argument X.
C
C Series for BI0        on the interval  0.          to  9.00000E+00
C                                        with weighted error   9.51E-34
C                                         log weighted error  33.02
C                               significant figures required  33.31
C                                    decimal places required  33.65
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DBSI0E, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DBESI0
      DOUBLE PRECISION X, BI0CS(18), XMAX, XSML, Y, D1MACH,
     1  DCSEVL, DBSI0E
      LOGICAL FIRST
      SAVE BI0CS, NTI0, XSML, XMAX, FIRST
      DATA BI0CS(  1) / -.7660547252 8391449510 8189497624 3285 D-1   /
      DATA BI0CS(  2) / +.1927337953 9938082699 5240875088 1196 D+1   /
      DATA BI0CS(  3) / +.2282644586 9203013389 3702929233 0415 D+0   /
      DATA BI0CS(  4) / +.1304891466 7072904280 7933421069 1888 D-1   /
      DATA BI0CS(  5) / +.4344270900 8164874513 7868268102 6107 D-3   /
      DATA BI0CS(  6) / +.9422657686 0019346639 2317174411 8766 D-5   /
      DATA BI0CS(  7) / +.1434006289 5106910799 6209187817 9957 D-6   /
      DATA BI0CS(  8) / +.1613849069 6617490699 1541971999 4611 D-8   /
      DATA BI0CS(  9) / +.1396650044 5356696994 9509270814 2522 D-10  /
      DATA BI0CS( 10) / +.9579451725 5054453446 2752317189 3333 D-13  /
      DATA BI0CS( 11) / +.5333981859 8625021310 1510774400 0000 D-15  /
      DATA BI0CS( 12) / +.2458716088 4374707746 9678591999 9999 D-17  /
      DATA BI0CS( 13) / +.9535680890 2487700269 4434133333 3333 D-20  /
      DATA BI0CS( 14) / +.3154382039 7214273367 8933333333 3333 D-22  /
      DATA BI0CS( 15) / +.9004564101 0946374314 6666666666 6666 D-25  /
      DATA BI0CS( 16) / +.2240647369 1236700160 0000000000 0000 D-27  /
      DATA BI0CS( 17) / +.4903034603 2428373333 3333333333 3333 D-30  /
      DATA BI0CS( 18) / +.9508172606 1226666666 6666666666 6666 D-33  /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBESI0
      IF (FIRST) THEN
         NTI0 = INITDS (BI0CS, 18, 0.1*REAL(D1MACH(3)))
         XSML = SQRT(4.5D0*D1MACH(3))
         XMAX = LOG (D1MACH(2))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y.GT.3.0D0) GO TO 20
C
      DBESI0 = 1.0D0
      IF (Y.GT.XSML) DBESI0 = 2.75D0 + DCSEVL (Y*Y/4.5D0-1.D0, BI0CS,
     1  NTI0)
      RETURN
C
 20   IF (Y .GT. XMAX) CALL XERMSG ('SLATEC', 'DBESI0',
     +   'ABS(X) SO BIG I0 OVERFLOWS', 2, 2)
C
      DBESI0 = EXP(Y) * DBSI0E(X)
C
      RETURN
      END
*DECK DBESI1
      DOUBLE PRECISION FUNCTION DBESI1 (X)
C***BEGIN PROLOGUE  DBESI1
C***PURPOSE  Compute the modified (hyperbolic) Bessel function of the
C            first kind of order one.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESI1-S, DBESI1-D)
C***KEYWORDS  FIRST KIND, FNLIB, HYPERBOLIC BESSEL FUNCTION,
C             MODIFIED BESSEL FUNCTION, ORDER ONE, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBESI1(X) calculates the double precision modified (hyperbolic)
C Bessel function of the first kind of order one and double precision
C argument X.
C
C Series for BI1        on the interval  0.          to  9.00000E+00
C                                        with weighted error   1.44E-32
C                                         log weighted error  31.84
C                               significant figures required  31.45
C                                    decimal places required  32.46
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DBSI1E, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DBESI1
      DOUBLE PRECISION X, BI1CS(17), XMAX, XMIN, XSML, Y, D1MACH,
     1  DCSEVL, DBSI1E
      LOGICAL FIRST
      SAVE BI1CS, NTI1, XMIN, XSML, XMAX, FIRST
      DATA BI1CS(  1) / -.1971713261 0998597316 1385032181 49 D-2     /
      DATA BI1CS(  2) / +.4073488766 7546480608 1553936520 14 D+0     /
      DATA BI1CS(  3) / +.3483899429 9959455866 2450377837 87 D-1     /
      DATA BI1CS(  4) / +.1545394556 3001236038 5984010584 89 D-2     /
      DATA BI1CS(  5) / +.4188852109 8377784129 4588320041 20 D-4     /
      DATA BI1CS(  6) / +.7649026764 8362114741 9597039660 69 D-6     /
      DATA BI1CS(  7) / +.1004249392 4741178689 1798080372 38 D-7     /
      DATA BI1CS(  8) / +.9932207791 9238106481 3712980548 63 D-10    /
      DATA BI1CS(  9) / +.7663801791 8447637275 2001716813 49 D-12    /
      DATA BI1CS( 10) / +.4741418923 8167394980 3880919481 60 D-14    /
      DATA BI1CS( 11) / +.2404114404 0745181799 8631720320 00 D-16    /
      DATA BI1CS( 12) / +.1017150500 7093713649 1211007999 99 D-18    /
      DATA BI1CS( 13) / +.3645093565 7866949458 4917333333 33 D-21    /
      DATA BI1CS( 14) / +.1120574950 2562039344 8106666666 66 D-23    /
      DATA BI1CS( 15) / +.2987544193 4468088832 0000000000 00 D-26    /
      DATA BI1CS( 16) / +.6973231093 9194709333 3333333333 33 D-29    /
      DATA BI1CS( 17) / +.1436794822 0620800000 0000000000 00 D-31    /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBESI1
      IF (FIRST) THEN
         NTI1 = INITDS (BI1CS, 17, 0.1*REAL(D1MACH(3)))
         XMIN = 2.0D0*D1MACH(1)
         XSML = SQRT(4.5D0*D1MACH(3))
         XMAX = LOG (D1MACH(2))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y.GT.3.0D0) GO TO 20
C
      DBESI1 = 0.D0
      IF (Y.EQ.0.D0)  RETURN
C
      IF (Y .LE. XMIN) CALL XERMSG ('SLATEC', 'DBESI1',
     +   'ABS(X) SO SMALL I1 UNDERFLOWS', 1, 1)
      IF (Y.GT.XMIN) DBESI1 = 0.5D0*X
      IF (Y.GT.XSML) DBESI1 = X*(0.875D0 + DCSEVL (Y*Y/4.5D0-1.D0,
     1  BI1CS, NTI1))
      RETURN
C
 20   IF (Y .GT. XMAX) CALL XERMSG ('SLATEC', 'DBESI1',
     +   'ABS(X) SO BIG I1 OVERFLOWS', 2, 2)
C
      DBESI1 = EXP(Y) * DBSI1E(X)
C
      RETURN
      END
*DECK INITDS
      FUNCTION INITDS (OS, NOS, ETA)
C***BEGIN PROLOGUE  INITDS
C***PURPOSE  Determine the number of terms needed in an orthogonal
C            polynomial series so that it meets a specified accuracy.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C3A2
C***TYPE      DOUBLE PRECISION (INITS-S, INITDS-D)
C***KEYWORDS  CHEBYSHEV, FNLIB, INITIALIZE, ORTHOGONAL POLYNOMIAL,
C             ORTHOGONAL SERIES, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C  Initialize the orthogonal series, represented by the array OS, so
C  that INITDS is the number of terms needed to insure the error is no
C  larger than ETA.  Ordinarily, ETA will be chosen to be one-tenth
C  machine precision.
C
C             Input Arguments --
C   OS     double precision array of NOS coefficients in an orthogonal
C          series.
C   NOS    number of coefficients in OS.
C   ETA    single precision scalar containing requested accuracy of
C          series.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891115  Modified error message.  (WRB)
C   891115  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  INITDS
      DOUBLE PRECISION OS(*)
C***FIRST EXECUTABLE STATEMENT  INITDS
      IF (NOS .LT. 1) CALL XERMSG ('SLATEC', 'INITDS',
     +   'Number of coefficients is less than 1', 2, 1)
C
      ERR = 0.
      DO 10 II = 1,NOS
        I = NOS + 1 - II
        ERR = ERR + ABS(REAL(OS(I)))
        IF (ERR.GT.ETA) GO TO 20
   10 CONTINUE
C
   20 IF (I .EQ. NOS) CALL XERMSG ('SLATEC', 'INITDS',
     +   'Chebyshev series too short for specified accuracy', 1, 1)
      INITDS = I
C
      RETURN
      END
*DECK INITS
      FUNCTION INITS (OS, NOS, ETA)
C***BEGIN PROLOGUE  INITS
C***PURPOSE  Determine the number of terms needed in an orthogonal
C            polynomial series so that it meets a specified accuracy.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C3A2
C***TYPE      SINGLE PRECISION (INITS-S, INITDS-D)
C***KEYWORDS  CHEBYSHEV, FNLIB, INITIALIZE, ORTHOGONAL POLYNOMIAL,
C             ORTHOGONAL SERIES, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C  Initialize the orthogonal series, represented by the array OS, so
C  that INITS is the number of terms needed to insure the error is no
C  larger than ETA.  Ordinarily, ETA will be chosen to be one-tenth
C  machine precision.
C
C             Input Arguments --
C   OS     single precision array of NOS coefficients in an orthogonal
C          series.
C   NOS    number of coefficients in OS.
C   ETA    single precision scalar containing requested accuracy of
C          series.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   891115  Modified error message.  (WRB)
C   891115  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  INITS
      REAL OS(*)
C***FIRST EXECUTABLE STATEMENT  INITS
      IF (NOS .LT. 1) CALL XERMSG ('SLATEC', 'INITS',
     +   'Number of coefficients is less than 1', 2, 1)
C
      ERR = 0.
      DO 10 II = 1,NOS
        I = NOS + 1 - II
        ERR = ERR + ABS(OS(I))
        IF (ERR.GT.ETA) GO TO 20
   10 CONTINUE
C
   20 IF (I .EQ. NOS) CALL XERMSG ('SLATEC', 'INITS',
     +   'Chebyshev series too short for specified accuracy', 1, 1)
      INITS = I
C
      RETURN
      END
*DECK DGAMLM
      SUBROUTINE DGAMLM (XMIN, XMAX)
C***BEGIN PROLOGUE  DGAMLM
C***PURPOSE  Compute the minimum and maximum bounds for the argument in
C            the Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7A, R2
C***TYPE      DOUBLE PRECISION (GAMLIM-S, DGAMLM-D)
C***KEYWORDS  COMPLETE GAMMA FUNCTION, FNLIB, LIMITS, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Calculate the minimum and maximum legal bounds for X in gamma(X).
C XMIN and XMAX are not the only bounds, but they are the only non-
C trivial ones to calculate.
C
C             Output Arguments --
C XMIN   double precision minimum legal value of X in gamma(X).  Any
C        smaller value of X might result in underflow.
C XMAX   double precision maximum legal value of X in gamma(X).  Any
C        larger value of X might cause overflow.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DGAMLM
      DOUBLE PRECISION XMIN, XMAX, ALNBIG, ALNSML, XLN, XOLD, D1MACH
C***FIRST EXECUTABLE STATEMENT  DGAMLM
      ALNSML = LOG(D1MACH(1))
      XMIN = -ALNSML
      DO 10 I=1,10
        XOLD = XMIN
        XLN = LOG(XMIN)
        XMIN = XMIN - XMIN*((XMIN+0.5D0)*XLN - XMIN - 0.2258D0 + ALNSML)
     1    / (XMIN*XLN+0.5D0)
        IF (ABS(XMIN-XOLD).LT.0.005D0) GO TO 20
 10   CONTINUE
      CALL XERMSG ('SLATEC', 'DGAMLM', 'UNABLE TO FIND XMIN', 1, 2)
C
 20   XMIN = -XMIN + 0.01D0
C
      ALNBIG = LOG (D1MACH(2))
      XMAX = ALNBIG
      DO 30 I=1,10
        XOLD = XMAX
        XLN = LOG(XMAX)
        XMAX = XMAX - XMAX*((XMAX-0.5D0)*XLN - XMAX + 0.9189D0 - ALNBIG)
     1    / (XMAX*XLN-0.5D0)
        IF (ABS(XMAX-XOLD).LT.0.005D0) GO TO 40
 30   CONTINUE
      CALL XERMSG ('SLATEC', 'DGAMLM', 'UNABLE TO FIND XMAX', 2, 2)
C
 40   XMAX = XMAX - 0.01D0
      XMIN = MAX (XMIN, -XMAX+1.D0)
C
      RETURN
      END
*DECK DLNGAM
      DOUBLE PRECISION FUNCTION DLNGAM (X)
C***BEGIN PROLOGUE  DLNGAM
C***PURPOSE  Compute the logarithm of the absolute value of the Gamma
C            function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7A
C***TYPE      DOUBLE PRECISION (ALNGAM-S, DLNGAM-D, CLNGAM-C)
C***KEYWORDS  ABSOLUTE VALUE, COMPLETE GAMMA FUNCTION, FNLIB, LOGARITHM,
C             SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DLNGAM(X) calculates the double precision logarithm of the
C absolute value of the Gamma function for double precision
C argument X.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, D9LGMC, DGAMMA, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900727  Added EXTERNAL statement.  (WRB)
C***END PROLOGUE  DLNGAM
      DOUBLE PRECISION X, DXREL, PI, SINPIY, SQPI2L, SQ2PIL, XMAX,
     1  Y, DGAMMA, D9LGMC, D1MACH, TEMP
      LOGICAL FIRST
      EXTERNAL DGAMMA
      SAVE SQ2PIL, SQPI2L, PI, XMAX, DXREL, FIRST
      DATA SQ2PIL / 0.9189385332 0467274178 0329736405 62 D0 /
      DATA SQPI2L / +.2257913526 4472743236 3097614947 441 D+0    /
      DATA PI / 3.1415926535 8979323846 2643383279 50 D0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DLNGAM
      IF (FIRST) THEN
         TEMP = 1.D0/LOG(D1MACH(2))
         XMAX = TEMP*D1MACH(2)
         DXREL = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS (X)
      IF (Y.GT.10.D0) GO TO 20
C
C LOG (ABS (DGAMMA(X)) ) FOR ABS(X) .LE. 10.0
C
      DLNGAM = LOG (ABS (DGAMMA(X)) )
      RETURN
C
C LOG ( ABS (DGAMMA(X)) ) FOR ABS(X) .GT. 10.0
C
 20   IF (Y .GT. XMAX) CALL XERMSG ('SLATEC', 'DLNGAM',
     +   'ABS(X) SO BIG DLNGAM OVERFLOWS', 2, 2)
C
      IF (X.GT.0.D0) DLNGAM = SQ2PIL + (X-0.5D0)*LOG(X) - X + D9LGMC(Y)
      IF (X.GT.0.D0) RETURN
C
      SINPIY = ABS (SIN(PI*Y))
      IF (SINPIY .EQ. 0.D0) CALL XERMSG ('SLATEC', 'DLNGAM',
     +   'X IS A NEGATIVE INTEGER', 3, 2)
C
      IF (ABS((X-AINT(X-0.5D0))/X) .LT. DXREL) CALL XERMSG ('SLATEC',
     +   'DLNGAM',
     +   'ANSWER LT HALF PRECISION BECAUSE X TOO NEAR NEGATIVE INTEGER',
     +   1, 1)
C
      DLNGAM = SQPI2L + (X-0.5D0)*LOG(Y) - X - LOG(SINPIY) - D9LGMC(Y)
      RETURN
C
      END
*DECK DBSI0E
      DOUBLE PRECISION FUNCTION DBSI0E (X)
C***BEGIN PROLOGUE  DBSI0E
C***PURPOSE  Compute the exponentially scaled modified (hyperbolic)
C            Bessel function of the first kind of order zero.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESI0E-S, DBSI0E-D)
C***KEYWORDS  EXPONENTIALLY SCALED, FIRST KIND, FNLIB,
C             HYPERBOLIC BESSEL FUNCTION, MODIFIED BESSEL FUNCTION,
C             ORDER ZERO, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBSI0E(X) calculates the double precision exponentially scaled
C modified (hyperbolic) Bessel function of the first kind of order
C zero for double precision argument X.  The result is the Bessel
C function I0(X) multiplied by EXP(-ABS(X)).
C
C Series for BI0        on the interval  0.          to  9.00000E+00
C                                        with weighted error   9.51E-34
C                                         log weighted error  33.02
C                               significant figures required  33.31
C                                    decimal places required  33.65
C
C Series for AI0        on the interval  1.25000E-01 to  3.33333E-01
C                                        with weighted error   2.74E-32
C                                         log weighted error  31.56
C                               significant figures required  30.15
C                                    decimal places required  32.39
C
C Series for AI02       on the interval  0.          to  1.25000E-01
C                                        with weighted error   1.97E-32
C                                         log weighted error  31.71
C                               significant figures required  30.15
C                                    decimal places required  32.63
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, INITDS
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C***END PROLOGUE  DBSI0E
      DOUBLE PRECISION X, BI0CS(18), AI0CS(46), AI02CS(69),
     1  XSML, Y, D1MACH, DCSEVL
      LOGICAL FIRST
      SAVE BI0CS, AI0CS, AI02CS, NTI0, NTAI0, NTAI02, XSML, FIRST
      DATA BI0CS(  1) / -.7660547252 8391449510 8189497624 3285 D-1   /
      DATA BI0CS(  2) / +.1927337953 9938082699 5240875088 1196 D+1   /
      DATA BI0CS(  3) / +.2282644586 9203013389 3702929233 0415 D+0   /
      DATA BI0CS(  4) / +.1304891466 7072904280 7933421069 1888 D-1   /
      DATA BI0CS(  5) / +.4344270900 8164874513 7868268102 6107 D-3   /
      DATA BI0CS(  6) / +.9422657686 0019346639 2317174411 8766 D-5   /
      DATA BI0CS(  7) / +.1434006289 5106910799 6209187817 9957 D-6   /
      DATA BI0CS(  8) / +.1613849069 6617490699 1541971999 4611 D-8   /
      DATA BI0CS(  9) / +.1396650044 5356696994 9509270814 2522 D-10  /
      DATA BI0CS( 10) / +.9579451725 5054453446 2752317189 3333 D-13  /
      DATA BI0CS( 11) / +.5333981859 8625021310 1510774400 0000 D-15  /
      DATA BI0CS( 12) / +.2458716088 4374707746 9678591999 9999 D-17  /
      DATA BI0CS( 13) / +.9535680890 2487700269 4434133333 3333 D-20  /
      DATA BI0CS( 14) / +.3154382039 7214273367 8933333333 3333 D-22  /
      DATA BI0CS( 15) / +.9004564101 0946374314 6666666666 6666 D-25  /
      DATA BI0CS( 16) / +.2240647369 1236700160 0000000000 0000 D-27  /
      DATA BI0CS( 17) / +.4903034603 2428373333 3333333333 3333 D-30  /
      DATA BI0CS( 18) / +.9508172606 1226666666 6666666666 6666 D-33  /
      DATA AI0CS(  1) / +.7575994494 0237959427 2987203743 8 D-1      /
      DATA AI0CS(  2) / +.7591380810 8233455072 9297873320 4 D-2      /
      DATA AI0CS(  3) / +.4153131338 9237505018 6319749138 2 D-3      /
      DATA AI0CS(  4) / +.1070076463 4390730735 8242970217 0 D-4      /
      DATA AI0CS(  5) / -.7901179979 2128946607 5031948573 0 D-5      /
      DATA AI0CS(  6) / -.7826143501 4387522697 8898980690 9 D-6      /
      DATA AI0CS(  7) / +.2783849942 9488708063 8118538985 7 D-6      /
      DATA AI0CS(  8) / +.8252472600 6120271919 6682913319 8 D-8      /
      DATA AI0CS(  9) / -.1204463945 5201991790 5496089110 3 D-7      /
      DATA AI0CS( 10) / +.1559648598 5060764436 1228752792 8 D-8      /
      DATA AI0CS( 11) / +.2292556367 1033165434 7725480285 7 D-9      /
      DATA AI0CS( 12) / -.1191622884 2790646036 7777423447 8 D-9      /
      DATA AI0CS( 13) / +.1757854916 0324098302 1833124774 3 D-10     /
      DATA AI0CS( 14) / +.1128224463 2189005171 4441135682 4 D-11     /
      DATA AI0CS( 15) / -.1146848625 9272988777 2963387698 2 D-11     /
      DATA AI0CS( 16) / +.2715592054 8036628726 4365192160 6 D-12     /
      DATA AI0CS( 17) / -.2415874666 5626878384 4247572028 1 D-13     /
      DATA AI0CS( 18) / -.6084469888 2551250646 0609963922 4 D-14     /
      DATA AI0CS( 19) / +.3145705077 1754772937 0836026730 3 D-14     /
      DATA AI0CS( 20) / -.7172212924 8711877179 6217505917 6 D-15     /
      DATA AI0CS( 21) / +.7874493403 4541033960 8390960332 7 D-16     /
      DATA AI0CS( 22) / +.1004802753 0094624023 4524457183 9 D-16     /
      DATA AI0CS( 23) / -.7566895365 3505348534 2843588881 0 D-17     /
      DATA AI0CS( 24) / +.2150380106 8761198878 1205128784 5 D-17     /
      DATA AI0CS( 25) / -.3754858341 8308744291 5158445260 8 D-18     /
      DATA AI0CS( 26) / +.2354065842 2269925769 0075710532 2 D-19     /
      DATA AI0CS( 27) / +.1114667612 0479285302 2637335511 0 D-19     /
      DATA AI0CS( 28) / -.5398891884 3969903786 9677932270 9 D-20     /
      DATA AI0CS( 29) / +.1439598792 2407526770 4285840452 2 D-20     /
      DATA AI0CS( 30) / -.2591916360 1110934064 6081840196 2 D-21     /
      DATA AI0CS( 31) / +.2238133183 9985839074 3409229824 0 D-22     /
      DATA AI0CS( 32) / +.5250672575 3647711727 7221683199 9 D-23     /
      DATA AI0CS( 33) / -.3249904138 5332307841 7343228586 6 D-23     /
      DATA AI0CS( 34) / +.9924214103 2050379278 5728471040 0 D-24     /
      DATA AI0CS( 35) / -.2164992254 2446695231 4655429973 3 D-24     /
      DATA AI0CS( 36) / +.3233609471 9435940839 7333299199 9 D-25     /
      DATA AI0CS( 37) / -.1184620207 3967424898 2473386666 6 D-26     /
      DATA AI0CS( 38) / -.1281671853 9504986505 4833868799 9 D-26     /
      DATA AI0CS( 39) / +.5827015182 2793905116 0556885333 3 D-27     /
      DATA AI0CS( 40) / -.1668222326 0261097193 6450150399 9 D-27     /
      DATA AI0CS( 41) / +.3625309510 5415699757 0068480000 0 D-28     /
      DATA AI0CS( 42) / -.5733627999 0557135899 4595839999 9 D-29     /
      DATA AI0CS( 43) / +.3736796722 0630982296 4258133333 3 D-30     /
      DATA AI0CS( 44) / +.1602073983 1568519633 6551253333 3 D-30     /
      DATA AI0CS( 45) / -.8700424864 0572298845 2249599999 9 D-31     /
      DATA AI0CS( 46) / +.2741320937 9374811456 0341333333 3 D-31     /
      DATA AI02CS(  1) / +.5449041101 4108831607 8960962268 0 D-1      /
      DATA AI02CS(  2) / +.3369116478 2556940898 9785662979 9 D-2      /
      DATA AI02CS(  3) / +.6889758346 9168239842 6263914301 1 D-4      /
      DATA AI02CS(  4) / +.2891370520 8347564829 6692402323 2 D-5      /
      DATA AI02CS(  5) / +.2048918589 4690637418 2760534093 1 D-6      /
      DATA AI02CS(  6) / +.2266668990 4981780645 9327743136 1 D-7      /
      DATA AI02CS(  7) / +.3396232025 7083863451 5084396952 3 D-8      /
      DATA AI02CS(  8) / +.4940602388 2249695891 0482449783 5 D-9      /
      DATA AI02CS(  9) / +.1188914710 7846438342 4084525196 3 D-10     /
      DATA AI02CS( 10) / -.3149916527 9632413645 3864862961 9 D-10     /
      DATA AI02CS( 11) / -.1321581184 0447713118 7540739926 7 D-10     /
      DATA AI02CS( 12) / -.1794178531 5068061177 7943574026 9 D-11     /
      DATA AI02CS( 13) / +.7180124451 3836662336 7106429346 9 D-12     /
      DATA AI02CS( 14) / +.3852778382 7421427011 4089801777 6 D-12     /
      DATA AI02CS( 15) / +.1540086217 5214098269 1325823339 7 D-13     /
      DATA AI02CS( 16) / -.4150569347 2872220866 2689972015 6 D-13     /
      DATA AI02CS( 17) / -.9554846698 8283076487 0214494312 5 D-14     /
      DATA AI02CS( 18) / +.3811680669 3526224207 4605535511 8 D-14     /
      DATA AI02CS( 19) / +.1772560133 0565263836 0493266675 8 D-14     /
      DATA AI02CS( 20) / -.3425485619 6772191346 1924790328 2 D-15     /
      DATA AI02CS( 21) / -.2827623980 5165834849 4205593759 4 D-15     /
      DATA AI02CS( 22) / +.3461222867 6974610930 9706250813 4 D-16     /
      DATA AI02CS( 23) / +.4465621420 2967599990 1042054284 3 D-16     /
      DATA AI02CS( 24) / -.4830504485 9441820712 5525403795 4 D-17     /
      DATA AI02CS( 25) / -.7233180487 8747539545 6227240924 5 D-17     /
      DATA AI02CS( 26) / +.9921475412 1736985988 8046093981 0 D-18     /
      DATA AI02CS( 27) / +.1193650890 8459820855 0439949924 2 D-17     /
      DATA AI02CS( 28) / -.2488709837 1508072357 2054491660 2 D-18     /
      DATA AI02CS( 29) / -.1938426454 1609059289 8469781132 6 D-18     /
      DATA AI02CS( 30) / +.6444656697 3734438687 8301949394 9 D-19     /
      DATA AI02CS( 31) / +.2886051596 2892243264 8171383073 4 D-19     /
      DATA AI02CS( 32) / -.1601954907 1749718070 6167156200 7 D-19     /
      DATA AI02CS( 33) / -.3270815010 5923147208 9193567485 9 D-20     /
      DATA AI02CS( 34) / +.3686932283 8264091811 4600723939 3 D-20     /
      DATA AI02CS( 35) / +.1268297648 0309501530 1359529710 9 D-22     /
      DATA AI02CS( 36) / -.7549825019 3772739076 9636664410 1 D-21     /
      DATA AI02CS( 37) / +.1502133571 3778353496 3712789053 4 D-21     /
      DATA AI02CS( 38) / +.1265195883 5096485349 3208799248 3 D-21     /
      DATA AI02CS( 39) / -.6100998370 0836807086 2940891600 2 D-22     /
      DATA AI02CS( 40) / -.1268809629 2601282643 6872095924 2 D-22     /
      DATA AI02CS( 41) / +.1661016099 8907414578 4038487490 5 D-22     /
      DATA AI02CS( 42) / -.1585194335 7658855793 7970504881 4 D-23     /
      DATA AI02CS( 43) / -.3302645405 9682178009 5381766755 6 D-23     /
      DATA AI02CS( 44) / +.1313580902 8392397817 4039623117 4 D-23     /
      DATA AI02CS( 45) / +.3689040246 6711567933 1425637280 4 D-24     /
      DATA AI02CS( 46) / -.4210141910 4616891492 1978247249 9 D-24     /
      DATA AI02CS( 47) / +.4791954591 0828657806 3171401373 0 D-25     /
      DATA AI02CS( 48) / +.8459470390 2218217952 9971707412 4 D-25     /
      DATA AI02CS( 49) / -.4039800940 8728324931 4607937181 0 D-25     /
      DATA AI02CS( 50) / -.6434714653 6504313473 0100850469 5 D-26     /
      DATA AI02CS( 51) / +.1225743398 8756659903 4464736990 5 D-25     /
      DATA AI02CS( 52) / -.2934391316 0257089231 9879821175 4 D-26     /
      DATA AI02CS( 53) / -.1961311309 1949829262 0371205728 9 D-26     /
      DATA AI02CS( 54) / +.1503520374 8221934241 6229900309 8 D-26     /
      DATA AI02CS( 55) / -.9588720515 7448265520 3386388206 9 D-28     /
      DATA AI02CS( 56) / -.3483339380 8170454863 9441108511 4 D-27     /
      DATA AI02CS( 57) / +.1690903610 2630436730 6244960725 6 D-27     /
      DATA AI02CS( 58) / +.1982866538 7356030438 9400115718 8 D-28     /
      DATA AI02CS( 59) / -.5317498081 4918162145 7583002528 4 D-28     /
      DATA AI02CS( 60) / +.1803306629 8883929462 3501450390 1 D-28     /
      DATA AI02CS( 61) / +.6213093341 4548931758 8405311242 2 D-29     /
      DATA AI02CS( 62) / -.7692189292 7721618632 0072806673 0 D-29     /
      DATA AI02CS( 63) / +.1858252826 1117025426 2556016596 3 D-29     /
      DATA AI02CS( 64) / +.1237585142 2813957248 9927154554 1 D-29     /
      DATA AI02CS( 65) / -.1102259120 4092238032 1779478779 2 D-29     /
      DATA AI02CS( 66) / +.1886287118 0397044900 7787447943 1 D-30     /
      DATA AI02CS( 67) / +.2160196872 2436589131 4903141406 0 D-30     /
      DATA AI02CS( 68) / -.1605454124 9197432005 8446594965 5 D-30     /
      DATA AI02CS( 69) / +.1965352984 5942906039 3884807331 8 D-31     /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBSI0E
      IF (FIRST) THEN
         ETA = 0.1*REAL(D1MACH(3))
         NTI0 = INITDS (BI0CS, 18, ETA)
         NTAI0 = INITDS (AI0CS, 46, ETA)
         NTAI02 = INITDS (AI02CS, 69, ETA)
         XSML = SQRT(4.5D0*D1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y.GT.3.0D0) GO TO 20
C
      DBSI0E = 1.0D0 - X
      IF (Y.GT.XSML) DBSI0E = EXP(-Y) * (2.75D0 +
     1  DCSEVL (Y*Y/4.5D0-1.D0, BI0CS, NTI0) )
      RETURN
C
 20   IF (Y.LE.8.D0) DBSI0E = (0.375D0 + DCSEVL ((48.D0/Y-11.D0)/5.D0,
     1  AI0CS, NTAI0))/SQRT(Y)
      IF (Y.GT.8.D0) DBSI0E = (0.375D0 + DCSEVL (16.D0/Y-1.D0, AI02CS,
     1  NTAI02))/SQRT(Y)
C
      RETURN
      END
*DECK DBSI1E
      DOUBLE PRECISION FUNCTION DBSI1E (X)
C***BEGIN PROLOGUE  DBSI1E
C***PURPOSE  Compute the exponentially scaled modified (hyperbolic)
C            Bessel function of the first kind of order one.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESI1E-S, DBSI1E-D)
C***KEYWORDS  EXPONENTIALLY SCALED, FIRST KIND, FNLIB,
C             HYPERBOLIC BESSEL FUNCTION, MODIFIED BESSEL FUNCTION,
C             ORDER ONE, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBSI1E(X) calculates the double precision exponentially scaled
C modified (hyperbolic) Bessel function of the first kind of order
C one for double precision argument X.  The result is I1(X)
C multiplied by EXP(-ABS(X)).
C
C Series for BI1        on the interval  0.          to  9.00000E+00
C                                        with weighted error   1.44E-32
C                                         log weighted error  31.84
C                               significant figures required  31.45
C                                    decimal places required  32.46
C
C Series for AI1        on the interval  1.25000E-01 to  3.33333E-01
C                                        with weighted error   2.81E-32
C                                         log weighted error  31.55
C                               significant figures required  29.93
C                                    decimal places required  32.38
C
C Series for AI12       on the interval  0.          to  1.25000E-01
C                                        with weighted error   1.83E-32
C                                         log weighted error  31.74
C                               significant figures required  29.97
C                                    decimal places required  32.66
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DBSI1E
      DOUBLE PRECISION X, BI1CS(17), AI1CS(46), AI12CS(69), XMIN,
     1  XSML, Y, D1MACH, DCSEVL
      LOGICAL FIRST
      SAVE BI1CS, AI1CS, AI12CS, NTI1, NTAI1, NTAI12, XMIN, XSML,
     1  FIRST
      DATA BI1CS(  1) / -.1971713261 0998597316 1385032181 49 D-2     /
      DATA BI1CS(  2) / +.4073488766 7546480608 1553936520 14 D+0     /
      DATA BI1CS(  3) / +.3483899429 9959455866 2450377837 87 D-1     /
      DATA BI1CS(  4) / +.1545394556 3001236038 5984010584 89 D-2     /
      DATA BI1CS(  5) / +.4188852109 8377784129 4588320041 20 D-4     /
      DATA BI1CS(  6) / +.7649026764 8362114741 9597039660 69 D-6     /
      DATA BI1CS(  7) / +.1004249392 4741178689 1798080372 38 D-7     /
      DATA BI1CS(  8) / +.9932207791 9238106481 3712980548 63 D-10    /
      DATA BI1CS(  9) / +.7663801791 8447637275 2001716813 49 D-12    /
      DATA BI1CS( 10) / +.4741418923 8167394980 3880919481 60 D-14    /
      DATA BI1CS( 11) / +.2404114404 0745181799 8631720320 00 D-16    /
      DATA BI1CS( 12) / +.1017150500 7093713649 1211007999 99 D-18    /
      DATA BI1CS( 13) / +.3645093565 7866949458 4917333333 33 D-21    /
      DATA BI1CS( 14) / +.1120574950 2562039344 8106666666 66 D-23    /
      DATA BI1CS( 15) / +.2987544193 4468088832 0000000000 00 D-26    /
      DATA BI1CS( 16) / +.6973231093 9194709333 3333333333 33 D-29    /
      DATA BI1CS( 17) / +.1436794822 0620800000 0000000000 00 D-31    /
      DATA AI1CS(  1) / -.2846744181 8814786741 0037246830 7 D-1      /
      DATA AI1CS(  2) / -.1922953231 4432206510 4444877497 9 D-1      /
      DATA AI1CS(  3) / -.6115185857 9437889822 5624991778 5 D-3      /
      DATA AI1CS(  4) / -.2069971253 3502277088 8282377797 9 D-4      /
      DATA AI1CS(  5) / +.8585619145 8107255655 3694467313 8 D-5      /
      DATA AI1CS(  6) / +.1049498246 7115908625 1745399786 0 D-5      /
      DATA AI1CS(  7) / -.2918338918 4479022020 9343232669 7 D-6      /
      DATA AI1CS(  8) / -.1559378146 6317390001 6068096907 7 D-7      /
      DATA AI1CS(  9) / +.1318012367 1449447055 2530287390 9 D-7      /
      DATA AI1CS( 10) / -.1448423418 1830783176 3913446781 5 D-8      /
      DATA AI1CS( 11) / -.2908512243 9931420948 2504099301 0 D-9      /
      DATA AI1CS( 12) / +.1266388917 8753823873 1115969040 3 D-9      /
      DATA AI1CS( 13) / -.1664947772 9192206706 2417839858 0 D-10     /
      DATA AI1CS( 14) / -.1666653644 6094329760 9593715499 9 D-11     /
      DATA AI1CS( 15) / +.1242602414 2907682652 3216847201 7 D-11     /
      DATA AI1CS( 16) / -.2731549379 6724323972 5146142863 3 D-12     /
      DATA AI1CS( 17) / +.2023947881 6458037807 0026268898 1 D-13     /
      DATA AI1CS( 18) / +.7307950018 1168836361 9869812612 3 D-14     /
      DATA AI1CS( 19) / -.3332905634 4046749438 1377861713 3 D-14     /
      DATA AI1CS( 20) / +.7175346558 5129537435 4225466567 0 D-15     /
      DATA AI1CS( 21) / -.6982530324 7962563558 5062922365 6 D-16     /
      DATA AI1CS( 22) / -.1299944201 5627607600 6044608058 7 D-16     /
      DATA AI1CS( 23) / +.8120942864 2427988920 5467834286 0 D-17     /
      DATA AI1CS( 24) / -.2194016207 4107368981 5626664378 3 D-17     /
      DATA AI1CS( 25) / +.3630516170 0296548482 7986093233 4 D-18     /
      DATA AI1CS( 26) / -.1695139772 4391041663 0686679039 9 D-19     /
      DATA AI1CS( 27) / -.1288184829 8979078071 1688253822 2 D-19     /
      DATA AI1CS( 28) / +.5694428604 9670527801 0999107310 9 D-20     /
      DATA AI1CS( 29) / -.1459597009 0904800565 4550990028 7 D-20     /
      DATA AI1CS( 30) / +.2514546010 6757173140 8469133448 5 D-21     /
      DATA AI1CS( 31) / -.1844758883 1391248181 6040002901 3 D-22     /
      DATA AI1CS( 32) / -.6339760596 2279486419 2860979199 9 D-23     /
      DATA AI1CS( 33) / +.3461441102 0310111111 0814662656 0 D-23     /
      DATA AI1CS( 34) / -.1017062335 3713935475 9654102357 3 D-23     /
      DATA AI1CS( 35) / +.2149877147 0904314459 6250077866 6 D-24     /
      DATA AI1CS( 36) / -.3045252425 2386764017 4620617386 6 D-25     /
      DATA AI1CS( 37) / +.5238082144 7212859821 7763498666 6 D-27     /
      DATA AI1CS( 38) / +.1443583107 0893824464 1678950399 9 D-26     /
      DATA AI1CS( 39) / -.6121302074 8900427332 0067071999 9 D-27     /
      DATA AI1CS( 40) / +.1700011117 4678184183 4918980266 6 D-27     /
      DATA AI1CS( 41) / -.3596589107 9842441585 3521578666 6 D-28     /
      DATA AI1CS( 42) / +.5448178578 9484185766 5051306666 6 D-29     /
      DATA AI1CS( 43) / -.2731831789 6890849891 6256426666 6 D-30     /
      DATA AI1CS( 44) / -.1858905021 7086007157 7190399999 9 D-30     /
      DATA AI1CS( 45) / +.9212682974 5139334411 2776533333 3 D-31     /
      DATA AI1CS( 46) / -.2813835155 6535611063 7083306666 6 D-31     /
      DATA AI12CS(  1) / +.2857623501 8280120474 4984594846 9 D-1      /
      DATA AI12CS(  2) / -.9761097491 3614684077 6516445730 2 D-2      /
      DATA AI12CS(  3) / -.1105889387 6262371629 1256921277 5 D-3      /
      DATA AI12CS(  4) / -.3882564808 8776903934 5654477627 4 D-5      /
      DATA AI12CS(  5) / -.2512236237 8702089252 9452002212 1 D-6      /
      DATA AI12CS(  6) / -.2631468846 8895195068 3705236523 2 D-7      /
      DATA AI12CS(  7) / -.3835380385 9642370220 4500678796 8 D-8      /
      DATA AI12CS(  8) / -.5589743462 1965838068 6811252222 9 D-9      /
      DATA AI12CS(  9) / -.1897495812 3505412344 9892503323 8 D-10     /
      DATA AI12CS( 10) / +.3252603583 0154882385 5508067994 9 D-10     /
      DATA AI12CS( 11) / +.1412580743 6613781331 6336633284 6 D-10     /
      DATA AI12CS( 12) / +.2035628544 1470895072 2452613684 0 D-11     /
      DATA AI12CS( 13) / -.7198551776 2459085120 9258989044 6 D-12     /
      DATA AI12CS( 14) / -.4083551111 0921973182 2849963969 1 D-12     /
      DATA AI12CS( 15) / -.2101541842 7726643130 1984572746 2 D-13     /
      DATA AI12CS( 16) / +.4272440016 7119513542 9778833699 7 D-13     /
      DATA AI12CS( 17) / +.1042027698 4128802764 1741449994 8 D-13     /
      DATA AI12CS( 18) / -.3814403072 4370078047 6707253539 6 D-14     /
      DATA AI12CS( 19) / -.1880354775 5107824485 1273453396 3 D-14     /
      DATA AI12CS( 20) / +.3308202310 9209282827 3190335240 5 D-15     /
      DATA AI12CS( 21) / +.2962628997 6459501390 6854654205 2 D-15     /
      DATA AI12CS( 22) / -.3209525921 9934239587 7837353288 7 D-16     /
      DATA AI12CS( 23) / -.4650305368 4893583255 7128281897 9 D-16     /
      DATA AI12CS( 24) / +.4414348323 0717079499 4611375964 1 D-17     /
      DATA AI12CS( 25) / +.7517296310 8421048054 2545808029 5 D-17     /
      DATA AI12CS( 26) / -.9314178867 3268833756 8484784515 7 D-18     /
      DATA AI12CS( 27) / -.1242193275 1948909561 1678448869 7 D-17     /
      DATA AI12CS( 28) / +.2414276719 4548484690 0515390217 6 D-18     /
      DATA AI12CS( 29) / +.2026944384 0532851789 7192286069 2 D-18     /
      DATA AI12CS( 30) / -.6394267188 2690977870 4391988681 1 D-19     /
      DATA AI12CS( 31) / -.3049812452 3730958960 8488450357 1 D-19     /
      DATA AI12CS( 32) / +.1612841851 6514802251 3462230769 1 D-19     /
      DATA AI12CS( 33) / +.3560913964 3099250545 1027090462 0 D-20     /
      DATA AI12CS( 34) / -.3752017947 9364390796 6682800324 6 D-20     /
      DATA AI12CS( 35) / -.5787037427 0747993459 5198231074 1 D-22     /
      DATA AI12CS( 36) / +.7759997511 6481619619 8236963209 2 D-21     /
      DATA AI12CS( 37) / -.1452790897 2022333940 6445987408 5 D-21     /
      DATA AI12CS( 38) / -.1318225286 7390367021 2192275337 4 D-21     /
      DATA AI12CS( 39) / +.6116654862 9030707018 7999133171 7 D-22     /
      DATA AI12CS( 40) / +.1376279762 4271264277 3024338363 4 D-22     /
      DATA AI12CS( 41) / -.1690837689 9593478849 1983938230 6 D-22     /
      DATA AI12CS( 42) / +.1430596088 5954331539 8720108538 5 D-23     /
      DATA AI12CS( 43) / +.3409557828 0905940204 0536772990 2 D-23     /
      DATA AI12CS( 44) / -.1309457666 2707602278 4573872642 4 D-23     /
      DATA AI12CS( 45) / -.3940706411 2402574360 9352141755 7 D-24     /
      DATA AI12CS( 46) / +.4277137426 9808765808 0616679735 2 D-24     /
      DATA AI12CS( 47) / -.4424634830 9826068819 0028312302 9 D-25     /
      DATA AI12CS( 48) / -.8734113196 2307149721 1530978874 7 D-25     /
      DATA AI12CS( 49) / +.4045401335 6835333921 4340414242 8 D-25     /
      DATA AI12CS( 50) / +.7067100658 0946894656 5160771780 6 D-26     /
      DATA AI12CS( 51) / -.1249463344 5651052230 0286451860 5 D-25     /
      DATA AI12CS( 52) / +.2867392244 4034370329 7948339142 6 D-26     /
      DATA AI12CS( 53) / +.2044292892 5042926702 8177957421 0 D-26     /
      DATA AI12CS( 54) / -.1518636633 8204625683 7134680291 1 D-26     /
      DATA AI12CS( 55) / +.8110181098 1875758861 3227910703 7 D-28     /
      DATA AI12CS( 56) / +.3580379354 7735860911 2717370327 0 D-27     /
      DATA AI12CS( 57) / -.1692929018 9279025095 9305717544 8 D-27     /
      DATA AI12CS( 58) / -.2222902499 7024276390 6775852777 4 D-28     /
      DATA AI12CS( 59) / +.5424535127 1459696550 4860040112 8 D-28     /
      DATA AI12CS( 60) / -.1787068401 5780186887 6491299330 4 D-28     /
      DATA AI12CS( 61) / -.6565479068 7228149388 2392943788 0 D-29     /
      DATA AI12CS( 62) / +.7807013165 0611452809 2206770683 9 D-29     /
      DATA AI12CS( 63) / -.1816595260 6689797173 7933315222 1 D-29     /
      DATA AI12CS( 64) / -.1287704952 6600848203 7687559895 9 D-29     /
      DATA AI12CS( 65) / +.1114548172 9881645474 1370927369 4 D-29     /
      DATA AI12CS( 66) / -.1808343145 0393369391 5936887668 7 D-30     /
      DATA AI12CS( 67) / -.2231677718 2037719522 3244822893 9 D-30     /
      DATA AI12CS( 68) / +.1619029596 0803415106 1790980361 4 D-30     /
      DATA AI12CS( 69) / -.1834079908 8049414139 0130843921 0 D-31     /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBSI1E
      IF (FIRST) THEN
         ETA = 0.1*REAL(D1MACH(3))
         NTI1 = INITDS (BI1CS, 17, ETA)
         NTAI1 = INITDS (AI1CS, 46, ETA)
         NTAI12 = INITDS (AI12CS, 69, ETA)
C
         XMIN = 2.0D0*D1MACH(1)
         XSML = SQRT(4.5D0*D1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y.GT.3.0D0) GO TO 20
C
      DBSI1E = 0.0D0
      IF (Y.EQ.0.D0)  RETURN
C
      IF (Y .LE. XMIN) CALL XERMSG ('SLATEC', 'DBSI1E',
     +   'ABS(X) SO SMALL I1 UNDERFLOWS', 1, 1)
      IF (Y.GT.XMIN) DBSI1E = 0.5D0*X
      IF (Y.GT.XSML) DBSI1E = X*(0.875D0 + DCSEVL (Y*Y/4.5D0-1.D0,
     1  BI1CS, NTI1) )
      DBSI1E = EXP(-Y) * DBSI1E
      RETURN
C
 20   IF (Y.LE.8.D0) DBSI1E = (0.375D0 + DCSEVL ((48.D0/Y-11.D0)/5.D0,
     1  AI1CS, NTAI1))/SQRT(Y)
      IF (Y.GT.8.D0) DBSI1E = (0.375D0 + DCSEVL (16.D0/Y-1.D0, AI12CS,
     1  NTAI12))/SQRT(Y)
      DBSI1E = SIGN (DBSI1E, X)
C
      RETURN
      END
*DECK DBSK0E
      DOUBLE PRECISION FUNCTION DBSK0E (X)
C***BEGIN PROLOGUE  DBSK0E
C***PURPOSE  Compute the exponentially scaled modified (hyperbolic)
C            Bessel function of the third kind of order zero.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESK0E-S, DBSK0E-D)
C***KEYWORDS  EXPONENTIALLY SCALED, FNLIB, HYPERBOLIC BESSEL FUNCTION,
C             MODIFIED BESSEL FUNCTION, ORDER ZERO, SPECIAL FUNCTIONS,
C             THIRD KIND
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBSK0E(X) computes the double precision exponentially scaled
C modified (hyperbolic) Bessel function of the third kind of
C order zero for positive double precision argument X.
C
C Series for BK0        on the interval  0.          to  4.00000E+00
C                                        with weighted error   3.08E-33
C                                         log weighted error  32.51
C                               significant figures required  32.05
C                                    decimal places required  33.11
C
C Series for AK0        on the interval  1.25000E-01 to  5.00000E-01
C                                        with weighted error   2.85E-32
C                                         log weighted error  31.54
C                               significant figures required  30.19
C                                    decimal places required  32.33
C
C Series for AK02       on the interval  0.          to  1.25000E-01
C                                        with weighted error   2.30E-32
C                                         log weighted error  31.64
C                               significant figures required  29.68
C                                    decimal places required  32.40
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DBESI0, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DBSK0E
      DOUBLE PRECISION X, BK0CS(16), AK0CS(38), AK02CS(33),
     1  XSML, Y, D1MACH, DCSEVL, DBESI0
      LOGICAL FIRST
      SAVE BK0CS, AK0CS, AK02CS, NTK0, NTAK0, NTAK02, XSML, FIRST
      DATA BK0CS(  1) / -.3532739323 3902768720 1140060063 153 D-1    /
      DATA BK0CS(  2) / +.3442898999 2462848688 6344927529 213 D+0    /
      DATA BK0CS(  3) / +.3597993651 5361501626 5721303687 231 D-1    /
      DATA BK0CS(  4) / +.1264615411 4469259233 8479508673 447 D-2    /
      DATA BK0CS(  5) / +.2286212103 1194517860 8269830297 585 D-4    /
      DATA BK0CS(  6) / +.2534791079 0261494573 0790013428 354 D-6    /
      DATA BK0CS(  7) / +.1904516377 2202088589 7214059381 366 D-8    /
      DATA BK0CS(  8) / +.1034969525 7633624585 1008317853 089 D-10   /
      DATA BK0CS(  9) / +.4259816142 7910825765 2445327170 133 D-13   /
      DATA BK0CS( 10) / +.1374465435 8807508969 4238325440 000 D-15   /
      DATA BK0CS( 11) / +.3570896528 5083735909 9688597333 333 D-18   /
      DATA BK0CS( 12) / +.7631643660 1164373766 7498666666 666 D-21   /
      DATA BK0CS( 13) / +.1365424988 4407818590 8053333333 333 D-23   /
      DATA BK0CS( 14) / +.2075275266 9066680831 9999999999 999 D-26   /
      DATA BK0CS( 15) / +.2712814218 0729856000 0000000000 000 D-29   /
      DATA BK0CS( 16) / +.3082593887 9146666666 6666666666 666 D-32   /
      DATA AK0CS(  1) / -.7643947903 3279414240 8297827008 8 D-1      /
      DATA AK0CS(  2) / -.2235652605 6998190520 2309555079 1 D-1      /
      DATA AK0CS(  3) / +.7734181154 6938582353 0061817404 7 D-3      /
      DATA AK0CS(  4) / -.4281006688 8860994644 5214643541 6 D-4      /
      DATA AK0CS(  5) / +.3081700173 8629747436 5001482666 0 D-5      /
      DATA AK0CS(  6) / -.2639367222 0096649740 6744889272 3 D-6      /
      DATA AK0CS(  7) / +.2563713036 4034692062 9408826574 2 D-7      /
      DATA AK0CS(  8) / -.2742705549 9002012638 5721191524 4 D-8      /
      DATA AK0CS(  9) / +.3169429658 0974995920 8083287340 3 D-9      /
      DATA AK0CS( 10) / -.3902353286 9621841416 0106571796 2 D-10     /
      DATA AK0CS( 11) / +.5068040698 1885754020 5009212728 6 D-11     /
      DATA AK0CS( 12) / -.6889574741 0078706795 4171355798 4 D-12     /
      DATA AK0CS( 13) / +.9744978497 8259176913 8820133683 1 D-13     /
      DATA AK0CS( 14) / -.1427332841 8845485053 8985534012 2 D-13     /
      DATA AK0CS( 15) / +.2156412571 0214630395 5806297652 7 D-14     /
      DATA AK0CS( 16) / -.3349654255 1495627721 8878205853 0 D-15     /
      DATA AK0CS( 17) / +.5335260216 9529116921 4528039260 1 D-16     /
      DATA AK0CS( 18) / -.8693669980 8907538076 3962237883 7 D-17     /
      DATA AK0CS( 19) / +.1446404347 8622122278 8776344234 6 D-17     /
      DATA AK0CS( 20) / -.2452889825 5001296824 0467875157 3 D-18     /
      DATA AK0CS( 21) / +.4233754526 2321715728 2170634240 0 D-19     /
      DATA AK0CS( 22) / -.7427946526 4544641956 9534129493 3 D-20     /
      DATA AK0CS( 23) / +.1323150529 3926668662 7796746240 0 D-20     /
      DATA AK0CS( 24) / -.2390587164 7396494513 3598146559 9 D-21     /
      DATA AK0CS( 25) / +.4376827585 9232261401 6571255466 6 D-22     /
      DATA AK0CS( 26) / -.8113700607 3451180593 3901141333 3 D-23     /
      DATA AK0CS( 27) / +.1521819913 8321729583 1037815466 6 D-23     /
      DATA AK0CS( 28) / -.2886041941 4833977702 3595861333 3 D-24     /
      DATA AK0CS( 29) / +.5530620667 0547179799 9261013333 3 D-25     /
      DATA AK0CS( 30) / -.1070377329 2498987285 9163306666 6 D-25     /
      DATA AK0CS( 31) / +.2091086893 1423843002 9632853333 3 D-26     /
      DATA AK0CS( 32) / -.4121713723 6462038274 1026133333 3 D-27     /
      DATA AK0CS( 33) / +.8193483971 1213076401 3568000000 0 D-28     /
      DATA AK0CS( 34) / -.1642000275 4592977267 8075733333 3 D-28     /
      DATA AK0CS( 35) / +.3316143281 4802271958 9034666666 6 D-29     /
      DATA AK0CS( 36) / -.6746863644 1452959410 8586666666 6 D-30     /
      DATA AK0CS( 37) / +.1382429146 3184246776 3541333333 3 D-30     /
      DATA AK0CS( 38) / -.2851874167 3598325708 1173333333 3 D-31     /
      DATA AK02CS(  1) / -.1201869826 3075922398 3934621245 2 D-1      /
      DATA AK02CS(  2) / -.9174852691 0256953106 5256107571 3 D-2      /
      DATA AK02CS(  3) / +.1444550931 7750058210 4884387805 7 D-3      /
      DATA AK02CS(  4) / -.4013614175 4357097286 7102107787 9 D-5      /
      DATA AK02CS(  5) / +.1567831810 8523106725 9034899033 3 D-6      /
      DATA AK02CS(  6) / -.7770110438 5217377103 1579975446 0 D-8      /
      DATA AK02CS(  7) / +.4611182576 1797178825 3313052958 6 D-9      /
      DATA AK02CS(  8) / -.3158592997 8605657705 2666580330 9 D-10     /
      DATA AK02CS(  9) / +.2435018039 3650411278 3588781432 9 D-11     /
      DATA AK02CS( 10) / -.2074331387 3983478977 0985337350 6 D-12     /
      DATA AK02CS( 11) / +.1925787280 5899170847 4273650469 3 D-13     /
      DATA AK02CS( 12) / -.1927554805 8389561036 0034718221 8 D-14     /
      DATA AK02CS( 13) / +.2062198029 1978182782 8523786964 4 D-15     /
      DATA AK02CS( 14) / -.2341685117 5792424026 0364019507 1 D-16     /
      DATA AK02CS( 15) / +.2805902810 6430422468 1517882845 8 D-17     /
      DATA AK02CS( 16) / -.3530507631 1618079458 1548246357 3 D-18     /
      DATA AK02CS( 17) / +.4645295422 9351082674 2421633706 6 D-19     /
      DATA AK02CS( 18) / -.6368625941 3442664739 2205346133 3 D-20     /
      DATA AK02CS( 19) / +.9069521310 9865155676 2234880000 0 D-21     /
      DATA AK02CS( 20) / -.1337974785 4236907398 4500531199 9 D-21     /
      DATA AK02CS( 21) / +.2039836021 8599523155 2208896000 0 D-22     /
      DATA AK02CS( 22) / -.3207027481 3678405000 6086997333 3 D-23     /
      DATA AK02CS( 23) / +.5189744413 6623099636 2635946666 6 D-24     /
      DATA AK02CS( 24) / -.8629501497 5405721929 6460799999 9 D-25     /
      DATA AK02CS( 25) / +.1472161183 1025598552 0803840000 0 D-25     /
      DATA AK02CS( 26) / -.2573069023 8670112838 1235199999 9 D-26     /
      DATA AK02CS( 27) / +.4601774086 6435165873 7664000000 0 D-27     /
      DATA AK02CS( 28) / -.8411555324 2010937371 3066666666 6 D-28     /
      DATA AK02CS( 29) / +.1569806306 6353689393 0154666666 6 D-28     /
      DATA AK02CS( 30) / -.2988226453 0057577889 7919999999 9 D-29     /
      DATA AK02CS( 31) / +.5796831375 2168365206 1866666666 6 D-30     /
      DATA AK02CS( 32) / -.1145035994 3476813321 5573333333 3 D-30     /
      DATA AK02CS( 33) / +.2301266594 2496828020 0533333333 3 D-31     /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBSK0E
      IF (FIRST) THEN
         ETA = 0.1*REAL(D1MACH(3))
         NTK0 = INITDS (BK0CS, 16, ETA)
         NTAK0 = INITDS (AK0CS, 38, ETA)
         NTAK02 = INITDS (AK02CS, 33, ETA)
         XSML = SQRT(4.0D0*D1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LE. 0.D0) CALL XERMSG ('SLATEC', 'DBSK0E',
     +   'X IS ZERO OR NEGATIVE', 2, 2)
      IF (X.GT.2.0D0) GO TO 20
C
      Y = 0.D0
      IF (X.GT.XSML) Y = X*X
      DBSK0E = EXP(X)*(-LOG(0.5D0*X)*DBESI0(X) - 0.25D0 +
     1  DCSEVL (.5D0*Y-1.D0, BK0CS, NTK0))
      RETURN
C
 20   IF (X.LE.8.D0) DBSK0E = (1.25D0 + DCSEVL ((16.D0/X-5.D0)/3.D0,
     1  AK0CS, NTAK0))/SQRT(X)
      IF (X.GT.8.D0) DBSK0E = (1.25D0 +
     1  DCSEVL (16.D0/X-1.D0, AK02CS, NTAK02))/SQRT(X)
C
      RETURN
      END
*DECK DBSK1E
      DOUBLE PRECISION FUNCTION DBSK1E (X)
C***BEGIN PROLOGUE  DBSK1E
C***PURPOSE  Compute the exponentially scaled modified (hyperbolic)
C            Bessel function of the third kind of order one.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESK1E-S, DBSK1E-D)
C***KEYWORDS  EXPONENTIALLY SCALED, FNLIB, HYPERBOLIC BESSEL FUNCTION,
C             MODIFIED BESSEL FUNCTION, ORDER ONE, SPECIAL FUNCTIONS,
C             THIRD KIND
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBSK1E(S) computes the double precision exponentially scaled
C modified (hyperbolic) Bessel function of the third kind of order
C one for positive double precision argument X.
C
C Series for BK1        on the interval  0.          to  4.00000E+00
C                                        with weighted error   9.16E-32
C                                         log weighted error  31.04
C                               significant figures required  30.61
C                                    decimal places required  31.64
C
C Series for AK1        on the interval  1.25000E-01 to  5.00000E-01
C                                        with weighted error   3.07E-32
C                                         log weighted error  31.51
C                               significant figures required  30.71
C                                    decimal places required  32.30
C
C Series for AK12       on the interval  0.          to  1.25000E-01
C                                        with weighted error   2.41E-32
C                                         log weighted error  31.62
C                               significant figures required  30.25
C                                    decimal places required  32.38
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DBESI1, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DBSK1E
      DOUBLE PRECISION X, BK1CS(16), AK1CS(38), AK12CS(33), XMIN,
     1  XSML, Y, D1MACH, DCSEVL, DBESI1
      LOGICAL FIRST
      SAVE BK1CS, AK1CS, AK12CS, NTK1, NTAK1, NTAK12, XMIN, XSML,
     1  FIRST
      DATA BK1CS(  1) / +.2530022733 8947770532 5311208685 33 D-1     /
      DATA BK1CS(  2) / -.3531559607 7654487566 7238316918 01 D+0     /
      DATA BK1CS(  3) / -.1226111808 2265714823 4790679300 42 D+0     /
      DATA BK1CS(  4) / -.6975723859 6398643501 8129202960 83 D-2     /
      DATA BK1CS(  5) / -.1730288957 5130520630 1765073689 79 D-3     /
      DATA BK1CS(  6) / -.2433406141 5659682349 6007350301 64 D-5     /
      DATA BK1CS(  7) / -.2213387630 7347258558 3152525451 26 D-7     /
      DATA BK1CS(  8) / -.1411488392 6335277610 9583302126 08 D-9     /
      DATA BK1CS(  9) / -.6666901694 1993290060 8537512643 73 D-12    /
      DATA BK1CS( 10) / -.2427449850 5193659339 2631968648 53 D-14    /
      DATA BK1CS( 11) / -.7023863479 3862875971 7837971200 00 D-17    /
      DATA BK1CS( 12) / -.1654327515 5100994675 4910293333 33 D-19    /
      DATA BK1CS( 13) / -.3233834745 9944491991 8933333333 33 D-22    /
      DATA BK1CS( 14) / -.5331275052 9265274999 4666666666 66 D-25    /
      DATA BK1CS( 15) / -.7513040716 2157226666 6666666666 66 D-28    /
      DATA BK1CS( 16) / -.9155085717 6541866666 6666666666 66 D-31    /
      DATA AK1CS(  1) / +.2744313406 9738829695 2576662272 66 D+0     /
      DATA AK1CS(  2) / +.7571989953 1993678170 8923781492 90 D-1     /
      DATA AK1CS(  3) / -.1441051556 4754061229 8531161756 25 D-2     /
      DATA AK1CS(  4) / +.6650116955 1257479394 2513854770 36 D-4     /
      DATA AK1CS(  5) / -.4369984709 5201407660 5808450891 67 D-5     /
      DATA AK1CS(  6) / +.3540277499 7630526799 4171390085 34 D-6     /
      DATA AK1CS(  7) / -.3311163779 2932920208 9826882457 04 D-7     /
      DATA AK1CS(  8) / +.3445977581 9010534532 3114997709 92 D-8     /
      DATA AK1CS(  9) / -.3898932347 4754271048 9819374927 58 D-9     /
      DATA AK1CS( 10) / +.4720819750 4658356400 9474493390 05 D-10    /
      DATA AK1CS( 11) / -.6047835662 8753562345 3735915628 90 D-11    /
      DATA AK1CS( 12) / +.8128494874 8658747888 1938379856 63 D-12    /
      DATA AK1CS( 13) / -.1138694574 7147891428 9239159510 42 D-12    /
      DATA AK1CS( 14) / +.1654035840 8462282325 9729482050 90 D-13    /
      DATA AK1CS( 15) / -.2480902567 7068848221 5160104405 33 D-14    /
      DATA AK1CS( 16) / +.3829237890 7024096948 4292272991 57 D-15    /
      DATA AK1CS( 17) / -.6064734104 0012418187 7682103773 86 D-16    /
      DATA AK1CS( 18) / +.9832425623 2648616038 1940046506 66 D-17    /
      DATA AK1CS( 19) / -.1628416873 8284380035 6666201156 26 D-17    /
      DATA AK1CS( 20) / +.2750153649 6752623718 2841203370 66 D-18    /
      DATA AK1CS( 21) / -.4728966646 3953250924 2810695680 00 D-19    /
      DATA AK1CS( 22) / +.8268150002 8109932722 3920503466 66 D-20    /
      DATA AK1CS( 23) / -.1468140513 6624956337 1939648853 33 D-20    /
      DATA AK1CS( 24) / +.2644763926 9208245978 0858948266 66 D-21    /
      DATA AK1CS( 25) / -.4829015756 4856387897 9698688000 00 D-22    /
      DATA AK1CS( 26) / +.8929302074 3610130180 6563327999 99 D-23    /
      DATA AK1CS( 27) / -.1670839716 8972517176 9977514666 66 D-23    /
      DATA AK1CS( 28) / +.3161645603 4040694931 3686186666 66 D-24    /
      DATA AK1CS( 29) / -.6046205531 2274989106 5064106666 66 D-25    /
      DATA AK1CS( 30) / +.1167879894 2042732700 7184213333 33 D-25    /
      DATA AK1CS( 31) / -.2277374158 2653996232 8678400000 00 D-26    /
      DATA AK1CS( 32) / +.4481109730 0773675795 3058133333 33 D-27    /
      DATA AK1CS( 33) / -.8893288476 9020194062 3360000000 00 D-28    /
      DATA AK1CS( 34) / +.1779468001 8850275131 3920000000 00 D-28    /
      DATA AK1CS( 35) / -.3588455596 7329095821 9946666666 66 D-29    /
      DATA AK1CS( 36) / +.7290629049 2694257991 6799999999 99 D-30    /
      DATA AK1CS( 37) / -.1491844984 5546227073 0240000000 00 D-30    /
      DATA AK1CS( 38) / +.3073657387 2934276300 7999999999 99 D-31    /
      DATA AK12CS(  1) / +.6379308343 7390010366 0048853410 2 D-1      /
      DATA AK12CS(  2) / +.2832887813 0497209358 3503028470 8 D-1      /
      DATA AK12CS(  3) / -.2475370673 9052503454 1454556673 2 D-3      /
      DATA AK12CS(  4) / +.5771972451 6072488204 7097662576 3 D-5      /
      DATA AK12CS(  5) / -.2068939219 5365483027 4553319655 2 D-6      /
      DATA AK12CS(  6) / +.9739983441 3818041803 0921309788 7 D-8      /
      DATA AK12CS(  7) / -.5585336140 3806249846 8889551112 9 D-9      /
      DATA AK12CS(  8) / +.3732996634 0461852402 2121285473 1 D-10     /
      DATA AK12CS(  9) / -.2825051961 0232254451 3506575492 8 D-11     /
      DATA AK12CS( 10) / +.2372019002 4841441736 4349695548 6 D-12     /
      DATA AK12CS( 11) / -.2176677387 9917539792 6830166793 8 D-13     /
      DATA AK12CS( 12) / +.2157914161 6160324539 3956268970 6 D-14     /
      DATA AK12CS( 13) / -.2290196930 7182692759 9155133815 4 D-15     /
      DATA AK12CS( 14) / +.2582885729 8232749619 1993956522 6 D-16     /
      DATA AK12CS( 15) / -.3076752641 2684631876 2109817344 0 D-17     /
      DATA AK12CS( 16) / +.3851487721 2804915970 9489684479 9 D-18     /
      DATA AK12CS( 17) / -.5044794897 6415289771 1728250880 0 D-19     /
      DATA AK12CS( 18) / +.6888673850 4185442370 1829222399 9 D-20     /
      DATA AK12CS( 19) / -.9775041541 9501183030 0213248000 0 D-21     /
      DATA AK12CS( 20) / +.1437416218 5238364610 0165973333 3 D-21     /
      DATA AK12CS( 21) / -.2185059497 3443473734 9973333333 3 D-22     /
      DATA AK12CS( 22) / +.3426245621 8092206316 4538880000 0 D-23     /
      DATA AK12CS( 23) / -.5531064394 2464082325 0124800000 0 D-24     /
      DATA AK12CS( 24) / +.9176601505 6859954037 8282666666 6 D-25     /
      DATA AK12CS( 25) / -.1562287203 6180249114 4874666666 6 D-25     /
      DATA AK12CS( 26) / +.2725419375 4843331323 4943999999 9 D-26     /
      DATA AK12CS( 27) / -.4865674910 0748279923 7802666666 6 D-27     /
      DATA AK12CS( 28) / +.8879388552 7235025873 5786666666 6 D-28     /
      DATA AK12CS( 29) / -.1654585918 0392575489 3653333333 3 D-28     /
      DATA AK12CS( 30) / +.3145111321 3578486743 0399999999 9 D-29     /
      DATA AK12CS( 31) / -.6092998312 1931276124 1600000000 0 D-30     /
      DATA AK12CS( 32) / +.1202021939 3698158346 2399999999 9 D-30     /
      DATA AK12CS( 33) / -.2412930801 4594088413 8666666666 6 D-31     /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBSK1E
      IF (FIRST) THEN
         ETA = 0.1*REAL(D1MACH(3))
         NTK1 = INITDS (BK1CS, 16, ETA)
         NTAK1 = INITDS (AK1CS, 38, ETA)
         NTAK12 = INITDS (AK12CS, 33, ETA)
C
         XMIN = EXP (MAX(LOG(D1MACH(1)), -LOG(D1MACH(2))) + 0.01D0)
         XSML = SQRT(4.0D0*D1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LE. 0.D0) CALL XERMSG ('SLATEC', 'DBSK1E',
     +   'X IS ZERO OR NEGATIVE', 2, 2)
      IF (X.GT.2.0D0) GO TO 20
C
      IF (X .LT. XMIN) CALL XERMSG ('SLATEC', 'DBSK1E',
     +   'X SO SMALL K1 OVERFLOWS', 3, 2)
      Y = 0.D0
      IF (X.GT.XSML) Y = X*X
      DBSK1E = EXP(X)*(LOG(0.5D0*X)*DBESI1(X) + (0.75D0 +
     1  DCSEVL (0.5D0*Y-1.D0, BK1CS, NTK1))/X )
      RETURN
C
 20   IF (X.LE.8.D0) DBSK1E = (1.25D0 + DCSEVL ((16.D0/X-5.D0)/3.D0,
     1  AK1CS, NTAK1))/SQRT(X)
      IF (X.GT.8.D0) DBSK1E = (1.25D0 +
     1  DCSEVL (16.D0/X-1.D0, AK12CS, NTAK12))/SQRT(X)
C
      RETURN
      END
*DECK D9LGMC
      DOUBLE PRECISION FUNCTION D9LGMC (X)
C***BEGIN PROLOGUE  D9LGMC
C***SUBSIDIARY
C***PURPOSE  Compute the log Gamma correction factor so that
C            LOG(DGAMMA(X)) = LOG(SQRT(2*PI)) + (X-5.)*LOG(X) - X
C            + D9LGMC(X).
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      DOUBLE PRECISION (R9LGMC-S, D9LGMC-D, C9LGMC-C)
C***KEYWORDS  COMPLETE GAMMA FUNCTION, CORRECTION TERM, FNLIB,
C             LOG GAMMA, LOGARITHM, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Compute the log gamma correction factor for X .GE. 10. so that
C LOG (DGAMMA(X)) = LOG(SQRT(2*PI)) + (X-.5)*LOG(X) - X + D9lGMC(X)
C
C Series for ALGM       on the interval  0.          to  1.00000E-02
C                                        with weighted error   1.28E-31
C                                         log weighted error  30.89
C                               significant figures required  29.81
C                                    decimal places required  31.48
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900720  Routine changed from user-callable to subsidiary.  (WRB)
C***END PROLOGUE  D9LGMC
      DOUBLE PRECISION X, ALGMCS(15), XBIG, XMAX, DCSEVL, D1MACH
      LOGICAL FIRST
      SAVE ALGMCS, NALGM, XBIG, XMAX, FIRST
      DATA ALGMCS(  1) / +.1666389480 4518632472 0572965082 2 D+0      /
      DATA ALGMCS(  2) / -.1384948176 0675638407 3298605913 5 D-4      /
      DATA ALGMCS(  3) / +.9810825646 9247294261 5717154748 7 D-8      /
      DATA ALGMCS(  4) / -.1809129475 5724941942 6330626671 9 D-10     /
      DATA ALGMCS(  5) / +.6221098041 8926052271 2601554341 6 D-13     /
      DATA ALGMCS(  6) / -.3399615005 4177219443 0333059966 6 D-15     /
      DATA ALGMCS(  7) / +.2683181998 4826987489 5753884666 6 D-17     /
      DATA ALGMCS(  8) / -.2868042435 3346432841 4462239999 9 D-19     /
      DATA ALGMCS(  9) / +.3962837061 0464348036 7930666666 6 D-21     /
      DATA ALGMCS( 10) / -.6831888753 9857668701 1199999999 9 D-23     /
      DATA ALGMCS( 11) / +.1429227355 9424981475 7333333333 3 D-24     /
      DATA ALGMCS( 12) / -.3547598158 1010705471 9999999999 9 D-26     /
      DATA ALGMCS( 13) / +.1025680058 0104709120 0000000000 0 D-27     /
      DATA ALGMCS( 14) / -.3401102254 3167487999 9999999999 9 D-29     /
      DATA ALGMCS( 15) / +.1276642195 6300629333 3333333333 3 D-30     /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  D9LGMC
      IF (FIRST) THEN
         NALGM = INITDS (ALGMCS, 15, REAL(D1MACH(3)) )
         XBIG = 1.0D0/SQRT(D1MACH(3))
         XMAX = EXP (MIN(LOG(D1MACH(2)/12.D0), -LOG(12.D0*D1MACH(1))))
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LT. 10.D0) CALL XERMSG ('SLATEC', 'D9LGMC',
     +   'X MUST BE GE 10', 1, 2)
      IF (X.GE.XMAX) GO TO 20
C
      D9LGMC = 1.D0/(12.D0*X)
      IF (X.LT.XBIG) D9LGMC = DCSEVL (2.0D0*(10.D0/X)**2-1.D0, ALGMCS,
     1  NALGM) / X
      RETURN
C
 20   D9LGMC = 0.D0
      CALL XERMSG ('SLATEC', 'D9LGMC', 'X SO BIG D9LGMC UNDERFLOWS', 2,
     +   1)
      RETURN
C
      END
*DECK D9LN2R
      DOUBLE PRECISION FUNCTION D9LN2R (X)
C***BEGIN PROLOGUE  D9LN2R
C***SUBSIDIARY
C***PURPOSE  Evaluate LOG(1+X) from second order relative accuracy so
C            that LOG(1+X) = X - X**2/2 + X**3*D9LN2R(X)
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C4B
C***TYPE      DOUBLE PRECISION (R9LN2R-S, D9LN2R-D, C9LN2R-C)
C***KEYWORDS  ELEMENTARY FUNCTIONS, FNLIB, LOGARITHM, SECOND ORDER
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Evaluate  LOG(1+X)  from 2-nd order with relative error accuracy so
C that    LOG(1+X) = X - X**2/2 + X**3*D9LN2R(X)
C
C Series for LN21       on the interval -6.25000E-01 to  0.
C                                        with weighted error   1.82E-32
C                                         log weighted error  31.74
C                               significant figures required  31.00
C                                    decimal places required  32.59
C
C Series for LN22       on the interval  0.          to  8.12500E-01
C                                        with weighted error   6.10E-32
C                                         log weighted error  31.21
C                               significant figures required  30.32
C                                    decimal places required  32.00
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   780401  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900720  Routine changed from user-callable to subsidiary.  (WRB)
C***END PROLOGUE  D9LN2R
      DOUBLE PRECISION X, XBIG, TXBIG, XMAX, TXMAX, XMIN, LN21CS(50),
     *  LN22CS(37), DCSEVL, D1MACH
      LOGICAL FIRST
      SAVE LN21CS, LN22CS, NTLN21, NTLN22, XMIN, XBIG, XMAX, FIRST
      DATA LN21CS(  1) / +.1811196251 3478809875 8949530430 71 D+0     /
      DATA LN21CS(  2) / -.1562712319 2872462669 6251555410 78 D+0     /
      DATA LN21CS(  3) / +.2867630536 1557275209 5406271020 51 D-1     /
      DATA LN21CS(  4) / -.5558699655 9481398781 1577251267 81 D-2     /
      DATA LN21CS(  5) / +.1117897665 2299837657 3356662797 27 D-2     /
      DATA LN21CS(  6) / -.2308050898 2327947182 2992795857 05 D-3     /
      DATA LN21CS(  7) / +.4859885334 1100175874 6815580687 50 D-4     /
      DATA LN21CS(  8) / -.1039012738 8903210765 5142426333 38 D-4     /
      DATA LN21CS(  9) / +.2248456370 7390128494 6218049464 08 D-5     /
      DATA LN21CS( 10) / -.4914059273 9266484875 3278025970 91 D-6     /
      DATA LN21CS( 11) / +.1082825650 7077483336 6201529715 97 D-6     /
      DATA LN21CS( 12) / -.2402587276 3420701435 9766754167 19 D-7     /
      DATA LN21CS( 13) / +.5362460047 2708133762 9844432501 63 D-8     /
      DATA LN21CS( 14) / -.1202995136 2138772264 6716464243 77 D-8     /
      DATA LN21CS( 15) / +.2710788927 7591860785 6225516322 66 D-9     /
      DATA LN21CS( 16) / -.6132356261 8319010068 7967284306 90 D-10    /
      DATA LN21CS( 17) / +.1392085836 9159469857 4369085439 78 D-10    /
      DATA LN21CS( 18) / -.3169930033 0223494015 2830572608 83 D-11    /
      DATA LN21CS( 19) / +.7238375404 4307505335 2143261970 11 D-12    /
      DATA LN21CS( 20) / -.1657001718 4764411391 4988055062 68 D-12    /
      DATA LN21CS( 21) / +.3801842866 3117424257 3644226318 76 D-13    /
      DATA LN21CS( 22) / -.8741118929 6972700259 7244298991 37 D-14    /
      DATA LN21CS( 23) / +.2013561984 5055748302 1187510281 54 D-14    /
      DATA LN21CS( 24) / -.4646445640 9033907031 1020081544 77 D-15    /
      DATA LN21CS( 25) / +.1073928214 7018339453 4533385549 25 D-15    /
      DATA LN21CS( 26) / -.2485853461 9937794755 5340218339 60 D-16    /
      DATA LN21CS( 27) / +.5762019795 0800189813 8881426281 81 D-17    /
      DATA LN21CS( 28) / -.1337306376 9804394701 4021999580 50 D-17    /
      DATA LN21CS( 29) / +.3107465322 7331824966 5338071668 05 D-18    /
      DATA LN21CS( 30) / -.7228810408 3040539906 9019579176 27 D-19    /
      DATA LN21CS( 31) / +.1683378378 8037385103 3132581868 88 D-19    /
      DATA LN21CS( 32) / -.3923946331 2069958052 5193727399 25 D-20    /
      DATA LN21CS( 33) / +.9155146838 7536789746 3855286408 53 D-21    /
      DATA LN21CS( 34) / -.2137889532 1320159520 9820958010 02 D-21    /
      DATA LN21CS( 35) / +.4996450747 9047864699 8285645687 46 D-22    /
      DATA LN21CS( 36) / -.1168624063 6080170135 3608061474 13 D-22    /
      DATA LN21CS( 37) / +.2735312347 0391863775 6286867865 59 D-23    /
      DATA LN21CS( 38) / -.6406802508 4792111965 0503458815 99 D-24    /
      DATA LN21CS( 39) / +.1501629320 4334124162 9490719402 66 D-24    /
      DATA LN21CS( 40) / -.3521737241 0398479759 4971450026 66 D-25    /
      DATA LN21CS( 41) / +.8264390101 4814767012 4827333973 33 D-26    /
      DATA LN21CS( 42) / -.1940493027 5943401918 0366178986 66 D-26    /
      DATA LN21CS( 43) / +.4558788001 8841283562 4515884373 33 D-27    /
      DATA LN21CS( 44) / -.1071549208 7545202154 3786250239 99 D-27    /
      DATA LN21CS( 45) / +.2519940800 7927592978 0966741333 33 D-28    /
      DATA LN21CS( 46) / -.5928908840 0120969341 7504768000 00 D-29    /
      DATA LN21CS( 47) / +.1395586406 1057513058 2371532799 99 D-29    /
      DATA LN21CS( 48) / -.3286457881 3478583431 4366975999 99 D-30    /
      DATA LN21CS( 49) / +.7742496795 0478166247 2546986666 66 D-31    /
      DATA LN21CS( 50) / -.1824773566 7260887638 1252266666 66 D-31    /
      DATA LN22CS(  1) / -.2224253253 5020460829 8601522355 2 D+0      /
      DATA LN22CS(  2) / -.6104710010 8078623986 8010475576 4 D-1      /
      DATA LN22CS(  3) / +.7427235009 7503945905 1962975572 9 D-2      /
      DATA LN22CS(  4) / -.9335018261 6369705656 1277960639 7 D-3      /
      DATA LN22CS(  5) / +.1200499076 8726012833 5073128735 9 D-3      /
      DATA LN22CS(  6) / -.1570472295 2820041128 2335260824 3 D-4      /
      DATA LN22CS(  7) / +.2081874781 0512710960 5078359275 9 D-5      /
      DATA LN22CS(  8) / -.2789195577 6467136540 5721305137 5 D-6      /
      DATA LN22CS(  9) / +.3769355823 7601320584 2289513544 7 D-7      /
      DATA LN22CS( 10) / -.5130902896 5277112582 4058993800 3 D-8      /
      DATA LN22CS( 11) / +.7027141178 1506947382 0621821539 2 D-9      /
      DATA LN22CS( 12) / -.9674859550 1343423892 4397200513 7 D-10     /
      DATA LN22CS( 13) / +.1338104645 9248873065 8849644974 8 D-10     /
      DATA LN22CS( 14) / -.1858102603 5340639816 2845384659 1 D-11     /
      DATA LN22CS( 15) / +.2589294422 5279197493 0860012307 0 D-12     /
      DATA LN22CS( 16) / -.3619568316 1415886744 6602538217 2 D-13     /
      DATA LN22CS( 17) / +.5074037398 0166230880 0685891739 6 D-14     /
      DATA LN22CS( 18) / -.7131012977 0311273027 0093874892 7 D-15     /
      DATA LN22CS( 19) / +.1004490328 5545674818 5338678412 6 D-15     /
      DATA LN22CS( 20) / -.1417906532 1840257919 0440507528 5 D-16     /
      DATA LN22CS( 21) / +.2005297034 7433261178 9108639607 4 D-17     /
      DATA LN22CS( 22) / -.2840996662 3398033053 6539671756 7 D-18     /
      DATA LN22CS( 23) / +.4031469883 9690798995 9987866282 6 D-19     /
      DATA LN22CS( 24) / -.5729325241 8322073204 5549895679 9 D-20     /
      DATA LN22CS( 25) / +.8153488253 8900106758 4892873386 6 D-21     /
      DATA LN22CS( 26) / -.1161825588 5497217876 0602746879 9 D-21     /
      DATA LN22CS( 27) / +.1657516611 6625383436 5933977599 9 D-22     /
      DATA LN22CS( 28) / -.2367336704 7108051901 1401728000 0 D-23     /
      DATA LN22CS( 29) / +.3384670367 9755213860 7656959999 9 D-24     /
      DATA LN22CS( 30) / -.4843940829 2157182042 9639679999 9 D-25     /
      DATA LN22CS( 31) / +.6938759162 5142737186 7613866666 6 D-26     /
      DATA LN22CS( 32) / -.9948142607 0314365719 2379733333 3 D-27     /
      DATA LN22CS( 33) / +.1427440611 2116986106 3475200000 0 D-27     /
      DATA LN22CS( 34) / -.2049794721 8982349115 6650666666 6 D-28     /
      DATA LN22CS( 35) / +.2945648756 4013622228 8554666666 6 D-29     /
      DATA LN22CS( 36) / -.4235973185 1849570276 6933333333 3 D-30     /
      DATA LN22CS( 37) / +.6095532614 0038320401 0666666666 6 D-31     /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  D9LN2R
      IF (FIRST) THEN
         EPS = D1MACH(3)
         NTLN21 = INITDS (LN21CS, 50, 0.1*EPS)
         NTLN22 = INITDS (LN22CS, 37, 0.1*EPS)
C
         XMIN = -1.0D0 + SQRT(D1MACH(4))
         SQEPS = SQRT (EPS)
         TXMAX = 8.0/SQEPS
         XMAX = TXMAX - (EPS*TXMAX**2 - 2.D0*LOG(TXMAX))
     1     / (2.D0*EPS*TXMAX)
         TXBIG = 6.0/SQRT(SQEPS)
         XBIG = TXBIG - (SQEPS*TXBIG**2 - 2.D0*LOG(TXBIG))
     1     / (2.D0*SQEPS*TXBIG)
      ENDIF
      FIRST = .FALSE.
C
      IF (X.LT.(-.625D0) .OR. X.GT.0.8125D0) GO TO 20
C
      IF (X.LT.0.0D0) D9LN2R = 0.375D0 + DCSEVL (16.D0*X/5.D0+1.D0,
     1  LN21CS, NTLN21)
      IF (X.GE.0.0D0) D9LN2R = 0.375D0 + DCSEVL (32.D0*X/13.D0-1.D0,
     1  LN22CS, NTLN22)
      RETURN
C
 20   IF (X .LT. XMIN) CALL XERMSG ('SLATEC', 'D9LN2R',
     +   'ANSWER LT HALF PRECISION BECAUSE X IS TOO NEAR -1', 1, 1)
      IF (X .GT. XMAX) CALL XERMSG ('SLATEC', 'D9LN2R',
     +   'NO PRECISION IN ANSWER BECAUSE X IS TOO BIG', 3, 2)
      IF (X .GT. XBIG) CALL XERMSG ('SLATEC', 'D9LN2R',
     +   'ANSWER LT HALF PRECISION BECAUSE X IS TOO BIG', 2, 1)
C
      D9LN2R = (LOG(1.D0+X) - X*(1.D0 - 0.5D0*X)) / X**3
      RETURN
C
      END
*DECK DCSEVL
      DOUBLE PRECISION FUNCTION DCSEVL (X, CS, N)
C***BEGIN PROLOGUE  DCSEVL
C***PURPOSE  Evaluate a Chebyshev series.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C3A2
C***TYPE      DOUBLE PRECISION (CSEVL-S, DCSEVL-D)
C***KEYWORDS  CHEBYSHEV SERIES, FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C  Evaluate the N-term Chebyshev series CS at X.  Adapted from
C  a method presented in the paper by Broucke referenced below.
C
C       Input Arguments --
C  X    value at which the series is to be evaluated.
C  CS   array of N terms of a Chebyshev series.  In evaluating
C       CS, only half the first coefficient is summed.
C  N    number of terms in array CS.
C
C***REFERENCES  R. Broucke, Ten subroutines for the manipulation of
C                 Chebyshev series, Algorithm 446, Communications of
C                 the A.C.M. 16, (1973) pp. 254-256.
C               L. Fox and I. B. Parker, Chebyshev Polynomials in
C                 Numerical Analysis, Oxford University Press, 1968,
C                 page 56.
C***ROUTINES CALLED  D1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900329  Prologued revised extensively and code rewritten to allow
C           X to be slightly outside interval (-1,+1).  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DCSEVL
      DOUBLE PRECISION B0, B1, B2, CS(*), ONEPL, TWOX, X, D1MACH
      LOGICAL FIRST
      SAVE FIRST, ONEPL
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DCSEVL
      IF (FIRST) ONEPL = 1.0D0 + D1MACH(4)
      FIRST = .FALSE.
      IF (N .LT. 1) CALL XERMSG ('SLATEC', 'DCSEVL',
     +   'NUMBER OF TERMS .LE. 0', 2, 2)
      IF (N .GT. 1000) CALL XERMSG ('SLATEC', 'DCSEVL',
     +   'NUMBER OF TERMS .GT. 1000', 3, 2)
      IF (ABS(X) .GT. ONEPL) CALL XERMSG ('SLATEC', 'DCSEVL',
     +   'X OUTSIDE THE INTERVAL (-1,+1)', 1, 1)
C
      B1 = 0.0D0
      B0 = 0.0D0
      TWOX = 2.0D0*X
      DO 10 I = 1,N
         B2 = B1
         B1 = B0
         NI = N + 1 - I
         B0 = TWOX*B1 - B2 + CS(NI)
   10 CONTINUE
C
      DCSEVL = 0.5D0*(B0-B2)
C
      RETURN
      END
*DECK D1MACH
      DOUBLE PRECISION FUNCTION D1MACH (I)
C***BEGIN PROLOGUE  D1MACH
C***PURPOSE  Return floating point machine dependent constants.
C***LIBRARY   SLATEC
C***CATEGORY  R1
C***TYPE      DOUBLE PRECISION (R1MACH-S, D1MACH-D)
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  Fox, P. A., (Bell Labs)
C           Hall, A. D., (Bell Labs)
C           Schryer, N. L., (Bell Labs)
C***DESCRIPTION
C
C   D1MACH can be used to obtain machine-dependent parameters for the
C   local machine environment.  It is a function subprogram with one
C   (input) argument, and can be referenced as follows:
C
C        D = D1MACH(I)
C
C   where I=1,...,5.  The (output) value of D above is determined by
C   the (input) value of I.  The results for various values of I are
C   discussed below.
C
C   D1MACH( 1) = B**(EMIN-1), the smallest positive magnitude.
C   D1MACH( 2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
C   D1MACH( 3) = B**(-T), the smallest relative spacing.
C   D1MACH( 4) = B**(1-T), the largest relative spacing.
C   D1MACH( 5) = LOG10(B)
C
C   Assume double precision numbers are represented in the T-digit,
C   base-B form
C
C              sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C   where 0 .LE. X(I) .LT. B for I=1,...,T, 0 .LT. X(1), and
C   EMIN .LE. E .LE. EMAX.
C
C   The values of B, T, EMIN and EMAX are provided in I1MACH as
C   follows:
C   I1MACH(10) = B, the base.
C   I1MACH(14) = T, the number of base-B digits.
C   I1MACH(15) = EMIN, the smallest exponent E.
C   I1MACH(16) = EMAX, the largest exponent E.
C
C   To alter this function for a particular environment, the desired
C   set of DATA statements should be activated by removing the C from
C   column 1.  Also, the values of D1MACH(1) - D1MACH(4) should be
C   checked for consistency with the local operating system.
C
C***REFERENCES  P. A. Fox, A. D. Hall and N. L. Schryer, Framework for
C                 a portable library, ACM Transactions on Mathematical
C                 Software 4, 2 (June 1978), pp. 177-188.
C***ROUTINES CALLED  XERMSG
C***REVISION HISTORY  (YYMMDD)
C   750101  DATE WRITTEN
C   890213  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900618  Added DEC RISC constants.  (WRB)
C   900723  Added IBM RS 6000 constants.  (WRB)
C   900911  Added SUN 386i constants.  (WRB)
C   910710  Added HP 730 constants.  (SMR)
C   911114  Added Convex IEEE constants.  (WRB)
C   920121  Added SUN -r8 compiler option constants.  (WRB)
C   920229  Added Touchstone Delta i860 constants.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C   920625  Added CONVEX -p8 and -pd8 compiler option constants.
C           (BKS, WRB)
C   930201  Added DEC Alpha and SGI constants.  (RWC and WRB)
C***END PROLOGUE  D1MACH
C
C
	external dlamch
      DOUBLE PRECISION DMACH(5),dlamch
	integer iflag

      SAVE DMACH,iflag
	data iflag/0/
C
C
C     MACHINE CONSTANTS FOR THE AMIGA
C     ABSOFT FORTRAN COMPILER USING THE 68020/68881 COMPILER OPTION
C
C     DATA SMALL(1), SMALL(2) / Z'00100000', Z'00000000' /
C     DATA LARGE(1), LARGE(2) / Z'7FEFFFFF', Z'FFFFFFFF' /
C     DATA RIGHT(1), RIGHT(2) / Z'3CA00000', Z'00000000' /
C     DATA DIVER(1), DIVER(2) / Z'3CB00000', Z'00000000' /
C     DATA LOG10(1), LOG10(2) / Z'3FD34413', Z'509F79FF' /
C
C     MACHINE CONSTANTS FOR THE AMIGA
C     ABSOFT FORTRAN COMPILER USING SOFTWARE FLOATING POINT
C
C     DATA SMALL(1), SMALL(2) / Z'00100000', Z'00000000' /
C     DATA LARGE(1), LARGE(2) / Z'7FDFFFFF', Z'FFFFFFFF' /
C     DATA RIGHT(1), RIGHT(2) / Z'3CA00000', Z'00000000' /
C     DATA DIVER(1), DIVER(2) / Z'3CB00000', Z'00000000' /
C     DATA LOG10(1), LOG10(2) / Z'3FD34413', Z'509F79FF' /
C
C     MACHINE CONSTANTS FOR THE APOLLO
C
C     DATA SMALL(1), SMALL(2) / 16#00100000, 16#00000000 /
C     DATA LARGE(1), LARGE(2) / 16#7FFFFFFF, 16#FFFFFFFF /
C     DATA RIGHT(1), RIGHT(2) / 16#3CA00000, 16#00000000 /
C     DATA DIVER(1), DIVER(2) / 16#3CB00000, 16#00000000 /
C     DATA LOG10(1), LOG10(2) / 16#3FD34413, 16#509F79FF /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM
C
C     DATA SMALL(1) / ZC00800000 /
C     DATA SMALL(2) / Z000000000 /
C     DATA LARGE(1) / ZDFFFFFFFF /
C     DATA LARGE(2) / ZFFFFFFFFF /
C     DATA RIGHT(1) / ZCC5800000 /
C     DATA RIGHT(2) / Z000000000 /
C     DATA DIVER(1) / ZCC6800000 /
C     DATA DIVER(2) / Z000000000 /
C     DATA LOG10(1) / ZD00E730E7 /
C     DATA LOG10(2) / ZC77800DC0 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM
C
C     DATA SMALL(1) / O1771000000000000 /
C     DATA SMALL(2) / O0000000000000000 /
C     DATA LARGE(1) / O0777777777777777 /
C     DATA LARGE(2) / O0007777777777777 /
C     DATA RIGHT(1) / O1461000000000000 /
C     DATA RIGHT(2) / O0000000000000000 /
C     DATA DIVER(1) / O1451000000000000 /
C     DATA DIVER(2) / O0000000000000000 /
C     DATA LOG10(1) / O1157163034761674 /
C     DATA LOG10(2) / O0006677466732724 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS
C
C     DATA SMALL(1) / O1771000000000000 /
C     DATA SMALL(2) / O7770000000000000 /
C     DATA LARGE(1) / O0777777777777777 /
C     DATA LARGE(2) / O7777777777777777 /
C     DATA RIGHT(1) / O1461000000000000 /
C     DATA RIGHT(2) / O0000000000000000 /
C     DATA DIVER(1) / O1451000000000000 /
C     DATA DIVER(2) / O0000000000000000 /
C     DATA LOG10(1) / O1157163034761674 /
C     DATA LOG10(2) / O0006677466732724 /
C
C     MACHINE CONSTANTS FOR THE CDC 170/180 SERIES USING NOS/VE
C
C     DATA SMALL(1) / Z"3001800000000000" /
C     DATA SMALL(2) / Z"3001000000000000" /
C     DATA LARGE(1) / Z"4FFEFFFFFFFFFFFE" /
C     DATA LARGE(2) / Z"4FFE000000000000" /
C     DATA RIGHT(1) / Z"3FD2800000000000" /
C     DATA RIGHT(2) / Z"3FD2000000000000" /
C     DATA DIVER(1) / Z"3FD3800000000000" /
C     DATA DIVER(2) / Z"3FD3000000000000" /
C     DATA LOG10(1) / Z"3FFF9A209A84FBCF" /
C     DATA LOG10(2) / Z"3FFFF7988F8959AC" /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES
C
C     DATA SMALL(1) / 00564000000000000000B /
C     DATA SMALL(2) / 00000000000000000000B /
C     DATA LARGE(1) / 37757777777777777777B /
C     DATA LARGE(2) / 37157777777777777777B /
C     DATA RIGHT(1) / 15624000000000000000B /
C     DATA RIGHT(2) / 00000000000000000000B /
C     DATA DIVER(1) / 15634000000000000000B /
C     DATA DIVER(2) / 00000000000000000000B /
C     DATA LOG10(1) / 17164642023241175717B /
C     DATA LOG10(2) / 16367571421742254654B /
C
C     MACHINE CONSTANTS FOR THE CELERITY C1260
C
C     DATA SMALL(1), SMALL(2) / Z'00100000', Z'00000000' /
C     DATA LARGE(1), LARGE(2) / Z'7FEFFFFF', Z'FFFFFFFF' /
C     DATA RIGHT(1), RIGHT(2) / Z'3CA00000', Z'00000000' /
C     DATA DIVER(1), DIVER(2) / Z'3CB00000', Z'00000000' /
C     DATA LOG10(1), LOG10(2) / Z'3FD34413', Z'509F79FF' /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -fn OR -pd8 COMPILER OPTION
C
C     DATA DMACH(1) / Z'0010000000000000' /
C     DATA DMACH(2) / Z'7FFFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3CC0000000000000' /
C     DATA DMACH(4) / Z'3CD0000000000000' /
C     DATA DMACH(5) / Z'3FF34413509F79FF' /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -fi COMPILER OPTION
C
C     DATA DMACH(1) / Z'0010000000000000' /
C     DATA DMACH(2) / Z'7FEFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3CA0000000000000' /
C     DATA DMACH(4) / Z'3CB0000000000000' /
C     DATA DMACH(5) / Z'3FD34413509F79FF' /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -p8 COMPILER OPTION
C
C     DATA DMACH(1) / Z'00010000000000000000000000000000' /
C     DATA DMACH(2) / Z'7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3F900000000000000000000000000000' /
C     DATA DMACH(4) / Z'3F910000000000000000000000000000' /
C     DATA DMACH(5) / Z'3FFF34413509F79FEF311F12B35816F9' /
C
C     MACHINE CONSTANTS FOR THE CRAY
C
C     DATA SMALL(1) / 201354000000000000000B /
C     DATA SMALL(2) / 000000000000000000000B /
C     DATA LARGE(1) / 577767777777777777777B /
C     DATA LARGE(2) / 000007777777777777774B /
C     DATA RIGHT(1) / 376434000000000000000B /
C     DATA RIGHT(2) / 000000000000000000000B /
C     DATA DIVER(1) / 376444000000000000000B /
C     DATA DIVER(2) / 000000000000000000000B /
C     DATA LOG10(1) / 377774642023241175717B /
C     DATA LOG10(2) / 000007571421742254654B /
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
C     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING CARD -
C     STATIC DMACH(5)
C
C     DATA SMALL /    20K, 3*0 /
C     DATA LARGE / 77777K, 3*177777K /
C     DATA RIGHT / 31420K, 3*0 /
C     DATA DIVER / 32020K, 3*0 /
C     DATA LOG10 / 40423K, 42023K, 50237K, 74776K /
C
C     MACHINE CONSTANTS FOR THE DEC ALPHA
C     USING G_FLOAT
C
C     DATA DMACH(1) / '0000000000000010'X /
C     DATA DMACH(2) / 'FFFFFFFFFFFF7FFF'X /
C     DATA DMACH(3) / '0000000000003CC0'X /
C     DATA DMACH(4) / '0000000000003CD0'X /
C     DATA DMACH(5) / '79FF509F44133FF3'X /
C
C     MACHINE CONSTANTS FOR THE DEC ALPHA
C     USING IEEE_FORMAT
C
C     DATA DMACH(1) / '0010000000000000'X /
C     DATA DMACH(2) / '7FEFFFFFFFFFFFFF'X /
C     DATA DMACH(3) / '3CA0000000000000'X /
C     DATA DMACH(4) / '3CB0000000000000'X /
C     DATA DMACH(5) / '3FD34413509F79FF'X /
C
C     MACHINE CONSTANTS FOR THE DEC RISC
C
C     DATA SMALL(1), SMALL(2) / Z'00000000', Z'00100000'/
C     DATA LARGE(1), LARGE(2) / Z'FFFFFFFF', Z'7FEFFFFF'/
C     DATA RIGHT(1), RIGHT(2) / Z'00000000', Z'3CA00000'/
C     DATA DIVER(1), DIVER(2) / Z'00000000', Z'3CB00000'/
C     DATA LOG10(1), LOG10(2) / Z'509F79FF', Z'3FD34413'/
C
C     MACHINE CONSTANTS FOR THE DEC VAX
C     USING D_FLOATING
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)
C     THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSTEMS
C     THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS
C
C     DATA SMALL(1), SMALL(2) /        128,           0 /
C     DATA LARGE(1), LARGE(2) /     -32769,          -1 /
C     DATA RIGHT(1), RIGHT(2) /       9344,           0 /
C     DATA DIVER(1), DIVER(2) /       9472,           0 /
C     DATA LOG10(1), LOG10(2) /  546979738,  -805796613 /
C
C     DATA SMALL(1), SMALL(2) / Z00000080, Z00000000 /
C     DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C     DATA RIGHT(1), RIGHT(2) / Z00002480, Z00000000 /
C     DATA DIVER(1), DIVER(2) / Z00002500, Z00000000 /
C     DATA LOG10(1), LOG10(2) / Z209A3F9A, ZCFF884FB /
C
C     MACHINE CONSTANTS FOR THE DEC VAX
C     USING G_FLOATING
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)
C     THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSTEMS
C     THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS
C
C     DATA SMALL(1), SMALL(2) /         16,           0 /
C     DATA LARGE(1), LARGE(2) /     -32769,          -1 /
C     DATA RIGHT(1), RIGHT(2) /      15552,           0 /
C     DATA DIVER(1), DIVER(2) /      15568,           0 /
C     DATA LOG10(1), LOG10(2) /  1142112243, 2046775455 /
C
C     DATA SMALL(1), SMALL(2) / Z00000010, Z00000000 /
C     DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C     DATA RIGHT(1), RIGHT(2) / Z00003CC0, Z00000000 /
C     DATA DIVER(1), DIVER(2) / Z00003CD0, Z00000000 /
C     DATA LOG10(1), LOG10(2) / Z44133FF3, Z79FF509F /
C
C     MACHINE CONSTANTS FOR THE ELXSI 6400
C     (ASSUMING REAL*8 IS THE DEFAULT DOUBLE PRECISION)
C
C     DATA SMALL(1), SMALL(2) / '00100000'X,'00000000'X /
C     DATA LARGE(1), LARGE(2) / '7FEFFFFF'X,'FFFFFFFF'X /
C     DATA RIGHT(1), RIGHT(2) / '3CB00000'X,'00000000'X /
C     DATA DIVER(1), DIVER(2) / '3CC00000'X,'00000000'X /
C     DATA LOG10(1), LOG10(2) / '3FD34413'X,'509F79FF'X /
C
C     MACHINE CONSTANTS FOR THE HARRIS 220
C
C     DATA SMALL(1), SMALL(2) / '20000000, '00000201 /
C     DATA LARGE(1), LARGE(2) / '37777777, '37777577 /
C     DATA RIGHT(1), RIGHT(2) / '20000000, '00000333 /
C     DATA DIVER(1), DIVER(2) / '20000000, '00000334 /
C     DATA LOG10(1), LOG10(2) / '23210115, '10237777 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES
C
C     DATA SMALL(1), SMALL(2) / O402400000000, O000000000000 /
C     DATA LARGE(1), LARGE(2) / O376777777777, O777777777777 /
C     DATA RIGHT(1), RIGHT(2) / O604400000000, O000000000000 /
C     DATA DIVER(1), DIVER(2) / O606400000000, O000000000000 /
C     DATA LOG10(1), LOG10(2) / O776464202324, O117571775714 /
C
C     MACHINE CONSTANTS FOR THE HP 730
C
C     DATA DMACH(1) / Z'0010000000000000' /
C     DATA DMACH(2) / Z'7FEFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3CA0000000000000' /
C     DATA DMACH(4) / Z'3CB0000000000000' /
C     DATA DMACH(5) / Z'3FD34413509F79FF' /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     THREE WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA SMALL(1), SMALL(2), SMALL(3) / 40000B,       0,       1 /
C     DATA LARGE(1), LARGE(2), LARGE(3) / 77777B, 177777B, 177776B /
C     DATA RIGHT(1), RIGHT(2), RIGHT(3) / 40000B,       0,    265B /
C     DATA DIVER(1), DIVER(2), DIVER(3) / 40000B,       0,    276B /
C     DATA LOG10(1), LOG10(2), LOG10(3) / 46420B,  46502B,  77777B /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     FOUR WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA SMALL(1), SMALL(2) /  40000B,       0 /
C     DATA SMALL(3), SMALL(4) /       0,       1 /
C     DATA LARGE(1), LARGE(2) /  77777B, 177777B /
C     DATA LARGE(3), LARGE(4) / 177777B, 177776B /
C     DATA RIGHT(1), RIGHT(2) /  40000B,       0 /
C     DATA RIGHT(3), RIGHT(4) /       0,    225B /
C     DATA DIVER(1), DIVER(2) /  40000B,       0 /
C     DATA DIVER(3), DIVER(4) /       0,    227B /
C     DATA LOG10(1), LOG10(2) /  46420B,  46502B /
C     DATA LOG10(3), LOG10(4) /  76747B, 176377B /
C
C     MACHINE CONSTANTS FOR THE HP 9000
C
C     DATA SMALL(1), SMALL(2) / 00040000000B, 00000000000B /
C     DATA LARGE(1), LARGE(2) / 17737777777B, 37777777777B /
C     DATA RIGHT(1), RIGHT(2) / 07454000000B, 00000000000B /
C     DATA DIVER(1), DIVER(2) / 07460000000B, 00000000000B /
C     DATA LOG10(1), LOG10(2) / 07764642023B, 12047674777B /
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86, AND
C     THE PERKIN ELMER (INTERDATA) 7/32.
C
C     DATA SMALL(1), SMALL(2) / Z00100000, Z00000000 /
C     DATA LARGE(1), LARGE(2) / Z7FFFFFFF, ZFFFFFFFF /
C     DATA RIGHT(1), RIGHT(2) / Z33100000, Z00000000 /
C     DATA DIVER(1), DIVER(2) / Z34100000, Z00000000 /
C     DATA LOG10(1), LOG10(2) / Z41134413, Z509F79FF /
C
C     MACHINE CONSTANTS FOR THE IBM PC
C     ASSUMES THAT ALL ARITHMETIC IS DONE IN DOUBLE PRECISION
C     ON 8088, I.E., NOT IN 80 BIT FORM FOR THE 8087.
C
C     DATA SMALL(1) / 2.23D-308  /
C     DATA LARGE(1) / 1.79D+308  /
C     DATA RIGHT(1) / 1.11D-16   /
C     DATA DIVER(1) / 2.22D-16   /
C     DATA LOG10(1) / 0.301029995663981195D0 /
C
C     MACHINE CONSTANTS FOR THE IBM RS 6000
C
C     DATA DMACH(1) / Z'0010000000000000' /
C     DATA DMACH(2) / Z'7FEFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3CA0000000000000' /
C     DATA DMACH(4) / Z'3CB0000000000000' /
C     DATA DMACH(5) / Z'3FD34413509F79FF' /
C
C     MACHINE CONSTANTS FOR THE INTEL i860
C
C     DATA DMACH(1) / Z'0010000000000000' /
C     DATA DMACH(2) / Z'7FEFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3CA0000000000000' /
C     DATA DMACH(4) / Z'3CB0000000000000' /
C     DATA DMACH(5) / Z'3FD34413509F79FF' /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR)
C
C     DATA SMALL(1), SMALL(2) / "033400000000, "000000000000 /
C     DATA LARGE(1), LARGE(2) / "377777777777, "344777777777 /
C     DATA RIGHT(1), RIGHT(2) / "113400000000, "000000000000 /
C     DATA DIVER(1), DIVER(2) / "114400000000, "000000000000 /
C     DATA LOG10(1), LOG10(2) / "177464202324, "144117571776 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR)
C
C     DATA SMALL(1), SMALL(2) / "000400000000, "000000000000 /
C     DATA LARGE(1), LARGE(2) / "377777777777, "377777777777 /
C     DATA RIGHT(1), RIGHT(2) / "103400000000, "000000000000 /
C     DATA DIVER(1), DIVER(2) / "104400000000, "000000000000 /
C     DATA LOG10(1), LOG10(2) / "177464202324, "476747767461 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C     DATA SMALL(1), SMALL(2) /    8388608,           0 /
C     DATA LARGE(1), LARGE(2) / 2147483647,          -1 /
C     DATA RIGHT(1), RIGHT(2) /  612368384,           0 /
C     DATA DIVER(1), DIVER(2) /  620756992,           0 /
C     DATA LOG10(1), LOG10(2) / 1067065498, -2063872008 /
C
C     DATA SMALL(1), SMALL(2) / O00040000000, O00000000000 /
C     DATA LARGE(1), LARGE(2) / O17777777777, O37777777777 /
C     DATA RIGHT(1), RIGHT(2) / O04440000000, O00000000000 /
C     DATA DIVER(1), DIVER(2) / O04500000000, O00000000000 /
C     DATA LOG10(1), LOG10(2) / O07746420232, O20476747770 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     16-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C     DATA SMALL(1), SMALL(2) /    128,      0 /
C     DATA SMALL(3), SMALL(4) /      0,      0 /
C     DATA LARGE(1), LARGE(2) /  32767,     -1 /
C     DATA LARGE(3), LARGE(4) /     -1,     -1 /
C     DATA RIGHT(1), RIGHT(2) /   9344,      0 /
C     DATA RIGHT(3), RIGHT(4) /      0,      0 /
C     DATA DIVER(1), DIVER(2) /   9472,      0 /
C     DATA DIVER(3), DIVER(4) /      0,      0 /
C     DATA LOG10(1), LOG10(2) /  16282,   8346 /
C     DATA LOG10(3), LOG10(4) / -31493, -12296 /
C
C     DATA SMALL(1), SMALL(2) / O000200, O000000 /
C     DATA SMALL(3), SMALL(4) / O000000, O000000 /
C     DATA LARGE(1), LARGE(2) / O077777, O177777 /
C     DATA LARGE(3), LARGE(4) / O177777, O177777 /
C     DATA RIGHT(1), RIGHT(2) / O022200, O000000 /
C     DATA RIGHT(3), RIGHT(4) / O000000, O000000 /
C     DATA DIVER(1), DIVER(2) / O022400, O000000 /
C     DATA DIVER(3), DIVER(4) / O000000, O000000 /
C     DATA LOG10(1), LOG10(2) / O037632, O020232 /
C     DATA LOG10(3), LOG10(4) / O102373, O147770 /
C
C     MACHINE CONSTANTS FOR THE SILICON GRAPHICS
C
C     DATA SMALL(1), SMALL(2) / Z'00100000', Z'00000000' /
C     DATA LARGE(1), LARGE(2) / Z'7FEFFFFF', Z'FFFFFFFF' /
C     DATA RIGHT(1), RIGHT(2) / Z'3CA00000', Z'00000000' /
C     DATA DIVER(1), DIVER(2) / Z'3CB00000', Z'00000000' /
C     DATA LOG10(1), LOG10(2) / Z'3FD34413', Z'509F79FF' /
C
C     MACHINE CONSTANTS FOR THE SUN
C
C     DATA DMACH(1) / Z'0010000000000000' /
C     DATA DMACH(2) / Z'7FEFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3CA0000000000000' /
C     DATA DMACH(4) / Z'3CB0000000000000' /
C     DATA DMACH(5) / Z'3FD34413509F79FF' /
C
C     MACHINE CONSTANTS FOR THE SUN
C     USING THE -r8 COMPILER OPTION
C
C     DATA DMACH(1) / Z'00010000000000000000000000000000' /
C     DATA DMACH(2) / Z'7FFEFFFFFFFFFFFFFFFFFFFFFFFFFFFF' /
C     DATA DMACH(3) / Z'3F8E0000000000000000000000000000' /
C     DATA DMACH(4) / Z'3F8F0000000000000000000000000000' /
C     DATA DMACH(5) / Z'3FFD34413509F79FEF311F12B35816F9' /
C
C     MACHINE CONSTANTS FOR THE SUN 386i
C
C     DATA SMALL(1), SMALL(2) / Z'FFFFFFFD', Z'000FFFFF' /
C     DATA LARGE(1), LARGE(2) / Z'FFFFFFB0', Z'7FEFFFFF' /
C     DATA RIGHT(1), RIGHT(2) / Z'000000B0', Z'3CA00000' /
C     DATA DIVER(1), DIVER(2) / Z'FFFFFFCB', Z'3CAFFFFF'
C     DATA LOG10(1), LOG10(2) / Z'509F79E9', Z'3FD34413' /
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES FTN COMPILER
C
C     DATA SMALL(1), SMALL(2) / O000040000000, O000000000000 /
C     DATA LARGE(1), LARGE(2) / O377777777777, O777777777777 /
C     DATA RIGHT(1), RIGHT(2) / O170540000000, O000000000000 /
C     DATA DIVER(1), DIVER(2) / O170640000000, O000000000000 /
C     DATA LOG10(1), LOG10(2) / O177746420232, O411757177572 /
C
C***FIRST EXECUTABLE STATEMENT  D1MACH
      IF (I .LT. 1 .OR. I .GT. 5) CALL XERMSG ('SLATEC', 'D1MACH',
     +   'I OUT OF BOUNDS', 1, 2)
C
	if (iflag.eq.0) then
	   iflag=1
	   dmach(1)=dlamch('u')
	   dmach(2)=dlamch('o')
	   dmach(3)=dlamch('e')
	   dmach(4)=dlamch('p')
	   dmach(5)=dlamch('b')
	   dmach(5)=log10(dmach(5))
	endif
      D1MACH = DMACH(I)
      RETURN
C
      END
*DECK I1MACH
      INTEGER FUNCTION I1MACH (I)
C***BEGIN PROLOGUE  I1MACH
C***PURPOSE  Return integer machine dependent constants.
C***LIBRARY   SLATEC
C***CATEGORY  R1
C***TYPE      INTEGER (I1MACH-I)
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  Fox, P. A., (Bell Labs)
C           Hall, A. D., (Bell Labs)
C           Schryer, N. L., (Bell Labs)
C***DESCRIPTION
C
C   I1MACH can be used to obtain machine-dependent parameters for the
C   local machine environment.  It is a function subprogram with one
C   (input) argument and can be referenced as follows:
C
C        K = I1MACH(I)
C
C   where I=1,...,16.  The (output) value of K above is determined by
C   the (input) value of I.  The results for various values of I are
C   discussed below.
C
C   I/O unit numbers:
C     I1MACH( 1) = the standard input unit.
C     I1MACH( 2) = the standard output unit.
C     I1MACH( 3) = the standard punch unit.
C     I1MACH( 4) = the standard error message unit.
C
C   Words:
C     I1MACH( 5) = the number of bits per integer storage unit.
C     I1MACH( 6) = the number of characters per integer storage unit.
C
C   Integers:
C     assume integers are represented in the S-digit, base-A form
C
C                sign ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C
C                where 0 .LE. X(I) .LT. A for I=0,...,S-1.
C     I1MACH( 7) = A, the base.
C     I1MACH( 8) = S, the number of base-A digits.
C     I1MACH( 9) = A**S - 1, the largest magnitude.
C
C   Floating-Point Numbers:
C     Assume floating-point numbers are represented in the T-digit,
C     base-B form
C                sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C                where 0 .LE. X(I) .LT. B for I=1,...,T,
C                0 .LT. X(1), and EMIN .LE. E .LE. EMAX.
C     I1MACH(10) = B, the base.
C
C   Single-Precision:
C     I1MACH(11) = T, the number of base-B digits.
C     I1MACH(12) = EMIN, the smallest exponent E.
C     I1MACH(13) = EMAX, the largest exponent E.
C
C   Double-Precision:
C     I1MACH(14) = T, the number of base-B digits.
C     I1MACH(15) = EMIN, the smallest exponent E.
C     I1MACH(16) = EMAX, the largest exponent E.
C
C   To alter this function for a particular environment, the desired
C   set of DATA statements should be activated by removing the C from
C   column 1.  Also, the values of I1MACH(1) - I1MACH(4) should be
C   checked for consistency with the local operating system.
C
C***REFERENCES  P. A. Fox, A. D. Hall and N. L. Schryer, Framework for
C                 a portable library, ACM Transactions on Mathematical
C                 Software 4, 2 (June 1978), pp. 177-188.
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   750101  DATE WRITTEN
C   891012  Added VAX G-floating constants.  (WRB)
C   891012  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900618  Added DEC RISC constants.  (WRB)
C   900723  Added IBM RS 6000 constants.  (WRB)
C   901009  Correct I1MACH(7) for IBM Mainframes. Should be 2 not 16.
C           (RWC)
C   910710  Added HP 730 constants.  (SMR)
C   911114  Added Convex IEEE constants.  (WRB)
C   920121  Added SUN -r8 compiler option constants.  (WRB)
C   920229  Added Touchstone Delta i860 constants.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C   920625  Added Convex -p8 and -pd8 compiler option constants.
C           (BKS, WRB)
C   930201  Added DEC Alpha and SGI constants.  (RWC and WRB)
C   930618  Corrected I1MACH(5) for Convex -p8 and -pd8 compiler
C           options.  (DWL, RWC and WRB).
C***END PROLOGUE  I1MACH
C
      INTEGER IMACH(16),OUTPUT
      SAVE IMACH
      EQUIVALENCE (IMACH(4),OUTPUT)
C
C     MACHINE CONSTANTS FOR THE AMIGA
C     ABSOFT COMPILER
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -126 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1022 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE APOLLO
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        129 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1025 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM
C
C     DATA IMACH( 1) /          7 /
C     DATA IMACH( 2) /          2 /
C     DATA IMACH( 3) /          2 /
C     DATA IMACH( 4) /          2 /
C     DATA IMACH( 5) /         36 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         33 /
C     DATA IMACH( 9) / Z1FFFFFFFF /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -256 /
C     DATA IMACH(13) /        255 /
C     DATA IMACH(14) /         60 /
C     DATA IMACH(15) /       -256 /
C     DATA IMACH(16) /        255 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         48 /
C     DATA IMACH( 6) /          6 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         39 /
C     DATA IMACH( 9) / O0007777777777777 /
C     DATA IMACH(10) /          8 /
C     DATA IMACH(11) /         13 /
C     DATA IMACH(12) /        -50 /
C     DATA IMACH(13) /         76 /
C     DATA IMACH(14) /         26 /
C     DATA IMACH(15) /        -50 /
C     DATA IMACH(16) /         76 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         48 /
C     DATA IMACH( 6) /          6 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         39 /
C     DATA IMACH( 9) / O0007777777777777 /
C     DATA IMACH(10) /          8 /
C     DATA IMACH(11) /         13 /
C     DATA IMACH(12) /        -50 /
C     DATA IMACH(13) /         76 /
C     DATA IMACH(14) /         26 /
C     DATA IMACH(15) /     -32754 /
C     DATA IMACH(16) /      32780 /
C
C     MACHINE CONSTANTS FOR THE CDC 170/180 SERIES USING NOS/VE
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         64 /
C     DATA IMACH( 6) /          8 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         63 /
C     DATA IMACH( 9) / 9223372036854775807 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         47 /
C     DATA IMACH(12) /      -4095 /
C     DATA IMACH(13) /       4094 /
C     DATA IMACH(14) /         94 /
C     DATA IMACH(15) /      -4095 /
C     DATA IMACH(16) /       4094 /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /    6LOUTPUT/
C     DATA IMACH( 5) /         60 /
C     DATA IMACH( 6) /         10 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         48 /
C     DATA IMACH( 9) / 00007777777777777777B /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         47 /
C     DATA IMACH(12) /       -929 /
C     DATA IMACH(13) /       1070 /
C     DATA IMACH(14) /         94 /
C     DATA IMACH(15) /       -929 /
C     DATA IMACH(16) /       1069 /
C
C     MACHINE CONSTANTS FOR THE CELERITY C1260
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          0 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / Z'7FFFFFFF' /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -126 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1022 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -fn COMPILER OPTION
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1023 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -fi COMPILER OPTION
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -p8 COMPILER OPTION
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         64 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         63 /
C     DATA IMACH( 9) / 9223372036854775807 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         53 /
C     DATA IMACH(12) /      -1023 /
C     DATA IMACH(13) /       1023 /
C     DATA IMACH(14) /        113 /
C     DATA IMACH(15) /     -16383 /
C     DATA IMACH(16) /      16383 /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -pd8 COMPILER OPTION
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         64 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         63 /
C     DATA IMACH( 9) / 9223372036854775807 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         53 /
C     DATA IMACH(12) /      -1023 /
C     DATA IMACH(13) /       1023 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1023 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE CRAY
C     USING THE 46 BIT INTEGER COMPILER OPTION
C
C     DATA IMACH( 1) /        100 /
C     DATA IMACH( 2) /        101 /
C     DATA IMACH( 3) /        102 /
C     DATA IMACH( 4) /        101 /
C     DATA IMACH( 5) /         64 /
C     DATA IMACH( 6) /          8 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         46 /
C     DATA IMACH( 9) / 1777777777777777B /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         47 /
C     DATA IMACH(12) /      -8189 /
C     DATA IMACH(13) /       8190 /
C     DATA IMACH(14) /         94 /
C     DATA IMACH(15) /      -8099 /
C     DATA IMACH(16) /       8190 /
C
C     MACHINE CONSTANTS FOR THE CRAY
C     USING THE 64 BIT INTEGER COMPILER OPTION
C
C     DATA IMACH( 1) /        100 /
C     DATA IMACH( 2) /        101 /
C     DATA IMACH( 3) /        102 /
C     DATA IMACH( 4) /        101 /
C     DATA IMACH( 5) /         64 /
C     DATA IMACH( 6) /          8 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         63 /
C     DATA IMACH( 9) / 777777777777777777777B /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         47 /
C     DATA IMACH(12) /      -8189 /
C     DATA IMACH(13) /       8190 /
C     DATA IMACH(14) /         94 /
C     DATA IMACH(15) /      -8099 /
C     DATA IMACH(16) /       8190 /
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
C
C     DATA IMACH( 1) /         11 /
C     DATA IMACH( 2) /         12 /
C     DATA IMACH( 3) /          8 /
C     DATA IMACH( 4) /         10 /
C     DATA IMACH( 5) /         16 /
C     DATA IMACH( 6) /          2 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         15 /
C     DATA IMACH( 9) /      32767 /
C     DATA IMACH(10) /         16 /
C     DATA IMACH(11) /          6 /
C     DATA IMACH(12) /        -64 /
C     DATA IMACH(13) /         63 /
C     DATA IMACH(14) /         14 /
C     DATA IMACH(15) /        -64 /
C     DATA IMACH(16) /         63 /
C
C     MACHINE CONSTANTS FOR THE DEC ALPHA
C     USING G_FLOAT
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1023 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE DEC ALPHA
C     USING IEEE_FLOAT
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE DEC RISC
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE DEC VAX
C     USING D_FLOATING
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         56 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE DEC VAX
C     USING G_FLOATING
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1023 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE ELXSI 6400
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         32 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -126 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1022 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE HARRIS 220
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          0 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         24 /
C     DATA IMACH( 6) /          3 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         23 /
C     DATA IMACH( 9) /    8388607 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         23 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         38 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /         43 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         36 /
C     DATA IMACH( 6) /          6 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         35 /
C     DATA IMACH( 9) / O377777777777 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         27 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         63 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE HP 730
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     3 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          4 /
C     DATA IMACH( 4) /          1 /
C     DATA IMACH( 5) /         16 /
C     DATA IMACH( 6) /          2 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         15 /
C     DATA IMACH( 9) /      32767 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         23 /
C     DATA IMACH(12) /       -128 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         39 /
C     DATA IMACH(15) /       -128 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     4 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          4 /
C     DATA IMACH( 4) /          1 /
C     DATA IMACH( 5) /         16 /
C     DATA IMACH( 6) /          2 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         15 /
C     DATA IMACH( 9) /      32767 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         23 /
C     DATA IMACH(12) /       -128 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         55 /
C     DATA IMACH(15) /       -128 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE HP 9000
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          7 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         32 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -126 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1015 /
C     DATA IMACH(16) /       1017 /
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86, AND
C     THE PERKIN ELMER (INTERDATA) 7/32.
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) /  Z7FFFFFFF /
C     DATA IMACH(10) /         16 /
C     DATA IMACH(11) /          6 /
C     DATA IMACH(12) /        -64 /
C     DATA IMACH(13) /         63 /
C     DATA IMACH(14) /         14 /
C     DATA IMACH(15) /        -64 /
C     DATA IMACH(16) /         63 /
C
C     MACHINE CONSTANTS FOR THE IBM PC
C
      DATA IMACH( 1) /          5 /
      DATA IMACH( 2) /          6 /
      DATA IMACH( 3) /          0 /
      DATA IMACH( 4) /          0 /
      DATA IMACH( 5) /         32 /
      DATA IMACH( 6) /          4 /
      DATA IMACH( 7) /          2 /
      DATA IMACH( 8) /         31 /
      DATA IMACH( 9) / 2147483647 /
      DATA IMACH(10) /          2 /
      DATA IMACH(11) /         24 /
      DATA IMACH(12) /       -125 /
      DATA IMACH(13) /        127 /
      DATA IMACH(14) /         53 /
      DATA IMACH(15) /      -1021 /
      DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE IBM RS 6000
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          0 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE INTEL i860
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR)
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         36 /
C     DATA IMACH( 6) /          5 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         35 /
C     DATA IMACH( 9) / "377777777777 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         27 /
C     DATA IMACH(12) /       -128 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         54 /
C     DATA IMACH(15) /       -101 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR)
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         36 /
C     DATA IMACH( 6) /          5 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         35 /
C     DATA IMACH( 9) / "377777777777 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         27 /
C     DATA IMACH(12) /       -128 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         62 /
C     DATA IMACH(15) /       -128 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     32-BIT INTEGER ARITHMETIC.
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         56 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     16-BIT INTEGER ARITHMETIC.
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         16 /
C     DATA IMACH( 6) /          2 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         15 /
C     DATA IMACH( 9) /      32767 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         56 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE SILICON GRAPHICS
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE SUN
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE SUN
C     USING THE -r8 COMPILER OPTION
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         53 /
C     DATA IMACH(12) /      -1021 /
C     DATA IMACH(13) /       1024 /
C     DATA IMACH(14) /        113 /
C     DATA IMACH(15) /     -16381 /
C     DATA IMACH(16) /      16384 /
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES FTN COMPILER
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          1 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         36 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         35 /
C     DATA IMACH( 9) / O377777777777 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         27 /
C     DATA IMACH(12) /       -128 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         60 /
C     DATA IMACH(15) /      -1024 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE Z80 MICROPROCESSOR
C
C     DATA IMACH( 1) /          1 /
C     DATA IMACH( 2) /          1 /
C     DATA IMACH( 3) /          0 /
C     DATA IMACH( 4) /          1 /
C     DATA IMACH( 5) /         16 /
C     DATA IMACH( 6) /          2 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         15 /
C     DATA IMACH( 9) /      32767 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         56 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C***FIRST EXECUTABLE STATEMENT  I1MACH
      IF (I .LT. 1  .OR.  I .GT. 16) GO TO 10
C
      I1MACH = IMACH(I)
      RETURN
C
   10 CONTINUE
      WRITE (UNIT = OUTPUT, FMT = 9000)
 9000 FORMAT ('1ERROR    1 IN I1MACH - I OUT OF BOUNDS')
C
C     CALL FDUMP
C
      STOP
      END
