C***************************************************************************
C
C
                      FUNCTION MDIAN2 (X,N)
C
C
C***************************************************************************
C  From the subroutine in Numerical Recipes with the same name.
C  (5/26/88) MB
C--------------------------------------------------------------------------
      REAL mdian2
      DIMENSION X(1)
      PARAMETER (BIG=1.E30,AFAC=1.5,AMP=1.5)
 
        tol = 0.004
      A=0.
      EPS=0.01
      AP=BIG
      AM=-BIG
      n_iter = 0
 
1       n_iter = n_iter + 1
        IF (n_iter .GT. 50) THEN
            WRITE(6, *) 'mdian2>> n iterations > 50'
            mdian2 = 0.5 * (xp + xm)
            RETURN
        END IF
        SUM=0.
      SUMX=0.
      NP=0
      NM=0
      XP=BIG
      XM=-BIG
      DO 11 J=1,N
        XX=X(J)
        IF(XX.NE.A)THEN
          IF(XX.GT.A)THEN
            NP=NP+1
            IF(XX.LT.XP)XP=XX
          ELSE IF(XX.LT.A)THEN
            NM=NM+1
            IF(XX.GT.XM)XM=XX
          ENDIF
          DUM=1./(EPS+ABS(XX-A))
          SUM=SUM+DUM
          SUMX=SUMX+XX*DUM
        ENDIF
11    CONTINUE
 
C       write(6,*) 'mdian2>> xm, xp = ', xm, xp
 
      IF( (FLOAT(np - nm) / n) .GE. tol ) THEN
        AM=A
        AA=XP+MAX(0.,SUMX/SUM-A)*AMP
        IF(AA.GT.AP)AA=0.5*(A+AP)
        EPS=AFAC*ABS(AA-A)
        A=AA
        GO TO 1
      ELSE IF( (FLOAT(nm - np) / n) .GE. tol ) THEN
        AP=A
        AA=XM+MIN(0.,SUMX/SUM-A)*AMP
        IF(AA.LT.AM)AA=0.5*(A+AM)
        EPS=AFAC*ABS(AA-A)
        A=AA
        GO TO 1
      ELSE
C           WRITE(6,*) 'mdian2>> iterations : ', n_iter
            XMED=0.5*(XP+XM)
      ENDIF
      mdian2 = XMED
      RETURN
      END
