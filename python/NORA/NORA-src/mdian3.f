 
 
 
C***************************************************************************
C
C
                      FUNCTION MDIAN3 (X, W, N)
C
C
C***************************************************************************
C  From the subroutine MDIAN2 in Numerical Recipes.
C  Adapted to handle distributions of points with weights.
C               X(N)    : array of points
C               W(N)    : array of weights
C  If more than MAXITER iterations, function returns with the mean of the
C  two closest points to the estimated median.
C  (11/5/88) MB
C--------------------------------------------------------------------------
      INTEGER J, JP, JM, niter, maxiter
      REAL mdian3, NP, NM, X, W, XMED
      DIMENSION X(1), W(1)
      PARAMETER (BIG=1.E30,AFAC=1.5,AMP=1.5)
      PARAMETER (maxiter = 40)
      A=0.
      EPS=1
      AP=BIG
      AM=-BIG
      niter = 0
1     SUM=0.
      SUMX=0.
      NP=0.
      NM=0.
      XP=BIG
      XM=-BIG
Cparece que hay que inicializar JP, sino da segmentation fault, hago:
      JP=1
      niter = niter + 1
      DO 11 J=1,N
        XX=X(J)
        IF(XX.NE.A)THEN
          IF(XX.GT.A)THEN
            NP=NP+w(J)
            IF(XX.LT.XP) THEN
                XP=XX
                JP = J
            END IF
          ELSE IF(XX.LT.A)THEN
            NM=NM+w(J)
            IF(XX.GT.XM) THEN
                XM=XX
                JM = J
            END IF
          ENDIF
          DUM=w(J)/(EPS+ABS(XX-A))
          SUM=SUM+DUM
          SUMX=SUMX+XX*DUM
        ENDIF
11    CONTINUE

      IF (niter.GE.maxiter) THEN
        WRITE(6,*) 'mdian3>> more than ', maxiter, ' iterations'
        mdian3 = 0.5 * (XP + XM)
        RETURN
      ENDIF


      IF(NP-NM.GE.(2.*w(JP)))THEN

        AM=A
        AA=XP+MAX(0.,SUMX/SUM-A)*AMP
        IF(AA.GT.AP)AA=0.5*(A+AP)
        EPS=AFAC*ABS(AA-A)
        A=AA
        GO TO 1
      ELSE IF(NM-NP.GE.(2.*w(JP)))THEN

        AP=A
        AA=XM+MIN(0.,SUMX/SUM-A)*AMP
        IF(AA.LT.AM)AA=0.5*(A+AM)
        EPS=AFAC*ABS(AA-A)
        A=AA
        GO TO 1
      ELSE

        IF(MOD(N,2).EQ.0)THEN
          IF(NP.EQ.NM)THEN
            XMED=0.5*(XP+XM)
          ELSE IF(NP.GT.NM)THEN
            XMED=0.5*(A+XP)
          ELSE
            XMED=0.5*(XM+A)
          ENDIF

        ELSE
          IF(NP.EQ.NM)THEN
            XMED=A
          ELSE IF(NP.GT.NM)THEN
            XMED=XP
          ELSE
            XMED=XM
          ENDIF

        ENDIF
      ENDIF

      mdian3 = XMED
      RETURN
      END
