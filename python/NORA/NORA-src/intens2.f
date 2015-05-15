C     ****************************************************************
      SUBROUTINE INTENS2(NP1,NP2,Rmin,Rmax)
C     ****************************************************************
      INCLUDE 'nora.def'
      DIMENSION AI(3,3),BI(3,3),DA(3),DB(3),ZA(3,3),ZB(3,3),WK(10),M(3)
      DOUBLE PRECISION LI1,LI2,LI3
      REAL THETA,PHI,L,I1,I2,I3,II1,II2,II3,ELLIP,P,Q,EPS1,EPS2
C
C     EXTERNAL EIGRS
C
      DO 10 I=1,3
      DO 10 J=1,3
      AI(I,J)=0.0
      BI(I,J)=0.0
      
   10 CONTINUE
C
      LI1 = 0.0
      LI2 = 0.0
      LI3 = 0.0

      DO 20 L=NP1,NP2
      IF ((x(L)*x(L) + y(L)*y(L) + z(L)*z(L)) .GT. Rmax**2) GO TO 20
      IF ((x(L)*x(L) + y(L)*y(L) + z(L)*z(L)) .LT. Rmin**2) GO TO 20
C            AI:  MOMENT OF INERTIA TENSOR
      AI(1,1) = AI(1,1) + (Y(L)**2 + Z(L)**2)*PMASS(L)
      AI(2,2) = AI(2,2) + (X(L)**2 + Z(L)**2)*PMASS(L)
      AI(3,3) = AI(3,3) + (X(L)**2 + Y(L)**2)*PMASS(L)
      AI(1,2) = AI(1,2) - X(L)*Y(L)*PMASS(L)
      AI(1,3) = AI(1,3) - X(L)*Z(L)*PMASS(L)
      AI(2,3) = AI(2,3) - Y(L)*Z(L)*PMASS(L)
C            BI:  KINETIC ENERGY TENSOR
      BI(1,1) = BI(1,1) + VX(L)**2*PMASS(L)
      BI(2,2) = BI(2,2) + VY(L)**2*PMASS(L)
      BI(3,3) = BI(3,3) + VZ(L)**2*PMASS(L)
      BI(1,2) = BI(1,2) + VX(L)*VY(L)*PMASS(L)
      BI(1,3) = BI(1,3) + VX(L)*VZ(L)*PMASS(L)
      BI(2,3) = BI(2,3) + VY(L)*VZ(L)*PMASS(L)
C            LI:  ANGULAR MOMENTUM VECTOR
      LI1 =LI1 + (Y(L)*VZ(L)-Z(L)*VY(L))*PMASS(L)
      LI2 =LI2 + (Z(L)*VX(L)-X(L)*VZ(L))*PMASS(L)
      LI3 =LI3 + (X(L)*VY(L)-Y(L)*VX(L))*PMASS(L)
   20 CONTINUE

      AI(2,1)=AI(1,2)
      AI(3,1)=AI(1,3)
      AI(3,2)=AI(2,3)
      BI(2,1)=BI(1,2)
      BI(3,1)=BI(1,3)
      BI(3,2)=BI(2,3)
      DO 30 I=1,3
      DO 30 J=1,3

      BI(I,J)=0.5*BI(I,J)
C                   CALCULATES THE ANGLES, THETA AND PHI OF THE
C                   ANGULAR MOMENTUM VECTOR.

      L=((LI1**2+LI2**2+LI3**2)**(0.5))
      THETA=(ACOS(LI3/L))*(180/3.141592654)
      PHI=ATAN2(LI2,LI1)
C       IF (PHI.LT.0) THEN PHI=-PHI
C       IF (PHI.GT.180) THEN PHI=PHI-180       
C      THETA=(ACOS(LI1/(L*SIN(PHI))))*(180/3.141592654)
C      THETA1=(ASIN(LI2/(L*SIN(PHI))))*(180/3.141592654)
C       IF (THETA1.LT.0) THEN THETA =360-THETA
      

   30 CONTINUE
C                   EIGENVALUES AND VECTORS OF INERTIA TENSOR
C     CALL EIGRS (AI,3,12,DA,ZA,3,WK,IER)
C            EIGENVALUES AND VECTORS OF KINETIC ENERGY TENSOR
C     CALL EIGRS (BI,3,12,DB,ZB,3,WK,IER)
C

C                   EIGENVALUES AND VECTORS OF INERTIA TENSOR
C                      AND CALCULATE THE ELLIPTICITY
      CALL EIGENVAL(AI,I1,I2,I3)
      II1=SQRT(I1**2)
      II2=SQRT(I2**2)
      II3=SQRT(I3**2)
C            AXIAL RATIOS
C            EPS1 = B/A       EPS2 = C/A
      P=II3/II2
      Q=II3/II1
      EPS1=(-P*Q-P+Q)/(P*Q-P-Q)
      EPS2=(-P*Q+P-Q)/(P*Q-P-Q)
      
      EPS1=SQRT(EPS1)
      EPS2=SQRT(EPS2)

C      ELLIP=1-EPS2
C      ELLIP=(II1-II3)/(II1+II2+II3)
      

      WRITE(6,2010)
      WRITE(6,2011)
      DO 50 K=1,3
      WRITE(6,2012) AI(K,1),AI(K,2),AI(K,3),DA(K),BI(K,1),BI(K,2),
     1              BI(K,3),DB(K)
   50 CONTINUE
      WRITE(6,2013)
      WRITE(6,2014) LI1,LI2,LI3,L,THETA,PHI
      WRITE(6,2015)
      WRITE(6,2017) I1,I2,I3,EPS1,EPS2
C,ELLIP
C
C            SORT EIGENVALUES MOMENT OF INERTIA
C            M(1)  IS INDEX SMALLEST MOMENT OF INERTIA
C            I.E. M(1) CORRESPONDS TO LONGEST AXIS, ETC
      M(1)=1
      M(2)=2
      M(3)=3
C     IF (DA(2).LT.DA(1)) M(1)=2
C     IF (DA(3).LT.DA(M(1))) M(1)=3
C     IF (DA(2).GT.DA(3)) M(3)=2
C     IF (DA(1).GT.DA(M(3))) M(3)=1
C     M(2)=6-M(1)-M(3)
C            AXIAL RATIOS
C            EPS1 = B/A       EPS2 = C/A
C     P=DA(M(1))/DA(M(2))
C     Q=DA(M(1))/DA(M(3))
C     EPS1=(-P*Q-P+Q)/(P*Q-P-Q)
C     EPS2=(-P*Q+P-Q)/(P*Q-P-Q)
C     IF (EPS1.LT.0.0) EPS1=0.0
C     IF (EPS2.LT.0.0) EPS2=0.0
C     EPS1=SQRT(EPS1)
C     EPS2=SQRT(EPS2)
C
C            VELOCITY ANISOTROPY PARAMETER
C     DEK=1.0-2.0*DB(M(3))/(DB(M(1))+DB(M(2)))
C
C     WRITE(6,2020) EPS1,EPS2,DEK,RCUT
 2000 FORMAT(1H0)
 2010 FORMAT(1H0,21X,'MOMENT OF INERTIA',30X,'KINETIC ENERGY')
 2011 FORMAT(1H ,15X,'MATRIX',15X,'EIG VALUES',24X,'MATRIX',
     1           15X,'EIG VALUES')
 2012 FORMAT(1H ,2(3F10.4,5X,F10.4,10X))
 2013 FORMAT(1H ,9X,'ANGULAR MOM.VECTOR',18X,'ANG. MOM.',8X,'THETA',10X,
     1 'PHI')
 2014 FORMAT(1H ,3X,3F10.4,10X,F10.4,5X,F10.4,5X,F10.4)
 2015 FORMAT(1H ,12X,' INERTIA EIGENV.',10X,
     1 'AXIAL RATIOS(EPS1=B/A EPS2=C/A) ')
 2017 FORMAT(1H ,3X,3F10.4,14X,2F10.4)
C2020 FORMAT(1H0,'AXIAL RATIOS:',5X,'B/A',F8.3,5X,'C/A',F8.3,5X,
C    1           'ANISOTROPY:   EKIN',F12.3,5X,'RADIUS',F8.3)
C
      OPEN (UNIT=1, FILE='data.dt', STATUS='UNKNOWN')
      WRITE (1,*) 'file to calculate the Moment of inertia'
      WRITE (1,*) '3'
      WRITE (1,*) '"L"'
      WRITE (1,1003) AI(1,1)
      WRITE (1,1004) AI(2,1),AI(2,2)
      WRITE (1,1005) AI(3,1),AI(3,2),AI(3,3)

 1003 FORMAT(F10.4)
 1004 FORMAT(2F10.4)
 1005 FORMAT(3F10.4)
      CLOSE (UNIT=1)

      RETURN
      END
