C**************************************************************************
C
C
              SUBROUTINE FindRm(totm, dist, Mr, Rm, FRM, nbin)
C
C
C**************************************************************************
C  Calculates the radii Rm containing 10%, 20%, 30%,... of the galaxy mass 
C  Mr is an accumulator of mass included within each radius.
C
C       INPUT:
C       totm       : Expected mass of the system
C       Mr      : array containing the mass integrated from center
C       dist    : array of NBIN distances, from 0.1*eps to Rmax.
C       OUTPUT:
C       Rm      : real vector(10) of the radii that contain 10%, 20%, 
C                 30%,... of the galaxy mass.
C MB (6/9/88)
C MB 9/9/93 modified to calculate FRM=0.01, 0.02, 0.05, 0.1, ..., 
C		..., 0.90, 0.95, 0.98, 0.99, 1.00 (16 values)
C	    Add linear interpolation in dist
C-----------------------------------------------------------------------------
 
 
        IMPLICIT NONE
        INTEGER i, k, nbin
        REAL totm, dist(*), Mr(*), Rm(*), FRM(*)

C-------
C  Initialize
C------

	FRM(1)=0.01
	FRM(2)=0.02
	FRM(3)=0.05
	DO 10 I=1,9
10          FRM(I+3)=0.1*FLOAT(I)
	FRM(13)=0.95
	FRM(14)=0.98
	FRM(15)=0.99
	FRM(16)=1.00
 
        DO 20 k = 1, 16
                Rm(k) = 1.e15
 20     CONTINUE

CCC	PRINT*, 'totm : ', totm
CCC	PRINT*, (Mr(k), k=1,nbin)
C---------
C  Compute  -- Need to polish branch out of loop
C---------
        DO 40 k = 1, 16
            DO 30 i = 1, nbin
CCC                IF (Mr(nbin) .GE. FRM(k)*totm) RETURN 
                IF (Mr(i) .GE. FRM(k)*totm) THEN 
                    Rm(k) = dist(i-1) + (FRM(k)*totm - Mr(i-1))
     &		         * ( (dist(i) - dist(i-1)) / (Mr(i)  - Mr(i-1)))
                    GO TO 40
                END IF
 30         CONTINUE
 40     CONTINUE
 
        RETURN
        END
