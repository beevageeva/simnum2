 
 
C************************************************************************
C
        SUBROUTINE scaleit 
     1	(x, y, z, vx, vy, vz, pmass, filehead, mrat, sizerat, oldG)
C
C************************************************************************
C  Scales coordinates by factors sizerat, mrat, Gnew/Gold. 
C  All factors are new/old. 
C  Gnew is 1.
C
C  MB 12/10/93
C-------------------------------------------------------------------------
 
	IMPLICIT NONE
	INTEGER n, nbod
	REAL x, y, z, vx, vy, vz, pmass, filehead
        DIMENSION x(1),y(1),z(1),vx(1)
        DIMENSION vy(1),vz(1),pmass(1)
        DIMENSION filehead(1)

        REAL mrat, sizerat, velfac, oldG, Grat
 
	nbod = filehead(1)
C----------------
C  Scale masses
C----------------
        DO 10 n = 1, nbod
                pmass(n) = mrat * pmass(n)
 10     CONTINUE
 
C---------------
C  Scale position coordinates
C---------------
        DO 20 n = 1, nbod
		x(n) = sizerat * x(n)
		y(n) = sizerat * y(n)
		z(n) = sizerat * z(n)
 20     CONTINUE
 
C---------------
C  Scale velocity coordinates 
C  Skip if velocity scaling factor is 1
C---------------
	Grat = 1.0 / oldG
 35     velfac = SQRT(Grat * mrat / sizerat)
        IF (ABS(velfac - 1.0e0) .LT. 1.0e-3) RETURN
 
        DO 40 n = 1, nbod
		vx(n) = velfac * vx(n)
		vy(n) = velfac * vy(n)
		vz(n) = velfac * vz(n)
 40     CONTINUE
 
        RETURN
        END
