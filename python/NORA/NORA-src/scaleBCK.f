C************************************************************************
C
C
                        PROGRAM scale
C
C
C************************************************************************
C       Program to read models from binary bodyfiles,
C	scale them by factors mrat, sizerat, Gnew/Gold,
C	and write them to new files in the same binary,
C       unformatted, direct access format.
C MB 12/10/93 modified for XVP bodyfiles
C------------------------------------------------------------------------
 
	IMPLICIT NONE
	INTEGER nmax
        PARAMETER (nmax=50000)
	REAL x, y, z, vx, vy, vz, pmass, ppot, filehead
        DIMENSION x(nmax),y(nmax),z(nmax),vx(nmax)
        DIMENSION vy(nmax),vz(nmax),pmass(nmax), ppot(nmax)
        DIMENSION filehead(128)
        INTEGER inmodel, outmodel, ierr 
        REAL mrat, sizerat, oldG
 
        CHARACTER*40 infile
        CHARACTER*40 outfile
 
        WRITE(6,201)
        WRITE(6,200)
 200     FORMAT(1x,
     1       '>>     SCALE binary file utility    <<')
        WRITE(6,201)
 201     FORMAT(1x, //)
 
 10      WRITE(6,'(A,$)') 'scale>> Name of INPUT File : '
        READ(5,2,ERR=10)infile
   2    FORMAT(a)
 15     WRITE(6,'(A,$)') 'scale>> Model : '
        READ(5,*,ERR=15) inmodel
 20     WRITE(6,'(A,$)') 'scale>> Name of OUTPUT File : '
        READ(5,2,ERR=20) outfile
 25     WRITE(6,'(A,$)') 'scale>> MODELNUM in output file : '
        READ(5,*,ERR=25) outmodel
30	WRITE(6,'(A,$)') 'scale>> mrat, sizerat, oldG : '
	READ(5,*,ERR=30) mrat, sizerat, oldG
 
        WRITE(6,'(A)') infile, outfile

                CALL xvpread(inmodel, infile, filehead, x, y, z,
     1                      vx, vy, vz, pmass, ppot, ierr)
 

		CALL scaleit 
     1		(x, y, z, vx, vy, vz, pmass, filehead, 
     1		mrat, sizerat, oldG)

		IF (filehead(100) .EQ. 1) THEN
		    CALL m2head(pmass, filehead)
		END IF

                CALL xvpwrite(outmodel, outfile, filehead, x,y,z,
     1                      vx,vy, vz,pmass,ppot,ierr)

 
        STOP
        END
 
 
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
 
