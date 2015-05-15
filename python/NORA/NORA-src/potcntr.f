

C*****************************************************************************
 
 
                SUBROUTINE potcntr(istart, iend, potfrac)
 
 
C*****************************************************************************
 
        IMPLICIT NONE

	INCLUDE 'nora.def'

        INTEGER n, istart, iend, npot
 
        REAL coord(nmax), xmed, ymed, zmed, vxmed, vymed, vzmed, mdian2
	REAL potlow, pothigh, potfrac, pot1, pot2
 
C-----------
C  Range of particle potentials
C----------
	CALL potrange(ppot, istart, iend, potlow, pothigh)
CCC	PRINT *, 'potlow, pothigh, potfrac = ', potlow, pothigh, potfrac
	pot1 = potlow
	pot2 = potlow * (1 - potfrac)
CCC	PRINT *, 'pot1, pot2 = ', pot1, pot2
C-----------
C  Positions...
C----------
	npot = 0
        DO 10 n = istart, iend
	   IF (ppot(n) .GT. pot1 .AND. ppot(n) .LT. pot2) THEN
		npot = npot + 1
		coord(npot) = x(n)
	   END IF
 10     CONTINUE
        xmed = MDIAN2(coord, npot)
CCC	PRINT *, 'npot = ', npot
 
	npot = 0
        DO 20 n = istart, iend
	   IF (ppot(n) .GT. pot1 .AND. ppot(n) .LT. pot2) THEN
		npot = npot + 1
		coord(npot) = y(n)
	   END IF
 20     CONTINUE
        ymed = MDIAN2(coord, npot)
 
	npot = 0
        DO 30 n = istart, iend
	   IF (ppot(n) .GT. pot1 .AND. ppot(n) .LT. pot2) THEN
		npot = npot + 1
		coord(npot) = z(n)
	   END IF
 30     CONTINUE
        zmed = MDIAN2(coord, npot)
 
C-----------
C  ...and Velocities
C----------
	npot = 0
        DO 40 n = istart, iend
	   IF (ppot(n) .GT. pot1 .AND. ppot(n) .LT. pot2) THEN
		npot = npot + 1
		coord(npot) = vx(n)
	   END IF
 40     CONTINUE
        vxmed = MDIAN2(coord, npot)
 
	npot = 0
        DO 50 n = istart, iend
	   IF (ppot(n) .GT. pot1 .AND. ppot(n) .LT. pot2) THEN
		npot = npot + 1
		coord(npot) = vy(n)
	   END IF
 50     CONTINUE
        vymed = MDIAN2(coord, npot)
 
	npot = 0
        DO 60 n = istart, iend
	   IF (ppot(n) .GT. pot1 .AND. ppot(n) .LT. pot2) THEN
		npot = npot + 1
		coord(npot) = vz(n)
	   END IF
 60     CONTINUE
        vzmed = MDIAN2(coord, npot)
 
        WRITE(6, 100) xmed, ymed, zmed
 100    FORMAT(/' medcntr>> median position : ', 3(1pE12.3))
 
        WRITE(6, 110) vxmed, vymed, vzmed
 110    FORMAT(' medcntr>> median velocity : ', 3(1pE12.3)/)
 
C----------
C  Subtract from coordinates
C---------
        DO 70 n = istart, iend
            x(n)  = x(n)  - xmed
            y(n)  = y(n)  - ymed
            z(n)  = z(n)  - zmed
            vx(n) = vx(n) - vxmed
            vy(n) = vy(n) - vymed
            vz(n) = vz(n) - vzmed
 70     CONTINUE
 
        RETURN
        END
