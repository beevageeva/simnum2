C********************************************************************
C
	SUBROUTINE potrange(ppot, istart, iend, potlow, pothigh)
C
C********************************************************************
C
	IMPLICIT NONE

	INTEGER n, istart, iend
	REAL ppot, potlow, pothigh
	DIMENSION ppot(*)

	potlow = 1.e10
	pothigh = -1.e10

	DO 10 n = istart, iend
	    IF (ppot(n) .GT. pothigh) pothigh = ppot(n)
	    IF (ppot(n) .LT. potlow)  potlow  = ppot(n)
10	CONTINUE

	RETURN
	END
