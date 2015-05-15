

C************************************************************************
C
C
	                SUBROUTINE bodsrange
     &         (up, down, nbods, nsplit, istart, iend)
C
C
C************************************************************************

	LOGICAL up, down

	istart = 1
	iend   = nbods
	IF (up)     istart=nsplit + 1
	IF (down)   iend  =nsplit

	RETURN
	END
