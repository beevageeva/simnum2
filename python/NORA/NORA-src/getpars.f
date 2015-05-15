C------------------------------------------------------------------------
	SUBROUTINE getpars(string, irest, par, maxpar, npar, ier)
C------------------------------------------------------------------------

	IMPLICIT NONE
	INTEGER maxpar, npar, ier, istart, lpar, irest, i, j
	CHARACTER*(*) string
	CHARACTER*(*) par(*)

C-----Zero 
        npar = 0
	DO 10 i = 1, maxpar
		DO 10 j = 1, LEN(par(1))
			par(i)(j:j) = ' ' 
10	CONTINUE

        istart = 1
        DO 130 i = 1, maxpar
                IF (irest .EQ. 0) GO TO 135
                istart = istart + irest - 1
                CALL getsubstr
     1                  (string(istart:), par(i), lpar, irest, ier)
                npar = npar + 1
130     CONTINUE
135     CONTINUE

	RETURN
	END
