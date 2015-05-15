
C--------------------------------------------------------------------
	REAL FUNCTION strgetr (string, ier)
C--------------------------------------------------------------------

        IMPLICIT NONE
        INTEGER length, strlength, ier, ipt, strgeti
        CHARACTER*(*) string
        CHARACTER IFMT(4)*1, RFMT(6)*1, NFMT(9)*1
        DATA IFMT(1), IFMT(2), IFMT(4) / '(', 'I', ')'/
        DATA RFMT(1), RFMT(2), RFMT(4), RFMT(6) / '(', 'F', '.', ')'/
        DATA NFMT /'1', '2', '3', '4', '5', '6', '7', '8', '9'/


	length = strlength(string, ier)

	IF (length .EQ. 0) GO TO 90

	ier = 0

	ipt = INDEX(string(1:length), '.')
CCC	PRINT *, ' strgetr>> ipt = ', ipt
	IF (ipt .EQ. 0) THEN
		strgetr = FLOAT(strgeti(string, ier))
	ELSE IF (ipt .EQ. length) THEN
		strgetr = FLOAT(strgeti(string, ier))
	ELSE
		RFMT(3) = NFMT(length)
		RFMT(5) = NFMT(length - ipt)
        	READ(string(1:length), RFMT, ERR=90) strgetr
	END IF

	RETURN

90	ier = 1
	strgetr = 0.
	RETURN

        END
