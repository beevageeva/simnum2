
C--------------------------------------------------------------------
	INTEGER FUNCTION strgeti (string, ier)
C------------------------------------------------------------

	IMPLICIT NONE 
	INTEGER length, strlength, ier, ipt
	CHARACTER*(*) string
	CHARACTER IFMT(4)*1, RFMT(6)*1, NFMT(9)*1
	DATA IFMT(1), IFMT(2), IFMT(4) / '(', 'I', ')'/
	DATA RFMT(1), RFMT(2), RFMT(4), RFMT(6) / '(', 'F', '.', ')'/
	DATA NFMT /'1', '2', '3', '4', '5', '6', '7', '8', '9'/

	length = strlength(string, ier)

	IF (length .EQ. 0) GO TO 90

	ier = 0

	ipt = INDEX(string(1:length), '.')
CCC	PRINT*, 'dec point at ', ipt
	IF (ipt .EQ. 0) THEN
		IFMT(3) = NFMT(length)
		READ(string(1:length), IFMT, ERR=90) strgeti
	ELSE
		IFMT(3) = NFMT(ipt - 1)
		READ(string(1:ipt - 1), IFMT, ERR=90) strgeti
	END IF
	RETURN

90	ier = 1
	strgeti = 0
	RETURN

	END
