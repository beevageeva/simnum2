


C--------------------------------------------------------
	INTEGER FUNCTION strlength(string, ier)
C--------------------------------------------------------
	
	IMPLICIT NONE
	INTEGER ier, LEN, i
	CHARACTER*(*) string
	
	ier = 0
	
	DO 10 i = LEN(string), 1, -1
		IF (string(i:i) .NE. ' ') THEN
			strlength = i
			RETURN
		END IF
10	CONTINUE
	strlength = 0

	RETURN
	END
