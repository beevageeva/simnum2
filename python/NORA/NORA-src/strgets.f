

C----------------------------------------------------------------------
	CHARACTER*(*) FUNCTION strgets (string, ier)
C--------------------------------------------------------------------

	IMPLICIT NONE
	INTEGER lstring, lsubstr, irest, ier
	CHARACTER *(*) string 

	ier = 0
	CALL getsubstr
     1  (string, strgets, lsubstr, irest, ier)
	lstring = lsubstr

CCC	IF (ier .NE. 0) THEN
CCC	  IF (lsubstr .EQ. 0) THEN
CCC		PRINT *, ' strgets: empty string'
CCC	  ELSE
CCC	     PRINT *, ' strgets: error reading ''', string, ''''
CCC	  END IF
CCC	END IF

	RETURN
	END
