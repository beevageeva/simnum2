C
C ---	FTS_LEN returns the effective length of a string ------------
C
	INTEGER FUNCTION FTS_LEN(STR)
C
	CHARACTER STR*(*)
	INTEGER   I
C
	I=LEN(STR)
	DO WHILE ((STR(I:I).EQ.' '.OR.STR(I:I).EQ.CHAR(0)).AND.I.GT.1)
	      I=I-1
	END DO
	FTS_LEN=I
	RETURN
	END
