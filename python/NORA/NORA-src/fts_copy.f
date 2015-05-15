C
C ---	FTS_COPY  Copies and rubs a string that may be quoted --------------
C
	SUBROUTINE FTS_COPY(NEW,OLD)
C
	CHARACTER NEW*(*),OLD*(*)
	INTEGER   L1,L2,I1,I2,FTS_LEN
C
	L1=FTS_LEN(OLD)
	L2=LEN(NEW)
	NEW=' '
C
C	Remove trailing spaces
C
	I1=1
	DO WHILE (OLD(I1:I1).EQ.' '.AND.I1.LT.L1)
	   I1=I1+1
	END DO
C
C	Skip any opeing quote
C
	IF (OLD(I1:I1).EQ.'''') I1=I1+1
	I2=1
	DO WHILE (OLD(I1:I1).NE.''''.AND.I1.LE.L1.AND.I2.LE.L2)
	   IF (ICHAR(OLD(I1:I1)).GE.ICHAR(' ').AND.
     &	       ICHAR(OLD(I1:I1)).LE.ICHAR('~')) NEW(I2:I2)=OLD(I1:I1)
	   I1=I1+1
	   I2=I2+1
	END DO
C
	RETURN
	END
