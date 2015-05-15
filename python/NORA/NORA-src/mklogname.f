
C---------------------

	SUBROUTINE mklogname(keyw,name)

	CHARACTER*(*) keyw, name
	INTEGER is3,icnt1,numzers1
	DATA is3,icnt1/0,0/

	numzers1=0

	IF (is3 .EQ. 0) THEN
		icnt1 = 1
		is3 = 1
	ELSE
		icnt1 = icnt1 + 1
	END IF

	numzers1 =4 - ( INT(LOG10(FLOAT(icnt1))) + 1)
c	print*,numzers1,FLOAT(icnt1),LOG10(FLOAT(icnt1))
	name(1:4) = keyw
	name(9:12) = '.prf'
	WRITE(name(5:8),'(I4)') icnt1
	IF (numzers1 .EQ. 1) THEN
		name(5:5) = '0'
	ELSE IF (numzers1.EQ. 2) THEN
		name(5:6) = '00'
	ELSE IF (numzers1 .EQ. 3) THEN
		name(5:7) = '000'
	END IF

	WRITE(6,'(A30)') name
	WRITE(6,'(A)') ' '

	RETURN
	END
