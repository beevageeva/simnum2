

C*****************************************************************************
C
C
	SUBROUTINE mkfitsname(keyw,name)
C
C
C*****************************************************************************

	CHARACTER*(*) keyw, name
	DATA is/0/

	IF (is .EQ. 0) THEN
		icnt = 1
		is = 1
	ELSE
		icnt = icnt + 1
	END IF

	numzers = 4 - ( INT(LOG10(FLOAT(icnt))) + 1)

	name(1:3) = keyw
	name(8:12) = '.fits'
	WRITE(name(4:7),'(I4)') icnt
	IF (numzers .EQ. 1) THEN
		name(4:4) = '0'
	ELSE IF (numzers .EQ. 2) THEN
		name(4:5) = '00'
	ELSE IF (numzers .EQ. 3) THEN
		name(4:6) = '000'
	END IF

	WRITE(6,'(1x,A30)') name
	WRITE(6,'(A)') ' '

	RETURN
	END
