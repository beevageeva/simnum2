
C---------------------

	SUBROUTINE mklogname2(keyw,name2)

	CHARACTER*(*) keyw
	CHARACTER*13 name2
c	CHARACTER*4 keyw,keyvieja
	INTEGER is, icnt,numzers
	DATA is,icnt/0,0/
	
	IF (is .EQ. 0) THEN
c	        keyvieja=keyw
c		icnt = 1000
		icnt = 1
		is = 1
	ELSE
		icnt = icnt + 1
	END IF

c	IF (keyw.ne.keyvieja) then
cc	   	icnt = 1000
c	        icnt = 1
c		is = 1
c	end if	

	numzers =5 - ( INT(LOG10(FLOAT(icnt))) + 1)

	name2(1:4) = keyw
	name2(10:13)='.los'
	WRITE(name2(5:9),'(I5)') icnt
	IF (numzers .EQ. 1) THEN
		name2(5:5) = '1'
	ELSE IF (numzers .EQ. 2) THEN
		name2(5:6) = '10'
	ELSE IF (numzers .EQ. 3) THEN
		name2(5:7) = '100'
	ELSE IF (numzers .EQ. 4) THEN
		name2(5:8) = '1000'
	END IF

	WRITE(6,'(A30)') name2
	WRITE(6,'(A)') ' '
c	keyvieja=keyw
	RETURN
	END
