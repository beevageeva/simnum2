

C******************************************************************
C
		SUBROUTINE psflush (gdev)
C
C******************************************************************

	IMPLICIT NONE
	CHARACTER*(*) gdev
	INTEGER status, SYSTEM
        CHARACTER*80 command

        IF (gdev .EQ. '/PS' .OR. gdev .EQ. '/VPS'
     &  .OR. gdev .EQ. '/CPS' .OR. gdev .EQ. '/VCPS') THEN
C---Read in dispose command
	OPEN (UNIT=10, FILE='~/.nora/psprint', STATUS='OLD', ERR=100)
	READ (10,'(A)') command
	CLOSE (10)
C---Show print command
	PRINT '(A)', '     printing :  ' // command
C---Flush to printer
        status = SYSTEM(command)
        IF(status .NE. 0) PRINT *, ' lpr error ', status
        END IF
	RETURN

100	PRINT*, 'psflush>> Cannot open dispose file ''~/.nora/psprint'''
	RETURN

	END
