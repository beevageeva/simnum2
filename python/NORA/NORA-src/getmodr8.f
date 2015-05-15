


C*************************************************************************
C
C
		SUBROUTINE getmodr8(model, filename, nbods)
C
C
C**************************************************************************

	IMPLICIT NONE

	INCLUDE 'nora.def'

	INTEGER model, nbods, ierr
	CHARACTER*40 filename

	PRINT*, 'getmodel>> model : ', model

	CALL xvprdr8(model,filename,header,
     1               x, y, z, vx, vy, vz, pmass, ppot, ierr)

CCC        CALL nuread(model,filename,ierr)
 
        PRINT *,' '
        PRINT *,' getmodel>> Header(1-3) : ',
     &                          header(1),header(2),header(3)
 
	IF (ierr .NE. 0) RETURN

CCC        PRINT *, ' '
CCC        PRINT *, ' getmodel>> Centering the system...'
CCC        PRINT *, ' '
        nbods=header(1)
CCC        CALL potcntr(1, nbods, 0.5)

	RETURN
	END
