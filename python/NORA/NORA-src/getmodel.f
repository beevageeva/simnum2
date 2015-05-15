


C*************************************************************************
C
C
		SUBROUTINE getmodel(model, filename, nbods)
C
C
C**************************************************************************

	IMPLICIT NONE

	INCLUDE 'nora.def'

	INTEGER model, nbods, ierr
	CHARACTER*40 filename

	PRINT*, 'getmodel>> model : ', model

	CALL xvpread(model,filename,header,
     1               x, y, z, vx, vy, vz, pmass, ppot, ierr)

 
        PRINT *,' getmodel:: nbods =', header(1),
     &		'  iteration =', header(2), 
     &		'  time =', header(3)

        nbods=header(1)
 
	IF (ierr .NE. 0) THEN
		print *, ' getmodel:: error in xvpread : ', ierr
		RETURN
	END IF

CCC        PRINT *, ' '
CCC        PRINT *, ' getmodel>> Centering the system...'
CCC        PRINT *, ' '
CCC        CALL potcntr(1, nbods, 0.5)

	RETURN
	END
