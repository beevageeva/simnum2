


C******************************************************************************
C
C
                  SUBROUTINE pickview (nviews, views)
C
C
C******************************************************************************
C  Fills up the array VIEWS(2,1:nviews) with values of theta, phi of observer.
C
C		views(1,*)	: theta
C		views(2,*)	: phi
C-----------------------------------------------------------------------------

	IMPLICIT NONE
	INTEGER nviews, i
	REAL views(2,*)

	nviews = 0
	DO 10 i = 1, 6
	    nviews     = nviews + 1
	    views(1,i) = 90.
	    views(2,i) = 30. * (i - 1)
 10	CONTINUE
	DO 20 i = 7, 10
	    nviews     = nviews + 1
	    views(1,i) = 45.
	    views(2,i) = 45. * (i - 7)
 20	CONTINUE

	nviews      = nviews + 1
	views(1,11) = 0.
	views(2,11) = 0.

	RETURN
	END
