


C******************************************************************************
C
C
                  SUBROUTINE pickview2 (nviews, views)
C
C
C******************************************************************************
C  Fills up the array VIEWS(2,1:nviews) with values of theta, phi of observer.
C
C		views(1,*)	: theta
C		views(2,*)	: phi
C-----------------------------------------------------------------------------

	IMPLICIT NONE
	INTEGER nviews, i,nu
	REAL views(2,*)
c        REAL views1(*),views2(*)
        REAL rand ,angulo,angulo2,PI
        
        PI = ACOS(-1.)
	nviews = 0
c	DO 10 i = 1, 6
c	    nviews     = nviews + 1
c	    views(1,i) = 90.
c	    views(2,i) = 30. * (i - 1)
c 10	CONTINUE
c	DO 20 i = 7, 10
c	    nviews     = nviews + 1
c	    views(1,i) = 45.
c	    views(2,i) = 45. * (i - 7)
c 20	CONTINUE

c	nviews      = nviews + 1
c	views(1,11) = 0.
c	views(2,11) = 0.
        nu=0
        DO 99 i=1,100
         nviews=nviews+1            
             angulo=rand(nu)

             angulo2=rand(nu)
             angulo=2.*(angulo-0.5)
c              PHI=180.*2.*(angulo2-0.5)
c              THETA=(180./PI)*ACOS(angulo)
              views(1,i)=(180./PI)*ACOS(angulo)
              views(2,i)=180.*2.*(angulo2-0.5)
c              views1(i)=(180./PI)*ACOS(angulo)
c              views2(i)=180.*2.*(angulo2-0.5)

 99     CONTINUE


	RETURN
	END
