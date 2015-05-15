 
 
C**************************************************************************
C
                      SUBROUTINE XYZtoRTZ(istart, iend, 
     &			x, y, z, rc, thc, zc)
C
C**************************************************************************
 
C   Find cilindrical coordinates for bodies. 
C               rc(n)  : R
C               thc(n) : theta
C               zc(n)  : z
C--------------------------------------------------------------------------
 
	IMPLICIT NONE
	INTEGER istart, iend, n
	REAL x(*), y(*), z(*), rc(*), thc(*), zc(*), PI

	PI = ACOS(-1.)
 
        DO 10 n = istart, iend
             rc(n) = SQRT( x(n)**2 + y(n)**2 )
             thc(n) = ATAN2( y(n), x(n) )
             zc(n) = z(n)
  10    CONTINUE
 
        RETURN
        END
