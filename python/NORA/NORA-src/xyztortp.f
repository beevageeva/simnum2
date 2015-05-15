 
 
C**************************************************************************
C
                      SUBROUTINE XYZtoRTP(istart, iend, 
     &			x, y, z, r, th, ph)
C
C**************************************************************************
 
C   Find spherical coordinates for bodies. 
C               r(n)  : R
C               th(n) : COS(theta)
C               ph(n) : phi
C--------------------------------------------------------------------------
 
	IMPLICIT NONE
	INTEGER istart, iend, n
	REAL x(*), y(*), z(*), r(*), th(*), ph(*), PI

	PI = ACOS(-1.)
 
        DO 10 n = istart, iend
             r(n) = SQRT( x(n)**2 + y(n)**2 + z(n)**2 )
             th(n) = z(n) / r(n)
             ph(n) = ACOS(x(n) / SQRT(x(n)**2 + y(n)**2))
             IF (y(n) .LT. 0.e0) ph(n) = 2.e0 * PI - ph(n)
  10    CONTINUE
 
        RETURN
        END
