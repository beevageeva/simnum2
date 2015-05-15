 
 
C***************************************************************************
C
             SUBROUTINE VelToCyl
     &		(istart, iend, r, th, ph, vx, vy, vz, vr, vt, phiv)
C
C***************************************************************************
 
C  Transforms velocities of the bodies from a Cartesian frame centered in
C  the galaxy to local cylindrical coordinates of the star. 
C       x axis : along the meridian, pointing "south"
C       y axis : pointing "east"  (right-handed system)
C       z axis : radial outward direction, or "up"
C  Output: vr, vt, phiv
C               vr(n) : radial velocity Vr
C               vt(n) : tangential velocity Vt
C               phiv(n) : angle from x axis to Vt (increasing "east") in rad
C  Input: cartesian velocities, positions in sphericals
C               r(n) : radial coordinate
C               th(n) : COS(theta)
C               ph(n) : phi (radians)
C--------------------------------------------------------------------------
 
	IMPLICIT NONE
	INTEGER istart, iend
	REAL r(*), th(*), ph(*)
	REAL vx(*), vy(*), vz(*), vr(*), vt(*), phiv(*)

        REAL xp, yp, zp, xpp, ypp, zpp, costheta, sintheta, PI
        INTEGER n

	PI = ACOS(-1.)
 
        DO 10 n = istart, iend
                costheta = th(n)
                sintheta = SQRT(1.e0 - costheta**2)
C--------------
C  First rotate an angle PHI around the z axis
C--------------
                xpp = vx(n)*COS(ph(n)) + vy(n)*SIN(ph(n))
                ypp = -vx(n)*SIN(ph(n)) + vy(n)*COS(ph(n))
                zpp = vz(n)
C--------------
C  Then rotate an angle THETA around the ypp axis
C--------------
                xp = xpp * costheta - zpp * sintheta
                yp = ypp
                zp = xpp * sintheta + zpp * costheta
C--------------
C  Now find radial and tangential components of velocity, and angle
C  from xp-axis to Vt.
C--------------
                vr(n) = zp
                vt(n) = SQRT(xp**2 + yp**2)
                IF (vt(n) .LT. 1.e-14) THEN
                     phiv(n) = 0.e0
                ELSE
                     phiv(n) = ACOS(xp / vt(n))
                END IF
C--------------
C  make provision for 3rd and 4th quadrants of xp-yp plane
C--------------
                IF (yp .LT. 0.e0)    phiv(n) = 2.e0 * pi - phiv(n)
 10     CONTINUE
 
        RETURN
        END
