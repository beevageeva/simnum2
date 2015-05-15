

C******************************************************************************
C
C
                  SUBROUTINE prject (istart, iend, theta, phi, 
     &              	      xbuff, ybuff, vbuff)

C
C
C******************************************************************************
C  Transforms the coordinates so that the original z axis is oriented at 
C  THETA, PHI (usual spherical coordinates) of the new system. 
C  Primed coordinates (xp, yp, zp) are the original system. Double primed 
C  coordinates (xpp,ypp,zpp) are the intermediate system.
C------------------------------------------------------------------------------

	IMPLICIT NONE 

	INCLUDE 'nora.def'

        REAL xp, yp, zp, xpp, ypp, zpp, xppp, yppp, zppp,
     &       theta, phi, sint, cost, sinp, cosp, pi, radian
        INTEGER n, istart, iend

	REAL xbuff, ybuff, vbuff
	DIMENSION xbuff(1), ybuff(1), vbuff(1)
	INTEGER i

C------------
C  Calculate sines and cosines
C------------
	pi = ACOS(-1.)
	radian = pi / 180. 
        sint  = SIN (theta * radian)
        cost  = COS (theta * radian)
        sinp  = SIN (phi * radian)
        cosp  = COS (phi * radian)

C	WRITE(6,*) 'prject>> xbuff = ', (xbuff(i), i=1,50)
C	WRITE(6,*) 'prject>> vbuff = ', (vbuff(i), i=1,50)

C-------------
C  Positions...
C-------------
        DO 10 n = istart, iend
                xp = x(n)
                yp = y(n)
                zp = z(n)
C-------------
C  First rotate by PHI around zp axis
C-------------
                xpp =  xp * cosp + yp * sinp
                ypp = -xp * sinp + yp * cosp
                zpp =  zp
C-------------
C Then rotate by THETA around ypp axis
C-------------
                xppp = xpp * cost - zpp * sint
                yppp = ypp

C------------
C interchange x and y axes for better orientation of plots
C------------
		xbuff(n) =  yppp
		ybuff(n) = -xppp
 10     CONTINUE
 
C-------------
C  ...and velocities
C-------------
        DO 20 n = istart, iend
                xp = vx(n)
                yp = vy(n)
                zp = vz(n)
C-------------
C  First rotate by PHI around zp axis
C-------------
                xpp =  xp * cosp + yp * sinp
                ypp = -xp * sinp + yp * cosp
                zpp =  zp
C-------------
C Then rotate by THETA around ypp axis
C-------------
		zppp = xpp * sint + zpp * cost

		vbuff(n) = -zppp

 20     CONTINUE

C	WRITE(6,*) 'prject>> xbuff = ', (xbuff(i), i=1,50)
C	WRITE(6,*) 'prject>> vbuff = ', (vbuff(i), i=1,50)

        RETURN
        END
