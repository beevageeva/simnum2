

C*****************************************************************************
 
 
                SUBROUTINE cmcntr(istart, iend)
 
 
C*****************************************************************************
 
        IMPLICIT NONE

	INCLUDE 'nora.def'

        INTEGER n, istart, iend
 
        REAL xcm, ycm, zcm, vxcm, vycm, vzcm, m

	xcm  = 0.e0
	ycm  = 0.e0
	zcm  = 0.e0
	vxcm = 0.e0
	vycm = 0.e0
	vzcm = 0.e0
	m    = 0.e0
C-----------
C  C of M
C----------
	
        DO 10 n = istart, iend
           xcm  = xcm  + pmass(n) * x(n)
           ycm  = ycm  + pmass(n) * y(n)
           zcm  = zcm  + pmass(n) * z(n)
           vxcm = vxcm + pmass(n) * vx(n)
           vycm = vycm + pmass(n) * vy(n)
           vzcm = vzcm + pmass(n) * vz(n)
	   m    = m    + pmass(n)
 10     CONTINUE

	xcm  = xcm  / m
	ycm  = ycm  / m
	zcm  = zcm  / m
	vxcm = vxcm / m
	vycm = vycm / m
	vzcm = vzcm / m
 
C----------
C  Subtract from coordinates
C---------
        DO 20 n = istart, iend
           x(n)  = x(n)  - xcm
           y(n)  = y(n)  - ycm
           z(n)  = z(n)  - zcm
           vx(n) = vx(n) - vxcm
           vy(n) = vy(n) - vycm
           vz(n) = vz(n) - vzcm
 20     CONTINUE

C---------
C  Printout
C---------
        WRITE(6, 100) xcm, ycm, zcm
 100    FORMAT(/' cmcntr>> cm position : ', 3(1pE12.3))
 
        WRITE(6, 110) vxcm, vycm, vzcm
 110    FORMAT(' cmcntr>> cm velocity : ', 3(1pE12.3)/)
 
        RETURN
        END
