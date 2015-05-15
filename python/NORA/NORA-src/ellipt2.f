C***********************************************************************

               SUBROUTINE ellipt (coordmax, istart, ienD)

C***********************************************************************


       IMPLICIT NONE

       INCLUDE 'nora.def'

	INTEGER nbin
	PARAMETER (nbin=40)
	REAL dist(nbin), logdist(nbin), dlogr

        INTEGER n, i, k, istart, iend, inbin(nbin), nbods, l
        REAL rho(nbin), Mr(nbin),
     1           totm,
     1          Rm(16), FRM(16), coordmax, eps, PI
C	CHARACTER*(*) gdev, infostr(*)
 
	REAL r, th, ph, vr, vt, phiv, rn, GRAVC
	DIMENSION r(nmax), th(nmax), ph(nmax)
	DIMENSION vr(nmax), vt(nmax), phiv(nmax)
        REAL A, B, maw, miw, theta, phi, mayaxis, equis, ygrec, zeta
        REAL BICHO, theta2, phi2, minaxis, ELLIP, ma(3000), mi(300)

	PI = ACOS(-1.)
C--------------
C  Create linearly-spaced grid of log-r values between eps and rmax
C--------------
	eps = 0.1
        CALL loggrid(coordmax, eps, nbin, logdist, dlogr)

        DO 50 i = 1, nbin
                dist(i) = 10 ** logdist(i)
 50     CONTINUE

C---------
C  Total mass
C--------
	nbods = header(1)
	totm = 0.0
	DO 5 n = istart, iend
	    totm = totm + pmass(n)
 5      CONTINUE
 
C----------
C  Fill spherical coordinate arrays: r, COS(theta), phi, 
C---------
	CALL XYZtoRTP (istart, iend, x, y, z, r, th, ph)
	CALL VelToCyl (istart, iend, r, th, ph, vx, vy, vz, vr, vt, phiv)

C--------------
        
        DO 10 i = 1, nbin
           rho(i) = 0.e0

 10     CONTINUE

        DO 20 n= istart, iend
           rn = LOG10 (r(n))
                IF (rn .LT. logdist(1)) THEN
                        i = 1
                ELSE
                        i = 2 + INT((rn - logdist(1)) / dlogr)
                END IF

                IF (i .GT. nbin) GO TO 20

                inbin(i) = inbin(i) + 1
                rho(i)  = rho(i)  + pmass(n)

 20     CONTINUE



C---------------
C  Find the radii containing 10%, 20%, 30%,... of the galaxy mass. 
C  This has to be done before rho is divided by the volume of each bin
C  Store in array Rm(16)
C---------------
	Mr(1) = rho(1)
	DO 28 i = 2, nbin
	    Mr(i) = Mr(i-1) + rho(i)
28	CONTINUE
        CALL findRm(totm, dist, Mr, Rm, FRM, nbin)
 
C---------------
C  Find mass per unit volume for each radial bin
C  Protect against rho=0 in empty bins
C---------------
C        DO 30 i = 1, nbin
C                rho(i) = rho(i) / 
C     1               (4.e0 * PI * (dist(i)**3 - dist(i-1)**3) / 3.e0)
C		IF (rho(i) .EQ. 0.) rho(i) = 1.e-10
C 30     CONTINUE


C----------------------------------
