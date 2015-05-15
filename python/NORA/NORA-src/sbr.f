C*************************************************************************
C
C 
                     SUBROUTINE sbr(gdev, coordmax, istart, iend,
     &				theta, phi, GRAVC, proy,infostr)
C
C
C*************************************************************************
C------------------------------------------------------------------------- 

	IMPLICIT NONE

	INCLUDE 'nora.def'

	INTEGER nbin, uobs, i, n, k
	PARAMETER (nbin=40, uobs=31)
	REAL logdist, dist, dlogr, eps, PI, GRAVC
	DIMENSION logdist(nbin), dist(nbin)

	INTEGER nbods,proy

	CHARACTER*(*) gdev, infostr(*)

        INTEGER istart, iend, Ngen(nbin)
        REAL totm, coordmax, surf(nbin), Mr(nbin), disp(nbin),
     1       dispint(nbin), meanv(nbin)
        REAL theta, phi, xproj(nmax), yproj(nmax), vproj(nmax), xn
 
	PI = ACOS(-1.)
	eps = 0.1
C--------------
C  Create linearly-spaced grid of log-r values between eps and rmax
C  Delogarithmize  array logdist
C--------------
        CALL loggrid(coordmax, eps, nbin, logdist, dlogr)

        DO 50 i = 1, nbin
                dist(i) = 10 ** logdist(i)
 50     CONTINUE
 
C------------
C  Total mass
C-----------
CCC        nbods = header(1)
CCC        totm = 0.0
CCC        DO 5 n = 1, nbods
CCC            totm = totm + pmass(n)
CCC 5      CONTINUE

C---------------
C   Find virtual coordinates of bodies
C---------------
	CALL prject(istart, iend, theta, phi, xproj, yproj, vproj)
CCC	PRINT '(3E12.4)', (xproj(k), yproj(k), vproj(k), k=1000, 1020)
 
C---------------
C  Zero accumulators
C--------------
        DO 30 i = 1, nbin
                Ngen(i) = 0
                surf(i) = 0.e0
                Mr(i)   = 0.e0
	 	meanv(i)= 0.e0
                disp(i) = 0.e0
 30     CONTINUE
 
C--------------
C  bin the surface brightness distribution
C--------------
        DO 40 n = istart, iend
            xn = LOG10(SQRT(xproj(n)**2 + yproj(n)**2))
            IF (xn .LE. logdist(1)) THEN
                i = 1
            ELSE 
                i = 2 + INT((xn - logdist(1)) / dlogr)
            END IF
 
            IF (i .GT. nbin) GO TO 40
            
            Ngen(i) = Ngen(i)  + 1
            surf(i) = surf(i)  + pmass(n)
	    meanv(i)= meanv(i) + pmass(n) * vproj(n)
 40     CONTINUE

	DO 41 i = 1, nbin
	    IF (surf(i) .LT. 1.e-14) THEN
                meanv(i) = 0.e0
            ELSE
                meanv(i) = meanv(i) / surf(i)
            END IF
 41     CONTINUE
 
        DO 42 n = istart, iend
            xn = LOG10(SQRT(xproj(n)**2 + yproj(n)**2))
            IF (xn .LE. logdist(1)) THEN
                i = 1
            ELSE 
                i = 2 + INT((xn - logdist(1)) / dlogr)
            END IF
 
            IF (i .GT. nbin) GO TO 42
            
            disp(i) = disp(i)  + 
     1			pmass(n) * (vproj(n) - meanv(i))**2
 
 42     CONTINUE
 
C------------
C  Find number of particles, projected mass and velocity dispersion
C  integrated from the center
C-----------
        Mr(1) = surf(1)
        dispint(1) = disp(1)
        DO 60 i = 2, nbin
                Ngen(i) = Ngen(i) + Ngen(i - 1)
                Mr(i)   = Mr(i - 1) + surf(i)
                dispint(i) = disp(i) + dispint(i - 1)
 60     CONTINUE
 
C----------
C  velocity dispersion
C----------
        DO 80 i = 1, nbin
            IF (surf(i) .EQ. 0) THEN
                disp(i) = 0.e0
            ELSE
                disp(i) = SQRT (disp(i) / surf(i)) 
            END IF
 80     CONTINUE
 
        DO 82 i = 1, nbin
            IF (Mr(i) .EQ. 0.) THEN
                dispint(i) = 0.e0
            ELSE
                dispint(i) = SQRT(dispint(i) / Mr(i))
            END IF
 82     CONTINUE

C------------
C  surface brightness
C------------
        surf(1) = surf(1) / (4.e0 * pi * dist(1)**2)
        DO 70 i = 2, nbin
            surf(i) = surf(i) 
     1              / (4.e0 * pi * (dist(i)**2 - dist(i-1)**2))
	    IF (surf(i) .EQ. 0.) surf(i) = 1.e-20
 70     CONTINUE
 
CCC	PRINT '(I8,4E12.4)',
CCC     &      (Ngen(k), dist(k), Mr(k), surf(k), disp(k), k=1,nbin)
 
C---------
C  Output
C---------
	CALL sbrout2(gdev, dist, Ngen, Mr, surf, disp, GRAVC,proy,
     &		infostr)

        RETURN
        END
