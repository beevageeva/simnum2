 


C*************************************************************************
C
C 
        SUBROUTINE loggrid(rmax, eps, nbin, logdist, dlogr) 
C
C
C*************************************************************************
C  Creates a linearly spaced grid of log-distances between 
C  0.1*eps and rmax.   eps and rmax are assumed in units of r0.
C------------------------------------------------------------------------- 
 
	IMPLICIT NONE
	INTEGER nbin, i
	REAL logdist, eps, rmax, dlogr
	DIMENSION logdist(*)
	
        logdist(1)    = LOG10(0.1 * eps)
        logdist(nbin) = LOG10(rmax)
        dlogr = (logdist(nbin) - logdist(1)) / (nbin - 1)
 
        DO 10 i = 2, nbin
                logdist(i) = logdist(1) + dlogr * FLOAT(i - 1)
 10     CONTINUE
 
        RETURN
        END
