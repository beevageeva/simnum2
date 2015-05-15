


C*******************************************************************
C
	    SUBROUTINE m2head (pmass, header)
C
C*******************************************************************
C       header(100) = 1         file contains xvp, not xvm, 0 otherwise
C       header(101) = nmasses   number of body mass groups
C       header(102) = n1        bodies 1:n1 have mass m1
C       header(103) = m1        the body mass m1
C       header(104) = n2        bodies n1+1:n2 have mass m2
C       header(105) = m2
C       header(106) = n3
C       header(107) = m3        etc     max 13 mass groups
C-----------------------------------------------------------------------
 
        IMPLICIT NONE
        REAL pmass, header, currmass
        DIMENSION pmass(*), header(*)
        INTEGER nbods, mgroup, n, i, nold
 
        nbods = header(1)
	nold  = header(101)

C---- NOT xvp format cases, warn, exit
	IF (header(100) .NE. 1) THEN
	    PRINT*, 
	1	'm2head>> not xvp bodyfile format : ', header(100)
	    RETURN
	END IF

C---- Code particle masses in header
            header(103) = pmass(1)
            mgroup = 1
            currmass = pmass(1)
            DO 10 n = 1, nbods
                IF (ABS(pmass(n)-currmass) .GT. 1.e-6 * currmass) THEN
		    PRINT *, n - 1
                    header(100+2*mgroup) = n - 1
                    mgroup = mgroup + 1
                    currmass = pmass(n)
                    header(101+2*mgroup) = currmass
                END IF
10          CONTINUE
            header(100+2*mgroup) = nbods
            header(101) = mgroup
            PRINT *, 'm2head>> ', mgroup, ' mass groups'

	IF (mgroup .LT. nold) THEN
	    DO 20 i = 101+2*mgroup+1, 101+2*nold
	        header(i) = 0.
20	    CONTINUE
	END IF

	RETURN
	END


