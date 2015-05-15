

C*****************************************************************************
 
 
                SUBROUTINE medcntr(istart, iend)
 
 
C*****************************************************************************
 
        IMPLICIT NONE

	INCLUDE 'nora.def'

        INTEGER n, istart, iend
 
        REAL coord(nmax), xmed, ymed, zmed, vxmed, vymed, vzmed, mdian2
 
C-----------
C  Positions...
C----------
        DO 10 n = istart, iend
           coord(n - istart + 1) = x(n)
 10     CONTINUE
        xmed = MDIAN2(coord, iend - istart + 1)
 
        DO 20 n = istart, iend
           coord(n - istart + 1) = y(n)
 20     CONTINUE
        ymed = MDIAN2(coord, iend - istart + 1)
 
        DO 30 n = istart, iend
           coord(n - istart + 1) = z(n)
 30     CONTINUE
        zmed = MDIAN2(coord, iend - istart + 1)
 
C-----------
C  ...and Velocities
C----------
        DO 40 n = istart, iend
           coord(n - istart + 1) = vx(n)
 40     CONTINUE
        vxmed = MDIAN2(coord, iend - istart + 1)
 
        DO 50 n = istart, iend
           coord(n - istart + 1) = vy(n)
 50     CONTINUE
        vymed = MDIAN2(coord, iend - istart + 1)
 
        DO 60 n = istart, iend
           coord(n - istart + 1) = vz(n)
 60     CONTINUE
        vzmed = MDIAN2(coord, iend - istart + 1)
 
        WRITE(6, 100) xmed, ymed, zmed
 100    FORMAT(/' medcntr>> median position : ', 3(1pE12.3))
 
        WRITE(6, 110) vxmed, vymed, vzmed
 110    FORMAT(' medcntr>> median velocity : ', 3(1pE12.3)/)
 
C----------
C  Subtract from coordinates
C---------
        DO 70 n = istart, iend
            x(n)  = x(n)  - xmed
            y(n)  = y(n)  - ymed
            z(n)  = z(n)  - zmed
            vx(n) = vx(n) - vxmed
            vy(n) = vy(n) - vymed
            vz(n) = vz(n) - vzmed
 70     CONTINUE
 
        RETURN
        END
