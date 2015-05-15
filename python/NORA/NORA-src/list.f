

C*****************************************************************************
 
 
                SUBROUTINE list (coord, p1, p2)
 
 
C*****************************************************************************
 
        IMPLICIT NONE

	INCLUDE 'nora.def'

        INTEGER i, p1, p2
	CHARACTER*3 coord
 
	IF (coord(1:3) .EQ. 'pos') THEN
	    DO 10 i = p1, p2
		WRITE(6,*) x(i), y(i), z(i)
10	    CONTINUE
	ELSE IF (coord(1:1) .EQ. 'v') THEN
            DO 20 i = p1, p2
                WRITE(6,*) vx(i), vy(i), vz(i)
20          CONTINUE
	ELSE IF (coord(1:1) .EQ. 'm') THEN
            DO 30 i = p1, p2
                WRITE(6,*) pmass(i)
30          CONTINUE
	ELSE IF (coord(1:3) .EQ. 'pot') THEN
            DO 40 i = p1, p2
                WRITE(6,*) ppot(i)
40          CONTINUE
	ELSE IF (coord(1:1) .EQ. 'h') THEN
            DO 50 i = p1, p2
                WRITE(6,*) header(i)
50          CONTINUE
	ELSE
	    PRINT*, 'list>> list what ? '
	    RETURN
	END IF

        END
