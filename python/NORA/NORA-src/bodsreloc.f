

C******************************************************************************
C
C
                  SUBROUTINE bodsreloc (istart, iend, after)
C
C
C******************************************************************************
C    istart	= index of first particle to relocate
C    iend	= index of last particle to relocate
C    after+1	= destination of particle ISTART 
C
C	number of relocated objects		= iend - istart + 1
C	destination of first invaded body	= after + iend - istart + 2
C	position shift of first invaded body	= iend - istart + 1
C	position shift of first relocated body	= -(istart - after - 1)
C------------------------------------------------------------------------------

	IMPLICIT NONE 

	INCLUDE 'nora.def'

        INTEGER n, istart, iend, after, shift

	REAL xbuff, ybuff, zbuff
	DIMENSION xbuff(nmax), ybuff(nmax), zbuff(nmax)


C---- Store selected bodies in buffer
	DO 10 n = istart, iend
		xbuff(n) = x(n)
		ybuff(n) = y(n)
		zbuff(n) = z(n)
10	CONTINUE

C---- Relocate invaded bodies after end of invaded region
C     Backwards to avoid tripping on already changed values
	shift = iend - istart + 1
	DO 20 n = istart-1, after+1, -1
		x(shift + n) = x(n)
		y(shift + n) = y(n)
		z(shift + n) = z(n)
20	CONTINUE

C---- Insert buffer particles in new destination
	shift = -(istart - after - 1)
	DO 30 n = istart, iend
		x(shift + n) = xbuff(n)
		y(shift + n) = ybuff(n)
		z(shift + n) = zbuff(n)
30	CONTINUE

C---- Repeat for velocities
C---- Store selected bodies in buffer
	DO 40 n = istart, iend
		xbuff(n) = vx(n)
		ybuff(n) = vy(n)
		zbuff(n) = vz(n)
40	CONTINUE

C---- Relocate invaded bodies after end of invaded region
	shift = iend - istart + 1
	DO 50 n = istart-1, after+1, -1
		vx(shift + n) = vx(n)
		vy(shift + n) = vy(n)
		vz(shift + n) = vz(n)
50	CONTINUE

C---- Insert buffer particles in new destination
	shift = -(istart - after - 1)
	DO 60 n = istart, iend
		vx(shift + n) = xbuff(n)
		vy(shift + n) = ybuff(n)
		vz(shift + n) = zbuff(n)
60	CONTINUE

C---- Repeat for masses, potential
C---- Store selected bodies in buffer
	DO 70 n = istart, iend
		xbuff(n) = ppot(n)
		ybuff(n) = pmass(n)
70	CONTINUE

C---- Relocate invaded bodies after end of invaded region
	shift = iend - istart + 1
	DO 80 n = istart-1, after+1, -1
		ppot(shift + n) = ppot(n)
		pmass(shift + n) = pmass(n)
80	CONTINUE

C---- Insert buffer particles in new destination
	shift = -(istart - after - 1)
	DO 90 n = istart, iend
		ppot(shift + n) = xbuff(n)
		pmass(shift + n) = ybuff(n)
90	CONTINUE

C---- Fix mass coding in header
	CALL m2head (pmass, header)

        RETURN
        END
