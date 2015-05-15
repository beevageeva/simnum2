

C**************************************************************************


	      SUBROUTINE contplot(gdev, map, imsize, coordmax, level,
     &				  nlev, infostr)


C**************************************************************************

	IMPLICIT NONE
	INTEGER imsize, nlev
	REAL map, level, TR, coordmax
	DIMENSION map(imsize, imsize), level(*), TR(6)
	CHARACTER*(*) gdev
	CHARACTER*(*) infostr(*)

C--------
C TR transform array
C--------
	TR(1) =  -coordmax
	TR(2) = 2. * coordmax / FLOAT(imsize)
	TR(3) = 0.
	TR(4) = -coordmax
	TR(5) = 0.
	TR(6) = 2. * coordmax / FLOAT(imsize)

C--------
C  PGPLOT contour plot
C--------
	CALL PGBEGIN (0, gdev, 1, 1)
	CALL PGSCF(2)
c	CALL PGVPORT (0.2, 0.8, 0.35, 0.95)
	CALL PGVPORT (0.2, 0.8, 0.35, 0.75)
	CALL PGWNAD (-coordmax, coordmax, -coordmax, coordmax)
	CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
CCC	CALL PGENV (-coordmax, coordmax, -coordmax, coordmax, 1, 0)
	CALL PGCONT (map, imsize, imsize, 1, imsize, 1, imsize,
     1		level, nlev, TR)

C--------
C  Label
C--------

        CALL PGSCH (0.8)
	CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
        CALL PGWINDOW (0., 1., 0., 1.)
        CALL PGTEXT (0., 0.7, 'space map')
        CALL PGTEXT (0., 0.6, infostr(1))
        CALL PGTEXT (0., 0.5, infostr(2))
        CALL PGTEXT (0., 0.4, infostr(3))
        CALL PGTEXT (0., 0.3, infostr(4))
        CALL PGTEXT (0., 0.2, infostr(5))
        CALL PGSCH (1.0)

	CALL PGEND

C--------
C  Grayscale overplot
C--------
	
C---Flush to printer
	CALL psflush (gdev)

	RETURN
	END
