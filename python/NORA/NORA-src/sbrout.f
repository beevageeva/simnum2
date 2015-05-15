 
 
 
C*****************************************************************************
 
 
        SUBROUTINE sbrout(gdev, dist, Ngen, Mr, surf, disp, 
     &              GRAVC, infostr)
 
 
C*****************************************************************************
 
 
        IMPLICIT NONE
	REAL dist, Mr, surf, disp, logdist, d14, logsurf, GRAVC
        INTEGER Ngen, nbin, i, axscale
        INTEGER uout
	PARAMETER(nbin=40, uout=18)
	CHARACTER*(*) gdev
	CHARACTER*(*) infostr(*)
 
        DIMENSION dist(*), Ngen(*), Mr(*), surf(*), 
     &		disp(*), logdist(nbin), d14(nbin), logsurf(nbin)

	INTEGER cross, square, triangle
	DATA cross, square, triangle /5, 6, 7/
 
C----------
C  Logarithms
C----------
	DO 5 i = 1, nbin
		logdist(i) = LOG10(dist(i))
		d14(i)     = dist(i)**0.25
		logsurf(i) = LOG10(surf(i))
5	CONTINUE

C---------
C  Open output device
C---------
        IF (gdev(1:4) .EQ. 'file') THEN
CCC	    OPEN(uout,FILE='sbr.out',STATUS='UNKNOWN',ACCESS='APPEND')
	    OPEN(uout,FILE='sbr.out',STATUS='UNKNOWN')

	    WRITE(uout, '(A)') '#'
	    WRITE(uout, '(A)') '#'//infostr(1)(1:79)
    	    WRITE(uout, '(A)') '#'//infostr(2)(1:79)
	    WRITE(uout, '(A)') '#'
            WRITE(uout,1005)
 1005       FORMAT('#',1x, 'Radius', 2x, 'particles',
     1         3x, 'Accum. Mass',
     1         2x, 'Surf dens', 8x, 'l.o.s. Disp')
 
            DO 10 i = 1, nbin
                WRITE(uout, '(1pe12.4, I8, 3(1pe12.4))')
     1          dist(i), Ngen(i), Mr(i), surf(i), disp(i)
 10         CONTINUE
	    CLOSE(uout)
            RETURN
        ELSE IF (gdev(1:1) .EQ. 't') THEN
	    DO 25 i = 1, nbin
	      WRITE(6,'(1pe12.4, I8, 3(1pe12.4))') 
     1		dist(i), Ngen(i), Mr(i), surf(i), disp(i)
 25	    CONTINUE
	    RETURN
        ELSE 
	    CALL PGBEGIN (0, gdev, 1, 1)
        END IF
C------
C  Plot
C-------
 
C----Scale for velocity plot axes as INT(SQRT(GRAVC))
        axscale = INT(SQRT(GRAVC))

	CALL PGSCF(2)
	CALL PGASK (.FALSE.)
	CALL PGADVANCE
C---Top: surface brightness
	CALL PGVPORT (0.2, 0.8, 0.65, 0.9)
	CALL PGWINDOW (logdist(1), logdist(nbin), -6.0, 1.0)	
	CALL PGBOX ('BCLST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL (' ', 'log(sbr)', ' ')
	CALL PGPOINT (nbin, logdist, logsurf, cross)
C---Bot: Dispersion
	CALL PGVPORT (0.2, 0.8, 0.4, 0.65)
	CALL PGWINDOW (logdist(1), logdist(nbin), 
	1       0.1*axscale, 0.99*axscale)
	CALL PGBOX ('BCNLST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL ('Radius', 'Disp', ' ')
	CALL PGPOINT (nbin, logdist, disp, cross)
C---Label 
	   CALL PGSCH (0.8)
	   CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
	   CALL PGWINDOW (0., 1., 0., 1.)
	   CALL PGTEXT (0., 0.7, 'Surf Brightness and Dispersion profiles')
	   CALL PGTEXT (0., 0.6, infostr(1))
	   CALL PGTEXT (0., 0.5, infostr(2))
	   CALL PGSCH (1.0)

C---Second page: r1/4 plot
	CALL PGASK(.TRUE.)
	CALL PGADVANCE
	CALL PGVPORT (0.2, 0.8, 0.4, 0.8)
	CALL PGWINDOW(d14(1), d14(nbin), -8., 1.)
	CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL ('r**1/4', 'log(sbr)', ' ')
	CALL PGPOINT (nbin, d14, logsurf, cross)
C---Label
           CALL PGSCH (0.8)
           CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
           CALL PGWINDOW (0., 1., 0., 1.)
           CALL PGTEXT (0., 0.7, 'Surf Brightness profile')
           CALL PGTEXT (0., 0.6, infostr(1))
           CALL PGTEXT (0., 0.5, infostr(2))
           CALL PGSCH (1.0)

	CALL PGEND

C---Flush to printer
	CALL psflush(gdev)
 
	RETURN 
        END
