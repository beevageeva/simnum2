

C**************************************************************************


	      SUBROUTINE ptplot(gdev, istart, iend, theta, phi, coordmax,
     &				infostr) 


C**************************************************************************

	IMPLICIT NONE

	INCLUDE 'nora.def'

	INTEGER istart, iend, i
	REAL theta, phi, coordmax, xbuff, ybuff, vbuff
	DIMENSION xbuff(nmax), ybuff(nmax), vbuff(nmax)
	CHARACTER*(*) gdev, infostr(*)

C-------
C  Zero arrays
C-------
	DO 10 i = 1, nmax
	    xbuff(i) = 0.0
	    ybuff(i) = 0.0
	    vbuff(i) = 0.0
10	CONTINUE

C-------
C  Project onto observing plane
C-------
	CALL prject(istart, iend, theta, phi, xbuff, ybuff, vbuff)

C-------
C  Prepare arrays for plotting
C-------
	DO 20 i = 1, iend-istart+1
	    xbuff(i) = xbuff(istart+i-1)
	    ybuff(i) = ybuff(istart+i-1)
	    vbuff(i) = vbuff(istart+i-1)
20	CONTINUE
C--------
C  PGPLOT points plot
C--------

        CALL PGBEGIN(0, gdev, 1, 1)        
	CALL PGSCF(2)
C        CALL PGVPORT (0.2, 0.8, 0.35, 0.95)
       CALL PGVPORT (0.2, 0.8, 0.35, 0.75)

        CALL PGWNAD (-coordmax, coordmax, -coordmax, coordmax)
        CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
C	CALL PGSCI(2)
        CALL PGPOINT(iend-istart+1, xbuff, ybuff, -1)
CCC        CALL PGPOINT(nmax, xbuff, ybuff, -1)

C--------
C  Label
C--------
c       comento esto temporalmente!!!
C        CALL PGSCH (0.8)
C        CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
C        CALL PGWINDOW (0., 1., 0., 1.)
C        CALL PGTEXT (0., 0.7, 'particle map')
C        CALL PGTEXT (0., 0.6, infostr(1))
C        CALL PGTEXT (0., 0.5, infostr(2))
C        CALL PGSCH (1.0)

	CALL PGEND

C---Flush to printer
	CALL psflush (gdev)

	RETURN
	END
