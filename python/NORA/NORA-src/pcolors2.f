
C**************************************************************************


	      SUBROUTINE pcolors2(gdev, istart, imed1,iend, 
     &	theta, phi, coordmax,infostr) 
CC
C       MOdificacion de points para que admita colores segun el tipo de 
C       particulas.                   A.C.G.G. (16/6/2000)
C
C**************************************************************************

	IMPLICIT NONE

	INCLUDE 'nora.def'

	INTEGER istart, iend, imed1,i,n,col
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
        col=2
        CALL PGBEG(0, gdev, 1, 1)        
	CALL PGSCF(2)
        CALL PGVPORT (0.2, 0.8, 0.35, 0.95)
        CALL PGWNAD (-coordmax, coordmax, -coordmax, coordmax)
        CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
	DO 100 n=istart, imed1
C	   col=INT(pmass(n)*(1.E5))
C	    if (pmass(n).ne.pmass(n-1)) then
C	       col=col+1
CINT(pmass(n)*(1.E5))
C	    end if   
C	    col=col

        CALL PGSCI(col)
        CALL PGPOINT(imed1-istart+1, xbuff(n), ybuff(n), -1)

 100	continue
	   col=3
	DO 105 N=imed1+1,iend
        CALL PGSCI(col)
        CALL PGPOINT(iend-imed1, xbuff(n), ybuff(n), -1)

 105	continue

C        DO 100 n=istart,header(102)
C              col=col
C	CALL PGSCI(col)
C        CALL PGPOINT(header(102), xbuff(n), ybuff(n), -1)
C 100    CONTINUE
C        DO 200 n=header(102)+1,header(104)
C              col=col
C	CALL PGSCI(col)
C        CALL PGPOINT(header(104)+1-header(102), xbuff(n), ybuff(n), -1)
C 200    CONTINUE
C        DO 300 n=header(104)+1,header(106)
C              col=col
C	CALL PGSCI(col)
C        CALL PGPOINT(header(106)+1-header(104), xbuff(n), ybuff(n), -1)
C 300    CONTINUE
C        DO 400 n=header(106)+1,header(108)
C              col=col
C	CALL PGSCI(col)
C        CALL PGPOINT(header(108)+1-header(106), xbuff(n), ybuff(n), -1)
C 400    CONTINUE
C        DO 500 n=header(108)+1,header(110)
C              col=col
C	CALL PGSCI(col)
C        CALL PGPOINT(header(110)+1-header(108), xbuff(n), ybuff(n), -1)
C 500    CONTINUE
C        DO 700 n=header(110)+1,header(112)
C              col=col
C	CALL PGSCI(col)
C        CALL PGPOINT(header(112)+1-header(110), xbuff(n), ybuff(n), -1)
C 700    CONTINUE
        
CCC        CALL PGPOINT(nmax, xbuff, ybuff, -1)

C--------
C  Label
C--------
	col=1
        CALL PGSCI(col)
        CALL PGSCH (0.8)
        CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
        CALL PGWINDOW (0., 1., 0., 1.)
        CALL PGTEXT (0., 0.7, 'particle map')
        CALL PGTEXT (0., 0.6, infostr(1))
        CALL PGTEXT (0., 0.5, infostr(2))
        CALL PGSCH (1.0)

	CALL PGEND

C---Flush to printer
	CALL psflush (gdev)

	RETURN
	END
