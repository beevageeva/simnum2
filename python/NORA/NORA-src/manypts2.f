

C**************************************************************************


	      SUBROUTINE manypts2(model1, model2, modinc,
     &			istart2,iend2,after,iempieza,ifin,filename,
     &			gdev, istart, iend, theta, phi, coordmax,
     &			nx, ny,
     &			infostr) 


C**************************************************************************

	IMPLICIT NONE

	INCLUDE 'nora.def'

	INTEGER nbods, istart, iend, i 
	INTEGER istart2,iend2,after,iempieza,ifin
	INTEGER model1, model2, modinc, model
	INTEGER nx, ny, maxpage, ibox
	PARAMETER (maxpage = 12)
	REAL theta, phi, coordmax, xbuff, ybuff, vbuff
	DIMENSION xbuff(nmax), ybuff(nmax), vbuff(nmax)
	CHARACTER*(*) gdev, infostr(*), filename
	CHARACTER*4 modelstr,ttt
	REAL 	pagx1, pagx2, pagy1, pagy2, xl, xh, yl, yh
	INTEGER ix, iy

C--------
C  PGPLOT start, divide each page in (nx, ny) windows 
C  Inquire size of physical page in mm
C--------
        CALL PGBEG(0, gdev, 1, 1)
C	CALL PGQVP(2, pagx1, pagx2, pagy1, pagy2)
C	CALL PGASK(.TRUE.)

C--------
C  Interate over models
C--------
	ibox = 0

	DO 500 model = model1, model2, modinc
	    ibox = ibox + 1
	    ix = MOD(ibox - 1, nx) + 1
	    iy = (ibox - 1) / nx   + 1
	    iy = ny - iy + 1
	    WRITE(modelstr, '(I4)') model
	    CALL getmodel(model,filename,header,nbods,
     &                      x,y,z,vx,vy,vz,pmass)
	 
	    WRITE(ttt, '(I4)') INT(header(3))
C	    ttt=INT(header(3))
            CALL bodsreloc (istart2, iend2, after)
	    istart=iempieza
	    iend=ifin

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
C  Viewport normalized coordinates
C--------
	xl = (ix - 1  + 0.02 * FLOAT(nx)) / FLOAT(nx)
	xh = (ix      - 0.02 * FLOAT(nx)) / FLOAT(nx)
	yl = (iy      + 0.02 * FLOAT(ny)) / FLOAT(ny+1)
	yh = (iy + 1  - 0.02 * FLOAT(ny)) / FLOAT(ny+1)
CCC	PRINT *, ibox, ix, iy, xl, xh, yl, yh
C--------
C  PGPLOT points plot
C--------
	CALL PGSCF(2)
	CALL PGSCH (0.4)
	CALL PGVPORT (xl, xh, yl, yh)
C        CALL PGVPORT (0.15, 0.85, 0.15, 0.85)
        CALL PGWNAD (-coordmax, coordmax, -coordmax, coordmax)
        CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
        CALL PGPOINT(iend-istart+1, xbuff, ybuff, -1)
	CALL PGSCH (0.5)
C	CALL PGLABEL(' ', ' ', modelstr)
C	CALL PGSCH (2.)
	CALL PGMTXT ('B',2.8,0.52,1.0,ttt)

C	CALL PGLAB(' ', ' ', ttt)
	IF (ibox .EQ. nx*ny .OR. model .EQ. model2) THEN
C--------
C  Label
C--------
	        CALL PGSCH (0.8)
		CALL PGVPORT (0.1, 0.8, 0.050, 0.25)
        	CALL PGWINDOW (0., 1., 0., 1.)
		CALL PGTEXT (0., 0.7, 'particle map')
        	CALL PGTEXT (0., 0.6, infostr(1))
		CALL PGTEXT (0., 0.5, infostr(2))
        	CALL PGSCH (1.0)
	END IF
	IF (ibox .EQ. nx*ny .AND. model .NE. model2) THEN
		ibox = 0
		CALL PGPAGE
	END IF
C---------
C  End loop over models
C---------
500	CONTINUE

	CALL PGEND

C---Flush to printer
	CALL psflush (gdev)

	RETURN
	END
