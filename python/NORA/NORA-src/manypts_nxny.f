

C**************************************************************************


	      SUBROUTINE manypts(model1, model2, modinc,
     &			filename,
     &			gdev, istart, iend, theta, phi, coordmax,
     &			infostr) 


C**************************************************************************

	IMPLICIT NONE

	INCLUDE 'nora.def'

	INTEGER nbods, istart, iend, i 
	INTEGER model1, model2, modinc, model, nmodels
	INTEGER nx, ny, maxpage, ibox
	PARAMETER (maxpage = 12)
	REAL theta, phi, coordmax, xbuff, ybuff, vbuff
	DIMENSION xbuff(nmax), ybuff(nmax), vbuff(nmax)
	CHARACTER*(*) gdev, infostr(*), filename
	CHARACTER*5 modelstr

C-------
C  Find out number of x and y windows needed
C-------
	nmodels = (model2 - model1) / modinc + 1
	IF (nmodels .EQ. 1) THEN
		nx = 1
		ny = 1
	ELSE IF (nmodels .EQ. 2) THEN
		nx = 1
		ny = 2
	ELSE IF (nmodels .LE. 4) THEN
		nx = 2
		ny = 3
	ELSE IF (nmodels .LE. 6) THEN
                nx = 2
                ny = 3
	ELSE IF (nmodels .LE. 9) THEN
                nx = 3
                ny = 3
	ELSE 
                nx = 3
                ny = 4
	END IF

C--------
C  PGPLOT start, divide each page in (nx, ny) windows 
C--------
        CALL PGBEG(0, gdev, nx, ny)
	    CALL PGPAGE
	    CALL PGPAGE
C--------
C  Interate over models
C--------
	ibox = 0
	DO 500 model = model1, model2, modinc
	    WRITE(modelstr, '(A)') model
	ibox = ibox + 1
	    CALL getmodel(model,filename,header,nbods,
     &                      x,y,z,vx,vy,vz,pmass)

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
	CALL PGSCF(2)
        CALL PGVPORT (0.15, 0.85, 0.15, 0.85)
        CALL PGWNAD (-coordmax, coordmax, -coordmax, coordmax)
        CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
        CALL PGPOINT(iend-istart+1, xbuff, ybuff, -1)
	CALL PGLABEL(' ', ' ', modelstr)
	CALL PGPAGE
C---------
C  End loop over models
C---------
500	CONTINUE

C--------
C  Label
C--------
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
