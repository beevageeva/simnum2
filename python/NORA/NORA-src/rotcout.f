 
 
 
C*****************************************************************************
 
 
                SUBROUTINE rotcout(gdev, coord, vr, disp, err, nbin,
     &		    errtop, errbot,
     &              istart, iend, theta, phi, PA, coordmax, swidth,
     &              filename, model, GRAVC, infostr)
 
 
C*****************************************************************************
 
 
        IMPLICIT NONE
 
        REAL coord, vr, disp, err, theta, phi, PA, coordmax, swidth
        INTEGER istart, iend, nbin, j, model, i
        INTEGER urot
        CHARACTER*40 filename
	PARAMETER(urot=18)
	CHARACTER*(*) gdev
	CHARACTER*(*) infostr(*)
 
        DIMENSION coord(*), vr(*), disp(*), err(*)
	INTEGER cross, square, triangle, axscale
	REAL errtop, errbot, GRAVC
	DIMENSION errtop(*), errbot(*)

	DATA cross, square, triangle /5, 6, 7/
 
C---------
C  Open output device
C---------
        IF (gdev(1:4) .EQ. 'file') THEN
CCC	    OPEN(urot,FILE='rotc.out',STATUS='UNKNOWN',ACCESS='APPEND')
	    OPEN(urot,FILE='rotc.out',STATUS='UNKNOWN')
	    DO 20 j = 1, nbin
	      WRITE(urot,'(4F15.4)') coord(j), vr(j), disp(j), err(j)
 20	    CONTINUE
	    CLOSE(urot)
            RETURN
        ELSE IF (gdev(1:1) .EQ. 't') THEN
	    DO 25 j = 1, nbin
	      WRITE(6,'(4F15.4)') coord(j), vr(j), disp(j), err(j)
 25	    CONTINUE
	    RETURN
        ELSE 
	    CALL PGBEGIN (0, gdev, 1, 1)
        END IF
C------
C  Plot
C-------
 
C----Scale for velocity plot axes as INT(SQRT(GRAVC)) -cambio para DEVA 1.5
C        axscale = INT(300*SQRT(GRAVC))
        axscale = INT(SQRT(GRAVC))
	CALL PGSCF(2)
	CALL PGASK (.FALSE.)
	CALL PGADVANCE
C---Rotation
	CALL PGVPORT (0.2, 0.8, 0.4, 0.65)
	CALL PGWINDOW (-coordmax, coordmax, 
	1	-0.8*axscale, 0.7999*axscale)	
	CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL ('Slit', 'Vel', ' ')
	CALL PGPOINT (nbin, coord, vr, cross)
	DO 30 i = 1, nbin
	    errtop(i) = vr(i) + err(i)
	    errbot(i) = vr(i) - err(i)
30	CONTINUE
	CALL PGERRY (nbin, coord, errtop, errbot, 1.)
C---Dispersion
	CALL PGVPORT (0.2, 0.8, 0.65, 0.9)
	CALL PGWINDOW (-coordmax, coordmax, 
	1	0.001*axscale, 1.0*axscale)	
	CALL PGBOX ('BCST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL (' ', 'Disp', 'Rotation Curve')
	CALL PGPOINT (nbin, coord, disp, cross)
	DO 40 i = 1, nbin
	    errtop(i) = disp(i) + err(i)
	    errbot(i) = disp(i) - err(i)
40	CONTINUE
	CALL PGERRY (nbin, coord, errtop, errbot, 1.)
C---Label hardcopy plot
CCC	IF (gdev .EQ. '/PS' .OR. gdev .EQ. '/VPS'
CCC  CCC   &  .OR. gdev .EQ. '/CPS' .OR. gdev .EQ. '/VCPS') THEN
	   CALL PGSCH (0.8)
	   CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
	   CALL PGWINDOW (0., 1., 0., 1.)
	   CALL PGTEXT (0., 0.7, 'rot curve')
	   CALL PGTEXT (0., 0.6, infostr(1))
	   CALL PGTEXT (0., 0.5, infostr(2))
	   CALL PGSCH (1.0)
CCC	END IF

	CALL PGEND

C---Flush to printer
	CALL psflush (gdev)
 
	RETURN 
        END
