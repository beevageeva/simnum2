

C*****************************************************************************
 
 
                           SUBROUTINE vdist
     &     (gdev,vel,wght, in_bin, coord, maxinbin, nbin, istart, iend,
     &	    theta, phi, GRAVC, infostr)
 
 
C*****************************************************************************
        IMPLICIT NONE
        REAL vel, wght, vrarray, vmax, vr, coord, vrhisto, hmax,
     &       theta, phi, GRAVC
        INTEGER maxinbin, nbin, nhisto, in_bin, istart, iend
        DIMENSION vel(maxinbin, nbin), wght(maxinbin, nbin),
     &            in_bin(nbin), coord(nbin)
        PARAMETER (nhisto = 40)
        DIMENSION vrarray(nhisto), vrhisto(nhisto)
        INTEGER ipix, ipix1, ipix2, i, j, n
	CHARACTER*(*) gdev, infostr(*)
        DATA vmax /2./

C-------
C  Assign abscissa velocity velocity values
C------
        DO 2 i = 1, nhisto/2
            vr = vmax * FLOAT(i) / ((nhisto)/2.)
            vrarray(nhisto / 2 + i)     =  vr
            vrarray(nhisto / 2 - i + 1) = -vr
 2      CONTINUE
 
C--------
C  Input pixel number from terminal, EXIT for input < 0.
C-------
 3      WRITE(6,100)
 100    FORMAT('vdist>> slit first pixel, last pixel (quit: <1) ? : ',$)
        READ(5,*,ERR=3) ipix1, ipix2
C       WRITE(6,*) '>> vdist:  ipix = ', ipix
        IF(ipix1 .LE. 0 .OR. ipix1 .GT. nbin) THEN
            CLOSE(10)
            RETURN
        END IF
 
C---------
C  zero histogram
C--------
        DO 10 i = 1,nhisto
                vrhisto(i) = 0.
 10     CONTINUE
 
C---------
C  build histogram
C--------
	DO 40 ipix = ipix1, ipix2
        DO 30 n = 1, in_bin(ipix)
            vr = vel(n,ipix)
            J = INT(ABS(vr) / vmax * (nhisto / 2)) + 1
            IF (J .GT. nhisto / 2) GO TO 30
            IF (vr .GT. 0.) THEN
                    J = nhisto / 2 + J
                ELSE IF (vr .LT. 0.) THEN
                    J = nhisto / 2 - J + 1
            END IF
            vrhisto(J) = vrhisto(J) + wght(n, ipix)
 30     CONTINUE
 40	CONTINUE

C-----------
C  Maximum of histogram
C-----------
	hmax = 0.0
	DO 50 j = 1, nhisto
	    hmax = MAX(vrhisto(j), hmax)
 50	CONTINUE

C-----------
C  Output to file
C-----------
        IF (gdev(1:4) .EQ. 'file') THEN

CCC        OPEN(10,FILE='vdist.out',STATUS ='UNKNOWN',ACCESS='APPEND')
           OPEN(10,FILE='vdist.out',STATUS ='UNKNOWN')
 
           WRITE(10,120) ipix1, ipix2, coord(ipix1), istart, iend
 120       FORMAT(/1x, 'pixels = ', 2I5, ' radius = ', F5.1,
     &         '  particles = ', 2I8/)
 
           DO 60 j = 1, nhisto
            WRITE(10, 130) vrarray(j), vrhisto(j)
 60        CONTINUE
 130       FORMAT(1x, F10.3, F10.3)

	ELSE IF (gdev(1:1) .EQ. 't') THEN

           WRITE(10,120) ipix1, ipix2, coord(ipix1), istart, iend

           DO 70 j = 1, nhisto
            WRITE(10, 130) vrarray(j), vrhisto(j)
 70        CONTINUE
	ELSE IF (gdev(1:1) .EQ. '/') THEN
	   CALL PGBEGIN (0, gdev, 1, 1)

           CALL PGSCF(2)
           CALL PGASK (.FALSE.)
           CALL PGADVANCE
           CALL PGVPORT (0.2, 0.8, 0.4, 0.65)
	   CALL PGWINDOW (-vmax, vmax, 0., hmax)
	   CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
	   CALL PGLABEL ('vel', 'f(v)', 'Velocity Distribution')
	   CALL PGLINE (nhisto, vrarray, vrhisto)
C---Label
           CALL PGSCH (0.8)
           CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
           CALL PGWINDOW (0., 1., 0., 1.)
           CALL PGTEXT (0., 0.7, 'Tangential velocity distribution')
           CALL PGTEXT (0., 0.6, infostr(1))
           CALL PGTEXT (0., 0.5, infostr(2))
           CALL PGSCH (1.0)

           CALL PGEND
	   CALL psflush(gdev)

	ELSE
	   PRINT*, 'device ?'
	   RETURN
	END IF
 
        GO TO 3
 
        END
