 
 
 
C*****************************************************************************
 
 
        SUBROUTINE densout (gdev, dist, inbin, dens, dispR, dispT, Rm,
     &              FRM, totm, GRAVC, infostr)
 
 
C*****************************************************************************
 
 
        IMPLICIT NONE
	REAL dist, dens, dispR, dispT, Rm, FRM, logdist, logdens
	REAL totm, GRAVC
        INTEGER inbin, nbin, i, k, axscale
        INTEGER uout
	PARAMETER(nbin=40)
	CHARACTER*(*) gdev
	CHARACTER*(*) infostr(*)
 
        DIMENSION dist(*), inbin(*), dens(*), dispR(*), dispT(*),
     &		Rm(*), FRM(*), logdist(nbin), logdens(nbin)

	INTEGER cross, square, triangle
	DATA cross, square, triangle /5, 6, 7/

	uout = 18
 
C----------
C  Logarithms
C----------
	DO 5 i = 1, nbin
		logdist(i) = LOG10(dist(i))
		logdens(i) = LOG10(dens(i))
5	CONTINUE

C---------
C  Open output device
C---------
        IF (gdev(1:4) .EQ. 'file') THEN
	    OPEN(uout,FILE='dens.out',STATUS='UNKNOWN')

	    WRITE(uout, '(A)') '#'
	    WRITE(uout, '(A)') '#'//infostr(1)(1:79)
    	    WRITE(uout, '(A)') '#'//infostr(2)(1:79)

	    WRITE(uout, '(A)') '#'
	    WRITE(uout,*)	    '#totm = ', totm
	    WRITE(uout,1020) (FRM(k), k=1,8)
	    WRITE(uout,1030) (Rm(k),  k=1,8)
	    WRITE(uout,1020) (FRM(k), k=9,16)
	    WRITE(uout,1030) (Rm(k),  k=9,16)
1020	    FORMAT('#FRM : ', 8F9.2)
1030	    FORMAT('#Rm  : ', 8(1PE9.2))

	    WRITE(uout, '(A)') ' '
            WRITE(uout,1005)
 1005       FORMAT('#',1x, 'Radius', 5x, 'particles',
     1         5x, 'Dens', 5x, 'DispR', 5x, 'DispT')
 
            DO 10 i = 1, nbin
                WRITE(uout, '(1pe12.4, I8, 3(1pe12.4))')
     1          dist(i), inbin(i), dens(i), dispR(i), dispT(i)
 10         CONTINUE
	    CLOSE(uout)
            RETURN

        ELSE IF (gdev(1:1) .EQ. 't') THEN
	    WRITE(6,*)	    '#totm = ', totm
	    WRITE(6,1020) (FRM(k), k=1,8)
	    WRITE(6,1030) (Rm(k),  k=1,8)
	    WRITE(6,1020) (FRM(k), k=9,16)
	    WRITE(6,1030) (Rm(k),  k=9,16)

	    WRITE(6,'(A)') ' ' 
            WRITE(6,1005)
	    DO 25 i = 1, nbin
	        WRITE(6,'(1pe12.4, I8, 3(1pe12.4))') 
     1			dist(i), inbin(i), dens(i), dispR(i), dispT(i)
 25	    CONTINUE
	    RETURN

        ELSE IF (gdev(1:2) .EQ. 'Rm') THEN
	    IF (gdev(3:6) .EQ. 'file') THEN
		OPEN(uout, FILE='Rm.out',STATUS='UNKNOWN',ACCESS='APPEND')
	    ELSE IF (gdev(3:6) .EQ. 'term') THEN
		uout = 6
	    END IF
	    WRITE(uout, '(A)') '#'
	    WRITE(uout, '(A)') '#'//infostr(1)(1:79)
	    WRITE(uout, '(A)') '#'//infostr(2)(1:79)
	    WRITE(uout,'((A),1pe10.3)')   '#'// 'totm = ', totm
	    WRITE(uout,1020) (FRM(k), k=1,8)
	    WRITE(uout,1030) (Rm(k),  k=1,8)
	    WRITE(uout,1020) (FRM(k), k=9,16)
	    WRITE(uout,1030) (Rm(k),  k=9,16)
	    IF (gdev(3:6) .EQ. 'file') CLOSE (uout)
	    gdev = '/XDISP'
	    RETURN

        ELSE 
	    CALL PGBEGIN (0, gdev, 1, 1)
        END IF

CCC 1010       FORMAT(1pe12.4, I8, 3(1pe12.4))
 1010       FORMAT(e12.4, I8, 3(e12.4))
C------
C  Plot
C-------

C----Scale for velocity plot axes as INT(SQRT(GRAVC))
	axscale = INT(2. * SQRT(GRAVC))
	CALL PGSCF(2)
	CALL PGASK (.FALSE.)
	CALL PGADVANCE
C---Top: Mass density profile
	CALL PGVPORT (0.2, 0.8, 0.65, 0.9)
	CALL PGWINDOW (logdist(1), logdist(nbin), -7.0, 3.0)	
	CALL PGBOX ('BCLST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL (' ', 'log(dens)', ' ')
	CALL PGPOINT (nbin, logdist, logdens, cross)
C---Bot: Radial and Tangential Dispersions
	CALL PGVPORT (0.2, 0.8, 0.4, 0.65)
	CALL PGWINDOW (logdist(1), logdist(nbin), 
	1	0., 0.99*axscale)
	CALL PGBOX ('BCNLST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL ('Radius', 'Disp', ' ')
	CALL PGPOINT (nbin, logdist, dispR, triangle)
	CALL PGPOINT (nbin, logdist, dispT, cross)
C---Label 
	   CALL PGSCH (0.8)
	   CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
	   CALL PGWINDOW (0., 1., 0., 1.)
	   CALL PGTEXT (0., 0.7, 'Vol Density and Dispersion profiles')
	   CALL PGTEXT (0., 0.6, infostr(1))
	   CALL PGTEXT (0., 0.5, infostr(2))
	   CALL PGSCH (1.0)

	CALL PGEND

C---Flush to printer
	CALL psflush(gdev)
 
	RETURN 
        END
