

C*****************************************************************************
 
 
                           SUBROUTINE vdist4
     &     (gdev,vel,wght, in_bin, coord, maxinbin, nbin, istart, iend,
     &	    theta, phi, GRAVC, infostr,keyw)
 
 
C*****************************************************************************
        IMPLICIT NONE
        REAL vel, wght, vrarray, vmax, vr, coord, vrhisto, hmax,
     &       theta, phi, GRAVC
        INTEGER maxinbin, nbin, nhisto, in_bin, istart, iend
        DIMENSION vel(maxinbin, nbin), wght(maxinbin, nbin),
     &            in_bin(nbin), coord(nbin)
        PARAMETER (nhisto = 40)
        DIMENSION vrarray(nhisto), vrhisto(nhisto)
        INTEGER ipix, ipix1, ipix2, i, j, n, ll
	CHARACTER*(*) gdev, infostr(*)
        DATA vmax /2./
        CHARACTER*4  KEYW
        CHARACTER*12 loslog

C-------
C  Assign abscissa velocity velocity values
C------
        DO 2 i = 1, nhisto/2
            vr = vmax * FLOAT(i) / ((nhisto)/2.)
            vrarray(nhisto / 2 + i)     =  vr
            vrarray(nhisto / 2 - i + 1) = -vr
 2      CONTINUE
 
cC--------
cC  Input pixel number from terminal, EXIT for input < 0.
cC-------
c     cambio esto para que me lo calcule como yo quiero
c 3      WRITE(6,100)
c 100    FORMAT('vdist>> slit first pixel, last pixel (quit: <1) ? : '$)
c        READ(5,*,ERR=3) ipix1, ipix2
cC       WRITE(6,*) '>> vdist:  ipix = ', ipix
c        IF(ipix1 .LE. 0 .OR. ipix1 .GT. nbin) THEN
c            CLOSE(10)
c            RETURN
c        END IF
 
C---------
C  zero histogram
C--------
        ipix1=1

        do 45 ll=1,10
        DO 10 i = 1,nhisto
                vrhisto(i) = 0.
 10     CONTINUE
 
C---------
C  build histogram
C--------

cc        ipix2=ipix1+2 
        if (ll.eq.1) then
           ipix1=1
           ipix2=4
        endif
        if (ll.eq.2) then
           ipix1=5
           ipix2=7
        endif
        if (ll.eq.3) then
           ipix1=8
           ipix2=8
        endif
        if (ll.eq.4) then
           ipix1=9
           ipix2=9
        endif
        if (ll.eq.5) then
           ipix1=10
           ipix2=10
        endif
        if (ll.eq.6) then
           ipix1=11
           ipix2=11
        endif

        if (ll.eq.7) then
           ipix1=12
           ipix2=12
        endif
        if (ll.eq.8) then
           ipix1=13
           ipix2=13
        endif
        if (ll.eq.9) then
           ipix1=14
           ipix2=16
        endif
        if (ll.eq.10) then
           ipix1=17
           ipix2=20
        endif


cccccccccccccc
   
       print*,ipix1,ipix2
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
c           ipix1=ipix1+5
cc           ipix1=ipix1+1
cc           if (ll.eq.1) then 
cc              ipix1=1
cc           end if   
c           print*,'hola',ipix1
c 45     CONTINUE   

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
c        IF (gdev(1:4) .EQ. 'file') THEN

CCC        OPEN(10,FILE='vdist.out',STATUS ='UNKNOWN',ACCESS='APPEND')
c           OPEN(10,FILE='vdist.out',STATUS ='UNKNOWN')
            call mklogname2(keyw,loslog)
            OPEN(10,FILE=loslog,STATUS ='UNKNOWN')
           WRITE(10,120) ipix1, ipix2, coord(ipix1), istart, iend
 120       FORMAT(/1x, 'pixels = ', 2I5, ' radius = ', F5.1,
     &         '  particles = ', 2I8/)
 
           DO 60 j = 1, nhisto
            WRITE(10, 130) vrarray(j), vrhisto(j)
 60        CONTINUE
 130       FORMAT(1x, F10.3, F10.3)

c	ELSE IF (gdev(1:1) .EQ. 't') THEN

c           WRITE(10,120) ipix1, ipix2, coord(ipix1), istart, iendc

c           DO 70 j = 1, nhisto
c            WRITE(10, 130) vrarray(j), vrhisto(j)
c 70        CONTINUE
c	ELSE IF (gdev(1:1) .EQ. '/') THEN
c	   CALL PGBEGIN (0, gdev, 1, 1)
c
c           CALL PGSCF(2)
c           CALL PGASK (.FALSE.)
c           CALL PGADVANCE
c           CALL PGVPORT (0.2, 0.8, 0.4, 0.65)
c	   CALL PGWINDOW (-vmax, vmax, 0., hmax)
c	   CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
c	   CALL PGLABEL ('vel', 'f(v)', 'Velocity Distribution')
c	   CALL PGLINE (nhisto, vrarray, vrhisto)
cC---Label
c           CALL PGSCH (0.8)
c           CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
c           CALL PGWINDOW (0., 1., 0., 1.)
c           CALL PGTEXT (0., 0.7, 'Tangential velocity distribution')
c           CALL PGTEXT (0., 0.6, infostr(1))
c           CALL PGTEXT (0., 0.5, infostr(2))
c           CALL PGSCH (1.0)
c
c           CALL PGEND
c	   CALL psflush(gdev)
c
c	ELSE
c	   PRINT*, 'device ?'
c	   RETURN
c	END IF
           close(10)
c        GO TO 3
  45     CONTINUE   
        END
