

C*****************************************************************************
 
 
                SUBROUTINE rotcv(gdev, nbods, istart, iend,  theta, phi,
     &				 PA, coordmax, swidth,
     &                           filename, model1, model2, modinc, 
     &				 add,ivdist, GRAVC, infostr, keyw)
 
 
C*****************************************************************************
 
 
        IMPLICIT NONE
 
	INCLUDE 'nora.def'

	LOGICAL add
	INTEGER nbods
        CHARACTER *40 filename
        INTEGER model1, model2, modinc, model, ivdist
        INTEGER istart, iend, nbin, nhalf, maxinbin, i, j, k
c cambio nbin de 50 a 20 y luego no agrupo de 5 en 5 en vdist4
c cambio de nuevo a 50 y agrupo distinto para que sigan siendo 20 puntos.
        PARAMETER (nbin=50, nhalf = nbin/2,  maxinbin = 40000)
                                                        ! want nbin even.

        REAL theta, phi, GRAVC
 
        INTEGER in_bin
        DIMENSION in_bin(nbin)
	CHARACTER*(*) infostr(*),keyw(*)
 
        REAL xbuff, ybuff, vbuff, pi, radians, slength, swidth,
     &       PA, coordmax, coord(nbin), sinpa, cospa, MDIAN3,
     &       top, bot, logtop
        DIMENSION xbuff(nmax), ybuff(nmax), vbuff(nmax)
 
        REAL vel, wght,
     &       vl(maxinbin), wt(maxinbin),
     &       vrad(nbin), vsqr(nbin), m_inbin(nbin),
     &       xnew, ynew, xlog, weight
        DIMENSION vel(maxinbin, nbin), wght(maxinbin, nbin)
 
        REAL vr(nbin), disp(nbin), err(nbin), m_smooth
	REAL errtop(nbin), errbot(nbin)
	CHARACTER*(*) gdev
 
        DATA top, bot /10., 1./
 
 
        pi = ACOS(-1.)
        radians = pi / 180.
 
 
        slength = 2. * coordmax
        sinPA = SIN (PA * radians)
        cosPA = COS (PA * radians)
 
C---------------
C  Create array of distances - logarithmically spaced
C---------------
        logtop = LOG10(top)
        CALL slit (coordmax, coord, nhalf, top, bot)
 
CCC       WRITE(6,*) 'coord = ', coord
 
C--------------
C  Project system onto observing plane
C--------------
        CALL prject (istart, iend, theta, phi,
     &                          xbuff, ybuff, vbuff)
 
C--------------
C  Zero accumulators
C--------------
        DO 20 i = 1, nbin
                in_bin(i) = 0
                m_inbin(i) = 0.e0
                vrad(i) = 0.e0
                vsqr(i) = 0.e0
                vr(i) = 0.e0
                disp(i) = 0.e0
            DO 15 k = 1, maxinbin
                vel(k,i) = 0.e0
                wght(k,i) = 0.e0
 15         CONTINUE
 20     CONTINUE
 
C-------------
C  Add models if required
C------------
CCC	DO 41 model = model1, model2, modinc
CCC	    IF (add) THEN
CCC		CALL getmodel(model, filename, nbods)
CCC		CALL prject(istart, iend, theta, phi, 
CCC     &			    xbuff, ybuff, vbuff)
CCC	    END IF
	    
C-------------
C  Locate particles along slit
C------------
        DO 40 i = istart, iend
            xnew =  xbuff(i) * cosPA + ybuff(i) * sinPA
            ynew = -xbuff(i) * sinPA + ybuff(i) * cosPA
 
C------ exclude if not within slit width
            IF (ABS(ynew) .GT. (swidth/2.) ) GO TO 40
 
C------ exclude if beyond slit length
            IF (ABS(xnew) .GE. coordmax) GO TO 40
 
C------ logarithmic bins...
            xlog = log10(bot + ABS(xnew) * (top - bot) / coordmax)
            J = 1 + INT(FLOAT(nhalf) * xlog / logtop)
 
            IF (xnew .GT. 0.e0) THEN
                J = nhalf + J
            ELSE
                J = nhalf - J + 1
            END IF
 
C------ exclude if beyond slit length
            IF (J .LT. 1 .OR. J .GT. nbin) GO TO 40
 
C------ update...
            weight = pmass(i) / pmass(1)
            in_bin(J) = in_bin(J) + 1
            vel(in_bin(J), J)  = vbuff(i)
            wght(in_bin(J), J) = weight
 
c            IF (in_bin(J) .GT. maxinbin)
c     &                  PRINT *,'rotv>> in_bin(',J,') : ', i
 
            m_inbin(J) = m_inbin(J) + weight
            vrad(J) = vrad(J) + weight * vbuff(i)
 40     CONTINUE
CCC 41	CONTINUE
 
C------ If enough particles in bin, recalculate VRAD via MEDIAN
        DO 50 j = 1, nbin
            IF (in_bin(j) .GE. 6) THEN
                DO 45 k = 1, in_bin(j)
                    vl(k) = vel(k,j)
                    wt(k) = wght(k,j)
 45             CONTINUE
C               WRITE(6,*) 'rotv>> j, in_bin(j) : ', j, in_bin(j)
C               WRITE(6,*) 'rotv>> vl(k) = ', (vl(k), k=1, in_bin(j))
C               WRITE(6,*) 'rotv>> wt(k) = ', (wt(k), k=1, in_bin(j))
                vrad(j) = MDIAN3 (vl, wt, in_bin(j))
                vrad(j) = m_inbin(j) * vrad(j)
            END IF
 50     CONTINUE
 
C----------
C  Find Dispersions
C---------
        DO 70 j = 1, nbin
            DO 65 k = 1, in_bin(j)
                vsqr(j) = vsqr(j) + wght(k,j) *
     &                       (vel(k,j) - vrad(j)/m_inbin(j))**2
 65         CONTINUE
 70     CONTINUE
 
C------ Smooth radial velocities and dispersions
 
        DO 60 j = 3, nbin - 2
            m_smooth = 3. * m_inbin(j) +
     &                 2. * m_inbin(j-1) + 2. * m_inbin(j+1) +
     &                 m_inbin(j-2) + m_inbin(j+2)
 
            vr(j)    = 3. * vrad(j) +
     &                 2. * vrad(j-1) + 2. * vrad(j+1) +
     &                 vrad(j-2) + vrad(j+2)
 
            disp(j)  = 3. * vsqr(j) +
     &                 2. * vsqr(j-1) + 2. * vsqr(j+1) +
     &                 vsqr(j-2) + vsqr(j+2)
 
          IF(m_smooth .LT. 1.e-7 .OR. in_bin(j) .EQ. 0) THEN
            vr(j)    = 100.
            disp(j)  = 100.
            err(j)   = 0.1
          ELSE
            vr(j)    = vr(j) / m_smooth
            disp(j) = SQRT(disp(j) / m_smooth)
            err(j)  = disp(j) / SQRT(FLOAT(in_bin(j)) ) / SQRT(5.)
          END IF
 60     CONTINUE
 
C       DO 75 j = 1, nbin
C           WRITE(6,*) coord(j), vr(j), disp(j), err(j)
C 75    CONTINUE
 
C----------
C  Exclude two outermost bins on each side.
C---------
        DO 80 j = 1, nbin - 4
            coord(j) = coord(j + 2)
            vr(j)    = vr(j + 2)
            disp(j)  = disp(j + 2)
            err(j)   = err(j + 2)
 80     CONTINUE
C-----------
C  Plotting 
C-----------
        IF (ivdist .EQ. 0) THEN
            CALL rotcout(gdev, coord, vr, disp, err, nbin - 4,
     &		    errtop, errbot,
     &              istart, iend, theta, phi, PA, coordmax, swidth,
     &              filename, model, GRAVC, infostr)
        END IF
 
C-----------
C  Velocity distribution
C-----------
        IF (ivdist .EQ. 1) THEN
c           nbin=100
            CALL vdist4
     &      (gdev, vel, wght, in_bin, coord, maxinbin, nbin, 
     &       istart, iend,
     &	     theta, phi, GRAVC, infostr,keyw)
        END IF
 
        RETURN
        END
