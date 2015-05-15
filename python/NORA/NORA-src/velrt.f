

C*****************************************************************************
 
 
               SUBROUTINE velrt (gdev, nbods, istart, iend, theta, phi,
     &				 PA, coordmax, swidth,
     &                           filename, model1, model2, modinc, 
     &				 add,ivdist, GRAVC, infostr)
 
 
C*****************************************************************************
 
 
        IMPLICIT NONE
 
	INCLUDE 'nora.def'

	LOGICAL add
	INTEGER nbods
        CHARACTER *40 filename
        INTEGER model1, model2, modinc, model, ivdist
        INTEGER istart, iend, nbin, nhalf, maxinbin, i, j, k
        PARAMETER (nbin=50, nhalf = nbin/2,  maxinbin = 40000)
                                                        ! want nbin even.
 
        REAL theta, phi, GRAVC
 
        INTEGER in_bin
        DIMENSION in_bin(nbin)
	CHARACTER*(*) infostr(*)
 
        REAL pi, radians, slength, swidth,
     &       PA, coordmax, coord(nbin), sinpa, cospa, MDIAN3,
     &       top, bot, logtop
 
        REAL vel, wght,
     &       vl(maxinbin), wt(maxinbin),
     &       vraw(nhalf), vsqr(nhalf), m_inbin(nhalf),
     &       xlog, weight
        DIMENSION vel(maxinbin, nhalf), wght(maxinbin, nhalf)
 
        REAL rot(nhalf), disp(nhalf), err(nhalf), m_smooth
	REAL errtop(nhalf), errbot(nhalf)
	CHARACTER*(*) gdev

        REAL r, th, ph, vr, vt, phiv, rnew, znew
        DIMENSION r(nmax), th(nmax), ph(nmax)
        DIMENSION vr(nmax), vt(nmax), phiv(nmax)
 
        DATA top, bot /10., 1./
 
	PI = ACOS(-1.0)
	radians = pi / 180.
 
        slength = 2. * coordmax
        sinPA = SIN (PA * radians)
        cosPA = COS (PA * radians)
 
C---------------
C  Create array of distances - logarithmically spaced
C---------------
        logtop = LOG10(top)
        CALL slit (coordmax, coord, nhalf, top, bot)

C-------------
C  Only positive coord needed for the radial dependence of vt
C------------
	DO 10 i = 1, nhalf
		coord(i) = coord(nhalf+i)
10	CONTINUE
 
C--------------
C  Find position in sphericals, radial and tangential velocities
C--------------
        CALL XYZtoRTP(istart, iend, x, y, z, r, th, ph)
        CALL VelToCyl
     &          (istart, iend, r, th, ph, vx, vy, vz, vr, vt, phiv)
 
C--------------
C  Zero accumulators
C--------------
        DO 20 i = 1, nhalf
                in_bin(i) = 0
                m_inbin(i) = 0.e0
                vraw(i) = 0.e0
                vsqr(i) = 0.e0
                rot(i) = 0.e0
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
	    rnew = r(i)
	    znew = r(i) * th(i)
 
C------ exclude if not within slit width
            IF (ABS(znew) .GT. (swidth/2.) ) GO TO 40
 
C------ exclude if beyond slit length
            IF (ABS(rnew) .GE. coordmax) GO TO 40
 
C------ logarithmic bins...
            xlog = log10(bot + ABS(rnew) * (top - bot) / coordmax)
            J = 1 + INT(FLOAT(nhalf) * xlog / logtop)
 
C------ exclude if beyond slit length
            IF (J .LT. 1 .OR. J .GT. nhalf) GO TO 40
 
C------ update...
            weight = pmass(i) / pmass(1)
            in_bin(J) = in_bin(J) + 1
            vel(in_bin(J), J)  = vt(i) * SIN(phiv(i))
            wght(in_bin(J), J) = weight
 
            IF (in_bin(J) .GT. maxinbin)
     &                  PRINT *,'rotv>> in_bin(',J,') : ', i
 
            m_inbin(J) = m_inbin(J) + weight
            vraw(J) = vraw(J) + weight * vt(i) * SIN(phiv(i))
 40     CONTINUE
CCC 41	CONTINUE
 
C------ If enough particles in bin, recalculate VRAD via MEDIAN
        DO 50 j = 1, nhalf
            IF (in_bin(j) .GE. 6) THEN
                DO 45 k = 1, in_bin(j)
                    vl(k) = vel(k,j)
                    wt(k) = wght(k,j)
 45             CONTINUE
C               WRITE(6,*) 'rotv>> j, in_bin(j) : ', j, in_bin(j)
C               WRITE(6,*) 'rotv>> vl(k) = ', (vl(k), k=1, in_bin(j))
C               WRITE(6,*) 'rotv>> wt(k) = ', (wt(k), k=1, in_bin(j))
                vraw(j) = MDIAN3 (vl, wt, in_bin(j))
                vraw(j) = m_inbin(j) * vraw(j)
            END IF
 50     CONTINUE
 
C----------
C  Find Dispersions
C---------
        DO 70 j = 1, nhalf
            DO 65 k = 1, in_bin(j)
                vsqr(j) = vsqr(j) + wght(k,j) *
     &                       (vel(k,j) - vraw(j)/m_inbin(j))**2
 65         CONTINUE
 70     CONTINUE
 
C------ Smooth radial velocities and dispersions
 
        DO 60 j = 3, nhalf - 2
            m_smooth = 3. * m_inbin(j) +
     &                 2. * m_inbin(j-1) + 2. * m_inbin(j+1) +
     &                 m_inbin(j-2) + m_inbin(j+2)
 
            rot(j)    = 3. * vraw(j) +
     &                 2. * vraw(j-1) + 2. * vraw(j+1) +
     &                 vraw(j-2) + vraw(j+2)
 
            disp(j)  = 3. * vsqr(j) +
     &                 2. * vsqr(j-1) + 2. * vsqr(j+1) +
     &                 vsqr(j-2) + vsqr(j+2)
 
          IF(m_smooth .LT. 1.e-7 .OR. in_bin(j) .EQ. 0) THEN
            rot(j)    = 100.
            disp(j)  = 100.
            err(j)   = 0.1
          ELSE
            rot(j)    = rot(j) / m_smooth
            disp(j) = SQRT(disp(j) / m_smooth)
            err(j)  = disp(j) / SQRT(FLOAT(in_bin(j)) )
          END IF
 60     CONTINUE
 
C       DO 75 j = 1, nhalf
C           WRITE(6,*) coord(j), rot(j), disp(j), err(j)
C 75    CONTINUE
 
C-----------
C  Output 
C-----------
CCC        IF (ivdist .EQ. 0) THEN
            CALL rotcout(gdev, coord, rot, disp, err, nhalf,
     &		    errtop, errbot,
     &              istart, iend, theta, phi, PA, coordmax, swidth,
     &              filename, model, GRAVC, infostr)
CCC        END IF
 
C-----------
C  Velocity distribution
C-----------
CCC        IF (ivdist .EQ. 1) THEN
            CALL vdist
     &      (gdev,
     &	     vel, wght, in_bin, coord, maxinbin, nhalf, istart, iend,
     &	     theta, phi, GRAVC, infostr)
CCC        END IF
 
        RETURN
        END
