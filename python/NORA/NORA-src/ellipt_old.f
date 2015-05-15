C***********************************************************************

               SUBROUTINE ellipt (istart, iend, coordmax)

C***********************************************************************


       IMPLICIT NONE

       INCLUDE 'nora.def'

	INTEGER nbin
	PARAMETER (nbin=40)
	REAL dist(nbin), logdist(nbin), dlogr

        INTEGER n, i, k, istart, iend, inbin(nbin), nbods, l
        REAL rho(nbin), Mr(nbin),
     1           totm,
     1          Rm(16), FRM(16), coordmax, eps, PI
C	CHARACTER*(*) gdev, infostr(*)
 
	REAL r, th, ph, vr, vt, phiv, rn, GRAVC
	DIMENSION r(nmax), th(nmax), ph(nmax)
	DIMENSION vr(nmax), vt(nmax), phiv(nmax)
        REAL A, B, maw, miw, theta, phi, mayaxis, equis, ygrec, zeta
        REAL BICHO, theta2, phi2, minaxis, ELLIP, ma(3000), mi(nmax)
        REAL DR1, DT1, DP1, DR2, DT2, DP2, DEN, DEN2(nmax), epss
        REAL DR(nmax),DT(nmax), DP(nmax),DDR(nmax),DDT(nmax), DDP(nmax)

	PI = ACOS(-1.)
C--------------
C  Create linearly-spaced grid of log-r values between eps and rmax
C--------------
	eps = 0.1
        CALL loggrid(coordmax, eps, nbin, logdist, dlogr)

        DO 50 i = 1, nbin
                dist(i) = 10 ** logdist(i)
 50     CONTINUE

C---------
C  Total mass
C--------
	nbods = header(1)
	totm = 0.0
	DO 5 n = istart, iend
	    totm = totm + pmass(n)
 5      CONTINUE
 
C----------
C  Fill spherical coordinate arrays: r, COS(theta), phi, 
C---------
	CALL XYZtoRTP (istart, iend, x, y, z, r, th, ph)
	CALL VelToCyl (istart, iend, r, th, ph, vx, vy, vz, vr, vt, phiv)

C--------------
        
        DO 10 i = 1, nbin
           rho(i) = 0.e0

 10     CONTINUE

        DO 20 n= istart, iend
           rn = LOG10 (r(n))
                IF (rn .LT. logdist(1)) THEN
                        i = 1
                ELSE
                        i = 2 + INT((rn - logdist(1)) / dlogr)
                END IF

                IF (i .GT. nbin) GO TO 20

                inbin(i) = inbin(i) + 1
                rho(i)  = rho(i)  + pmass(n)

 20     CONTINUE



C---------------
C  Find the radii containing 10%, 20%, 30%,... of the galaxy mass. 
C  This has to be done before rho is divided by the volume of each bin
C  Store in array Rm(16)
C---------------
	Mr(1) = rho(1)
	DO 28 i = 2, nbin
	    Mr(i) = Mr(i-1) + rho(i)
28	CONTINUE
        CALL findRm(totm, dist, Mr, Rm, FRM, nbin)
 
C---------------
C  Find mass per unit volume for each radial bin
C  Protect against rho=0 in empty bins
C---------------
C        DO 30 i = 1, nbin
C                rho(i) = rho(i) / 
C     1               (4.e0 * PI * (dist(i)**3 - dist(i-1)**3) / 3.e0)
C		IF (rho(i) .EQ. 0.) rho(i) = 1.e-10
C 30     CONTINUE


C----------------------------------

        A=Rm(7)
        B=Rm(8)
        
C        ma=0.0
        maw=0.0
        mayaxis=0.0
        theta=0.0
        phi=0.0

C        mi=0.0
        miw=0.0
        minaxis=0.0
        theta2=0.0
        phi2=0.0

C        ma=0.0

        DO 40 k= istart, iend

           IF (r(k).GE.A .AND. r(k).LE.B) THEN
           DO 45 l= 1,3000
   
             ma(l)=SQRT(r(k)**2)
               IF (ma(l).GT.maw) THEN
               maw=SQRT(r(k)**2)
                 mayaxis=r(k)
                 theta=ACOS(th(k))
                 phi=ph(k)
                 GO TO 45

               ELSE 
                 GO TO 45
               END IF
 45         CONTINUE  

             GO TO 40
  
           ELSE
             GO TO 40
  
           END IF
          
 40     CONTINUE   


        equis=SIN(theta)*COS(phi)
        ygrec=SIN(theta)*COS(phi)
        zeta =COS(theta)
        miw=maw
C-----------------------------------------------------------------------
C Ahora ya tengo el semieje mayor, voy a calcular la densidad en torno  
C a el.
C-----------------------------------------------------------------------

        epss=0.1

        DR1=maw-epss
        DT1=theta-epss
        DP1=phi-epss

        DR2=maw+epss
        DT2=theta+epss
        DP2=phi+epss
        
        DEN=0.0

        DO 70 k= istart, iend

           IF (SQRT(r(k)**2).GT.DR1 .AND.SQRT(r(k)**2).LT.DR2) THEN

              IF (ACOS(th(k)).GT.DT1 .AND. ACOS(th(k)).LT.DT2) THEN
                 
                 IF (ph(k).GT.DP1 .AND. ph(k).LT.DP2) THEN

                     DEN=DEN+1.0*pmass(k)
                 ELSE
                    GO TO 70
                 END IF   
              ELSE
                 GO TO 70
              END IF   
            ELSE     
              GO TO 70
           END IF
           
C           DEN = DEN/((2*epss)**3)

 70     CONTINUE

        

C-----------------------------------------------------------------------
        DO 500 k= istart, iend
           
C           IF (r(k).GE.A .AND. r(k).LE.B) THEN
           
              BICHO=((equis*SIN(ACOS(th(k)))*COS(ph(k)))+
     &              (ygrec*SIN(ACOS(th(k)))*SIN(ph(k)))+(zeta*th(k)))

              IF (BICHO.LT.0.01 .AND. r(k).LT.Rm(8)) THEN

                 DR(k)=SQRT((r(k))**2)-epss
                 DT(k)=ACOS(th(k))-epss
                 DP(k)=ph(k)-epss
                 DDR(k)=SQRT((r(k))**2)+epss
                 DDT(k)=ACOS(th(k))+epss
                 DDP(k)=ph(k)+epss
                 DEN2(k)=0.0
               
                 DO 600 l= istart, iend
C                    DEN2(k)=0.0
                    
                    IF (SQRT(r(l)**2).GT.DR(k) .AND. 
     &              SQRT(r(l)**2).LT.DDR(k)) THEN 

                       IF (ACOS(th(l)).GT.DT(k) .AND. 
     &                      ACOS(th(l)).LT.DDT(k)) THEN

                          IF (ph(l).GT.DP(k) .AND. ph(l).LT.DDP(k)) THEN

                             DEN2(k)=DEN2(k)+1.0*pmass(k)
                          ELSE
                             GO TO 600
                          END IF
                       ELSE
                          GO TO 600
                       END IF
   
                    ELSE
                       GO TO 600
                    END IF
 600             CONTINUE

C                 DEN2(k)=DEN2(k)/((2*epss)**3)

                 IF (DEN2(k).GT.(DEN*0.99) .AND. 
     &               DEN2(k).LT.(DEN*1.01)) THEN
                     
C                     DO 55 j=1,300
                     mi(k)=SQRT(r(k)**2)

                       IF (mi(k).LT.miw .AND. mi(k).GT.(0.3*maw)) THEN
                            miw=SQRT(r(k)**2)
                            minaxis=r(k)
                            theta2=ACOS(th(k))
                            phi2=ph(k)
                            GO TO 500
                        ELSE
                            GO TO 500
                        END IF
    
 55                  CONTINUE   

                 ELSE
                    GO TO 500

                 END IF   
                 
             ELSE 
                GO TO 500
C                 DO 55 l= 1,300
C                   mi(l)=SQRT(r(k)**2)
C                        IF (mi(l).GT.miw) THEN
C                            miw=SQRT(r(k)**2)
C                            minaxis=r(k)
C                            theta2=ACOS(th(k))
C                            phi2=ph(k)
C                            GO TO 55
C                        ELSE
C                            GO TO 55

C                        END IF
C 55              CONTINUE
C                 GO TO 500
                 
C              ELSE
C                 GO TO 500
C              END IF

C              GO TO 500
C           ELSE

C              GO TO 500

           END IF

      
 500     CONTINUE


        ELLIP=1.0-(miw/maw)

        WRITE(6,2010)
        WRITE(6,2011)

        WRITE(6,2012) ELLIP, mayaxis, theta*(180/PI), phi*(180/PI), 
     &      minaxis, theta2*(180/PI), phi2*(180/PI), DEN

C        WRITE(6,2013)


 2010   FORMAT(1H0,30X,'ELLIPTICITY FOR THE HALF MASS RADIUS')
 2011   FORMAT(1H, 9X,'ELLIP',10X,'SEMI-MAJOR AXIS(r,th,ph)',10x,
     1                'SEMI-MINOR AXIS (r,th,phi)')
 2012   FORMAT(1H,4X,F10.4,5X,3F10.4,5X,3F10.4,3X,F10.4)

        RETURN
        END
