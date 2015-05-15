


C************************************************************************
 
                SUBROUTINE ring (gdev, coordmax, istart, iend,
     &				GRAVC, infostr)
 
C************************************************************************
c     A.C.G.G. 27/05/2004
C-----------------------------------------------------------------------------
 
	IMPLICIT NONE

	INCLUDE 'nora.def'

	INTEGER nbin,binth, uout
	PARAMETER (nbin=40)
c        PARAMETER (binth=16)
        PARAMETER (binth=32)
	REAL dist(nbin), logdist(nbin), dlogr, thbin(binth+1)
        INTEGER inbin(nbin,binth)
        REAL zinbin(nbin,binth),meanzbin(nbin,binth)
        REAL medzbin(nbin,binth),vzinbin(nbin,binth)
        REAL meanvzbin(nbin,binth)

        INTEGER n, i, k, h, j, istart, iend, nbods
        REAL  coordmax, eps, PI
	CHARACTER*(*) gdev, infostr(*)
 
	REAL rc, thc, zc,rn, GRAVC, mdian2
	DIMENSION rc(nmax), thc(nmax), zc(nmax)

        uout = 18
	PI = ACOS(-1.)

C--------------
C  Create linearly-spaced grid of log-r values between eps and rmax
C--------------
	eps = 0.1
        CALL loggrid(coordmax, eps, nbin, logdist, dlogr)

        DO 50 i = 1, nbin
                dist(i) = 10 ** logdist(i)
 50     CONTINUE

C-----------
c     Create the bin in theta, 16 bins
c-----------
       thbin(1)=-1*PI
        do 55 h=2,binth
           thbin(h)=(360./binth)*(h-1) - 180
           thbin(h)= thbin(h) *PI/180.
 55     continue   

C---------
C  Total mass
C--------
	nbods = header(1)
c	totm = 0.0
C----------
C  Fill cilindrical coordinate arrays: R, theta, z 
C---------
	CALL XYZtoRTZ (istart, iend, x, y, z, rc, thc, zc)
c	CALL VelToCyl (istart, iend, r, th, ph, vx, vy, vz, vr, vt, phiv)

C-------------
C  Bin the generated radial distribution
C  Bin the distribution of radial and tangential velocity dispersions
C----------------
        DO 10 i = 1, nbin
           do 11 j=1,binth
                inbin(i,j) = 0
                zinbin(i,j)= 0.e0
                meanzbin(i,j) = 0.e0
                vzinbin(i,j)= 0.e0
                meanvzbin(i,j) = 0.e0
                
 11     continue        
 10     CONTINUE
        DO 20 n = istart, iend
		rn = LOG10 (rc(n))
                IF (rn .LT. logdist(1)) THEN
                        i = 1
                ELSE
                        i = 2 + INT((rn - logdist(1)) / dlogr)
                END IF

                IF (i .GT. nbin) GO TO 20
                
                do 35 j=1,binth
                   thc(n)=thc(n)
                if (j.eq.binth) then
                if (thc(n).gt.thbin(j) .and. thc(n).lt.2*PI) then
                inbin(i,j) = inbin(i,j) + 1
                zinbin(i,j)=zc(n)
                vzinbin(i,j)=vz(n)
                goto 35
                endif
                endif
          if (thc(n).gt.thbin(j) .and. thc(n).lt.thbin(j+1)) then
                                
                inbin(i,j) = inbin(i,j) + 1
                zinbin(i,j)=zc(n)
                vzinbin(i,j)=vz(n)
          endif
 35     continue
CCC                meanvT(i) = meanvT(i) + pmass(n) * vt(n)
 20     CONTINUE

        DO 21 i = 1, nbin
           do 22 j = 1,binth
              meanzbin(i,j) = zinbin(i,j) / inbin(i,j)
              meanvzbin(i,j)=vzinbin(i,j) / inbin(i,j)
              medzbin(i,j) = mdian2(zinbin(i,j),inbin(i,j))
 22          CONTINUE
 21       CONTINUE
            OPEN(uout,FILE='ring.out',STATUS='UNKNOWN')
            WRITE(uout, '(A)') '#'
            DO 1000 i=1,nbin
               do 1001 j=1,binth
            WRITE(UOUT,*) dist(i),thbin(j),meanzbin(i,j),medzbin(i,j),
     &                 meanvzbin(i,j)
 1001       continue
 1000       continue
            close (uout)

        RETURN
        END
