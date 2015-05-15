


C************************************************************************
 
                SUBROUTINE densn (gdev, npbin, coordmax, istart, iend,
     &				infostr, dmin, dmax)

c density profile using bins containing fixed number of particles 
c per bin, radius of bin at average of all radii of points in bin

 
	IMPLICIT NONE

	INCLUDE 'nora.def'

	INTEGER nbin, maxbin, npbin
	PARAMETER (maxbin=1000)
	REAL rb(maxbin), dmin(2), dmax(2)

        INTEGER n, i, k, istart, iend, nbods, indx(nmax), m, l, m0
        REAL rho(maxbin), mb(maxbin), rold,
     1          dispR(maxbin), dispT(maxbin), totm,
     1          coordmax, eps, PI, pi43,rav(maxbin),
     1		meanvR(maxbin), meanvT(maxbin)
	CHARACTER*(*) gdev, infostr(*)
 
	REAL r, th, ph, vr, vt, phiv, rn
	DIMENSION r(nmax), th(nmax), ph(nmax)
	DIMENSION vr(nmax), vt(nmax), phiv(nmax)

	PI = ACOS(-1.)
        pi43=pi*4./3.

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
CCC	PRINT '(3E12.4)', (r(k), th(k), ph(k), k=100,120)
	CALL VelToCyl (istart, iend, r, th, ph, vx, vy, vz, vr, vt, phiv)
CCC	PRINT '(3E12.4)', (vr(k), vt(k), phiv(k), k=100,120)

C-------------
C  Order radii and determine # points per bin 
C-------------
        call indexx(nbods,r,indx)
        nbin=nbods/npbin
        if(nbin.gt.maxbin)then
          nbin=maxbin
          npbin=nbods/nbin
          print *,' npbin = ',npbin
        endif

C-------------
C  Bin the generated radial distribution
C  Bin the distribution of radial and tangential velocity dispersions
C----------------

        rold=0.
        do i=1,nbin
          m=i*npbin
          rb(i)=.5*(r(indx(m))+r(indx(m+1)))
          if(m.ge.nbods)then
            m=nbods
            rb(i)=r(indx(m))
          endif
          m0=m-npbin
          mb(i)=0.
          meanvR(i)=0.
          meanvT(i)=0.
          dispR(i)=0.
          dispT(i)=0.
          rav(i)=0.
          do l=m0+1,m
            rav(i)=rav(i)+r(indx(l))
            mb(i)=mb(i)+pmass(indx(l))
            meanvR(i) = meanvR(i) + pmass(indx(l)) * vr(indx(l))
            meanvT(i) = meanvT(i) + pmass(indx(l)) * vt(indx(l))
          enddo
          rav(i)=rav(i)/float(m-m0)
          rho(i)=mb(i)/(pi43*(rb(i)**3-rold**3))
          rold=rb(i)
        enddo


        DO 21 i = 1, nbin
		meanvR(i) = meanvR(i) / mb(i)
		meanvT(i) = meanvT(i) / mb(i)
 21     CONTINUE

        DO 22 i = 1, nbin
          m=i*npbin
          if(m.gt.nbods)m=nbods
          m0=m-npbin
          dispR(i)=0.
          dispT(i)=0.
          do l=m0+1,m
                dispR(i) = dispR(i) + 
     1	      pmass(indx(l)) * (vr(indx(l))-meanvR(i))**2
                dispT(i) = dispT(i) + 
     1	      pmass(indx(l)) * (vt(indx(l))-meanvT(i))**2
          enddo
 22     CONTINUE

C------------
C  Velocity dispersions
C---------------
        DO 25 i = 1, nbin


                        dispR(i) = SQRT (dispR(i) / mb(i))
                        dispT(i) = SQRT (dispT(i) / mb(i))
 25     CONTINUE
 
C--------
C  Output 
C--------
C        call densNout (gdev, nbin, rb, rav, mb, rho, meanvR, dispR,
C     &   meanvT, dispT, infostr, coordmax, dmin, dmax, npbin)
 
        RETURN
        END
