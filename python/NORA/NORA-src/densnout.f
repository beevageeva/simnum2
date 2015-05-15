 
 
 
C*****************************************************************************
 
 
        SUBROUTINE densNout (gdev, nbin, dist, dav, mb, dens, 
     &      meanvR, dispR,meanvT, dispT, infostr, coordmax, dmin,
     &      dmax, npbin)
 
 
C*****************************************************************************
 
 
        IMPLICIT NONE
	REAL logdist, logdens
	REAL errtop,errbot,meanvr,dmin(2),dmax(2),edge
        REAL vmin,vmax, coordmax, dold,rmin,rmax
        REAL mb(nbin), dist(nbin), dens(nbin), meanvR(nbin)
        REAL dispR(nbin), meanvT(nbin), dispT(nbin),dav(nbin)
        REAL xmin,xmax,xedge, logdav
        INTEGER inbin, nbin, i, k, n, npbin
        INTEGER uout
	INTEGER maxbin
	PARAMETER(maxbin=1000)
	CHARACTER*(*) gdev
	CHARACTER*(*) infostr(*)

        DIMENSION logdist(maxbin), logdens(maxbin),
     &          errtop(maxbin),errbot(maxbin), logdav(maxbin)
 

	INTEGER cross, square, triangle
	DATA cross, square, triangle /5, 6, 7/

	uout = 18
 
C----------
C  Logarithms
C----------
ccc        dold=0.
	DO 5 i = 1, nbin
                if(coordmax.gt.0.and.dist(i).gt.coordmax)then
                  nbin=i-1
                  goto 6
                endif
		logdist(i) = LOG10(dist(i))
                logdav(i)  = LOG10(dav(i))
ccc                dold=dist(i)
		logdens(i) = LOG10(dens(i))
5	CONTINUE
  6     continue


C---------
C  Open output device
C---------
        IF (gdev(1:4) .EQ. 'file') THEN
	    OPEN(uout,FILE='dens.out',STATUS='UNKNOWN')

	    WRITE(uout, '(A)') '#'
	    WRITE(uout, '(A)') '#'//infostr(1)(1:79)
    	    WRITE(uout, '(A)') '#'//infostr(2)(1:79)
            WRITE(uout,1005)
 1005       FORMAT('#',1x, 'OuterRadius', 5x,' < R >',5x, 'mass',
     1         5x, 'Dens', 5x, 'meanvR', 5x, 'DispR')
 
            DO 10 i = 1, nbin
                WRITE(uout, '(2(1pe12.4), i8, 4(1pe12.4))')
     1          dist(i),dav(i), npbin, dens(i), meanvR(i), dispR(i)
 10         CONTINUE
	    CLOSE(uout)
            RETURN

        ELSE 
	    CALL PGBEGIN (0, gdev, 1, 1)
        END IF

CCC 1010       FORMAT(1pe12.4, I8, 3(1pe12.4))
 1010       FORMAT(e12.4, I8, 3(e12.4))
C------
C  Plot
C-------
	CALL PGSCF(2)
	CALL PGASK (.FALSE.)
	CALL PGADVANCE
C---Top: Mass density profile
	CALL PGVPORT (0.2, 0.8, 0.65, 0.9)
        if(dmin(2).lt..5)goto 51
        dmin(1)=10000.
        do n=1,nbin
          dmin(1)=min(dmin(1),logdens(n))
        enddo
  51    if(dmax(2).lt..5)goto 52
        dmax(1)=-10000.
        do n=1,nbin
          dmax(1)=max(dmax(1),logdens(n))
        enddo
  52    edge=.1*(dmax(1)-dmin(1))
        dmax(1)=dmax(1)+edge
        dmin(1)=dmin(1)-edge
        vmin=1.e10
        vmax=-1.e10
        DO 40 i = 1, nbin
            errtop(i) = meanvr(i)+dispr(i)
            errbot(i) = meanvr(i)-dispr(i)
            if(errbot(i).gt.-100.)then
              vmin=min(vmin,errbot(i))
              vmax=max(vmax,errtop(i))
            endif
40      CONTINUE
        edge=.1*(vmax-vmin)
        vmin=vmin-edge
        vmax=vmax+edge


        xmin=min(logdav(1),logdist(1))
        xmax=max(logdav(nbin),logdist(nbin))
        xedge=.1*(xmax-xmin)
        xmin=xmin-xedge
        xmax=xmax+xedge

	CALL PGWINDOW (xmin,xmax, dmin(1), dmax(1))	
	CALL PGBOX ('BCLST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL (' ', 'log(dens)', ' ')
	CALL PGPOINT (nbin, logdist, logdens, cross)
        CALL PGPOINT (nbin, logdav, logdens, square)
C---Bot: Radial and Tangential Dispersions
	CALL PGVPORT (0.2, 0.8, 0.4, 0.65)
	CALL PGWINDOW (logdist(1), logdist(nbin), vmin,vmax)
	CALL PGBOX ('BCNLST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL ('Radius', 'vR', ' ')
	CALL PGPOINT (nbin, logdist, meanvR, triangle)
        CALL PGERRY (nbin, logdist, errtop, errbot, 1.)
ccc	CALL PGPOINT (nbin, logdist, dispR, cross)
        CALL PGMOVE(LOGDIST(1),0.)
        CALL PGDRAW(LOGDIST(NBIN),0.)

C---Label 
	   CALL PGSCH (0.8)
	   CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
	   CALL PGWINDOW (0., 1., 0., 1.)
	   CALL PGTEXT (0., 0.7, 'Vol Density and Radial Velocity profiles')
	   CALL PGTEXT (0., 0.6, infostr(1))
	   CALL PGTEXT (0., 0.5, infostr(2))
	   CALL PGSCH (1.0)

	CALL PGEND

C---Flush to printer
	CALL psflush(gdev)
 
	RETURN 
        END
