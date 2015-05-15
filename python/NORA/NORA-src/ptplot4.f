

C**************************************************************************


	      SUBROUTINE ptplot4(gdev, istart, iuno, imid, iend,  
     &				theta, phi, coordmax,infostr) 


C**************************************************************************

	IMPLICIT NONE

	INCLUDE 'nora.def'

	INTEGER istart, iend, i,iuno,imid,j
	REAL theta, phi, coordmax, scalel,xbuff, ybuff, vbuff
	REAL xbuff1, ybuff1, vbuff1,xbuff2, ybuff2, vbuff2
	REAL xbuff3, ybuff3, vbuff3
	REAL xbuff11, ybuff11, vbuff11,xbuff22, ybuff22, vbuff22
	REAL xbuff33, ybuff33, vbuff33
c	DIMENSION xbuff(nmax), ybuff(nmax), vbuff(nmax)
	DIMENSION xbuff1(nmax), ybuff1(nmax), vbuff1(nmax)
	DIMENSION xbuff2(nmax), ybuff2(nmax), vbuff2(nmax)
	DIMENSION xbuff3(nmax), ybuff3(nmax), vbuff3(nmax)
	DIMENSION xbuff11(nmax), ybuff11(nmax), vbuff11(nmax)
	DIMENSION xbuff22(nmax), ybuff22(nmax), vbuff22(nmax)
	DIMENSION xbuff33(nmax), ybuff33(nmax), vbuff33(nmax)

	CHARACTER*(*) gdev, infostr(*)
	scalel=1.
	print*,istart,iuno,imid,iend
C-------
C  Zero arrays
C-------
	DO 10 i = 1, nmax
	    xbuff1(i) = 0.0
	    ybuff1(i) = 0.0
	    vbuff1(i) = 0.0
	    xbuff11(i) = 0.0
	    ybuff11(i) = 0.0
	    vbuff11(i) = 0.0
10	CONTINUE

C-------
C  Project onto observing plane
C-------
c	CALL prject(istart, iend, theta, phi, xbuff, ybuff, vbuff)
	CALL prject(istart, iuno, theta, phi, xbuff1, ybuff1, vbuff1)

	
C-------
C  Prepare arrays for plotting
C-------
C	DO 20 i = 1, iend-istart+1
C	    xbuff(i) = xbuff(istart+i-1)/scalel
C	    ybuff(i) = ybuff(istart+i-1)/scalel
C	    vbuff(i) = vbuff(istart+i-1)
C20	CONTINUE
	j=0
	DO 20 i = 1, iuno-istart+1,10
	   j=j+1
	    xbuff11(j) = xbuff1(istart+i-1)/scalel
	    ybuff11(j) = ybuff1(istart+i-1)/scalel
	    vbuff11(j) = vbuff1(istart+i-1)
 20	CONTINUE


C--------
C  PGPLOT points plot
C--------
        CALL PGBEGIN(0, gdev, 1, 1)        
	CALL PGSCF(2)
	CALL PGSLW(3)
C        CALL PGVPORT (0.2, 0.8, 0.35, 0.95)
       CALL PGVPORT (0.2, 0.8, 0.35, 0.75)

        CALL PGWNAD (-coordmax/scalel, coordmax/scalel, 
     &               -coordmax/scalel, coordmax/scalel)
        CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
        CALL PGLABEL ('R', 'R', ' ')
	CALL PGSCI(1)
        CALL PGPOINT((iuno-istart+1)/10, xbuff11, ybuff11, -1)
CCC        CALL PGPOINT(nmax, xbuff, ybuff, -1)

C-----
C-------
C  Zero arrays
C-------
	DO 11 i = 1, nmax
	    xbuff2(i) = 0.0
	    ybuff2(i) = 0.0
	    vbuff2(i) = 0.0
	    xbuff22(i) = 0.0
	    ybuff22(i) = 0.0
	    vbuff22(i) = 0.0
 11	 CONTINUE

C-------
C  Project onto observing plane
C-------
c	CALL prject(istart, iend, theta, phi, xbuff, ybuff, vbuff)
	CALL prject(iuno, imid, theta, phi, xbuff2, ybuff2, vbuff2)


C-------
C  Prepare arrays for plotting
C-------
C	DO 20 i = 1, iend-istart+1
C	    xbuff(i) = xbuff(istart+i-1)/scalel
C	    ybuff(i) = ybuff(istart+i-1)/scalel
C	    vbuff(i) = vbuff(istart+i-1)
C20	CONTINUE
	j=0
	DO 21 i = 1, imid-iuno+1,10
	   j=j+1
	    xbuff22(j) = xbuff2(iuno+i-1)/scalel
	    ybuff22(j) = ybuff2(iuno+i-1)/scalel
	    vbuff22(j) = vbuff2(iuno+i-1)
 21	 CONTINUE


C--------
	CALL PGSCI(3)
        CALL PGPOINT((imid-iuno+1)/10, xbuff22, ybuff22, -1)
C--------
C-----
C-------
C  Zero arrays
C-------
	DO 12 i = 1, nmax
	    xbuff3(i) = 0.0
	    ybuff3(i) = 0.0
	    vbuff3(i) = 0.0
	    xbuff33(i) = 0.0
	    ybuff33(i) = 0.0
	    vbuff33(i) = 0.0
 12	 CONTINUE

C-------
C  Project onto observing plane
C-------
c	CALL prject(istart, iend, theta, phi, xbuff, ybuff, vbuff)
	CALL prject(imid, iend, theta, phi, xbuff3, ybuff3, vbuff3)


C-------
C  Prepare arrays for plotting
C-------
C	DO 20 i = 1, iend-istart+1
C	    xbuff(i) = xbuff(istart+i-1)/scalel
C	    ybuff(i) = ybuff(istart+i-1)/scalel
C	    vbuff(i) = vbuff(istart+i-1)
C20	CONTINUE
	j=0
	DO 22 i = 1, iend-imid+1,10
	   j=j+1
	    xbuff33(j) = xbuff3(imid+i-1)/scalel
	    ybuff33(j) = ybuff3(imid+i-1)/scalel
	    vbuff33(j) = vbuff3(imid+i-1)
 22	 CONTINUE


C--------
	CALL PGSCI(2)
        CALL PGPOINT((iend-imid+1)/10, xbuff33, ybuff33, -1)
C--------
C--------
C  Label
C--------
c       comento esto temporalmente!!!
C        CALL PGSCH (0.8)
C        CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
C        CALL PGWINDOW (0., 1., 0., 1.)
C        CALL PGTEXT (0., 0.7, 'particle map')
C        CALL PGTEXT (0., 0.6, infostr(1))
C        CALL PGTEXT (0., 0.5, infostr(2))
C        CALL PGSCH (1.0)

	CALL PGEND

C---Flush to printer
	CALL psflush (gdev)

	RETURN
	END
