 
 
C*****************************************************************************
 
 
        SUBROUTINE velRout 
     &		(gdev, dist, inbin, meanvR, meanvT, aniso, Rm,
     &              FRM, totm, istart,iend,coordmax,r,vr,vt,eb,infostr)
 
 
C*****************************************************************************
 
 
        IMPLICIT NONE
	REAL dist, meanvR, Rm, FRM, logdist, logdens,emin,emax
	REAL totm, meanvT,vrvtot,vtvtot,vtot,aniso,v,ant,eb
        REAL r,vr,vt,vmin,vmax,edge,coordmax,avr(100000)
        INTEGER inbin, nbin, i, k,nbods,istart,iend
        INTEGER uout
	PARAMETER(nbin=40)
	CHARACTER*(*) gdev
	CHARACTER*(*) infostr(*)
 
        DIMENSION dist(*), inbin(*), meanvT(*), meanvR(*), aniso(*),
     &		Rm(*), FRM(*), logdist(nbin),r(*),vr(*),vt(*),
     &          vrvtot(nbin), vtvtot(nbin), ant(nbin),eb(*)

	INTEGER dot,cross, square, triangle
	DATA dot,cross, square, triangle /-1,5, 6, 7/
c        intrinsic pgend
	uout = 18
 
C----------
C  Logarithms
C----------
	DO 5 i = 1, nbin
		logdist(i) = LOG10(dist(i))
                vtot=sqrt(meanvr(i)**2+meanvt(i)**2)
                if(inbin(i).ge.1)then
                  vrvtot(i)=abs(meanvR(i))/vtot
                  vtvtot(i)=abs(meanvT(i))/vtot
                else
                  vrvtot(i)=-10.
                  vtvtot(i)=-10.
                endif
                ant(i)=1.-aniso(i)
5	CONTINUE

C---------
C  Open output device
C---------
        IF (gdev(1:4) .EQ. 'file') THEN
	    OPEN(uout,FILE='velR.out',STATUS='UNKNOWN')

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
     1         5x, 'meanvR', 5x, 'vr/v', 5x, ' meanvT',5x,' <vr/v>')
 
            DO 10 i = 1, nbin
                WRITE(uout, '(1pe12.4, I8, 4(1pe12.4))')
     1          dist(i), inbin(i), meanvR(i), vrvtot(i), meanvt(i),
     1          aniso(i)
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
	        WRITE(6,'(1pe12.4, I8, 4(1pe12.4))') 
     1        dist(i), inbin(i), meanvR(i), vrvtot(i), meanvt(i),
     1        aniso(i)
 25	    CONTINUE
	    RETURN

        ELSE IF (gdev(1:2) .EQ. 'Rm') THEN
	    IF (gdev(3:6) .EQ. 'file') THEN
		OPEN(uout, FILE='Rm.out',STATUS='UNKNOWN')
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
	    IF (gdev(1:4) .EQ. 'file') CLOSE (uout)
	    RETURN

        ELSE 
	    CALL PGBEGIN (0, gdev, 1, 1)
        END IF

CCC 1010       FORMAT(1pe12.4, I8, 3(1pe12.4))
 1010       FORMAT(e12.4, I8, 3(e12.4))


        vmin=min(vr(1),vt(1))
        vmax=max(vr(1),vt(1))
        emin=eb(1)
        emax=emin
        do 137 i=istart+1,iend
          if(r(i).gt.coordmax)goto 137
          vmin=min(vmin,vr(i))
          vmin=min(vmin,vt(i))
          vmax=max(vmax,vr(i))
          vmax=max(vmax,vt(i))
          emin=min(emin,eb(i))
          emax=max(emax,eb(i))
 137    continue
        edge=.1*(emax-emin)
        emin=emin-edge
        emax=emax+edge
        edge=.1*(vmax-vmin)
        vmin=vmin-edge
        vmax=vmax+edge

C------
C  Plot
C-------
	CALL PGSCF(2)
	CALL PGASK (.FALSE.)
	CALL PGADVANCE
C---Top: Radial and Tangential velocities
	CALL PGVPORT (0.2, 0.8, 0.65, 0.9)
	CALL PGWINDOW (logdist(1), logdist(nbin), vmin,vmax)	
	CALL PGBOX ('BCLST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL (' ', 'vR  vT', 
     +'radial (triangle) tangential (cross) velocities')
	CALL PGPOINT (nbin, logdist, meanvT, cross)
        CALL PGPOINT (nbin, logdist, meanvR, triangle)
C---Bot: ratios to total velocity
	CALL PGVPORT (0.2, 0.8, 0.4, 0.65)
	CALL PGWINDOW (logdist(1), logdist(nbin), 0., 1.)
	CALL PGBOX ('BCNLST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL ('Radius', 'ratios', ' ')
	CALL PGPOINT (nbin, logdist, ant, cross)
	CALL PGPOINT (nbin, logdist, aniso, triangle)
C---Label 
	   CALL PGSCH (0.8)
	   CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
	   CALL PGWINDOW (0., 1., 0., 1.)
	   CALL PGTEXT (0., 0.7, ' velocities and anisotropy measures')
	   CALL PGTEXT (0., 0.6, infostr(1))
	   CALL PGTEXT (0., 0.5, infostr(2))
	   CALL PGSCH (1.0)

CCCCCCCCCCCCCCCCCCCCCCC
C  Second plot : scatter Vr vs R
CCCCCCCCCCCCCCCCCCCCCCC


        nbods=iend+1-istart
	CALL PGASK(.TRUE.)
	CALL PGADVANCE
	CALL PGVPORT (0.2, 0.8, 0.35, 0.95)
	CALL PGWINDOW (0. , coordmax, vmin, vmax )	
	CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL ('R', 'Vr', ' ')
	CALL PGPOINT (nbods, r, vr, dot)

CCCCCCCCCCCCCCCCCCCCCCC
C  Third plot : scatter Vt vs R & |Vr| vs R
CCCCCCCCCCCCCCCCCCCCCCC


        do i=istart,iend
          avr(i)=abs(vr(i))
        enddo

        nbods=iend+1-istart
	CALL PGASK(.TRUE.)
	CALL PGADVANCE
	CALL PGVPORT (0.2, 0.8, 0.70, 0.95)
	CALL PGWINDOW (0. , coordmax, -edge, vmax )	
	CALL PGBOX ('BCST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL (' ', 'Vt', ' ')
	CALL PGPOINT (nbods, r, vt, dot)
	CALL PGVPORT (0.2, 0.8, 0.45, 0.70)
	CALL PGWINDOW (0. , coordmax, -edge, vmax )	
	CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL ('R', '|Vr|', ' ')
	CALL PGPOINT (nbods, r, avr, dot)
	CALL PGVPORT (0.2, 0.8, 0.10, 0.35)
	CALL PGWINDOW (vmin,vmax, -edge, vmax )	
	CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL ('Vr', 'Vt', ' ')
	CALL PGPOINT (nbods, vr, vt, dot)

CCCCCCCCCCCCCCCCCCCCCCC
C  Fourth plot : scatter Vr vs Eb & Vt vs Eb
CCCCCCCCCCCCCCCCCCCCCCC


        nbods=iend+1-istart
	CALL PGASK(.TRUE.)
	CALL PGADVANCE
	CALL PGVPORT (0.2, 0.8, 0.50, 0.90)
	CALL PGWINDOW (emin, emax, vmin, vmax )	
	CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL (' ', 'Vr', ' ')
	CALL PGPOINT (nbods, eb, vr, dot)
	CALL PGVPORT (0.2, 0.8, 0.10, 0.50)
	CALL PGWINDOW (emin, emax, -edge, vmax )	
	CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL ('E ', 'Vt', ' ')
	CALL PGPOINT (nbods, eb, vt, dot)

	CALL PGEND

C---Flush to printer
	CALL psflush(gdev)
 
	RETURN 
        END
