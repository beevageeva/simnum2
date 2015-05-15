 
 
 
C*****************************************************************************
 
 
      SUBROUTINE ebinoutc(gdev,dist,ebin,dist2,t2wbin,coordmin,coordmax,
     &  	istart,iuno,imid,iend,r, eb,t2w,mass,r0,infostr)
 
 
C*****************************************************************************
 
 
        IMPLICIT NONE
	INCLUDE 'nora.def'
	REAL ebin,dist,fmax,r,eb,coordmin,coordmax,r0,ec,rr,ebb,
     +   	edge,dlow,dupp,dmin,dmax,dmin2,dmax2,fmax2,dlow2,dupp2,
     +          rgr(56), dist2,t2wbin,t2w,mass,rmod(21),emod(21)
        INTEGER nbin, i, k,nbods,iend,iuno,imid,istart
        INTEGER uout, il, il1, il2
	PARAMETER(nbin=40)
	CHARACTER*(*) gdev
	CHARACTER*(*) infostr(*)
 
        DIMENSION ebin(*),dist(*),r(*),eb(*),t2w(*),dist2(*),t2wbin(*)
        DIMENSION rr(nmax),ebb(nmax)
	INTEGER cross, square, triangle, dot
	DATA dot, cross, square, triangle /-1, 5, 6, 7/

 


      IL=56
      IL1=IL-1
      IL2=IL1-1
      RgR(1)=0.0
      RgR(2)=0.0020
      DO I=3,IL
        RgR(I)=RgR(I-1)*10**0.1
      enddo

      ec=1.5*mass/r0

	uout = 18

        dmin=1000.
        dmax=-100000.
        dmin2=1000.
        dmax2=-100000.
        fmax=0.
        fmax2=0.
        do i=1,nbin
          fmax=max(fmax,ebin(i))
          if(ebin(i).ge.0.)then
            dmin=min(dmin,dist(i))
            dmax=max(dmax,dist(i))
          endif
          fmax2=max(fmax2,t2wbin(i))
          if(t2wbin(i).ge.0)then
            dmin2=min(dmin2,dist2(i))
            dmax2=max(dmax2,dist2(i))
          endif
        enddo
        fmax=fmax*1.1
        fmax2=fmax2*1.1
        nbods=iend+1-istart
C---------
C  Open output device
C---------
        IF (gdev(1:4) .EQ. 'file') THEN
	    OPEN(uout,FILE='ebind.out',STATUS='UNKNOWN')

            WRITE(uout,1005)
 1005       FORMAT('#',1x, 'Radius',
     1         5x, 'Number')
 
            DO 10 i = 1, nbin
                WRITE(uout, '(2e12.4)')
     1          dist(i), ebin(i)
 10         CONTINUE
	    CLOSE(uout)
            RETURN

        ELSE 
	    CALL PGBEGIN (0, gdev, 1, 1)
        END IF

C------
C First Plot : Binding Energy spectrum
C-------
	CALL PGSCF(2)
	CALL PGASK (.FALSE.)
	CALL PGADVANCE
        edge=.1*(dmax-dmin)
        dlow=dmin-edge
        dupp=dmax+edge
        edge=.1*(dmax2-dmin2)
        dlow2=dmin2-edge
        dupp2=dmax2+edge
	CALL PGVPORT (0.2, 0.8, 0.65, .95)
	CALL PGWINDOW (dlow, dupp, 0., fmax)	
	CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL ('Ebind ', 'fraction', ' ')
ccc        CALL PGHIST (NBIN, EB,EMIN,EMAX,NBIN,0)
	CALL PGBIN (nbin, dist, ebin, .TRUE.)
        CALL PGVPORT(0.2, 0.8, 0.25 ,0.55)
	CALL PGWINDOW (dlow2, dupp2, 0., fmax2)	
	CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
        CALL PGLABEL('2T/W','FRACTION',' ')
	CALL PGBIN (nbin, dist2, t2wbin, .TRUE.)

C--------------------------------------------------------------------------
C Second Plot : scatter plot, Ebind vs. R
C--------------------------------------------------------------------------

        do i=1,21
          rmod(i)=.05*float(i-1)*r0
          emod(i)=ec*((rmod(i)/r0)**2-1.)
        enddo        
        dupp=1.
c        dlow=-3.
        dlow=-1.5
c        dlow=-2.
	CALL PGASK(.TRUE.)
	CALL PGADVANCE
	CALL PGVPORT (0.2, 0.8, 0.35, 0.95)
	CALL PGWINDOW (coordmin , coordmax, dlow,dupp )	
        CALL PGSCF(2)
        CALL PGSCH(2.5)
        CALL PGSLW (3)
	CALL PGBOX ('BCNST', coordmax/2.0,INT(coordmax/200.), 
     &       'BCNST', 0.0, 0)

	CALL PGLABEL ('R', 'Ebind', ' ')
        	   CALL PGSCH (2.0)
                   CALL PGSCI(1)
c	CALL PGPOINT (nbods, r, eb, dot)
        do 1 i=1,nmax
           rr(i)=0.
           ebb(i)=0.
 1      continue  
                   DO 6 i=1,(iuno-istart+1)
                      rr(i)=r(istart+i)
                      ebb(i)=eb(istart+i)
 6                 continue  
        CALL PGPOINT ((iuno-istart+1),rr,ebb,dot)
                   CALL PGSCI(3)
        do 2 i=1,nmax
           rr(i)=0.
           ebb(i)=0.
 2         continue  
                   DO 7 i=1,(imid-iuno+1)
                      rr(i)=r(iuno+i)
                      ebb(i)=eb(iuno+i)
 7                    continue  
        CALL PGPOINT ((imid-iuno+1),rr,ebb,dot)
                   CALL PGSCI(2)
        do 3 i=1,nmax
           rr(i)=0.
           ebb(i)=0.
 3         continue  
                   DO 8 i=1,(iend-imid+1)
                      rr(i)=r(imid+i)
                      ebb(i)=eb(imid+i)
 8                    continue  
        CALL PGPOINT ((iend-imid+1),rr,ebb,dot)
        print*,'HOLA',nbods,istart,iend,r(istart),eb(istart)
ccc        CALL PGLINE(21,rmod,emod)
                   CALL PGSCI(1)
        CALL PGMOVE(COORDMIN,0.)
        CALL PGDRAW(COORDMAX,0.)
        CALL PGSLW (1)
cccccc  draw radial grid boundaries ccccccc
cc        do i=1,il
cc          call pgmove(rgr(i),dlow)
cc          call pgdraw(rgr(i),dupp)
cc        enddo
ccccccccccccccccccccccccccccccccccccccccccc
C---Label 
c	   CALL PGSCH (0.8)
c	   CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
c	   CALL PGWINDOW (0., 1., 0., 1.)
c	   CALL PGTEXT (0., 0.7, 'Binding Energy Spectrum')
c	   CALL PGTEXT (0., 0.6, infostr(1))
c	   CALL PGTEXT (0., 0.5, infostr(2))
c	   CALL PGSCH (1.0)


        return

C--------------------------------------------------------------------------
C Third Plot : scatter plot, T  W
C--------------------------------------------------------------------------

       

	CALL PGASK(.TRUE.)
	CALL PGADVANCE
	CALL PGVPORT (0.2, 0.8, 0.35, 0.95)
	CALL PGWINDOW (coordmin , coordmax, dmin2, dmax2 )	
	CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL ('R', '2T/W', ' ')
	CALL PGPOINT (nbods, r, t2w, dot)
ccc        CALL PGMOVE(COORDMIN,0.)
ccc        CALL PGDRAW(COORDMAX,0.)
cccccc  draw radial grid boundaries ccccccc
        do i=1,56
          call pgmove(rgr(i),dlow)
          call pgdraw(rgr(i),dupp)
        enddo
ccccccccccccccccccccccccccccccccccccccccccc
C---Label 
	   CALL PGSCH (0.8)
	   CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
	   CALL PGWINDOW (0., 1., 0., 1.)
	   CALL PGTEXT (0., 0.7, 'Virial Ratio Spectrum')
	   CALL PGTEXT (0., 0.6, infostr(1))
	   CALL PGTEXT (0., 0.5, infostr(2))
	   CALL PGSCH (1.0)
C--------------------------------------------------------------------------
C Fourth Plot : scatter plot, 2T/W vs. Ebind
C--------------------------------------------------------------------------
	CALL PGASK(.TRUE.)
	CALL PGADVANCE
	CALL PGVPORT (0.2, 0.8, 0.35, 0.95)
	CALL PGWINDOW (dlow , dupp, dlow2,dupp2 )	
	CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL ('E', '2T/W', ' ')
	CALL PGPOINT (nbods, eb, t2w, dot)
ccc        CALL PGMOVE(COORDMIN,0.)
ccc        CALL PGDRAW(COORDMAX,0.)
cccccc  draw radial grid boundaries ccccccc
ccc        do i=1,56
ccc          call pgmove(rgr(i),dlow)
ccc          call pgdraw(rgr(i),dupp)
ccc        enddo
ccccccccccccccccccccccccccccccccccccccccccc
C---Label 
	   CALL PGSCH (0.8)
	   CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
	   CALL PGWINDOW (0., 1., 0., 1.)
	   CALL PGTEXT (0., 0.7, 'Virial Ratio vs Ebind')
	   CALL PGTEXT (0., 0.6, infostr(1))
	   CALL PGTEXT (0., 0.5, infostr(2))
	   CALL PGSCH (1.0)

	CALL PGEND

C---Flush to printer
	CALL psflush(gdev)
 
	RETURN 
        END
