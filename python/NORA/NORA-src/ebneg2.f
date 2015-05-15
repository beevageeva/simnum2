C***********************************************************************
          SUBROUTINE ebneg2 (gdev, coordmin, coordmax, elow, eupp,
     &istart, iend, infostr)
 
C***********************************************************************
C  Takes into account only particles with negative binding energy
C                             A.C.G.G. 23/2/00
C-----------------------------------------------------------------------
 
	IMPLICIT NONE

	INCLUDE 'nora.def'

	INTEGER nbin
	PARAMETER (nbin=40)
	REAL dist(nbin), dist2(nbin),coordmin, eupp, elow

        INTEGER n, i, k, istart, iend, nbods,m
        REAL  eb(nmax),pi,dbin,etot,ebav,t2w(nmax),t2wbin(nbin),
     1        emin,emax,coordmax,ebin(nbin),ek,t2wmin,t2wmax,dbin2,
     2        ektot,mass,ec,r0,ekin,poten,po
        REAL tmass,mratio
	CHARACTER*(*) gdev, infostr(*)
        INTEGER parti,pfirst,psecond
	REAL r, th, ph, vr, vt, phiv, rn
	DIMENSION r(nmax), th(nmax), ph(nmax)
	DIMENSION vr(nmax), vt(nmax), phiv(nmax)
        INTEGER uout

        uout=18

	PI = ACOS(-1.)
C--------------
C  Create linearly-spaced grid of Ebind values between emin and emax
C--------------

        emax=-1000.
        emin=1000.
        t2wmax=-1000.
        t2wmin=1000.
        etot=0.
        ektot=0.
        ebav=0.
        tmass=0.
        mass=0.
        parti=0
        pfirst=0
        psecond=0
        ekin=0.
        poten=0.
        CALL XYZtoRTP(istart, iend, x, y, z, r, th, ph)
        r0=-1000.
        ec=0.
	DO 3 n = istart, iend            
           r0=max(r0,r(n))
            if(r(n).le.coordmax.and.r(n).ge.coordmin)then 
              ek=.5*(vx(n)*vx(n)+vy(n)*vy(n)+vz(n)*vz(n))
              eb(n)=ppot(n)+ek
              po=po+ppot(n)
              tmass=tmass+pmass(n)
                if(eb(n).gt.0)then
                   pmass(n)=0
C                   goto 5
                else 
                   if(eb(n).le.0)then
             parti=parti+1  
             mass=mass+pmass(n)
             ekin=ekin+ek*pmass(n)
             poten=poten+0.5*ppot(n)*pmass(n)
                   endif
                endif
c 4              parti=parti+1  
c 5           mass=mass+pmass(n)
c             ekin=ekin+ek*pmass(n)
c             poten=poten+ppot(n)*pmass(n)
             if(pmass(n).ne.pmass(1))then
                psecond=psecond+1
             else
                pfirst=pfirst+1
             endif   
          endif
 3      CONTINUE
C        poten=0.5*poten
        po=po/tmass
        mratio=(mass/tmass)*100
C        print *,'% of mass kept=',npart,mratio, mass, tmass
C---------
C  Open output device
C---------
C        IF (gdev(1:1) .EQ. 't') THEN
C            WRITE(6,*) (' Npart   mass rat.   Npart1    Npart2 ')
C            WRITE(6,*),parti,mratio,pfirst,psecond,tmass,mass
C            RETURN
C        IF (gdev(1:4) .EQ. 'file') THEN
C            OPEN(uout,FILE='ebneg.out',STATUS='UNKNOWN')
C            WRITE(uout,1000) 
C            WRITE(uout,1010),parti,mratio,pfirst,psecond,tmass,mass
C 1000     FORMAT(7X,'Npart',7X,'mass rat.',7X,'Npart1'7X,'Npart2')
C 1010     FORMAT(5X,F10.4,5X,F10.4,5X,F10.4,5X,F10.4)
C            CLOSE(uout)
C            RETURN
C        ELSE IF (gdev(1:1) .EQ. 't') THEN
            PRINT *, ' Npart   mass rat. bind_mass  E_k   E_p'
            PRINT *,parti,mratio,mass,ekin,poten,po
C 1000     FORMAT(7X,'Npart',7X,'mass rat.',7X,'Npart1'7X,'Npart2')
C 1010     FORMAT(5X,F10.4,5X,F10.4,5X,F10.4,5X,F10.4)
C        END IF
        RETURN

        END
