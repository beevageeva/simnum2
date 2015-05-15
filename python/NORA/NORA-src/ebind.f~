


C************************************************************************
 
                SUBROUTINE ebind (gdev, coordmin, coordmax, elow, eupp,
     &istart, iend, infostr)
 
C************************************************************************
C  Generates binding energy spectrum within radius COORDMAX
C-----------------------------------------------------------------------------
 
	IMPLICIT NONE

	INCLUDE 'nora.def'

	INTEGER nbin
	PARAMETER (nbin=40)
	REAL dist(nbin), dist2(nbin),coordmin, eupp, elow

        INTEGER n, i, k, istart, iend, nbods,m
        REAL  eb(nmax),pi,dbin,etot,ebav,t2w(nmax),t2wbin(nbin),
     1        emin,emax,coordmax,ebin(nbin),ek,t2wmin,t2wmax,dbin2,
     2        ektot,mass,ec,r0,www
	CHARACTER*(*) gdev, infostr(*)
 
	REAL r, th, ph, vr, vt, phiv, rn, rmin
	DIMENSION r(nmax), th(nmax), ph(nmax)
	DIMENSION vr(nmax), vt(nmax), phiv(nmax)

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
        www=0.
        ebav=0.
        mass=0.
        CALL XYZtoRTP(istart, iend, x, y, z, r, th, ph)
        r0=-1000.
        ec=0.
        rmin=1000.
	DO 3 n = istart, iend
            ec=ec+pmass(n)
            r0=max(r0,r(n))
            if(r(n).le.coordmax.and.r(n).ge.coordmin)then 
              if (r(n).lt.rmin) then 
                 rmin=r(n)
              endif
              mass=mass+pmass(n)
              ek=.5*(vx(n)*vx(n)+vy(n)*vy(n)+vz(n)*vz(n))
              eb(n)=ppot(n)+ek
              emin=min(emin,eb(n))
              emax=max(emax,eb(n))
              ebav=ebav+eb(n)
              ektot=ektot+ek*pmass(n)
              www=www+ppot(n)*pmass(n)*0.5
              etot=etot+eb(n)-.5*ppot(n)
              t2w(n)=2.*ek/ppot(n)
              t2wmin=min(t2wmin,t2w(n))
              t2wmax=max(t2wmax,t2w(n))
            endif
 3      CONTINUE

        print *,' < eb > = ',ebav
        print *,' <etot> = ',etot
        print *,'   mass = ',mass
        print *,'  ektot = ',ektot
        print *,'  eptot = ',www
        print *,'  ektot/mass = ',ektot/mass
        print *,' elow eupp =',elow,eupp
        print *,ektot,etot,mass
        print *,'r min =',rmin
ccc        return
        if(eupp.gt.elow) then
          emax=eupp
          emin=elow
        endif

        dbin=(emax-emin)/float(nbin)
        dbin2=(t2wmax-t2wmin)/float(nbin)
        do i=1,nbin
          ebin(i)=0.
          dist(i)=emin+(float(i)-.5)*dbin
          t2wbin(i)=0.
          dist2(i)=t2wmin+(float(i)-.5)*dbin2
        enddo


C---------
C  Binding Energies
C--------
	DO 5 n = istart, iend
            if(r(n).le.coordmax.and.r(n).ge.coordmin)then
              m=int((eb(n)-emin)/dbin)+1
              if(m.ge.1.and.m.le.nbin)ebin(m)=ebin(m)+1.
              m=int((t2w(n)-t2wmin)/dbin2)+1
              if(m.ge.1.and.m.le.nbin)t2wbin(m)=t2wbin(m)+1.
            endif
 5      CONTINUE


        nbods=iend+1-istart
        do i=1,nbin
          if(ebin(i).le..99)then
            ebin(i)=-10000.
          else
            ebin(i)=ebin(i)/float(nbods)
          endif
          if(t2wbin(i).le..99)then
            t2wbin(i)=-10000.
          else
            t2wbin(i)=t2wbin(i)/float(nbods)
          endif
        enddo



 
C--------
C  Output 
C--------
        
	CALL ebinout(gdev,dist, ebin, dist2, t2wbin, coordmin,coordmax,
     &	istart,iend,r, eb,t2w, ec,r0,infostr)
 
        RETURN
        END
