


C************************************************************************
 
                SUBROUTINE ex (gdev, coordmin, coordmax, elow, eupp,
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
        REAL ll(nmax),exc(nmax),l1,l2,l3,vtot1,vtot2,vmed1,vmed2
        REAL sig,sig1,sig2
        INTEGER cont1,cont2
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
        l1=0.
        l2=0.
        l3=0.
        ek=0.
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
              eb(n)=(ppot(n)+ek)
c*pmass(n)

              l1=(vz(n)*y(n))-(vy(n)*z(n))
              l2=(vx(n)*z(n))-(vz(n)*x(n))
              l3=(vy(n)*x(n))-(vx(n)*y(n))
              ll(n)=((l1*l1)+(l2*l2)+(l3*l3))
c*pmass(n)*pmass(n)

              exc(n)=sqrt((2.*ll(n)*eb(n))+1)



            endif
 3      CONTINUE
        print*,exc(istart),istart,ll(istart),eb(istart)

C---------
C  Binding Energies
C--------
        cont1=0
        cont2=0
        vtot1=0.
        vtot2=0.
        sig1=0.
        sig2=0.
	DO 5 n = istart, iend
            if(exc(n).le.0.15)then
               cont1=cont1+1
               vtot1=vtot1+sqrt(vx(n)*vx(n)+vy(n)*vy(n)+vz(n)*vz(n))
            endif
            if(exc(n).gt.0.85)then
               cont2=cont2+1
               vtot2=vtot2+sqrt(vx(n)*vx(n)+vy(n)*vy(n)+vz(n)*vz(n))
            endif   
 5      CONTINUE

        vmed1=vtot1/cont1
        vmed2=vtot2/cont2

        DO 55 n = istart, iend
            if(exc(n).le.0.15)then
               sig=sqrt(vx(n)*vx(n)+vy(n)*vy(n)+vz(n)*vz(n))-vmed1
               sig1=sig1+sig*sig
            endif
            if(exc(n).gt.0.85)then
               sig=sqrt(vx(n)*vx(n)+vy(n)*vy(n)+vz(n)*vz(n))-vmed2
               sig2=sig2+sig*sig
            endif   
 55      CONTINUE
         sig1=sig1/cont1
         sig2=sig2/cont2


C--------
C  Output 
C--------
        
         print*,'Orbitas circulares: (e=0-0.15)'
         print*,'Num.part.,<V>,sigma^2',cont1,vmed1,sig1,sqrt(sig1)
         print*,'Orbitas radiales: (e=0.85-1)'
         print*,'Num.part.,<V>,sigma^2',cont2,vmed2,sig2,sqrt(sig2)


        RETURN
        END
