C***************************************************************************
C
             SUBROUTINE VSFIND
     &        (istart, iend, coordmax, sigma3, meanvt2)
C
C***************************************************************************

       IMPLICIT NONE

       
       INCLUDE 'nora.def'

       INTEGER istart, iend
       REAL coordmax
       REAL phi(nmax),vr(nmax), vt(nmax),r(nmax)
       REAL th(nmax),ph(nmax),phiv(nmax)
       REAL rc(nmax),thc(nmax),zc(nmax)
       REAL meanvr,meanvt,meanvz,sigmar2,sigmat2,sigmaz2
       REAL meanvt2,sigma3,liminf,limsup

       INTEGER n,tot,tot2

       tot=0
       tot2=0
       meanvr=0.
       meanvt=0.
       meanvz=0.
       sigmar2=0.
       sigmat2=0.
       sigmaz2=0.
       meanvt2=0.
       sigma3=0.

           CALL XYZtoRTP(istart, iend, 
     &          x, y, z, r, th, ph)

           CALL XYZtoRTZ(istart, iend, 
     &          x, y, z, rc, thc, zc)


       Do 5 n = istart, iend
          
          phi(n)=ATAN2(y(n),x(n))
          vt(n)=(vy(n)*cos(phi(n)))-(vx(n)*sin(phi(n)))
 5     continue   

       do 6 n=1,10
          print*,vt(n)
 6     continue   

       Do 10 n = istart, iend

          if (r(n).gt.1.3.and.r(n).lt.coordmax) then
             tot=tot+1
             meanvr=meanvr+vx(n)
             meanvt=meanvt+vy(n)
             meanvz=meanvz+vz(n)
          endif
 10    CONTINUE

       meanvr=meanvr/tot
       meanvt=meanvt/tot
       meanvz=meanvz/tot
C       print*,'medias y total:',meanvr,meanvt,meanvz,tot,r(1234)
       Do 20 n = istart, iend
          if (r(n).gt.1.3.and.r(n).lt.coordmax) then
            sigmar2=sigmar2+((vx(n)-meanvr)*(vx(n)-meanvr))
            sigmat2=sigmat2+((vy(n)-meanvt)*(vy(n)-meanvt))
            sigmaz2=sigmaz2+((vz(n)-meanvz)*(vz(n)-meanvz))
          endif
 20    CONTINUE
       
       sigmar2=sigmar2/tot
       sigmat2=sigmat2/tot
       sigmaz2=sigmaz2/tot

       sigma3=sigmar2+sigmat2+sigmaz2

       sigma3=sqrt(sigma3)

       liminf=coordmax-1.
c(coordmax/10.)
       if (liminf.lt.0) then 
          liminf=0.
       endif   
       limsup=coordmax+1.
c(coordmax/10.)

c       open(6,status='unknown',file='sigma3.out')

       Do 30 n=istart, iend
          if (rc(n).gt.liminf.and.rc(n).lt.limsup) then
c         if (rc(n).lt.limsup) then

c             if((zc(n)*zc(n)).le.(4.)) then
             if (zc(n).gt.(-2.).and.zc(n).lt.(2.)) then
             tot2=tot2+1
             meanvt2=meanvt2+vt(n)
c             write(6,*) n,vt(n),rc(n),zc(n)
          endif
          endif
 30    CONTINUE

c       close(6)
       meanvt2=meanvt2/tot2


       print*,'sigma3, <V_phi>:',sigma3,meanvt2,tot2,liminf,limsup

       RETURN
       END

