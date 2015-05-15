C************************************************************************
 
                SUBROUTINE cambioeje(gdev,eulert,euleri,nbods)

C************************************************************************
 
	IMPLICIT NONE
	INCLUDE 'nora.def'
        INTEGER n,l,nbods
C        REAL Rmax,zmod,coordmax,rmod
c        INTEGER istart,iend
c        PARAMETER (Rmax=20)
        REAL eulert,euleri,pi

        CHARACTER*(*) gdev
c        REAL x2(nmax), y2(nmax), z2(nmax)
        REAL xp,yp,zp,xpp,ypp,zpp
        REAL sint1,cost1,sinp1,cosp1

c-----------------------
c     cambio de coordenadas
c-----------------------
        pi = 3.141592654
 

C------------
C  Calculate sines and cosines
C------------
        sint1  = SIN (eulert * pi / 180.)
        cost1  = COS (eulert * pi / 180.)
        sinp1  = SIN (euleri * pi / 180.)
        cosp1  = COS (euleri * pi / 180.)

        print*,nbods,eulert,euleri,sint1,cost1,sinp1,cosp1


        do 30 l=1,nbods
           
           xp=x(l)
           yp=y(l)
           zp=z(l)


C-------------
C Then rotate by PHI1 around zpp axis
C-------------
                xpp = xp * cosp1 + yp * sinp1
                ypp = -xp * sinp1 + yp * cosp1
                zpp = zp
 
C-------------
C  First rotate by THETA1 around xp axis
C-------------
                x(l) =  xpp 
                y(l) =  ypp * cost1 + zpp * sint1
                z(l) = -ypp * sint1 + zpp * cost1

 30     CONTINUE     

        RETURN
        END
      
