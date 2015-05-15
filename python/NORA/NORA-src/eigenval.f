ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine eigenval(a,x1,x2,x3)


      real*4     b,c
      dimension  a(3,3)
c      real*4     a(3,3)
      real*4     adet,a1,a2,a3,a2d,a2o,a21,tr
c     ,c1,c2,c3
      real*4     x1,x2,x3


        a2d=a(1,1)**2.+a(2,2)**2.+a(3,3)**2.
        a2o=a(1,2)**2.+a(1,3)**2.+a(2,3)**2.
        a21=a(1,1)*a(3,3)+a(2,2)*a(3,3)+a(1,1)*a(2,2)
C        a2=a21-(3.*a2o)-a2d
        tr=a(1,1)+a(2,2)+a(3,3)

C        a2=3*(a21-a2o)-tr**2

        adet=a(1,1)*a(2,2)*a(3,3)+2.*a(1,2)*a(1,3)*a(2,3)
        adet=adet-a(1,1)*a(2,3)*a(2,3)-a(2,2)*a(1,3)*a(1,3)
        adet=adet-a(3,3)*a(1,2)*a(1,2)

        a1=-tr
        a2=-1.*(a2o-a21)
        a3=-adet
        

C        b=a2/9.
        
C        c1=-1.*tr*(a21-a2o)
C        c2=-1.*adet
C        c3=-1.*tr*tr*tr

        b=(3*a2-a1**2)/9

C        c=((9.*c1)-(27.*c2)-(2*c3))/54.

        c=((9*a1*a2)-(27*a3)-(2*a1*a1*a1))/54
C        c=-adet

        call cubic(b,c,a1,x1,x2,x3)

       return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine cubic(b,c,a1,x1,x2,x3)


c     Solving the cubic equation  x^3+ a x^2 +b x + c = 0, whereby all 
c     roots are real. The roots are ordered in descending order.


      double precision    twopi
      parameter           (twopi=6.283185307179586d0)

      real*4      b,c
      real*4      x1,x2,x3
      real        theta,phi,xh
      
        Q=b
        R=c

        Q3=Q*Q*Q
        R2=R*R
        Q12=sqrt(-Q)

        if ((-1.*R2).lt.Q3) then 
          if (R.gt.0.) then 
            theta=3.*twopi/2. 
            go to 10
          else
            theta=-3.*twopi/2.
            go to 10
          endif
        else
          theta=(R)/sqrt(-1.*Q3)
          theta=3.*acos(theta)
        endif

 10     continue

        phi=theta
        x1=2.*Q12*cos(phi)-(a1/3.)
        phi=(theta+twopi/6.)
        x2=-2.*Q12*cos(phi)-(a1/3.)
        phi=(theta-twopi/6.)
        x3=2.*Q12*cos(phi)-(a1/3.)

c       Ordering the 3 roots:

        if (x1.lt.x2) then
          xh=x1
          x1=x2
          x2=xh
        endif
        if (x2.lt.x3) then
          xh=x2
          x2=x3
          x3=xh
          if (x1.lt.x2) then
            xh=x1
            x1=x2
            x2=xh
          endif
        endif

       return
      end
