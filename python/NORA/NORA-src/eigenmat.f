
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine eigenmat(a,x1,x2,x3)


      real*4     b,c
      real*4     a(3,3)
      real*4     adet,a2,a3,a212
      real*4     x1,x2,x3


        a2d=a(1,1)**2.+a(2,2)**2.+a(3,3)**2.
        a2o=a(1,2)**2.+a(1,3)**2.+a(2,3)**2.
        a2=a2d+2.*a2o

        adet=a(1,1)*a(2,2)*a(3,3)+2.*a(1,2)*a(1,3)*a(2,3)
        adet=adet-a(1,1)*a(2,3)*a(2,3)-a(2,2)*a(1,3)*a(1,3)
        adet=adet-a(3,3)*a(1,2)*a(1,2)

        b=-a2/2.
        c=-adet

        call cubic(b,c,x1,x2,x3)

       return
      end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine cubic(b,c,x1,x2,x3)


c     Solving the cubic equation  x^3+ b x + c = 0, whereby all 
c     roots are real. The roots are ordered in descending order.
c     Solving according to the method in Numerical Recipes, pg. 179 (2nd ed)


      double precision    twopi
      parameter           (twopi=6.283185307179586d0)

      real*4      b,c
      real*4      x1,x2,x3
 
      
        Q=-b/3.
        R=c/2.

        Q3=Q*Q*Q
        R2=R*R
        Q12=sqrt(Q)

        if (R2.gt.Q3) then 
          if (R.gt.0.) then 
            theta=1.
          else
            theta=-1.
          endif
        else
          theta=R/sqrt(Q3)
          theta=acos(theta)
        endif

        phi=theta/3.
        x1=-2.*Q12*cos(phi)
        phi=(theta+twopi)/3.
        x2=-2.*Q12*cos(phi)
        phi=(theta-twopi)/3.
        x3=-2.*Q12*cos(phi)

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
        

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      real function matdet(a)


      real*4     a(3,3)


        matdet=a(1,1)*a(2,2)*a(3,3)+2.*a(1,2)*a(1,3)*a(2,3)
        matdet=matdet-a(1,1)*a(2,3)*a(2,3)-a(2,2)*a(1,3)*a(1,3)
        matdet=matdet-a(3,3)*a(1,2)*a(1,2)

       return
      end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      real function mat2(a)


      real*4     a(3,3)


        a2d=a(1,1)**2.+a(2,2)**2.+a(3,3)**2.
        a2o=a(1,2)**2.+a(1,3)**2.+a(2,3)**2.
        mat2=a2d+2.*a2o

       return
      end


