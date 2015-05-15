      subroutine sphere(Rs,istart,iend)

c cuts sphere around origin out of point set, only those points are used 

      include 'nora.def'

      in=0
      do i=istart,iend
        r=sqrt(x(i)**2+y(i)**2+z(i)**2)
        if(r.le.Rs)then
          in=in+1
          x(in)=x(i)
          y(in)=y(i)
          z(in)=z(i)
          vx(in)=vx(i)
          vy(in)=vy(i)
          vz(in)=vz(i)
         ppot(in)=ppot(i)
        endif
      enddo
      istart=1
      iend=in
      nbods=in
      header(1)=float(nbods)
      return
      end
