      subroutine translate(dX,dY,dZ,istart,iend)

c translates by vector  (dX, dY, dZ)

      include 'nora.def'
 10   do i=istart,iend
        x(i)=x(i)+dx
        y(i)=y(i)+dy
        z(i)=z(i)+dz
      enddo
      return
      end
