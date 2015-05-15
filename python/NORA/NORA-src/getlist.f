      subroutine getlist(listfile,istart,iend)

C reads in integer list containing indices of particles that need
C to be considered further 
C orders the list

      include 'nora.def'

      character*30 listfile
      INTEGER sublist(nmax)
      open(10,file=listfile,form='unformatted',status='old')
      read(10)nlist
      read(10)(sublist(i),i=1,nlist)
      close(10)

      call sort(nlist,sublist)

      istart=1
      iend=nlist
      do i=istart,iend
        x(i)=x(sublist(i))
        y(i)=y(sublist(i))
        z(i)=z(sublist(i))
        vx(i)=vx(sublist(i))
        vy(i)=vy(sublist(i))
        vz(i)=vz(sublist(i))
        ppot(i)=ppot(sublist(i))
      enddo
      nbods=nlist
      header(1)=float(nbods)
      return
      end
