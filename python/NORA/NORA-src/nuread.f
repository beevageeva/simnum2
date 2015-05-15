 

c************************************************************************
C                                                                      
C
      SUBROUTINE nuread(model,filename,ierr)
C
C
c************************************************************************
C*    Subroutine to read unformatted models from the file : filename    *
C*    model is the requested model number.                              *
C------------------------------------------------------------------------
C     header(1)=number particles
C     header(2)=iter
C     header(3)=time
C     header(4)=etot
C     header(5)=am
C     header(6)=total mass
C     header(8)=G (Newton's constant)
C     header(9)=epsilon
C     ierr=0 : no errors
C     ierr=1 : error on read error codes are ios1 ios2 ios3

      INCLUDE 'nora.def'
      
      PARAMETER (irecsize=128)
      PARAMETER (nobytes=4)
      PARAMETER (lu=11)

      DIMENSION headbuff(irecsize)
      CHARACTER*40 filename
      INTEGER extend

c     nobytes = 8 CRAY
c     nobytes = 4 VAX UNIX
c     nobytes = 1 VAX VMS
 
      LOGICAL tail

      ISIZE=(7*irecsize)*nobytes
      tail=.false.
      ierr=0.

      open(unit=lu,file=filename,access='direct',
     &     status='unknown',recl=isize)

      read(lu,rec=1,err=101,iostat=ios1)(headbuff(j),j=1,irecsize)
      if(headbuff(1).lt.0.or.headbuff(1).gt.1000000)THEN
         write(6,161)
 161     format(1x,'nuread>> Bad initial header <<')
         ierr=1
         goto 101
         endif
      n=headbuff(1)
      numrec=n/irecsize + 1
      remain=(float(n)/float(irecsize)) -  n/irecsize
      if(remain.ne.0)then
        numrec=numrec+1
        tail=.true.
        endif
      ireci=(model-1)*numrec+1
      irec=ireci
      i=1
      extend=irecsize

      read(lu,rec=irec,err=101,iostat=ios2)
     &    (headbuff(j),j=1,irecsize)

c     This bit is because the stupid CRAY doesn't know whether a given
c     record exists or not. Hence a record beyound the last record
c     written can be read without tripping the read error flag and
c     hence delivers junk into the header. *#!*$@!!!!!!

      if(headbuff(1).ne.float(n))then
         ierr=1
         write(6,999)
 999     format(1x,'nuread>> End of file <<')
         goto 101
         endif

      DO 163 jj=1,irecsize
 163     header(jj)=headbuff(jj)

      irec=irec+1
      do 10 j=2,numrec
         if(j.eq.numrec)then
            if(tail)then
               extend=n-(numrec-2)*irecsize
               endif
            endif
         k1=i
         k2=i+extend-1
         read(lu,rec=irec,err=101,iostat=ios3)
     &       (x(l),y(l),z(l),vx(l),vy(l),vz(l)
     &                  ,pmass(l),l=k1,k2)
         i=i+extend
         irec=irec+1
  10     continue
       CLOSE(unit=lu)
       write(6,100)n,model,filename
 100   format(1x,'nuread>> Reading ',I8,' particles from model ',
     &          I8,' of file :',a40)
       goto 102
 101   write(6,103)
 103   format(1x,'nuread>> Error on read << ')
       write(6,104)ios1,ios2,ios3
 104   format(1x,'nuread>> ios1 = ',i5,' ios2 = ',i5,
     &        ' ios3 = ',i5)
       ierr=1
       CLOSE(unit=lu)
 102   RETURN
      END
