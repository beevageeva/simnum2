c************************************************************************
C                                                                      
C
      SUBROUTINE xvpread(model,filename,header,
     1			x, y, z, vx, vy, vz, pmass, ppot,
     2			ierr)
C
C
c************************************************************************
C*    Subroutine to read unformatted models from the file : filename    *
C*    model is the requested model number.                              *
C*    Version of nuread which reads files with pos, vel, pot
C*    or with pos, vel, pmass
C*    MB 23/9/93
C------------------------------------------------------------------------
C     header(1)=number particles
C     header(2)=iter
C     header(3)=time
C     header(4)=etot
C     header(5)=am
C     header(6)=total mass
C     header(8)=G (Newton's constant)
C     header(9)=epsilon
C	header(100) = 1		file contains xvp, not xvm, 0 otherwise
C	header(101) = nmasses	number of body mass groups
C	header(102) = n1	bodies 1:n1 have mass m1
C	header(103) = m1	the body mass m1
C	header(104) = n2	bodies n1+1:n2 have mass m2
C	header(105) = m2
C	header(106) = n3
C	header(107) = m3	etc	max 13 mass groups
C     ierr=0 : no errors
C     ierr=1 : error on read error codes are ios1 ios2 ios3
C-------------------------------------------------------------------

      REAL x, y, z, vx, vy, vz, pmass, ppot
      DIMENSION x(*),  y(*),  z(*),
     &          vx(*), vy(*), vz(*), pmass(*), ppot(*)
      REAL header,headbuff
      PARAMETER (irecsize=128)
      PARAMETER (nobytes=4)
      PARAMETER (lu=11)

      DIMENSION headbuff(irecsize),header(irecsize)
      CHARACTER*40 filename
      INTEGER extend

      INTEGER mgroups, nfirst, nlast

c     nobytes = 8 CRAY
c     nobytes = 4 VAX UNIX
c     nobytes = 1 VAX VMS
 
      LOGICAL tail

      ISIZE=(7*irecsize)*nobytes
      tail=.false.
      ierr=0.

      open(unit=lu,file=filename,access='direct',
     &     status='OLD',recl=isize,
     &	   ERR=101, IOSTAT=ios0)

      read(lu,rec=1,err=101,iostat=ios1)(headbuff(j),j=1,irecsize)
      if(headbuff(1).lt.1.or.headbuff(1).gt.10000000.)THEN
         write(6,161)
 161     format(1x,'xvpread>> Bad initial header <<')
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
 999     format(1x,'xvpread>> End of file <<')
         goto 101
         endif

      DO 163 jj=1,irecsize
 163     header(jj)=headbuff(jj)

C-------Read according to which body file format we have:
C	header(100) = 0 means standard pjq
C		      1 means mbc format, xvp
      irec=irec+1
      do 10 j=2,numrec
         if(j.eq.numrec)then
            if(tail)then
               extend=n-(numrec-2)*irecsize
               endif
            endif
         k1=i
         k2=i+extend-1
	IF (INT(header(100)) .EQ. 0) THEN
             read(lu,rec=irec,err=101,iostat=ios3)
     &       (x(l),y(l),z(l),vx(l),vy(l),vz(l) ,pmass(l),l=k1,k2)
	ELSE IF (INT(header(100)) .EQ. 1) THEN
             read(lu,rec=irec,err=101,iostat=ios3)
     &       (x(l),y(l),z(l),vx(l),vy(l),vz(l) ,ppot(l),l=k1,k2)
	ELSE
		PRINT*, 'xvpread>> ambiguous file format'
		CLOSE(lu)
		RETURN
	END IF
         i=i+extend
         irec=irec+1
  10     continue
       CLOSE(unit=lu)

C--------If xvp format, assign body masses
	IF (INT(header(100)) .EQ. 1) THEN
	    mgroups = INT(header(101))
	    IF (mgroups .LE. 0 .OR. mgroups .GT. 13) THEN
		PRINT*, 'xvpread>> bad number of mass groups : ', mgroups
		RETURN
	    END IF
	    nlast = 0
	    DO 20 i = 1, mgroups
		nfirst = nlast + 1
		nlast  = header(100 + 2*i)
		DO 15 j = nfirst, nlast
		    pmass(j) = header(100 + 2*i + 1)
15		CONTINUE
20	    CONTINUE
	END IF

       write(6,100)n,header(103),filename
 100   format(1x,'xvpread>> Reading ',I8,' particles from model ',
     &          F10.4,' of file :',a40)
       goto 102
 101   write(6,103)
 103   format(1x,'xvpread>> Error on read << ')
       write(6,104) ios0,ios1,ios2,ios3
 104   format(1x,'xvpread>> ios0 = ', i5, ' ios1 = ',i5,' ios2 = ',i5,
     &        ' ios3 = ',i5)
       ierr=1
       CLOSE(unit=lu)
 102   RETURN
      END
