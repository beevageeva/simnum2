C***********************************************************************
               SUBROUTINE xvpwrite2 
     1	       (modelnum, filename, header, istart, iend,
     1          x, y, z, vx, vy, vz, pmass, ppot, 
     1          ierr)
C***********************************************************************
C
C
C     Subroutine to output the body data in a format identical to
C     that can be read by NUREAD:  Unformatted, direct access records.
C     128 particles to a direct access record.  7 numbers per particle. 
C     nobytes=number of bytes per real number, machine dependent.    
C     Header:  A 128-real number header record precedes each model.  
C
C*    Version of outbods which writes files with pos, vel, pot
C*    or with pos, vel, pmass
C*    MB 23/9/93
C
C=======================================================================
 
C-----------------------------------------------------------------------
 
C       header(1)=number particles
C       header(2)=iteration number
C       header(3)=the time
C       header(4)=the total energy
C       header(5)=the total angular momentum
C       header(8)=G (Newton's constant)
C       header(9)=epsilon (the softening length)
C       header(18)=1 if a satellite is present , 0 if not.
C       header(19)=ndim
C     header(20)=1 if two-galaxies model, 0 otherwise
C     header(21)=ngal1+1
C     header(22)=mtot1
C     header(23)=r0 galaxy 1 (half mass radius)
C     header(24)=rmax galaxy 1 (initial rmax)
C     header(25,26,27)= am(x,y,z) galaxy 1
C     header(28)=mtot2
C     header(29)=r0 galaxy 2 (half mass radius)
C     header(30)=rmax galaxy 2 (initial rmax)
C     header(31,32,33)= am(x,y,z) galaxy 2
C       header(100) = 1         file contains xvp, not xvm, 0 otherwise
C       header(101) = nmasses   number of body mass groups
C       header(102) = n1        bodies 1:n1 have mass m1
C       header(103) = m1        the body mass m1
C       header(104) = n2        bodies n1+1:n2 have mass m2
C       header(105) = m2
C       header(106) = n3
C       header(107) = m3        etc     max 13 mass groups
c
c       nobytes = 8 CRAY
c       nobytes = 4 VAX UNIX
c       nobytes = 1 VAX VMS
C----------------------------------------------------------------------

	REAL x, y, z, vx, vy, vz, pmass, ppot
	DIMENSION x(*),  y(*),  z(*),
     &            vx(*), vy(*), vz(*), pmass(*), ppot(*)
	REAL header(*)
	INTEGER modelnum
	PARAMETER (irecsize = 128)
        PARAMETER (nobytes = 4)
	CHARACTER*40 filename
 
        INTEGER extend,records, ubods, ulog
	PARAMETER(ubods=11, ulog=6)
        LOGICAL tail

        isize=(7*irecsize)*nobytes
        tail=.false.
        ierr=0.

        OPEN(unit=ubods,file=filename,access='direct',
     &       form='unformatted',status='unknown',recl=isize)

        header(1)=iend
        n=header(1)+1.e-15
c        n=iend+1.e-15
        numrec=n/irecsize + 1
        remain=(float(n)/float(irecsize)) -  n/irecsize
        IF(remain.ne.0)THEN
          numrec=numrec+1
          tail=.true.
          ENDIF 
        ireci=(modelnum-1)*numrec+1
        irec=ireci
        i=1
c        i=istart
        extend=irecsize
        WRITE(ubods,rec=irec)(header(j),j=1,irecsize)

        irec=irec+1
        DO 10 j=2,numrec
           IF(j.eq.numrec)THEN
              IF(tail)THEN
                 extend=n-(numrec-2)*irecsize
                 ENDIF
              ENDIF 
           k1=i
           k2=i+extend-1
c        k1=istart
c        k2=iend
c        print*,k1,k2
c        pause
        IF (INT(header(100)) .EQ. 0) THEN
           WRITE(ubods,rec=irec)
     &                   (x(l), y(l), z(l), vx(l), vy(l), vz(l), 
     &                   pmass(l),l=k1,k2)
        ELSE IF (INT(header(100)) .EQ. 1) THEN
           WRITE(ubods,rec=irec)
     &                   (x(l), y(l), z(l), vx(l), vy(l), vz(l),
     &                   ppot(l),l=k1,k2)
        ELSE
                PRINT*, 'xvpwrite>> ambiguous file format'
                CLOSE(ubods)
                RETURN
        END IF
           i=i+extend
           irec=irec+1
  10       CONTINUE  
        CLOSE(unit=ubods)
        IF(tail)THEN
           records=numrec-2
           WRITE(ulog,100)records,irecsize,extend,modelnum,filename
 100       FORMAT(1x,'outbods>> Writing ',i3,'*',i4,'+',i4,
     &           ' particles to model ',i3,' of file :',a20)
           ELSE 
           records=numrec-1
           WRITE(ulog,200)records,irecsize,modelnum,filename
 200       FORMAT(1x,'xvpwrite>> Writing ',i3,'*',i4,
     &           ' particles to model ',i3,' of file :',a40)
           ENDIF 

        RETURN
        END
