C************************************************************************
 
                SUBROUTINE zscale(gdev,coordmax,istart,iend)

C************************************************************************
 
	IMPLICIT NONE
	INCLUDE 'nora.def'
        INTEGER nbin, uout
        INTEGER istart,iend
        PARAMETER (nbin=160, uout=18)
        INTEGER n,l,k
        REAL Rmax,zmod,coordmax,rmod
c        PARAMETER (Rmax=20)
        REAL facd,facu,zmeana(nbin),zmean(nbin),znum(nbin)
        INTEGER zbin(nbin),ztot
        CHARACTER*(*) gdev


        
C--------------
c     Read particle's z component and bin it
C--------------

        print*,' '
        Rmax=coordmax

        do 30 n=1,nbin
           zbin(n)=0
           znum(n)=0.
           zmeana(n)=0.
 30         continue
            ztot=0


        DO 10 n=1,nbin

        DO 40 k=istart,iend    

           facd=Rmax*(n-1)/nbin
           facu=Rmax*n/nbin
           
           zmod=(z(k)*z(k))
c           if (zmod.gt.0.4) then 
c              goto 40
c           endif   
           rmod=sqrt(x(k)*x(k)+y(k)*y(k))
           if (rmod.lt.facu .and.rmod.gt.facd) then
           zbin(n)= zbin(n)+1
           znum(n)=znum(n)+1.
           zmeana(n)=zmeana(n)+zmod
c           if (n.eq.140) then
c              print*,zmod,z(k)
c           endif   
           end if


 40     CONTINUE

 10     CONTINUE   

        DO 25 l=1,nbin

           zmeana(l)=zmeana(l)/znum(l)
           zmean(l)=(zmeana(l))
           ztot=ztot+zbin(l)
        print*,Rmax*l/nbin,zmeana(l),zbin(l),zmeana(l)*znum(l),l
 25     CONTINUE

        IF (gdev(1:4) .EQ. 'file') THEN
        OPEN(uout,FILE='zscale.out',STATUS='UNKNOWN')
        DO 20 l=1,nbin
        WRITE(UOUT,*)Rmax*l/nbin,zmeana(l),zbin(l)
 20     CONTINUE
        CLOSE(uout)
        RETURN
        end if

        RETURN
        
        END
