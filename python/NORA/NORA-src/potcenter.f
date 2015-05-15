

C*****************************************************************************
 
 
                SUBROUTINE potcenter(istart, iend, potfrac)
 
 
C*****************************************************************************
 
        IMPLICIT NONE

	INCLUDE 'nora.def'
        integer ipar
        PARAMETER(IPAR=100000)
        INTEGER n, istart, iend, npot, nin
        INTEGER indx(ipar)
 
        REAL coord(nmax), xmed, ymed, zmed, vxmed, vymed, vzmed, mdian2
	REAL potlow, pothigh, potfrac, pot1, pot2
 

        n=iend-istart+1
        call indexx(n,ppot,indx)
        nin=int(n*potfrac)
        print *,' nin = ',nin
        xmed=0.
        ymed=0.
        zmed=0.
        vxmed=0.
        vymed=0.
        vzmed=0.

        DO 10 n = 1,nin
             xmed=xmed+x(indx(n))
             ymed=ymed+y(indx(n))
             zmed=zmed+z(indx(n))
 10     enddo
        xmed=xmed/float(nin)
        ymed=ymed/float(nin)
        zmed=zmed/float(nin)

        nin=iend-istart+1
        do 11 n=1,nin
             vxmed=vxmed+vx(indx(n))
             vymed=vymed+vy(indx(n))
             vzmed=vzmed+vz(indx(n))
 11     CONTINUE
        vxmed=vxmed/float(nin)
        vymed=vymed/float(nin)
        vzmed=vzmed/float(nin)
C----------
C  Subtract from coordinates
C---------
        print *,'>>   CoM position : ',xmed,ymed,zmed
        print *,'>> CoM velocities : ',vxmed,vymed,vzmed

        DO 70 n = istart, iend
            x(n)  = x(n)  - xmed
            y(n)  = y(n)  - ymed
            z(n)  = z(n)  - zmed
            vx(n) = vx(n) - vxmed
            vy(n) = vy(n) - vymed
            vz(n) = vz(n) - vzmed
 70     CONTINUE
 
        RETURN
        END
