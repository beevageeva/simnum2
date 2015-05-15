C---------------------------------------------------------------------
               SUBROUTINE parti(gdev,coordmin,coordmax,
     &istart, iend,infostr)
C---------------------------------------------------------------------
C     Gives number of particles within a certain radius with different
C                                 masses.
C                             A.C.G.G. 7/4/00
C---------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'nora.def'

        INTEGER n, istart, iend,partic,pfirst,psecond
        REAL coordmax,coordmin,tmass
        REAL massdos, unomass,dosmass
	CHARACTER*(*) gdev, infostr(*)

        REAL r, th, ph, PI,r0
	DIMENSION r(nmax), th(nmax), ph(nmax)

	PI = ACOS(-1.)

        CALL XYZtoRTP(istart, iend, x, y, z, r, th, ph)
        r0=-1000.
        partic=0
        tmass=0.
        pfirst=0
        psecond=0
	DO 3 n = istart, iend            
           r0=max(r0,r(n))
            if(r(n).le.coordmax.and.r(n).ge.coordmin)then 
             tmass=tmass+pmass(n)
             partic=partic+1
              if(pmass(n).eq.pmass(1))then
                 pfirst=pfirst+1
                 else
                    psecond=psecond+1
                    massdos=pmass(n)
              endif
            endif
 3      CONTINUE
        unomass=pfirst*pmass(1)
        dosmass=psecond*massdos
C--------------------------------------------------------------
C     OUTPUT
C--------------------------------------------------------------
        WRITE(6,*),('Ntot   Npart1  Npart2     Mass1     Mass2')
        WRITE(6,*),partic,pfirst,psecond,unomass,dosmass
        
        RETURN
        END
