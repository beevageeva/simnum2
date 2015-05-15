


       SUBROUTINE EvsE(gdev, istart, iend, mod1, mod2,
     &		       filename,elow,eupp,infostr)


       include 'nora.def'

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C   reads two models and calculates binding energies for particles
C   plots e2 vs e1
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       parameter(nbin=40)
       character*40 filename
       character*13 label1,label2
       character*14 label3
       character*(*)gdev, infostr(*)
       real e1(nmax),e2(nmax),emin,emax,etot1,etot2,
     &		el1,eh1,el2,eh2,edge,el,eh,
     &		dist1(nbin),dist2(nbin),elow,
     &		ebin1(nbin),ebin2(nbin),eupp,
     &          debin(nbin),distde(nbin),de(nmax)
       integer mod1,mod2,istart,iend
       emin=1000.e0
       emax=-1000. 
       

       nbods=iend-istart+1


       potfrac=.1

       call getmodel(mod1,filename,nbods)
       CALL potcntr(istart, iend, potfrac)
       eav1=0.
       etot1=0.
       do i=istart,iend
         e1(i)=ppot(i)
     +    +.5*(vx(i)**2+vy(i)**2+vz(i)**2)
         eav1=eav1+e1(i)
         etot1=etot1+e1(i)-.5*ppot(i)
         emin=min(emin,e1(i))
         emax=max(emax,e1(i))
       enddo
       eav1=eav1/float(nbods)
       print *,' e1 = ',eav1
       print *,' etot1 =',etot1
       if(eupp.gt.elow)then
         emax=eupp
         emin=elow
       endif
       edge=abs(emax-emin)*.1
       el1=emin-edge
       eh1=emax+edge
       dbin=(emax-emin)/float(nbin)
       dbin1=dbin
       do i=1,nbin
         ebin1(i)=0.
         dist1(i)=emin+(float(i)-.5)*dbin
       enddo

	DO 5 n = istart, iend
              m=int((e1(n)-emin)/dbin)+1
              if(m.ge.1.and.m.le.nbin)ebin1(m)=ebin1(m)+1.
 5      CONTINUE

       fmax1=-100.
       do i=1,nbin
         ebin1(i)=ebin1(i)/float(nbods)
         fmax1=max(fmax1,ebin1(i))
       enddo

       call getmodel(mod2,filename,nbods)
       CALL potcntr(istart, iend, potfrac)
       emin=1000.e0
       emax=-1000.e0

       eav2=0.
       etot2=0.
       do i=istart,iend
         e2(i)=ppot(i)
     +    +.5*(vx(i)**2+vy(i)**2+vz(i)**2)
         eav2=eav2+e2(i)
         etot2=etot2+e2(i)-.5*ppot(i)
         emin=min(emin,e2(i))
         emax=max(emax,e2(i))
       enddo
       if(eupp.gt.elow)then
         emax=eupp
         emin=elow
       endif
       eav2=eav2/float(nbods)
       print *,' e2 = ',eav2
       print *,' etot2 = ',etot2
       edge=abs(emax-emin)*.1
       el2=emin-edge
       eh2=emax+edge
       el=min(el1,el2)
       eh=max(eh2,eh1)

       dbin=(emax-emin)/float(nbin)
       dbin2=dbin
       do i=1,nbin
         ebin2(i)=0.
         dist2(i)=emin+(float(i)-.5)*dbin
       enddo
        demin=100000.
        demax=-10000.
	DO 10 n = istart, iend
              m=int((e2(n)-emin)/dbin)+1
              if(m.ge.1.and.m.le.nbin)ebin2(m)=ebin2(m)+1.
              de(n)=e2(n)-e1(n)
              demin=min(demin,de(n))
              demax=max(demax,de(n))
 10      CONTINUE
        dedge=.1*(demax-demin)
        dh=demax+dedge
        dl=demin-dedge
        ddebin=(demax-demin)/float(nbin)
        do i=1,nbin
          debin(i)=0.
          distde(i)=demin+(float(i)-.5)*ddebin
        enddo
	DO 20 n = istart, iend
              m=int((de(n)-demin)/ddebin)+1
              if(m.ge.1.and.m.le.nbin)debin(m)=debin(m)+1.
 20      CONTINUE

       edge=(demax-demin)*.1
       demin=demin-edge
       demax=demax+edge

       fmax2=-100.
       fmaxde=-100.
       do i=1,nbin
         ebin2(i)=ebin2(i)/float(nbods)
         fmax2=max(fmax2,ebin2(i))
         debin(i)=debin(i)/float(nbods)
         fmaxde=max(fmaxde,debin(i))
       enddo
       fmaxde=1.1*fmaxde
       fac1=dbin2/dbin1
       fac2=1./fac1
       f1=1.1*max(fmax1,fac2*fmax2)
       f2=1.1*max(fmax2,fac1*fmax1)
       fmax1=f1
       fmax2=f2

       label1=' E model     '
       label2=' E model     '
       label3=' E     - E    '
       write(label1(11:13),'(i3)')mod1
       write(label2(11:13),'(i3)')mod2
       write(label3(4:6),'(i3)')mod2
       write(label3(12:14),'(i3)')mod1

C------
C First Plot : Binding Energy spectra
C-------
        CALL PGBEGIN(0, gdev, 1, 1)        
	CALL PGSCF(2)
	CALL PGASK (.FALSE.)
	CALL PGADVANCE
	CALL PGVPORT (0.2, 0.8, 0.6, 0.95)
	CALL PGWINDOW (el, eh, 0., fmax1)	
	CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL (' ',label1, ' Energy spectra ')
	CALL PGBIN (nbin, dist1, ebin1, .TRUE.)
        do i=1,nbin
          ebin2(i)=ebin2(i)*fac2
        enddo
ccc        CALL PGLINE(nbin,dist2,ebin2)
ccc        CALL PGMOVE(eav1,0.)
ccc        CALL PGDRAW(eav1,fmax1)

	CALL PGVPORT (0.2, 0.8, 0.3, 0.6)
	CALL PGWINDOW (el, eh, 0., fmax2)	
	CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL ('Ebind ', label2, ' ')
        do i=1,nbin
          ebin1(i)=ebin1(i)*fac1
          ebin2(i)=ebin2(i)/fac2
        enddo
	CALL PGBIN (nbin, dist2, ebin2, .TRUE.)
ccc        CALL PGLINE(nbin,dist1,ebin1)
ccc        CALL PGMOVE(eav2,0.)
ccc        CALL PGDRAW(eav2,fmax2)
        do i=1,nbin
          ebin1(i)=ebin1(i)/fac1
        enddo
C--------------------------------------------------------------------------
C Second Plot : scatter plot, Ebind #1 vs. Ebind #2
C--------------------------------------------------------------------------
	CALL PGASK(.TRUE.)
	CALL PGADVANCE
	CALL PGSCF(2)
        CALL PGVPORT (0.2, 0.8, 0.35, 0.95)
        CALL PGWINDOW (el1, eh1, el2, eh2)
        CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
        CALL PGPOINT(iend-istart+1, e1, e2, -1)
        CALL PGLABEL (label1,label2,' ')
        CALL PGMOVE(el,el)
        CALL PGDRAW (eh,eh)
        CALL PGMOVE(eav1,el)
        CALL PGDRAW(eav1,eh)
        CALL PGMOVE(el,eav2)
        CALL PGDRAW(eh,eav2)
C--------
C  Label
C--------
        CALL PGSCH (0.8)
        CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
        CALL PGWINDOW (0., 1., 0., 1.)
        CALL PGTEXT (0., 0.7, 'dE vs E2 plot')
        CALL PGTEXT (0., 0.6, infostr)
ccc        CALL PGTEXT (0., 0.5, infostr2)
        CALL PGSCH (1.0)
C--------------------------------------------------------------------------
C THIRD Plot : ENERGY DIFFERENCE, SPECTRUM
C--------------------------------------------------------------------------
	CALL PGASK(.TRUE.)
	CALL PGADVANCE
	CALL PGSCF(2)
        CALL PGVPORT (0.2, 0.8, 0.35, 0.95)
	CALL PGWINDOW (dl, dh, 0., fmaxde)	
	CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
	CALL PGLABEL (label3,' ', ' ')
	CALL PGBIN (nbin, distde, debin, .TRUE.)
C--------------------------------------------------------------------------
C Fourth Plot : scatter plot, E2-E1 vs. Ebind #1
C--------------------------------------------------------------------------
	CALL PGASK(.TRUE.)
	CALL PGADVANCE
	CALL PGSCF(2)
        CALL PGVPORT (0.2, 0.8, 0.35, 0.95)
        CALL PGWINDOW (el1, eh1, demin,demax)
        CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
        CALL PGPOINT(iend-istart+1, e1, de, -1)
        CALL PGLABEL (label1,label3,' ')
        CALL PGMOVE(el1,0.)
        CALL PGDRAW(eh1,0.)
        CALL PGMOVE(0.,demin)
        CALL PGDRAW(0.,demax)
C--------
C  Label
C--------
        CALL PGSCH (0.8)
        CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
        CALL PGWINDOW (0., 1., 0., 1.)
        CALL PGTEXT (0., 0.7, 'E vs E plot')
        CALL PGTEXT (0., 0.6, infostr)
ccc        CALL PGTEXT (0., 0.5, infostr2)
        CALL PGSCH (1.0)
C--------------------------------------------------------------------------
C Fifth Plot : scatter plot, E2-E1 vs. Ebind #2
C--------------------------------------------------------------------------
	CALL PGASK(.TRUE.)
	CALL PGADVANCE
	CALL PGSCF(2)
        CALL PGVPORT (0.2, 0.8, 0.35, 0.95)
        CALL PGWINDOW (el2, eh2, demin,demax)
        CALL PGBOX ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
        CALL PGPOINT(iend-istart+1, e2, de, -1)
        CALL PGLABEL (label2,label3,' ')
        CALL PGMOVE(el2,0.)
        CALL PGDRAW(eh2,0.)
        CALL PGMOVE(0.,demin)
        CALL PGDRAW(0.,demax)
C--------
C  Label
C--------
        CALL PGSCH (0.8)
        CALL PGVPORT (0.1, 0.8, 0.050, 0.35)
        CALL PGWINDOW (0., 1., 0., 1.)
        CALL PGTEXT (0., 0.7, 'E vs E plot')
        CALL PGTEXT (0., 0.6, infostr)
ccc        CALL PGTEXT (0., 0.5, infostr2)
        CALL PGSCH (1.0)



	CALL PGEND

C---Flush to printer
	CALL psflush (gdev)


       return
       end
