C====================================================================
C
	SUBROUTINE ISOPLOT
C
C====================================================================

C		DIBUJA LOS PARAMETROS RESULTANTES DE PROF4
C		QUE ESTAN EN EL FICHERO *LOG*.DAT
c
C	LOS PARAMETROS SON
C
C		1.-   LOG INTENSIDAD -- R**0.25
C		2.-   LOG MODA       -- R**0.25
C		3.-   P.A.           -- LOG R
C		4.-   ELLIPTICITY    -- LOG R
C		5.-   B4             -- LOG R
C		6.-   B3             -- LOG R
C		7.-   B2             -- LOG R
C		8.-   B1             -- LOG R
C		9.-   XC             -- LOG R
C	       10.-   YC             -- LOG R
C	       11.-   DR/R           -- LOG R
C	  -------------------------------------------------
C
C				JIGS  GRON  MARZ-89
C				MBC GRoningen Oct 94
C        UTILIZA PGPLOT

	CHARACTER*32 ENTRADA
	REAL A(30,200),X(200),y(1,200),Y1(200),ybis(200),z(1,200)
	CHARACTER*70 TITLE
	CHARACTER*12 CACHA
	pi=4.0*atan(1.0)
	
	PRINT*,' FICHERO prf:   '
	READ(*,'(A)') ENTRADA
	OPEN(1,FILE=ENTRADA,STATUS='OLD')
	PRINT*,' length units / pixel (2 * rmax / 100) :   '
	READ(*,*) PIXEL


	
	PRINT*,' TITULO   '
	READ(*,'(A)') TITLE
	I=1
1	READ(1,*,END=2) (A(J,I),J=1,30)
	A(4,I)=PIXEL*A(4,I)                !SEMIEJE MAYOR EN unidades fisicas
	I=I+1
	GOTO 1
2	CONTINUE
	N=I-1
	PRINT*,'HAY ', N ,' PUNTOS '
	PRINT*,' CUANTOS PINTO?  '
	READ(*,*) IN

C	CALCULO MAXIMO Y MINIMO DEL SEMIEJE MAYOR
	XMAX=A(4,1)
	XMIN=XMAX
	DO I=1,IN
		IF(A(4,I).GT.XMAX) XMAX=A(4,I)
		IF(A(4,I).LT.XMIN) XMIN=A(4,I)
	ENDDO
	PRINT*,' MAXIMO Y MINIMO :    '
	PRINT*, XMAX, XMIN
	

C	ABRIMOS DEVICE
	Print*,'Device: '
	Read(*,'(a)') Cacha
	CALL PGBEGIN(0,Cacha,4,2)

	Call Pgvport (0.2,0.9,0.15,0.85)
	CALL PGSCH(1.5)
	CALL PGSLW(1)

	CALL PGPAGE

C	LOG I -- R**.25

	IPLOT=1
	idim=30
	CALL MAXMIN(A,IN,idim,IPLOT,YMAX,YMIN)
	DO KK=1,IN
	X(KK)=(A(4,KK))**0.25
	IF(A(1,KK).LE.0.0) THEN
	Y1(KK)=0.
	ELSE
	Y1(KK)=ALOG10(A(1,KK)-BACK)
	ENDIF
	ENDDO
	IF(YMIN.LE.0.0) THEN
	YMIN=-3.0
	ELSE
	YMIN=ALOG10(YMIN-BACK)-0.1
	ENDIF
	YMAX=ALOG10(YMAX-BACK)+0.1 
	XXMAX=XMAX**0.25+.5
	XXMIN=XMIN**0.25-0.5
	CALL PGWINDOW(XXMIN,XXMAX,YMIN,YMAX)
	CALL PGBOX('SBCNT',0.0,0,'SBCNT',0.0,0)
	CALL PGLABEL('a\u1/4\d ','Log I', title)
	CALL PGPOINT(IN,X,Y1,17)
	CALL PGADVANCE

C	LOG I (MODA) -- R**.25 + LOG I -- R**.25

	IPLOT=5
	CALL MAXMIN(A,IN,idim,IPLOT,YMAX,YMIN)
	DO KK=1,IN
	X(KK)=(A(4,KK))**0.25
	IF(A(5,KK).LE.0.0) THEN
	y(1,KK)=0.0
	ELSE
	y(1,KK)=ALOG10(A(5,KK))
	ENDIF
	ENDDO
	IF(YMIN.LE.0.0) THEN
	YMIN=-3.0
	ELSE
	YMIN=ALOG10(YMIN)-0.1
	ENDIF
	YMAX=ALOG10(YMAX)+0.1
	XXMAX=XMAX**0.25+.5
	XXMIN=XMIN**0.25-0.5
	CALL PGWINDOW(XXMIN,XXMAX,YMIN,YMAX)
	CALL PGBOX('SBCNT',0.0,0,'SBCNT',0.0,0)
	CALL PGLABEL('a\u1/4\d ','Log I (moda+med) (counts)', title)
	CALL PGPOINT(IN,X,Y,2)	
	CALL PGPOINT(IN,X,Y1,17)
	CALL PGADVANCE

c angulo posicion log(r)

	IPLOT=14
	CALL MAXMIN(A,IN,idim,IPLOT,YMAX,YMIN)
	DO KK=1,IN
	X(KK)=ALOG10(A(4,KK))
	y(1,KK)=A(IPLOT,KK)                              !COORDENADAS FRAME
	ENDDO
	YMIN=YMIN-10.
	YMAX=YMAX+10.
	XXMAX=ALOG10(XMAX)+0.1
	XXMIN=ALOG10(XMIN)-0.1
	CALL PGWINDOW(XXMIN,XXMAX,YMIN,YMAX)
	CALL PGBOX('SBCNT',0.0,0,'SBCNT',0.0,0)
	CALL PGLABEL('Log a ','P.A. (degrees)', title)
	CALL PGPOINT(IN,X,Y,17)
	CALL PGADVANCE

C	elipticidad

	IPLOT=13
	CALL MAXMIN(A,IN,idim,IPLOT,YMAX,YMIN)
	DO KK=1,IN
	X(KK)=ALOG10(A(4,KK))
	y(1,KK)=A(IPLOT,KK)
	ENDDO
	YMIN=YMIN-0.05
	YMAX=YMAX+0.05
	XXMAX=ALOG10(XMAX)+.1
	XXMIN=ALOG10(XMIN)-0.1
	CALL PGWINDOW(XXMIN,XXMAX,YMIN,YMAX)
	CALL PGBOX('SBCNT',0.0,0,'SBCNT',0.0,0)
	CALL PGLABEL('Log a ','Ellipticity', title)
	CALL PGPOINT(IN,X,Y,17)
	CALL PGADVANCE

C	B1 B2 A1 A2 (CROSSES)   -- LOG R

	IPLOT=20
	iplotb=iplot-1
	CALL MAXMIN(A,IN,idim,IPLOT,YMAX,YMIN)
	CALL MAXMIN(A,in,idim,IPLOTb,YMAXBIS,YMINBIS)
	IF (YMAXBIS.GT.YMAX) YMAX=YMAXBIS
	IF (YMINBIS.LT.YMIN) YMIN=YMINBIS
	DO KK=1,IN
	X(KK)=ALOG10(A(4,KK))
	y(1,KK)=A(IPLOT,KK)                             
	ybis(KK)=A(19,KK)
	ENDDO
	YMIN=YMIN-0.05
	YMAX=YMAX+0.05
	XXMAX=ALOG10(XMAX)+0.1
	XXMIN=ALOG10(XMIN)-0.1
	CALL PGWINDOW(XXMIN,XXMAX,YMIN,YMAX)
	CALL PGBOX('SBCNT',0.0,0,'SBCNT',0.0,0)
	CALL PGLABEL('Log a ','B1, A1', title)
	CALL PGPOINT(IN,X,Y,17)
	CALL PGPOINT (IN,X,YBIS,2)
	CALL PGADVANCE

C	B2 A2

	IPLOT=22
	iplotb=iplot-1
	CALL MAXMIN(A,IN,idim,IPLOT,YMAX,YMIN)
	CALL MAXMIN(A,IN,idim,IPLOTb,YMAXBIS,YMINBIS)
	if (ymaxbis.gt.ymax) ymax=ymaxbis
	if (yminbis.lt.ymin) ymin=yminbis
	DO KK=1,IN
	X(KK)=ALOG10(A(4,KK))
	y(1,KK)=A(IPLOT,KK)
	ybis(kk)=a(iplot-1,kk)
	ENDDO
	YMIN=YMIN-0.05
	YMAX=YMAX+0.05
	XXMAX=ALOG10(XMAX)+.1
	XXMIN=ALOG10(XMIN)-0.1
	CALL PGWINDOW(XXMIN,XXMAX,YMIN,YMAX)
	CALL PGBOX('SBCNT',0.0,0,'SBCNT',0.0,0)
	CALL PGLABEL('Log a ','B2, A2', title)
	CALL PGPOINT(IN,X,Y,17)
	CALL PGPOINT(in,x,ybis,2)
	CALL PGADVANCE

C	B3,B4, A3, A4  -- LOG R

	IPLOT=24
	iplotb=iplot-1	
	CALL MAXMIN(A,IN,idim,IPLOT,YMAX,YMIN)
	call maxmin(a,in,idim,iplotb,ymaxbis,yminbis)
	if (ymaxbis.gt.ymax) ymax=ymaxbis
	if (yminbis.lt.ymin) ymin=yminbis

	DO KK=1,IN
	X(KK)=ALOG10(A(4,KK))
	y(1,KK)=A(IPLOT,KK)                      
	ybis(kk)=a(iplot-1,kk)       
	ENDDO
	YMIN=YMIN-0.05
	YMAX=YMAX+0.05
	XXMAX=ALOG10(XMAX)+0.1
	XXMIN=ALOG10(XMIN)-0.1
	CALL PGWINDOW(XXMIN,XXMAX,YMIN,YMAX)
	CALL PGBOX('SBCNT',0.0,0,'SBCNT',0.0,0)
	CALL PGLABEL('Log a ','B3, A3', title)
	CALL PGPOINT(IN,X,Y,17)
	call pgpoint(in,x,ybis,2)
	CALL PGADVANCE

C	B4

	IPLOT=26
	iplotb=iplot-1
	CALL MAXMIN(A,IN,idim,IPLOT,YMAX,YMIN)
	call maxmin(a,in,idim,iplotb,ymaxbis,yminbis)
	if (ymaxbis.gt.ymax) ymax=ymaxbis
	if (yminnis.le.ymin) ymin=yminbis

	DO KK=1,IN
	X(KK)=ALOG10(A(4,KK))
	y(1,KK)=A(IPLOT,KK)
	ybis(kk)=a(iplot-1,kk)
	ENDDO
	YMIN=YMIN-0.05
	YMAX=YMAX+0.05
	XXMAX=ALOG10(XMAX)+.1
	XXMIN=ALOG10(XMIN)-0.1
	CALL PGWINDOW(XXMIN,XXMAX,YMIN,YMAX)
	CALL PGBOX('SBCNT',0.0,0,'SBCNT',0.0,0)
	CALL PGLABEL('Log a ','B4, A4', title)
	CALL PGPOINT(IN,X,Y,17)
	CALL PGPOINT(in,x,ybis,2)
	CALL PGADVANCE
c
c	Amplitud y fase 3theta

	IPLOT=24
	iplotb=iplot-1
	do kk=1,in
		aaa3=a(iplotb,kk)
		bbb3=a(iplot,kk)
		amp3=sqrt(aaa3**2+bbb3**2)
c		fase3=(1./3.)*atan(aaa3/bbb3)
		ttheta=atan(aaa3/bbb3)
		if(bbb3.le.0.0) ttheta=ttheta+pi		
		fase3=ttheta/3.

c		if (fase3.lt.-) fase3=fase3+pi
c		if (fase3.ge.pi/2.) fase3=fase3-pi/2.
		x(kk)=alog10(a(4,kk))
		y(1,kk)=fase3*180./pi
		z(1,kk)=amp3
	enddo
	YMIN=-90.0
	YMAX=90.0 
	XXMAX=ALOG10(XMAX)+.1
	XXMIN=ALOG10(XMIN)-0.1
	CALL PGWINDOW(XXMIN,XXMAX,YMIN,YMAX)
	CALL PGBOX('SBCNT',0.0,0,'SBCNT',0.0,0)
	CALL PGLABEL('Log a ','Phase 3 theta', title)
	CALL PGPOINT(IN,X,Y,17)
	CALL PGADVANCE

	call maxmin (z,in,1,1,ymax,ymin)
	ymin=ymin-0.05
	ymax=ymax+0.05
	call pgwindow (xxmin,xxmax,ymin,ymax)
	call pgbox ('SBCNT',0.0,0,'SBCNT',0.0,0)
	call pglabel('Log a ','Amplitude 3 theta',title)
	call pgpoint(in,x,z,17)
	CALL PGADVANCE

	IPLOT=26
	iplotb=iplot-1
	do kk=1,in
		aaa4=a(iplotb,kk)
		bbb4=a(iplot,kk)
		amp4=sqrt(aaa4**2+bbb4**2)
c		fase4=0.25*atan(aaa4/bbb4)
c		if (fase4.lt.0.0) fase4=fase4+pi
c		if (fase4.ge.pi/2.) fase4=fase4-pi/2.
		ttheta=atan(aaa4/bbb4)
		if(bbb4.le.0.0) ttheta=ttheta+pi
		fase4=ttheta/4.0
		x(kk)=alog10(a(4,kk))
		y(1,kk)=fase4*180./pi
		z(1,kk)=amp4
	enddo
c	CALL MAXMIN(y,IN,1,YMAX,YMIN)
	YMIN=-90.0
	YMAX=90.0 
	XXMAX=ALOG10(XMAX)+.1
	XXMIN=ALOG10(XMIN)-0.1
	CALL PGWINDOW(XXMIN,XXMAX,YMIN,YMAX)
	CALL PGBOX('SBCNT',0.0,0,'SBCNT',0.0,0)
	CALL PGLABEL('Log a ','Phase 4 theta', title)
	CALL PGPOINT(IN,X,Y,17)
	CALL PGADVANCE

	call maxmin (z,in,1,1,ymax,ymin)
	ymin=ymin-0.05
	ymax=ymax+0.05
	call pgwindow (xxmin,xxmax,ymin,ymax)
	call pgbox ('SBCNT',0.0,0,'SBCNT',0.0,0)
	call pglabel('Log a ','Amplitude 4 theta',title)
	call pgpoint(in,x,z,17)
	CALL PGADVANCE	

C	XC,YC -- LOG R

	IPLOT=15
	CALL MAXMIN(A,IN,idim,IPLOT,YMAX,YMIN)

	DO KK=1,IN
	X(KK)=ALOG10(A(4,KK))
	y(1,KK)=A(IPLOT,KK)                             
	ENDDO
	YMIN=YMIN-3.
	YMAX=YMAX+3.
	XXMAX=ALOG10(XMAX)+0.1
	XXMIN=ALOG10(XMIN)-0.1
	CALL PGWINDOW(XXMIN,XXMAX,YMIN,YMAX)
	CALL PGBOX('SBCNT',0.0,0,'SBCNT',0.0,0)
	CALL PGLABEL('Log a ','XC (pix)', title)
	CALL PGPOINT(IN,X,Y,17)
	CALL PGADVANCE

C	YC

	IPLOT=16
	CALL MAXMIN(A,IN,idim,IPLOT,YMAX,YMIN)

	DO KK=1,IN
	X(KK)=ALOG10(A(4,KK))
	y(1,KK)=A(IPLOT,KK)
	ENDDO
	YMIN=YMIN-3.
	YMAX=YMAX+3.
	XXMAX=ALOG10(XMAX)+.1
	XXMIN=ALOG10(XMIN)-0.1
	CALL PGWINDOW(XXMIN,XXMAX,YMIN,YMAX)
	CALL PGBOX('SBCNT',0.0,0,'SBCNT',0.0,0)
	CALL PGLABEL('Log a ','YC (pix)', title)
	CALL PGPOINT(IN,X,Y,17)
	CALL PGADVANCE
c	
c	DELTA R/R -- LOG R

	XC1=A(15,1)
	YC1=A(16,1)
	YMAX=-99999.
	
	DO KK=1,IN
	X(KK)=ALOG10(A(4,KK))
	RAD=SQRT((A(15,KK)-XC1)**2+(A(16,KK)-YC1)**2)
	y(1,KK)=PIXEL*RAD/A(4,KK) 
	IF(y(1,KK).GT.YMAX) YMAX=y(1,KK)
	ENDDO
	YMIN=0.0
	YMAX=YMAX+.1
	XXMAX=ALOG10(XMAX)+0.1
	XXMIN=ALOG10(XMIN)-0.1
	CALL PGWINDOW(XXMIN,XXMAX,YMIN,YMAX)
	CALL PGBOX('SBCNT',0.0,0,'SBCNT',0.0,0)
	CALL PGLABEL('Log a ','\gD R/R', title)
	CALL PGPOINT(IN,X,Y,17)


	CALL PGEND

	STOP
	END

	SUBROUTINE MAXMIN(B,INN,idim,IPLOT,YMAX,YMIN)
	REAL B(idim,200), YMAX,YMIN
	YMAX=B(IPLOT,1)
	YMIN=YMAX
	DO K=1,INN
	IF(B(IPLOT,K).GT.YMAX) YMAX=B(IPLOT,K)
	IF(B(IPLOT,K).LT.YMIN) YMIN=B(IPLOT,K)
	ENDDO
	RETURN
	END

