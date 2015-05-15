C	
C      DATA GENERAL    VERSION:   
C                          Ismael Perez Fournon, Feb. 87
C                         
C       Fourier coeficients:
C              Jose Ignacio Gonzalez Serrano, Dic. (?) 87
C
C       VAX (ASPIC)    version: IPF 20 Feb. 88  
C
C	GRONINGEN VERSION MIDAS    MAYO 1989 IGS
C
c	1990: Works with real*4 data 
c	    : subroutine PROF4_SUB
c		
C    Fits ellipses to images in pixel-data to find their radial-
C      profiles, twists, centre-shifts and ellipticity gradients.
c   


	Subroutine PROF4 (pix,nx,ny,naxis,bback,majfact,xx0,yy0,
     &				eell,data14,ncont,keyw,ang)



      integer*2  Bl, Inc
      Integer*2 Maxh,Framein,Idel,Frame,Dummy,Igal,Nmax
	real*4 pix(nx*ny)
c	INTEGER*2 pix(262144)
C	INTEGER*2 pix(4194304)
      Integer*2 Hmax,Out(512),Nlines,Radno,Minit,Maxit,Val,Hmin
      Integer*2 Sm,Line,Num(1000)     ,Nann,Nsamp
      Integer*2 Max2,Nframes,Mmin,Min,Max,Mmax
      Integer*4 Line4,Ipos(9000),Fcol,Lcol,Ix,Ntot,Nlines4,Nmax4
      Integer*4 Maxv,Fline,Lline,Hist(-1000:32767),Posn(9000),Index
      Real*4 Cycle(1000),Itlo,Ithi,Incfact,Trsh,Xpos(9000),Ypos(9000)
      Real*4 Minor,PARM(30),Maj,Maj1,Maj2,Majfact,Data(18),Delete(6)
      Real*4 X2,Y2,Majcos(9),Majsin(9),Mincos(9),Minsin(9)
      Real*4 KeepPow,Int,Sin2,Cos2,AA(5,5),BB(5),XX(5,1000)
      Real*4 AA1(9,9),BB1(9),XX1(9,1000)
      Real*4 Back,Qual,Ilow,Mode,Width,Peak
      Real*4 Lastint,Lastth,Maxdpos,Maxdth,Maxdell,Lastsin2,Lastcos2
      Real*4 Mindpos,Mindth,Mindell
      Character F*1,Filnam*30, Bell*1
      Logical*1 Diag,Demo,Midway,File,Args,Galfile,Delfile,Batch,Found
      Logical*1 Bfile,Ellerror,Inorder,Bytes,Poshalf,Thhalf,Ellhalf
      Equivalence (Ipos,Xpos),(Sin2,BB(4)),(Cos2,BB(5))
      Equivalence (Back,PARM(9)),(Data(15),X0)
      Equivalence (Data(16),Y0),(Data(13),Ell)
      Equivalence (BB(1),Int)

	CHARACTER*4  KEYW
	CHARACTER*30 PROFLOG

	integer*4 axis,NAXIS,PIN
        DIMENSION axis(2)
	INTEGER NELM


	NELM=NX*NY
	axis(1) = nx
	axis(2) = ny
        back = bback
        x0 = xx0
        y0 = yy0
        ell = eell
        data(14) = data14
      Bl = 7
      Pi=3.14159
      Pi2=Pi/180.0
      File=.FALSE.
      Do 10 I=1,1000
        XX(1,I)=1.0
	XX1(1,i)=1.0
10    CONTINUE
      Midway=.FALSE.

c	Make filename:

         CALL mklogname(KEYW,proflog)
	 open (31,file= proflog,STATUS='UNKNOWN')

C  Set up mode of use of program
      Inorder=.FALSE.
      Args=.FALSE.
      Demo=.FALSE.
      Diag=.FALSE.
	ishow=5
	f ='1'

C  Open pixel-data channel
C

	NMAX = AXIS (1)
	NLINES = AXIS (2)


      Nmax4=Nmax
      Nlines4=Nlines

        Hmin=-1000
        Hmax=32767
      Factx=1.0/Float(Nmax-1)
      Facty=1.0/Float(Nlines-1)
      Dfact=Amin1(Factx,Facty)
      Ntot=Nmax*Nlines4
      If(Ntot.GT.4194304) Stop 'STOP....Frame too large for program'

c*	set up program parameters

 70   CONTINUE

      Maxdell=0.02                   ! maximum change in ellipticity
      Maxdth=2.0                     ! maximum position-angle shift
      Maxdpos=0.5                    ! maximum x-y shift
	iadit=2 
	minit=10                     ! minimum and
	maxit=70                     ! maximum number of iterations
  	delfact=0.5                  ! minimum fraction of ellipse inside
                                     ! good part of frame
      Itlo=Float(Minit)
      Ithi=Float(Maxit)


   99 Sampfact=(Majfact-1.0)/Majfact
      Mindpos=Maxdpos/400.0
      Maxdth=Maxdth*Pi2
      Mindth=Maxdth/1000.0
      Mindell=Maxdell/1000.0

c	Start program:
c
	maj=2 		    ! start with semimajor axis = 2 pix.

        Data(4)=Maj
      Ang=Data(14)*Pi2

C  Start next radius
C

	RADNO = 0


  400 Radno=Radno+1

      PARM(4)=Maj

	IF(IADIT.EQ.1) THEN
	  NANN=IFIX(MAJFACT)	
	ELSE
      Nann=Ifix(Maj*Sampfact)

	ENDIF

      If(Nann.LT.1) Nann=1
      If(Nann.GT.9) Nann=9
      Nsamp=Nint(2.0*Pi*Maj)
      If(Nsamp.GT.1000) Nsamp=1000
C     Restrsh=Resfact/Sqrt(Float(Nsamp*Nann))
      If(Nsamp.LT.40) Nsamp=40
c*      print *,Nann,' annuli,',Nsamp,' samples per annulus'
      Iter=0
      Pnttrsh=Delfact*Nann*Nsamp
      Maj1=Maj-0.5*(Nann+1)
      Fiinc=2.0*Pi/Nsamp
      Dpos=Maxdpos
      If(Dpos.GT.0.1*Maj) Dpos=0.1*Maj
      Poshalf=.FALSE.
      Dtheta=Maxdth
      Lastsin2=0.0
      Thhalf=.FALSE.
      Dell=Maxdell
      Lastcos2=0.0
      Ellhalf=.FALSE.
      Ellerror=.FALSE.
      Ermin=2000.0

  410 Iter=Iter+1
 	IF (ISHOW.EQ.1          ) THEN
	RITER = ITER
 	ENDIF

      E2=1.0-Ell
      Sinang=Sin(Ang)
      Cosang=Cos(Ang)
      Do 20 I=1,Nann
        Maj2=Maj1+I
        Minor=Maj2*E2
        Majcos(I)=Maj2*Cosang
        Majsin(I)=Maj2*Sinang
        Mincos(I)=Minor*Cosang
        Minsin(I)=Minor*Sinang
20    CONTINUE
C
C  Reset stores
C
      Do 30 I=1,5
        BB(I)=0.0
        Do 25 J=1,5
          AA(I,J)=0.0
25      CONTINUE
30    CONTINUE
      Do 40 I = Hmin, Hmax
        Hist(I)=0
40    CONTINUE
C
C  Load loop
      Index=0
      YY=0.0
      If(Nann.LE.2) THEN
        Do 50 I=1,Nsamp
          Fi=(I-1)*Fiinc
          XX(2,I)=Sin(Fi)
          XX(3,I)=Cos(Fi)
          Fi=2.0*Fi
          XX(4,I)=Sin(Fi)
          XX(5,I)=Cos(Fi)
          Do 430 J=1,Nann
            X=X0+Majcos(J)*XX(3,I)-Minsin(J)*XX(2,I)
            If(X.LT.1.0.OR.X.GT.Nmax) Go to 430
            Y=Y0+Majsin(J)*XX(3,I)+Mincos(J)*XX(2,I)
            If(Y.LT.1.0.OR.Y.GT.Nlines) Go to 430
            Index=Index+1
            Xpos(Index)=X
            Ypos(Index)=Y
            Posn(Index)=I
  430     CONTINUE
50      CONTINUE
        Call FSORTR2(Ypos,Xpos,Posn,Index)
	ICOUNTER = 0
        Do 69 I=1,Index
          Call INTERP(pix,Nmax,Nmax,Nlines,Xpos(I),Ypos(I),Value)

            Val=Nint(Value)
	IF (VAL.GE.HMAX ) PRINT *,'INTERPOLATED VALUE = ',VALUE
          If(Val.GE.Hmin.AND.VAL.LT.HMAX ) THEN
	ICOUNTER = ICOUNTER + 1
            YY=YY+Value*Value

	IF (ICOUNTER .EQ.1) THEN
		MIN= VAL
		MAX= VAL
	ELSE  IF (VAL.GT.MAX) THEN
		MAX = VAL
	ELSE IF (VAL.LT.MIN) then
		min = val
	ENDIF
            Hist(Val)=Hist(Val)+1
            Do 60 J=1,5
              BB(J)=BB(J)+XX(J,Posn(I))*Value
              Do 55 K=1,5
                AA(K,J)=AA(K,J)+XX(K,Posn(I))*XX(J,Posn(I))
55            CONTINUE
60          CONTINUE
          END IF
69      CONTINUE
      ELSE
        Do 80 I=1,Nsamp
          Fi=(I-1)*Fiinc
          XX(2,I)=Sin(Fi)
          XX(3,I)=Cos(Fi)
          Fi=2.0*Fi
          XX(4,I)=Sin(Fi)
          XX(5,I)=Cos(Fi)
          Do 450 J=1,Nann
            Ix=Nint(X0+Majcos(J)*XX(3,I)-Minsin(J)*XX(2,I))
            If(Ix.LT.1.OR.Ix.GT.Nmax) Go to 450
            Iy=Nint(Y0+Majsin(J)*XX(3,I)+Mincos(J)*XX(2,I))
            If(Iy.LT.1.OR.Iy.GT.Nlines) Go to 450
            Index=Index+1
            Ipos(Index)=(Iy-1)*Nmax4+Ix
            Posn(Index)=I
  450     CONTINUE
80      CONTINUE
        Call FSORTI1(Ipos,Posn,Index)
	MIN = 32767
	MAX = -32767
        Do 90 I=1,Index
          Val=pix(Ipos(I))


	IF (VAL.GE.HMAX ) PRINT *,'INTERPOLATED VALUE = ',VAL
          If(Val.GE.Hmin.AND.VAL.LT.HMAX) THEN
	IF (VAL.GT.MAX) THEN
		MAX = VAL
	ELSE
	IF (VAL.LT.MIN) MIN= VAL
	ENDIF
            Value=Float(Val)
            If(Demo) THEN
              Cycle(Posn(I))=Cycle(Posn(I))+Value
              Num(Posn(I))=Num(Posn(I))+1
            END IF
            YY=YY+Value*Value
            Hist(Val)=Hist(Val)+1

            Do 82 J=1,5
              BB(J)=BB(J)+XX(J,Posn(I))*Value
              Do 85 K=1,5
                AA(K,J)=AA(K,J)+XX(K,Posn(I))*XX(J,Posn(I))
85            CONTINUE
82          CONTINUE
          END IF
90      CONTINUE
      END IF
C
C  Analyse loop
C
      Points=AA(1,1)
      If(Points.EQ.0.0) Go to 10011
      If(Iter.GE.Minit.AND.Points.LT.Pnttrsh) Go to 10012
      Dev=Sqrt(Abs(YY/Points-(Int/Points)**2))
      Call MATINV(AA,5,5,BB,1,Det)
      Pow1=Sqrt(BB(2)**2+BB(3)**2)
      KeepPow=Pow1
      BB(2)=BB(2)*E2
      IF(ABS(BB(3)). LT. 1.e-8) PRINT*, 'maj, iter, BB = ', maj,iter,BB
      Theta=Ang+Atan2(BB(2),BB(3))
      Acos2=Abs(Cos2)
      Asin2=Abs(Sin2)
      If(Iter.EQ.1) Lastth=Theta
      If(Maxdpos.EQ.0.0) Pow1=0.0
      If(Maxdell.EQ.0.0) Acos2=0.0
      If(Maxdth.EQ.0.0) Asin2=0.0
C     Error=Amax1(Pow1,Acos2,Asin2)
      Error=Amax1(Pow1,Acos2,Asin2)
      IF (ISHOW.EQ.1) print *,'        Residuals are',KEEPPOW ,Cos2,Sin2

C  Retain best parameters so far

	IF (ISHOW.EQ.1) PRINT *,' error = ',error,' min  error = ',ermin

      If(Error.LT.Ermin) THEN
        Ermin=Error
        PARM(1)=Int
        PARM(2)=Dev
        If(Radno.EQ.1) THEN
          PARM(3)=0.0
        ELSE
	 if(iadit.eq.1) then
          PARM(3)=(PARM(1)-Lastint)
	  ELSE
          PARM(3)=(PARM(1)-Lastint)*Majfact/(Majfact-1.0)/Maj
	  ENDIF
        END IF
        Sm=-1
	IF (ISHOW.EQ.1) THEN
 	PRINT *, ' MINIMUM VALUE OF ELLIPSE  = ',MIN
	 PRINT *, ' MAXIMUM VALUE OF ELLIPSE  = ',MAX
	PRINT*,'HMIN=  ',HMIN
	PRINT*,'HMAX=  ',HMAX
	PRINT*,'POINTS=, ',POINTS
	ENDIF
        Call HISTPEAK(Hist(Min), Min, max,Sm,Mode,Width,Peak,Mmin,Mmax,
     &    Bk, 0.5, *10007)
  460   PARM(5)=Mode
        PARM(6)=Width
        PARM(8)=Points
        PARM(10)=KeepPow
        PARM(11)=Cos2
        PARM(12)=Sin2
        PARM(13)=Ell
        PARM(14)=Atan(Tan(Ang))
        PARM(15)=X0
        PARM(16)=Y0
      END IF
      If(Diag) THEN
        Diagout=Diagout+1
      END IF

C  Stop iterations if sufficiently good fit or if too many iterations
      If(Ermin.LE.0.001) print *,'ermin  .le.  0.001) '
      If(Iter.GE.Maxit) Go to 500

C  Correct worst parameter
      If(Asin2.GT.Pow1) Go to 470
      If(Acos2.GT.Pow1) Go to 480

C  Correct x,y position
	IF (ISHOW.EQ.1) THEN
               print *,'	Correcting x,y position'
	ENDIF


      If(Poshalf.OR.Cos(Theta-Lastth).LT.0.0) THEN
        Poshalf=.TRUE.
        Dpos=Dpos/2.0
        If(Dpos.LT.Mindpos) THEN
          Dpos=Maxdpos/10.0
          Poshalf=.FALSE.
        END IF
      END IF
      X0=X0+Dpos*Cos(Theta)
      Y0=Y0+Dpos*Sin(Theta)
      Lastth=Theta
      Go to 410
  470 If(Acos2.GT.Asin2) Go to 480
C
C  Correct angle
	IF (ISHOW.EQ.1) THEN
               print *,'	Correcting position-angle'
	ENDIF

      If(Thhalf.OR.Abs(Sin2-Lastsin2).GT.Abs(Sin2+Lastsin2)) THEN
        Thhalf=.TRUE.
        Dtheta=Dtheta/2.0
        If(Dtheta.LT.Mindth) THEN
          Dtheta=Maxdth/10.0
          Thhalf=.FALSE.
        END IF
      END IF
      Ang=Ang+Sign(Dtheta,Sin2)
      Lastsin2=Sin2
      Go to 410
C
C  Correct ellipticty
  480 If(Ellhalf.OR.Abs(Cos2-Lastcos2).GT.Abs(Cos2+Lastcos2)) THEN
        Ellhalf=.TRUE.
        Dell=Dell/2.0
        If(Dell.LT.Mindell) THEN
          Dell=Maxdell/10.0
          Ellhalf=.FALSE.
        END IF
      END IF

	IF (ISHOW.EQ.1) THEN
               print *,'Correcting ellipticity'
	ENDIF
      Enew=Ell+Sign(Dell,Cos2)
      If(Enew.LT.0.0) THEN
        Ell=Ell/2.0
        Ang=Atan(Tan(Ang+Pi/2.0))
        LastCos2=0.0
        Ellhalf=.FALSE.
      ELSE
        Ell=Enew
        LastCos2=Cos2
      END IF
      If(Ell.GE.0.95) THEN
        If(Ellerror) Go to 499
        print *,'Data error at centre of image.....changing ellipticity'
        Ell=0.1
        Dell=0.1
        Ellhalf=.FALSE.
        Ellerror=.TRUE.
      END IF
      Go to 410
C
C  End of iterations, output data
C
  499 print *,'Unable to fit ellipse due to data error at centre'
      Iter=-Iter
  500 Continue
C
C  Output parameters
C
      PARM(7)=Iter
      If(Iter.EQ.Maxit) PARM(7)=-PARM(7)
      Ang=PARM(14)
      PARM(14)=PARM(14)/Pi2

        If(.NOT.Batch) Write(*,99998) Igal,Iter,Maj,PARM(1),PARM(15),
     &    PARM(16),PARM(13),PARM(14)

        Linout=Linout+1

C	AQUI  CALCULA LOS COEFICIENTES A(I),B(I)
C	COEF FINALES
	ELL=PARM(13)
	X0=PARM(15)
	Y0=PARM(16)


      E2=1.0-Ell
      Sinang=Sin(Ang)
      Cosang=Cos(Ang)
      Do 95 I=1,Nann
        Maj2=Maj1+I
        Minor=Maj2*E2
        Majcos(I)=Maj2*Cosang
        Majsin(I)=Maj2*Sinang
        Mincos(I)=Minor*Cosang
        Minsin(I)=Minor*Sinang
95    CONTINUE
C
C  Reset stores
C
      Do 100 I=1,9
        BB1(I)=0.0
        Do 98 J=1,9
          AA1(I,J)=0.0
98      CONTINUE
100   CONTINUE
      Do 105 I = Hmin, Hmax
        Hist(I)=0
105   CONTINUE
C
C  Load loop
      Index=0
      YY=0.0
      If(Nann.LE.2) THEN
        Do 110 I=1,Nsamp
          Fi=(I-1)*Fiinc
          XX1(2,I)=Sin(Fi)
          XX1(3,I)=Cos(Fi)
          Fi=2.0*Fi
          XX1(4,I)=Sin(Fi)
          XX1(5,I)=Cos(Fi)
	  FI=3.0*(I-1)*FIINC
	  XX1(6,I)=SIN(FI)
	  XX1(7,I)=COS(FI)
	   FI=4.0*(I-1)*FIINC
	  XX1(8,I)=SIN(FI)
	  XX1(9,I)=COS(FI)
          Do 4309 J=1,Nann
            X=X0+Majcos(J)*XX1(3,I)-Minsin(J)*XX1(2,I)
            If(X.LT.1.0.OR.X.GT.Nmax) Go to 4309
            Y=Y0+Majsin(J)*XX1(3,I)+Mincos(J)*XX1(2,I)
            If(Y.LT.1.0.OR.Y.GT.Nlines) Go to 4309
            Index=Index+1
            Xpos(Index)=X
            Ypos(Index)=Y
            Posn(Index)=I
4309      CONTINUE
110     CONTINUE
        Call FSORTR2(Ypos,Xpos,Posn,Index)
	ICOUNTER = 0
        Do 120 I=1,Index
          Call INTERP(pix,Nmax,Nmax,Nlines,Xpos(I),Ypos(I),Value)

            Val=Nint(Value)
	IF (VAL.GE.HMAX ) PRINT *,'INTERPOLATED VALUE = ',VALUE
          If(Val.GE.Hmin.AND.VAL.LT.HMAX ) THEN
	ICOUNTER = ICOUNTER + 1
            YY=YY+Value*Value

	IF (ICOUNTER .EQ.1) THEN
		MIN= VAL
		MAX= VAL
	ELSE  IF (VAL.GT.MAX) THEN
		MAX = VAL
	ELSE IF (VAL.LT.MIN) then
		min = val
	ENDIF
            Hist(Val)=Hist(Val)+1
            Do 115 J=1,9
              BB1(J)=BB1(J)+XX1(J,Posn(I))*Value
              Do 112 K=1,9
                AA1(K,J)=AA1(K,J)+XX1(K,Posn(I))*XX1(J,Posn(I))
112           CONTINUE
115         CONTINUE
          END IF
120     CONTINUE
      ELSE
        Do 130 I=1,Nsamp
          Fi=(I-1)*Fiinc
          XX1(2,I)=Sin(Fi)
          XX1(3,I)=Cos(Fi)
          Fi=2.0*Fi
          XX1(4,I)=Sin(Fi)
          XX1(5,I)=Cos(Fi)
	  FI=3.0*(I-1)*FIINC
	  XX1(6,I)=SIN(FI)
	  XX1(7,I)=COS(FI)
	  FI=4.0*(I-1)*FIINC
	  XX1(8,I)=SIN(FI)
	  XX1(9,I)=COS(FI)
          Do 4509 J=1,Nann
            Ix=Nint(X0+Majcos(J)*XX1(3,I)-Minsin(J)*XX1(2,I))
            If(Ix.LT.1.OR.Ix.GT.Nmax) Go to 4509
            Iy=Nint(Y0+Majsin(J)*XX1(3,I)+Mincos(J)*XX1(2,I))
            If(Iy.LT.1.OR.Iy.GT.Nlines) Go to 4509
            Index=Index+1
            Ipos(Index)=(Iy-1)*Nmax4+Ix
            Posn(Index)=I
4509      CONTINUE
130     CONTINUE
        Call FSORTI1(Ipos,Posn,Index)
	MIN = 32767
	MAX = -32767
        Do 140 I=1,Index
          Val=pix(Ipos(I))


	IF (VAL.GE.HMAX ) PRINT *,'INTERPOLATED VALUE = ',VAL
          If(Val.GE.Hmin.AND.VAL.LT.HMAX) THEN
	IF (VAL.GT.MAX) THEN
		MAX = VAL
	ELSE
	IF (VAL.LT.MIN) MIN= VAL
	ENDIF
            Value=Float(Val)
            If(Demo) THEN
              Cycle(Posn(I))=Cycle(Posn(I))+Value
              Num(Posn(I))=Num(Posn(I))+1
            END IF
            YY=YY+Value*Value
            Hist(Val)=Hist(Val)+1

            Do 135 J=1,9
              BB1(J)=BB1(J)+XX1(J,Posn(I))*Value
              Do 132 K=1,9
                AA1(K,J)=AA1(K,J)+XX1(K,Posn(I))*XX1(J,Posn(I))
132           CONTINUE
135         CONTINUE
          END IF
140     CONTINUE
      END IF
C
C  Analyse loop
C
      Points=AA1(1,1)
      If(Points.EQ.0.0) Go to 10011
      If(Iter.GE.Minit.AND.Points.LT.Pnttrsh) Go to 10012
      Dev=Sqrt(Abs(YY/Points-(Int/Points)**2))
      Call MATINV(AA1,9,9,BB1,1,Det)
C	CALCULO ERRORES     (SALVO EL PRIMER CONTORNO)
	IF(PARM(3).NE.0.0) THEN

	ERRXC=-PARM(10)/PARM(3)
	ERRYC=-PARM(10)*E2/PARM(3)
	ERRAN=-2.*PARM(11)*E2/MAJ/PARM(3)/(E2*E2-1.)
        ERREL= 2.*PARM(12)*E2/MAJ/PARM(3)
	ELSE
	ERRXC=0.0
	ERRYC=0.0
	ERRAN=0.0
	ERREL=0.0
	ENDIF
	DO 150 LOCO=1,9
	BB1(LOCO)=BB1(LOCO)/PARM(1)
150	CONTINUE

	
	PARM(18)=BB1(1)     !  ME CARGO EL PARM(18) ANTERIOR.ERA 0
	PARM(19)=BB1(2)
	PARM(20)=BB1(3)
	PARM(21)=BB1(4)
	PARM(22)=BB1(5)
	PARM(23)=BB1(6)
	PARM(24)=BB1(7)
	PARM(25)=BB1(8)
	PARM(26)=BB1(9)                   ! EL FAMOSO B4
	PARM(27)=ERRXC
	PARM(28)=ERRYC
	PARM(29)=ERRAN
	PARM(30)=ERREL

c	Write parameters to file

	WRITE(31,*) PARM(4), PARM(26)

C
C  Finish analysis if profile reached sky
	if (PARM(5).lt.back    ) go to 10014
C  Start next radius

	if (radno.eq.ncont  ) return

c*
c*
c*     Ahora llamamos a alguna subrutina que nos dara un numerito
c*     para los diversos parametros.
c*     
c*     Call SALIDA (PARM(), file?, numeritos?,......)

      X0=PARM(15)
      Y0=PARM(16)
      Ell=PARM(13)
      Lastint=PARM(1)
      If(PARM(1).LT.Ilow) Ilow=PARM(1)
	IF(IADIT.EQ.1) MAJ=MAJ+MAJFACT
      	IF(IADIT.EQ.2) Maj=Maj*Majfact
      If(Demo) THEN
        print *,'print 0 for next galaxy'
        read (*,99999)        F
        If(F.EQ.'0') Go to 600
      END IF
      If(Maj.GT.1.0.OR.Majfact.GT.1.0) Go to 400
C
C  End of galaxy
C

  600 RETURN

10001 print *,'STOP....End of galaxies'
      Go to 10101
10002 print *,'STOP....End of data'
      Go to 10101
10003 print *,'Unable to read first line of old output-file'
      Igal=1
      Go to 70
10004 print *,'STOP....In BATCH-mode image-parameters must be from file'
      Go to 10101
10005 print *,'STOP....In BATCH-mode output must be to file'
      Go to 10101
10006 print *,'STOP....Bad data-frame, no data in histogram'
      Go to 10101
10007 If (.NOT. Batch .AND. Iter .EQ.1) print *, Bell, 'WARNING....Bad',
     &  ' analysis of modal intensity'
      Mode = Bk
      Width = 0.0
      Go to 460
10011 print *,'EOG....No good points at all'
      Go to 600
10012 print *,'EOG....Too few good points (after sufficient iterations)'
      Go to 600
10013 print *,'EOG....Profile started to rise by too much'
      Go to 600
10014 print *,'EOG....Profile sufficiently close to sky'
      Go to 600
10015 print *,'WARNING....Unable to open file  FILEIN: .GAL'
10016 print *,'File-name for images to be analysed? (0=enter manually)'
      read (*,99999)        Filnam
      If(Filnam.EQ.'0'.OR.Filnam.EQ.' ') THEN
        If(Batch) Go to 10004
        print *,'You will be asked to enter approx x,y positions,'
        print *,'  ellipticities and position-angles of images to be'
        print *,'  analysed. All these are non-critical and will be'
        print *,'  corrected by the program.'
      ELSE
        Galfile=.TRUE.
        If(Batch) print *,'GALAXY-FILE IS GASPIN: ',Filnam
      END IF

10017 print *,'WARNING....Unable to open file  FILEIN: .DEL'
10018 print *,'File-name for images to be deleted? (0=none)'
      read (*,99999)        Filnam
      If(Filnam.EQ.'0'.OR.Filnam.EQ.' ') THEN
      ELSE
        Delfile=.TRUE.
        If(Batch) print *,'STAR-FILE IS GASPIN: ',Filnam
      END IF

10019 print *,'WARNING....Unable to read backgrounds from file'
      print *,'print 1 to continue (backgrounds will be calculated)'
      read (*,99999)        F

      Go to 10102

10021 print *,Bell,'STOP....Unable to open a new output-file'
      Go to 10101
10101 If(File) THEN
        print *,'Output profiles are in file  FILEOUT: .PRF'
        print *,'Use PLOTPROF or PROFREAD to access data'
      END IF
10102 continue
      CLOSE(31)
      Stop
99999 Format(A)
99998 Format(1x,I4,' After',I4,' iterations:',f7.2,2f8.2,f7.2,f7.3,f9.2)
99997 Format(' Starting image',I4,', quality',f4.1)

      END
