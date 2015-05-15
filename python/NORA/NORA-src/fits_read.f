C
C ---	-------------------------------------------------------------------
C
	SUBROUTINE FITS_READ(File,Lun,Plane,Array,NN,DX,DY,Object,
     &	                     Comment,FBlank,Err)
C
	implicit none
C
	CHARACTER File*(*)
	INTEGER   Lun
	INTEGER   Plane
	REAL      Array(*)
	INTEGER   NN
	INTEGER   DX,DY
	CHARACTER Object*(*)
	CHARACTER Comment*(*)
	REAL      FBlank
	INTEGER   Err
C
C ---	Local copies of some arguments and header items ----------------
C
	INTEGER   ILun,BTPix
	REAL      BS,BZ
C
C ***	FITS Definitions and buffers *************************************
C
	INTEGER   FITSRCL, nobytes
	INTEGER   FITSCARDS
	PARAMETER(FITSRCL=2880,FITSCARDS=FITSRCL/80, nobytes=1)
C  		nobytes = 1	UNIX
C		nobytes = 4	VMS
C
	LOGICAL*1 BBUF(FITSRCL)
	INTEGER*2 I2BUF(FITSRCL/2)
	INTEGER*4 I4BUF(FITSRCL/4)
	CHARACTER CBUF(FITSCARDS)*80
	EQUIVALENCE(BBUF(1),CBUF(1),I2BUF(1),I4BUF(1))
C
	REAL      FTSMAX(4),FTSMIN(4),FTSRANGE(4)
	DATA      FTSMAX  /255., 32765.,0., 2140000000./
	DATA      FTSMIN  /  0.,-32765.,0.,-2140000000./
	DATA      FTSRANGE/253., 65000.,0., 4000000000./
C
	INTEGER   FTSBLANK(4)
	DATA      FTSBLANK/  0, -32768, 0, -2147483648/
C
	COMMON /FTS_BUF/BBUF
C
C ***	******************************************************************
C
C ---	Local variables, counters etc. --------------------------------
C
	INTEGER   I,DZ,IVAL,Blank,NBlank
	INTEGER*4 I1, I2, I3
	LOGICAL   Opened,Exists,End_found
	INTEGER   IRec,IOff,NDone,NReq,Ios,ICard
	CHARACTER ITEM*8
C
C ---	Find a free logical unit -----------------------------------
C
	IF (Lun.EQ.0) THEN
	   ILun=0
	   Opened=.TRUE.
	   DO WHILE (ILun.LT.16.AND.Opened)
	      ILun=ILun+1
	      INQUIRE(ILun,OPENED=Opened)
	   END DO
	   IF (Opened) THEN
	      CALL ERROR(6,'FITS_WRITE: No free LUN')
	      Err=1
	      GOTO 99
	   END IF
	ELSE
	   ILun=Lun
	END IF
C
C ---	Open the file for writing/appending ------------------------
C
	INQUIRE(FILE=File,EXIST=Exists)
	IF (Exists) THEN
	    OPEN(UNIT=ILun,FILE=File,access='direct',
     &	         status='old',recl=fitsrcl/nobytes,IOSTAT=Ios)
	ELSE
	    CALL ERROR(6,'FITS_READ: File does not exist.')
	    Err=3
	    GOTO 99
	END IF
	IF (Ios.NE.0) THEN
	    CALL ERROR(6,'FITS_READ: Could not open file')
	    Err=2
	    GOTO 99
	END IF
C
C ---	Read the header --------------------------------------------
C
	I1=1
	IRec=1
	End_found=.false.
	DO WHILE (.NOT.End_found)
	   READ(ILun,REC=IRec) BBUF
	   ICard=1
	   DO WHILE (ICard.LE.FITSCARDS.AND..NOT.End_found)
	      ITEM=CBUF(ICard)(1:8)
	      IF (ITEM.EQ.'END        ') THEN
	          End_found=.TRUE.
	      ELSE IF (ITEM.EQ.'BITPIX  ') THEN
	          READ(CBUF(ICard)(11:),*) BTPix
	          Blank=FTSBLANK(BTPix/8)
	      ELSE IF (ITEM.EQ.'NAXIS1  ') THEN
	          READ(CBUF(ICard)(11:),*) DX
	      ELSE IF (ITEM.EQ.'NAXIS2  ') THEN
	          READ(CBUF(ICard)(11:),*) DY
	      ELSE IF (ITEM.EQ.'NAXIS3  ') THEN
	          READ(CBUF(ICard)(11:),*) DZ
	      ELSE IF (ITEM.EQ.'BSCALE  ') THEN
	          READ(CBUF(ICard)(11:),*) BS
	      ELSE IF (ITEM.EQ.'BZERO   ') THEN
	          READ(CBUF(ICard)(11:),*) BZ
	      ELSE IF (ITEM.EQ.'BLANK   ') THEN
	          READ(CBUF(ICard)(11:),*) Blank
	      ELSE IF (ITEM.EQ.'OBJECT  ') THEN
	          CALL FTS_COPY(Object,CBUF(ICARD)(11:))
	      ELSE IF (ITEM.EQ.'COMMENT   '.AND.I1.LE.Plane) THEN
	          CALL FTS_COPY(Comment,CBUF(ICARD)(11:))
	          I1=I1+1
	      END IF
	      ICard=ICard+1
	   END DO
	   IRec=IRec+1
	END DO
C
C ---	Find the first record to read -----------------------------------
C
	IF (Plane.GT.DZ) THEN
	    CALL ERROR(6,'FITS_READ: Plane not in file...')
	    Err=7
	    GOTO 98
	END IF
	IRec=IRec+(BTPix/8)*DX*DY*(Plane-1)/FITSRCL
	IOff=MOD((BTPix/8)*DX*DY*(Plane-1),FITSRCL)/(BTPix/8)+1
C
C ---	Now read the data from the file ---------------------------------
C
	NBlank=0
	NDone=1
	NReq=DX*DY
	READ(ILun,REC=IRec) BBUF
	IF (BTPix.GT.8) CALL FTS_SWAP(BBUF,FITSRCL,BTPix/8)
C
	IF (BTPix.EQ.8) THEN
	   DO WHILE (NDone.LE.NReq.AND.NDone.LE.NN)
	      IF (IOff.GT.FITSRCL) THEN
	         Ioff=1
	         IRec=IRec+1
	         READ(ILun,REC=IRec) BBUF
	      END IF
	      IVAL=BBUF(IOff)
	      IF (IVAL.EQ.Blank) THEN
	         ARRAY(NDone)=FBlank
	         NBlank=NBlank+1
	      ELSE IF (IVAL.GE.0) THEN
	         ARRAY(NDone)=FLOAT(IVAL)*BS+BZ
	      ELSE
	         ARRAY(NDone)=FLOAT(IVAL+256)*BS+BZ
	      END IF
	      NDone=NDone+1
	      Ioff=Ioff+1
	   END DO
	ELSE IF (BTPix.EQ.16) THEN
	   DO WHILE (NDone.LE.NReq.AND.NDone.LE.NN)
	      IF (IOff.GT.FITSRCL/2) THEN
	         Ioff=1
	         IRec=IRec+1
	         READ(ILun,REC=IRec) BBUF
	         CALL FTS_SWAP(BBUF,FITSRCL,2)
	      END IF
	      IVAL=I2BUF(Ioff)
	      IF (IVAL.EQ.Blank) THEN
	         ARRAY(NDone)=FBlank
	         NBlank=NBlank+1
	      ELSE
	         ARRAY(NDone)=FLOAT(IVAL)*BS+BZ
	      END IF
	      NDone=NDone+1
	      Ioff=Ioff+1
	   END DO
	ELSE IF (BTPix.EQ.32) THEN
	   DO WHILE (NDone.LE.NReq.AND.NDone.LE.NN)
	      IF (IOff.GT.FITSRCL/4) THEN
	         Ioff=1
	         IRec=IRec+1
	         READ(ILun,REC=IRec) BBUF
	         CALL FTS_SWAP(BBUF,FITSRCL,4)
	      END IF
	      IVAL=I4BUF(Ioff)
	      IF (IVAL.EQ.Blank) THEN
	         ARRAY(NDone)=FBlank
	         NBlank=NBlank+1
	      ELSE
	         ARRAY(NDone)=FLOAT(IVAL)*BS+BZ
	      END IF
*	if (ndone.le.20) write(16,*) NDone,Ioff,I4BUF(IOff),Array(NDone)
	      NDone=NDone+1
	      Ioff=Ioff+1
	   END DO
	END IF
C
	Err=NBlank
C
  98	CLOSE(ILun)
C
  99	RETURN
	END
