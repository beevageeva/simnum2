C
C ---	-------------------------------------------------------------------
C	SFITS contains two simple FITS-routines to read and write
C ---	-------------------------------------------------------------------
C	Marco de Vos, Kapteyn Lab, March 1990 (DEVOSCM@RUG.NL)
C ---	-------------------------------------------------------------------
C
C ***	*******************************************************************
C	Three sections in this file are placed between starred lines.
C	These sections should be identical, and could be put in an 
C	include	file. Beware of this when making changes!
C ***	*******************************************************************
C
C ---	-------------------------------------------------------------------
C	FITS_WRITE(File,Append,Lun,Array,DX,DY,
C	           Object,Comment,Bitpix,Bscale,Bzero,Err)
C
C	FITS_WRITE writes or appends one plane to a Disk-file
C
C	CHARACTER File*(*)     The full filename to write the FITS-data in
C	LOGICAL   Append       If true, the data is appended to the file,
C	                       if false, an existing file will be overwritten
C	INTEGER   Lun          The logical unit on which the file is to be
C	                       opened, if Lun.LE.0 the routine finds one.
C
C	REAL      Array(*)     The array (DX*DY) to write in the file
C	INTEGER   DX,DY        The number of pixels in both directions
C
C	CHARACTER Object*(*)   A text-string, written as the object name
C	CHARACTER Comment*(*)  A text-string, written as a comment
C
C	INTEGER   Bitpix       The number of bits per pixel in the file
C	REAL      Bscale       The scaling to be applied to the data,
C	                       if Bscale.LE.0., autoscaling is applied
C	REAL      Bzero        The offset, ARRAY=PIXEL*Bscale+Bzero
C
C	INTEGER   Err          An error-code, or the number of undefineds
C ---	-------------------------------------------------------------------
C
C ---	-------------------------------------------------------------------
C	FITS_READ(File,Lun,Plane,Array,NN,DX,DY,Object,Comment,FBlank,Err)
C
C	FITS_READ reads one plane out of a Disk-file
C
C	CHARACTER File*(*)     The full filename to read the FITS-data from
C	INTEGER   Lun          The logical unit on which the file is to be
C	                       opened, if Lun.LE.0 the routine finds one.
C	INTEGER   Plane        The 2D plane to be read from the file
C
C	REAL      Array(*)     The array (NN) to read data in
C	INTEGER   NN           The number of elements in the Array
C	INTEGER   DX,DY        The number of pixels according to the file
C
C	CHARACTER Object*(*)   The object accoriding to the header
C	CHARACTER Comment*(*)  A comment, read from the header
C
C	REAL      FBlank       The value to be given to undefined pixels
C	INTEGER   Err          An error-code, or the number of undefineds
C ---	-------------------------------------------------------------------
C
C ---	-------------------------------------------------------------------
C	FITS_SCALE(Array,NN,DMIN,DMAX,Scales,Zeros,Errors)
C
C	FITS_SCALE determines the scaling for a data-array 
C
C	REAL     Array(*)    The array containing the number to be scaled
C	INTEGER  NN          The number of elements in the array
C	REAL     DMIN,DMAX   The range of values in the Array
C	REAL     Scales(3)   The scaling computed for BITPIX=8,16,32
C	REAL     Zeros(3)    The offset computed for BITPIX=8,16,32
C	REAL     Errors(3)   The deviations computed  for BITPIX=8,16,32:
C	                      MAX(ABS(Array(I)-Unscale(Scale(Array(I))))) 
C ---	-------------------------------------------------------------------
C
C	Error-codes:
C
C	    Err<= 0    ABS(Err) is the number of blanks read or written
C	    
C	    Err = 1    Could not find a free Logical unit
C	    Err = 2    Could not open file
C	    Err = 3    File does not exist
C	    Err = 4    Error accessing file
C	    Err = 5    Inconsisten X-axis in append
C	    Err = 6    Inconsisten Y-axis in append
C	    Err = 7    Plane not present in read
C
C ---	-------------------------------------------------------------------
C
	SUBROUTINE FITS_WRITE(File,Append,Lun,Array,DX,DY,Object,
     &	                      Comment,Bitpix,Bscale,Bzero,Err)
C
	implicit none
C
	CHARACTER File*(*)
	LOGICAL   Append
	INTEGER   Lun
	REAL      Array(*)
	INTEGER   DX,DY
	CHARACTER Object*(*)
	CHARACTER Comment*(*)
	INTEGER   Bitpix
	REAL      Bscale
	REAL      Bzero
	INTEGER   Err
C
C ---	Local copies of some arguments ---------------------------------
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
C ---	Formats for header items ---------------------------------------
C
C ---	----------------------------------------------------------------
C	Simple FITS-Header record according to definition:
C ---	----------------------------------------------------------------
C	         1         2         3         4         5         6  8
C	123456789012345678901234567890123456789012345678901234567890..0
C
C	key_____= .....................................................
C
C	Key:       Max. 8 capitals, left justified with trailing spaces
C
C 101	End:       Keyword END, rest Blanks, indicates end of header
C
C 102	Comment-like record: no equal sign, col 10-80 is text string
C
C 103	Floating:  Decimal point must be present,
C	           If exponential format: right justified in col 11-30,
C	           if complex: imaginary part in col 31-50
C
C 104	Integer:   Right justified in col 11-30, if complex:
C	           imaginary part right justified in 31-50
C	
C 105	Logical:   T or F in column 30
C
C 106	Character: Minimal 8 characters of which at most 8 essential,
C	           embedded in quotes; open ' in col 11, closing ' not
C	           before col 20
C
C 107	Dates:     character data, 'dd/mm/yy'
C
C 108	Times:     character data, 'hh:mm:ss'
C
C	Comment:   separated from value by <space><slash>;
C	           here: if possible aligned in column 50.
C
C ---	---------------------------------------------------------------
C
 101	FORMAT('END',77X)
 102	FORMAT(A8,'  ',A)
 103	FORMAT(A8,'= ',1PE20.6,' / ',A)
 104	FORMAT(A8,'= ',I20,' / ',A)
 105	FORMAT(A8,'= ',19X,L1,' / ',A)
 106	FORMAT(A8,'= ''',A,''' / ',A)
 107	FORMAT(A8,'= ''',I2.2,'/',I2.2,'/',I2.2,''' /',A)
 108	FORMAT(A8,'= ''',I2.2,':',I2.2,':',I2.2,''' /',A)
C
C ---	Function definitions -------------------------------------
C
	INTEGER   FTS_LEN
C
C ---	Local variables, counters etc ----------------------------
C
	INTEGER   I,Blank,NBlank
	INTEGER*4 I1, I2, I3
	REAL      DMAX,DMIN,FVAL
	LOGICAL   Opened,Exists,End_found
	INTEGER   IRec,IOff,NDone,NReq,Ios,ICard
	CHARACTER ITEM*8,CARD*70
C
C ---	Find the proper scale-factors ----------------------------
C
	IF (Bscale.LE.0.) THEN
	    DMAX=ARRAY(1)
	    DMIN=ARRAY(1)
	    DO I=1,DX*DY
	       IF (ARRAY(I).GT.DMAX) DMAX=ARRAY(I)
	       IF (ARRAY(I).LT.DMIN) DMIN=ARRAY(I)
	    END DO
	    BS=(DMAX-DMIN)/FTSRANGE(Bitpix/8)
	    IF (BS.LT.1.E-35) BS=1.
	    BZ=DMIN - BS * (FTSMAX(Bitpix/8) + FTSMIN(Bitpix/8) 
     &                      - FTSRANGE(Bitpix/8)) / 2.0
	ELSE
	    BS=Bscale
	    BZ=Bzero
	END IF
C
C ---	Correct the scale-factors for rounding errors --------------
C
	WRITE(CARD,'(1PE20.6)') BS
	READ(CARD,*) BS
	WRITE(CARD,'(1PE20.6)') BZ
	READ(CARD,*) BZ
	Blank=FTSBLANK(Bitpix/8)
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
	IF (Append.AND.Exists) THEN	
	    OPEN(UNIT=ILun,FILE=File,access='direct',
     &	         status='old',recl=fitsrcl/nobytes,IOSTAT=Ios)
	ELSE
	    IF (Exists) THEN
	        OPEN(UNIT=ILun,FILE=FILE,STATUS='OLD')
	        CLOSE(ILun,STATUS='DELETE')
	    END IF
	    OPEN(UNIT=ILun,FILE=File,access='direct',
     &	         status='new',recl=fitsrcl/nobytes,IOSTAT=Ios)
	END IF
	IF (Ios.NE.0) THEN
	    CALL ERROR(6,'FITS_WRITE: Cannot open file '//FILE)
	    Err=2
	    GOTO 99
	END IF
C
C ---	If append, read and update old header ------------------------------
C
	BTPix=Bitpix
	IF (Append.AND.Exists) THEN
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
	          ELSE IF (ITEM.EQ.'NAXIS1  ') THEN
	              READ(CBUF(ICard)(11:),*) I1
	              IF (I1.NE.DX) THEN
	                 CALL ERROR(6,'Inconsistent length of X-axis')
	                 Err=5
	                 GOTO 98
	              END IF
	          ELSE IF (ITEM.EQ.'NAXIS2  ') THEN
	              READ(CBUF(ICard)(11:),*) I2
	              IF (I2.NE.DY) THEN
	                 CALL ERROR(6,'Inconsistent length of Y-axis')
	                 Err=6
	                 GOTO 98
	              END IF
C
C	Update number of axes
C
	          ELSE IF (ITEM.EQ.'NAXIS3  ') THEN
	              READ(CBUF(ICard)(11:),*) I3
	              WRITE(CBUF(ICard),104) 'NAXIS3  ',I3+1,' '
	              WRITE(ILun,REC=IRec) BBUF
	          ELSE IF (ITEM.EQ.'BSCALE  ') THEN
	              READ(CBUF(ICard)(11:),*) BS
	          ELSE IF (ITEM.EQ.'BZERO   ') THEN
	              READ(CBUF(ICard)(11:),*) BZ
	          ELSE IF (ITEM.EQ.'BLANK   ') THEN
	              READ(CBUF(ICard)(11:),*) Blank
	          END IF
	          ICard=ICard+1
	       END DO
	       IRec=IRec+1
	    END DO
C
C	If possible, add comment to header
C
	    IF (ICard.LT.FITSCARDS) THEN
	       CALL FTS_COPY(CARD,Comment)
	       WRITE(CBUF(13),102) 'COMMENT ',
     &	             CARD(1:MIN(69,FTS_LEN(CARD)))
	       WRITE(CBUF(ICard),101)
	       WRITE(ILun,REC=IRec-1) BBUF
	    END IF
C
C	Compute number of bytes filled in last record written
C
	    IRec=IRec+(BTPix/8)*I1*I2*I3/FITSRCL
	    IOff=MOD((BTPix/8)*I1*I2*I3,FITSRCL)/(BTPix/8)+1
C
C	If necessary, read last record
C
	    IF (IOff.GT.1) READ(ILun,REC=IRec) BBUF
C
C ---	If new file, create a header record ----------------------------
C
	ELSE
	   BTPix=Bitpix
	   DO ICard=1,FITSCARDS
	      CBUF(ICard)=' '
	   END DO
CCC	   CALL IDATE(I1,I2,I3)
	   CALL TIME(ITEM)
	   WRITE(CBUF( 1),105) 'SIMPLE  ',.TRUE.,'Simple Fits Format'
	   WRITE(CBUF( 2),104) 'BITPIX  ',BTPix, 'Bits per pixel'
	   WRITE(CBUF( 3),104) 'NAXIS   ',3,     'Series of 2D planes'
	   WRITE(CBUF( 4),104) 'NAXIS1  ',DX,    'Pixels on X-axis'
	   WRITE(CBUF( 5),104) 'NAXIS2  ',DY,    'Pixels on Y-axis'
	   WRITE(CBUF( 6),104) 'NAXIS3  ',1,     'Number of planes'
	   WRITE(CBUF( 7),103) 'BSCALE  ',BS,'Real = Pixel*BSCALE+BZERO'
	   WRITE(CBUF( 8),103) 'BZERO   ',BZ,' '
	   WRITE(CBUF( 9),104) 'BLANK   ',Blank,   'Undefined value'
	   WRITE(CBUF(10),107) 'DATE-OBS',I1,I2,I3,'/Date file written'
	   WRITE(CBUF(11),106) 'TIME-OBS',ITEM,    'Time file written'
	   CALL FTS_COPY(CARD,Object)
	   WRITE(CBUF(12),106) 'OBJECT  ',
     &	         CARD(1:MAX(8,MIN(32,FTS_LEN(CARD)))),'Object name'
	   CALL FTS_COPY(CARD,Comment)
	   WRITE(CBUF(13),102) 'COMMENT ',CARD(1:MIN(69,FTS_LEN(CARD)))
	   WRITE(CBUF(14),101)
	   WRITE(ILun,REC=1) BBUF
C
	   IRec=2
	   IOff=1
C
	END IF
C
C ---	Now write the data to the file ---------------------------------
C
	NBlank=0
	NDone=1
	NReq=DX*DY
	DMAX=FTSMAX(BTPix/8)
	DMIN=FTSMIN(BTPix/8)
C
	IF (BTPix.EQ.8) THEN
	   DO WHILE (NDone.LE.NReq)
	      FVAL=(ARRAY(NDone)-BZ)/BS
	      IF (FVAL.GT.DMAX.OR.FVAL.LT.DMIN) THEN
	         BBUF(IOff)=Blank
	         NBlank=NBlank+1
	      ELSE IF (FVAL.LE.127.) THEN
	         BBUF(IOff)=INT(FVAL)
	      ELSE
	         BBUF(IOff)=INT(FVAL-256.)
	      END IF
	      NDone=NDone+1
	      IOff=IOff+1
	      IF (IOff.GT.FITSRCL) THEN
	         WRITE(ILun,REC=IRec) BBUF
	         IRec=IRec+1
	         IOff=1
	      END IF
	   END DO
	   IF (IOff.NE.1) WRITE(ILun,REC=IRec) BBUF
	ELSE IF (BTPix.EQ.16) THEN
	   DO WHILE (NDone.LE.NReq)
	      FVAL=(ARRAY(NDone)-BZ)/BS
	      IF (FVAL.GT.DMAX.OR.FVAL.LT.DMIN) THEN
	         I2BUF(IOff)=Blank
	         NBlank=NBlank+1
	      ELSE
	         I2BUF(IOff)=INT(FVAL)
	      END IF
	      NDone=NDone+1
	      IOff=IOff+1
	      IF (IOff.GT.FITSRCL/2) THEN
	         CALL FTS_SWAP(BBUF,FITSRCL,2)
	         WRITE(ILun,REC=IRec) BBUF
	         IRec=IRec+1
	         IOff=1
	      END IF
	   END DO
	   IF (IOff.NE.1) THEN
	      CALL FTS_SWAP(BBUF,FITSRCL,2)
	      WRITE(ILun,REC=IRec) BBUF
	   END IF
	ELSE IF (BTPix.EQ.32) THEN
	   DO WHILE (NDone.LE.NReq)
	      FVAL=(ARRAY(NDone)-BZ)/BS
	      IF (FVAL.GT.DMAX.OR.FVAL.LT.DMIN) THEN
	         I4BUF(IOff)=Blank
	         NBlank=NBlank+1
	      ELSE
	         I4BUF(IOff)=INT(FVAL)
	      END IF
*	if (ndone.le.20) write(16,*) NDone,Ioff,I4BUF(IOff),Array(NDone)
	      NDone=NDone+1
	      IOff=IOff+1
	      IF (IOff.GT.FITSRCL/4) THEN
	         CALL FTS_SWAP(BBUF,FITSRCL,4)
	         WRITE(ILun,REC=IRec) BBUF
	         IRec=IRec+1
	         IOff=1
	      END IF
	   END DO
	   IF (IOff.NE.1) THEN
	      CALL FTS_SWAP(BBUF,FITSRCL,4)
	      WRITE(ILun,REC=IRec) BBUF
	   END IF
	END IF
C
	Err=NBlank
C
   98	CLOSE(ILun)
C
   99	RETURN
	END
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
C
C ---	-----------------------------------------------------------------
C
	SUBROUTINE FITS_SCALE(ARRAY,NN,DMIN,DMAX,SCALES,ZEROS,ERRORS)
C
	REAL      ARRAY(*)
	INTEGER   NN
	REAL      DMIN,DMAX
	REAL      Scales(3),Zeros(3),Errors(3)
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
C ---	Local variables, counters etc ----------------------------
C
	INTEGER   I,J,IVAL,BTPIX(3)
	DATA      BTPIX/1,2,4/
	REAL      RANGE,FVAL,VAL
C
C ---	Find extrema in array ------------------------------------
C
	DMIN=ARRAY(1)
	DMAX=ARRAY(1)
	DO I=1,NN
	   IF (ARRAY(I).GT.DMAX) DMAX=ARRAY(I)
	   IF (ARRAY(I).LT.DMIN) DMIN=ARRAY(I)
	END DO
C
C ---	Find the three scales and offsets ------------------------
C
	RANGE=DMAX-DMIN
	DO J=1,3
	   I=BTPIX(J)
	   SCALES(J)=RANGE/FTSRANGE(I)
	   IF (SCALES(J).LT.1.E-35) SCALES(J)=1.
	   ZEROS(J)=DMIN-SCALES(J) * (FTSMAX(I) + FTSMIN(I) 
     &                      - FTSRANGE(I)) / 2.0

	END DO
C
C ---	Determine the error bounds ---------------------------------
C
	DO J=1,3
	  ERRORS(J)=0.
	END DO
C
	DO I=1,NN
	   VAL=ARRAY(I)
	   DO J=1,3
	      FVAL=(VAL-ZEROS(J))/SCALES(J)
	      IF (FVAL.GE.FTSMIN(BTPIX(J)).AND.
     &	          FVAL.LE.FTSMAX(BTPIX(J))) THEN
	          IVAL=INT(FVAL)
	          FVAL=(FLOAT(IVAL)*SCALES(J))+ZEROS(J)
	          ERRORS(J)=MAX(ERRORS(J),ABS(VAL-FVAL))
	      END IF
	   END DO
	END DO
C
	RETURN
	END
C
C ---	------------------------------------------------------------------
C	FTS_SWAP, FTS_COPY, and FTS_LEN are supporting routines
C ---	------------------------------------------------------------------
C
C ---	FTS_SWAP  Swaps bytes in integers --------------------------------
C
	SUBROUTINE FTS_SWAP(BUF,NBYTES,SWAP)
C
	LOGICAL*1 BUF(*),S1,S2,S3,S4
	INTEGER   NBYTES,NSWAP,I
C
	if (nswap.eq.4) then
	  do 100 i=1,nbytes,4
	    s1=buf(i)
	    s2=buf(i+1)
	    s3=buf(i+2)
	    s4=buf(i+3)
	    buf(i+3)=s1
	    buf(i+2)=s2
	    buf(i+1)=s3
	    buf(i)  =s4
  100	  continue
	end if
C
	if (nswap.eq.2) then
	  do 200 i=1,nbytes,2
	    s1=buf(i)
	    s2=buf(i+1)
	    buf(i+1)=s1
	    buf(i)  =s2
  200	  continue
	end if
C
	RETURN
	END
C
C ---	FTS_COPY  Copies and rubs a string that may be quoted --------------
C
	SUBROUTINE FTS_COPY(NEW,OLD)
C
	CHARACTER NEW*(*),OLD*(*)
	INTEGER   L1,L2,I1,I2,FTS_LEN
C
	L1=FTS_LEN(OLD)
	L2=LEN(NEW)
	NEW=' '
C
C	Remove trailing spaces
C
	I1=1
	DO WHILE (OLD(I1:I1).EQ.' '.AND.I1.LT.L1)
	   I1=I1+1
	END DO
C
C	Skip any opeing quote
C
	IF (OLD(I1:I1).EQ.'''') I1=I1+1
	I2=1
	DO WHILE (OLD(I1:I1).NE.''''.AND.I1.LE.L1.AND.I2.LE.L2)
	   IF (ICHAR(OLD(I1:I1)).GE.ICHAR(' ').AND.
     &	       ICHAR(OLD(I1:I1)).LE.ICHAR('~')) NEW(I2:I2)=OLD(I1:I1)
	   I1=I1+1
	   I2=I2+1
	END DO
C
	RETURN
	END
C
C ---	FTS_LEN returns the effective length of a string ------------
C
	INTEGER FUNCTION FTS_LEN(STR)
C
	CHARACTER STR*(*)
	INTEGER   I
C
	I=LEN(STR)
	DO WHILE ((STR(I:I).EQ.' '.OR.STR(I:I).EQ.CHAR(0)).AND.I.GT.1)
	      I=I-1
	END DO
	FTS_LEN=I
	RETURN
	END
