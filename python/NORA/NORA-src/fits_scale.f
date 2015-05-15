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
