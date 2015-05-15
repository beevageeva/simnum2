


C******************************************************************************
C
C
	    SUBROUTINE outFITS(array, nx, ny)
C
C
C******************************************************************************


	IMPLICIT NONE
	INTEGER nx, ny, lun, bitpix, err
	REAL array(nx, ny), bscale, bzero
	CHARACTER fitsfile*40, object*20, comment*60
	LOGICAL append
	PARAMETER(bitpix=16)

C-----------
C  Set default parameters
C------------
	bscale = 0.
	bzero  = 0.
	lun    = 0
	append = .FALSE.

C------------
C  Make FITS file name, self-numbering
C------------
	CALL mkfitsname('map', fitsfile)

C-------------
C  Comment field
C-------------
 10	WRITE(6,'(A)') ' outFITS>>  object name :'
	READ(5,'(A)',ERR=10) object
 20	WRITE(6,'(A)') ' outFITS>>  comment field :'
	READ(5,'(A)',ERR=20) comment

CC--------------
CC  FITS-WRITE routine
CC--------------
C	CALL fits_write
C     &		(fitsfile, append, lun, array, nx, ny,
C     &		object, comment, bitpix, bscale, bzero, err)

C---------------
C  Error conditions
C---------------
	IF (err .GT. 0) 
     &     WRITE(6,100) err
 100	FORMAT(1x, 'outFITS>> error', I5, ' in FITS_WRITE')

	RETURN
	END
