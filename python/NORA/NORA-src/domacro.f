C**************************************************************************
C
	SUBROUTINE domacro (macrofile, macroname, maxpar, erpar, 
     1		     mclines, mcrem,
     1               inline, command, prevcom)
C
C***************************************************************************

	IMPLICIT NONE

	CHARACTER*(*) macrofile, macroname, command, prevcom
	CHARACTER*80 inline
	INTEGER erpar(*), maxpar, i
	INTEGER umacro, maxlines, mclines, mcrem
	PARAMETER(umacro=15, maxlines=500)
	CHARACTER macline*80
	DIMENSION macline(maxlines)
c	CHARACTER macline*80(maxlines)---da error en linux

C---- Read in macro

	IF (mcrem .EQ. 0) THEN

	    OPEN(UNIT=umacro,FILE=macrofile, STATUS='OLD', ERR=90)
	    mclines = 0
	    DO 10 i = 1, maxlines
	        READ(umacro,'(A)',END=20) macline(i)
	        mclines = mclines + 1
10	    CONTINUE
20	    CLOSE(umacro)
	    mcrem = mclines

	    RETURN

C----- Execute macro lines one after another

	ELSE
	    DO 30 i = 1, maxpar
		erpar(i) = 0
30	    CONTINUE
	    prevcom = command
	    inline = macline(mclines - mcrem + 1)
	    PRINT '(A)', 'domacro>> '//inline
	    mcrem = mcrem - 1
	    RETURN

	END IF

90	PRINT *, 'domacro>> error opening macro file'
	RETURN

	END
