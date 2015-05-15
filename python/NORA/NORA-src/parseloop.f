C------------------------------------------------------------------------
C Routines for a command parser
C MB 12/8/93
C
C	parseloop	subr	testing routine
C	strlength	INTfun 	index of last non-blank character
C	getsubstr	subr	returns left most substr (',' or ' ' delim)
C	strgeti		INTfun	integer read from left-just string
C	strgetr		REALfun	real read from left-just string
C	strgets		CHARfun	char read from left-just string
C------------------------------------------------------------------------
		
C========================================================================
C
CCC		SUBROUTINE parseloop

	IMPLICIT NONE
	INTEGER i, maxcmmds, maxpar, k
	INTEGER npar, len, lcom, lpar
	INTEGER strlength, strgeti, istart, irest, ier, lstring
	INTEGER modelnum
	PARAMETER (maxcmmds=16, maxpar=9)
	REAL mass, strgetr
CCC	CHARACTER cmmds(maxcmmds)*10
	CHARACTER*80 inline
	CHARACTER*80 command, par(maxpar)
	CHARACTER*80 filename
	CHARACTER*80 strgets
C------------------------------------------------------------------------
C	strlength	INTfun 	index of last non-blank character
C	getsubstr	subr	1st: inline 
C						3rd: first string, length lcom
C						5th: first non-blank after string
C	strgeti		INTfun	integer read from left-just string
C	strgetr		REALfun	real read from left-just string
C	strgets		CHARfun	char read from left-just string
C------------------------------------------------------------------------
	ier = 0
C----Initialize
5	DO 6 i = 1,80
	    inline(i:i) = ' '
6	CONTINUE
	DO 7 k = 1, maxpar
	    DO 7 i = 1, 80
		par(k)(i:i) = ' '
7	CONTINUE
C----Read Command line
	PRINT '($,A)', ' Your command >> '
	READ(5,'(A)') inline
	
C----command, parameters
	len = strlength(inline, ier)
	CALL getsubstr (inline(1:len), command, lcom, irest, ier)
	npar = 0
	istart = 1
	DO 10 i = 1, maxpar
		IF (irest .EQ. 0) GO TO 15
		istart = istart + irest - 1
		len = len - irest + 1
		CALL getsubstr
     1			(inline(istart:), par(i), lpar, irest, ier)
		npar = npar + 1
10	CONTINUE
15	PRINT *, ' parser>> npar = ', npar

C---Case data
C------------
	IF (     command(1:lcom) .EQ. 'data' 
     1		.OR. command(1:lcom) .EQ. 'DATA') THEN
 	    PRINT *, ' Case DATA'
C---Get filename
	    filename = strgets(par(1), lstring, ier)
	    IF (ier .NE. 0) GO TO 25
C--Get modelnum
	    modelnum = strgeti(par(2),ier)
	    IF (ier .NE. 0) GO TO 25
C--Get mass
	    mass = strgetr(par(3), ier)
            IF (ier .NE. 0) GO TO 25
	
	    len = strlength(filename, ier)
	    WRITE(6,1000) filename(1:len), modelnum, mass
1000	    FORMAT(1x, (A), 1x, I5, F6.1)
	    GO TO 5
C--Errors
25	    PRINT *
	    PRINT *, 'usage     data filename modelnum [model2 modinc]'
	    PRINT *
	    GO TO 5
	ELSE
	    PRINT *, 'ciao'
	    STOP
	END IF	
90	PRINT*, 'Error in parser'
	GO TO 5

	END
