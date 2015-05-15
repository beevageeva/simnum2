		
C------------------------------------------------------------
		SUBROUTINE getsubstr
     1	(string, substr, lsubstr, irest, ier)
C------------------------------------------------------------
	
C	Finds leftmost comma- or space-ended substring in string.
C	Gets rid of leading blanks
C	Returns length of substr and index of first non-blank after substr
C		string		input character string
C 		substr		output, left-justified
C		lsubstr	INT	length substr excl trailing blanks
C		irest	INT	index of first non-blank following substr
C				=0 when no non-blanks after substr
C MB 12/8/93

	IMPLICIT NONE
	INTEGER lsubstr, irest, ier, i, first, last
	INTEGER length, LEN
	CHARACTER*(*) string, substr
	
	
	ier = 0

C----erase substr
	DO 5 i = 1, LEN(substr)
		substr(i:i) = ' '
5	CONTINUE
	
	length = LEN(string)
C----first non blank
	first = 0
	DO 10 i = 1, length
		IF (string(i:i) .NE. ' ') THEN
			first = i
			GO TO 15
		END IF
10	CONTINUE
	lsubstr = 0
	irest = 0
	ier = 1
CCC	PRINT *, ' getsubstr>> Empty string '
	RETURN
C----Next separator: ' ' or ','
15	CONTINUE
	DO 20 i = first, length
		IF (     string(i:i) .EQ. ' ' 
     1		    .OR. string(i:i) .EQ. ',' ) THEN
			last = i-1
			GO TO 25
		END IF
20	CONTINUE
C----Case full string
	last = length
	irest = 0
	GO TO 35
C----Next non-blank after substring
25	DO 30 i = last+1, length
		IF (     string(i:i) .EQ. ' '
     1		    .OR.     string(i:i) .EQ. ',' ) GO TO 30
			irest = i
			GO TO 35
30	CONTINUE
	irest = 0
C----Sort things out
35	CONTINUE
	substr = string(first:last)
	lsubstr = last - first + 1
	
	RETURN
	END
