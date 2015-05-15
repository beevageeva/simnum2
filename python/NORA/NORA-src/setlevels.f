 
C*****************************************************************************
 
	   SUBROUTINE setlevels(cmin, cmax, nlev, ctype, level, maxlev)
 
C*****************************************************************************

	IMPLICIT NONE
	INTEGER nlev, maxlev, i
	REAL level, cmin, cmax, top, z
	DIMENSION level(*)
	CHARACTER ctype*3

        DO 10 i = 1, maxlev
                level(i) = 1.0e+10
 10     CONTINUE

        IF (nlev .LT. 1 .or. nlev .GT. maxlev) then
            write(6,*) ' 1 to ', maxlev, '  levels only.'
            RETURN
	END IF
C--------
C  find levels
C--------

C--------Linear
        IF (ctype .EQ. 'lin' .OR. ctype .EQ. 'LIN') THEN
            DO 20 i = 1, nlev
                level(i) = cmin + FLOAT(i - 1)*
     &                              (cmax - cmin)/(nlev - 1)
 20         CONTINUE
C--------Logarithmic
        ELSE IF (ctype .EQ. 'log' .OR. ctype .EQ. 'LOG') THEN
            IF (cmin .LE. 0.0 .OR. cmax .LE. 0.0) THEN
                      write(6,*) ' Can''t take log of negative #s'
                      RETURN
            ELSE
                   TOP = LOG10(cmax - cmin)
                   DO 30 I = 1,NLEV
                      Z = I/(NLEV+1.0)  ! range: 0<Z<1
                      LEVEL(I) = cmin + 10**(Z*(TOP))
 30                CONTINUE
            END IF
        ELSE
            WRITE(6,'(A)') ' setlevels>> lin or log ? '
            RETURN
        END IF

	RETURN
	END
