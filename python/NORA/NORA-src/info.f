

C*****************************************************************************
	    SUBROUTINE info(ushow, command, gdev, filename, 
     1                model, model1, model2, modinc,
     2                istart, iend,
     3                theta, phi,
     4                coordmax, PA, Rmax, swidth,
     5                level, nlev, 
     6                infostr)
C*****************************************************************************
C  Max 25 cont lev
C  ushow = 0 does not output result
C---------------------

	IMPLICIT NONE
	INTEGER ushow, model, model1, model2, modinc, istart, iend
	INTEGER nlev,L, i, done, ipos, L1, L2, L3, strlength, ier
	REAL theta, phi, coordmax, PA, Rmax, swidth, level
	DIMENSION level(*)
	CHARACTER filename*40
	CHARACTER*80 command, infostr(6), aux1, aux2, aux3
	CHARACTER*(*) gdev

c---EN LINUX NLEV DA UN ERROR (VER HOLA5) HAGO NLEV=0 AQUI
	nlev=0
C------erase
	DO 10 i = 1, 80
		infostr(1)(i:i) = ' '
		infostr(2)(i:i) = ' '
		infostr(3)(i:i) = ' '
		infostr(4)(i:i) = ' '
		infostr(5)(i:i) = ' '
		infostr(6)(i:i) = ' '
10	CONTINUE

C------String 1
	ipos = 1
	infostr(1)(ipos:ipos+4) = 'FILE '
	ipos = ipos + 5
	L = strlength(filename, ier)
	infostr(1)(ipos:(ipos+L-1))=filename(1:L)
	ipos = ipos + L + 4
	infostr(1)(ipos:ipos+5) = 'MODEL '
	ipos = ipos + 7
	WRITE(infostr(1)(ipos:ipos+3),'(I4)') model
	ipos = ipos + 7
	IF (modinc .LE. model2 - model1) THEN
	    WRITE(infostr(1)(ipos:ipos+9),'(2I5)') model2, modinc
	END IF

C------String 2
	ipos = 0
	infostr(2)(1:5) = 'PART '
	ipos = 6
	WRITE(aux1,'(I7)') istart
	L1 = strlength(aux1, ier)
	WRITE(aux2,'(I7)') iend
	L2 = strlength(aux2, ier)
	infostr(2)(ipos:ipos+L1+L2+1) = aux1(1:L1) // '-' // aux2(1:L2)
	ipos = ipos + L1 + L2 + 2
	infostr(2)(ipos:ipos+5) = ' VIEW '
	ipos = ipos + 6
	WRITE(aux1, '(F6.1)') theta
	L1 = strlength(aux1, ier)
	WRITE(aux2, '(F6.1)') phi
	L2 = strlength(aux2, ier)
	infostr(2)(ipos:ipos+L1+L2+1) = aux1(1:L1) // ',' // aux2(1:L2)
	ipos = ipos + L1 + L2 + 2
	infostr(2)(ipos:ipos+5) = ' RMAX '
	ipos = ipos + 6
	WRITE(infostr(2)(ipos:ipos+4),'(F5.1)') coordmax
	ipos = ipos + 6
	infostr(2)(ipos:ipos+5) = ' SLIT '
	ipos = ipos + 6
	WRITE(aux1, '(F5.1)') PA
	L1 = strlength(aux1, ier)
	WRITE(aux2, '(F4.1)') Rmax
	L2 = strlength(aux2, ier)
	WRITE(aux3, '(F5.2)') swidth
	L3 = strlength(aux3, ier)
	infostr(2)(ipos:ipos+L1+L2+L3+2) = 
     &		aux1(1:L1) // ',' // aux2(1:L2)// ',' // aux3(1:L3)

C------String 3, 4, 5
	infostr(3)(1:9) = 'CONTOURS '
	ipos = 10

	IF (nlev .EQ. 0) GO TO 40
	DO 15 i  = 1, nlev
	  IF (ipos .LT. 73) THEN
	    WRITE(infostr(3)(ipos:ipos+7), '(F8.2)') level(i)
	    done = i
	    ipos = ipos + 9
	  ELSE

	    GOTO 20
	  END IF
15	CONTINUE

20	ipos = 1
	IF (done .LT. nlev) THEN
	DO 25 i = done + 1, nlev
	  IF (ipos .LT. 73) THEN
	    WRITE(infostr(4)(ipos:ipos+7), '(F8.2)') level(i)
	    done = i
	    ipos = ipos + 9
	  ELSE

	    GOTO 30
	  END IF
25	CONTINUE
	END IF

30	ipos = 1
C        print*,'HOLA5',done,nlev
        pause
	IF (done .LT. nlev) THEN
	DO 35 i = done + 1, nlev
	    WRITE(infostr(5)(ipos:ipos+7), '(F8.2)') level(i)
	    ipos = ipos + 9
35	CONTINUE
	END IF
	print*,'HOLA6'
	pause
C------String 6
40	infostr(6)(1:13) = 'LAST COMMAND '
	ipos = 15
	L = strlength(command, ier)
	infostr(6)(ipos:ipos+L-1) = command(1:L)
	ipos = ipos + L + 1
	infostr(6)(ipos:ipos+9) = ' GRAPHICS '
	ipos = ipos + 11
	L = strlength(gdev, ier)
	infostr(6)(ipos:ipos+L-1) = gdev(1:L)

C------Write out
	IF (ushow .NE. 0) THEN
	DO 50 i = 1, 6
	    WRITE(ushow, '(A)') infostr(i)
50	CONTINUE
	END IF

	RETURN
	END
