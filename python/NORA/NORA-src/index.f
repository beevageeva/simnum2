      SUBROUTINE INDEXX(nbod,ARRIN,INDX)
      real arrin(nbod)
      integer INDX(nbod)

c indexes an array ARR(1:N), i.e., outputs the array INDX(1:N)
c such that arr(indx(j)) is in ascendeing order for j=1,2,...,N.
c the input quantities N and ARR are not changed.

      DO 11 J=1,nbod
        INDX(J)=J
11    CONTINUE
      L=nbod/2+1
      IR=nbod
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          INDXT=INDX(L)
          Q=ARRIN(INDXT)
        ELSE
          INDXT=INDX(IR)
          Q=ARRIN(INDXT)
          INDX(IR)=INDX(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            INDX(1)=INDXT
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
          ENDIF
          IF(Q.LT.ARRIN(INDX(J)))THEN
            INDX(I)=INDX(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        INDX(I)=INDXT
      GO TO 10
      END

