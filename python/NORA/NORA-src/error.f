


C***********************************************************************
C
C
                       SUBROUTINE error(uerr, message)
C
C
C***********************************************************************
C     Subroutine to output error messages to logical unit uerr
C=======================================================================
     
        INTEGER uerr
        CHARACTER*(*) message
     
        WRITE(uerr,40)
 40     FORMAT(/,1x,72('/'))
        WRITE(uerr,50) message
 50     FORMAT(/,a)
        WRITE(uerr,40)
     
        RETURN
        END
