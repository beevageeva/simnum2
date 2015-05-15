 
 
C*****************************************************************************
 
 
                SUBROUTINE slit(coordmax, coord, nhalf, top, bot)
 
 
C*****************************************************************************
 
        IMPLICIT NONE
        REAL coordmax, coord, z, top, bot, logtop, logbot
        INTEGER nhalf, i
        DIMENSION coord(1)
 
C--------- levels even between LOG10(1) and LOG10(top), projected
C          onto the interval [0., coordmax]
 
        logtop = LOG10(top)
        logbot = LOG10(bot)
 
        DO 10 i = 1, nhalf
                z = logbot + FLOAT(i)/ nhalf
                coord(nhalf + i)     = 10**(z*logtop)
                coord(nhalf + i) = (coord(nhalf + i) - bot) *
     &                                           coordmax / (top - bot)
                coord(nhalf - i + 1) = -coord(nhalf + i)
 10     CONTINUE
 
        RETURN
        END
 
