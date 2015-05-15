 
 
C******************************************************************************
C
C
                        SUBROUTINE mklabel
     &     (filename,model, istart, iend, theta, phi, PA, swidth, label)
C
C
C******************************************************************************
 
        IMPLICIT NONE
        CHARACTER*(*) filename
        CHARACTER *(*) label
        INTEGER model, istart, iend
        REAL theta, phi, PA, swidth
 
        CHARACTER modnum*3, angles*26, th*4, ph*4, ppa*4, sl*5
        CHARACTER i1*5, i2*5, who*18
 
        WRITE(i1,'(I5)') istart
        WRITE(i2,'(I5)') iend
        who = '  N=' // i1 // ' to ' // i2
 
        WRITE(modnum, '(I3)') model
        WRITE(th,'(f4.0)') theta
        WRITE(ph,'(f4.0)') phi
        WRITE(ppa,'(f4.0)') PA
        WRITE(sl,'(f5.2)') swidth
 
        angles = '  (\gq,\gf)= ('//th//','//ph//') '
 
        label = ' Model=' // modnum // who // angles //
     &          ' PA=' // ppa // ' Width=' // sl
 
        RETURN
        END
