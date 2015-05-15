

C**************************************************************************

	SUBROUTINE plotlabel (id, filename, model, option,  
     1			theta, phi, coordmax, nsplit)

C**************************************************************************
	IMPLICIT NONE

	CHARACTER*20 id
        CHARACTER*40 filename
        CHARACTER*15 minmax
        CHARACTER*80 title
        CHARACTER*10 option, modeltit
        CHARACTER*3 modnum
        CHARACTER*6 maxcoord
        CHARACTER*5 cnsplit
        CHARACTER*14 split
        CHARACTER*12 angles

	INTEGER model, nsplit
        REAL theta, phi, coordmax
        LOGICAL up,down

        WRITE(modnum,'(i3)')model
        modeltit='model='//modnum
        WRITE(maxcoord,'(f3.1)')coordmax
 
        WRITE(angles,210) theta, phi
 210    FORMAT(F4.0, ',', F4.0)
 
        minmax='limits = +/-'//maxcoord
        WRITE(cnsplit,'(i5)')nsplit
        IF(up)THEN
           split=' split_u='//cnsplit
        ELSEIF(down)THEN
           split=' split_d='//cnsplit
        ELSE
           split=' split='//cnsplit
        ENDIF
 
        title=  id//filename(1:20)//modeltit//option//angles
     1                //minmax//split

	CALL PGLABEL ('X', 'Y', title)
	CALL PGEND


	RETURN 
	END
