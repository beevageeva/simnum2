

C************************************************************************
C
C
	                   SUBROUTINE coadd 
     &         (imsize, newimsize, map1, newmap1, map, newmap)
C
C
C************************************************************************

	IMPLICIT NONE
	INTEGER imsize, newimsize, i, j  
	REAL map, newmap, map1, newmap1
	DIMENSION map(imsize, imsize), map1(imsize, imsize)
	DIMENSION newmap(newimsize, newimsize)
	DIMENSION newmap1(newimsize, newimsize)

	DO 10 i = 1, imsize
	    DO 10 j = 1, imsize
	       map(i,j) = map(i,j) + map1(i,j)
 10	CONTINUE
        DO 20 i = 1, newimsize
            DO 20 j = 1, newimsize
	       newmap(i,j) = newmap(i,j) + newmap1(i,j)
 20	CONTINUE

	RETURN
	END
