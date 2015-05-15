

C*****************************************************************************


	     SUBROUTINE setmapzero(imsize, newimsize, map, newmap)


C*****************************************************************************

	REAL map, newmap
	DIMENSION map(imsize, imsize), newmap(newimsize, newimsize)

	DO 10 i = 1, imsize
	   DO 10 j = 1, imsize
	      map(i,j) = 0.
 10	CONTINUE
	DO 11 i = 1, newimsize
	   DO 11 j = 1, newimsize
	      newmap(i,j) = 0.
 11	CONTINUE

	RETURN
	END
