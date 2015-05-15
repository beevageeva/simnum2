
 

C**************************************************************************
	SUBROUTINE CREAIMA(A,NX,NY,matriz)
C**************************************************************************
	REAL*4 A(NX,NY),matriz(nx,ny)
	INTEGER NX,NY
	do j=1,ny
	do i=1,nx
	a(i,j)=matriz(i,j)
	enddo
	enddo
	RETURN
	END
