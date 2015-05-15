C***********************************************************************
C	This file contains FUNCTIONs and SUBROUTINES 
C	which perform various operations on vectors (arrays)
C	Vectorized versions of them exist on the CRAY, 
C	these here are scalar representations. 
C	From TREECODE's TREEUTILSUN.F file by L. Hernquist.
C
C	Contents:
C
C  FUNCTION cvmgp(y1,y2,y3)
C			conditional vector merge on positive
C  FUNCTION cvmgt(y1,y2,y3)
C			conditional vector merge on true 
C  FUNCTION ismax(n,x,inc)
C			locate index of maximum element of a real vector.
C  FUNCTION ismin(n,x,inc)
C			locate index of minimum element of a real vector.
C  FUNCTION isrchigt(n,iarray,inc,itarget)
C			Function to return index of first element 
C			of iarray greater than itarget, or n+1 
C			if none is found.
C  SUBROUTINE wheneq(n,iarray,inc,itarget,index,nval)
C			Subroutine to return locations of all elements 
C			of an integer vector equal to itarget.
C  SUBROUTINE whenfgt(n,array,inc,target,index,nval)
C			Subroutine to return locations of all elements 
C			of a real vector greater than target.
C  SUBROUTINE whenflt(n,array,inc,target,index,nval)
C			Subroutine to return locations of all elements 
C			of a real vector less than target.
C  SUBROUTINE whenigt(n,iarray,inc,itarget,index,nval)
C			Subroutine to return locations of all elements 
C			of an integer vector greater than itarget.
C  SUBROUTINE whenile(n,iarray,inc,itarget,index,nval)
C			Subroutine to return locations of all elements 
C			of an integer vector less than or equal to itarget.
C  SUBROUTINE whenilt(n,iarray,inc,itarget,index,nval)
C			Subroutine to return locations of all elements 
C			of an integer vector less than itarget.
C  SUBROUTINE whenne(n,iarray,inc,itarget,index,nval)
C			Subroutine to return locations of all elements 
C			of an integer vector not equal to itarget.
C***********************************************************************
