


C************************************************************************

	SUBROUTINE spacemap(istart,iend,xbuff,ybuff,imsize,
     &		            coordmax,smooth,newimsize,newmap,map,
     &                       pmass)

C************************************************************************
C	Subroutine to produce a spatial density map

	DIMENSION xbuff(1),ybuff(1),pmass(1)
	REAL map(imsize,imsize)
	REAL newmap(newimsize,newimsize)
	REAL newmax

	IF(smooth.eq.-1.)THEN

	   DO 1 j=1,imsize
 	      DO 1 i=1,imsize
    1            map(i,j)=0.1

	   DO 10 i=istart,iend
	         ix=int(((xbuff(i)+coordmax)/
     &                   (2.*coordmax))*float(imsize-1)+1.)
  	         iy=int(((ybuff(i)+coordmax)/
     &                   (2.*coordmax))*float(imsize-1)+1.)
	         IF(ix.lt.1.or.ix.gt.imsize)GOTO 10
	         IF(iy.lt.1.or.iy.gt.imsize)GOTO 10
  	         map(ix,iy)=map(ix,iy)+pmass(i)/pmass(1)
  10	         CONTINUE

	   ELSE

	   del=2*coordmax/float(imsize)
	   newmax=coordmax+del

	   DO 5 j=1,imsize
 	      DO 5 i=1,imsize
   5             map(i,j)=0.1
	   DO 6 j=1,newimsize
 	      DO 6 i=1,newimsize
   6             newmap(i,j)=0.1

	   DO 20 i=istart,iend
	         ix=int(((xbuff(i)+newmax)/
     &                   (2.*newmax))*float(newimsize-1)+1.)
  	         iy=int(((ybuff(i)+newmax)/
     &                   (2.*newmax))*float(newimsize-1)+1.)
	         IF(ix.lt.1.or.ix.gt.newimsize)GOTO 20
	         IF(iy.lt.1.or.iy.gt.newimsize)GOTO 20
  	         newmap(ix,iy)=newmap(ix,iy)+pmass(i)/pmass(1)
  20	         CONTINUE

 	    DO 30 j=3,newimsize-2
	       DO 30 i=3,newimsize-2
	          jj=j-2
		  DO 31 l=1,5
		     ii=i-2
		     DO 32 m=1,5
  		        map(i-2,j-2)=map(i-2,j-2)+newmap(ii,jj)
  32			ii=ii+1
  31                  jj=jj+1
  30              CONTINUE
	   DO 7 j=1,imsize
 	      DO 7 i=1,imsize
   7             map(i,j)=map(i,j)/16.
	   
C*************************************************************
C		TOO SLOW !!!
C************************************************************
C	   s2=smooth*smooth

C	   DO 5 j=1,imsize
C	      DO 5 i=1,imsize
C   5            map(i,j)=0.00001

C	   del=2.*coordmax/float(imsize)
C	   y=-coordmax
C	   DO 20 i=1,imsize
C	      PRINT *,'spacemap>> Row : ',i,' y : ',y
C	      x=-coordmax
C	      DO 21 j=1,imsize
C		 DO 22 k=1,nbods
C	            dist2=(xbuff(k)-x)*(xbuff(k)-x)+
C     &		          (ybuff(k)-y)*(ybuff(k)-y)
C  22		    map(j,i)=map(j,i)+exp(-dist2/(2.*s2))
C  21		 x=x+del
C  20	      y=y+del
C***************************************************************

	   
	   ENDIF


CCC	DO 2 j=1,imsize
CCC 	   DO 2 i=1,imsize
CCC    2         map(i,j)=alog10(map(i,j))

	RETURN
	END
