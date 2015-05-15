

C************************************************************************

C	Subroutine to produce a velocity map

C************************************************************************


	SUBROUTINE velmap(istart,iend,xbuff,ybuff,vbuff,imsize,
     &		            coordmax,smooth,newimsize,
     &                      inbin,newinbin,newmap,map,pmass)

	DIMENSION xbuff(1),ybuff(1),vbuff(1),pmass(1)
	REAL map(imsize,imsize)
	REAL newmap(newimsize,newimsize)
	INTEGER inbin(imsize,imsize)
	INTEGER newinbin(imsize,imsize)
	REAL newmax

	IF(smooth.eq.-1.)THEN

	   DO 1 j=1,imsize
 	      DO 1 i=1,imsize
                 map(i,j)=0.
   1   		 inbin(i,j)=0

	   DO 10 i=istart,iend
	         ix=int(((xbuff(i)+coordmax)/
     &                   (2.*coordmax))*float(imsize-1)+1.)
  	         iy=int(((ybuff(i)+coordmax)/
     &                   (2.*coordmax))*float(imsize-1)+1.)
	         IF(ix.lt.1.or.ix.gt.imsize)GOTO 10
	         IF(iy.lt.1.or.iy.gt.imsize)GOTO 10
  	         map(ix,iy)=map(ix,iy)+vbuff(i)*pmass(i)/pmass(1)
		 inbin(ix,iy)=inbin(ix,iy)+1
  10	         CONTINUE

	   DO 11 j=1,imsize
 	      DO 11 i=1,imsize
	         IF(inbin(i,j).ne.0)THEN
                    map(i,j)=map(i,j)/float(inbin(i,j))
                    ENDIF
  11             CONTINUE

	   ELSE

	   del=2*coordmax/float(imsize)
	   newmax=coordmax+del

	   DO 5 j=1,imsize
 	      DO 5 i=1,imsize
   5             map(i,j)=0.

	   DO 6 j=1,newimsize
 	      DO 6 i=1,newimsize
                 newmap(i,j)=0.
   6             newinbin(i,j)=0

	   DO 20 i=istart,iend
	         ix=int(((xbuff(i)+newmax)/
     &                   (2.*newmax))*float(newimsize-1)+1.)
  	         iy=int(((ybuff(i)+newmax)/
     &                   (2.*newmax))*float(newimsize-1)+1.)
	         IF(ix.lt.1.or.ix.gt.newimsize)GOTO 20
	         IF(iy.lt.1.or.iy.gt.newimsize)GOTO 20
  	         newmap(ix,iy)=newmap(ix,iy)+vbuff(i)*pmass(i)/pmass(1)
		 newinbin(ix,iy)=newinbin(ix,iy)+1
  20	         CONTINUE

	   DO 21 j=1,newimsize
 	      DO 21 i=1,newimsize
	         IF(newinbin(i,j).ne.0)THEN
                    newmap(i,j)=newmap(i,j)/float(newinbin(i,j))
	            ENDIF
  21             CONTINUE

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
		  OPEN(unit=8,STATUS='unknown',FILE='velmap.out')
	   DO 7 j=1,imsize
 	      DO 7 i=1,imsize
   7             map(i,j)=map(i,j)/16.
	   ENDIF

	   do 666 i=1,imsize
	      do 667 j=1,imsize
		 write(8,*) i,j,map(i,j)
 667          continue
 666	      continue

	   print*,map(1,1),map(25,25)
	   close(8)
	RETURN
	END
