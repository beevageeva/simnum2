C
C ---	------------------------------------------------------------------
C	FTS_SWAP, FTS_COPY, and FTS_LEN are supporting routines
C ---	------------------------------------------------------------------
C
C ---	FTS_SWAP  Swaps bytes in integers --------------------------------
C
	SUBROUTINE FTS_SWAP(BUF,NBYTES,SWAP)
C
	LOGICAL*1 BUF(*),S1,S2,S3,S4
	INTEGER   NBYTES,NSWAP,I
C
	if (nswap.eq.4) then
	  do 100 i=1,nbytes,4
	    s1=buf(i)
	    s2=buf(i+1)
	    s3=buf(i+2)
	    s4=buf(i+3)
	    buf(i+3)=s1
	    buf(i+2)=s2
	    buf(i+1)=s3
	    buf(i)  =s4
  100	  continue
	end if
C
	if (nswap.eq.2) then
	  do 200 i=1,nbytes,2
	    s1=buf(i)
	    s2=buf(i+1)
	    buf(i+1)=s1
	    buf(i)  =s2
  200	  continue
	end if
C
	RETURN
	END
