

      Subroutine MATINV(A,L,N,B,M,Determ)
C
C  Inverts matrix A and solves linear equations A.C(I)=B(I) for I=1,M
C  Inverted matrix is returned in A and resultant vectors in B
C
C N is size of matrix (N < 10)
C M is number of vector equations to solve (if M <= 0 no equations
C    are solved)
C Determ is determinant, if Determ=0.0 then inversion has failed
C
C  Uses Gaussian elimination with optimised pivots
C
C     Integer*2 N,L,M
      Real*4 Pivot,Temp,Determ,A(L,L),B(L,1)
      Dimension Index(2,10)
      Logical*1 Lpivot(10)
      Determ=1.0
      Do 2 I=1,N
        Lpivot(I)=.FALSE.
2      END DO
      Do 60 I=1,N
        Pivot=0.0
C
C Search for pivot element
C
        Do 10 J=1,N
          IF(Lpivot(J)) Go to 10
          Do 5 K=1,N
            IF(Lpivot(K)) Go to 5
            IF(Abs(Pivot).GE.Abs(A(K,J))) Go to 5
            Pivot=A(K,J)
            Irow=J
            Icolum=K
    5     CONTINUE
   10   CONTINUE
        Determ=Determ*Pivot
        IF(Pivot.EQ.0.0) Return
        Lpivot(Icolum)=.TRUE.
        Index(1,I)=Irow
        Index(2,I)=Icolum
C
C Interchange rows to put pivot element on diagonal
C
        IF(Irow.EQ.Icolum) Go to 23
        Determ=-Determ
        Do 15 J=1,N
          Temp=A(J,Irow)
          A(J,Irow)=A(J,Icolum)
          A(J,Icolum)=Temp
15      CONTINUE
        IF(M.LE.0) Go to 23
        Do 20 J=1,M
          Temp=B(Irow,J)
          B(Irow,J)=B(Icolum,J)
          B(Icolum,J)=Temp
20      CONTINUE
C
C Divide pivot row by pivot element
C
   23   A(Icolum,Icolum)=1.0
        Do 25 J=1,N
          A(J,Icolum)=A(J,Icolum)/Pivot
25      CONTINUE
        IF(M.LE.0) Go to 33
        Do 30 J=1,M
          B(Icolum,J)=B(Icolum,J)/Pivot
30      CONTINUE
C
C Subtract pivot row from other rows
C
   33   Do 50 J=1,N
          IF(J.EQ.Icolum) Go to 50
          Temp=A(Icolum,J)
          A(Icolum,J)=0.0
          Do 40 K=1,N
            A(K,J)=A(K,J)-A(K,Icolum)*Temp
40        CONTINUE
          IF(M.LE.0) Go to 50
          Do 45 K=1,M
            B(J,K)=B(J,K)-B(Icolum,K)*Temp
45        CONTINUE
   50   CONTINUE
60     CONTINUE
C
C Interchange columns
C
      K=N+1
      Do 2000 I=1,N
        K=K-1
        IF(Index(1,K).EQ.Index(2,K)) Go to 2000
        Irow=Index(1,K)
        Icolum=Index(2,K)
        Do 70 J=1,N
          Temp=A(Irow,J)
          A(Irow,J)=A(Icolum,J)
          A(Icolum,J)=Temp
70      CONTINUE
 2000 CONTINUE
      Return
      End
