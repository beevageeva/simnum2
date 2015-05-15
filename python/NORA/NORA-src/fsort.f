


      Subroutine FSORT(X,L,N)
C
C  Fast-sorts one real array (X) and integer*4 array (L).
C
      Integer*4 N,L(N)
      Real*4 X(N)
      Int=2
   10 Int=2*Int
      If(Int.LT.N) Go to 10
      Int=Min0(n,(3*Int)/4-1)
   20 Int=Int/2
      Ifin=N-Int
      Do 70 Ii=1,Ifin
        I=Ii
        J=I+Int
        If(X(I).LE.X(J)) Go to 70
        Sx=X(J)
        Il=L(J)
   40   X(J)=X(I)
        L(J)=L(I)
        J=I
        I=I-Int
        If(I.LE.0) Go to 60
        If(X(I).GT.Sx) Go to 40
   60   X(J)=Sx
        L(J)=Il
   70 END DO
      If(Int.GT.1) Go to 20
      Return
      End
