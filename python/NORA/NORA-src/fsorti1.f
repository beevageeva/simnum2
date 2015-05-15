
      Subroutine FSORTI1(K,L,N)
C
C  Fast-sorts one integer*4 array (K) and integer*4 array (L).
C
      Integer*4 N,KK,K(N),LL,L(N)
      Int=2
   10 Int=2*Int
      If(Int.LT.N) Go to 10
      Int=Min0(N,(3*Int)/4-1)
   20 Int=Int/2
      Ifin=N-Int
      Do 70 Ii=1,Ifin
        I=Ii
        J=I+Int
        If(K(I).LE.K(J)) Go to 70
        KK=K(J)
        LL=L(J)
   40   K(J)=K(I)
        L(J)=L(I)
        J=I
        I=I-Int
        If(I.LE.0) Go to 60
        If(K(I).GT.KK) Go to 40
   60   K(J)=KK
        L(J)=LL
   70 END DO
      If(Int.GT.1) Go to 20
      Return
      End
