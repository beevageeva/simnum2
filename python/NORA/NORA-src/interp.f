


      Subroutine INTERP(Pix,Nwid,Nmax,Nlines,X,Y,Val)
C
C  Bilinearly interpolates an array of I*2 data to find a value at
C    a mid-pixel position.
C
      Integer*2 Nwid,Nmax,Nlines,Xlo,Ylo,Xhi,Yhi
c	INTEGER*2 PIX(NWID,*)
	real*4 pix(nwid,*)
      Real*4 X,Y,Val,Dx,Dy,Dx2,Dy2
      Xlo=Ifix(X)
      If(Xlo.LT.1) Go to 100
      Xhi=Xlo+1
      If(Xhi.GT.Nmax) Go to 100
      Ylo=Ifix(Y)
      If(Ylo.LT.1) Go to 100
      Yhi=Ylo+1
      If(Yhi.GT.Nlines) Go to 100
      If(Pix(Xlo,Ylo).LT.-1000) Go to 100
      If(Pix(Xlo,Yhi).LT.-1000) Go to 100
      If(Pix(Xhi,Yhi).LT.-1000) Go to 100
      If(Pix(Xhi,Ylo).LT.-1000) Go to 100
      Dx=X-Xlo
      Dx2=1.0-Dx
      Dy=Y-Ylo
      Dy2=1.0-Dy
      Val=Pix(Xlo,Ylo)*Dx2*Dy2+Pix(Xlo,Yhi)*Dx2*Dy+Pix(Xhi,Ylo)*Dx*Dy2+
     &  Pix(Xhi,Yhi)*Dx*Dy
      Return
  100 Val=-32768.0
      Return
      End
