


      Subroutine HISTPEAK(Hist,Hmin,Hmax,Sm,Back,Wid,Peak,Min,Max,
     &  Bk,Prop,*)
C
C  Analyses a histogram of pixel-values and determines the modal value
C    by parabolic interploation. If the histogram is noisy there is the
C    option to smooth the data before finding the peak position.
C
C  Sm -ve = find correct width to smooth histogram and use it
C      0  = no smoothing
C     1-5 = smooth with gaussian of sigma 1 to 5.
C
      Integer*2 Loop,Min,Lo,Ns(2:6),Hmin,Hmax,Sm,Max
c;c;c;Integer*2 Rng
      Integer*4 Maxpos,Min2,Mode,Nrun,Max2,Sum,Maxsum,Tot
      Integer*4 Median,Htot,Hist(Hmin:Hmax)
      Real*4 Weight(0:10,5),Rhist(-1000:32767),Back,Wid,Bk,Prop
      Real*4 AA(3,3),BB(3),XX(3),Peak
      Data Weight/.383,.2417,.0606,.0062,.0,.0,.0,.0,.0,.0,.0,
     & .1974,.1747,.121,.0655,.0279,.0092,.003,.0,.0,.0,.0,
     & .1322,.1253,.1061,.0808,.0548,.0334,.0183,.0089,.0039,.0023,.0,
     & .0996,.0954,.0888,.0752,.0605,.0458,.0324,.0217,.0136,.008,.0045,
     & .0796,.0781,.0736,.0665,.0579,.0484,.0389,.03,.0222,.0159,.0108/
      Data Ns/3,6,9,10,10/
      Loop=0
      XX(1)=1.0
	
C
C  Find range of histogram, maximum value and total
C
      Min=Hmin
      Mode=Hmin
      Tot=0
      Do I=Hmin,Hmax
        If(Hist(I).GT.0) THEN
          Max=I
          If(Hist(I).GT.Hist(Mode)) Mode=I
          If(Min.EQ.Hmin) Min=I
          Tot=Tot+Hist(I)
        END IF
      END DO
      Peak=Float(Hist(Mode))
c      Type *,Min,Max,Mode,Hist(Mode),Tot
C
C  Error if no data in histogram
C
      If(Tot.EQ.0) THEN
        Back=0.0
        Wid=0.0
        Bk=0.0
        Return 1
      END IF
      Back=Float(Mode)
C
C  Return if all data in one bin
C
      If(Hist(Mode).EQ.Tot) THEN
        Bk=Back
        Wid=0.5
c        Type *,'All histogram data in one bin'
        Return
      END IF
C
C  Find the value a given proportion of the way up the histogram
C
      Bk=Hist(Min)
      Do I=Min+1,Max
        If(Bk.GE.Prop*Tot) THEN
          Bk=Float(I)-1.0
          Go to 10
        END IF
        Bk=Bk+Hist(I)
      END DO
C
C  Use centre of gravity (mean) if data very sparse
C
   10 Occup=Float(Tot)/(Max-Min)
      If(Hist(Mode)*Occup.LT.1.0) THEN
c        Type *,Hist(Mode),Occup
        Maxpos=Min
        Maxpos=(Maxpos+Max)/2
        If(Hist(Mode).GT.10) Maxpos=Mode
        Cent=0.0
        Wid=0.0
        Do I=Min,Max
          If(Hist(I).GT.0) THEN
            Cent=Cent+Hist(I)*(I-Maxpos)
            Wid=Wid+Hist(I)*(I-Maxpos)*(I-Maxpos)
          END IF
        END DO
        Cent=Cent/Float(Tot)
        Wid=Wid/Float(Tot)
        Wid=Sqrt(Wid-Cent**2)
        Back=Maxpos+Cent
c        Type *,'Sparse',Hist(Mode),Occup,Back,Wid
        Return
      END IF
      Wid=Tot/(2.506628*Peak)
C
C  Find median of histogram values
C
      Htot=Hist(Min)
      Median=Min
      Do I=Min+1,Max
        If(Hist(I).GT.0) THEN
          If(Htot.LT.Tot/2) THEN
            Htot=Htot+Hist(I)
            Median=I
          ELSE
            Go to 20
          END IF
        END IF
      END DO
C
C  Replace mode by median if the difference is too great
C
   20 If(Iabs(Mode-Median).GT.2.0*Wid) Mode=Median
c      Type *,Median,Mode,Wid,Peak
C
C  Check whether histogram should be smoothed
C
      If(Sm.LT.0) THEN
        Sm=0
        Sum=0
C
C  Sum histogram values within +/- one sigma of mode
C
        Do I=Max0(Nint(Mode-Wid),INT(Min)),
     &                              Min0(Nint(Mode+Wid),INT(Max))
          Sum=Sum+Hist(I)
        END DO
C
C  Check that at least 60% of the expected occupancy (ie. 68.25%) is
C    within this range
C
        Fact=Sum/(0.6825*Tot)
c        Type *,'Factor',Fact
        If(Fact.LT.0.6) Sm=Ifix(Wid)
      END IF
C
C  Smooth histogram within +/- 10 sigma of mode, if necessary
C
      Min2=Min
      Max2=Max
      If(Sm.GT.0) THEN
        If(Sm.GT.5) Sm=5
c        Type *,'Smoothing histogram, length',Sm
C
C  Display raw histogram
C
        Min2=Max0(Nint(Mode-10.0*Wid),INT(Min))
        Max2=Min0(Nint(Mode+10.0*Wid),INT(Max))
c;c;c;c;Rng=Max2-Min2+1
c;c;c;c;Do I=Min2,Max2
c;c;c;c;c;Rhist(I)=Hist(I)
c;c;c;c;END DO
c;c;c;c;Call GRAPHR(Rhist(Min2),Rng,0.0,Peak,0.0,.FALSE.,.TRUE.,.FALSE.)
C
C  Smooth histogram
C
        Mode=Min2
        Do I=Min2,Max2
          Rhist(I)=0.0
          Do J=-Ns(Sm),Ns(Sm)
            If(I+J.GE.Min.AND.I+J.LE.Max) Rhist(I)=Rhist(I)+
     &        Weight(Iabs(J),Sm)*Hist(I+J)
          END DO
          If(Rhist(I).GT.Rhist(Mode)) Mode=I
        END DO
        Peak=Rhist(Mode)
        If(Nint(Peak).LT.1) THEN
c          Type *,'Histogram smoothed to nothing'
          Return 1
        END IF
C
C  Display smoothed histogram
C
c;c;c;c;Call GRAPHR(Rhist(Min2),Rng,0.0,Peak,0.0,.FALSE.,.TRUE.,.FALSE.)
C
C  Replace histogram by smoothed values
C
        Tot=0
        Do I=Min2,Max2
          Hist(I)=Nint(Rhist(I))
          Tot=Tot+Hist(I)
        END DO
        Wid=Tot/(2.506628*Peak)
c        Type *,Mode,Peak,Tot,Wid
      END IF
C
C  Return if width too narrow to interpolate
C
      If(Wid.LT.1.0) THEN
c        Type *,'Too narrow to interpolate'
        Return
      END IF
C
C  Find maximum running-mean of the correct length (2-sigma)
C
      Nrun=Ifix(Wid)
      Lo=Min2+Nrun
      Sum=0
      Do I=Lo-Nrun,Lo+Nrun
        Sum=Sum+Hist(I)
      END DO
      Maxpos=Lo
      Maxsum=Sum
      Do I=Lo+1,Max2-Nrun
        Sum=Sum-Hist(I-Nrun-1)+Hist(I+Nrun)
        If(Sum.GT.Maxsum) THEN
          Maxsum=Sum
          Maxpos=I
        END IF
      END DO
C
C  Find centre of gravity of histogram within +/- one sigma of the peak
C
  100 Cent=0.0
      Sum=0.0
      Do I=-Nrun,Nrun
        Sum=Sum+Hist(I+Mode)
        Cent=Cent+I*Hist(I+Maxpos)
      END DO
      Back=Float(Maxpos)+Cent/Sum
C
C  Use the centre of gravity if the occupancy is too low
C
      Occup=Float(Sum)/(2*Nrun+1)
c      Type *,Nrun,Maxpos,Sum,Back,Occup
      If(Occup.LT.5.0) THEN
c        Type *,'Occupancy too low, using c of g'
        Return
      END IF
C
C  Find the peak by parabolic interpolation
C
      Do I=1,3
        BB(I)=0.0
        Do J=1,3
          AA(J,I)=0.0
        END DO
      END DO
      Do I=-Nrun,Nrun
        If(Hist(I+Maxpos).GT.0) THEN
          Z=Alog(Float(Hist(I+Maxpos)))
c          Type *,I,Hist(I+Maxpos),Z
          XX(2)=Float(I)
          XX(3)=XX(2)*XX(2)
          Do J=1,3
            BB(J)=BB(J)+XX(J)*Z
            Do K=1,3
              AA(K,J)=AA(K,J)+XX(K)*XX(J)
            END DO
          END DO
        END IF
      END DO
c      Type *,AA(1,1),' points in fit'
      Call MATINV(AA,3,3,BB,1,Det)
C
C  Loop if histogram is flat or minima found
C
  200 If(BB(3).GT.-0.001) THEN
        Nrun=Nrun+1
        If(Maxpos-Nrun.LE.Min2.OR.Maxpos+Nrun.GE.Max2) THEN
c          Type *,'Interpolating over too great a range'
          Return
        END IF
        If(Loop.EQ.20) THEN
c          Type *,'Looped too many times'
          Return
        END IF
        Loop=Loop+1
c        Type *,'Loop=',Loop,'   Nrun=',Nrun
        Go to 100
      END IF
c      Type *,BB
C
C  Extract modal position, width and peak from parabola coefficients
C    and check for reasonable values
C
      Width=-0.5/BB(3)
      Back=Float(Maxpos)+BB(2)*Width
      Wid=Sqrt(Width)
      If(Sm.GT.1.AND.Wid.GT.Sm-1) Wid=Sqrt(Width-(Sm-1)**2)
      P=BB(1)-BB(2)**2/4.0/BB(3)
      If(P.GT.10.0) THEN
c        Type *,'Looping because peak height is impossibly high'
        BB(3)=0.0
        Go to 200
      END IF
      Peak=Exp(P)
c      Type *,Back,Wid,Peak
c      Type *,'Successful histogram analysis'
      Return
      End
