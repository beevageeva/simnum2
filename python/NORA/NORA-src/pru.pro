pro pru

N=20
nhalf=N/2
coord=fltarr(N)
for i=0, nhalf-1 do begin
zeta=float(i)/float(nhalf)

coord(nhalf+i)=10^(zeta)
coord(nhalf+i)=(coord(nhalf+i)-1)*1./9.
coord(nhalf-i+1)=-coord(nhalf+i)

endfor

print,coord

end

