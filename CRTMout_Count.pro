 file = 'CRTMtest.dat'
 xs   = 221
 ys   = 127

 C   = bytarr(xs,ys)

 openr,1,file
 readu,1,C
 close,1

 C = reverse(C,2)

 r = 3
 window,0,xs=xs*r,ys=ys*r
 C = congrid(C,xs*r,ys*r)

 tv,C

end
