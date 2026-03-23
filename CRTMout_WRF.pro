 file = 'CRTMout.wrf.dat'
 file = 'CRTMout.wrf.vis.091404'
 xs   = 221
 ys   = 127

 Counts = bytarr(xs,ys,1)
 BTs    = fltarr(xs,ys,1)

 openr,1,file
 readu,1,Counts
 readu,1,BTs
 close,1
 
 Counts = reverse(Counts,2)
 BTs    = reverse(BTs,2)
 r = 3
; window,0,xs=xs*r,ys=ys*r

 B03 = reform(Counts[*,*,0],xs,ys)
;B08 = reform(Counts[*,*,1],xs,ys)
;B09 = reform(Counts[*,*,2],xs,ys)
;B10 = reform(Counts[*,*,3],xs,ys)
;B13 = reform(Counts[*,*,6],xs,ys)
;
 B03 = congrid(B03,xs*r,ys*r)
;B08 = congrid(B08,xs*r,ys*r)
;B09 = congrid(B09,xs*r,ys*r)
;B10 = congrid(B10,xs*r,ys*r)
;B13 = congrid(B13,xs*r,ys*r)
 
 window,2,xs=xs*r,ys=ys*r
 tv,B03
; window,1,xs=xs*r,ys=ys*r
; tv,B13


  
 

 

end
