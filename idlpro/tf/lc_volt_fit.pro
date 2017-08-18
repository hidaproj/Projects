function LC_volt_fit,lc,wl0,ret0,temp0

;wl0=656.28  as nm
;ret0=0.5    as wave
;lc='LC3'    as text
;t=20.0      as c

; example
; ret=findgen(360)/360.
; vv=fltarr(360)
; for i=0,360-1 do vv[i]=lc_volt_fit('LC3',656.28,ret[i],20.0) ;<- LC, wl, ret, temp 
; plot,ret,vv

kk='3'
file='C:\Projects\data\ak\ak_'+LC+kk+'_m05.dat'
restore,file
;print,file

back:
sc=size(ck)

bk=fltarr(sc[1],sc[2])	;order of ak, oder of bk
for j=0,sc[2]-1 do begin
	for i=0,sc[1]-1 do begin
		bk[i,j]=poly(temp0,ck[i,j,*])
	endfor
endfor

;---------------------------------------------------
sb=size(bk)
ak=fltarr(sb[2])	;wl,temp,nk
for i=0,sb[2]-1 do begin
		ak[i]=poly(wl0,bk[i,*])
endfor

v=1./poly(ret0,ak)+0.5

;print,v

if (v ge 9.5) or (v lt 0.) then begin
	ret0=ret0+1.0
	goto,back
endif
return,v

end