file=file_search('C:\data\20160813\','test03*.fits',count=nf)
x=252
y=232

;--------------------------------
prof=fltarr(100,nf)
window,0,xs=640,ys=512
for i=0,nf-1 do begin
	mreadfits,file[i],index,data
	prof[*,i]=data[x,y,*]
	if i eq 0 then begin
		plot,prof[*,0]
	endif else begin
		oplot,prof[*,i]
	endelse
endfor

END