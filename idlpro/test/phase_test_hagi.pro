
nkrget,'C:\data\dst\20090203\drk_20090203_181032.dat',dh,dark


files=file_search('C:\data\dst\20090203\he_20090203_18????.dat',count=n)
pp=fltarr(40,100)
xx=findgen(40)
for jj=0,n-1 do begin
	nkrget,files[jj],h,imgs
	for kk=0,39 do pp[kk,jj]=mean(imgs[*,*,kk]-dark[*,*,kk])
	rr=fitsinn(xx,pp[*,jj],4,yfit=pp)
stop
endfor
tvscl,pp
profiles,pp

cor = fltarr(n-1)
	lag=(findgen(11)-5)
for jj=0,n-2 do begin
	p1=pp[*,jj]
	p2=pp[*,jj+1]
	cc=c_correlate(p1,p2,lag)
	cor[jj]=lag(where(cc eq max(cc)))
endfor
plot,cor
end