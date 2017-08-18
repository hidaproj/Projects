pro lc_fileread,$
	LCN,temp,$	;input
	wl,rets,volt	;output

dir='C:\Projects\data\LC\'
file=file_search(dir+$
		'LC'+strtrim(fix(LCN),2)+$
		'_T'+strtrim(fix(temp),2)+$
		'_Ip00_01_v_ret.dat')
restore,file[0]

end

pro lc_fileread2,$
	LCN,temp,$	;input
	wl,rets,volt	;output

dir='C:\Projects\data\LC\'
files=file_search(dir+$
		'LC'+strtrim(fix(LCN),2)+$
		'_T*'+$
		'_Ip00_01_v_ret.dat')

s=sort(files)

files=files[s]

n=n_elements(s)

retss=dblarr(1034,20,n)

for i=0,n-1 do begin

	restore,files[i]
	retss[*,*,i]=rets
endfor


stop
end


pro lc_vout,$
	wl,rets,volt,wl0,in_ret,$	;input
	oret,outv,$			;output
	pl=pl				;plot option

	wl0=wl0/10.
	wl1=where(abs(wl-wl0) eq min(abs(wl-wl0)))
	ret=rets*360.
	vrange=[1.5,max(volt)]
	k=7
	ii=where(volt ge vrange(0) and volt le vrange(1))
	volt1=double(volt(ii))
	ret1=double(ret(ii))
	coeff=poly_fit(ret1,1./volt1,k,yfit,/double)
	d=long(max(ret1)-min(ret1))*10.
	x=min(ret1)+((max(ret1)-min(ret1))/d)*findgen(d+1)
	y=poly_coeff(x,coeff)
	if in_ret ge min(x) then begin
		ox=(where(abs(x-in_ret) eq min(abs(x-in_ret))))[0]
		oret=x[ox]
		outv=y[ox]
	endif else begin
		ox=(where(abs(x-(in_ret+360.)) eq min(abs(x-(in_ret+360.)))))[0]
		oret=x[ox]-360.
		outv=y[ox]
	endelse
	if keyword_set(pl) then begin
	tit='applied voltage : '+string(1./outv,form='(f5.2)')+'v'
	plot,ret1,1./volt1,xtitle='ret [deg]',ytitle='inv_volt [1/v]',charsize=1.5,title=tit,$
		xthick=2,ythick=2,psym=1
	oplot,x,y,line=0,thick=1
	oplot,[1,1]*oret,[0,outv],line=2
	oplot,[0,oret],[1,1]*outv,line=2
	endif
	outv=1./outv
end

pro lc_volt_cltlib
end