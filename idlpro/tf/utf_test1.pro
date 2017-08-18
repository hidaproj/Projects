;************************************************* main
utftbl='utf32def.tbl'

f=utf_init(utftbl,tbldir='C:\Projects\data\UTF32\')

f.wl0=632.80
set_lc_offset,f

v=fltarr(100)
for j=0,100-1 do begin
	dwl=0.002*(j-50)
	utf_set_wl,f,dwl=dwl,wait=0.1
	tt=get_therm_temp(ch=0,time=time,vv=vv,nsample=20)
	v[j]=vv[2]
	print,j,v[j]
endfor
utf_close


end