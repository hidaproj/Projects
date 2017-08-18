@caiolib2

;pro temp_monitor,sec,interval,range

sec=60*2
interval=5
range=10.

t=get_therm_temp(ch=0,/init)

n=float(sec)/float(interval) ;1hour
tt0=get_therm_temp(ch=0,time=time)
damy=findgen(n)
damy[*]=tt0
window,0
plot,indgen(n),damy,yrange=2*[-5,5]+range,/nodata,/ystyle,/xstyle

; 電源電圧（V)     --- ch7
; 固定抵抗電圧（V) --- ch0-6
t=fltarr(n)
v=fltarr(n,8)
tim=strarr(n)

t=tt0
tt1=tt0
for i=0,n-1 do begin
	tt=get_therm_temp(ch=0,time=time)
	plots,i,tt,psym=1
	t=[t,tt]
	nn=n_elements(t)
	print,string(tt,form='(f5.2)')+'C ',time,' sa='+string(tt1-tt,form='(f6.2)')
	if nn gt 5 then $
	print,'mean='+string(mean(t[nn-5:nn-1]),form='(f5.2)')+', '+string(stddev(t[nn-5:nn-1]),form='(f5.2)')+'C '
	wait,float(interval)
	tt1=tt
endfor

stop

end