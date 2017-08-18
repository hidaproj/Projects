@wd_sldbx
@xmovie

; T3 sync check
function version
  ver=0.1  ;  2015.03.26 TTI based on T3_event_check
  return,ver
end
;--------------------------------------------------------------------   
function fname2msec,fname,ip

tim=(strmid(fname,ip,2)*3600l+strmid(fname,ip+2,2)*60l $
    +strmid(fname,ip+4,2))*1000l+strmid(fname,ip+7,3)
return,tim

end


;--------------------------------------------------------------------   

; 観測データ（生データ）の保存先
dir='E:\T3\'
;dir='F:\T3\'

; 観測時間
day='20160705'
hh='16'
mm='03'

;plot_filename='E:\'+day+'_T3_plot.gif'


start_time=''
nn_file_log=intarr(2,3)
dt1=30*1000l  ; time interval for movie

cd,dir+day+'\'+hh+'\'+mm+'\'
cfi=file_search('Co_*')
hfi=file_search('Ha_*')

;cd,dir+day+'\'+hh+'\'
;cfi=file_search('??\Co_*')
;hfi=file_search('??\Ha_*')

;cd,dir+day+'\'
;cfi=file_search('*\??\Co_*')
;hfi=file_search('*\??\Ha_*')

print,'loaded Co files, ',n_elements(cfi),' files'
print,'loaded Ha files, ',n_elements(hfi),' files'



nn_file_log[0,0]=n_elements(cfi)
nn_file_log[1,0]=n_elements(hfi)




;-----  select simultaneous images ------
ip=strpos(cfi[0],'.tif',/reverse_search)-10
ctim=fname2msec(cfi,ip)
htim=fname2msec(hfi,ip)
if keyword_set(start_time) then begin
  stim=fname2msec(start_time,0)
  ii=where(ctim gt stim)
  ctim=ctim[ii]
  cfi=cfi[ii]
  ii=where(htim gt stim)
  htim=htim[ii]
  hfi=hfi[ii]
endif
nc=n_elements(cfi)
nh=n_elements(hfi)
hfi2=strarr(nc)
for i=0l,nc-1 do begin
  dti=abs(ctim[i]-htim)
  dmy=min(dti,i0)
  if dti[i0] lt 30 then hfi2[i]=hfi[i0]
endfor

ii=where(hfi2 ne '')
if ii[0] ne -1 then begin
  cfi2=cfi[ii]
  hfi2=hfi2[ii]
endif else begin
  cfi2=cfi
endelse

print,n_elements(cfi2),' pairs (dt<30msec) counted'

ctim2=fname2msec(cfi2,ip)
htim2=fname2msec(hfi2,ip)

nn_file_log[0,1]=n_elements(cfi2)
nn_file_log[1,1]=n_elements(hfi2)




ii=where(abs(htim2-ctim2) lt 6)
cfi3=cfi2[ii]
hfi3=hfi2[ii]
ctim3=fname2msec(cfi3,ip)
htim3=fname2msec(hfi3,ip)

nn_file_log[0,2]=n_elements(cfi3)
nn_file_log[1,2]=n_elements(hfi3)

nn=n_elements(cfi3)

c_delta_t=fltarr(2,nn-1)
for i=0d,nn-2 do begin
c_delta_t[0,i]=(ctim3[i+1]+ctim3[i])/2.
c_delta_t[1,i]=ctim3[i+1]-ctim3[i]
endfor



window,xs=1200,ys=900
!p.multi=[0,1,2]
plot,ctim2/1000./3600.,ctim2-htim2,psym=1, $
  title=day,xrange=[5,18],yrange=[-30,30],/xstyle,/ystyle, $
    ytitle='Difference Cont/Ha [msec]',xtitle='Hour [JST]'
plot,c_delta_t[0,*]/1000./3600.,c_delta_t[1,*]/1000.,/ylog,psym=1,$
  title=day,xrange=[5,18],yrange=[10,100000.]/1000.,/xstyle,/ystyle, $
    ytitle='Cadence (diff. < 6msec) [sec]',xtitle='Hour [JST]'

!p.multi=[0,1,1]

print,nn,' pairs (dt<6msec) counted'

tmp=tvrd()
tmp2=abs(tmp-255)
;write_gif,plot_filename,bytscl(tmp2)

end
  
