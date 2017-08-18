; T3 select images
;   
dt=5*1000l  ; msec

dir='E:\T3\'
day='20110906'
event=['13:10','14:50']
event=''

;;if 0 then begin
fi1=''
cd,dir+day
hfi=file_search('Ha_*')
print,'loaded Ha files'
seldir=dir+'selected\'+day
evdir=dir+'events\'+day
spawn,'mkdir '+seldir

;-----  select simultaneous images ------
htim=(strmid(hfi,3,2)*3600l+strmid(hfi,5,2)*60l+strmid(hfi,7,2))*1000l+strmid(hfi,10,3)
nh=n_elements(hfi)

;-----  select dt cadence ------
nn=n_elements(hfi)
dur=htim[nn-1]-htim[0]
tims=htim[0]+lindgen(dur/dt)*dt
ns=n_elements(tims)
iis=lonarr(ns)
for i=0l,ns-1 do begin
  dmy=min(abs(tims[i]-htim),i0)
  iis[i]=i0
endfor

hfi4=hfi[iis]
print,ns,' files selected'

;-----  copy files ------
for i=0,ns-1 do begin
  spawn,'copy '+hfi4[i]+' '+seldir,/hide
 ; img=read_tiff(cfi4[i])
 ; write_tiff,outdir+'\'+cfi4[i],img
 ; img=read_tiff(hfi4[i])
 ; write_tiff,outdir+'\'+hfi4[i],img
  print,i,'  ',hfi4[i]
endfor

if keyword_set(event) then begin
  etim1=(strmid(event[0],0,2)*3600l+strmid(event[0],3,2)*60l)*1000l
  etim2=(strmid(event[1],0,2)*3600l+strmid(event[1],3,2)*60l)*1000l
  iie=where(ctim2 gt etim1 and ctim2 lt etim2)
  hfi5=hfi[iie]
  nne=n_elements(cfi5)
  print,nne,'  files in event'
  for i=0l,nne-1 do begin
    spawn,'copy '+hfi5[i]+' '+evdir,/hide
    print,i,'   ',hfi5[i]
  endfor
endif



stop
num=0l
num1=0l
for i=0l, n_elements(cfi)-1 do begin
  if i lt num then continue
  for a=0l, 400 do begin
    num=i+a
    dif= cfi1[num]- cfi1[num1]
    if dif gt 2 then begin
      spawn,'copy '+cfi[num]+' '+temp,/hide
      break
    endif else continue
    num1=num
    if num1 gt n_elements(cfi) then return
  endfor
  print,i
endfor

end
  
