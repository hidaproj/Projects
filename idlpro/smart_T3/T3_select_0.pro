; T3 select images
;   
dt=5*1000l  ; msec

dir='E:\T3\'
day='20110908'
event=['07:10','08:30']
;event=''

;;if 0 then begin
fi1=''
cd,dir+day
cfi=file_search('Co_*')
print,'loaded Co files'
hfi=file_search('Ha_*')
print,'loaded Ha files'
seldir=dir+'selected\'+day
evdir=dir+'events\'+day
spawn,'mkdir '+seldir

;-----  select simultaneous images ------
ctim=(strmid(cfi,3,2)*3600l+strmid(cfi,5,2)*60l+strmid(cfi,7,2))*1000l+strmid(cfi,10,3)
htim=(strmid(hfi,3,2)*3600l+strmid(hfi,5,2)*60l+strmid(hfi,7,2))*1000l+strmid(hfi,10,3)
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
ctim2=(strmid(cfi2,3,2)*3600l+strmid(cfi2,5,2)*60l+strmid(cfi2,7,2))*1000l+strmid(cfi2,10,3)
htim2=(strmid(hfi2,3,2)*3600l+strmid(hfi2,5,2)*60l+strmid(hfi2,7,2))*1000l+strmid(hfi2,10,3)
plot,ctim2-htim2
ans=''
read,'Ok? (y/n)',ans
if ans eq 'n' then stop 

ii=where(abs(htim2-ctim2) lt 5)
cfi3=cfi2[ii]
hfi3=hfi2[ii]
ctim3=(strmid(cfi3,3,2)*3600l+strmid(cfi3,5,2)*60l+strmid(cfi3,7,2))*1000l+strmid(cfi3,10,3)
htim3=(strmid(hfi3,3,2)*3600l+strmid(hfi3,5,2)*60l+strmid(hfi3,7,2))*1000l+strmid(hfi3,10,3)
;;endif

;-----  select dt cadence ------
nn=n_elements(cfi3)
dur=ctim3[nn-1]-ctim3[0]
tims=ctim3[0]+lindgen(dur/dt)*dt
ns=n_elements(tims)
iis=lonarr(ns)
for i=0l,ns-1 do begin
  dmy=min(abs(tims[i]-ctim3),i0)
  iis[i]=i0
endfor

cfi4=cfi3[iis]
hfi4=hfi3[iis]
print,ns,' files selected'

;-----  copy files ------
for i=0,ns-1 do begin
  spawn,'copy '+cfi4[i]+' '+seldir,/hide
  spawn,'copy '+hfi4[i]+' '+seldir,/hide
 ; img=read_tiff(cfi4[i])
 ; write_tiff,outdir+'\'+cfi4[i],img
 ; img=read_tiff(hfi4[i])
 ; write_tiff,outdir+'\'+hfi4[i],img
  print,i,'  ',cfi4[i],'   ',hfi4[i]
endfor

if keyword_set(event) then begin
  spawn,'mkdir '+evdir
  etim1=(strmid(event[0],0,2)*3600l+strmid(event[0],3,2)*60l)*1000l
  etim2=(strmid(event[1],0,2)*3600l+strmid(event[1],3,2)*60l)*1000l
  iie=where(ctim2 gt etim1 and ctim2 lt etim2)
  cfi5=cfi2[iie]
  hfi5=hfi2[iie]
  nne=n_elements(cfi5)
  print,nne,'  files in event'
  for i=0l,nne-1 do begin
    spawn,'copy '+cfi5[i]+' '+evdir,/hide
    spawn,'copy '+hfi5[i]+' '+evdir,/hide
    print,i,' /',nne,'   ',cfi5[i],'   ',hfi5[i]
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
  
