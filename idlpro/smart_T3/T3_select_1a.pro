; T3 select images
function version
  ver=0.1  ;  2011.9.25  k.i., t.k.,   frame selecton
  ver=0.2  ;  2011.10.08 TTI cloud check, rms save 
  ver=0.3  ;  2012.01.24 TTI (separate selected/events)
  return,ver
end
;--------------------------------------------------------------------   
function fname2msec,fname,ip

tim=(strmid(fname,ip,2)*3600l+strmid(fname,ip+2,2)*60l $
    +strmid(fname,ip+4,2))*1000l+strmid(fname,ip+7,3)
return,tim
end

;--------------------------------------------------------------------   
pro T3_select_1,day

dir='E:\T3\'
;day='20111018'
start_time=''


pair_filename=dir+'selected\'+day+'\'+day+'_pair0_files.sav'

seldir=dir+'selected\'+day

cd,dir+day
cfi=file_search('*\*\Co_*')
print,'loaded Co files, ',n_elements(cfi),' files'
hfi=file_search('*\*\Ha_*')
print,'loaded Ha files, ',n_elements(hfi),' files'

spawn,'mkdir '+seldir

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
ctim2=fname2msec(cfi2,ip)
htim2=fname2msec(hfi2,ip)
plot,ctim2-htim2
ans=''
read,'Ok? (y/n)',ans
if ans eq 'n' then stop 

ii=where(abs(htim2-ctim2) lt 6)
cfi3=cfi2[ii]
hfi3=hfi2[ii]
ctim3=fname2msec(cfi3,ip)
htim3=fname2msec(hfi3,ip)
;;endif

save,cfi3,hfi3,file=pair_filename

return


end
  
