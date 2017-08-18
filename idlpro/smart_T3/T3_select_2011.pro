; T3 select images
function version
  ver=0.1  ;  2011.9.25  k.i., t.k.,   frame selecton
  ver=0.2  ;  2011.10.08 TTI cloud check, rms save 
  return,ver
end
;--------------------------------------------------------------------   
function fname2msec,fname,ip

tim=(strmid(fname,ip,2)*3600l+strmid(fname,ip+2,2)*60l $
    +strmid(fname,ip+4,2))*1000l+strmid(fname,ip+7,3)
return,tim
end

;--------------------------------------------------------------------   
dir='E:\T3\'
day='20120122'
;event=[['06:29','06:43'],['06:54','07:15'],['16:01','16:08'],['16:20','16:23'] ]
event=''
;event=[['08:22','08:32'],['09:05','09:10']]
;event=['08:06','08:23']
start_time=''

dt=5*1000l  ; time interval for selection, msec
frame_select=1
;frame_select=0

dark_level=800
;dark_level=800./256.  ;for 8-bit
selected_filename=dir+'selected\'+day+'\'+day+'_selected_files.sav'

;select_log=file_search(selected_filename)
;if select_log ne '' then begin
;  restore,select_log  ; --> cfi4,hfi4,rms2_arr
;  ns=n_elements(cfi4)
;  ip=strpos(cfi4[ns-1],'.tif',/reverse_search)
;  start_time=strmid(cfi4[ns-1],ip-10,10)
;  print,'start_time=',start_time  ;start_time='080947.104'  ; <-- 
;endif

;goto,L2

seldir=dir+'selected\'+day
evdir=dir+'events\'+day

if 1 then begin
fi1=''
cd,dir+day
cfi=file_search('*\*\Co_*')
print,'loaded Co files, ',n_elements(cfi),' files'
hfi=file_search('*\*\Ha_*')
print,'loaded Ha files, ',n_elements(hfi),' files'
spawn,'mkdir '+seldir
endif

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

L1:
;-----  select dt cadence ------
nn=n_elements(cfi3)
print,nn,' pairs (dt<6msec) counted'
dur=ctim3[nn-1]-ctim3[0]
ns=dur/dt
iis=lonarr(ns)
rms1_arr=fltarr(ns)

window,0,xs=800,ys=600
if frame_select then begin  ; frame selection
  jj=lindgen(nn/5000+1)*5000
  cfii=cfi3(jj)
  is=smenu(cfii,title='select an image')
  img1=read_tiff(cfii[is],/unsigned)
  imgsize,img1,nx,ny
  tvscl,rebin(img1,nx/2,ny/2)
  print,'select box...'
  box_cur1,x02, y02, nxb2, nyb2
  x0=x02*2 &  y0=y02*2 &  nxb=nxb2*2 &  nyb=nyb2*2
  window,2,xs=nxb2,ys=nyb2
endif 

k=0l
t1=ctim3[0] &  t2=t1+dt
;print,t1,t2
while t2 le ctim3[nn-1] do begin
    ii=where(ctim3 ge t1 and ctim3 lt t2, count)
    if count ne 0 then begin
      if frame_select then begin
        wset,2
        rms1=fltarr(count)
        for i=0,count-1 do begin
          img1=read_tiff(cfi3[ii[i]],sub_rect=[x0,y0,nxb,nyb],/unsigned)
;          img1=read_tiff(cfi3[ii[i]],sub_rect=[x0,y0,nxb,nyb])
          imgb=rebin(img1,nxb2,nyb2)
          av=mean(float(imgb))
          rms1[i]=total((imgb-av)^2)
          tvscl,imgb
        endfor
        dmy=max(rms1,i0)
      endif else begin
        i0=0
        dmy=-1.0
      endelse
      print,cfi3[ii[i0]]
      img1=read_tiff(cfi3[ii[i0]],/unsigned)
      imgsize,img1,nx,ny        
      if mean(img1) gt dark_level then begin
        iis[k]=ii[i0]        
        rms1_arr[k]=dmy
        print,cfi3[iis[k]],' /  ',count
        wset,0
        tvscl,rebin(img1,nx/2,ny/2)
        xyouts,10,10,cfi3[iis[k]],/dev
        k=k+1
      endif
    endif
;;    stop
    t1=t2 & t2=t1+dt
endwhile
iis=iis[0:k-1]
rms1_arr=rms1_arr[0:k-1]

ns=n_elements(iis)
cfi4=cfi3[iis]
hfi4=hfi3[iis]

print,ns,' files selected'
save,cfi4,hfi4,rms1_arr,file=selected_filename

;-----  copy selected files ------
for i=0,ns-1 do begin
  spawn,'copy '+cfi4[i]+' '+seldir,/hide
  spawn,'copy '+hfi4[i]+' '+seldir,/hide
 ; img=read_tiff(cfi4[i])
 ; write_tiff,outdir+'\'+cfi4[i],img
 ; img=read_tiff(hfi4[i])
 ; write_tiff,outdir+'\'+hfi4[i],img
  print,i,' / ',ns,'    ',cfi4[i],'   ',hfi4[i]
endfor

L2:
;-----  copy event files ------
if keyword_set(event) then begin
  spawn,'mkdir '+evdir
  imgsize,event,n2,nev
  for k=0,nev-1 do begin
    event1=event[*,k]
    etim1=(strmid(event1[0],0,2)*3600l+strmid(event1[0],3,2)*60l)*1000l
    etim2=(strmid(event1[1],0,2)*3600l+strmid(event1[1],3,2)*60l)*1000l
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
  endfor
endif


end
  
