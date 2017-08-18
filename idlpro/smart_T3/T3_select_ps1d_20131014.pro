; T3 select images
function version
  ver=0.1  ;  2011.9.25  k.i., t.k.,   frame selecton
  ver=0.2  ;  2011.10.08 TTI cloud check, rms save 
  ver=0.3  ;  2012.01.24 TTI (separate selected/events)
  ver=0.4  ;  2012.05.02 TTI, k.i  frame selection using 1D spatial psd
  return,ver
end
;--------------------------------------------------------------------   
function fname2msec,fname,ip

tim=(strmid(fname,ip,2)*3600l+strmid(fname,ip+2,2)*60l $
    +strmid(fname,ip+4,2))*1000l+strmid(fname,ip+7,3)
return,tim
end

;----------------------------------------------------------------
; imgpsd1.pro
;   1D spatial psd of image
;       2012.4.29       k.i.

function imgpsd1,data

pix1=0.215      ; arcsec
profs=ishft(data,-4)
imgsize,profs,nx,ny

profs=profs-mean(profs)

n2=nx/2
pow=fltarr(n2)
for j=0,ny-1 do begin
        f=fft(profs[*,j],-1)
        ff=float(f*conj(f))
        pow=pow+ff[0:n2-1]
endfor
kx=findgen(nx)/(pix1*nx)
ii=where(kx ge 0.3 and kx lt 0.6)
pp=mean(pow[ii])        ; <-- seeing?

return,pp

end

;--------------------------------------------------------------------   
;;dir='E:\T3\'
;;dir='F:\T3\'
;;dirout='D:\T3\'
;;day = '20131012'

event=''

;event=['13:12','13:18','13:30']
;event記入方法
;event=['start','peak','end']
;event=['07:37','07:44','07:56'] ;jst
;二回あった場合
;event=[['start','peak','end'],['start','peak','end']]
;event=[['07:56','08:07','08:19'],['10:55','11:00','11:09']] ;jst

event=[['07:03','07:16','07:30'],['09:04','09:08','09:12'],['09:27','09:44','10:30']]
;event=['08:06','08:07','09:30']

;event=[['08:00','08:03','08:06'],['09:06','09:17','09:30'], $
;        ['10:04','10:06','10:10'],['10:42','10:47','10:50'],['11:00','11:09','11:20']]



;start_time=''

seldir=dirout+'selected\'+day

dt1=1*1000l  ; time interval for selection, msec
dt2=5*1000l

frame_select=1
;frame_select=0

dark_level=800
selected_filename=dirout+'selected\'+day+'\'+day+'_selected_files.sav'
pair_1s_filename=dirout+'selected\'+day+'\'+day+'_pair_files_1s.sav'

events_log=dirout+'selected\'+day+'\'+day+'_events_log.sav'
save,event,file=events_log


pair_all_filename=dirout+'selected\'+day+'\'+day+'_pair_files_all.sav'
pair_nodark_filename=dirout+'selected\'+day+'\'+day+'_pair_files_nodark.sav'
;restore,pair_all_filename
cfi2=cfi99
hfi2=hfi99

cd,dir+day

ii=where(abs(htim2-ctim2) lt 6)
cfi3=cfi2[ii]
hfi3=hfi2[ii]
ctim3=fname2msec(cfi3,ip)
htim3=fname2msec(hfi3,ip)

;-----  select dt cadence ------
nn=n_elements(cfi3)
print,nn,' pairs (dt<6msec) counted'
dark_check=intarr(nn)
dt=dt1

dur=ceil(ctim3[nn-1]/1000.)*1000.-fix(ctim3[0]/1000.)*1000.
ns=ceil(dur/dt)
iis=lonarr(ns)
pp1_arr=fltarr(ns)
pp2_arr=fltarr(ns)
pp3_arr=fltarr(ns)
br1_arr=fltarr(ns)
br3_arr=fltarr(ns)
tt1_arr=fltarr(ns)

k=0l
t1=fix(ctim3[0]/dt)*dt &  t2=t1+dt
while t2 le ctim3[nn-1] do begin
    ii=where(ctim3 ge t1 and ctim3 lt t2, count)
    if count ne 0 then begin
      if frame_select then begin
        pp1=fltarr(count)
        br1=fltarr(count)
        for i=0,count-1 do begin
        ;print,cfi3[ii[i]]   ; add 2012.09.14 sm
	  img1=read_tiff(cfi3[ii[i]],sub_rect=[0,600-4,1600,8])
	   br1[i]=mean(img1)
	 	  if br1[i] gt dark_level then begin
		  dark_check[ii[i]]=1
		  endif else begin 
		  img0=read_tiff(cfi3[ii[i]],/unsigned)
		  if mean(img0) gt dark_level then dark_check[ii[i]]=1
		  endelse
          pp1[i]=imgpsd1(img1)
        endfor
        dmy=max(pp1,i0)
        dmy2=min(pp1)
        dmy3=mean(pp1)
        dmy4=mean(br1)
      endif else begin
        i0=0
        dmy=-1.0 & dmy2=-1 & dmy3=-1
      endelse
      img1=read_tiff(cfi3[ii[i0]],/unsigned)
      imgsize,img1,nx,ny        
      if mean(img1) gt dark_level then begin
        iis[k]=ii[i0]        
        pp1_arr[k]=dmy
        pp2_arr[k]=dmy2
        pp3_arr[k]=dmy3
        br1_arr[k]=br1[i0]
        br3_arr[k]=dmy4
	tt1_arr[k]=t1+dt/2.
        print,cfi3[iis[k]],' /  ',count
  ;      wset,0
   ;     tvscl,rebin(img1,nx/2,ny/2)
   ;     xyouts,10,10,cfi3[iis[k]],/dev
        k=k+1
      endif
    endif
;;    stop
    t1=t2 & t2=t1+dt
endwhile
iis=iis[0:k-1]
pp1_arr=pp1_arr[0:k-1]
pp2_arr=pp2_arr[0:k-1]
pp3_arr=pp3_arr[0:k-1]
br1_arr=br1_arr[0:k-1]
br3_arr=br3_arr[0:k-1]
tt1_arr=tt1_arr[0:k-1]


ns=n_elements(iis)
cfi4=cfi3[iis]
hfi4=hfi3[iis]
ctim4=fname2msec(cfi4,ip)
htim4=fname2msec(hfi4,ip)

dok=where(dark_check eq 1)
cfi2=cfi3[dok]
hfi2=hfi3[dok]

save,cfi2,hfi2,file=pair_nodark_filename
save,cfi4,hfi4,pp1_arr,pp2_arr,pp3_arr,br1_arr,br3_arr,tt1_arr,nn_file_log, $
	file=pair_1s_filename

;-------------------------------------------------------

cfi3=cfi4
hfi3=hfi4
ctim3=fname2msec(cfi3,ip)
htim3=fname2msec(hfi3,ip)

nn=n_elements(cfi3)
dt=dt2

dur=ceil(ctim3[nn-1]/1000.)*1000.-fix(ctim3[0]/1000.)*1000.
ns=ceil(dur/dt)
iis=lonarr(ns)
pp2_arr=fltarr(ns)

k=0l
t1=fix(ctim3[0]/dt)*dt &  t2=t1+dt
while t2 le ctim3[nn-1] do begin
    ii=where(ctim3 ge t1 and ctim3 lt t2, count)
    if count ne 0 then begin
      if frame_select then begin
        pp1=fltarr(count)
        for i=0,count-1 do begin
        pp1[i]=pp1_arr[ii[i]]
        endfor
        dmy=max(pp1,i0)
      endif else begin
        i0=0
        dmy=-1.0
      endelse
        iis[k]=ii[i0]        
        pp2_arr[k]=dmy
        print,cfi3[iis[k]],' /  ',count
        k=k+1
    endif
;;    stop
    t1=t2 & t2=t1+dt
endwhile
iis=iis[0:k-1]
pp2_arr=pp2_arr[0:k-1]

ns=n_elements(iis)
cfi4=cfi3[iis]
hfi4=hfi3[iis]

print,ns,' files selected'
save,cfi4,hfi4,pp2_arr,file=selected_filename

;-----  copy selected files ------
for i=0,ns-1 do begin
  spawn,'copy '+cfi4[i]+' '+seldir,/hide
  spawn,'copy '+hfi4[i]+' '+seldir,/hide
  print,i,' / ',ns,'    ',cfi4[i],'   ',hfi4[i]
endfor


;-------------

if keyword_set(event) then begin


restore,pair_1s_filename
restore,pair_nodark_filename
ip=strpos(cfi2[0],'.tif',/reverse_search)-10
ctim4=fname2msec(cfi4,ip)
htim4=fname2msec(hfi4,ip)
ctim2=fname2msec(cfi2,ip)
htim2=fname2msec(hfi2,ip)
evdir=dirout+'events\'+day


  spawn,'mkdir '+evdir
  imgsize,event,n2,nev
  for k=0,nev-1 do begin
    event1=event[*,k]
    etim1=(strmid(event1[0],0,2)*3600l+strmid(event1[0],3,2)*60l-360l)*1000l
    etim2=(strmid(event1[0],0,2)*3600l+strmid(event1[0],3,2)*60l-120l)*1000l
;    etim3=(strmid(event1[1],0,2)*3600l+strmid(event1[1],3,2)*60l+360l)*1000l ; 2013.6.21 AO
    etim3=(strmid(event1[1],0,2)*3600l+strmid(event1[1],3,2)*60l+60l)*1000l
    etim4=(strmid(event1[2],0,2)*3600l+strmid(event1[2],3,2)*60l+120l)*1000l
    iie=where(ctim4 gt etim1 and ctim4 lt etim2, numiie) ; 2012.04.16. SM
    IF numiie GT 0 THEN BEGIN ; 2012.04.16. SM
      cfi5=cfi4[iie]
      hfi5=hfi4[iie]
      nne=n_elements(cfi5)
      print,nne,'  files in preflare phase'
      for i=0l,nne-1 do begin
        spawn,'copy '+cfi5[i]+' '+evdir,/hide
        spawn,'copy '+hfi5[i]+' '+evdir,/hide
        print,i,' /',nne,'   ',cfi5[i],'   ',hfi5[i]
      endfor
    ENDIF ; 2012.04.16.  SM
    iie=where(ctim2 gt etim2 and ctim2 lt etim3)
    cfi5=cfi2[iie]
    hfi5=hfi2[iie]
    nne=n_elements(cfi5)
    print,nne,'  files in impulsive phase'
    for i=0l,nne-1 do begin
      spawn,'copy '+cfi5[i]+' '+evdir,/hide
      spawn,'copy '+hfi5[i]+' '+evdir,/hide
      print,i,' /',nne,'   ',cfi5[i],'   ',hfi5[i]
    endfor
    iie=where(ctim4 gt etim3 and ctim4 lt etim4)
    cfi5=cfi4[iie]
    hfi5=hfi4[iie]
    nne=n_elements(cfi5)
    print,nne,'  files in decay phase'
    for i=0l,nne-1 do begin
      spawn,'copy '+cfi5[i]+' '+evdir,/hide
      spawn,'copy '+hfi5[i]+' '+evdir,/hide
      print,i,' /',nne,'   ',cfi5[i],'   ',hfi5[i]
    endfor
  endfor
endif


  

  


end
  
