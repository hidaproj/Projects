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
;dir='E:\T3\'
;day='20120327'

event=''
;event=['17:17','17:25','17:30']
;event=[['07:47','07:55','08:00'],['10:50','10:53','10:56'],['11:01','11:02','11:20']]
start_time=''

seldir=dir+'selected\'+day

dt1=1*1000l  ; time interval for selection, msec
dt2=5*1000l

frame_select=1
;frame_select=0

dark_level=800
selected_filename=dir+'selected\'+day+'\'+day+'_selected_files.sav'
pair_1s_filename=dir+'selected\'+day+'\'+day+'_pair_files_1s.sav'



pair_all_filename=dir+'selected\'+day+'\'+day+'_pair_files_all.sav'
pair_nodark_filename=dir+'selected\'+day+'\'+day+'_pair_files_nodark.sav'
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

dur=ctim3[nn-1]-ctim3[0]
ns=dur/dt
iis=lonarr(ns)
rms1_arr=fltarr(ns)
rms2_arr=fltarr(ns)
rms3_arr=fltarr(ns)

window,0,xs=800,ys=600
if frame_select then begin  ; frame selection
  jj=lindgen(nn/5000+1)*5000
  cfii=cfi3(jj)
  is=smenu(cfii,title='select an image')
  img1=read_tiff(cfii[is],/unsigned)
  imgsize,img1,nx,ny
  tvscl,rebin(img1,nx/2,ny/2)
;  print,'select box...'
;  box_cur1,x02, y02, nxb2, nyb2
;  x0=x02*2 &  y0=y02*2 &  nxb=nxb2*2 &  nyb=nyb2*2
  print,'%% Click the center position for frame selection'
  nxb2=80 & nyb2=40 & nxb=nxb2*2 &  nyb=nyb2*2
  cursor,x02,y02,/dev 
  x0=x02*2-nxb2 & y0=y02*2-nyb2
  window,2,xs=nxb2,ys=nyb2
endif 

k=0l
t1=ctim3[0] &  t2=t1+dt
while t2 le ctim3[nn-1] do begin
    ii=where(ctim3 ge t1 and ctim3 lt t2, count)
    if count ne 0 then begin
      if frame_select then begin
        wset,2
        rms1=fltarr(count)
        for i=0,count-1 do begin
          img1=read_tiff(cfi3[ii[i]],sub_rect=[x0,y0,nxb,nyb],/unsigned)
	 	  if mean(img1) gt dark_level then begin
		  dark_check[ii[i]]=1
		  endif else begin 
		  img0=read_tiff(cfi3[ii[i]],/unsigned)
		  if mean(img0) gt dark_level then dark_check[ii[i]]=1
		  endelse
          imgb=rebin(img1,nxb2,nyb2)
          av=mean(imgb)
          rms1[i]=total((imgb-av)^2)
          tvscl,imgb
        endfor
        dmy=max(rms1,i0)
        dmy2=min(rms1)
        dmy3=mean(rms1)
      endif else begin
        i0=0
        dmy=-1.0 & dmy2=-1 & dmy3=-1
      endelse
      img1=read_tiff(cfi3[ii[i0]],/unsigned)
      imgsize,img1,nx,ny        
      if mean(img1) gt dark_level then begin
        iis[k]=ii[i0]        
        rms1_arr[k]=dmy
        rms2_arr[k]=dmy2
        rms3_arr[k]=dmy3
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
rms2_arr=rms2_arr[0:k-1]
rms3_arr=rms3_arr[0:k-1]

ns=n_elements(iis)
cfi4=cfi3[iis]
hfi4=hfi3[iis]
ctim4=fname2msec(cfi4,ip)
htim4=fname2msec(hfi4,ip)

dok=where(dark_check eq 1)
cfi2=cfi3[dok]
hfi2=hfi3[dok]

save,cfi2,hfi2,file=pair_nodark_filename
save,cfi4,hfi4,rms1_arr,rms2_arr,rms3_arr,nn_file_log,file=pair_1s_filename

;-------------------------------------------------------

cfi3=cfi4
hfi3=hfi4
ctim3=fname2msec(cfi3,ip)
htim3=fname2msec(hfi3,ip)

nn=n_elements(cfi3)
dt=dt2

dur=ctim3[nn-1]-ctim3[0]
ns=dur/dt
iis=lonarr(ns)
rms2_arr=fltarr(ns)

k=0l
t1=ctim3[0] &  t2=t1+dt
while t2 le ctim3[nn-1] do begin
    ii=where(ctim3 ge t1 and ctim3 lt t2, count)
    if count ne 0 then begin
      if frame_select then begin
        rms1=fltarr(count)
        for i=0,count-1 do begin
        rms1[i]=rms1_arr[ii[i]]
        endfor
        dmy=max(rms1,i0)
      endif else begin
        i0=0
        dmy=-1.0
      endelse
        iis[k]=ii[i0]        
        rms2_arr[k]=dmy
        print,cfi3[iis[k]],' /  ',count
        k=k+1
    endif
;;    stop
    t1=t2 & t2=t1+dt
endwhile
iis=iis[0:k-1]
rms2_arr=rms2_arr[0:k-1]

ns=n_elements(iis)
cfi4=cfi3[iis]
hfi4=hfi3[iis]

print,ns,' files selected'
save,cfi4,hfi4,rms2_arr,file=selected_filename

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
evdir=dir+'events\'+day


  spawn,'mkdir '+evdir
  imgsize,event,n2,nev
  for k=0,nev-1 do begin
    event1=event[*,k]
    etim1=(strmid(event1[0],0,2)*3600l+strmid(event1[0],3,2)*60l-360l)*1000l
    etim2=(strmid(event1[0],0,2)*3600l+strmid(event1[0],3,2)*60l-60l)*1000l
    etim3=(strmid(event1[1],0,2)*3600l+strmid(event1[1],3,2)*60l+60l)*1000l
    etim4=(strmid(event1[2],0,2)*3600l+strmid(event1[2],3,2)*60l+60l)*1000l
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
  
