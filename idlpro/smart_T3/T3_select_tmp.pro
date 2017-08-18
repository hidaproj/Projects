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
dir='E:\T3\'
day='20111231'

start_time=''

seldir=dir+'selected\'+day

dt1=1*1000l  ; time interval for selection, msec
dt2=5*1000l

frame_select=1
;frame_select=0
cd,dir+day
dark_level=800
selected_filename=dir+'selected\'+day+'\'+day+'_selected_files.sav'
pair_all_filename=dir+'selected\'+day+'\'+day+'_pair_files_all.sav'
pair_1s_filename=dir+'selected\'+day+'\'+day+'_pair_files_1s.sav'

restore,pair_1s_filename

;-------------------------------------------------------
ip=18

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



  


end
  
