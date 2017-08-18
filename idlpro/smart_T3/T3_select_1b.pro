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

pro T3_select_2,day

dir='E:\T3\'

pair_filename=dir+'selected\'+day+'\'+day+'_pair0_files.sav'

seldir=dir+'selected\'+day

restore,pair_filename


dt=5*1000l  ; time interval for selection, msec
frame_select=1
;frame_select=0

dark_level=800
selected_filename=dir+'selected\'+day+'\'+day+'_selected_files.sav'
pair_filename=dir+'selected\'+day+'\'+day+'_pair1_files.sav'

seldir=dir+'selected\'+day

;-----  select dt cadence ------
nn=n_elements(cfi3)
print,nn,' pairs (dt<6msec) counted'
dark_check=intarr(nn)

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
		  if mean(img0) gt dark_level then dark_cehck[ii[i]]=1
		  endelse
          imgb=rebin(img1,nxb2,nyb2)
          av=mean(imgb)
          rms1[i]=total((imgb-av)^2)
          tvscl,imgb
        endfor
        dmy=max(rms1,i0)
      endif else begin
        i0=0
        dmy=-1.0
      endelse
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

dok=where(dark_check eq 1)
cfi2=cfi3[dok]
hfi2=hfi3[dok]

save,cfi2,hfi2,file=pair_filename


print,ns,' files selected'
save,cfi4,hfi4,rms1_arr,file=selected_filename

;-----  copy selected files ------
for i=0,ns-1 do begin
  spawn,'copy '+cfi4[i]+' '+seldir,/hide
  spawn,'copy '+hfi4[i]+' '+seldir,/hide
  print,i,' / ',ns,'    ',cfi4[i],'   ',hfi4[i]
endfor


return

end
  
