@spemirh
dir='/work/TF/20130928/'
sdir='/work/hagino/TF/20130928/'
readtxt,dir+'log.txt',ll,nn

   winf='C:\Projects\data\block\block7\20130928\'

files=''
for j=0,nn-1 do begin
   seq=strsplit(ll[j],/ex)
   if seq[1] eq 'data50set' then begin
      file=(str_sep(seq[0],winf))[1]
      files=[files,file]
   endif
;   if seq[1] eq 'flat' then begin
;      ftime0=(str_sep(seq[0],winf+'block7_20130928_'))[1]
;      ftime0=(str_sep(ftime0,'.dat'))[0]
;      ftime1='2013/09/28 '+ftime0
;      print,ftime1
;   endif
endfor

ftime1_1=anytim2tai('2013/09/28 101102')
ftime1_2=anytim2tai('2013/09/28 101247')

ftime2_1=anytim2tai('2013/09/28 105801')
ftime2_1=anytim2tai('2013/09/28 110035')

restore,dir+'flat.dat'
restore,dir+'dark.dat'

files=files[1:n_elements(files)-1]
nfile=n_elements(files)
   wdef,0

for j=3,nfile-1 do begin
   restore,dir+files[j]
img0=img0[*,*,0:4]
   ss=size(img0)
   data=img0
   atim=anytim2tai(f.time)
   if atim lt ftime2_1 then flat=flat1 else flat=flat2
   drk=dark[*,*,(where(dexp eq g.exp))[0]]

   for i=0,ss[3]-1 do data[*,*,i]=(img0[*,*,i]-drk)/flat
   tvscl,rebin(data[*,*,0],512,512)
;w_png,sdir+'dataset50_0_'+string(j+1,form='(i3.3)')+'.png'

   p=spemir_st()
   p.D=600.;　　　　口径(mm)　@DST
   p.Dco=210.;　　　中心遮蔽(mm)　@DST
   p.pix1=0.1;　　　arcsec/pix
   p.w=512;　　　　セグメントサイズ(pix)
   p.wl=656.3;　　　波長(nm)

   rimg0=spemirh(data,p=p,/deconv $
                 ,avimg=avimg,saimg=saimg,rimg=rimg,consis=consis,istf=istf)
wdef,0,512,512
rr=round(max(rimg0)/100)*100
tv,cscale(rebin(rimg0,512,512),drange=rr+[-900,0])
tit='data50_'+string(j+1,form='(i3.3)')+' '$
             +f.time+' !4k!3='+strtrim(string(f.wl0,form='(f6.2)'),2)+'A'
     xyouts,10,10,tit,/dev,charsize=2,color=255
stop
w_png,sdir+'dataset50_'+string(j+1,form='(i3.3)')+'_512.png'
save,filename=sdir+'dataset50_'+string(j+1,form='(i3.3)')+'_512.sav',p,rimg0,f
stop
endfor

end
