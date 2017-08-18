pro pol_level0,file,imgs,hd,dt,rtwv

;file='/dst_arch/vs/2010/20100123/ref0_ca20100123_160307850.fits'

dir=file_dirname(file)+'/'

;filelen=strlen(file)
;dpos0=strpos(file,'/')
;if dpos0 eq -1 then begin
;    dir=''
;endif else begin
;    dpos=dpos0
;    file1=file
;    while dpos0 ne -1 do begin
;        file1=strmid(file,dpos+1,filelen)
;        dpos0=strpos(file1,'/')
;        if dpos0 ne -1 then dpos=dpos0+dpos+1
;    endwhile
;endelse
;dir=strmid(file,0,dpos+1)

fits_pos=strpos(file,'.fits')
time=strmid(file,fits_pos-18,18)
;    print,time
stmpfile=file_search(dir,'stmp'+time+'.fits')
pltfile=file_search(dir,'wvplt'+time+'.sav')
mreadfits,stmpfile,h,dt
restore,file=pltfile
    
mreadfits,file,hd,imgs0
imgs=float(imgs0)


end
