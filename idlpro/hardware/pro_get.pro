; pro_get.pro
;   record burst images from Prosilica camera
;	2009.8.30	T.A.

cd,'C:\Projects\cprog\VS2005\prosilica'
dllfile='C:\Projects\cprog\VS2005\prosilica\Debug\prosilica.dll'

;-----------------------------------------------------------------------
outfile_path='C:\anan\test\prosilica\'
fnam='pro_'

;dstangl,hangl,zd
p={prosilica_param, $
	expo:		1,		$		; exposure time [msec]
	gain:		28,		$		; gain 0Å`28
	nimg:		100, 		$		;  # of image 
	binx:		2, 		$		; Binning X 1Å`8
	biny:		2, 		$		; Binning Y 1Å`1200
	Height:		400,	 	$		; Height  max=1200 (biny=1)
	Width:		400,	 	$		; Width  max=1600 (binx=1)
	RegionX:	300, 		$		; start of region read out,pixel,left edge
	RegionY:	300, 		$		; start of region read out,pixel,top edge
	clock: 		79861111l, 	$		; TimeStanmpFrequency [Hz]
	timelo:		0,		$		; Time stamp, lower 32-bits
	timehi:		0,		$		; Time stamp, upper 32-bits
	n_evsample: 	0l 		$		; omake
	}

r=call_external(dllfile,'SetParam',p.expo,p.gain,p.binx,p.biny,p.Width,p.Height,p.RegionX,p.RegionY,/all_value,/cdecl)

;-----------------------------------------------------------------------

window,0,xs=p.Width,ys=p.Height
;---------- start cap ----------;
img=intarr(p.Width,p.Height,p.nimg)
img0=intarr(p.Width,p.Height)
ans=''

print,'Start?' &	read,ans
caldat,systime(/JULIAN), mon , day , year , hour , minu , seco

r=call_external(dllfile,'GrabImg',p.nimg,/all_value,/cdecl)

h=p
for i=0,p.nimg-1 do begin
	r=call_external(dllfile,'DivBuf',i,/all_value,/cdecl)
	r=call_external(dllfile,'GoIdl',img0,/cdecl)
	img[*,*,i]=img0
	
	p.timehi=call_external(dllfile,'TimestampHi',i,/all_value,/cdecl)
	p.timelo=call_external(dllfile,'TimestampLo',i,/all_value,/cdecl)
	if i ne 0 then h=[h,p]
endfor

;================================== 
for i=0,p.nimg-1 do begin
	tvscl,img(*,*,i)
	xyouts,10,10,string(i),/dev,size=3
endfor

yyyymmdd_hhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
	+'_'+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')
outfile=outfile_path+fnam+yyyymmdd_hhmmss+'.fits'

save,h,img,file=outfile
;mwrite_fits,h,img,outfile=outfile

;================================== 


end
