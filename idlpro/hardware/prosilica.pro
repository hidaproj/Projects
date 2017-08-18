; prosilica.pro
;�@
cd,'C:\Projects\cprog\VS2005\prosilica'
dllfile='C:\Projects\cprog\VS2005\prosilica\Debug\prosilica.dll'
;r=call_external(dllfile,'test',/all_value,/cdecl)




r=call_external(dllfile,'CamOpen', 'Wooo', /PORTABLE )


expo=2000  &  gain=0  &  integ=2  &  ny=600  &  nx=800

;outfile_path='C:\data\dst\20090205\'
;fnam='he_'

p={prosilica_param, $
	expo:		expo,		$		; exposure time [msec]
	gain:		gain,		$		; gain 0�`28
	nimg:		integ, 		$		;  # of image 
	binx:		2, 		$		; Binning X 1�`8
	biny:		2, 		$		; Binning Y 1�`1200
	Height:		ny,	 	$		; Height  max=1200 (biny=1)
	Width:		nx,	 	$		; Width  max=1600 (binx=1)
	RegionX:	0, 		$		; start of region read out,pixel,left edge
	RegionY:	0, 		$		; start of region read out,pixel,top edge
	clock: 		79861111l, 	$		; TimeStanmpFrequency [Hz]
	timelo:		0,		$		; Time stamp, lower 32-bits
	timehi:		0,		$		; Time stamp, upper 32-bits
	n_evsample: 	0l 		$		; omake
	}

img=intarr(p.Width,p.Height,p.nimg)
img0=intarr(p.Width,p.Height)
timehi=intarr(p.nimg)
timelo=intarr(p.nimg)

r=call_external(dllfile,'CamInit',/all_value,/cdecl)
r=call_external(dllfile,'SetParam',p.expo,p.gain,p.binx,p.biny,p.Width,p.Height,p.RegionX,p.RegionY,/all_value,/cdecl)
r=call_external(dllfile,'GrabImg',p.nimg,/all_value,/cdecl)
for i=0,p.nimg-1 do begin
	r=call_external(dllfile,'DivBuf',i,/all_value,/cdecl)
	;r=call_external(dllfile,'GoIdl2',long(i),img0)
	r=call_external(dllfile,'GoIdl',img0,/cdecl)
	img[*,*,i]=img0
	
	timehi[i]=call_external(dllfile,'TimestampHi',i,/all_value,/cdecl)
	timelo[i]=call_external(dllfile,'TimestampLo',i,/all_value,/cdecl)
endfor
r=call_external(dllfile,'writeimage',/all_value,/cdecl)
r=call_external(dllfile,'CamFin',/all_value,/cdecl)


window,1,xsize=p.Width,ysize=p.Height
tvscl,img[*,*,0]

;.r C:\anan\testpro\prosilica\pro_init.pro
;.r C:\anan\testpro\prosilica\pro_prev.pro
;.r C:\anan\testpro\prosilica\pro_get.pro
;.r C:\anan\testpro\prosilica\pro_fin.pro
end
