; prosilica.pro
;Å@
cd,'C:\Projects\cprog\VS2005\prosilica'
dllfile='C:\Projects\cprog\VS2005\prosilica\Debug\prosilica.dll'

time=call_external(dllfile,'getime',/all_value,/cdecl)
print,time

expo=100  &  gain=20  &  integ=5  &  ny=600  &  nx=800

;outfile_path='C:\data\dst\20090205\'
;fnam='he_'

p={prosilica_param, $
	expo:		expo,		$		; exposure time [msec]
	gain:		gain,		$		; gain 0Å`28
	nimg:		integ, 		$		;  # of image
	binx:		2l, 		$		; Binning X 1Å`8
	biny:		2l, 		$		; Binning Y 1Å`1200
	Height:		ny,	 	$		; Height  max=1200 (biny=1)
	Width:		nx,	 	$		; Width  max=1600 (binx=1)
	RegionX:	0l, 		$		; start of region read out,pixel,left edge
	RegionY:	0l, 		$		; start of region read out,pixel,top edge
	clock: 		79861111l, 	$		; TimeStanmpFrequency [Hz]
	timelo:		0l,		$		; Time stamp, lower 32-bits
	timehi:		0l,		$		; Time stamp, upper 32-bits
	n_evsample: 	0l 		$		; omake
	}

img=intarr(p.Width,p.Height,p.nimg)
img0=intarr(p.Width,p.Height)
timehi=intarr(p.nimg)
timelo=intarr(p.nimg)
stamp=fltarr(p.nimg)

r=call_external(dllfile,'CamInit',/all_value,/cdecl)
r=call_external(dllfile,'SetParam',p.expo,p.gain,p.binx,p.biny,p.Width,p.Height,p.RegionX,p.RegionY,/all_value,/cdecl)
;r=call_external(dllfile,'TTest',img,/cdecl)
r=call_external(dllfile,'GrabImg',p.nimg,/all_value,/cdecl)
print,'ok'
;for i=0,p.nimg-1 do begin
;	r=call_external(dllfile,'DivBuf',i,/all_value,/cdecl)
;	;r=call_external(dllfile,'GoIdl2',long(i),img0)
;	r=call_external(dllfile,'GoIdl',img0,/cdecl)
;	img[*,*,i]=img0
;endfor

;=========================== Make Fits File Test ==========================;
caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
yyyymmdd_hhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
	+'_'+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')
print,'ok'
outfile='C:/anan/test/prosilica/'+yyyymmdd_hhmmss+'.fits'
	print,outfile
r=call_external(dllfile,'openfits',outfile,/all_value,/cdecl)
	print,r
r=call_external(dllfile,'WriteImage',p.nimg,/all_value,/cdecl)
	print,r
;r=call_external(dllfile,'WriteTimeStamp',p.nimg,/all_value,/cdecl)  &  	print,r
r=call_external(dllfile,'addkeywords_prosilica',			$
				'NORMAL',	$
				'10830',	$
				'111',		$
				'1_1',		$
				'',		$
				'22',		$
				'33',		$
				'44',		$
				'?',		$
				'??',		$
				'pro_obs',	$
				/all_value,/cdecl)
	print,r
r=call_external(dllfile,'closefits',/all_value,/cdecl)
	print,r

;=========================== Time Stamp Test ==========================;
;stamp=(timehi*4294967296+timelo)*1./79861111.	;[sec]

;caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
;yyyymmdd_hhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
;		+'_'+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')

;outfile='C:/anan/test/prosilica/stamp'+yyyymmdd_hhmmss+'.fits'
;print,outfile
;r=call_external(dllfile,'TimeStamp',outfile,p.nimg,/all_value,/cdecl)
;	print,r
;======================================================================;

r=call_external(dllfile,'CamFin',/all_value,/cdecl)


window,1,xsize=p.Width,ysize=p.Height
tvscl,img[*,*,0]


;.r C:\anan\testpro\prosilica\pro_init.pro
;.r C:\anan\testpro\prosilica\pro_prev.pro
;.r C:\anan\testpro\prosilica\pro_get.pro
;.r C:\anan\testpro\prosilica\pro_fin.pro




end
