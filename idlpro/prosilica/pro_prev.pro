pro pro_prev,expo,gain
;   preview images from Prosilica camera
;	2009.8.30	T.A.

;expo=100	; msec
;gain=28
nx=800 &	ny=600


;===============================;
cd,'C:\Projects\cprog\VS2005\prosilica'
dllfile='C:\Projects\cprog\VS2005\prosilica\Debug\prosilica.dll'
integ=1
;======= param =================;

p={prosilica_param, $
	expo:		expo,		$		; exposure time [msec]
	gain:		gain,		$		; gain 0Å`28
	nimg:		integ, 		$		;  # of image 
	binx:		2, 		$		; Binning X 1Å`8
	biny:		2, 		$		; Binning Y 1Å`1200
	Height:		ny,	 	$		; Height  max=1200 (biny=1)
	Width:		nx,	 	$		; Width  max=1600 (binx=1)
	RegionX:	0, 		$		; start of region read out,pixel,left edge
	RegionY:	0, 		$		; start of region read out,pixel,top edge
	clock: 		79861111l, 	$		; TimeStanmpFrequency [Hz]
	timelo:		0,		$		; Time stamp, lower 32-bits
	timehi:		0,		$		; Time stamp, upper 32-bits
	n_evsample: 	0l 		$		; omake
	}

r=call_external(dllfile,'SetParam',p.expo,p.gain,p.binx,p.biny,p.Width,p.Height,    $
				p.RegionX,p.RegionY,/all_value,/cdecl)

;---------- start cap ----------;
img1=intarr(nx,ny)
ans=''

;Create a base widget. 
base1 = WIDGET_BASE(title='Prosilica',/column)

xpdmenu,['/EXIT/'],base1,/frame,title='pro_prev'

;Attach a 256 x 256 draw widget. 
draw = WIDGET_DRAW(base1, XSIZE = nx, YSIZE = ny) 
 
;Realize the widgets. 
WIDGET_CONTROL, /REALIZE, base1

;Obtain the window index. 
WIDGET_CONTROL, draw, GET_VALUE = index 

;Set the new widget to be the current graphics window 

uvalue=''
	while (uvalue ne "EXIT") do begin
		WSET, index 
		r=call_external(dllfile,'GrabImg',p.nimg,/all_value,/cdecl)
		r=call_external(dllfile,'DivBuf',0,/all_value,/cdecl)
		r=call_external(dllfile,'GoIdl',img1,/cdecl)
		tvscl,img1
		event = widget_event(base1,/nowait)
		if event.id ne 0 then $
		WIDGET_CONTROL, get_uvalue=uvalue, event.id
	endwhile

WIDGET_CONTROL, /destroy, base1

end
