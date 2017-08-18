;++++++++++++++++++++++++++++++++++++++++++++++++++++++
function value_vsdst
;--------------------------------
common vsdst,dd,aa0,bb0,colphi,ff,wwspectrum,wwgrating

dd=1e7 /632.		; constant of grooves [A/groove]
aa0=1.219*!dtor         ; [rad]
bb0=-1.688*!dtor        ; [rad]
colphi=261.		; diameter of collimated light [mm]
ff=14000.		; focal length [mm]
wwspectrum=10.;500	; width of spctrum image [mm]??
wwgrating=408.		; width of grating [mm]
ba=15.*!dtor

return,''
END
;++++++++++++++++++++++++++++++++++++++++++++++++++++++
;pro info_set,ng,ba,aa0d,bb0d,colphi,ff,wwspectrum
;input
;       ng: number of grooves [#/mm]
;       aa0d: [deg]
;       bb0d: [deg]
;       
;	ga: grating angle [deg]
;	mm: order
;--------------------------------
;++++++++++++++++++++++++++++++++++++++++++++++++++++++
function ga2wl,ga,mm
;input
;	ga: grating angle [deg]
;	mm: order
;output
;	wl: wavelength [A]
;--------------------------------
common vsdst

aa=ga*!dtor+aa0
bb=ga*!dtor+bb0
res=dd/float(mm) *(sin(aa)+sin(bb)) 

return,res
END
;++++++++++++++++++++++++++++++++++++++++++++++++++++++
function wl2ga,wl,mm
;input
;	wl: wavelength [A]
;	mm: order
;output
;	ga: grating angle [deg]
;--------------------------------
common vsdst

th=mm*wl/dd/2./cos((aa0-bb0)/2.)
res=(asin(th)-((aa0+bb0)/2.))*!radeg

return,res
END
;++++++++++++++++++++++++++++++++++++++++++++++++++++++
function resolutionpower,wl,mm
;input
;	wl: wavelength [A]
;	mm: order
;output
;	resolution power [A]
;--------------------------------
common vsdst

ga=wl2ga(wl,mm)
aa=ga*!dtor+aa0
nn=((colphi/cos(aa))<wwgrating) *(1e7 /dd)
;nn=400.*(1e7 /dd)
l_dl=nn*mm/1.22
res=wl/l_dl

;print,'w/dw='+string(l_dl)
return,res
END
;++++++++++++++++++++++++++++++++++++++++++++++++++++++
function resolution,wl,mm
;input
;	wl: wavelength [A]
;	mm: order
;output
;	resolution [mm/A]
;--------------------------------
common vsdst

ga=wl2ga(wl,mm)
bb=ga*!dtor+bb0
res0=mm/dd/cos(bb)
res=ff*res0

return,res
END
;++++++++++++++++++++++++++++++++++++++++++++++++++++++
function widthspectrum,wl,mm
;input
;	wl: wavelength [A]
;	mm: order
;output
;	widthspectrum [A]
;coment
;       about value
;--------------------------------
common vsdst

res=wwspectrum/resolution(wl,mm)

return,res
END
;++++++++++++++++++++++++++++++++++++++++++++++++++++++
function intensity,ba,wl,mm
;--------------------------------
common vsdst

ga=wl2ga(wl,mm)
aa=ga*!dtor+aa0
bb=ga*!dtor+bb0
nn=((colphi/cos(aa))<wwgrating) *(1e7 /dd)
xa=!pi/wl*dd* cos(ba*!dtor)^2 *cos(0.)/cos(ba*!dtor)*$
	(sin(aa-ba*!dtor)+sin(bb-ba*!dtor))
yy=!pi /wl*dd* cos(0.)* (sin(aa)+sin(bb))
res=(sin(xa)/xa)^2 ;* (sin(nn*yy)/sin(yy))^2

;print,xa,yy,nn

return,res
END






