; plot_mm_cal.pro
; 2012.5.28

dir='C:\data\TF\Luceo_WP\'
files=['HI-Retax1.1_20120528_MM','HI-Retax1.0_20120406_MM']+'.sav'
nf=n_elements(files)

window,xs=800,ys=600

restore,dir+files[0]	; -> wl,mm,s

plot,wl,s.ret/!pi*180,xtitle='wavelength (nm)',xrange=[400,1100],xstyle=1, $
	charsize=1.5,ytitle='retardation (deg.)'

for i=1,nf-1 do begin
	restore,dir+files[i]
	oplot,wl,s.ret/!pi*180.,line=i
endfor


end
