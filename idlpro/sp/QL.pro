;---------------;
dir='C:/data/dst/20101006/'
files=file_search(dir,'ac_01_*.fits',count=nf)
rtrd=105.
nx=200
ny=1200
;yr=[0,ny-1]
ys=200
yw=400
xx=51

nn=[0,1]
;---------------;
print,files

i=0
if nn[0] then mkiquv_00,files[i],rtrd,iquv,iquverr,hd,amp

if nn[1] then begin
	iquv=fltarr(nx,ny,4)
	for i=0,nf-1 do begin
		mkiquv_00,files[i],rtrd,iquv0,iquverr,hd,amp
		iquv=iquv+iquv0/float(nf)
	endfor
endif

wdef,0,800,400
!p.multi=[0,4,1]
tvscl,iquv[*,ys:(ys+400)<(ny-1),0],0
tvscl,iquv[*,ys:(ys+400)<(ny-1),1],1
tvscl,iquv[*,ys:(ys+400)<(ny-1),2],2
tvscl,iquv[*,ys:(ys+400)<(ny-1),3],3
!p.multi=0
;write_png,'C:\anan\iquv20100929.png',tvrd()

wdef,1,800,400
!p.multi=[0,2,2]
plot,iquv[xx,ys:(ys+400)<(ny-1),0],title='I',ytitle='I',xtitle='wavelength [pix]'
plot,iquv[xx,ys:(ys+400)<(ny-1),1],title='Q/I',ytitle='Q/I',xtitle='wavelength [pix]'
plot,iquv[xx,ys:(ys+400)<(ny-1),2],title='U/I',ytitle='U/I',xtitle='wavelength [pix]'
plot,iquv[xx,ys:(ys+400)<(ny-1),3],title='V/I',ytitle='V/I',xtitle='wavelength [pix]'
!p.multi=0
;write_png,'C:\anan\iquv_prof20100929.png',tvrd()


end