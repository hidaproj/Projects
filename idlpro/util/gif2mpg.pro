; gif2isv.pro
dir='c:\nogis\000525\tmp\'
;dir='h:\000529_0\movie\'
mpgfile='000525.mpg'
fnam='p2_'
seq=[0,300]
nbin=1


files=fnam+string(indgen(seq(1)-seq(0)+1)+seq(0),format='(i4.4)') $
	+'.gif'
nn=n_elements(files)
read_gif,dir+files(0),img
imgsize,img,nx,ny

if nbin ne 1 then begin
	nx2=nx/nbin
	ny2=ny/nbin
endif else begin
	nx2=nx
	ny2=ny
endelse
mID=mpeg_open([nx,ny])
for i=0,nn-1 do begin
	print,dir+files(i)
	read_gif,dir+files(i),img
	img=rebin(img(0:nx2*nbin-1,0:ny2*nbin-1),nx2,ny2)
	tv,img,7
	mpeg_put,mID,image=rotate(img,7),frame=i
endfor
mpeg_save,mID,filename=mpgfile
mpeg_close,mID

end
