; gif2isv.pro
dir='d:\nogis\000525\movie\'
;dir='h:\000529_0\movie\'
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
imgs=bytarr(nx2,ny2,nn)

for i=0,nn-1 do begin
	print,dir+files(i)
	read_gif,dir+files(i),img
	img=rebin(img(0:nx2*nbin-1,0:ny2*nbin-1),nx2,ny2)
	imgs(*,*,i)=img
endfor
xmovie,imgs,/fit

end

