;  tiff plot
;  2010.3.20	k.i.	for use browsing of streampix output

dir=''
file=dialog_pickfile(path= dir, title='Tiff');,/multi)
dwh=10
sp=read_tiff(file)
imgsize,sp,nx,ny
prof=transpose(rebin(sp[nx/2-dw:nx/2+dw,*],1,ny))
plot,prof



end
