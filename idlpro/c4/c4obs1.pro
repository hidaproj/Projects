;  c4obs1.pro
;	+++ 2013.10.16
@prosilicalib

p=CamInit()

n=20		; # of image
dt=1.		; time interval
expo=20000l	; exposure, usec
fnam='t'
outdir='C:\data\c4-2013\20131023\'


p=CamSetParam(expo=expo)
wait,0.01
window,xs=800,ys=600
for i=0,n-1 do begin
	print,i
	time1=gettime()
	img=Gigobs1()
	time2=gettime()
	wait,0.01
	tvscl,rebin(img,800,600),order=1
	xyouts,10,10,string(i,form='(i5.0)'),/dev,chars=1.5
	outfile=fnam+time1+'.sav'
	save,img,p,time1,time2,file=outdir+outfile
	;outfile=fnam+time1+'.tif'
	;write_tiff,outdir+outfile,img/2.^8
	wait,dt
endfor

;CamFin

end
