;  prev.pro
@prosilicalib

n=1000		; # of image
expo=20000l	; exposure, usec

p=CamInit()

p=CamSetParam(expo=expo)
wait,0.01
window,xs=800,ys=600
for i=0,n-1 do begin
	img=Gigobs1()
	tvscl,rebin(img,800,600),order=1
endfor

end
