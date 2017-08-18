@orcalib


;**************************************************************

p=orcainit()

expo=0.1 &	bin=2
p=OrcaSetParam(expo=expo,bin=bin)

;nimg=3
nimg=1
wdef,0,2048/bin,2048/bin
wdef,2,640,512
for i=0,100 do begin
	imgs=OrcaObs(nimg=nimg)
	wset,0
	tvscl,imgs[*,*,0]
	wset,2
	s=size(imgs)
	plot,imgs[s[1]/2,*],/xstyle
	img=0
endfor

orcafin

end