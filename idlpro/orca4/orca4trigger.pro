@orcalib


;**************************************************************

nimg=100
expo=0.001	;s
bin=4
outfile='G:\data\20140506\dark_b4exp001.sav'


p=orcainit()
p=OrcaSetParam(expo=expo,bin=bin)
settriggermode
settriggerpolarity

CALDAT, systime(/JULIAN), Month, Day, Year,h,m1,s1

imgs=OrcaObs(nimg=nimg)

CALDAT, systime(/JULIAN), Month, Day, Year,h,m2,s2

orcafin

window,2,xs=400,ys=400
imgs1=congrid(imgs,400,400,nimg)
for i=0,nimg-1 do begin
	tvscl,imgs1[*,*,i]
	wait,0.01
endfor

print,'time',(m2*60+s2)-(m1*60+s1)


;save,imgs,file='E:\data\20131031\4b0100ms4p_10.sav'
save,nimg,expo,bin,imgs,file=outfile



end