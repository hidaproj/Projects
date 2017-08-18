@orcalib


;**************************************************************

nimg=1
expo=2.
bin=4



stop
p=orcainit()
stop
p=OrcaSetParam(expo=expo,bin=bin)
stop
;settriggermode
;settriggerpolarity
orca_setoutputtrigmode,'Programable',delay=1.,period=10.,polarity=1

CALDAT, systime(/JULIAN), Month, Day, Year,h,m1,s1

for i=0,3 do imgs=OrcaObs(nimg=nimg)

CALDAT, systime(/JULIAN), Month, Day, Year,h,m2,s2

orcafin

window,2,xs=2048/bin,ys=2048/bin
for i=0,nimg-1 do begin
	tvscl,imgs[*,*,i]
	wait,0.1
endfor

print,'time',(m2*60+s2)-(m1*60+s1)


;save,imgs,file='E:\data\20131031\4b0100ms4p_10.sav'
;save,imgs,file='G:\data\20140502\test.sav'



end