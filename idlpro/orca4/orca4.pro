@orcalib

;  2-13.6.10 	k.i.	orcaprev






;**************************************************************


p=orcainit()


stop

expo=0.001 &	bin=1
p=OrcaSetParam(expo=expo,bin=bin)


orcaprev,p,img=img


stop

nimg=3
imgs=OrcaObs(nimg=nimg)


stop

filename='c:\tmp\test1.raw'
nimg=3
dmy=orcaobs(file=filename,nimg=nimg)


img=intarr(p.Width,p.Height,nframe)
openr,1,filename &	readu,1,img &	close,1


stop

r=call_external(orcadll,'DivBuf',0,/all_value,/cdecl)
r=call_external(orcadll,'GoIdl',img,/cdecl)
;	img[*,*,*] store only 1 image even if nimg>1

orcafin

end
