; sqfilter.pro
function sqfilter,dat,xm,ym,il
;  Sqrt filter to reduce the bit depth 
;  dat[*]	- data
;  xm		- max of data, ex. 2^10-1
;  ym		- max of filtered data, ex. 2^8-1
;  il		- linear range
;	2007.8.7	k.i.

;xm=2^10-1 &	ym=2^8-1 &	il=120
;dat=findgen(xm)

if ym ge xm then return,dat

xmf=float(xm) &	ymf=float(ym) &	ilf=float(il)
b=(4.*ilf*xmf-ymf^2-2*ymf*ilf-ilf^2)/4/(xmf-ymf)
c=2*b-il
a=2*sqrt(ilf-b)
;;print,a,b,c

dat2=dat
ii=where(dat le il, count) &	if count ne 0 then dat2[ii]=dat[ii]
ii=where(dat gt il, count) &	if count ne 0 then dat2[ii]=a*sqrt(dat[ii]-b)+c

return,dat2

end
