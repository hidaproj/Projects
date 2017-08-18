; R-T relation
; 2012.9.27  k.i.

file='C:\home\idlpro\OPTIC\narrowfilt\data\Thermister_44036.txt'
strs=rdcsv(file,sep='	',skip=2)
T=transpose(float(strs[0,*]))		; T (C)
R=transpose(float(strs[1,*]))/10000.	; R (10kOhm)

window,0
t2=1./(15+T)
plot,r,t2,xtitle='R (ohm)',ytitle='1./(15.+T)'
coef=poly_fit(r,t2,3)
t2f=poly(r,coef)
oplot,r,t2f,line=1

window,1
T_fit=1./t2-15
plot,r,T,xtitle='R (ohm)',ytitle='T (C)'
oplot,r,T_fit,line=2

window,2
plot,r,T-T_fit,line=0,xtitle='R (ohm)',ytitle='T-T_fit(C)'

print,coef

end
