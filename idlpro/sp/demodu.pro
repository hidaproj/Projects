;+
; NAME       : demodu.pro (procedure)
; PURPOSE :
; 	culculate amplitude of some components on fluctuation value 
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        demodu,dat,xt,res,chi
; INPUTS :
; 	dat  --  3-d data
; 	xt   --  phase of roteting waveplate angle (rad.)
;       res  --  array = [cc,s1,c1,s2,c2,s4,c4]
;       chi  --  X squre
; OUTPUT :
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '10/06/08		
;        T.A. '2010/09/17/		
;-
;*************************************************************************
pro demodu,dat,xt,res,chi

sd=size(dat)
nx=sd[1]
ny=sd[2]
nt=sd[3]
res=fltarr(nx,ny,7)
chi=fltarr(nx,ny,7)

ss1= sin(xt)
cc1= cos(xt)
ss2= sin(2.*xt)
cc2= cos(2.*xt)
ss4= sin(4.*xt)
cc4= cos(4.*xt)

mat=invert([$
             [nt           ,total(1.*ss1) ,total(1.*cc1) ,total(1.*ss2) ,total(1.*cc2) ,total(1.*ss4) ,total(1.*cc4) ],     $
             [total(ss1*1.),total(ss1*ss1),total(ss1*cc1),total(ss1*ss2),total(ss1*cc2),total(ss1*ss4),total(ss1*cc4)],     $
             [total(cc1*1.),total(cc1*ss1),total(cc1*cc1),total(cc1*ss2),total(cc1*cc2),total(cc1*ss4),total(cc1*cc4)],     $
             [total(ss2*1.),total(ss2*ss1),total(ss2*cc1),total(ss2*ss2),total(ss2*cc2),total(ss2*ss4),total(ss2*cc4)],     $
             [total(cc2*1.),total(cc2*ss1),total(cc2*cc1),total(cc2*ss2),total(cc2*cc2),total(cc2*ss4),total(cc2*cc4)],     $
             [total(ss4*1.),total(ss4*ss1),total(ss4*cc1),total(ss4*ss2),total(ss4*cc2),total(ss4*ss4),total(ss4*cc4)],     $
             [total(cc4*1.),total(cc4*ss1),total(cc4*cc1),total(cc4*ss2),total(cc4*cc2),total(cc4*ss4),total(cc4*cc4)]      $
             ])

rmat0=total(dat*1.,3)
for i=0,nt-1 do begin
    if i eq 0 then begin
        rmat1=dat[*,*,0]*sin(xt[0])
        rmat2=dat[*,*,0]*cos(xt[0])
        rmat3=dat[*,*,0]*sin(2.*xt[0])
        rmat4=dat[*,*,0]*cos(2.*xt[0])
        rmat5=dat[*,*,0]*sin(4.*xt[0])
        rmat6=dat[*,*,0]*cos(4.*xt[0])
    endif else begin
        rmat1=rmat1+dat[*,*,i]*sin(xt[i])
        rmat2=rmat2+dat[*,*,i]*cos(xt[i])
        rmat3=rmat3+dat[*,*,i]*sin(2.*xt[i])
        rmat4=rmat4+dat[*,*,i]*cos(2.*xt[i])
        rmat5=rmat5+dat[*,*,i]*sin(4.*xt[i])
        rmat6=rmat6+dat[*,*,i]*cos(4.*xt[i])
    endelse
endfor

;tmp=fltarr(nx,ny,nt)
;rmat0=total(dat*1.,3)
;for i=0,nt-1 do tmp[*,*,i]=dat[*,*,i]*sin(xt[i])  &  rmat1=total(tmp,3)
;for i=0,nt-1 do tmp[*,*,i]=dat[*,*,i]*cos(xt[i])  &  rmat2=total(tmp,3)
;for i=0,nt-1 do tmp[*,*,i]=dat[*,*,i]*sin(2.*xt[i])  &  rmat3=total(tmp,3)
;for i=0,nt-1 do tmp[*,*,i]=dat[*,*,i]*cos(2.*xt[i])  &  rmat4=total(tmp,3)
;for i=0,nt-1 do tmp[*,*,i]=dat[*,*,i]*sin(4.*xt[i])  &  rmat5=total(tmp,3)
;for i=0,nt-1 do tmp[*,*,i]=dat[*,*,i]*cos(4.*xt[i])  &  rmat6=total(tmp,3)

cc=mat[0,0]*rmat0+mat[1,0]*rmat1+mat[2,0]*rmat2+mat[3,0]*rmat3+mat[4,0]*rmat4+mat[5,0]*rmat5+mat[6,0]*rmat6
s1=mat[0,1]*rmat0+mat[1,1]*rmat1+mat[2,1]*rmat2+mat[3,1]*rmat3+mat[4,1]*rmat4+mat[5,1]*rmat5+mat[6,1]*rmat6
c1=mat[0,2]*rmat0+mat[1,2]*rmat1+mat[2,2]*rmat2+mat[3,2]*rmat3+mat[4,2]*rmat4+mat[5,2]*rmat5+mat[6,2]*rmat6
s2=mat[0,3]*rmat0+mat[1,3]*rmat1+mat[2,3]*rmat2+mat[3,3]*rmat3+mat[4,3]*rmat4+mat[5,3]*rmat5+mat[6,3]*rmat6
c2=mat[0,4]*rmat0+mat[1,4]*rmat1+mat[2,4]*rmat2+mat[3,4]*rmat3+mat[4,4]*rmat4+mat[5,4]*rmat5+mat[6,4]*rmat6
s4=mat[0,5]*rmat0+mat[1,5]*rmat1+mat[2,5]*rmat2+mat[3,5]*rmat3+mat[4,5]*rmat4+mat[5,5]*rmat5+mat[6,5]*rmat6
c4=mat[0,6]*rmat0+mat[1,6]*rmat1+mat[2,6]*rmat2+mat[3,6]*rmat3+mat[4,6]*rmat4+mat[5,6]*rmat5+mat[6,6]*rmat6



res[*,*,0]=cc
res[*,*,1]=s1
res[*,*,2]=c1
res[*,*,3]=s2
res[*,*,4]=c2
res[*,*,5]=s4
res[*,*,6]=c4
;res=[[[cc]],[[s1]],[[c1]],[[s2]],[[c2]],[[s4]],[[c4]]]
;print,'array = [cc,s1,c1,s2,c2,s4,c4]'

dat1 = fltarr(nx,ny,nt)
for i=0,nt-1 do dat1[*,*,i] = cc + s1*sin(xt[i]) + c1*cos(xt[i]) + $
  s2*sin(2.*xt[i]) + c2*cos(2.*xt[i]) + s4*sin(4.*xt[i]) + c4*cos(4.*xt[i])
chi0 = abs(dat-dat1)
chi1 = total(chi0,3)

chi[*,*,0]=chi1/(nt*1.)
chi[*,*,1]=chi1/total(abs(ss1*1.))
chi[*,*,2]=chi1/total(abs(cc1*1.))
chi[*,*,3]=chi1/total(abs(ss2*1.))
chi[*,*,4]=chi1/total(abs(cc2*1.))
chi[*,*,5]=chi1/total(abs(ss4*1.))
chi[*,*,6]=chi1/total(abs(cc4*1.))

end
