; kuhn_coefs subroutine to get the a,b,c,d variables from
; the magnetic frames
;
;2009-01-22 S.Jaeggli slight modification to streamline this code with
;Haosheng's procedure get_mm_inst.pro
;and corrected swap of c and d

;function kuhn_coefs2, q, u, v, lambda0, dlambda, i0, i1, j0, j1, q2, u2, v2
function ta_ct_kuhn_0, iquv, refxr,refyr,niquv,nov=nov,ab=ab,abcd=abcd,imax=imax
; 
; iquv : array (xx,yy,iquv)
; refxr: array (2) x range of reference stoes profiles
; refyr: array (2) wavelength range of line center
; conyr: array (2) wide wavelength range (pix)
; niquv: inverted IQUV
;

if keyword_set(ab) then ab0=ab
if keyword_set(abcd) then ab0=abcd[0:1]

if keyword_set(imax) then begin
	q=transpose(iquv[*,*,1]/(iquv[*,*,0]>max(iquv[*,*,0])*1e-10))
	u=transpose(iquv[*,*,2]/(iquv[*,*,0]>max(iquv[*,*,0])*1e-10))
	v=transpose(iquv[*,*,3]/(iquv[*,*,0]>max(iquv[*,*,0])*1e-10))
	
	imax=max(total(iquv[*,*,0],1))
	q=transpose(iquv[*,*,1]/imax)
	u=transpose(iquv[*,*,2]/imax)
	v=transpose(iquv[*,*,3]/imax)
endif else begin
	q=transpose(iquv[*,*,1]/iquv[*,*,0])
	u=transpose(iquv[*,*,2]/iquv[*,*,0])
	v=transpose(iquv[*,*,3]/iquv[*,*,0])
endelse
il0=refyr[0]
il1=refyr[1]
j0=refxr[0]
j1=refxr[1]
;efg=[median(q[conyr[0]:conyr[1],*]),	$
;	median(u[conyr[0]:conyr[1],*]),	$
;	median(v[conyr[0]:conyr[1],*])]
efg=[mean(q[refyr[0]:refyr[1],refxr[0]:refxr[1]]),	$
	mean(u[refyr[0]:refyr[1],refxr[0]:refxr[1]]),	$
	mean(v[refyr[0]:refyr[1],refxr[0]:refxr[1]])]
q=q-efg[0]
u=u-efg[1]
v=v-efg[2]
;---------------------;
;---- kuhn_coefs2 ----;
num_spot=j1-j0+1
sze=size(q)
nx=sze(1)
ny=sze(2)

qvec=fltarr(ny)
uvec=fltarr(ny)
vvec=fltarr(ny)

;il0=lambda0-dlambda
;il1=lambda0+dlambda


for j=0,ny-1 do begin
 qvec(j)=mean(q(il0:il1,j))
 uvec(j)=mean(u(il0:il1,j))
 vvec(j)=mean(v(il0:il1,j))
endfor


qumat=fltarr(2,num_spot)
for j=j0,j1 do begin
 qumat(0,j-j0)=qvec(j)
 qumat(1,j-j0)=uvec(j)
endfor

if keyword_set(ab) or keyword_set(abcd) then begin
	ab=ab0
	;print,ab
endif else ab=regress(qumat,vvec(j0:j1),replicate(1.0,num_spot),/relative_weight)
;stop
if not keyword_set(nov) then v2=v-ab(0)*q-ab(1)*u else v2=v	;anan

c=fltarr(ny)
d=fltarr(ny)
for j=0,ny-1 do begin
 qvtemp=total(q(*,j)*v2(*,j))
 vvtemp=total(v2(*,j)*v2(*,j))
 c(j)=qvtemp/vvtemp
 uvtemp=total(u(*,j)*v2(*,j))
 vvtemp=total(v2(*,j)*v2(*,j))
 d(j)=uvtemp/vvtemp
endfor

avalue=ab(0)
bvalue=ab(1)
if keyword_set(abcd) then begin
	dvalue=abcd[3]
	cvalue=abcd[2]
endif else begin
	dvalue=median(d(j0:j1))
	cvalue=median(c(j0:j1))
endelse
q2=q-cvalue*v2
u2=u-dvalue*v2

;---------------------;
;---------------------;

niquv=iquv
if keyword_set(imax) then begin
	niquv[*,*,1]=transpose(q2)*imax
	niquv[*,*,2]=transpose(u2)*imax
	niquv[*,*,3]=transpose(v2)*imax
endif else begin
	niquv[*,*,1]=transpose(q2)*iquv[*,*,0]
	niquv[*,*,2]=transpose(u2)*iquv[*,*,0]
	niquv[*,*,3]=transpose(v2)*iquv[*,*,0]
endelse

;stop
return,[avalue,bvalue,cvalue,dvalue]

end
