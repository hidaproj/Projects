; mklib32.pro
; library for controling MK-5 via "Comm32.dll"
;   '95/09/20  k.i.
;   '96/07/15  k.i.
;   '98/01/10  k.i.	mkmove2, mkpos2
;   '98/01/14  k.i.	mksens2, mkinit,pn1=pn1,pn2=pn2
;   '02/08/15  k.i.	for IDL 5.1 & VC++6.0
;   '08/07/24  k.i.	for origin for DST

;*****************************************************
pro mkinit,COMx,MK_s1,MK_f1,MK_r1,BPS=BPS,pn1=pn1o,pn2=pn2o
;-----------------------------------------------------
; initialize MK-5
;  COMx	-	COM port ("COM1" or "COM2")
;  MK_s	-	minimum speed (pps)
;  MK_f	-	maximum speed (pps)
;  MK_r	-	acc time (msec)

common mklib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

;dllfile='c:\home\cprog\Comm32.dll'
dllfile='C:\nkrprj\CPROG\Comm32\Debug\Comm32.dll'
if not keyword_set(COMx) then COMx='COM1'
if not keyword_set(MK_s1) then begin
	MK_s1=100 &	MK_f1=10000 &	MK_r1=150	; for 25cm GB
;	MK_s=100 &	MK_f=2000 &	MK_r=150
endif
if not keyword_set(bps) then bps=9600
MK_s=MK_s1 &	MK_f=MK_f1 &	MK_r=MK_r1

bpsc=strcompress(string(bps),/remove_all)
r=call_external(dllfile,'CommOpen', COMx, bpsc, /PORTABLE )

;comstr='baud='+bpsc+' parity=N data=8 stop=1'
;r=call_external(dllfile,'CommOpen', COMx, comstr, /PORTABLE )

MK_sc=strcompress(string(MK_s),/remove_all)
MK_fc=strcompress(string(MK_f),/remove_all)
MK_rc=strcompress(string(MK_r),/remove_all)
com='D:S'+MK_sc+',F'+MK_fc+',R'+MK_rc+',S'+MK_sc+',F'+MK_fc+',R'+MK_rc
st=call_external(dllfile,'CommWrite',com)
print,com
;print,MK_s,MK_f,MK_r
wait,0.2
PN1=0l &	PN2=0l
mkstat,PN1,PN2,st
print,PN1,PN2,format='("PN1 & PN2 are set as",i6,",",i6)'
pn1o=PN1 &	pn2o=PN2

end 
 
;*****************************************************
pro mkcom,com
;-----------------------------------------------------
; send command string to MK-5
common mklib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

retn = call_external(dllfile,'CommWrite',com)

end

;*****************************************************
pro mkmove,k,pn,gwait=gwait
;-----------------------------------------------------
; move motor by # of pulse
;  k   -- motor 1 or 2
;  pn  -- number of pulse
;  gwait - if 1, wait for movement
common mklib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

if keyword_set(gwait) then gwait=1 else gwait=0
if pn eq 0 then return
if k eq 1 then com="M:X"
if k eq 2 then com="M:Y"
if pn gt 0 then com=com+"+P"+strcompress(string(pn),/remove_all)
if pn lt 0 then com=com+"-P"+strcompress(string(-pn),/remove_all)
mkcom,com
mkcom,'G'

if gwait then begin
	;// ----- by status
	;//busy=1;
	;//while(busy) {
	;//	mkcom("Q:");
	;//	st=0;
	;//	while(!(st && 0x01)) {
	;//		st=GetCommEventMask(ComID,0x3FF);
	;//		}
	;//	ret=ReadComm(ComID,buf,50);
	;//	buf[ret]='\0';
	;//	if(buf[ret-1] != 'B') busy=0;
	;//	}
	;// ----- by counting
	apn=pn &	if pn lt 0 then apn=-pn
	dt1=float(MK_f-MK_s)/12500.*MK_r	; // acc. ms
	pna=long((float(MK_f+MK_s)*dt1/1000.))
	if pna ge apn then 	dt=long(float(apn)/float(MK_f+MK_s)*1000*2.) $
	else 			dt=long(2.*dt1+float(apn-pna)/MK_f*1000.)
	wait,dt/1000     ; more delay is required
endif

;if keyword_set(gwait) then wait,0.25 ;<== unknowm delay
if k eq 1 then PN1=PN1+pn
if k eq 2 then PN2=PN2+pn

;mkstat
;for i=0,4 do begin
;	mkstat
;	wait,0.2
;endfor
mkstat,busy=busy,/nodisp	; & wait,0.2
if keyword_set(gwait) then begin
	mkstat,busy=busy,/nodisp
	;print,'busy=',busy
	while busy do begin
		wait,0.1
		print,'.',format='(a,$)'
		mkstat,busy=busy,/nodisp
	endwhile
endif

end 

;*****************************************************
pro mkmove2,pn1m,pn2m,gwait=gwait
;-----------------------------------------------------
; move motor by # of pulse
;  pn1m $ pn2m  -- number of pulse
;  gwait - if 1, wait for movement
common mklib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

if keyword_set(gwait) then gwait=1 else gwait=0
retn = call_external(dllfile,'mkmove2D',long(pn1m),long(pn2m), $
	long(gwait),value = [1b,1b,1b])
;if keyword_set(gwait) then wait,0.25 ;<== unknowm delay
PN1=PN1+pn1m &	PN2=PN2+pn2m

;mkstat
;for i=0,4 do begin
;	mkstat
;	wait,0.2
;endfor
mkstat,busy=busy,/nodisp	; & wait,0.2
if keyword_set(gwait) then begin
	mkstat,busy=busy,/nodisp
	;print,'busy=',busy
	while busy do begin
		wait,0.1
		print,'.',format='(a,$)'
		mkstat,busy=busy,/nodisp
	endwhile
endif

end 

;*****************************************************
pro mkpos,k,pn,gwait=gwait
;-----------------------------------------------------
; move motor to a pulse position
;  k   -- motor 1 or 2
;  pn  -- pulse position
;  gwait - if 1, wait for movement
common mklib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

case k of
    1: pnm=pn-PN1
    2: pnm=pn-PN2
    else: return
endcase
if keyword_set(gwait) then gwait=1 else gwait=0
mkmove,k,pnm,gwait=gwait
end 

;*****************************************************
pro mkpos2,pn1p,pn2p,gwait=gwait
;-----------------------------------------------------
; move motor to a pulse position
;  pn1p & pn2p -- pulse position
;  gwait - if 1, wait for movement
common mklib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

pn1m=pn1p-PN1 &	pn2m=pn2p-PN2
if keyword_set(gwait) then gwait=1 else gwait=0
mkmove2,pn1m,pn2m,gwait=gwait
end 

;*****************************************************
pro mkclose
;-----------------------------------------------------
; close COM port
common mklib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

retn = call_external(dllfile,'CommClose')

end 

;*****************************************************
pro mkstat,pns1,pns2,st,pnset=pnset,busy=busy,nodisp=nodisp
;-----------------------------------------------------
; get status string from MK-5
;   pns1  --  current position of motor 1
;   pns2  --  current position of motor 2
;   st    --  3 char status string
;    		st[0]   X - commend error,  K - ok
;    		st[1]   L - limit switch,   K - normal
;    		st[3]   B - busy,           R - ready
;   pnset --  if set, PN1 & PN2 are set to correct value
;   busy  --  if busy set 1 else 0
;   nodisp -  no display
common mklib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

st1=call_external(dllfile,'CommWrite','Q:')
wait,0.1
ststr=call_external(dllfile,'MK_Q_Read',/S_VALUE, /PORTABLE)

str9=strmid(ststr,strlen(ststr)-1,1)
if str9 ne 'B' and str9 ne 'R' then begin
	print,'mkstat error! try again'
	wait,0.5
	;ststr = call_external(dllfile,'mkstatD',/S_VALUE)
	ststr=call_external(dllfile,'MK_Q_Read',/S_VALUE, /PORTABLE)
endif
koron1=strpos(ststr,':')
ststr1=strmid(ststr,1,koron1-1)
;--
if strmid(ststr1,0,1) eq '*' then ststr1=strmid(ststr1,1,strlen(ststr1)-1)	; 2008.07.12
;;print,'ststr1= ',ststr1
;--
pns1=long(ststr1)
koron2=strpos(strmid(ststr,koron1+1,strlen(ststr)-koron1-1),':') $
	+koron1+1
pns2=long(strmid(ststr,koron1+1,koron2-koron1-1))
st=strmid(ststr,koron2+1,3)
if keyword_set(pnset) then begin
	PN1=pns1
	PN2=pns2
	if not keyword_set(nodisp) then  $
		print,'PN1 & PN2 are set to ',PN1,PN2
endif else begin
if not keyword_set(nodisp) then begin
	print,ststr
;	print,pns1,pns2,st,format='(2i8,"   st=",a)'
endif
endelse
if strmid(st,2,1) eq 'B' then busy=1 else busy=0

end 

;*****************************************************
pro mkorig
;-----------------------------------------------------
; get mechanical origin
common mklib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

mkstat,pn1,pn2,st3
if strmid(st3,0,1) eq 'K' then mkcom,"H:"
mkstat	&	wait,0.5
mkstat	&	wait,0.5
mkstat	&	wait,0.5
mkstat	&	wait,0.5	; unknow
busy=1
while busy eq 1 do begin
	wait,1
	mkstat,busy=busy
endwhile
wait,0.5
;mkmove,1,5750,/gwait
;mkmove,1,5750/2-600,/gwait	; modification 2002.7.6 
mkmove,1,42000l,/gwait		; <=== origin for DST 2008.7.24  
;wait,0.5
mkcom,"R:"
mkstat & wait,0.5 & mkstat & wait,0.5
mkstat & wait,0.5 & mkstat & wait,0.5
mkcom,"R:"
mkstat & wait,0.5 & mkstat & wait,0.5
mkstat & wait,0.5 & mkstat & wait,0.5
PN1=0
PN2=0
print,'Mechanical Origin: '
print,'   PN1 & PN2 are set to ',PN1,PN2

end 

;*****************************************************
function mkpn,k
;-----------------------------------------------------
; return current position
common mklib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

if n_elements(k) ne 0 then begin
    case k of
	1: pn=PN1
	2: pn=PN2
	else: pn=0l
    endcase
endif else begin
	pn=[PN1,PN2]
endelse

return,pn
end 


;*****************************************************
pro mksens2,xo,yo	; NOT work at 02/08/15
;-----------------------------------------------------
; return sensor signal
;	xo,yo	-- if origin sens ON -> 1, else 0

common mklib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

xo=0 &	yo=0
st1=call_external(dllfile,'CommWrite','I:')
wait,0.3
st=call_external(dllfile,'MK_I_Read',/S_VALUE, /PORTABLE)

ic=strpos(st,':')
if ic eq -1 then return
ix=fix(strmid(st,ic+2,1)) &	ix=ix-ix/8*8
iy=fix(strmid(st,ic+1,1)) &	iy=iy-iy/8*8
if ix/4 eq 1 then xo=1
if iy/4 eq 1 then yo=1
;print,st,xo,yo
stop

end

