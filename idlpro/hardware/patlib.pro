; patlib.pro
; library for controling PAT-001 via "Comm32.dll"
;   '09/02/10  k.i.	for IDL 5.4 & VC++6.0  (??)
;   '09/05/06  k.i.	for IDL 6.3 & VS2005

;*****************************************************
pro patinit,COMx,MK_s1,MK_f1,MK_r1,BPS=BPS,pn1=pn1o
;-----------------------------------------------------
; initialize MK-5
;  COMx	-	COM port ("COM1" or "COM2")
;  MK_s	-	minimum speed (pps)
;  MK_f	-	maximum speed (pps)
;  MK_r	-	acc time (msec)


common patlib,dllfile,PN1,MK_s,MK_f,MK_r

dllfile='C:\Projects\cprog\VS2005\RS232C\Debug\RS232C.dll'

if not keyword_set(COMx) then COMx='COM1'
if not keyword_set(MK_s1) then begin
;	MK_s1=100 &	MK_f1=10000 &	MK_r1=150	; for 25cm GB
	MK_s1=100 &	MK_f1=3500 &	MK_r1=100	; for SGSP-60YAW
	; minmum speed =100pps
endif
if not keyword_set(bps) then bps=9600
MK_s=MK_s1 &	MK_f=MK_f1 &	MK_r=MK_r1

bpsc=strcompress(string(bps),/remove_all)
r=call_external(dllfile,'CommOpen', COMx, bpsc, /PORTABLE )

MK_sc=strcompress(string(MK_s),/remove_all)
MK_fc=strcompress(string(MK_f),/remove_all)
MK_rc=strcompress(string(MK_r),/remove_all)
com='D:1S'+MK_sc+'F'+MK_fc+'R'+MK_rc;  +',S'+MK_sc+',F'+MK_fc+',R'+MK_rc
print,COMx,'  ',com
st=call_external(dllfile,'CommWrite',com)

PN1=0l
;mkstat,PN1,PN2,st
print,PN1,format='("PN1 was set as",i6)'
pn1o=PN1



end

;*****************************************************
pro patcom,com
;-----------------------------------------------------
; send command string to PAT-001
common patlib,dllfile,PN1,MK_s,MK_f,MK_r

retn = call_external(dllfile,'CommWrite',com)

end

;*****************************************************
pro patmove,pn,gwait=gwait
;-----------------------------------------------------
; move motor by # of pulse
;  pn  -- number of pulse
;  gwait - if 1, wait for movement
common patlib,dllfile,PN1,MK_s,MK_f,MK_r

if keyword_set(gwait) then gwait=1 else gwait=0
if pn eq 0 then return
com="M:1"
if pn gt 0 then com=com+"+P"+strcompress(string(pn),/remove_all)
if pn lt 0 then com=com+"-P"+strcompress(string(-pn),/remove_all)
patcom,com
wait,0.001	; necessary, 2009.5.6
patcom,'G:'
PN1=PN1+pn

if keyword_set(gwait) then begin
  patstat,busy=busy,/nodisp
  ;print,'busy=',busy
  while busy do begin
    wait,0.1
    print,'.',format='(a,$)'
    patstat,busy=busy,/nodisp
  endwhile
endif


end

;*****************************************************
pro patpos,pn,gwait=gwait
;-----------------------------------------------------
; move motor to a pulse position
;  pn  -- pulse position
;  gwait - if 1, wait for movement
common patlib,dllfile,PN1,MK_s,MK_f,MK_r

pnm=pn-PN1
if keyword_set(gwait) then gwait=1 else gwait=0
patmove,pnm,gwait=gwait
end 

;*****************************************************
pro patclose
;-----------------------------------------------------
; close COM port
common patlib,dllfile,PN1,MK_s,MK_f,MK_r

retn = call_external(dllfile,'CommClose')

end 

;*****************************************************
pro patstat,pns1,st,pnset=pnset,busy=busy,nodisp=nodisp
;-----------------------------------------------------
; get status string from PAT-001
;   pns1  --  current position of motor 1
;   st    --  3 char status string
;    		st[0]   X - commend error,  K - ok
;    		st[1]   L - limit switch,   K - normal
;    		st[3]   B - busy,           R - ready
;   pnset --  if set, PN1 & PN2 are set to correct value
;   busy  --  if busy set 1 else 0
;   nodisp -  no display
common patlib,dllfile,PN1,MK_s,MK_f,MK_r

st1=call_external(dllfile,'CommWrite','Q:')
wait,0.1
ststr=call_external(dllfile,'MK_Q_Read',/S_VALUE, /PORTABLE)
bb=byte(ststr)
ipr=where(bb eq 10)  ;  CR
ipr1=max(ipr)+1
ststr=strmid(ststr,ipr1,strlen(ststr)-ipr1) ;  2009.2.25
ststr=strcompress(ststr,/remove_all)
;help,ststr
str9=strmid(ststr,strlen(ststr)-1,1)
; print,str9
if str9 ne 'B' and str9 ne 'R' then begin
	print,'mkstat error! try again'
	wait,0.5
	;ststr = call_external(dllfile,'mkstatD',/S_VALUE)
	ststr=call_external(dllfile,'MK_Q_Read',/S_VALUE, /PORTABLE)
endif
koron1=strpos(ststr,',')
pns1=long(strmid(ststr,1,koron1-1))
st=strmid(ststr,koron1+1,3)

; if strmid(st,2,1) eq 'B' then busy=1 else busy=0
if str9 eq 'B' then busy=1 else busy=0

end 

;*****************************************************
pro patorig
;-----------------------------------------------------
; get mechanical origin
common patlib,dllfile,PN1,MK_s,MK_f,MK_r

patstat,pns1,st3
if strmid(st3,0,1) eq 'K' then patcom,"H:1"
patstat	&	wait,0.5
patstat	&	wait,0.5
patstat	&	wait,0.5
patstat	&	wait,0.5	; unknow
busy=1
while busy eq 1 do begin
	wait,1
	patstat,busy=busy
endwhile
wait,0.5
;mkmove,1,5750,/gwait
;mkmove,1,5750/2-600,/gwait	; modification 2002.7.6 
patmove,10l,/gwait		; <=== origin for DST 2008.7.24  
;wait,0.5
patcom,"R:1"
patstat & wait,0.5 & patstat & wait,0.5
patstat & wait,0.5 & patstat & wait,0.5
patcom,"R:1"
patstat & wait,0.5 & patstat & wait,0.5
patstat & wait,0.5 & patstat & wait,0.5
PN1=0
print,'Mechanical Origin: '
print,'   PN1 was set to ',PN1

end 
