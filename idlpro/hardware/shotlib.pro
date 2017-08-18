; shotlib.pro
; library for controling SHOT-202 via "RS232C.dll"
;   '09/05/30  k.i.	from mklib32.pro

;*****************************************************
pro shotinit,COMx,MK_s1,MK_f1,MK_r1,BPS=BPS,pn1=pn1o,pn2=pn2o, $
	Offset1=Offset1,Offset2=Offset2
;-----------------------------------------------------
; initialize SHOT-202
;  COMx	-	COM port ("COM1" or "COM2")
;  MK_s	-	minimum speed (pps)
;  MK_f	-	maximum speed (pps)
;  MK_r	-	acc time (msec)

common shotlib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

;dllfile='C:\nkrprj\CPROG\Comm32\Debug\Comm32.dll'
dllfile='C:\Projects\cprog\VS2005\RS232C\Debug\RS232C.dll'
if not keyword_set(COMx) then COMx='COM1'
if not keyword_set(MK_s1) then begin
;	MK_s1=100 &	MK_f1=10000 &	MK_r1=150	; for 25cm GB
	MK_s1=100 &	MK_f1=3500 &	MK_r1=100	; for SGSP-60YAW
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
;com='D:S'+MK_sc+',F'+MK_fc+',R'+MK_rc+',S'+MK_sc+',F'+MK_fc+',R'+MK_rc
com='D:WS'+MK_sc+'F'+MK_fc+'R'+MK_rc+'S'+MK_sc+'F'+MK_fc+'R'+MK_rc
st=call_external(dllfile,'CommWrite',com)
print,com
;print,MK_s,MK_f,MK_r
wait,0.2
PN1=0l &	PN2=0l
shotstat,PN1,PN2,st
if keyword_set(offset1) then PN1=PN1-Offset1
if keyword_set(offset2) then PN2=PN2-Offset2
print,PN1,PN2,format='("PN1 & PN2 are set as",i6,",",i6)'
pn1o=PN1 &	pn2o=PN2

end 

;*****************************************************
pro shotcom,com
;-----------------------------------------------------
; send command string to SHOT-202
common shotlib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

retn = call_external(dllfile,'CommWrite',com)

end

;*****************************************************
pro shotmove,k,pn0,gwait=gwait
;-----------------------------------------------------
; move motor by # of pulse
;  k   -- motor 1 or 2
;  pn  -- number of pulse
;  gwait - if 1, wait for movement
common shotlib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

pn=round(pn0)
if keyword_set(gwait) then gwait=1 else gwait=0
if pn eq 0 then return
if k eq 1 then com="M:1"
if k eq 2 then com="M:2"
if pn gt 0 then com=com+"+P"+strcompress(string(pn),/remove_all)
if pn lt 0 then com=com+"-P"+strcompress(string(-pn),/remove_all)
print,com
shotcom,com
wait,0.001	; necessary, 2009.5.6
shotcom,'G:'

if keyword_set(gwait) then begin
	shotstat,busy=busy,/nodisp
	;print,'busy=',busy
	while busy do begin
		wait,0.1
		print,'.',format='(a,$)'
		shotstat,busy=busy,/nodisp
	endwhile
endif

if k eq 1 then PN1=PN1+pn
if k eq 2 then PN2=PN2+pn

end 

;*****************************************************
pro shotpos,k,pn,gwait=gwait
;-----------------------------------------------------
; move motor to a pulse position
;  k   -- motor 1 or 2
;  pn  -- pulse position
;  gwait - if 1, wait for movement
common shotlib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

case k of
    1: pnm=pn-PN1
    2: pnm=pn-PN2
    else: return
endcase
if keyword_set(gwait) then gwait=1 else gwait=0
shotmove,k,pnm,gwait=gwait
end 

;*****************************************************
pro shotclose
;-----------------------------------------------------
; close COM port
common shotlib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

retn = call_external(dllfile,'CommClose')

end 

;*****************************************************
pro shotstat,pns1,pns2,st,pnset=pnset,busy=busy,nodisp=nodisp
;-----------------------------------------------------
; get status string from SHOT-202
;   pns1  --  current position of motor 1
;   pns2  --  current position of motor 2
;   st    --  3 char status string
;    		st[0]   X - commend error,  K - ok
;    		st[1]   L - limit switch,   K - normal
;    		st[3]   B - busy,           R - ready
;   pnset --  if set, PN1 & PN2 are set to correct value
;   busy  --  if busy set 1 else 0
;   nodisp -  no display
common shotlib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

st1=call_external(dllfile,'CommWrite','Q:')
wait,0.1
ststr=call_external(dllfile,'MK_Q_Read2',/S_VALUE, /PORTABLE)
bb=byte(ststr)
ipr=where(bb eq 10)  ;  CR
ipr1=max(ipr)+1
ststr=strmid(ststr,ipr1,strlen(ststr)-ipr1) ;  2009.2.25
ststr=strcompress(ststr,/remove_all)
;help,ststr
st5=strsep(ststr,sep=',')
if n_elements(st5) ne 5 then begin
	print,'shotstat error! try again'
	return
endif
pns1=long(st5[0])
pns2=long(st5[1])
st=st5[2:4]
if st[2] eq 'B' then busy=1 else busy=0

end 

;*****************************************************
pro shotorig,k,offset=offset
;-----------------------------------------------------
; get mechanical origin
;  k	-- motor #
;  offset -- offset pulse
common shotlib,dllfile,PN1,PN2,MK_s,MK_f,MK_r

case k of
    1: com='H:1'
    2: com='H:2'
    else: return
endcase

shotstat,pn1,pn2,st3
wait,0.01
if st3[0] eq 'K' then shotcom,com

shotstat,busy=busy,/nodisp
;print,'busy=',busy
while busy do begin
	wait,0.1
	print,'.',format='(a,$)'
	shotstat,busy=busy,/nodisp
endwhile
print,''
if keyword_set(offset) then begin
	shotmove,k,offset
endif else offset=0
case k of
    1: pn1=0l
    2: pn2=0l
endcase
print,'pn'+string(k,form='(i1)')+' set 0,   Offset='+string(Offset)

end
