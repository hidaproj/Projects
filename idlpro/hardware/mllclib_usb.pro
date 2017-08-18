; mllclib_usb.pro
;   Meadowlark LC driver lib. for USB
;   '12/08/07  M.H.  
;   '14/05/20  k.i.  noDev keyword, retn-> keyword in lcdrv_Open

;*****************************************************
pro lccom,icmd, USBx=USBx
;-----------------------------------------------------
; send command string to D3050
common mllclib_usb, dllfile, Dev_exist

retn = call_external(dllfile,'D3050_Write', icmd,$
		     /F_VALUE, /CDECL)

end

;*****************************************************
pro lcdrv_Open,USBx=USBx,noDev=noDev,retn=retn
;-----------------------------------------------------
common mllclib_usb, dllfile, Dev_exist
; initialize D3050
;  USBx	-	USB port ("USB1" or "USB2")

dllfile='C:\Projects\cprog\VS2010\D3050_64\x64\Debug\D3050_64.dll'
cd,'C:\Projects\cprog\VS2010\D3050_64\x64\Debug\'

if keyword_set(noDev) then begin
	Dev_exist=0
	return
endif else Dev_exist=1

if not keyword_set(USBx) then USBx='USB1'
case USBx of
	'USB1':retn = call_external(dllfile,'D3050_Open')
	'USB2':retn = call_external(dllfile,'D3050_Open2')
endcase

end

;*****************************************************
pro lcdrv_close, USBx=USBx
;-----------------------------------------------------
; close USB port
common mllclib_usb, dllfile, Dev_exist

if not Dev_exist then return
if not keyword_set(USBx) then USBx='USB1'
case USBx of
	'USB1':retn = call_external(dllfile,'D3050_Close')
	'USB2':retn = call_external(dllfile,'D3050_Close2')
endcase

end 

;*****************************************************
pro lcdrv_zero, USBx=USBx
;-----------------------------------------------------
; put zero volt on all LC
common mllclib_usb, dllfile, Dev_exist

if not Dev_exist then return

;if not keyword_set(USBx) then USBx='USB1'
for n=0,4-1 do lcvolt,n+1,0,USBx='USB1'
for n=0,4-1 do lcvolt,n+1,0,USBx='USB2'
end 


;*****************************************************
pro lcvolt,n,V, USBx=USBx
;-----------------------------------------------------
;  put voltage on LC chan-n
; n -	channel,  1-4
; V -   volt

common mllclib_usb, dllfile, Dev_exist

if not Dev_exist then return

in_i=strtrim(long(v*6553.5),2)
icmd='ld:'+strtrim(n,2)+','+in_i
;print,icmd
if not keyword_set(USBx) then USBx='USB1'
case USBx of
	'USB1':retn = call_external(dllfile,'D3050_Write', icmd, /F_VALUE, /CDECL)
	'USB2':retn = call_external(dllfile,'D3050_Write2', icmd, /F_VALUE, /CDECL)
endcase

;lccom,icmd

end


;*****************************************************
pro lcvoltm,V, USBx=USBx
;-----------------------------------------------------
;  put voltage on LC all channels(1-4)
; V -   volt

common mllclib_usb, dllfile, Dev_exist

if not Dev_exist then return

in_i=strtrim(long(v*6553.5),2)

icmd='ldd:'+in_i+','+in_i+in_i+','+in_i
;print,icmd
if not keyword_set(USBx) then USBx='USB1'
case USBx of
	'USB1':retn = call_external(dllfile,'D3050_Write', icmd, /F_VALUE, /CDECL)
	'USB2':retn = call_external(dllfile,'D3050_Write2', icmd, /F_VALUE, /CDECL)
endcase

;lccom,icmd

end

;ï°êîÇÃchÇ…ìdà≥ÇÇ©ÇØÇÈÅB
;readÇ™ìÆÇ¢ÇƒÇ¢Ç»Ç¢ÅB


;*****************************************************
pro lcread
;-----------------------------------------------------
;  put voltage on LC all channels(1-4)

common mllclib_usb, dllfile, Dev_exist

if not Dev_exist then return

retn = call_external(dllfile,'D3050_Read', /S_VALUE, /CDECL)


end
