; usb1024lib.pro
;   IDL lib for USB-1024LS
;   2009/.2.2	k.i., t.a.

;***************************************************** 
pro init_usb1024
;----------------------------------------------------- 
common usb1024,usbiodll

usbiodll='C:\Projects\cprog\USB1024LS\Debug\USB1024LS.dll'

com='Test_usb1024'
val=call_external(usbiodll,'init_usb1024',value=[0d])
print,val

end

;***************************************************** 
function get_usbio
;----------------------------------------------------- 
common usb1024,usbiodll

return,call_external(usbiodll,'usb_digin_a',value=[0d])
end

;***************************************************** 
pro wp_orig,time1
;----------------------------------------------------- 
common usb1024,usbiodll
;  wait for the origin of wave plate

time1=call_external(usbiodll,'usb_trig_a0',value=[0d],/S_VALUE)

end
