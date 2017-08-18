; dm97lib.pro

;	2012.11.7	k.i., m.y.
;	2012.11.18	k.i. instruction by Miura

;***************************************************************
pro dm97init
common dm97lib,rs232cdll, xusbdll, buf

rs232cdll='C:\Projects\cprog\VS2005\RS232C\Debug\RS232C.dll'
xusbdll='C:\Projects\cprog\VS2005\xinetics_usb\Debug\xinetics_usb.dll'

COMx='COM1'
retn = call_external(rs232cdll,'CommOpen', COMx, $
	' baud=115200 parity=N data=8 stop=1', /PORTABLE )

retn = call_external(rs232cdll,'CommWrite','0')
wait,1
retn = call_external(rs232cdll,'CommWrite','Y3')
wait,1
retn = call_external(rs232cdll,'CommWrite','MN')
wait,1
retn = call_external(rs232cdll,'CommWrite','1')

devName=call_external(xusbdll,'Xusbinit',/S_value)

nch=97
buf=bytarr(nch*2)

end

;***************************************************************
pro dm97close
common dm97lib,rs232cdll, xusbdll, buf

retn = call_external(rs232cdll,'CommWrite','0')
retn=call_external(xusbdll,'Xusbclose')
retn = call_external(rs232cdll,'CommClose')

end

;***************************************************************
pro dm97write,ich,iv
;  put voltage on ch-i
;  ich[*]	ch #
;  iv[*]	voltage
common dm97lib,rs232cdll, xusbdll, buf

buf[ich*2]=iv mod 256 &	buf[ich*2+1]=iv/256

ret=call_external(xusbdll,'Xusbwrite',buf)


end

;***************************************************************
common dm97lib,rs232cdll, xusbdll,buf





end
