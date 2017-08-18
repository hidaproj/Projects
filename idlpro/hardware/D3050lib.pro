; D3050lib.pro
; library for controling D3050 via "D3050_64.dll"
;   '12/01/03  k.i.   not work yet!

;*****************************************************
;pro D3050_Open
;-----------------------------------------------------
common D3050lib, dllfile

dllfile='C:\Projects\cprog\VS2010\D3050_64\Debug\D3050_64.dll'
dllfile='C:\Projects\cprog\VS2005\D3050_32\Debug\D3050_32.dll'

;retn = call_external(dllfile,'D3050_Open', /PORTABLE )
retn = call_external(dllfile,'D3050_Open')

print,retn
ans=''

cmd='ld:1,1000'
;retn = call_external(dllfile,'D3050_Write', cmd, /PORTABLE )
retn = call_external(dllfile,'D3050_Write', cmd)

read,ans

retn = call_external(dllfile,'D3050_Close', /PORTABLE )


end
