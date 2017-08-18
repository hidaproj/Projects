; DIO8.pro
;Å@
;cd,'C:\Projects\cprog\VS2005\DIO8'
dllfile='C:\Projects\cprog\VS2005\DIO8\Debug\DIO8.dll'

r=call_external(dllfile,'test', 'Wooo', /PORTABLE )
r=call_external(dllfile,'Cdio_Init',/all_value,/cdecl)

InByte=call_external(dllfile,'Cdio_InByte',/all_value,/cdecl)
print,InByte

OutByte=3
r=call_external(dllfile,'Cdio_OutByte',OutByte,/all_value,/cdecl)
outstate=call_external(dllfile,'Cdio_OutState',/all_value,/cdecl)
print,outstate

r=call_external(dllfile,'Cdio_Exit',/all_value,/cdecl)


end
