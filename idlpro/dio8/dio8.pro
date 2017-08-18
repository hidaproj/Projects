; dio8.pro

dllfile='C:\Projects\cprog\VS2010\DIO8\x64\Debug\DIO8.dll'
print,call_external(dllfile,'Test',/all_value,/cdecl)
print,call_external(dllfile,'Cdio_Init',/all_value,/cdecl)
print,call_external(dllfile,'Cdio_InByte',/all_value,/cdecl)
print,call_external(dllfile,'Cdio_Exit',/all_value,/cdecl)

end
