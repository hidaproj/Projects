;**************************************************************
pro make_dir,dir
;	2017.4.1	k.i.  oscomdll=file_search()
;--------------------------------------------------------------
  if !version.arch eq 'x86_64' then begin
	dfile='\Projects\cprog\VS2010\oscom64\x64\Debug\oscom64.dll'
	oscomdll=file_search('C:'+dfile) 
	if oscomdll eq '' then oscomdll=file_search('D:'+dfile) 
	oscomdll=oscomdll[0]
  endif else begin
    	oscomdll='C:\Projects\cprog\VS2008\oscom32\Debug\oscom32.dll'
  endelse
  d=file_search(dir)
  if d[0] eq '' then dmy=call_external(oscomdll,'Dmkdirs',dir)

end

