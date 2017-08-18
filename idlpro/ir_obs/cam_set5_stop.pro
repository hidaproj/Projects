;---------- stop obs ----------;
	r3=call_external(dllfile,'stopfan',m_hCam,/all_value,/cdecl);
	m_hCam=call_external(dllfile,'stopcap',m_hCam,/all_value,/cdecl)
end
