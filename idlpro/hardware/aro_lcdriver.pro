;  AROptics LC driver
;	Nishida,K.

;---------------------------------------------------
; �ʏ�� LCinit �𖾎��I�ɌĂԕK�v�͂Ȃ�
; �Ȃ��Ȃ�ASetDACVoltage �̏���̎��s���Ɏ����I�ɏ��������s������

pro LCinit

result = CALL_EXTERNAL('C:\Projects\cprog\VS2005\CallLCDriver\Debug\CallLCDriver2.dll', 'reset', /cdecl)
if result eq 0 then message,'error in CallLCDriver2.dll'

end

;---------------------------------------------------
pro setLCvolt,volt0,volt1
;  use AROptics LC driver
result = CALL_EXTERNAL('C:\Projects\cprog\VS2005\CallLCDriver\Debug\CallLCDriver2.dll', 'SetDACVoltage', double(volt0), double(volt1), /cdecl)
if result eq 0 then message,'error in CallLCDriver2.dll'
end
