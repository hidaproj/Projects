;  AROptics LC driver
;	Nishida,K.

;---------------------------------------------------
; 通常は LCinit を明示的に呼ぶ必要はない
; なぜなら、SetDACVoltage の初回の実行時に自動的に初期化を行うから

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
