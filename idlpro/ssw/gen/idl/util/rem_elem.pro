function rem_elem, inarray, remarray, count
;
;+
;   Name: rem_elem
;
;   Purpose: return subscripts of input array remaining after elements in 
;	     a second array are removed
;
;   Input Parameters:
;      inarray - array to search/remove from
;      remarray - array of elements to search/remove from inarray
;
;   Output Parameters:
;      count - number of elements (subscripts) returned     
;
;   Calling Sequence:
;      ss = rem_elem(inarray,remarray) ; subscripts remaining or -1
;
;   History:
;      slf, 20-jan-1993
;      slf,  7-feb-1993 - documentation carification and variable name change
;-
temp_arr = inarray
nrem=n_elements(remarray)
rem_arr_cnt=0				; initialize 
vcnt = n_elements(inarray)
ss=indgen(vcnt)				; originally, all subscripts
;
; check each elements in remarray via where
while rem_arr_cnt lt nrem and vcnt ne 0 do begin
   valid=where(temp_arr ne remarray(rem_arr_cnt),vcnt)
   if vcnt gt 0 then begin
      temp_arr=temp_arr(valid) 		; some still remain
      ss=ss(valid)			; 
   endif else  ss=-1
   rem_arr_cnt=rem_arr_cnt+1		; next element
endwhile
   
; if ss=-1 then nothing left after removal
if ss(0) eq -1 then count=0 else count=n_elements(ss)

return,ss
end
