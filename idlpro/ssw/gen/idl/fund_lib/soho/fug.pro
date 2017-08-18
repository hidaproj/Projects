; 
; FUG:  just a shorthand to make nice number formatting easier: it
; trims off those leading spaces we love to hate.
;
function fug,a
return,strtrim(string(a(0)),2)
end