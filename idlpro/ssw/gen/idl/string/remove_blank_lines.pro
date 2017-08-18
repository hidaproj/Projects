;+
; NAME
;
;           REMOVE_BLANK_LINES()
;
; PROJECT
;
;           SOLAR-B/EIS
;
; EXPLANATION
;
;           Remove blank lines from a string array
;
; INPUTS
;
;           array - a string array
;
; HISTORY
;
;     V0.1, Written 28-June-2006, John Rainnie
;-
FUNCTION remove_blank_lines , array

string_lengths = STRLEN(array)
; If there are no blank lines, then bail out
IF (WHERE(string_lengths[0]) EQ -1) THEN RETURN , array

; Get the indicies of blank lines
indicies  = WHERE(string_lengths NE 0 , count)

RETURN , array[indicies]

END
