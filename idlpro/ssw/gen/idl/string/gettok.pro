function gettok,st,char, exact=exact
;+
; NAME:
;	GETTOK                                    
; PURPOSE:
;	Retrieve the first part of a (vector) string up to a specified character
; EXPLANATION:
;	GET TOKen - Retrieve first part of string until the character char 
;	is encountered.   
;
; CALLING SEQUENCE:
;	token = gettok( st, char, [ /EXACT ] )
;
; INPUT:
;	char - character separating tokens, scalar string
;
; INPUT-OUTPUT:
;	st - string to get token from (on output token is removed),
;            scalar or vector
;
; OUTPUT:
;	token - extracted string value is returned, same dimensions as st
; OPTIONAL INPUT KEYWORD:
;       /EXACT -  The default behaviour of GETTOK is to remove any leading 
;              blanks and (if the token is a blank) convert tabs to blanks.    
;              Set the /EXACT keyword to skip these steps and leave the 
;              input string unchanged before searching for the  character 
;              tokens. 
;
; EXAMPLE:
;	If ST is ['abc=999','x=3.4234'] then gettok(ST,'=') would return
;	['abc','x'] and ST would be left as ['999','3.4234'] 
;
; PROCEDURE CALLS:
;       REPCHR()
; HISTORY
;	version 1  by D. Lindler APR,86
;	Remove leading blanks    W. Landsman (from JKF)    Aug. 1991
;	Converted to IDL V5.0   W. Landsman   September 1997
;       V5.3 version, accept vector input   W. Landsman February 2000
;       Slightly faster implementation  W. Landsman   February 2001
;       Added EXACT keyword  W. Landsman March 2004
;       Use COMPLEMENT keyword to WHERE W. Landsman   March 2004
;-
;----------------------------------------------------------------------
  On_error,2                           ;Return to caller

  if N_params() LT 2 then begin
      print,'Syntax - token = gettok( st, char, [ /EXACT ] )'
      return,-1
  endif 

; if char is a blank treat tabs as blanks

 if not keyword_set(exact) then begin
    st = strtrim(st,1)              ;Remove leading blanks and tabs
    if char EQ ' ' then begin 
       tab = string(9b)                 
       if max(strpos(st,tab)) GE 0 then st = repchr(st,tab,' ')
    endif
  endif
  token = st

; find character in string

  pos = strpos(st,char)
  test = pos EQ -1
  bad = where(test, Nbad)
  if Nbad GT 0 then st[bad] = ''
 
; extract token
 good = where(1b-test, Ngood)
 if Ngood GT 0 then begin
    stg = st[good]
    pos = reform( pos[good], 1, Ngood )
    pos=pos[0]		; <=== for IDL5.2,  2006.11.10 k.i.
    token[good] = strmid(stg,0,pos)
    st[good] = strmid(stg,pos+1)
 endif

;  Return the result.

 return,token
 end