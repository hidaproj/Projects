;+
; Project     : HESSI
;
; Name        : SYNOP_PATH
;
; Purpose     : return path to synoptic data files based on date
;
; Category    : synoptic sockets
;                   
; Inputs      : DATE = input date, default = current
;
; Outputs     : PATH = path to synoptic data files
;
; Keywords    : None
;
; History     : 28-Dec-2004,  D.M. Zarro (L-3Com/GSFC) -Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-


function synop_path,date,soho=soho

err=''
time=anytim2tai(date,err=err)
if err ne '' then begin
 get_utc,time 
 time=anytim2tai(time)
endif

change_date=anytim2tai('15-dec-04')

soho=keyword_set(soho)

if (time gt change_date) and (1-soho) then return,'/ancillary' else return,'/synoptic'

end
