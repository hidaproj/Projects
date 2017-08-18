;+
; Project     : Solar-B/EIS
;
; Name        : expand_dir
;
; Purpose     : Expand directory names below input directory
;
; Example     : IDL> dir=expand_dir('+$SSW/gen')
;
;               Can have multiple directories separated by ':'
;               This function essentially replaces 'find_all_dir'
;
; Category    : utility string
;
; Syntax      : IDL> out=expand_dir(dirs)
;
; Inputs      : DIRS = top directory to expand
;
; Outputs     : ODIRS  = array of top directory with subdirectories 
;
; Keywords    : PLUS_REQUIRED - if set, a '+' is required to force
;               expansion.
;               PATH_FORMAT - if set, expanded directories is returned
;               as a delimited string
;
; History     : 30-May-2006, Zarro (L-3Com/GSFC) - written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-
                                                                                         
function expand_dir,dirs,plus_required=plus_required,path_format=path_format,_extra=extra
                                                                                         
if is_blank(dirs) then return,''                                                         
                                                                                         
path_delim=get_path_delim()                                                              
plus_required=keyword_set(plus_required)                                                 
tdirs=fix_path(dirs)                                                                
tdirs=str2arr(tdirs,path_delim)                                                          
 
;-- loop thru each directory, filtering out blanks, duplicates, and
;   non-existent directories
                                                                                        
delim=get_delim()                                                                        
ndirs=n_elements(tdirs)                                                                  
for i=0,ndirs-1 do begin                                                                 
  pdir=tdirs[i]                                                                          
  has_plus=stregex(pdir,'\+',/bool)                                                      
  if (not has_plus) and (not plus_required) then pdir='+'+pdir                           
  edirs=expand_path(pdir,/all,/array)                                                    
  nedirs=n_elements(edirs)                                                               
  if not exist(odirs) then odirs=edirs else begin                                        
   match,odirs,edirs,p,q                                                                 
   if q[0] eq -1 then odirs=[temporary(odirs),temporary(edirs)] else begin               
    if n_elements(q) ne nedirs then begin                                                
     temp=1b+bytarr(n_elements(nedirs))                                                  
     temp[q]=0b                                                                          
     chk=where(temp,count)                                                               
     if count gt 0 then edirs=edirs[chk]                                                 
     odirs=[temporary(odirs),temporary(edirs)]                                           
    endif                                                                                
   endelse                                                                               
  endelse                                                                                
endfor                                                                                   
                                                                                         
if keyword_set(path_format) then odirs=arr2str(odirs,path_delim)                         
return,odirs                                                                             
                                                                                         
end                                                                                      
                                                                                         
                                                                                         
                                                                                         
                                                                                         
                                                                                         
                                                                                         
                                                                                         
                                                                                         
                                                                                         
