;+                                                                              
; Project     : HESSI                                                           
;                                                                               
; Name        : SOCK_COPY                                                       
;                                                                               
; Purpose     : copy file via HTTP sockets                                      
;                                                                               
; Category    : utility system sockets                                          
;                                                                               
; Syntax      : IDL> sock_copy,url_file,outdir=outdir                           
;                                                                               
; Inputs      : URL_FILE = remote file name to copy with URL path               
;                                                                               
; Outputs     : None                                                            
;                                                                               
; Keywords    : OUT_DIR = output directory to copy file                         
;               ERR   = string error message                                    
;               NOWAIT = set to execute copy in background (>= IDL 6.3)         
;                                                                               
; Example     :                                                                 
;                                                                               
; IDL> f='smmdac.nascom.nasa.gov/synop_data/kanz/kanz_halph_fd_20000114_1016.fts
; IDL> sock_copy,f                                                              
;                                                                               
; History     : 27-Dec-2001,  D.M. Zarro (EITI/GSFC) - Written                  
;               23-Dec-2005, Zarro (L-3Com/GSFC) - removed COMMON               
;               26-Dec-2005, Zarro (L-3Com/GSFC) - added /HEAD_FIRST            
;                             to HTTP->COPY to ensure checking for              
;                             file before copying                               
;               18-May-2006, Zarro (L-3Com/GSFC) - added IDL-IDL bridge         
;                            capability for background copying                  
;                                                                               
; Contact     : DZARRO@SOLAR.STANFORD.EDU                                       
;-                                                                              
                                                                                
pro sock_copy,url,err=err,_ref_extra=extra,copy_file=copy_file,nowait=nowait    
                                                                                
err=''                                                                          
                                                                                
;-- handoff to IDL-IDL bridge version if /NOWAIT                                

if is_string(extra) then extra=uniq_key(extra)                                                                                
if keyword_set(nowait) then begin                                               
 sock_download,url,err=err,copy_file=copy_file,_extra=extra                     
 if is_string(err) then message,'reverting to regular SOCK_COPY..',/cont else return
endif                                                                           
                                                                                
if is_blank(url) then begin                                                     
 err='missing input URL'                                                        
 pr_syntax,'sock_copy,url'                                                      
 return                                                                         
endif                                                                           
                                                                                
;-- check if using FTP                                                          
                                                                                
is_ftp=stregex(url,'ftp://',/bool)                                              
if is_ftp[0] then begin                                                         
 read_ftp,url,/file,err=err,_extra=extra,copy_file=copy_file                                        
 return                                                                         
endif                                                                           
                                                                                
http=obj_new('http',err=err)                                                    
if err ne '' then return                                                        
                                                                                
http->hset,_extra=extra                                                         
n_url=n_elements(url)                                                           
copy_file=strarr(n_url)                                                         
for i=0,n_url-1 do begin                                                        
 http->copy,url[i],_extra=extra,err=err,copy_file=temp,$                        
                                    /head_first,/verbose                        
 copy_file[i]=temp                                                              
endfor                                                                          

if n_url eq 1 then copy_file=copy_file[0]
                                                                                
obj_destroy,http                                                                
                                                                                
return                                                                          
end                                                                             
