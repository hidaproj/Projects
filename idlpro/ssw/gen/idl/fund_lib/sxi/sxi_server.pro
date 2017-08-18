;+                                                              
; Project     : SXI                                             
;                                                               
; Name        : SXI_SERVER                                      
;                                                               
; Purpose     : return first available SXI data server          
;                                                               
; Category    : synoptic sockets                                
;                                                               
; Inputs      : None                                            
;                                                               
; Outputs     : SERVER = SXI data server name                   
;                                                               
; Keywords    : NETWORK = 1 if network is up                    
;               PATH = top data path                            
;               NO_CHECK= skip network check                    
;               FULL = include HTTP in server name              
;               SATELLITE = GOES satellite [def=12]
;                                                               
; History     : 15-Jan-2003,  D.M. Zarro (EER/GSFC) - written    
;               28-Jul-2006, Zarro (ADNET/GSFC) - added SATELLITE
;                                                               
; Contact     : DZARRO@SOLAR.STANFORD.EDU                       
;-                                                              
                                                                
function sxi_server,_ref_extra=extra,network=network,path=path,$
                    no_check=no_check,full=full,satellite=satellite
                                                                
;-- primary server                                              
                                                                
path='/archive/fits/goes'
psat='12'
if is_number(satellite) then psat=strtrim(satellite,2)
path=path+psat
server='sxidata.ngdc.noaa.gov'                                  
                                                                
if keyword_set(no_check) then network=1b else $                 
 network=have_network(server,_extra=extra)                      
                                                                
if keyword_set(full) then server='http://'+server               
                                                                
return,server                                                   
                                                                
end                                                             
