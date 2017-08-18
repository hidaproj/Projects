;+
; NAME       : ta_phase.pro (procedure)
; PURPOSE :
; 	return phase and period of rotating waveplate 
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        ta_phase,rtwv,dt,expo,ph,perio,hardtrigger=hardtrigger,period=period
; INPUTS :
; 	rtwv --  voltage of sensor monitaring waveplate angle 
; 	dt   --  raw data of time to capture image (Prosilica GE1650)
; OUTPUT :
;	ph   --  phase of rotating waveplate (rad)
;	prd  --  period of rotating waveplate (sec)
;	fps  --  frame per sec (fps)
; OPTIONAL INPUT PARAMETERS : 
;	period-  period of rotating waveplate (sec)
;                ,when you decide period forcibly
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '10/12/20		
;-
;*************************************************************************
pro ta_phase,rtwv,dt,ph,prd,fps,hardtrigger=hardtrigger,    $
          period=period

dt   = ULong64(dt)
tt   = (dt[*,0]*(ULong64(2)^32)+dt[*,1])/  $
  79861111.d*0.999987d   & tt=tt-tt[1]          ; time stamp [sec]
ntt  = size(tt,/dim)   & ntt=ntt[0]
coe  = poly_fit(findgen(ntt-1)+1,tt[1:ntt-1],1,chisq=chi)
fps  = 1./coe[1]                               ; frame rate [frames/sec]
;tt   = tt-coe[0]
;tt   = tt-tt[1]
;tt   = [tt[1]-coe[1],tt[1:*]]

nwv  = size(rtwv,/dim)   & nwv=nwv[1]
thr  = (mean(max(rtwv))+mean(min(rtwv)))*.5    ; thresh fold [V]
drtwv= rtwv[0,1:nwv-1]-rtwv[0,0:nwv-2]
p0   = where(drtwv ge thr)                     ; before edgerising
if keyword_set(period) then begin
    prd=period*1.                              ; period [sec]
endif else begin
    np0  = size(p0,/dim)  & np0=np0[0]
    if np0 lt 2 then begin
        print,'please use keyword "period"'
        ph  = -1
        prd = -1                                ; period [sec]
    endif else begin
        p1  = p0[1:np0-1]-p0[0:np0-2]
        prd = float(median(p1))/2000.           ; period [sec]
    endelse
endelse

if keyword_set(hardtrigger) then begin
    ph=tt/prd*2*!pi
endif else begin
    p0=where(drtwv le -1.*thr) ; before edgefalling
    t0=max(tt)-float((nwv-1)-p0[0])/2000.
    ph=2.*!pi*(tt-t0)/prd
endelse


end
