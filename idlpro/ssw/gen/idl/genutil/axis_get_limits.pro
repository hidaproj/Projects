; Time-stamp: <Tue Feb 22 2005 16:08:13 csillag auriga.ethz.ch>
;+
; PROJECT:
;       HESSI
;
; NAME:
;       axis_get_limits()
;
; PURPOSE:
;       Get the index of the first and last axis element included 
;       a specific range. The axis passed is an array 
;       of mean values, not edges.
;
; CATEGORY:
;       gen/utilities
;
; CALLING SEQUENCE:
;       result = axis_get_limits( mean, range )
;
; INPUTS:
;       mean: a n-element vector of mean values (usually an axis)
;       range: then range to check the elements against
;
; OUTPUTS:
;       result: the indices of the axis
;
; EXAMPLES:
;       print, axis_get_limits( indgen(4), [0,1] )
;           0           1
;       print, axis_get_limits( indgen(4), [-0.1,1] )
;           0           1
;       print, axis_get_limits( indgen(4), [0.4,1] )
;           0           1
;       print, axis_get_limits( indgen(4), [0.5,1] )
;           1           1
;    print, axis_get_limits( indgen(4), [0.5, 2] )
;           1           2

; SEE ALSO:
;       edge_products, get_edge_products
;
; HISTORY:
;       june 2004 --- acs, csillag@fh-aargau.ch created
;
;--------------------------------------------------------------------------

function axis_get_limits, axis, range

n_els = N_elements( axis )
if not valid_range( range ) then return, [0, n_els-1]

; use mean to calculate the edges.... because we have to check on the edges
; and not on the mean.

; in fact i'll move all this into the binning class. bbut not now
; because i'm late with this
edges = get_edge_products( axis, /mean )
;edges = get_edge_products( axis, /edges_1 ) 
if n_elements( edges ) gt 1 then begin 
    limit = (Value_Locate( edges, range ) + [1,1] ) > 0 <  (n_els-1)
endif else begin 
; this might not be the last version....
    if range[0] + edges le (range[1]-range[0])/2. then limit = [0]
    if range[1] - edges ge (range[1]-range[0])/2. then limit = append_arr( limit, 1 )
endelse

IF limit[0] GT limit[1] THEN limit = limit[[1, 0]]

return, limit

end

;-------------------------------------------------------------------- 

pro axis_get_limits_test

print, axis_get_limits( indgen(4), [0,1] )
;           0           1
print, axis_get_limits( indgen(4), [-0.1,1] )
;           0           1
print, axis_get_limits( indgen(4), [0.4,1] )
;           0           1
print, axis_get_limits( indgen(4), [0.5,1] )
;           1           1
print, axis_get_limits( indgen(4), [0.5, 2] )
;           1           2

end
