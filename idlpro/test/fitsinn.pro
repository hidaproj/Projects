;
function fitsinnfunc,x,m
    common fitsinncom,sinOrder

    ret=[1.]
    for i=1,sinOrder do ret=[ret,[cos(i*x)],[sin(i*x)]]
    return,ret
end
;+
;NAME:
;fitsinn - fit to terms of  Asin(nx-phi) 
;SYNTAX:coefI=fitsinn(x,y,N)
;ARGS:
;       x  - independent var. (x values for fit). should already be in radians
;       y  - measured dependent variable.
;       N  - number of terms to fit 1 .. N
;RETURN:
;   coefI: {}  hold the fit info .. see belo
;  yfit: float   optionally evalute and return the fit
;
;DESCRIPTION:
;   Do a linear least squares fit (svdfit) to a sin wave with integral
;values of the frequency 1 through N. Return the coeficients in coefI.
;
;EXAMPLE:
;
; let az,azerr be the azimuths in deg and the azimuth errors.
; Fit a0 +a1*sin(az-ph1) + a2*sin(2az-ph2) + .. an*sin(naz-phn)
;
;   N=3           ; use sin(az, 2az,3az)
;   coefI=fitsinn(az*!dtor,azerr,N,yfit=yfit)
;   help,coefI
;** Structure <8bd0e6c>, 5 tags, length=124, data length=122, refs=1:
;   N               INT       3        .. number we fit to
;   C0              DOUBLE    0.       .. the constant term
;   PHDEG           DOUBLE    Array[3] .. phase indegrees
;   AMP             DOUBLE    Array[3] .. amplitue of fit
;   PHRD            DOUBLE    Array[3]  .. phase in radians
;   COSSIN          DOUBLE    Array[2, 3] .. cos,sin Amplitudes for each order
; 
;NOTES:   
; Asin(Nt-phi)= Asin(Nt)cos(phi) - Acos(Nt)sin(phi) =  Bsin(Nt) + Ccos(Nt)
;      B=Acos(phi)
;      C=-Asin(phi)
;    phi      = atan(sin(phi)/cos(phi))/ = atan(-c,b)
;    amplitude=sqrt(B^2+C^2)
; so the fit for B,C is linear.
;-           
;  
function fitsinn,x,y,N,yfit=yfit
    common fitsinncom,sinOrder
;
;   could not seem to embed quote in the string 
;
    sinOrder=n
    coef=svdfit(x,y,1+2*N,function_name='fitsinnfunc',singular=sng,yfit=yfit)
;
    if  sng ne 0 then  message,"svdfit returned singularity"

    cossin=reform(coef[1:*],2,n)
    amp   = sqrt(reform(cossin[0,*])^2 + reform(cossin[1,*])^2)
    ph=reform((atan(-cossin[1,*],cossin[0,*])))
    ii=where (ph lt 0,cnt)
    if cnt gt 0 then ph[ii]=ph[ii]+2*!pi
    coefI={   n   : n   , $ ; how many orders we used
			 c0   : coef[0] , $ ; constant term
             phDeg: ph*!radeg,$
             amp : amp,$
             phRd: ph , $ ; 
           cossin:cossin } ; cos(),sin()
    return,coefI
end
