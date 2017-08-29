;  library to make IQUV map
;
; version 1
; 2012.09.02 T.A.

;-
;*************************************************************************
;directory(procedure)
;mk_header.pro (function)
;hazdincli.pro (procedure)
;timestamp.pro (function)
;phase.pro (procedure)
;offsetangle.pro (function)
;rot_amp.pro (function)
;retardation.pro (function)
;demoduration.pro (procedure)
;modu_coefficent.pro (function)
;dualfit.pro (procedure)
;mm_dst.pro (function)
;dst_calibration.pro (function)
;*************************************************************************

pro directory
common pollib,dir

dir='/work1_kujira/anan/works/program_anan/DSTPOL/'

END
;*************************************************************************
;+
; NAME       : file2data.pro (function)
; PURPOSE :
; 	read data from file
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        file2data,file,data,hd,dark=dark,flat=flat 
; INPUTS :
;        file : file name
; OUTPUT :
;        data : data
;        hd   : header
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
;        dark : dark
;        flat : flat field
;        cam_id:'XEVA640' or not 
; MODIFICATION HISTORY :
;        T.A. '2011/11/04/		
;        T.A. '2011/12/27/ cam_id		
;-
;*************************************************************************
pro file2data,file,data,hd,dark=dark,flat=flat,ver=ver

mreadfits,file,hd,imgs & imgs=float(imgs)
if not keyword_set(dark) then dark=fltarr(hd[0].naxis1,hd[0].naxis2)
if not keyword_set(flat) then flat=fltarr(hd[0].naxis1,hd[0].naxis2)+1.
if not keyword_set(ver) then ver=0

case ver of
1:begin;--------------------------------
data=imgs
for i=0,hd[0].naxis3-1 do begin
    if (hd[0].DETNAM eq 'XEVA640') and keyword_set(dark) then begin
        data[0:hd[0].naxis1/2-1,*,i]=$
          float( ((imgs[0:hd[0].naxis1/2-1,*,i]-dark-351.56214)>0.)^$
                 (1./0.76143312)/59222.059d )
        data[hd[0].naxis1/2:hd[0].naxis1-1,*,i]=$
          float( ((imgs[hd[0].naxis1/2:hd[0].naxis1-1,*,i]-dark-298.70353)>0)^$
                 (1./0.77864712)/69704.267d )
        data[*,*,i]=(data[*,*,i]/flat)<7.
    endif else begin
        data[*,*,i] = (imgs[*,*,i] - dark)/flat
    endelse
endfor
end
else:begin;-----------------------------
data=imgs
for i=0,hd[0].naxis3-1 do begin
    if (hd[0].CAM_ID eq 'XEVA640') and keyword_set(dark) then begin
        data[0:hd[0].naxis1/2-1,*,i]=$
          float( ((imgs[0:hd[0].naxis1/2-1,*,i]-dark-351.56214)>0.)^$
                 (1./0.76143312)/59222.059d )
        data[hd[0].naxis1/2:hd[0].naxis1-1,*,i]=$
          float( ((imgs[hd[0].naxis1/2:hd[0].naxis1-1,*,i]-dark-298.70353)>0)^$
                 (1./0.77864712)/69704.267d )
        data[*,*,i]=(data[*,*,i]/flat)<7.
    endif else begin
        data[*,*,i] = (imgs[*,*,i] - dark)/flat
    endelse
endfor
end
endcase
END
;*************************************************************************
;+
; NAME       : mk_header.pro (function)
; PURPOSE :
; 	make a structure of camera header
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        res=mk_header(hd) 
; INPUTS :
;        hd : original header
; OUTPUT :
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2011/09/04/		
;-
;*************************************************************************
function mk_header,hd,$
              	ha=ha,zd=zd,$
		prog_ver=prog_ver,period=period
common pollib

;if not keyword_set(period) then period=507904./float(hd.mt_pulse)
if not keyword_set(ha) then ha=hd.ha
if not keyword_set(zd) then zd=hd.zd
ha=ha*1.
zd=zd*1.

case hd.cam_id of
'XEVA640':begin
	temp=long(hd.cam_temp)
	power=long(hd.cam_pwr)
	tmp=long(hd.radius)
	radius=float(tmp/100*60+(tmp mod 100))
	tmp=long(hd.pangle)
	pangle=float(tmp/100)+float(tmp mod 100)/60.
	tmp=long(hd.incli)
	incli=float(tmp/100)+float(tmp mod 100)/60.
	binx=1
	biny=1
	yyyy=string(hd.date_obs/10000,format='(i4.4)')
	mm=string((hd.date_obs mod 10000)/100,format='(i2.2)')
	dd=string(hd.date_obs mod 100,format='(i2.2)')
	hh=string(((hd.time_obs/10000)+24-9) mod 24,format='(i2.2)')
	mmm=string((hd.time_obs mod 10000)/100,format='(i2.2)')
	ss=string(hd.time_obs mod 100,format='(i2.2)')+'.000'
	date=yyyy+'-'+mm+'-'+dd+'T'+hh+':'+mmm+':'+ss
	date_obs=yyyy+'-'+mm+'-'+dd+'T'+hh+':'+mmm+':'+ss
	date_end=' '
	expo=float(hd.expo)*1e-3
	obs_mode='POL'
end
'GE1650':begin
	temp=long(-1)
	power=long(-1)
	radius=float(strmid(hd.radius,0,strpos(hd.radius,'m')))*60.+$
		float(strmid(hd.radius,strpos(hd.radius,'m')+1,strpos(hd.radius,'s')))
	pangle=float(strmid(hd.pa,0,strpos(hd.pa,'d')))+$
		float(strmid(hd.pa,strpos(hd.pa,'d')+1,strpos(hd.pa,'m')))/60.
	incli=float(strmid(hd.incli,0,strpos(hd.incli,'d')))+$
		float(strmid(hd.incli,strpos(hd.incli,'d')+1,strpos(hd.incli,'m')))/60.
	binx=hd.binx
	biny=hd.biny
	yyyymmdd=long(strmid(hd.date,0,strpos(hd.date,'_')))
	yyyy=string(yyyymmdd/10000,format='(i4.4)')
	mm=string((yyyymmdd mod 10000)/100,format='(i2.2)')
	dd=string(yyyymmdd mod 100,format='(i2.2)')
	hhmmss=long(strmid(hd.time_obs,strpos(hd.time_obs,'_')+1,strlen(hd.time_obs)))
	hh=string(((hhmmss/10000000)+24-9) mod 24,format='(i2.2)')
	mmm=string((hhmmss mod 10000000)/100000,format='(i2.2)')
	ss=hhmmss mod 100000
	ss=string(ss/1000,format='(i2.2)')+'.'+string(ss mod 1000,format='(i3.3)')
	date_obs=yyyy+'-'+mm+'-'+dd+'T'+hh+':'+mmm+':'+ss
	hhmmss=long(strmid(hd.time_end,strpos(hd.time_end,'_')+1,strlen(hd.time_end)))
	hh=string(((hhmmss/10000000)+24-9) mod 24,format='(i2.2)')
	mmm=string((hhmmss mod 10000000)/100000,format='(i2.2)')
	ss=hhmmss mod 100000
	ss=string(ss/1000,format='(i2.2)')+'.'+string(ss mod 1000,format='(i3.3)')
	date_end=yyyy+'-'+mm+'-'+dd+'T'+hh+':'+mmm+':'+ss
	date=yyyy+'-'+mm+'-'+dd+'T'+hh+':'+mmm+':'+ss
	expo=float(hd.expo)*1e-6	
	obs_mode=hd.obs_mode
end
else:print,'no detactor name'
endcase

res={dstsp_v1			,$
     SIMPLE  : 'T'   		,$
     BITPIX  : hd.BITPIX	,$
     NAXIS   : hd.NAXIS		,$
     NAXIS1  : hd.NAXIS1	,$
     NAXIS2  : hd.NAXIS2	,$
     NAXIS3  : hd.NAXIS3	,$
     EXTEND  : 'F'		,$
     BSCALE  : 1.0		,$
     BZERO   : 0.0		,$
     ORIGIN  : 'HIDA OBSERVATORY',$
     OBSERVAT: 'HIDA OBSERVATORY',$
     TELESCOP: 'DST'		,$
     INSTRUME: ' '		,$
     PROGRAM : hd.PROGRAM	,$
     PROG_VER: 0		,$
     OBS_TYPE: obs_mode		,$
     POLSTATE: hd.POLSTATE	,$
     TIMESYS : 'UTC'		,$
     DATE    : date		,$
     DATE_OBS: date_obs		,$
     DATE_END: date_end		,$
     WVPLATE : hd.wvplate	,$     
     PERIOD  : period		,$
     DETNAM  : hd.cam_id	,$            
     DET_TMP : temp		,$             
     DET_PWR : power		,$             
     EXPTIME : EXPO		,$
     CAMGAIN : hd.CAMGAIN	,$
     FGBINX  : BINX		,$
     FGBINY  : BINY		,$
     X0      : hd.CCDX0		,$
     X1      : hd.CCDX1		,$
     Y0      : hd.CCDY0		,$
     Y1      : hd.CCDY1		,$
     WAVE    : hd.WAVE		,$
     R       : RADIUS		,$
     P       : PANGLE		,$
     I       : INCLI		,$
     HA      : HA		,$
     ZD      : ZD		,$
     COMMENT : ' '		,$
     HISTORY : ' '		 $
     }

;help,res,/str
return,res
END
;*************************************************************************
;+
; NAME       : hd2angle.pro (procedure)
; PURPOSE :
; 	read angle informations from header
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        hd2angle,hd,ha,zd,r,p,i 
; INPUTS :
; 	hd   --  header of file
; OUTPUT :
;       ha   --  hour angle [rad]
;       zd   --  zenith distance [rad]
;       r    --
;       p    --
;       i    --  inclination of slit [rad]
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2011/09/05/		
;-
;*************************************************************************
pro hd2angle,hd,ha,zd,r,p,i

;zd = abs(hd.zd/3600.*!dtor)      ;rad
zd = hd.zd/3600.*!dtor      ;rad
ha = hd.ha/3600.*15.*!dtor
pos=where(ha ge !pi/2.,npos)
if npos ge 1 then ha[pos]=ha[pos]-2.*!pi ;rad
;if ha ge !pi/2. then ha=ha-2.*!pi ;rad
r=hd.r/3600.*!dtor
p=hd.p*!dtor
i=hd.i*!dtor

END

;*************************************************************************
function selectdark,darkhd,hd

res=where($
	(darkhd.detnam eq hd.detnam) and $
	(darkhd.exptime eq hd.exptime) and $
	(darkhd.camgain eq hd.camgain) and $
	(darkhd.fgbinx eq hd.fgbinx) and $
	(darkhd.fgbiny eq hd.fgbiny) and $
	(darkhd.x0 eq hd.x0) and $
	(darkhd.x1 eq hd.x1) and $
	(darkhd.y0 eq hd.y0) and $
	(darkhd.y1 eq hd.y1),$
	npos)
if npos eq 0 then begin
	res=-1
	;print,'no dark'
	;stop
endif

return,res
END
;*************************************************************************
function selectref,refhd,hd

res=where($
	(refhd.obs_type eq 'POL') and $
	(refhd.wave eq hd.wave) and $
	(refhd.wvplate eq hd.wvplate) and $
	(refhd.period eq hd.period) and $
	(refhd.detnam eq hd.detnam) and $
	(refhd.exptime eq hd.exptime) and $
	(refhd.camgain eq hd.camgain) and $
	(refhd.fgbinx eq hd.fgbinx) and $
	(refhd.fgbiny eq hd.fgbiny) and $
	(refhd.x0 eq hd.x0) and $
	(refhd.x1 eq hd.x1) and $
	(refhd.y0 eq hd.y0) and $
	(refhd.y1 eq hd.y1),$
	npos)
if npos eq 0 then begin
	res=-1
endif

return,res
END
;*************************************************************************
;+
; NAME       : timestamp.pro (function)
; PURPOSE :
; 	read time stamp [sec]
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        timestamp,file 
; INPUTS :
; 	file --  file name
;        hd  --  header
; OUTPUT :
;       res  --  time stamp [sec]
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2011/09/05/		
;-
;*************************************************************************
function timestamp,file
common pollib

dirfile=file_dirname(file)+'/'
time=strmid(file,strpos(file,'.fits')-18,18)
stmpfile=file_search(dirfile,'stmp'+time+'.fits')
mreadfits,stmpfile,h,dt
dt = ULong64(dt)
res = (dt[*,0]*(ULong64(2)^32)+dt[*,1])/  $
  79861111.d*0.999987d   & res=res-res[1] ; time stamp [sec]

return,res
END
;*************************************************************************
; NAME       : phase.pro (procedure)
; PURPOSE :
; 	culculate angles of rotating waveplate on each frames from trigger
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        phase,file,hd,ph,startframe 
; INPUTS :
;        file--  file name
;        hd  --  header
; OUTPUT :
;        ph  --  phase of rotating waveplate from trigger [rad]
;        startframe- starting frame
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
;        frate-  frame/rev
; MODIFICATION HISTORY :
;        T.A. '2011/09/11/		
;-
;*************************************************************************
pro phase,file,hd,ph,startframe,frate=frate

period=hd.period   ;sec
case hd.DETNAM of
    'GE1650':begin
        startframe=1
        time=timestamp(file)            ;sec
        ph = time[1:*]/period *2.*!pi        ;rad
    end
    'XEVA640':begin
        startframe=20
        case period of
            4:begin
                case hd.exptime of   ;msec
                    ;0.200:begin
                    0.200d:begin
                        if not keyword_set(frate) then $
                          frate=18.9798 ;20110927[frames/rev] 
                        ph=findgen(hd.naxis3-startframe)/frate *2.*!pi ;rad
                    end
                    0.100d:begin
                        if not keyword_set(frate) then $
                          frate=19.0285 ;20140708[frames/rev] 
                        ph=findgen(hd.naxis3-startframe)/frate *2.*!pi ;rad
			end
			else:help,hd.exptime
                endcase
            end
            2:begin
                case float(hd.exptime) of   ;msec
                    0.100:begin
                        if not keyword_set(frate) then $
                          frate=18.1818 ;20120613_2[frames/rev] 
                        ph=findgen(hd.naxis3-startframe)/frate *2.*!pi ;rad
                    end
                    else:stop
                endcase
            end
        endcase
    end
endcase

END
;*************************************************************************
;+
; NAME       : offsetangle.pro (function)
; PURPOSE :
; 	read offset angle [rad]
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        res=offsetangle(cam_id,WVPLATE,MT_PULSE,BINX,BINY)
; INPUTS :
;       cam_id-  camera ID
;       wvplate- name of waveplate
;       mt_pulse-# of pulse to rotate waveplate
;       binx --  X binning
;       biny --  Y binning
;       expo --  exposure time [us]
;       wave --  wavelength [A]
; OUTPUT :
;       res  --  offset angle [rad]
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2011/09/11/		
;-
;*************************************************************************
function offsetangle,cam_id,WVPLATE,MT_PULSE,BINX,BINY,expo;,wave
common pollib

restore,dir+'angle.sav'
pos=where($
           (offset_angle.CAM_ID eq CAM_ID) and $
           (offset_angle.WAVEPLATE eq WVPLATE) and $
           (offset_angle.MT_PULSE eq MT_PULSE) and $
           (offset_angle.BINX eq BINX) and $
           (offset_angle.BINY eq BINY)  $
         )
if mean(pos) eq -1 then begin
    res=0. 
endif else begin
    angle0=offset_angle[pos].coe1*double(expo)+offset_angle[pos].coe0 ;deg
    res=angle0*!dtor
endelse

;    case WVPLATE of
;        'APSAW':begin
;            restore,dir+'/APSAW.sav'
;            offset=-(interpol(smooth(apsaw_axs[*,0],10),wl,6562.81)-$
;                     interpol(smooth(apsaw_axs[*,0],10),wl,float(wave)))
;            ;if hd[0].wave eq '4227' then offset=-6.07846*!dtor ;20111003
;           ;if hd[0].wave eq '5890' then offset=0.816015*!dtor ;20111003
;           ;if hd[0].wave eq '6303' then offset=0.221827*!dtor ;20111003
;            ;if hd[0].wave eq '6563' then offset=-0.135796*!dtor ;20111003
;            ;if hd[0].wave eq '8542' then offset=0.299906*!dtor ;20111003
;            if wave eq '10830' then offset=4.27486*!dtor ;20111003
;            res=-(angle0*!dtor+offset)+!pi/2. ;rad
;        endcase
;        'Quarts':begin
;            offset=22.5*!dtor ;rad
;            res=angle0*!dtor+offset ;rad
;            res=-29.2117*!dtor  ;+offset ;rad
;            res=-29.9158*!dtor  ;+offset ;rad
;        endcase
;        'Quartz':begin
;            offset=22.5*!dtor ;rad
;            res=angle0*!dtor+offset ;rad
;            res=-29.2117*!dtor  ;+offset ;rad
;            res=-29.9158*!dtor  ;+offset ;rad
;        endcase
;        else:res=0.
;    endcase
;endelse

return,res     
END
;*************************************************************************
;+
; NAME       : rot_amp.pro (function)
; PURPOSE :
;            rotate maps derived from demodulation
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        res=offsetangle(amp,angle,ofsetangleslit)
; INPUTS :
; 	amp  --  array after demodulation
;       angle--  rotation angle ;rad
;       ofsetangleslit -- offset angle along slit
; OUTPUT :
;       res  --  amp changed phase (offset angle)
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2011/10/10/		
;-
;*************************************************************************
function rot_amp,amp,angle,ofsetangleslit

nx=(size(amp,/dim))[0]
res=amp
res[*,*,0]=amp[*,*,0]
for i=0,nx-1 do begin
    th=-(angle+ofsetangleslit[i])   ;rad
    res[i,*,1]=amp[i,*,1]*cos(1.*th)-amp[i,*,2]*sin(1.*th)
    res[i,*,2]=amp[i,*,1]*sin(1.*th)+amp[i,*,2]*cos(1.*th)
    res[i,*,3]=amp[i,*,3]*cos(2.*th)-amp[i,*,4]*sin(2.*th)
    res[i,*,4]=amp[i,*,3]*sin(2.*th)+amp[i,*,4]*cos(2.*th)
    res[i,*,5]=amp[i,*,5]*cos(4.*th)-amp[i,*,6]*sin(4.*th)
    res[i,*,6]=amp[i,*,5]*sin(4.*th)+amp[i,*,6]*cos(4.*th)
endfor

return,res
END
;*************************************************************************
;+
; NAME       : retardation.pro (function)
; PURPOSE :
; 	culculate retardation at the wavelength
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        res=retardation(hd) 
; INPUTS :
;       wave  --  waavelength [A]
;       wvplate-  name of waveplate
; OUTPUT :
;       res  --   retardation at the wavelength [rad]
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2011/09/11/		
;-
;*************************************************************************
function retardation,wave,wvplate
common pollib

wl0=float(wave)                 ;A
case wvplate of
    'APSAW':begin
        restore,dir+'/APSAW.sav'
        ret=apsaw_ret[*,0]         ;rad
    end
    'Quarts':begin
        restore,dir+'quarts_ret.sav'
        wl=wl*10.
        ret=(-ret+360.)*!dtor      ;rad
    end
endcase
res=interpol(ret,wl,wl0)

return,res
END
;*************************************************************************
;+
; NAME       : demoduration.pro (procedure)
; PURPOSE :
; 	culculate amplitude of some components on fluctuation value 
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        demodu,dat,xt,res,chi
; INPUTS :
; 	dat  --  3-d data
; 	xt   --  phase of roteting waveplate angle (rad.)
; OUTPUT :
;       res  --  array = [cc,s1,c1,s2,c2,s4,c4]
;       err  --  error of [cc,s1,c1,s2,c2,s4,c4]
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2010/11/30/		
;-
;*************************************************************************
pro demodulation,dat,xt,res,err,datfit,const=const
common pollib

sd=size(dat)
nx=sd[1]
ny=sd[2]
nt=sd[3]
res=fltarr(nx,ny,7)
err=fltarr(nx,ny,7)

ss1= sin(xt)
cc1= cos(xt)
ss2= sin(2.*xt)
cc2= cos(2.*xt)
ss4= sin(4.*xt)
cc4= cos(4.*xt)

mat=invert([$
             [nt           ,total(1.*ss1) ,total(1.*cc1) ,total(1.*ss2) ,total(1.*cc2) ,total(1.*ss4) ,total(1.*cc4) ],     $
             [total(ss1*1.),total(ss1*ss1),total(ss1*cc1),total(ss1*ss2),total(ss1*cc2),total(ss1*ss4),total(ss1*cc4)],     $
             [total(cc1*1.),total(cc1*ss1),total(cc1*cc1),total(cc1*ss2),total(cc1*cc2),total(cc1*ss4),total(cc1*cc4)],     $
             [total(ss2*1.),total(ss2*ss1),total(ss2*cc1),total(ss2*ss2),total(ss2*cc2),total(ss2*ss4),total(ss2*cc4)],     $
             [total(cc2*1.),total(cc2*ss1),total(cc2*cc1),total(cc2*ss2),total(cc2*cc2),total(cc2*ss4),total(cc2*cc4)],     $
             [total(ss4*1.),total(ss4*ss1),total(ss4*cc1),total(ss4*ss2),total(ss4*cc2),total(ss4*ss4),total(ss4*cc4)],     $
             [total(cc4*1.),total(cc4*ss1),total(cc4*cc1),total(cc4*ss2),total(cc4*cc2),total(cc4*ss4),total(cc4*cc4)]      $
             ])

rmat0=total(dat*1.,3)
for i=0,nt-1 do begin
    if i eq 0 then begin
        rmat1=dat[*,*,i]*ss1[i]
        rmat2=dat[*,*,i]*cc1[i]
        rmat3=dat[*,*,i]*ss2[i]
        rmat4=dat[*,*,i]*cc2[i]
        rmat5=dat[*,*,i]*ss4[i]
        rmat6=dat[*,*,i]*cc4[i]
    endif else begin
        rmat1=rmat1+dat[*,*,i]*ss1[i]
        rmat2=rmat2+dat[*,*,i]*cc1[i]
        rmat3=rmat3+dat[*,*,i]*ss2[i]
        rmat4=rmat4+dat[*,*,i]*cc2[i]
        rmat5=rmat5+dat[*,*,i]*ss4[i]
        rmat6=rmat6+dat[*,*,i]*cc4[i]
    endelse
endfor

cc=mat[0,0]*rmat0+mat[1,0]*rmat1+mat[2,0]*rmat2+mat[3,0]*rmat3+mat[4,0]*rmat4+mat[5,0]*rmat5+mat[6,0]*rmat6

if not keyword_set(const) then begin
    s1=mat[0,1]*rmat0+mat[1,1]*rmat1+mat[2,1]*rmat2+mat[3,1]*rmat3+mat[4,1]*rmat4+mat[5,1]*rmat5+mat[6,1]*rmat6
    c1=mat[0,2]*rmat0+mat[1,2]*rmat1+mat[2,2]*rmat2+mat[3,2]*rmat3+mat[4,2]*rmat4+mat[5,2]*rmat5+mat[6,2]*rmat6
    s2=mat[0,3]*rmat0+mat[1,3]*rmat1+mat[2,3]*rmat2+mat[3,3]*rmat3+mat[4,3]*rmat4+mat[5,3]*rmat5+mat[6,3]*rmat6
    c2=mat[0,4]*rmat0+mat[1,4]*rmat1+mat[2,4]*rmat2+mat[3,4]*rmat3+mat[4,4]*rmat4+mat[5,4]*rmat5+mat[6,4]*rmat6
    s4=mat[0,5]*rmat0+mat[1,5]*rmat1+mat[2,5]*rmat2+mat[3,5]*rmat3+mat[4,5]*rmat4+mat[5,5]*rmat5+mat[6,5]*rmat6
    c4=mat[0,6]*rmat0+mat[1,6]*rmat1+mat[2,6]*rmat2+mat[3,6]*rmat3+mat[4,6]*rmat4+mat[5,6]*rmat5+mat[6,6]*rmat6

    res[*,*,0]=cc
    res[*,*,1]=s1
    res[*,*,2]=c1
    res[*,*,3]=s2
    res[*,*,4]=c2
    res[*,*,5]=s4
    res[*,*,6]=c4
;res=[[[cc]],[[s1]],[[c1]],[[s2]],[[c2]],[[s4]],[[c4]]]
;print,'array = [cc,s1,c1,s2,c2,s4,c4]'

;===== error [gosaron-oyobi-keisanhou] S. Miyamoto p.70 ======;

    datfit = fltarr(nx,ny,nt)
    for i=0,nt-1 do datfit[*,*,i] = cc + s1*ss1[i] + c1*cc1[i] + s2*ss2[i] + c2*cc2[i] + s4*ss4[i] + c4*cc4[i]
    chi = sqrt(total((dat-datfit)^2,3)/float(nt))
    
    ecc=sqrt(total(( mat[0,0]*1.+mat[1,0]*ss1+mat[2,0]*cc1+mat[3,0]*ss2+mat[4,0]*cc2+mat[5,0]*ss4+mat[6,0]*cc4 )^2))
    es1=sqrt(total(( mat[0,1]*1.+mat[1,1]*ss1+mat[2,1]*cc1+mat[3,1]*ss2+mat[4,1]*cc2+mat[5,1]*ss4+mat[6,1]*cc4 )^2))
    ec1=sqrt(total(( mat[0,2]*1.+mat[1,2]*ss1+mat[2,2]*cc1+mat[3,2]*ss2+mat[4,2]*cc2+mat[5,2]*ss4+mat[6,2]*cc4 )^2))
    es2=sqrt(total(( mat[0,3]*1.+mat[1,3]*ss1+mat[2,3]*cc1+mat[3,3]*ss2+mat[4,3]*cc2+mat[5,3]*ss4+mat[6,3]*cc4 )^2))
    ec2=sqrt(total(( mat[0,4]*1.+mat[1,4]*ss1+mat[2,4]*cc1+mat[3,4]*ss2+mat[4,4]*cc2+mat[5,4]*ss4+mat[6,4]*cc4 )^2))
    es4=sqrt(total(( mat[0,5]*1.+mat[1,5]*ss1+mat[2,5]*cc1+mat[3,5]*ss2+mat[4,5]*cc2+mat[5,5]*ss4+mat[6,5]*cc4 )^2))
    ec4=sqrt(total(( mat[0,6]*1.+mat[1,6]*ss1+mat[2,6]*cc1+mat[3,6]*ss2+mat[4,6]*cc2+mat[5,6]*ss4+mat[6,6]*cc4 )^2))
    
    err[*,*,0]=chi*ecc
    err[*,*,1]=chi*es1
    err[*,*,2]=chi*ec1
    err[*,*,3]=chi*es2
    err[*,*,4]=chi*ec2
    err[*,*,5]=chi*es4
    err[*,*,6]=chi*ec4
endif else begin
    res=cc
    datfit = fltarr(nx,ny,nt)+res
    err=-1
endelse

end 
;*************************************************************************
;+
; NAME       : modu_coefficent.pro (function)
; PURPOSE :
; 	culculate coefficient of modulation
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        res=modu_coefficient(hd,ret,rr,dn)
; INPUTS :
;       expo  --  exposure time [us]
;       mt_pulse- # of pulse to rotate waveplate
;       wave  --  wavelength [A]
;       ret   --  retardation of waveplate [rad]
;       rr    --  inner reflection rate of waveplate ^2
;       dn    --  thickness [m]* refraction rate of waveplate
; OUTPUT :
;       res  --   coefficient of modulation
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
;       period-   period [s]
; MODIFICATION HISTORY :
;        T.A. '2011/08/13/		
;-
;*************************************************************************
function modu_coefficient,expo,period,wave,ret,rr,dn
;expo   [sec]
;period [sec]
;wave   [A]
;ret    [rad]

wl=float(wave)           ;A
ss=size(ret)
if ss[0] eq 0 then arr=0. else arr=fltarr(ss[1],ss[2])

res={coe_modu,$
     ci1:    arr,     $
     ci2:    arr,     $
     cq1:    arr,     $
     cq2:    arr,     $
     cq3:    arr,     $
     cu1:    arr,     $
     cu2:    arr,     $
     cv1:    arr     $
    }

res.ci1 = expo/2. + $
  rr *expo*cos(4.*!pi*dn/(wl*1e-10))*cos(ret)
res.ci2 = -rr *period/2./!pi*sin(2.*!pi*expo/period)*$
  sin(4.*!pi*dn/(wl*1e-10))*sin(ret)
res.cq1 = res.ci2
res.cq2 = period/8./!pi*sin(4.*!pi*expo/period) *(1.-cos(ret))/2. + $
  rr *period/4./!pi*sin(4.*!pi*expo/period)* $
  cos(4.*!pi*dn/(wl*1e-10))*sin(ret/2.)*sin(3./2.*ret)
res.cq3 = expo/2. *(1.+ cos(ret))/2. + $
  rr *expo*cos(4.*!pi*expo/period)*cos(ret/2.)*cos(3./2.*ret)
res.cu1 = res.cq1
res.cu2 = res.cq2
res.cv1 = period/4./!pi*sin(2.*!pi*expo/period)*sin(ret) + $
  rr *period/2./!pi*sin(2.*!pi*expo/period)*$
  cos(4.*!pi*expo/period)*sin(2.*ret)

return,res
END
;*************************************************************************
;+
; NAME       : modu_coefficent1.pro (function)
; PURPOSE :
; 	culculate coefficient of modulation
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        res=modu_coefficient(hd,ret,rr,dn)
; INPUTS :
;       expo  --  exposure time [s]
;       period--  period of rotating waveplate [sec]
;       wave  --  wavelength [A]
;       ret   --  retardation of waveplate [rad]
;       rr    --  inner reflection rate of waveplate ^2
;       dn    --  thickness [m]* refraction rate of waveplate
; OUTPUT :
;       res  --   coefficient of modulation
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
;       period-   period [s]
; MODIFICATION HISTORY :
;        T.A. '2011/08/13/		
;-
;*************************************************************************
pro modu_coefficient1,expo,period,wave,ret,rr,dn,$
                      ci1,ci2,cq1,cq2,cq3,cu1,cu2,cv1
;expo   [sec]
;period [sec]
;wave   [A]
;ret    [rad]

wl=float(wave)           ;A

ci1 = expo/2. + $
  rr *expo*cos(4.*!pi*dn/(wl*1e-10))*cos(ret)
ci2 = -rr *period/2./!pi*sin(2.*!pi*expo/period)*$
  sin(4.*!pi*dn/(wl*1e-10))*sin(ret)
cq1 = ci2
cq2 = period/8./!pi*sin(4.*!pi*expo/period) *(1.-cos(ret))/2. + $
  rr *period/4./!pi*sin(4.*!pi*expo/period)* $
  cos(4.*!pi*dn/(wl*1e-10))*sin(ret/2.)*sin(3./2.*ret)
cq3 = expo/2. *(1.+ cos(ret))/2. + $
  ;rr *expo*cos(4.*!pi*expo/period)*cos(ret/2.)*cos(3./2.*ret)
  rr *expo*cos(4.*!pi*dn/(wl*1e-10))*cos(ret/2.)*cos(3./2.*ret);2013.08.03
cu1 = cq1
cu2 = cq2
cv1 = period/4./!pi*sin(2.*!pi*expo/period)*sin(ret) + $
  rr *period/2./!pi*sin(2.*!pi*expo/period)*$
;  cos(4.*!pi*expo/period)*sin(2.*ret)
  cos(4.*!pi*dn/(wl*1e-10))*sin(2.*ret);2013.08.03



END
;*************************************************************************
;+
; NAME       : modu_coefficent2.pro (function)
; PURPOSE :
; 	culculate coefficient of modulation
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        res=modu_coefficient(hd,ret,rr,dn)
; INPUTS :
;       expo  --  exposure time [s]
;       period--  period of rotating waveplate [sec]
;       wave  --  wavelength [A]
;       ret   --  retardation of waveplate [rad]
;       rr    --  inner reflection rate of waveplate ^2
;       dn    --  thickness [m]* refraction rate of waveplate
; OUTPUT :
;       res  --   coefficient of modulation
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
;       period-   period [s]
; MODIFICATION HISTORY :
;        T.A. '2013/08/04/		
;-
;*************************************************************************
pro modu_coefficient2,expo,period,wave,ret,rr,dn,$
                      ci1,ci2,cq1,cq2,cq3,cu1,cu2,cv1
;expo   [sec]
;period [sec]
;wave   [A]
;ret    [rad]

wl=float(wave)           ;A

ci1 = expo/2. + $
  rr *expo*cos(4.*!pi*dn/(wl*1e-10))*cos(ret)
ci2 = -rr *period/2./!pi*sin(2.*!pi*expo/period)*$
  sin(4.*!pi*dn/(wl*1e-10))*sin(ret)
cq1 = ci2
cq2 = period/8./!pi*sin(4.*!pi*expo/period) *(1.-cos(ret))/2. + $
  rr *period/4./!pi*sin(4.*!pi*expo/period)* $
  cos(4.*!pi*dn/(wl*1e-10))*sin(ret/2.)*sin(3./2.*ret)
cq3 = expo/2. *(1.+ cos(ret))/2. + $
  rr *expo*cos(4.*!pi*dn/(wl*1e-10))*cos(ret/2.)*cos(3./2.*ret)
cu1 = cq1
cu2 = cq2
cv1 = - ( period/4./!pi*sin(2.*!pi*expo/period)*sin(ret) + $
  rr *period/2./!pi*sin(2.*!pi*expo/period)*$
  cos(4.*!pi*dn/(wl*1e-10))*sin(2.*ret))



END
;*************************************************************************
;+
; NAME       : dualfit.pro (procedure)
; PURPOSE :
; 	fit dual beam
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        dualfit,dualfitfile,arr,lpr,pmr
; INPUTS :
;        dualfitfile -- save file made by ta_poldualfit
;        arr         -- 3D array
;        hd          -- header
;        ratio       -- ratio of transparent VS
; OUTPUT :
;        lpr         -- left + right
;        lmr         -- left - right
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2011/09/10/		
;-
;*************************************************************************
pro dualfit,arr,ratio,kx,ky,lpr,lmr
;dualfitfile='/tmp_mnt/work_dstraid/anan/save/dstvspol/dualfit/20110906.sav'

sa=size(arr,/dim) & nx=sa[0] & ny=sa[1] & nt=sa[2]
lpr=fltarr(nx/2,ny,nt) & lmr=fltarr(nx/2,ny,nt)
for i=0,nt-1 do begin
    lpr[*,*,i]=arr[0:nx/2-1,*,i]+ratio*poly_2d(arr[nx/2:nx-1,*,i],kx,ky)
    lmr[*,*,i]=arr[0:nx/2-1,*,i]-ratio*poly_2d(arr[nx/2:nx-1,*,i],kx,ky)
endfor

END
;*************************************************************************
;+
; NAME       : dstsp_dualfit.pro (procedure)
; PURPOSE :
; 	fit dual beam
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        dstsp_dualfit,iquv0,kx,ky,ratio
; INPUTS :
;        iquv0       -- array, [[[I]],[[qq]],[[uu]],[[vv]]]
;        kx,ky       -- output of poly_warp
;        ratio       -- ratio of transparent VS
; OUTPUT :
;        lpr         -- left + right
;        lmr         -- left - right
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2011/09/10/		
;-
;*************************************************************************
function dstsp_dualfit,iquv0,kx,ky,ratio,minus=minus,intensity=intensity,deg=deg

if not keyword_set(deg) then deg=1

if keyword_set(intensity) then begin
	sa=size(iquv0,/dim) & nx=sa[0] & ny=sa[1]
	res=fltarr(nx/2,ny)
	res[*,*]=iquv0[0:nx/2-1,*]+ratio*poly_2d(iquv0[nx/2:nx-1,*],kx,ky,deg)
	goto,jump
endif
sa=size(iquv0,/dim) & nx=sa[0] & ny=sa[1] & nt=sa[2]
res=fltarr(nx/2,ny,4)
res[*,*,0]=iquv0[0:nx/2-1,*,0]+ratio*poly_2d(iquv0[nx/2:nx-1,*,0],kx,ky,deg)
if not keyword_set(minus) then begin
for i=1,3 do $
  res[*,*,i]=iquv0[0:nx/2-1,*,i]-ratio*poly_2d(iquv0[nx/2:nx-1,*,i],kx,ky,deg)
endif else begin
for i=1,3 do $
  res[*,*,i]=-iquv0[0:nx/2-1,*,i]+ratio*poly_2d(iquv0[nx/2:nx-1,*,i],kx,ky,deg)
endelse

jump:
res=res/2.

return,res
END
;*************************************************************************
;+
; NAME       : muellermatrix_mirr.pro (function)
; PURPOSE :
; return normalized Mueller matrix for a mirror reflection
;positive Q-direction is in the plane of incidence
; CATEGORY :
;        idlpro/optic/ray/lib
; CALLING SEQUENCE :
;        mat = muellermatrix_mirr(delta,X)
; INPUTS :
;       delta   --  
;       X       --
;       //delta,X are function of the incident angle,N=n+ik.
; OUTPUT :
; OPTIONAL INPUT PARAMETERS :
;       gen     -- general form 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '09/08/24      ; Stenflo "Solar Magnetic Field", p320.
;        T.A. '11/06/14      ; general form
;*************************************************************************

function muellermatrix_mirr,tau,ro,gen=gen

tau	= float(tau)
ro	= float(ro)

if not keyword_set(gen) then begin
    mat	= 0.5*[ $
                [ro^2+1.,  ro^2-1.,               0.,              0.],     $
	  	[ro^2-1.,  ro^2+1.,               0.,              0.],     $
                [0.,            0.,  -2.*ro*cos(tau),  2.*ro*sin(tau)],     $
  	  	[0.,            0.,  -2.*ro*sin(tau), -2.*ro*cos(tau)]      $
    		]     
endif else begin
    mat = 1./(1.+ro)*[$
                       [1.,ro,0.,0.],$
                       [ro,1.,0.,0.],$
                       [0.,0.,-sqrt(1.-ro^2)*cos(tau),-sqrt(1.-ro^2)*sin(tau)],$
                       [0.,0.,sqrt(1.-ro^2)*sin(tau),-sqrt(1.-ro^2)*cos(tau)] $
                       ]
endelse


return,mat

end
;*************************************************************************
;+
; NAME       : muellermatrix_wp.pro (function)
; PURPOSE :
; 	return Mueller matrix of linear retarder
; CATEGORY :
;        idlpro/optic/raylib
; CALLING SEQUENCE :
;        mat = muellermatrix_wp(del,phai,/jones,ref=ref,thick=thick,wv=wv)
; INPUTS :
; 	del  --  retardance (rad.)
; 	phai --  angle of the axis (rad., counter clockwise)
;	jones -  return Jones vector
;       ref  --  reflective index
;       thick -  thickness of wave plate (mm)
;       wv    -  wave length (nm)
; OUTPUT :
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        k.i. '95/10/26  from lwp.pro
;        k.i. '96/02/17		jones keyword
;        T.A. '10/03/16		ref,thick,wv keyword
;        T.A. '10/04/07		modificate L61 and keywords(thck,wvl)
;-
;*************************************************************************
function muellermatrix_wp,del,phai,jones=jones,ref=ref,thick=thck,wv=wvl

if not keyword_set(jones) then begin	; Mueller matrix
    c2 = cos(2.*phai)                                                      
    s2 = sin(2.*phai)  
    cd = cos(del)
    sd = sin(del)

    if not keyword_set(ref) then begin	; no reflection
 	mat=[ [	1.,	0.,		0.,		0.	],	$
      	      [ 0.,	c2^2+s2^2*cd,	s2*c2*(1.-cd),	-s2*sd	],	$
              [ 0.,	s2*c2*(1.-cd),	s2^2+c2^2*cd,	c2*sd],	$
              [	0.,	s2*sd,		-c2*sd,		cd	] ]
    endif else begin	; reflection
    	if (not keyword_set(thck)) or (not keyword_set(wvl)) then begin
            print,'you must input two keywords "thick [mm]" and "wv [nm]"'
            mat = -1
        endif else begin
            ref=ref*1.
            thick=thck*10.^(-3)	;[m]		
            wv=wvl*10.^(-9)		;[m]
            rr  = 2.*((1.-ref)/(1.+ref))^2
            clm = cos(4.*!pi*thick*ref/wv)
            slm = sin(4.*!pi*thick*ref/wv)
            c2d = cos(2.*del)	
            s2d = sin(2.*del)	
            f11 = rr*clm*cd+1.
            f12 = -1.*rr*slm*sd
            f33 = rr*clm*c2d+cd
            f43 = -rr*clm*s2d-sd

            mat0=[ [	f11,	f12*c2,			f12*s2,			0.	],	$
                  [ f12*c2,	f11*c2^2+f33*s2^2,	s2*c2*(f11-f33),	f43*s2],	$
                  [ f12*s2,	s2*c2*(f11-f33),	f11*s2^2+f33*c2^2,	-f43*c2	],	$
                   [	0.,	-f43*s2,		f43*c2,	        	f33	] ]
            mat = (1.-(1.-ref)^2/(1.+ref)^2)^2 *mat0
        endelse
    endelse

endif else begin	; Jones matrix
    c1=cos(phai)
    s1=sin(phai)
    i=complex(0,1)

    if not keyword_set(ref) then begin	; no reflection
    	edel=exp(-i*del)	; sign ok?
    	m11=c1^2+edel*s1^2
    	m22=s1^2+edel*c1^2
    	m12=(1.-edel)*c1*s1
    	mat=[ [ 	m11,	m12 ],	$
		[	m12,	m22 ] ]
    ;mat=mat*conj(m11)/abs(m11)
    endif else begin	; reflection
    	if (not keyword_set(thck)) or (not keyword_set(wvl)) then begin
            print,'you must input two keywords "thick [mm]" and "wv [nm]"'
            mat = -1
        endif else begin
            ref  = ref*1.
            thick= thck*10.^(-3)	;[m]
            wv   = wvl*10.^(-9)		;[m]
            lx   = (4.*!pi*thick*ref/wv+del)*0.5    ; x is fast axis
            ly   = (4.*!pi*thick*ref/wv-del)*0.5
            rr   = (1.-ref)^2/(1.+ref)^2
            fx   = rr*exp(3.*i*lx)+exp(i*lx)
            fy   = rr*exp(3.*i*ly)+exp(i*ly)
            m11  = fx*c1^2+fy*s1^2
            m22  = fx*s1^2+fy*c1^2
            m12  = fx*s1*c1-fy*c1*s1
            
            mat0  = [ [ m11,	m12 ],	$
		     [	m12,	m22 ] ]
            mat = (1-(1.-ref)^2/(1.+ref)^2) *mat0
        endelse
    endelse
endelse

return,mat
end
;*************************************************************************
;+
; NAME       : muellermatrix_rot.pro (finction)
; PURPOSE :
; 	return Mueller matrix for axis rotation
; CALLING SEQUENCE :
;        mat = m_rotaxis(phai,/jones)
; INPUTS :
; 	phai --  angle of axis rotation 
;		(rad., counterclockwise when we view towards the sun)
; OUTPUT :
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;      T.A. '09/08/23
;*******************************************************************
function muellermatrix_rot,phai

c2=cos(2.*phai)
s2=sin(2.*phai)
mat=[$
      [1.,	0.,	0.,	0.],	$
      [0.,	c2,	s2,	0.],	$
      [0.,	-s2,	c2,	0.],	$
      [0.,      0.,	0.,	1.] ]

return,mat

end
;+
; NAME       : mm_dst.pro (function)
; PURPOSE :
; 	return Mueller matrix for DST model
; CALLING SEQUENCE :
;        res=m_dst1(ha,zd,incli,telpos,ro_N,tau_N,ro_C,tau_C,sc,
;                     th_en,del_en,th_ex,del_ex)
; INPUTS :
; OUTPUT :
;	ro_N	ro of Newton mirror
;	tau_N	tau of Newton mirror
;	ro_C	ro of Coude mirror
;	tau_N	tau of Coude mirror
;	ha	hour angle
;	zd	zenith distance
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;      T.A. '11/06/14
;      T.A. '12/06/03  phi_N=za => -za
;      T.A. '12/06/16  sign
;      T.A. '13/08/04  keyword newton
;      T.A. '13/08/17  sign of zd
;
;*******************************************************************
function mm_dst,hd,par,newton=newton,phin=phin

if hd.zd ge 0 then telpos='west' else telpos='east'
hd2angle,hd,ha,zd,r,p,incli
lat=36.252/!radeg		;Hidaten
zd=abs(zd)			;20130817
za=asin(cos(lat)*sin(ha)/sin(zd))
phi_N=za
if telpos eq 'west' then begin
    phi_C=-zd
    phi_v=+zd-za+incli
endif else begin
    phi_C=+zd
    phi_v=-zd-za+incli
endelse

M_S=[$
	[1.+par.sc ,0.,0.,0.],	$
	[0.    ,1.,0.,0.],	$
	[0.    ,0.,1.,0.],	$
	[0.    ,0.,0.,1.]	$
	]
M_p=[$
	[1.,0.,0.,0.],	$
	[0.,1.,0.,0.],	$
	[0.,0.,-1.,0.],	$
	[0.,0.,0.,-1.]	$
	]
M_G=M_p
M_N=muellermatrix_mirr(par.tn,par.xn,/gen)
M_C=muellermatrix_mirr(par.tc,par.xc,/gen)
D_en=Muellermatrix_WP(par.dlen,par.t_en)
D_ex=Muellermatrix_WP(par.dlex,par.t_ex)
R_N=muellermatrix_rot(phi_N)
R_C=muellermatrix_rot(phi_C)
R_pl=muellermatrix_rot(phi_v)

mat=M_S##R_pl##D_ex##M_C##M_G##R_C##M_N##M_P##D_en##R_N
if keyword_set(newton) then mat=M_S##R_pl##D_ex##M_C##M_G##R_C##M_N##M_P##D_en
phin=phi_N

return,mat
END
;*************************************************************************
;+
; NAME       : m44_iquv.pro (function)
; PURPOSE :
; 	calibrate matrix(4*4) ## iquv
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        res=m44_iquv(mm,iquv)
; INPUTS :
;        mm  -- matrix ;array(4,4)
;        iquv-- iquv ;[[[ii]],[[qq]],[[uu]],[[vv]]]
; OUTPUT :
;        res -- iquv=[[[ii]],[[qq]],[[uu]],[[vv]]]
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2011/10/10/		
;-
;*************************************************************************
function m44_iquv,mm,iquv
common pollib

ii=mm[0,0]*iquv[*,*,0]+mm[1,0]*iquv[*,*,1]+mm[2,0]*iquv[*,*,2]+mm[3,0]*iquv[*,*,3]
qq=mm[0,1]*iquv[*,*,0]+mm[1,1]*iquv[*,*,1]+mm[2,1]*iquv[*,*,2]+mm[3,1]*iquv[*,*,3]
uu=mm[0,2]*iquv[*,*,0]+mm[1,2]*iquv[*,*,1]+mm[2,2]*iquv[*,*,2]+mm[3,2]*iquv[*,*,3]
vv=mm[0,3]*iquv[*,*,0]+mm[1,3]*iquv[*,*,1]+mm[2,3]*iquv[*,*,2]+mm[3,3]*iquv[*,*,3]
res=[[[ii]],[[qq]],[[uu]],[[vv]]]

return,res
END
;*************************************************************************
;+
; NAME       : par_dst.pro (function)
; PURPOSE :
; 	parameters of DST instrumental polarization
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        res=par_dst(wave,telpos)
; INPUTS :
;        wave-- wavelength [A]
;        telpos-telescope position, 'west' or 'east'
; OUTPUT :
;        res -- structure of result
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2011/11/23/		
;-
;*************************************************************************
function par_dst,hd,telpos=telpos

wave=float(hd.wave)
if not keyword_set(telpos) then begin
if hd.zd ge 0 then telpos='west' else telpos='east'
endif


ref=[$
      ;[0,10830.,-0.0236,  9.544*!dtor, 0.0219, 5.566*!dtor,0.0000],$ ;20120429
      [0,10830.,-0.0236, -9.547*!dtor, 0.0219,-5.561*!dtor,0.0000],$ ;20120429
      [0,10050.,-0.0275, -9.713*!dtor, 0.0270, 2.268*!dtor,0.0045],$ ;20120614
      [0, 9016.,-0.0417, -8.646*!dtor, 0.0246,16.335*!dtor,0.0499],$ ;20120614
      [0, 8662.,-0.0477, -9.878*!dtor, 0.0153,21.319*!dtor,0.0035],$ ;20120614
      [0, 8542.,-0.0512,-11.038*!dtor, 0.0113,23.180*!dtor,0.0200],$ ;20120614
      [0, 8498.,-0.0505,-10.610*!dtor, 0.0097,23.454*!dtor,0.0078],$ ;20120614
      [0, 8392.,-0.0505,-10.262*!dtor, 0.0062,24.779*!dtor,0.0005],$ ;20120614
      [0, 6563.,-0.0419,-17.057*!dtor,-0.0399,29.294*!dtor,0.0096],$ ;20120625
      [1, 6563.,-0.0417,-15.707*!dtor,-0.0392,30.443*!dtor,0.0084],$ ;20120625
      [0, 6303.,-0.0406,-17.693*!dtor,-0.0385,26.178*!dtor,0.0146],$ ;20120625
      [1, 6303.,-0.0407,-16.301*!dtor,-0.0378,27.453*!dtor,0.0041],$ ;20120625
      [0, 5890.,-0.0371,-19.969*!dtor,-0.0368,20.876*!dtor,0.0227],$ ;20120625
      [1, 5890.,-0.0390,-17.711*!dtor,-0.0340,21.373*!dtor,0.0000],$ ;20120625
      [0, 5100.,-0.0369,-21.639*!dtor,-0.0271, 7.647*!dtor,0.0177],$ ;20120625
      [1, 5100.,-0.0374,-20.579*!dtor,-0.0276, 9.103*!dtor,0.0000],$ ;20120625
      [0, 4861.,-0.0372,-22.886*!dtor,-0.0252, 1.075*!dtor,0.0405],$ ;20120625
      [1, 4861.,-0.0365,-21.858*!dtor,-0.0251, 2.737*!dtor,0.0000],$ ;20120625
      [0, 4340.,-0.0394,-24.283*!dtor,-0.0212,-9.910*!dtor,0.0548],$ ;20120625
      [1, 4340.,-0.0400,-22.434*!dtor,-0.0265,-7.655*!dtor,0.0000],$ ;20120625
      [0, 4101.,-0.0440,-25.292*!dtor,-0.0211,-14.340*!dtor,0.1317]$ ;20120625
      ;[0,10050.,-0.0274,  9.713*!dtor, 0.0270,-2.268*!dtor,0.0045],$ ;20120614&correct V
      ;[0, 9016.,-0.0417,  8.646*!dtor, 0.0246,-16.335*!dtor,0.0499],$ ;20120614
      ;[0, 8662.,-0.0477,  9.878*!dtor, 0.0153,-21.319*!dtor,0.0199],$ ;20120614
      ;[0, 8542.,-0.0512, 11.038*!dtor, 0.0113,-23.180*!dtor,0.0200],$ ;20120614
      ;[0, 8498.,-0.0505, 10.610*!dtor, 0.0097,-23.454*!dtor,0.0229],$ ;20120614
      ;[0, 8392.,-0.0505, 10.262*!dtor, 0.0062,-24.779*!dtor,0.0213],$ ;20120614
      ;[0, 6563.,-0.0419, 17.057*!dtor,-0.0399,-29.294*!dtor,0.0096],$ ;20120625
      ;[1, 6563.,-0.0417, 15.707*!dtor,-0.0392,-30.443*!dtor,0.0084],$ ;20120625
      ;[0, 6303.,-0.0406, 17.693*!dtor,-0.0385,-26.178*!dtor,0.0146],$ ;20120625
      ;[1, 6303.,-0.0407, 16.301*!dtor,-0.0378,-27.453*!dtor,0.0041],$ ;20120625
      ;[0, 5890.,-0.0371, 19.969*!dtor,-0.0368,-20.876*!dtor,0.0227],$ ;20120625
      ;[1, 5890.,-0.0390, 17.711*!dtor,-0.0340,-21.373*!dtor,0.0000],$ ;20120625
      ;[0, 5100.,-0.0369, 21.639*!dtor,-0.0271, -7.647*!dtor,0.0177],$ ;20120625
      ;[1, 5100.,-0.0374, 20.579*!dtor,-0.0276, -9.103*!dtor,0.0000],$ ;20120625
      ;[0, 4861.,-0.0372, 22.886*!dtor,-0.0252, -1.075*!dtor,0.0405],$ ;20120625
      ;[1, 4861.,-0.0365, 21.858*!dtor,-0.0251, -2.737*!dtor,0.0000],$ ;20120625
      ;[0, 4340.,-0.0394, 24.283*!dtor,-0.0212,  9.910*!dtor,0.0548],$ ;20120625
      ;[1, 4340.,-0.0400, 22.434*!dtor,-0.0265,  7.655*!dtor,0.0000],$ ;20120625
      ;[0, 4101.,-0.0440, 25.292*!dtor,-0.0211, 14.340*!dtor,0.1317]$ ;20120625
    ]

if telpos eq 'west' then $
  pos=where(ref[0,*] eq 0) else $
  pos=where(ref[0,*] eq 1)
res={par_dst,$
     xn      : interpol(ref[2,pos],ref[1,pos],wave),$
     tn      : interpol(ref[3,pos],ref[1,pos],wave),$
     xc      : interpol(ref[4,pos],ref[1,pos],wave),$   
     tc      : interpol(ref[5,pos],ref[1,pos],wave),$
     sc      : interpol(ref[6,pos],ref[1,pos],wave),$ 
     t_en    : 0.    ,$
     dlen    : 0.    ,$
     t_ex    : 0.    ,$
     dlex    : 0.    $ 
     }

return,res
END
;*************************************************************************
;+
; NAME       : fun_ct.pro (function)
;-
;*************************************************************************
function fun_ct,x,p

weight=10
nd	= size(x,/dim)	& nd = nd[0]
phic 	= x[*,0]
key	= x[*,1]
ymod	= dblarr(nd)

for i=0,nd-1 do begin
    case key[i] of
        0:ymod0= ( p[2]+p[0]*cos(2.*phic[i]) )*weight 
        1:ymod0= ( -sqrt(1.-p[2]^2)*p[0]*cos(p[4])*sin(2.*phic[i]) )*weight 
        2:ymod0= ( sqrt(1.-p[2]^2)*p[0]*sin(p[4])*sin(2.*phic[i]) )*weight 
        3:ymod0= sqrt(1.-p[0]^2)*sin(p[1])*sin(2.*phic[i])
        4:ymod0= sqrt((1.-p[0])*(1.-p[2]))*$
          (sin(p[1]+p[3])*cos(phic[i])^2 + sin(p[3]-p[1])*sin(phic[i])^2)
        5:ymod0= sqrt(1.-p[2]^2)*sin(p[4])*sin(2.*phic[i])
        6:ymod0= sqrt((1.-p[0])*(1.-p[2]))*$
          (-sin(p[1]+p[3])*cos(phic[i])^2 + sin(p[3]-p[1])*sin(phic[i])^2)
    endcase
    ymod[i]=ymod0/(1.+p[0])/(1.+p[2])
endfor

return,ymod
END
;*************************************************************************
;+
; NAME       : crosstalk.pro (function)
; PURPOSE :
; 	parameters of DST instrumental polarization abter calibrate
; 	for crosstalk
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        res=crosstalk(hd,i2q,i2u,i2v,v2q=v2q,v2u=v2u)
; INPUTS :
;        hd  -- header
;        i2q -- crosstalk of (I=>Q)
;        i2u -- crosstalk of (I=>U)
;        i2v -- crosstalk of (I=>V)
;        v2q -- crosstalk of (V=>Q)
;        v2u -- crosstalk of (V=>U)
;        q2v -- crosstalk of (Q=>V)
;        u2v -- crosstalk of (U=>V)
; OUTPUT :
;        res -- structure of result
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2011/11/23/		
;-
;*************************************************************************
function crosstalk,hd,i2q,i2u,i2v,v2q=v2q,v2u=v2u,q2v=q2v,u2v=u2v

weight=10
nk = 7
nd =(size(hd))[1]
key= [replicate(0,nd),replicate(1,nd),replicate(2,nd)]
;key= [replicate(1,nd),replicate(2,nd)]

;if not keyword_set(i2q) then begin
;    i2q=fltarr(nd)
;    nk=nk-1
;endif else key=[key,replicate(0,nd)]

if not keyword_set(v2q) then begin
    v2q=fltarr(nd)
    nk=nk-1
endif else key=[key,replicate(3,nd)]

if not keyword_set(v2u) then begin
    v2u=fltarr(nd)
    nk=nk-1
endif else key=[key,replicate(4,nd)]

if not keyword_set(q2v) then begin
    q2v=fltarr(nd)
    nk=nk-1
endif else key=[key,replicate(5,nd)]

if not keyword_set(u2v) then begin
    u2v=fltarr(nd)
    nk=nk-1
endif else key=[key,replicate(6,nd)]

phic= fltarr(nd*nk)
yy = fltarr(nd*nk)

par=par_dst(hd[0].wave) & res=par
lat=36.252/!radeg		;Hidaten
for i=0,nd-1 do begin
    hazdincli,hd[i],ha,zd,incli
    za=asin(cos(lat)*sin(ha)/sin(zd))
    if hd[i].position eq 'west' then begin
        phin=za
        phic0=-zd
        phiv=+zd-za+incli
    endif else begin
        phin=za
        phic0=+zd
        phiv=-zd-za+incli
    endelse

    mm = mm_dst(ha,zd,incli,hd[i].position,par) ## $
      invert([$
               [1.     ,0.,0.,0.],$
               [-i2q[i],1.,0.,-v2q[i]],$
               [-i2u[i],0.,1.,-v2u[i]],$
               [-i2v[i],-q2v[i],-u2v[i],1.]$
             ])
    ct = muellermatrix_rot(-phiv) ## mm ## muellermatrix_rot(-phin)

    for j=0,nk-1 do begin
        phic[i+nd*j]=phic0
        case key[i+j*nd] of
            0:yy[i+j*nd]=ct[0,1]*weight
            1:yy[i+j*nd]=ct[0,2]*weight
            2:yy[i+j*nd]=ct[0,3]*weight
            3:yy[i+j*nd]=ct[3,1]
            4:yy[i+j*nd]=ct[3,2]
            5:yy[i+j*nd]=ct[1,3]
            6:yy[i+j*nd]=ct[2,3]
            else:print,'else'
        endcase
    endfor
endfor

parinfo = replicate({value:0.D,fixed:0,  $
		limited:[0,0],limits:[0.D,0],step:0.d},5)
parinfo[0].step         = 0  ;pn
parinfo[1].step 	= 0  ;tn
parinfo[2].step 	= 0  ;pc
parinfo[3].step 	= 0  ;tc
parinfo[4].step 	= 0  ;s
parinfo[0].limited[*] 	= 1
parinfo[1].limited[*] 	= 0
parinfo[2].limited[*] 	= 1
parinfo[3].limited[*]	= 0
parinfo[4].limited[*]	= 1
parinfo[0].limits[*]	= [-1.D,1.D]
parinfo[2].limits[*]	= [-1.D,1.D]
parinfo[4].limits[*]	= [0.D,1.D]
parinfo[0].fixed        = 0
parinfo[1].fixed        = 0
parinfo[2].fixed        = 0
parinfo[3].fixed        = 0
parinfo[4].fixed        = 0
parinfo[*].value        = [par.xn,par.tn,par.xc,par.tc,par.sc]
sy  = .001
res0= mpfitfun('fun_ct',[[phic],[key]],yy,sy,parinfo[*].value,$
               yfit=yfit,errmsg=errmsg,STATUS=status,$
               GTOL=1d-10,parinfo=parinfo,perror=perror)
print,status,'msg=',errmsg
print,perror
res.xn=res0[0]
res.tn=res0[1]
res.xc=res0[2]
res.tc=res0[3]
res.sc=res0[4]

yfit0=fun_ct([[phic],[key]],parinfo[*].value)
wdef,1
set_line_color
plot,yy,psym=1,xr=[-1,5*nd+1],/xstyle
oplot,yfit0,line=1
oplot,yfit,line=0,color=3
loadct,0
print,'old param.',[par.xn,par.tn,par.xc,par.tc,par.sc]
print,'new param.',res

return,res
END
;*************************************************************************
function ref2irarp,file,dark,kx,ky,mq=mq,single=single,$
                     header=header,check=check,array=array,ver=ver


directory
if not keyword_set(ver) then ver=0
file2data,file,data,hdn,dark=dark,ver=ver
if keyword_set(header) then hd=header else hd=hdn[0]
nx=hd.naxis1 & ny=hd.naxis2 & nt=hd.naxis3
img=(reform(rebin(data,nx,ny,1),nx,ny))[0:nx/2-1,*]
phase,file,hd,ph,startframe
demodulation,data[*,*,startframe:nt-1],ph,amp,err,datfit

amp4=sqrt(amp[*,*,5]^2+amp[*,*,6]^2)

if not keyword_set(single) then begin;----------------------------------
;ratio
ratio=amp4[0:nx/2-1,*]/poly_2d(amp4[nx/2:nx-1,*],kx,ky,1)
;axis
axis0=atan(amp[*,*,5],amp[*,*,6])/4.
if keyword_set(mq) then $
  axis0[0:nx/2-1,*]=axis0[0:nx/2-1,*]+!pi/4. else $
  axis0[nx/2:nx-1,*]=axis0[nx/2:nx-1,*]+!pi/4.
while abs(median(axis0[0:nx/2-1,*])-median(axis0[nx/2:nx-1,*])) ge !pi/4 do begin
    if (median(axis0[0:nx/2-1,*])-median(axis0[nx/2:nx-1,*])) ge !pi/4 then $
      axis0[nx/2:nx-1,*]=axis0[nx/2:nx-1,*]+!pi/2.
    if (median(axis0[0:nx/2-1,*])-median(axis0[nx/2:nx-1,*])) le -!pi/4 then $
      axis0[0:nx/2-1,*]=axis0[0:nx/2-1,*]+!pi/2.
endwhile
axis=((axis0[0:nx/2-1,*]+poly_2d(axis0[nx/2:nx-1,*],kx,ky,1))/2.) mod (!pi/2)

;retardation, pd of polarizer
coe=hd[0].period/4./!pi/hd[0].exptime*$
  sin(4.*!pi*hd[0].exptime/hd[0].period)
if not keyword_set(mq) then begin
    xp=coe*(amp[*,*,0]/amp4)[0:nx/2-1,*]
    xm=poly_2d(coe*(amp[*,*,0]/amp4)[nx/2:nx-1,*],kx,ky,1)
endif else begin
    xp=poly_2d(coe*(amp[*,*,0]/amp4)[nx/2:nx-1,*],kx,ky,1)
    xm=coe*(amp[*,*,0]/amp4)[0:nx/2-1,*]
endelse
ret=acos(((xp-xm-2.)/(xp-xm+2.))<1.>(-1.))
pd=(xp-xm+2.)/(xp+xm)
    ;ret=total(minmax(rebin(resarr[x1:x2,*],1,ny)))/2.    fringe
xx=where(rebin(img,nx/2,1) ge mean(rebin(img,nx/2,1)))
    ;if mean(ret[xx,*]) ge 1 then begin
    ;    pd[*]=1
    ;    ret=acos((xp-3.)/(xp+1.))
    ;endif
if mean(where(pd[xx,*] ge 1)) ne -1 then begin
	pd[where(pd ge 1)]=1.
	ret[where(pd ge 1)]=$
          (acos((xp-3.)/(xp+1.)))[where(pd ge 1)]
endif
endif else begin;-----------------------------------------------------------

	;ratio
	ratio=fltarr(nx/2,ny)-1.

	;axis
	axis0=atan(amp[*,*,5],amp[*,*,6])/4.
	if keyword_set(mq) then $
	  axis0[0:nx/2-1,*]=axis0[0:nx/2-1,*]+!pi/4. else $
	  axis0[nx/2:nx-1,*]=axis0[nx/2:nx-1,*]+!pi/4.
	while abs(median(axis0[0:nx/2-1,*])-median(axis0[nx/2:nx-1,*])) ge !pi/4 do begin
	    if (median(axis0[0:nx/2-1,*])-median(axis0[nx/2:nx-1,*])) ge !pi/4 then $
	      axis0[nx/2:nx-1,*]=axis0[nx/2:nx-1,*]+!pi/2.
	    if (median(axis0[0:nx/2-1,*])-median(axis0[nx/2:nx-1,*])) le -!pi/4 then $
	      axis0[0:nx/2-1,*]=axis0[0:nx/2-1,*]+!pi/2.
	endwhile
	axis=axis0[0:nx/2-1,*] mod (!pi/2)

	;retardation, pd of polarizer
	coe=hd[0].period/4./!pi/hd[0].exptime*$
	  sin(4.*!pi*hd[0].exptime/hd[0].period)
	if not keyword_set(mq) then begin
	    xp=coe*(amp[*,*,0]/amp4)[0:nx/2-1,*]
	endif else begin
	    xp=coe*(amp[*,*,0]/amp4)[nx/2:nx-1,*]
	endelse
	pd=fltarr(nx/2,ny)+1.
	ret=acos((xp-3.)/(xp+1.))
endelse
print, 'img ratio axis ret pd'
res=[[[img]],[[ratio]],[[axis]],[[ret]],[[pd]]]

if keyword_set(check) then begin
    print,'check the offset angle of dual beam'
    plot_image,axis0
    ans=''
    read,ans
endif

if keyword_set(array) then goto,jump

res1=fltarr(5)
prof=reform(rebin(res[*,*,0],nx/2,1,1))
pos=where(prof ge 0.9*median(prof))
for i=0,4 do res1[i]=median(res[pos,*,i])
res=res1


jump:
;stop
return,res
END
;*************************************************************************
;+
; NAME       : nofringe_sin2.pro (function)
; PURPOSE :
;          delete fringe from amp. of sin(2th)
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        res=nofringe_sin2(amp,dth)
; INPUTS :
;        amp  -- answer derived from demoduration
;        dth  -- width of searching angle [rad]
; OUTPUT :
;        res -- amp
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2011/11/10/		
;-
;*************************************************************************
function nofringe_sin2,amp,dth
common pollib

ss=size(amp) & nx=ss[1] & ny=ss[2] & nn=ss[4] & if ss[0] eq 3 then nn=1
amp1=reform(amp[*,*,*,0],nx,ny,7)
stepth=0.05*!dtor
nstep=long(dth/stepth)*2+1
ths=(findgen(nstep)-(nstep-1)/2)*stepth ;rad
xx=where(total(amp[0:nx/2,*,0],2) ge .5*mean(total(amp[0:nx/2,*,0],2)))
chi=fltarr(nstep)
res=amp
res[*,*,0,*]=amp[*,*,0,*]
for j=0,nn-1 do begin
    for i=0,nstep-1 do begin
        s2=amp[*,*,3,j]*cos(2.*ths[i])-amp[*,*,4,j]*sin(2.*ths[i])
        chi[i]=stddev(s2[xx,*])
    endfor
    tmp=min(chi,pos)
    nn=3
    if (pos ge nn-1) and (pos le nstep-3) then begin
        coe=poly_fit(ths[mean(pos)-nn:mean(pos)+nn],$
                     chi[mean(pos)-nn:mean(pos)+nn],2)
        th=-coe[1]/2./coe[2] & print,'offset',th*!radeg,' deg'
    endif else begin
	th=0.
    	print,'we need more large range, in nofringe_sin2'
    endelse
    res[*,*,1,j]=amp[*,*,1,j]*cos(1.*th)-amp[*,*,2,j]*sin(1.*th)
    res[*,*,2,j]=amp[*,*,1,j]*sin(1.*th)+amp[*,*,2,j]*cos(1.*th)
    res[*,*,3,j]=amp[*,*,3,j]*cos(2.*th)-amp[*,*,4,j]*sin(2.*th)
    res[*,*,4,j]=amp[*,*,3,j]*sin(2.*th)+amp[*,*,4,j]*cos(2.*th)
    res[*,*,5,j]=amp[*,*,5,j]*cos(4.*th)-amp[*,*,6,j]*sin(4.*th)
    res[*,*,6,j]=amp[*,*,5,j]*sin(4.*th)+amp[*,*,6,j]*cos(4.*th)
endfor


return,res
END
;*************************************************************************
;+
; NAME       : dstsp_mkiquv.pro (procedure)
; PURPOSE :
;          make IQUV
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        dstsp_mkiquv,file,ret,offset,iquv,hd
; INPUTS :
; OUTPUT :
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '??		
;        T.A. '2013/08/04/	version (mkiquv) 2		
;-
;*************************************************************************
pro dstsp_mkiquv,file,ret,offset,dark=dark,flat=flat,$     ;input 
                 iquv,hd,$                                 ;output
	mkiquv_ver=mkiquv_ver,header=header,frate=frate,fringe=fringe,shiftr=shiftr,ver=ver,useframe=useframe,data=data,datfit=datfit,ph=ph

if not keyword_set(mkiquv_ver) then mkiquv_ver=2
if not keyword_set(dark) then dark=0
if not keyword_set(flat) then flat=0
if not keyword_set(frate) then frate=0
file2data,file,data,hd,dark=dark,flat=flat,ver=ver
if keyword_set(shiftr) then begin
	off=get_correl_offsets(data[shiftr[0]:shiftr[1],*,*])
	off[1,*]=0
	data=shift_img(data,off)
endif
if keyword_set(header) then hd=header
phase,file,hd[0],ph,startframe,frate=frate
;demodulation,data[*,*,startframe:*],ph,amp,err,datfit
if not keyword_set(useframe) then begin
	useframe1=indgen((size(ph))[1])
	useframe2=indgen((size(ph))[1])+startframe
endif else begin
	pos=where(useframe ge startframe)
	useframe1=useframe[pos-startframe]
	useframe2=useframe[pos]
endelse
;demodulation,data[*,*,useframe2],ph[useframe1],amp,err,datfit
data=data[*,*,useframe2]
ph=ph[useframe1]
demodulation,data,ph,amp,err,datfit

case mkiquv_ver of
	1:modu_coefficient1,$
	  hd[0].exptime,hd[0].period,hd[0].wave,ret,0.,1.,$
	  ci1,ci2,cq1,cq2,cq3,cu1,cu2,cv1
	2:modu_coefficient2,$
	  hd[0].exptime,hd[0].period,hd[0].wave,ret,0.,1.,$
	  ci1,ci2,cq1,cq2,cq3,cu1,cu2,cv1
endcase
amp1=rot_amp(amp,-offset,replicate(0,hd[0].naxis1))
if keyword_set(fringe) then amp1=nofringe_sin2(amp1,15*!dtor)
qq0 = amp1[*,*,6]/cq2
uu0 = amp1[*,*,5]/cu2
vv0 = amp1[*,*,3]/cv1
ii0 = (amp[*,*,0]-qq0*cq3)/ci1
iquv=[[[ii0]],[[qq0]],[[uu0]],[[vv0]]]

;stop

END
;************************************************************************
pro dstsp_mkiquv_dual_demodu,file,ret,offset,kx,ky,deg,ratio,dark=dark,flat=flat,$     ;input 
                 iquv,hd,$                                 ;output
	mkiquv_ver=mkiquv_ver,header=header,frate=frate,fringe=fringe,shiftr=shiftr,ver=ver,useframe=useframe,data=data,datfit=datfit,ph=ph

if not keyword_set(mkiquv_ver) then mkiquv_ver=2
if not keyword_set(dark) then dark=0
if not keyword_set(flat) then flat=0
if not keyword_set(frate) then frate=0
file2data,file,data,hd,dark=dark,flat=flat,ver=ver
if keyword_set(shiftr) then begin
	off=get_correl_offsets(data[shiftr[0]:shiftr[1],*,*])
	off[1,*]=0
	data=shift_img(data,off)
endif
if keyword_set(header) then hd=header
nx=hd[0].naxis1 & ny=hd[0].naxis2 & nd=hd[0].naxis3
lpr=fltarr(nx/2,ny,nd)
lmr=fltarr(nx/2,ny,nd)
for iii=0,nd-1 do begin
	lpr[*,*,iii]=(data[0:nx/2-1,*,iii]+ratio*poly_2d(data[nx/2:nx-1,*,iii],kx,ky,deg))/2.
	lmr[*,*,iii]=(data[0:nx/2-1,*,iii]-ratio*poly_2d(data[nx/2:nx-1,*,iii],kx,ky,deg))/2.
endfor
phase,file,hd[0],ph,startframe,frate=frate
;demodulation,data[*,*,startframe:*],ph,amp,err,datfit
if not keyword_set(useframe) then begin
	useframe1=indgen((size(ph))[1])
	useframe2=indgen((size(ph))[1])+startframe
endif else begin
	pos=where(useframe-startframe ge 0)
	useframe1=useframe[pos]
	pos=where(useframe ge startframe)
	useframe2=useframe[pos]
endelse
demodulation,lpr[*,*,useframe2],ph[useframe1],cc,err,datfit,/const
demodulation,lmr[*,*,useframe2],ph[useframe1],amp,err,datfit
case mkiquv_ver of
	1:modu_coefficient1,$
	  hd[0].exptime,hd[0].period,hd[0].wave,ret,0.,1.,$
	  ci1,ci2,cq1,cq2,cq3,cu1,cu2,cv1
	2:modu_coefficient2,$
	  hd[0].exptime,hd[0].period,hd[0].wave,ret,0.,1.,$
	  ci1,ci2,cq1,cq2,cq3,cu1,cu2,cv1
endcase
amp1=rot_amp(amp,-offset,replicate(0,hd[0].naxis1))
if keyword_set(fringe) then amp1=nofringe_sin2(amp1,15*!dtor)
qq0 = amp1[*,*,6]/cq2
uu0 = amp1[*,*,5]/cu2
vv0 = amp1[*,*,3]/cv1
ii0 = (cc)/ci1
iquv=[[[ii0]],[[qq0]],[[uu0]],[[vv0]]]

end







;************************************************************************
pro txt2hazd,dir,time,ha0,zd0,check=check,radius=radius,pangle=pangle,incli=incli,gangle=gangle

files=file_search(dir,'20*.txt',count=nf)
hazd=rd_tfile(files[0],7)
for i=0,nf-1 do begin
	hazd1=rd_tfile(files[i],7)
	if mean(hazd1 ne '') then hazd=[[hazd],[hazd1]]
endfor
time=float(strmid(hazd[0,*],12,2))+$
	60.*float(strmid(hazd[0,*],10,2))+$
	60.^2 *float(strmid(hazd[0,*],8,2))
ha0=float(strmid(hazd[1,*],0,strlen(hazd[1,*])-1));arcsec
zd0=float(hazd[2,*])		;arcsec
rd0=abs(hazd[3,*])
rdd=long(rd0)/3600
rdm=long(rd0-rdd*3600)/60
rds=long(rd0-rdd*3600-rdm*60)
radius=rdm*60+rds		;arcsec
pangle=float(hazd[4,*])/3600.	;deg
incli=float(hazd[5,*])/3600.	;deg
gangle=float(hazd[6,*])/3600.	;deg

if keyword_set(check) then begin
	ha=ha0/3600.*15.*!dtor
	pos=where(ha ge !pi/2.,npos) & if npos ge 1 then  ha[pos]=ha[pos]-2.*!pi
	zd = zd0/3600.*!dtor
	wdef,0,600,600
	plot,time/3600.,ha*!radeg,psym=3,yr=[-100,100],/ystyle
	oplot,time/3600.,zd*!radeg,psym=3
endif

END
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
function index_form

hn={dstsp_v1                    ,$
     SIMPLE  : 0        ,$
     BITPIX  : 0l       ,$
     NAXIS   : 0l       ,$
     NAXIS1  : 0l       ,$
     NAXIS2  : 0l       ,$
     NAXIS3  : 0l       ,$
     EXTEND  : 0        ,$
     BSCALE  : 0l       ,$
     BZERO   : 0l       ,$
     ORIGIN  : ''       ,$
     OBSERVAT: ''       ,$
     TELESCOP: ''       ,$
     INSTRUME: ''       ,$
     PROGRAM : ''       ,$
     PROG_VER: 0        ,$
     OBS_TYPE: ''       ,$
     POLSTATE: ''       ,$
     TIMESYS : ''       ,$
     DATE    : ''       ,$
     DATE_OBS: ''       ,$
     DATE_END: ''       ,$
     WVPLATE : ''       ,$     
     PERIOD  : 0d       ,$
     DETNAM  : ''       ,$            
     DET_TMP : 0l       ,$             
     DET_PWR : 0l       ,$             
     EXPTIME : 0d       ,$
     CAMGAIN : 0l       ,$
     FGBINX  : 0l       ,$
     FGBINY  : 0l       ,$
     X0      : 0l       ,$
     X1      : 0l       ,$
     Y0      : 0l       ,$
     Y1      : 0l      ,$
     WAVE    : ''       ,$
     R       : 0d       ,$
     P       : 0d       ,$
     I       : 0d       ,$
     HA      : 0d       ,$
     ZD      : 0d       ,$
     COMMENT : strarr(15)      ,$
     HISTORY : strarr(5)       $
     }

return,hn
end
;--------------------------------------------------------------


















;---------------------------------
function eff_filter,iquv,p_fil

mf=invert($
           1./(1.+p_fil[0])*$
           [$
             [1.,0.,0.,0.],	$
             [0.,p_fil[3],0.,0.],	$
             [0.,0.,p_fil[3],0.],	$
             [0.,0.,0.,p_fil[3]]	$
           ] ## $
           muellermatrix_rot(-p_fil[1]) ## $
           [$
             [1.,0.,0.,0.],	$
             [0.,1.,0.,0.],	$
             [0.,0.,cos(p_fil[2]),sin(p_fil[2])],	$
             [0.,0.,-sin(p_fil[2]),cos(p_fil[2])]	$
           ] ## $
           [$
             [1.,p_fil[0],0.,0.],	$
             [p_fil[0],1.,0.,0.],	$
             [0.,0.,sqrt(1.-p_fil[0]^2),0.],	$
             [0.,0.,0.,sqrt(1.-p_fil[0]^2)]	$
           ] ## $
           muellermatrix_rot(p_fil[1]))

ii=mf[0,0]*iquv[*,*,0]+mf[1,0]*iquv[*,*,1]+mf[2,0]*iquv[*,*,2]+mf[3,0]*iquv[*,*,3]
qq=mf[0,1]*iquv[*,*,0]+mf[1,1]*iquv[*,*,1]+mf[2,1]*iquv[*,*,2]+mf[3,1]*iquv[*,*,3]
uu=mf[0,2]*iquv[*,*,0]+mf[1,2]*iquv[*,*,1]+mf[2,2]*iquv[*,*,2]+mf[3,2]*iquv[*,*,3]
vv=mf[0,3]*iquv[*,*,0]+mf[1,3]*iquv[*,*,1]+mf[2,3]*iquv[*,*,2]+mf[3,3]*iquv[*,*,3]
res=[[[ii]],[[qq]],[[uu]],[[vv]]]

quvi0=[$
       [[iquv[*,*,1]/iquv[*,*,0]]],$
       [[iquv[*,*,2]/iquv[*,*,0]]],$
       [[iquv[*,*,3]/iquv[*,*,0]]]$
       ]
quvi=[$
       [[res[*,*,1]/res[*,*,0]]],$
       [[res[*,*,2]/res[*,*,0]]],$
       [[res[*,*,3]/res[*,*,0]]]$
       ]
;stop
return,res
END
;*************************************************************************
;+
; NAME       : ta_fun_fil.pro (function)
;-
;*************************************************************************
function ta_fun_fil,xx,p

nd=(size(xx))[1]
qi=xx[*,0]
ui=xx[*,1]
vi=xx[*,2]
key=xx[*,3]
th =p[0]
ret=p[1]
dia=p[2]
ymod=fltarr(nd)
for i=0,nd-1 do begin
    case key[i] of
        0:ymod0=dia*cos(2.*th)*1. +$
          (cos(2.*th)^2 + sqrt(1.-dia^2)*cos(ret)*sin(2.*th)^2)*qi[i]+$
          sin(2.*th)*cos(2.*th)*(1.-sqrt(1.-dia^2)*cos(ret))*ui[i]+$
          (-sqrt(1.-dia^2)*sin(ret)*sin(2.*th))*vi[i]
        1:ymod0=dia*sin(2.*th)*1.+$
          sin(2.*th)*cos(2.*th)*(1.-sqrt(1.-dia^2)*cos(ret))*qi[i]+$
          (sin(2.*th)^2 + sqrt(1.-dia^2)*cos(ret)*cos(2.*th)^2)*ui[i]+$
          sqrt(1.-dia^2)*sin(ret)*cos(2.*th)*vi[i]
        2:ymod0=sqrt(1.-dia^2)*sin(ret)*sin(2.*th)*qi[i]+$
          (-sqrt(1.-dia^2)*sin(ret)*cos(2.*th))*ui[i]+$
          sqrt(1.-dia^2)*cos(ret)*vi[i]
    endcase
    ymod[i]=ymod0/(1.+dia*cos(2.*th)*qi[i]+dia*sin(2.*th)*ui[i])
endfor

return,ymod
END
;*************************************************************************
;+
; NAME       : ta_effect_filter.pro (function)
; PURPOSE :
;          calculate effct of filter
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        res=effct_filter(iquv0,iquv1)
; INPUTS :
;        iquv0-- no effect of filter
;        iquv1-- with effect of filter
; OUTPUT :
;        res -- structure, angle of axis, retardation, diattenuation
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2011/11/10/		
;-
;*************************************************************************
function ta_effct_filter,iquv0,iquv1

res={filter,$
     th    : 0.   ,$ ;angle of axis [rad]
     ret   : 0.   ,$ ;retardation   [rad]
     dia   : 0.    $ ;diattenuation
     }

quvi0=[median(iquv0[*,*,1]/iquv0[*,*,0]),$
      median(iquv0[*,*,2]/iquv0[*,*,0]),$
      median(iquv0[*,*,3]/iquv0[*,*,0])]
xx=[[replicate(quvi0[0],3)],$
    [replicate(quvi0[1],3)],$
    [replicate(quvi0[2],3)],$
   [indgen(3)]]
yy=[median(iquv1[*,*,1]/iquv1[*,*,0]),$
    median(iquv1[*,*,2]/iquv1[*,*,0]),$
    median(iquv1[*,*,3]/iquv1[*,*,0])]

sy	= .001
parinfo = replicate({value:0.D,fixed:0,  $
		limited:[0,0],limits:[0.D,0],step:0.d},3)
parinfo[0].step         = 0  ;th
parinfo[1].step 	= 0  ;ret
parinfo[2].step 	= 0  ;dia
parinfo[0].fixed        = 0
parinfo[1].fixed        = 0
parinfo[2].fixed        = 0
parinfo[0].limited[*] 	= 1
parinfo[1].limited[*] 	= 1
parinfo[2].limited[*] 	= 1
parinfo[0].limits[*]	= [-180,180]*!dtor
parinfo[1].limits[*]	= [-180,180]*!dtor
parinfo[2].limits[*]	= [-1.,1.]
parinfo[*].value        = [res.th,res.ret,res.dia]

res0 = mpfitfun('ta_fun_fil',xx,yy,sy,[res.th,res.ret,res.dia],$
               yfit=yfit,errmsg=errmsg,STATUS=status,$
               GTOL=1d-10,XTOL=1d-7,FTOL=1d-4,parinfo=parinfo)
print,'status=',status,'  err msg=',errmsg

res.th =res0[0]
res.ret=res0[1]
res.dia=res0[2]

plot,yy,psym=1,xr=[-1,3]
oplot,yfit
print,yy-yfit

return,res
END
;*************************************************************************
;+
; NAME       : ta_mm_filter.pro (function)
; PURPOSE :
;          calculate Mueller matrix of filter
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        res=ta_mm_filter(filter)
; INPUTS :
;        filter-- result of ta_effect_filter
; OUTPUT :
;        res -- Mueller matrix of filter
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;        T.A. '2011/11/10/		
;-
;*************************************************************************
function ta_mm_filter,filter

mm=1./(1.+filter.dia)*$
  muellermatrix_rot(-filter.th) ## $
  [$
    [1.,0.,0.,0.],	$
    [0.,1.,0.,0.],	$
    [0.,0.,cos(filter.ret),sin(filter.ret)],	$
    [0.,0.,-sin(filter.ret),cos(filter.ret)]	$
  ] ## $
  [$
    [1.,filter.dia,0.,0.],	$
    [filter.dia,1.,0.,0.],	$
    [0.,0.,sqrt(1.-filter.dia^2),0.],	$
    [0.,0.,0.,sqrt(1.-filter.dia^2)]	$
  ] ## $
  muellermatrix_rot(filter.th)

return,mm
END
;==========================================================;
FUNCTION ta_fun_dc2dstmodel,X,P
;<2e-7
;X=phic,phiv,key

weight=1.
nd	= size(x,/dim)		&	nd	= nd[0]
phic 	= x[*,0]
phiv 	= x[*,1]
key	= x[*,2]
ymod	= dblarr(nd)

;=======MM
m00 = (1.+p[4])*(1.+p[0]*p[2]*cos(2.*phic))
m01 = (1.+p[4])*(p[0]+p[2]*cos(2.*phic)) 
m02 = (1.+p[4])*(p[2]*sqrt(1.-p[0]^2)*cos(p[1])*sin(2.*phic))  
m03 = (1.+p[4])*(p[2]*sqrt(1.-p[0]^2)*sin(p[1])*sin(2.*phic)) 
m10 = p[2]+p[0]*cos(2.*phic)
m11 = p[0]*p[2]+cos(2.*phic)
m12 = sqrt(1.-p[0]^2)*cos(p[1])*sin(2.*phic)
m13 = sqrt(1.-p[0]^2)*sin(p[1])*sin(2.*phic)
m20 = -sqrt(1.-p[2]^2)*p[0]*cos(p[3])*sin(2.*phic)
m21 = -sqrt(1.-p[2]^2)*cos(p[3])*sin(2.*phic)
m22 = sqrt((1.-p[0]^2)*(1.-p[2]^2))*(cos(p[3]+p[1])*cos(phic)^2 - cos(p[3]-p[1])*sin(phic)^2)
m23 = sqrt((1.-p[0]^2)*(1.-p[2]^2))*(sin(p[3]+p[1])*cos(phic)^2 + sin(p[3]-p[1])*sin(phic)^2) 
m30 = sqrt(1.-p[2]^2)*p[0]*sin(p[3])*sin(2.*phic)
m31 = sqrt(1.-p[2]^2)*sin(p[3])*sin(2.*phic)
m32 = sqrt((1.-p[0]^2)*(1.-p[2]^2))*(-sin(p[3]+p[1])*cos(phic)^2 + sin(p[3]-p[1])*sin(phic)^2)
m33 = sqrt((1.-p[0]^2)*(1.-p[2]^2))*(cos(p[3]+p[1])*cos(phic)^2 + cos(p[3]-p[1])*sin(phic)^2)

;=======calibrate angle of polarizer & phi_V
a00	= m00
a01	= m01*cos(2.*p[5]) - m02*sin(2.*p[5])
a02	= m01*sin(2.*p[5]) + m02*cos(2.*p[5])
a03	= m03
a10	= m10*cos(2.*phiv) + m20*sin(2.*phiv)
a11	= (m11*cos(2.*p[5]) - m12*sin(2.*p[5]))*cos(2.*phiv) + $
  (m21*cos(2.*p[5]) - m22*sin(2.*p[5]))*sin(2.*phiv) ;
a12	= (m11*sin(2.*p[5]) + m12*cos(2.*p[5]))*cos(2.*phiv) + $
  (m21*sin(2.*p[5]) + m22*cos(2.*p[5]))*sin(2.*phiv)
a13	= m13*cos(2.*phiv) + m23*sin(2.*phiv)
a20	= -m10*sin(2.*phiv) + m20*cos(2.*phiv)
a21	= -(m11*cos(2.*p[5]) - m12*sin(2.*p[5]))*sin(2.*phiv) + $
  (m21*cos(2.*p[5]) - m22*sin(2.*p[5]))*cos(2.*phiv)
a22	= -(m11*sin(2.*p[5]) + m12*cos(2.*p[5]))*sin(2.*phiv) + $
  (m21*sin(2.*p[5]) + m22*cos(2.*p[5]))*cos(2.*phiv)
a23	= -m13*sin(2.*phiv) + m23*cos(2.*phiv)
a30	= m30
a31	= m31*cos(2.*p[5]) - m32*sin(2.*p[5])
a32	= m31*sin(2.*p[5]) + m32*cos(2.*p[5])
a33	= m33

;=======ymod
for i=0,nd-1 do begin
	case key[i] of
            0	:ymod[i]	= a10[i]/a00[i]*weight
            1	:ymod[i]	= a20[i]/a00[i]*weight
            2	:ymod[i]	= a30[i]/a00[i]*weight
            3	:ymod[i]	= (a10[i]+a11[i]*p[6])/(a00[i]+a01[i]*p[6])
            4	:ymod[i]	= (a20[i]+a21[i]*p[6])/(a00[i]+a01[i]*p[6])
            5	:ymod[i]	= (a30[i]+a31[i]*p[6])/(a00[i]+a01[i]*p[6])
            6	:ymod[i]	= (a10[i]-a11[i]*p[6])/(a00[i]-a01[i]*p[6])
            7	:ymod[i]	= (a20[i]-a21[i]*p[6])/(a00[i]-a01[i]*p[6])
            8	:ymod[i]	= (a30[i]-a31[i]*p[6])/(a00[i]-a01[i]*p[6])
            9	:ymod[i]	= (a10[i]+a12[i]*p[6])/(a00[i]+a02[i]*p[6])
            10	:ymod[i]	= (a20[i]+a22[i]*p[6])/(a00[i]+a02[i]*p[6])
            11	:ymod[i]	= (a30[i]+a32[i]*p[6])/(a00[i]+a02[i]*p[6])
            12	:ymod[i]	= (a10[i]-a12[i]*p[6])/(a00[i]-a02[i]*p[6])
            13	:ymod[i]	= (a20[i]-a22[i]*p[6])/(a00[i]-a02[i]*p[6])
            14	:ymod[i]	= (a30[i]-a32[i]*p[6])/(a00[i]-a02[i]*p[6])
	endcase
endfor

return,ymod
END
;*************************************************************************
;+
; NAME       : ta_dc2dstmodel.pro (function)
; PURPOSE :
; 	return parameters of DST model fitted observational data
;	by using mpfit 
; CALLING SEQUENCE :
;        res=ta_calib5()
; INPUTS :
;         stks   :observational stokes vector, [#,[Q/I,U/I,V/I]]
;         hd     :header of data
;         telpos :telescope position, 'west' or 'east'
;         incli  :inclination of VS
;         p0     :start parameters, [pn,tn,pc,tc,s,thp,deg_p]
;         perror :error of parameters
; OUTPUT :
;         res    :parameters, [pn,tn,pc,tc,s,thp,deg_p]
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
; MODIFICATION HISTORY :
;      T.A. '12/01/06
;*******************************************************************
function ta_dc2dstmodel,stks,hd,telpos,incli,p0,perror=perror

lat	= 36.252*!dtor		;Hidaten
parinfo = replicate({value:0.D,fixed:0,  $
		limited:[0,0],limits:[0.D,0],step:0.d},7)
parinfo[0].step         = 0  ;pn
parinfo[1].step 	= 0  ;tn
parinfo[2].step 	= 0  ;pc
parinfo[3].step 	= 0  ;tc
parinfo[4].step 	= 0  ;s
parinfo[5].step 	= 0  ;thp
parinfo[6].step 	= 0  ;polarization degree of polarizer
parinfo[0].limited[*] 	= 1
parinfo[1].limited[*] 	= 0
parinfo[2].limited[*] 	= 1
parinfo[3].limited[*]	= 0
parinfo[4].limited[*]	= 1
parinfo[5].limited[*]	= 0
parinfo[6].limited[*]	= 1
parinfo[0].limits[*]	= [-1.D,1.D]
parinfo[2].limits[*]	= [-1.D,1.D]
parinfo[4].limits[*]	= [0.D,1.D]
parinfo[6].limits[*]	= [0.D,1.D]
sy	= .001
parinfo[0].fixed = 0
parinfo[1].fixed = 0
parinfo[2].fixed = 0
parinfo[3].fixed = 0
parinfo[4].fixed = 1
parinfo[5].fixed = 1
parinfo[6].fixed = 1
parinfo[*].value = p0
    
nd = n_elements(hd)
zd = [hd.zd,hd.zd,hd.zd]/3600.*!dtor & if telpos eq 'east' then zd=-zd 
ha = [hd.ha,hd.ha,hd.ha]/3600.*15.*!dtor
for i=0,n_elements(ha)-1 do if ha[i] ge !pi/2. then ha[i]=ha[i]-2.*!pi
yy = [stks[*,0],stks[*,1],stks[*,2]]
key= [replicate(0,nd),replicate(1,nd),replicate(2,nd)]

za	= asin(cos(lat)*sin(ha)/sin(zd))
if telpos eq 'west' then begin
    phic = -zd
    phiv = zd - za +incli*(fltarr(nd*3)+1.)
endif else begin
    phic = zd
    phiv = -zd - za +incli*(fltarr(nd*3)+1.)
endelse

yfit0=ta_fun_dc2dstmodel([[phic],[phiv],[key]],p0)
res0 = mpfitfun('ta_fun_dc2dstmodel',[[phic],[phiv],[key]],yy,sy,p0,$
               yfit=yfit,errmsg=errmsg,STATUS=status,$
               GTOL=1d-10,parinfo=parinfo,perror=perror)
print,status,'msg=',errmsg
print,perror

plot,yy,psym=1,yr=minmax([yy,yfit0,yfit])
oplot,yfit0,line=1
oplot,yfit
;stop
res={par_dst,$
     xn      : res0[0],$   ; ratio of reflectivity of Newton mirror
     tn      : res0[1],$   ; retardation of Newton mirror
     xc      : res0[2],$   ; ratio of reflectivity of Coude mirror
     tc      : res0[3],$   ; retardation of Coude mirror
     sc      : res0[4],$   ; stray light
     t_en    : 0.     ,$   ; angle of axis of entrance window
     dlen    : 0.     ,$   ; retardation of entrance window
     t_ex    : 0.     ,$   ; angle of axis of exit window
     dlex    : 0.     $     ; retardation of exit window
     }

return,res
END
