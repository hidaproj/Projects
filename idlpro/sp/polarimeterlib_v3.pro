;  library to make IQUV map (version 3)
;  work in Solar Soft Idl
;
; version 1
; 2012.09.02 T.A.
; 2014.08.29 T.A. include ORCA-Flash4.0
;
; 2016.08.20 T.A. phase
; .....
; 2016.11.03 T.A. hd2az, par_mmsp2, version 2 in mm_dst
; 2016.11.08 T.A. add hsp in main
; 2016.11.10 T.A. dark in file2data, mkdark_xeva640
; 2016.12.27 T.A. merginx, merginy, in mkkxky_orca
;-
function version
ver='Version 2.1'    ;2016.12.31 T.A. modify ref2irarp for hsp
ver='Version 2.2'    ;2017.01.02 T.A. dstsp_mkiquv, 
ver='Version 3.0'    ;2017.01.17 T.A. change index for HSP
return,ver
end
;*************************************************************************
;*************************************************************************
;== HEADER ==;
;FUNCTION dstsp_mkindex,header=header,ver=ver
;PRO file2data,file,data,hd
;PRO dstsp_mkdark,files,camera,darks,dh
;PRO dstsp_mkkxky,files,kx,ky,shiftx1,kxkyh
;-

;== INCLUDE ==;
@/home/anan/lib/DSTPOL/polarimeterlib_v2.pro
;-

;*************************************************************************
;== MAIN ==;
;*************************************************************************


;-------------------------------------------------------------
;+
; NAME:
;       dstsp_mkindex
; PURPOSE:
;       make index
; CATEGORY:
; CALLING SEQUENCE:
;       INDEX=dstsp_mkindex()
; INPUTS:
; KEYWORD PARAMETERS:
;         HEADER = input index
;         VER = version
;               0) template
;               1) ORCA (2017.01.17 now)
;               2) XEVA640 (2017.02.06 now)
; OUTPUTS:
;         INDEX = index of DST/SP
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;         2017.01.18   T.A.
;         2017.02.06   T.A.    ver=2
;-
;-------------------------------------------------------------
FUNCTION dstsp_mkindex,header=header,ver=ver, $
                       instrume=instrume,PROGRAM=program,PROG_VER=prog_ver,  $
		       OBS_TYPE=obs_type,POLSTATE=polstate,DATE=date,        $
                       DATE_OBS=date_obs,DATE_END=date_end,WVPLATE=wvplate,  $
                       PERIOD=period,DETNAM=detnam,DET_TMP=det_tmp,          $
                       DET_PWR=det_pwr,EXPTIME=exptime,CAMGAIN=camgain,      $
                       FGBINX=fgbinx,FGBINY=fgbiny,X0=x0,X1=x1,Y0=y0,Y1=y1,  $
                       WAVE=wave,R=r,P=p,I=i,HA=ha,ZD=zd,AZ=az,IMGROT=imgrot

if not keyword_set(ver) then ver=0

res={DSTSP              ,$
     SIMPLE  : 1        ,$
     BITPIX  : 0l       ,$
     NAXIS   : 3        ,$
     NAXIS1  : 0l       ,$
     NAXIS2  : 0l       ,$
     NAXIS3  : 0l       ,$
     EXTEND  : 0        ,$
     BSCALE  : 1.0      ,$
     BZERO   : 0.0      ,$
     ORIGIN  : 'HIDA OBSERVATORY',$
     OBSERVAT: 'HIDA OBSERVATORY',$
     TELESCOP: 'DST'    ,$
     INSTRUME: 'HSP'    ,$ ;VSP or HSP
     PROGRAM : ''       ,$
     PROG_VER: 0        ,$
     OBS_TYPE: 'POL'    ,$ ;NOM, POL, or CAL
     POLSTATE: ''       ,$ ;Angle of polarizer of the calibration unit
     TIMESYS : 'UTC'    ,$
     DATE    : 'yyyy-mm-ddThh:mm:ss',$
     DATE_OBS: 'yyyy-mm-ddThh:mm:ss',$
     DATE_END: 'yyyy-mm-ddThh:mm:ss',$
     WVPLATE : 'QUARTZ'  ,$     
     PERIOD  : 0d       ,$
     DETNAM  : 'GE1650' ,$            
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
     AZ      : 0d       ,$
     IMGROT  : 0d       ,$
     COMMENT : strarr(15)      ,$
     HISTORY : strarr(5)       $
     }

if keyword_set(header) then begin
   res0=res
   for iheader=0,n_elements(header)-1 do begin
      case ver of
         1:begin; ORCA (2017.01.17 now)
            if keyword_set(instrume) then tmpinstrume=string(instrume) else tmpinstrume=string(header[iheader].instrume)
            if keyword_set(program)  then tmpprogram =string(program)  else tmpprogram=string(header[iheader].program)
            if keyword_set(prog_ver) then tmpprog_ver=fix(prog_ver)    else tmpprog_ver=fix(header[iheader].prog_ver)
            if keyword_set(obs_type) then tmpobs_type=string(obs_type) else tmpobs_type=string(header[iheader].obs_type)
            if keyword_set(polstate) then tmppolstate=string(polstate) else tmppolstate=string(header[iheader].polstate)
            if keyword_set(date)     then tmpdate=string(date)         else tmpdate=  $
                 strmid(anytim(anytim(header[iheader].date,/seconds)-9.*60.*60.,/ccsds),0,19)
            if keyword_set(date_obs) then tmpdate_obs=string(date_obs) else tmpdate_obs= $
                 strmid(anytim(anytim(header[iheader].date_obs,/seconds)-9.*60.*60.,/ccsds),0,19)
            if keyword_set(date_end) then tmpdate_end=string(date_end) else tmpdate_end= $
                 strmid(anytim(anytim(header[iheader].date_end,/seconds)-9.*60.*60.,/ccsds),0,19)
            if keyword_set(wvplate)  then tmpwvplate=string(wvplate)   else tmpwvplate=string(header[iheader].wvplate)
            if keyword_set(period)   then tmpperiod=double(period)     else tmpperiod=double(header[iheader].period)
            if keyword_set(detnam)   then tmpdetnam=string(detnam)     else tmpdetnam=string(header[iheader].detnam)
            if keyword_set(det_tmp)  then tmpdet_tmp=long(det_tmp)     else tmpdet_tmp=long(header[iheader].det_tmp)
            if keyword_set(det_pwr)  then tmpdet_pwr=long(det_pwr)     else tmpdet_pwr=long(header[iheader].det_pwr)
            if keyword_set(exptime)  then tmpexptime=double(exptime)   else tmpexptime=double(header[iheader].exptime)
            if keyword_set(camgain)  then tmpcamgain=long(camgain)     else tmpcamgain=long(header[iheader].camgain)
            if keyword_set(fgbinx)   then tmpfgbinx=long(fgbinx)       else tmpfgbinx=long(header[iheader].fgbinx)
            if keyword_set(fgbiny)   then tmpfgbiny=long(fgbiny)       else tmpfgbiny=long(header[iheader].fgbiny)
            if keyword_set(x0)       then tmpx0=long(x0)               else tmpx0=long(header[iheader].x0)
            if keyword_set(x1)       then tmpx1=long(x1)               else tmpx1=long(header[iheader].x1)
            if keyword_set(y0)       then tmpy0=long(y0)               else tmpy0=long(header[iheader].y0)
            if keyword_set(y1)       then tmpy1=long(y1)               else tmpy1=long(header[iheader].y1)
            if keyword_set(wave)     then tmpwave=string(wave)         else tmpwave=string(header[iheader].wave)
            if keyword_set(r)        then tmpr=dounle(r[iheader])      else tmpr=double(header[iheader].r)
            if keyword_set(p)        then tmpp=double(p[iheader])      else tmpp=double(header[iheader].p)
            if keyword_set(i)        then tmpi=double(i[iheader])      else tmpi=double(header[iheader].i)
            if keyword_set(ha)       then tmpha=double(ha[iheader])    else tmpha=double(header[iheader].ha)
            if keyword_set(zd)       then tmpzd=double(zd[iheader])    else tmpzd=double(header[iheader].zd)
            if keyword_set(az)       then tmpaz=double(az[iheader])    else tmpaz=double(header[iheader].az)
            if keyword_set(imgrot)   then tmpimgrot=double(imgrot[iheader])     else tmpimgrot=double(header[iheader].imgrot)
         end
         2:begin; XEVA640 (2017.02.06 now)
            if keyword_set(instrume) then tmpinstrume=string(instrume) else tmpinstrume=string(header[iheader].instrume)
            if keyword_set(program)  then tmpprogram =string(program)  else tmpprogram=string(header[iheader].program)
            if keyword_set(prog_ver) then tmpprog_ver=fix(prog_ver)    else tmpprog_ver=fix(header[iheader].prog_ver)
            if keyword_set(obs_type) then tmpobs_type=string(obs_type) else tmpobs_type=string(header[iheader].obs_type)
            if keyword_set(polstate) then tmppolstate=string(polstate) else tmppolstate=string(header[iheader].polstate)
            if keyword_set(date)     then tmpdate=string(date)         else tmpdate=  $
                 strmid(anytim(anytim(header[iheader].date,/seconds),/ccsds),0,19)
            if keyword_set(date_obs) then tmpdate_obs=string(date_obs) else tmpdate_obs= $
                 strmid(anytim(anytim(header[iheader].date_obs,/seconds),/ccsds),0,19)
            if keyword_set(date_end) then tmpdate_end=string(date_end) else tmpdate_end= $
                 strmid(anytim(anytim(header[iheader].date_end,/seconds),/ccsds),0,19)
            if keyword_set(wvplate)  then tmpwvplate=string(wvplate)   else tmpwvplate=string(header[iheader].wvplate)
            if keyword_set(period)   then tmpperiod=double(period)     else tmpperiod=double(header[iheader].period)
            if keyword_set(detnam)   then tmpdetnam=string(detnam)     else tmpdetnam=string(header[iheader].detnam)
            if keyword_set(det_tmp)  then tmpdet_tmp=long(det_tmp)     else tmpdet_tmp=long(header[iheader].det_tmp)
            if keyword_set(det_pwr)  then tmpdet_pwr=long(det_pwr)     else tmpdet_pwr=long(header[iheader].det_pwr)
            if keyword_set(exptime)  then tmpexptime=double(exptime)   else tmpexptime=double(header[iheader].exptime)
            if keyword_set(camgain)  then tmpcamgain=long(camgain)     else tmpcamgain=long(header[iheader].camgain)
            if keyword_set(fgbinx)   then tmpfgbinx=long(fgbinx)       else tmpfgbinx=long(header[iheader].fgbinx)
            if keyword_set(fgbiny)   then tmpfgbiny=long(fgbiny)       else tmpfgbiny=long(header[iheader].fgbiny)
            if keyword_set(x0)       then tmpx0=long(x0)               else tmpx0=long(header[iheader].x0)
            if keyword_set(x1)       then tmpx1=long(x1)               else tmpx1=long(header[iheader].x1)
            if keyword_set(y0)       then tmpy0=long(y0)               else tmpy0=long(header[iheader].y0)
            if keyword_set(y1)       then tmpy1=long(y1)               else tmpy1=long(header[iheader].y1)
            if keyword_set(wave)     then tmpwave=string(wave)         else tmpwave=string(header[iheader].wave)
            if keyword_set(r)        then tmpr=dounle(r[iheader])      else tmpr=double(header[iheader].r)
            if keyword_set(p)        then tmpp=double(p[iheader])      else tmpp=double(header[iheader].p)
            if keyword_set(i)        then tmpi=double(i[iheader])      else tmpi=double(header[iheader].i)
            if keyword_set(ha)       then tmpha=double(ha[iheader])    else tmpha=double(header[iheader].ha)
            if keyword_set(zd)       then tmpzd=double(zd[iheader])    else tmpzd=double(header[iheader].zd)
            if keyword_set(az)       then tmpaz=double(az[iheader])    else tmpaz=double(header[iheader].az)
            if keyword_set(imgrot)   then tmpimgrot=double(imgrot[iheader])     else tmpimgrot=double(header[iheader].imgrot)
         end
         else:begin
            print,'no version in '
            res=-1
            goto,dstsp_mkindex_jump1
         end
      endcase
      res0.SIMPLE  =header[iheader].SIMPLE
      res0.BITPIX  =header[iheader].BITPIX
      res0.NAXIS   =header[iheader].NAXIS
      res0.NAXIS1  =header[iheader].NAXIS1
      res0.NAXIS2  =header[iheader].NAXIS2
      res0.NAXIS3  =header[iheader].NAXIS3
      res0.EXTEND  =header[iheader].EXTEND
      res0.BSCALE  =header[iheader].BSCALE
      res0.BZERO   =header[iheader].BZERO
      res0.INSTRUME=tmpinstrume
      res0.PROGRAM =tmpprogram
      res0.PROG_VER=tmpprog_ver
      res0.OBS_TYPE=tmpobs_type
      res0.POLSTATE=tmppolstate
      res0.DATE    =tmpdate
      res0.DATE_OBS=tmpdate_obs
      res0.DATE_END=tmpdate_end
      res0.WVPLATE =tmpwvplate
      res0.PERIOD  =tmpperiod
      res0.DETNAM  =tmpdetnam
      res0.DET_TMP =tmpdet_tmp
      res0.DET_PWR =tmpdet_pwr
      res0.EXPTIME =tmpexptime
      res0.CAMGAIN =tmpcamgain
      res0.FGBINX  =tmpfgbinx
      res0.FGBINY  =tmpfgbiny
      res0.X0      =tmpx0
      res0.X1      =tmpx1
      res0.Y0      =tmpy0
      res0.Y1      =tmpy1
      res0.WAVE    =tmpwave
      res0.R       =tmpr
      res0.P       =tmpp
      res0.I       =tmpi
      res0.HA      =tmpha
      res0.ZD      =tmpzd
      res0.AZ      =tmpaz
      res0.IMGROT  =tmpimgrot
      if iheader eq 0 then res=res0 else res=[res,res0]
   endfor
endif
dstsp_mkindex_jump1:


RETURN,res
END
;*************************************************************************
;+
; NAME       : file2data.pro
; PURPOSE :
;       read data and index from file
; CATEGORY :
; CALLING SEQUENCE :
;        file2data,file,data,hd,dark=dark,flat=flat 
; INPUTS :
;        FILE = file names
; OUTPUT :
;        DATA = data
;        HD = index
; OPTIONAL INPUT PARAMETERS : 
;        DARK = dark
;        FLAT = flat field
; KEYWORD PARAMETERS :
;        VER = version of program
;        ORCA = ORCA4 data
;        LINEAR = don't calibrate non linearity of XEVA640
; MODIFICATION HISTORY :
;        T.A. '2011/11/04/              
;        T.A. '2011/12/27/ cam_id               
;        T.A. '2014/08/29/ ORCA
;        T.A. '2016/11/10/ XEVA ver 1, range in dark array
;        T.A. '2016/11/17/ linear keyword
;        T.A. '2017/02/06/ =>polarimeterlib_v3.pro
;-
;*************************************************************************
PRO file2data,file,           $;input
	      data,hd,        $;output
              dark=dark,flat=flat,ver=ver,orca=orca,linear=linear

if not keyword_set(orca) then begin
        mreadfits,file,hd,imgs
	imgs=float(imgs)
	hd=dstsp_mkindex(header=hd,ver=2,    $
                         az=fltarr(n_elements(hd)),imgrot=fltarr(n_elements(hd)))
endif else begin
        read_orca,file,hd,imgs
	hd=dstsp_mkindex(header=hd,ver=1,    $
                         az=fltarr(n_elements(hd)),imgrot=fltarr(n_elements(hd)))
endelse
if not keyword_set(dark) then dark=fltarr(hd[0].naxis1,hd[0].naxis2)
if not keyword_set(flat) then flat=fltarr(hd[0].naxis1,hd[0].naxis2)+1.
if not keyword_set(ver) then ver=0

case ver of
	1:begin;--------------------------------
		data=imgs
		for i=0,hd[0].naxis3-1 do begin
			if (hd[0].DETNAM eq 'XEVA640') and $
			   keyword_set(dark) and $
			   (hd[0].exptime eq 0.2d) then begin
				if not keyword_set(linear) then begin
          				data[0:hd[0].naxis1/2-1,*,i]=float( ((imgs[0:hd[0].naxis1/2-1,*,i]-  $
                                                dark[0:hd[0].naxis1/2-1,*]-351.56214)>0.)^$
                                                (1./0.76143312)/59222.059d )
          				data[hd[0].naxis1/2:hd[0].naxis1-1,*,i]=   $
          			        	float( ((imgs[hd[0].naxis1/2:hd[0].naxis1-1,*,i]-  $
                            			dark[hd[0].naxis1/2:hd[0].naxis1-1,*]-298.70353)>0)^$
                   				(1./0.77864712)/69704.267d )
					data[*,*,i]=(data[*,*,i]/flat)<7.
        			endif else begin
        				data[*,*,i] = (imgs[*,*,i] - dark)/flat
				endelse
			endif else begin
				 data[*,*,i] = (imgs[*,*,i] - dark)/flat
			endelse
		endfor
	end
	else:begin;-----------------------------
		data=imgs
		for i=0,hd[0].naxis3-1 do begin
			if (hd[0].CAM_ID eq 'XEVA640') and keyword_set(dark) then begin
        			data[0:hd[0].naxis1/2-1,*,i]=float( ((imgs[0:hd[0].naxis1/2-1,*,i]-dark-351.56214)>0.)^$
                 						(1./0.76143312)/59222.059d )
				data[hd[0].naxis1/2:hd[0].naxis1-1,*,i]=float( ((imgs[hd[0].naxis1/2:hd[0].naxis1-1,*,i]-dark-298.70353)>0)^$
                 						(1./0.77864712)/69704.267d )
				data[*,*,i]=(data[*,*,i]/flat)<7.
			endif else begin
				data[*,*,i] = (imgs[*,*,i] - dark)/flat
			endelse
		endfor
	end
endcase

END
;-------------------------------------------------------------
;+
; NAME:
;       dstsp_mkdark
; PURPOSE:
;       make dark and index
; CATEGORY:
; CALLING SEQUENCE:
;       dstsp_mkdark,files,camera,darks,dh
; INPUTS:
;         FILES = dark file names
;         CAMERA = camera name, 'orca4', 'xeva640'
; KEYWORD PARAMETERS:
; OUTPUTS:
;         DARKS = dark images
;         INDEX = index
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;         2017.02.06   T.A.
;-
;-------------------------------------------------------------
PRO dstsp_mkdark,files,camera,    $;input
                 darks,dh          ;output

case camera of
   'orca4':begin
	nf=n_elements(files)
	darks=fltarr(2048,2048,nf)
	for i=0,nf-1 do begin
        	print,i,nf
        	file2data,files[i],data,index,ver=1,orca=1
        	darks[0:index[0].naxis1-1,0:index[0].naxis2-1,i]=float(reform(rebin(data,index[0].naxis1,index[0].naxis2,1)))
        	if i eq 0 then dh=index[0] else dh=[dh,index[0]]
	endfor
   end
   'xeva640':begin
        nf=n_elements(files)
        darks=fltarr(640,512,nf)
        for i=0,nf-1 do begin
           print,i,nf
           mreadfits,files[i],index,data          ; 20161110 TA
           darks[0:index[0].naxis1-1,0:index[0].naxis2-1,i]=despike_gen(float(reform(rebin(data,index[0].naxis1,index[0].naxis2,1))))
           if i eq 0 then dh=index[0] else dh=[dh,index[0]]
        endfor
	dh=dstsp_mkindex(header=dh,ver=2,    $
                         az=fltarr(n_elements(dh)),imgrot=fltarr(n_elements(dh)))
   end
   else:print,'no camera in dstsp_mkdark'
endcase
nx=(size(darks))[1]
ny=(size(darks))[2]

nopos=1
pos=selectdark(dh,dh[0],nopos=nopos)
darks1=fltarr(nx,ny)
darks1[0:dh[0].naxis1-1,0:dh[0].naxis2-1]=      $       ;20151104
      reform(rebin(darks[0:dh[0].naxis1-1,0:dh[0].naxis2-1,pos],dh[0].naxis1,dh[0].naxis2,1))
ndh=dh[0]
ndarks=(size(darks))[3]
if (size(nopos))[0] eq 0 then ndarks1=0 else ndarks1=(size(nopos))[1]
iii=0l
while ndarks1 ne 0 do begin
   iii=iii+1l
   dh=dh[nopos]
   darks=darks[*,*,nopos]
   ndarks=ndarks1

   nopos=1
   pos=selectdark(dh,dh[0],nopos=nopos)
   darks0=darks1
   darks1=fltarr(nx,ny,iii+1)
   darks1[*,*,0:iii-1]=darks0
   darks1[0:dh[0].naxis1-1,0:dh[0].naxis2-1,iii]=  $       ;20151104
      reform(rebin(darks[0:dh[0].naxis1-1,0:dh[0].naxis2-1,pos],dh[0].naxis1,dh[0].naxis2,1))
   ndh=[ndh,dh[0]]
   if (size(nopos))[0] eq 0 then ndarks1=0 else ndarks1=(size(nopos))[1]
endwhile

darks=darks1
dh=ndh

END
;-------------------------------------------------------------
;+
; NAME:
;       dstsp_mkkxky
; PURPOSE:
;       return parameters for alignment of the dual spectra 
; CATEGORY:
; CALLING SEQUENCE:
;       dstsp_mkkxky,files,kx,ky,shiftx1,kxkyh
; INPUTS:
;         FILES = file names
; KEYWORD PARAMETERS:
;         MERGINX = mergin in x
;         MERGINY = mergin in y
;         CAMERA = 'orca4' or 'xeva640'
; OUTPUTS:
;         KX = kx for poly_2d
;         KY = ky for poly_2d
;         SHIFTX1 = shift in x
;         KXKYH = index
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;         2017.02.06   T.A.   mkkxky_orca4 in polarimeterlib_v2.pro => polarimeterlib_v3.pro
;-
;-------------------------------------------------------------
PRO dstsp_mkkxky,files,         $;input
        merginx=merginx,merginy=merginy, $  ;20161227 TA
        offx=offx,offy=offy,camera=camera,$;keywords
        kx,ky,shiftx1,kxkyh,outfile=outfile;outputs

if not keyword_set(merginx) then merginx=5 ; 20161227 TA
if not keyword_set(merginy) then merginy=5 ; 20161227 TA
if not keyword_set(offx) then offx=[0.,0.,0.,0.]
if not keyword_set(offy) then offy=[0.,0.,0.,0.]
if not keyword_set(camera) then camera='orca4'

print,'kx, ky'
nf=n_elements(files)
for i=0,nf-1 do begin
        print,i,nf
        case camera of
                'ge1650':mreadfits,files[i],index,data
                'xeva640':begin
			mreadfits,files[i],index,data
		        index=dstsp_mkindex(header=index,ver=2,    $
                         	az=fltarr(n_elements(index)),imgrot=fltarr(n_elements(index)))
		end
                'orca4':begin
			read_orca,files[i],index,data
		        index=dstsp_mkindex(header=index,ver=1,    $
                         	az=fltarr(n_elements(index)),imgrot=fltarr(n_elements(index)))
		end
        endcase
        if i eq 0 then begin
                arrs=fltarr(index[0].naxis1,index[0].naxis2,nf)
                kxkyh=index[0]
        endif
        arrs[0:index[0].naxis1-1,0:index[0].naxis2-1,i]=  $
		float(reform(rebin(data,index[0].naxis1,index[0].naxis2,1)))
endfor
flat0=reform(rebin(arrs,index[0].naxis1,index[0].naxis2,1))

set_plot,'x'
wdef,0,800,600
!p.multi=0
loadct,0
plot_image,flat0
print,'click center between left and right images'
xycursor,x,y

shiftx1=index[0].naxis1/2-x
arr=shift_img(flat0,[shiftx1,0])

limg=arr[0:index[0].naxis1/2-1,*]
limg=limg/max(rebin(limg,index[0].naxis1/2,1))
rimg=arr[index[0].naxis1/2:index[0].naxis1-1,*]
rimg=rimg/max(rebin(rimg,index[0].naxis1/2,1))
shiftx0=(centroid(limg))[0]-(centroid(rimg))[0]
rimg0=shift_img(rimg,[shiftx0,0.])
;stop

wx=500.
wy=600.
factorx=wx/index[0].naxis1*2.
factory=wy/index[0].naxis2
for ii=0,1 do begin
        wdef,ii*2,wx,wy
        case ii of
                0:begin
                        lrimg=limg
                end
                1:begin
                        lrimg=rimg
                end
        endcase

        tvscl,congrid(lrimg,wx,wy)
        print,'click left hair line'
        cursor,/dev,h1x,h1y
        h1x=h1x/factorx
        h1y=h1y/factory
        wait,1.
        print,'click right hair line'
        cursor,/dev,h2x,h2y
        h2x=h2x/factorx
        h2y=h2y/factory
        wait,1.
        print,'click lower spectral line'
        cursor,/dev,l1x,l1y
        l1x=l1x/factorx
        l1y=l1y/factory
        wait,1.
        print,'click upper spectral line'
        cursor,/dev,l2x,l2y
        l2x=l2x/factorx
        l2y=l2y/factory
        wait,1.

        plot_image,lrimg

        yy=findgen(abs(l2y-l1y))+min([l2y,l1y])
        xx=fltarr(abs(l2y-l1y))
        ;dd=mergin ;comout 20161227
        dd=merginx ;20161227
        for i=0,1 do begin
                case i of
                        0:hx=h1x
                        1:hx=h2x
                endcase
                for iy=0,(size(yy))[1]-1 do begin
                        tmp=min(lrimg[hx-dd:hx+dd,yy[iy]],pos)
                        xx[iy]=pos+hx-dd
                endfor
                coe=poly_fit(yy,xx,1,yfit=yfit)
                oplot,yfit,yy,psym=3
                case i of
                        0:begin
                                coeh1=coe
                        end
                        1:begin
                                coeh2=coe
                        end
                endcase
        endfor

        xx=findgen(abs(h1x-h2x))+min([h1x,h2x])
        yy=fltarr(abs(h1x-h2x))
        ;dd=mergin ;comout 20161227
        dd=merginy ;20161227
        for i=0,1 do begin
                case i of
                        0:ly=l1y
                        1:ly=l2y
                endcase
                for ix=0,(size(xx))[1]-1 do begin
                        tmp=min(lrimg[xx[ix],ly-dd:ly+dd],pos)
                        yy[ix]=pos+ly-dd
                endfor
                coe=poly_fit(xx,yy,1,yfit=yfit)
                ;oplot,xx,yfit,psym=3
                oplot,xx,yfit,line=0
                oplot,xx,yy,psym=3
                  case i of
                        0:begin
                                coel1=coe
                        end
                        1:begin
                                coel2=coe
                        end
                endcase
        endfor

        case ii of
                0:begin
                        coelh1=coeh1
                        coelh2=coeh2
                        coell1=coel1
                        coell2=coel2
                end
                1:begin
                        coerh1=coeh1
                        coerh2=coeh2
                        coerl1=coel1
                        coerl2=coel2
                end
        endcase
wait,1.
endfor

jump:
xi=fltarr(4)
yi=fltarr(4)
xo=fltarr(4)
yo=fltarr(4)
for i=0,3 do begin
        case i of
                0:begin
                        bl=coelh1
                        br=coerh1
                        al=coell1
                        ar=coerl1
                end
                1:begin
                        bl=coelh1
                        br=coerh1
                        al=coell2
                        ar=coerl2
                end
                2:begin
                        bl=coelh2
                        br=coerh2
                        al=coell1
                        ar=coerl1
                end
                3:begin
                        bl=coelh2
                        br=coerh2
                        al=coell2
                        ar=coerl2
                end
        endcase

        xi[i]=(al[0]*bl[1]+bl[0])/(1.-al[1]*bl[1])
        xo[i]=(ar[0]*br[1]+br[0])/(1.-ar[1]*br[1])
        yi[i]=(al[1]*bl[0]+al[0])/(1.-al[1]*bl[1])
        yo[i]=(ar[1]*br[0]+ar[0])/(1.-ar[1]*br[1])
        plots,xi[i],yi[i],psym=2
endfor
;xo=xo-off[0]+offx
;yo=yo-off[1]+offy
xo=xo+offx
yo=yo+offy
polywarp,xo,yo,xi,yi,1,kx,ky
rimg3=poly_2d(rimg,kx,ky,1)

wdef,1,600,800
plot_image,limg-rimg3

if keyword_set(outfile) then save,kx,ky,shiftx1,kxkyh,file=outfile
END
;-------------------------------------------------------------
;+
; NAME       : hd2angle.pro (procedure)
; PURPOSE :
;       read angle informations from header
; CATEGORY :
;        idlpro/polobs/
; CALLING SEQUENCE :
;        hd2angle,hd,ha,zd,r,p,i 
; INPUTS :
;       hd   --  header of file
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
;        T.A. '17/02/09  => dstsp_hd2angle, polarimeterlib_v3.pro
;-
;-------------------------------------------------------------
pro dstsp_hd2angle,hd,ha,zd,r,p,i,az,imgrot

;zd = abs(hd.zd/3600.*!dtor)      ;rad
zd = hd.zd/3600.*!dtor      ;rad
ha = hd.ha/3600.*15.*!dtor
pos=where(ha ge !pi/2.,npos)
if npos ge 1 then ha[pos]=ha[pos]-2.*!pi ;rad
;if ha ge !pi/2. then ha=ha-2.*!pi ;rad
r=hd.r/3600.*!dtor
p=hd.p*!dtor
i=hd.i*!dtor
az=hd.az/3600.*!dtor
imgrot=hd.imgrot/3600.*!dtor

END
;-------------------------------------------------------------
;+
; NAME       : dstsp_mmdst.pro (function)
; PURPOSE :
;       return Mueller matrix for DST model
; CALLING SEQUENCE :
;        res=mm_dst(ha,zd,incli,telpos,ro_N,tau_N,ro_C,tau_C,sc,
;                     th_en,del_en,th_ex,del_ex)
; INPUTS :
; OUTPUT :
;       ro_N    ro of Newton mirror
;       tau_N   tau of Newton mirror
;       ro_C    ro of Coude mirror
;       tau_N   tau of Coude mirror
;       ha      hour angle
;       zd      zenith distance
; OPTIONAL INPUT PARAMETERS : 
; KEYWORD PARAMETERS :
;       imgrot  angle of image rotator [rad]
;       wvl     wavelength [nm]
;       version = 0)VSP, 1)hogehoge?, 2)HSP ()
;                 3)HSP(20170103) IR fixed measured, MIRR fitted
;             
; MODIFICATION HISTORY :
;      T.A. '11/06/14
;      T.A. '12/06/03  phi_N=za => -za
;      T.A. '12/06/16  sign
;      T.A. '13/08/04  keyword newton
;      T.A. '13/08/17  sign of zd
;      T.A. '16/09/09  horizontal spectropolarimeter, imgrot
;      T.A. '16/11/02  add keywords zd, ha, wave, and az
;      T.A. '16/11/03  version 2
;      T.A. '17/01/16  revive DST/VS (version 0)
;      T.A. '17/01/18  version 3, MMSP2(IR&MIRR) measured 2017.01.03 
;      T.A. '17/02/09  => dstsp_mmdst, polarimeterlib_v3.pro
;-------------------------------------------------------------
function dstsp_mmdst,hd,par,newton=newton,phin=phin,         $
        hsp=hsp,imgrot=imgrot,zd=zd,ha=ha,az=az,incli=incli,    $ ;2016.11.02 T.A.
        version=version
        ;xn=xn,tn=tn,xc=xc,tc=tc,sc=sc,dlen=dlen,t_en=t_en,dlex=dlex,t_ex=t_ex;2016.11.02 T.A.
lat=36.252/!radeg               ;Hidaten
if not keyword_set(version) then begin  ;=> 20170116 TA
     if keyword_set(hsp) then begin
         version=2
     endif else begin
         version=0
     endelse
endif                                   ;20170116 TA <=
if keyword_set(zd) then zd0=zd else zd0=0.
if keyword_set(ha) then ha0=ha else ha0=0.
if keyword_set(az) then az0=az else az0=0.
if keyword_set(imgrot) then imgrot0=imgrot else imgrot0=0.
if keyword_set(incli) then incli0=incli else incli0=0.
if size(hd,/type) eq 8 then dstsp_hd2angle,hd,ha0,zd0,r0,p0,incli0,az0,imgrot0
zd=zd0
ha=ha0
az=az0
imgrot=imgrot0
incli=incli0
if zd ge 0 then telpos='west' else telpos='east'

case version of
        0:begin ;=> 20170116 TA
                xn=par.xn
                tn=par.tn
                xc=par.xc
                tc=par.tc
                sc=par.sc
                dlen=par.dlen
                ten=par.t_en
                dlex=par.dlex
                tex=par.t_ex
        end     ;20170116 TA <=
        1:begin
                xn=par.xn
                tn=par.tn
                xc=par.xc
                tc=par.tc
                sc=par.sc
                dlen=par.dlen
                ten=par.t_en
                dlex=par.dlex
                tex=par.t_ex
                th_dst_mmsp2=par.th_dst_mmsp2
                th_mmsp2_hsp=par.th_mmsp2_hsp
                res_iter10_2=par.par_mmsp2
        end
        2:begin
                xn=par[0]
                tn=par[1]
                xc=par[2]
                tc=par[3]
                sc=par[4]
                dlen=par[5]
                ten=par[6]
                dlex=par[7]
                tex=par[8]
                th_dst_mmsp2=par[9]
                th_mmsp2_hsp=par[10]
                res_iter10_2=par[11:44]
        end
        3:begin
                xn=par[0]
                tn=par[1]
                xc=par[2]
                tc=par[3]
                sc=par[4]
                dlen=par[5]
                ten=par[6]
                dlex=par[7]
                tex=par[8]
                th_dst_mmsp2=par[9]
                th_mmsp2_hsp=par[10]
                mm_45=reform(par[27:42],4,4)
        end
       else:print,'no version in mmdst.pro'
endcase

;-------
zd=abs(zd)                      ;20130817
za=asin(cos(lat)*sin(ha)/sin(zd))
phi_N=za
if not keyword_set(hsp) then begin  ; VS/SP 20160927 TA
        if telpos eq 'west' then begin
            phi_C=-zd
            phi_v=+zd-za+incli
        endif else begin
            phi_C=+zd
            phi_v=-zd-za+incli
        endelse
endif else begin                        ; HS/SP 20160927 TA
        phi_v=(-az+!pi)
        if telpos eq 'west' then begin
            phi_C=-zd
        endif else begin
            phi_C=+zd
        endelse
 endelse


M_S=[$
        [1.+sc ,0.,0.,0.],      $
        [0.    ,1.,0.,0.],      $
        [0.    ,0.,1.,0.],      $
        [0.    ,0.,0.,1.]       $
        ]
M_p=[$
        [1.,0.,0.,0.],  $
        [0.,1.,0.,0.],  $
        [0.,0.,-1.,0.], $
        [0.,0.,0.,-1.]  $
        ]
M_G=M_p
M_N=muellermatrix_mirr(tn,xn,/gen)
M_C=muellermatrix_mirr(tc,xc,/gen)
D_en=Muellermatrix_WP(dlen,ten)
D_ex=Muellermatrix_WP(dlex,tex)
R_N=muellermatrix_rot(phi_N)
R_C=muellermatrix_rot(phi_C)
R_pl=muellermatrix_rot(phi_v)

mat=R_pl##D_ex##M_C##M_G##R_C##M_N##M_P##D_en##R_N
if keyword_set(newton) then mat=R_pl##D_ex##M_C##M_G##R_C##M_N##M_P##D_en
phin=phi_N


if keyword_set(hsp) then begin
   R_mmsp2=muellermatrix_rot(th_dst_mmsp2)
   R_rw=muellermatrix_rot(th_mmsp2_hsp)
   case version of
      2:begin
        mm_ir=reform(res_iter10_2[0:15],4,4)
        mm_45=reform(res_iter10_2[16:31],4,4)
        th1=res_iter10_2[32]*!dtor
        th2=res_iter10_2[33]*!dtor
        mmsp2=  muellermatrix_rot(-th2) ## $
                muellermatrix_rot(imgrot) ## $
                  mm_ir ## $
                muellermatrix_rot(-imgrot) ## $
                mm_45 ## $
                muellermatrix_rot(th2) ## $
                muellermatrix_rot(th1)
        ;for ii=0,3 do for jj=0,3 do mmsp2[ii,jj]=mmsp2[ii,jj]/mmsp2[0,0]  no effect
        ;mmsp2[2,*]=-mmsp2[2,*]
        ;mmsp2[*,2]=-mmsp2[*,2]
        mmsp2[3,*]=-mmsp2[3,*]
        mmsp2[*,3]=-mmsp2[*,3]
      end
      3:begin
        ;files=file_search('/home/kouui/mmsp2/image-rotator/save/*.sav',count=nf)
        files=file_search('/sp_pub/image-rotator/save/*.sav',count=nf)
        files=[files[0:51],files[53:*]]
        angles=[0.00,1.00,2.00,3.00,4.00,6.00,8.00,10.00,12.01,14.00,16.00, $
                17.99,20.00,22.01,24.00,26.00,28.00,30.00,32.00,34.00,36.01,$
                38.00,40.00,42.01,44.00,46.00,48.01,50.00,52.01,54.00,56.00,$
                58.00,60.00,62.01,64.00,66.00,68.01,70.00,72.01,74.00,76.00,$
                78.00,80.00,82.00,84.00,86.00,87.99,90.00,92.01,94.00,96.00,$
                98.00,-2.00,-4.00,-6.00,-8.00,-10.00,-12.00,-14.00,-16.00,  $
                -20.00,-21.99,-23.99,-26.00,-28.00,-30.00,-31.99,    $
                -34.00,-36.00,-37.99,-40.00,-42.00,-43.99,-46.00,-48.00,    $
                -49.99,-52.00,-54.00,-56.00,-57.99,100.01,102.00,104.00,    $
                106.00,108.01,110.00,112.00,114.00,116.00,118.00,120.01,    $
                122.01,124.00,126.00,128.00,130.00,132.01,134.00,136.00,    $
                138.01,140.00,142.01,144.00,146.00,148.01,150.00]
        pos=where(angles ne -30.0 and angles ne -31.990)
        files=files[pos]
        angles=angles[pos]
        dangle=imgrot*!radeg-angles
        tmp=min(abs(dangle),i0)
;print,'restore, ',files[i0]
        restore,files[i0]
        tmp=min(abs(wl-float(hd.wave)*.1),i)
        mm_ir=reform(mm[*,*,i])
        ;mm_ir=reform(rebin(mm[*,*,i-5:i+5],4,4,1))
        mmsp2=  muellermatrix_rot(dangle[i0]*!dtor) ## $
                mm_ir ## $
                muellermatrix_rot(-dangle[i0]*!dtor) ## $
                muellermatrix_rot((88.2 - 358.2)*!dtor) ## $
                  mm_45
       end
   endcase

   mat= R_rw ## mmsp2 ## R_mmsp2 ## mat
endif
mat=m_s##mat
;stop
return,mat
END
;-------------------------------------------------------------
;+
; NAME:
;       main
; PURPOSE:
;       reduction to full Stokes spectra
; CATEGORY:
; CALLING SEQUENCE:
;       main,files,darkfile,flatfile,kxkyfile,hazdfile,reffile,dst,iquv,hds
; INPUTS:
;         FILES = file names
;         DARKFILE = file name created by 
;         FLATFILE = file name created by 
;         KXKYFILE = file name created by 
;         HAZDFILE = file name created by 
;         REFFILE = file name created by 
;         DSTFILE = parameters for calibration () 
; KEYWORD PARAMETERS:
;         ORCA4 = 0) not using ORCA4 1) using ORCA4
;         SIGNV = sign of V of induceing light to slit, 1) for +V, -1) for -V
;         MINUS = 0) for normal camera setting, 1) for reverse
;         ABCD  = parameters for Kuhn's method
;         LIMB  = rotate +Q axis to parallel limb
;         REMOVE_ICROSSTALK = range of continuum to subtract crosstalk from I
;                             [x1,x2,y1,y2]
;         SKY   = [x1,x2,y1,y2], range of continuum to subtract sky
;         LINEAR = linearity of detector, 1)linear 2)non linear for XEVA640
;         OUTFILE = save file
;         SLITIQUV = return full Stokes IQUV at slit
;         DUALIQUV = return full Stokes IQUV of dual lights
; OUTPUTS:
;         IQUV = full Stokes IQUV (slit,wavelength,iquv,# file)
;         HDS  = INDEX
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;         2017.02.06   T.A.
;-
;-------------------------------------------------------------
pro main,files,darkfile,flatfile,kxkyfile,hazdfile,reffile,dst,         $;inputs
        orca4=orca4,    $
        signv=signv,    $; 1 for +V, -1 for -V, sign of V of induceing light to slit
        minus=minus,    $; 0 for normal camera setting, 1 for reverse
        abcd=abcd,      $; Kuhn's method
        limb=limb,      $; rotate +Q axis to parallel limb
        remove_icrosstalk=remove_icrosstalk,    $
          ; [x1,x2,y1,y2], range of continuum to subtract crosstalk from I
        sky=sky,        $; [x1,x2,y1,y2], range of continuum to subtract sky
        linear=linear,  $; linearity of detector, 1)linear 2)non linear for XEVA640
	ref_index=ref_index,      $
        iquv,hds,outfile=outfile,slitiquv=slitiquv,dualiquv=dualiquv,outarr=outarr   

if not keyword_set(orca4) then orca=0. else orca=1
if not keyword_set(signv) then signv=1.
if not keyword_set(minus) then minus=0.
if not keyword_set(linear) then linear=0.
cutfov=0                ; 1 for cut FOV, 0 for full FOV
fringe=0
restore,darkfile
restore,flatfile

ss=size(kxkyfile)
if ss[0] eq 0 then nf=1 else nf=ss[1]
for i=0,nf-1 do begin
        restore,kxkyfile[i]
        if i eq 0 then begin
                ss=size(kx)
                if ss[0] eq 2 then nk=1 else nk=ss[3]
                kxs=fltarr(4,nf*nk)
                kys=fltarr(4,nf*nk)
        endif
        for ik=0,nk-1 do begin
                kxs[*,i*nk+ik]=kx[*,*,ik]
                kys[*,i*nk+ik]=ky[*,*,ik]
        endfor
        if i eq 0 then begin
                shiftx1s=shiftx1
                tmp=kxkyh
        endif else begin
                shiftx1s=[shiftx1s,shiftx1]
                tmp=[tmp,kxkyh]
        endelse
endfor
kxkyh=tmp

restore,hazdfile
restore,reffile
if keyword_set(ref_index) then begin
	ref_time=((anytim2utc(ref_index.date_end)).time/1000d0 + 9.*3600.) mod (24.*3600.)
endif

ss=size(files)
if ss[0] eq 0 then nf=1 else nf=ss[1]
;---------------
ss=0b;!NULL
darkfile=0b;!NULL
flatfile=0b;!NULL
kx=0b;!NULL
ky=0b;!NULL
shiftx1=0b;!NULL
hazdfile=0b;!NULL
reffile=0b;!NULL
;---------------

key=1
for i=0,nf-1 do begin
;if i ge 2 and i le 54 then goto,jump
        print,i,nf
        if keyword_set(orca4) then begin
                read_orca,files[i],index,data,/nodata
                hn=dstsp_mkindex(header=index[0],ver=1,    $
			az=fltarr(n_elements(index)),imgrot=fltarr(n_elements(index)))
        endif else begin
                mreadfits,files[i],index,data,/nodata
                hn=dstsp_mkindex(header=index[0],ver=2,    $
			az=fltarr(n_elements(index)),imgrot=fltarr(n_elements(index)))
        endelse

        index=0b;!NULL
        nx=hn.naxis1
        ny=hn.naxis2
	time0=((anytim2utc(hn.date_end)).time/1000d0 +9.*60.*60.) mod (24.*60.*60.)
        hn.ha=interpol(ha0,time,time0)
        hn.zd=interpol(zd0,time,time0)
        hn.r=interpol(radius,time,time0)
        hn.p=interpol(pangle,time,time0)
        hn.i=interpol(incli,time,time0)
        hn.az=interpol(azimuth,time,time0)*3600.; arcsec        ;*!dtor  ;rad
        hn.imgrot=interpol(irangle,time,time0)*3600. ;arcsec    ;*!dtor  ;rad
        ;az=hd2az(hn);rad

        dpos=selectdark(dh,hn)
        if dpos[0] eq -1 then begin
                print,'no dark'
                goto,jump
        endif
        dark=reform(rebin(darks[0:nx-1,0:ny-1,dpos],nx,ny,1))

        if min(flats) eq 1 then begin
                fpos=selectkxky(fh,hn)
                if fpos[0] eq -1 then begin
                        print,'no flat'
                        flat=fltarr(nx,ny)+1.
                endif else begin
                        flat=reform(rebin(flats[0:nx-1,0:ny-1,fpos],nx,ny,1))
                endelse
        endif else begin
                flat=flats
        endelse

        pos=selectkxky(kxkyh,hn[0])
        if pos[0] ne -1 then begin
                kx=kxs[*,pos]
                ky=kys[*,pos]
                shiftx1=shiftx1s[pos]
        endif else begin
                print,'no kx, ky'
                goto,jump
        endelse
if hn.DETNAM eq 'XEVA640' then hn.period=10.   ;oukyusyochi
        ret=median(rets[selectref(refh,hn)])
        offset=median(offsets[selectref(refh,hn)])
        ratio=median(ratios[selectref(refh,hn)])
        frate=median(frates[selectref(refh,hn)])
        offsety=reform(rebin(offsetys[*,selectref(refh,hn)],ny,1))
        if (size(hspbs03s))[0] ge 1 then hspbs03=reform(rebin(hspbs03s[*,*,selectref(refh,hn)],nx,ny,1))
        if (size(pbsret1s))[0] ge 1 then pbsret1=reform(rebin(pbsret1s[*,*,selectref(refh,hn)],nx,1,1)) else pbsret1=0
        print,'retardation (deg)',ret*!radeg
        print,'ratio',ratio
        print,'offset',offset*!radeg
        print,'frame rate (Hz)',frate

        dstsp_mkiquv,files[i],ret,offset,dark=dark,flat=flat,$
                iquv0,hd,$
                header=hn,fringe=fringe,ver=1,frate=frate,   $
                offsetx=0,offsety=offsety,orca=orca,         $
                outarr=outarr,linear=linear,pbsret1=pbsret1
                ;header=hn,fringe=fringe,ver=1,offsetx=0,offsety=offsety,orca=orca,outarr=outarr
        for jj=0,3 do  iquv0[*,*,jj]=shift_img(iquv0[*,*,jj],[shiftx1,0])
        ;iquv1=dstsp_dualfit(iquv0,kx,ky,ratio,minus=minus,hspbs03=0,ret=ret)
        iquv1=dstsp_dualfit(iquv0,kx,ky,ratio,minus=minus,ret=ret)
        iquv1[*,*,3]=signv*iquv1[*,*,3]

        ; Cut and DST parameters
        ;if i eq 0 then begin
        if key then begin
                if cutfov then begin
                        facx=700./(nx/2.)
                        facy=700./ny
                         wdef,0,700,700;nx/8,ny/8
                        tvscl,congrid(iquv1[*,*,0],700,700,1)
                        print,'click left-lower corner 1'
                        cursor,/dev,x1,y1
                        wait,1.
                        print,'click right-upper corner 2'
                        cursor,/dev,x2,y2
                        wait,1.
                        x1=fix(x1/facx)
                        x2=fix(x2/facx)
                        y1=fix(y1/facy)
                        y2=fix(y2/facy)
                endif else begin
                        x1=0
                        x2=nx/2-1
                        y1=0
                        y2=ny-1
                endelse
                nnx=(x2-x1+1)
                nny=(y2-y1+1)
                ;iquv=fltarr(nnx,nny,4,nf)
                ;slitiquv=fltarr(nnx,nny,4,nf)
                iquv=fltarr(1024,2048,4,nf)
                slitiquv=fltarr(1024,2048,4,nf)
                dualiquv=fltarr(1024*2,2048,4,nf)
                if n_elements(dst) eq 5 then begin
                        par=par_dst(hd[0])
                        par.xn=dst[0];xn
                        par.tn=dst[1];tn
                        par.xc=dst[2];xc
                        par.tc=dst[3];tc
                        par.sc=dst[4];sc
                        hsp=0
                endif else begin
                        par=dst
                        hsp=1
                endelse
                hds=hd

                key=0
        endif else hds=[hds,hd]
        if cutfov eq 0 then begin
                x1=0
                x2=nx/2-1
                y1=0
                y2=ny-1
        endif
        if x1 lt 0 or x2 ge nx or y1 lt 0 or y2 ge ny then goto,jump
;help,slitiquv[*,*,*,i],iquv1
        slitiquv[0:nx/2-1,0:ny-1,*,i]=iquv1
        dualiquv[0:nx-1,0:ny-1,*,i]=iquv0
        if hsp eq 0 then begin
                imdst=invert(dstsp_mmdst(hd,par))
        endif else begin
                imdst=invert(dstsp_mmdst(hd,par,/hsp,version=3))
        endelse
        iquv2=(m44_iquv(imdst,iquv1))[x1:x2,y1:y2,*]

        ; Kuhn
        iquv3=iquv2
        if keyword_set(abcd) then begin
                print,'Kuhn s method'
                tmp=ta_ct_kuhn_0(iquv2,[0,0],[0,0],iquv3,nov=0,abcd=abcd)
        endif

        ; Limb
        iquv4=iquv3
        if keyword_set(limb) then begin
                print,'rotate +Q axis to pararel limb',hd.p
                mm=muellermatrix_rot(hd.p*!dtor)
                iquv4=m44_iquv(mm,iquv3)
        endif

        ; Subtract crosstalk I
        iquv5=iquv4
        if keyword_set(remove_icrosstalk) then begin
                print,'remove crosstalk from I'
                for quv=1,3 do begin
                        img=iquv4[*,*,quv]/iquv4[*,*,0]
                        img=img-mean(img[remove_icrosstalk[0]:remove_icrosstalk[1],remove_icrosstalk[2]:remove_icrosstalk[3]])
                        iquv5[*,*,quv]=img*iquv4[*,*,0]
                endfor
        endif

        ; Subtract sky
        iquv6=iquv5
        if keyword_set(sky) then begin
                print,'subtract sky'
                sky=reform(rebin(iquv5[sky[0]:sky[1],*,0],1,ny,1,1))
                for ii=0,3 do begin
                        for x=0,nx/2-1 do begin
                                iquv6[x,*,ii,i]=iquv5[x,*,ii]-sky/median(sky[sky[2]:sky[3]])*median(iquv5[x,sky[2]:sky[3],ii,i])
                        endfor
                endfor
        endif

        iquv[0:nx/2-1,0:ny-1,*,i]=iquv6
        if i eq 0 then begin
                nxmax=nx
                nymax=ny
        endif else begin
                nxmax=nxmax>nx
                nymax=nymax>ny
        endelse

	if keyword_set(ref_index) then begin
		time_data=((anytim2utc(hds[i].date_end)).time/1000d0 + 9.*3600.) mod (24.*3600.)
		tmp=min(abs(ref_time-time_data),pos)
		hds[i].OBS_TYPE=ref_index[pos].OBS_TYPE
		hds[i].POLSTATE=ref_index[pos].POLSTATE
	endif
        jump:
endfor
iquv=iquv[0:nxmax/2-1,0:nymax-1,*,*]
slitiquv=slitiquv[0:nxmax/2-1,0:nymax-1,*,*]
;stop
if keyword_set(outfile) then save,hds,iquv,x1,x2,y1,y2,file=outfile
 

END




























