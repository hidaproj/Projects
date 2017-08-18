; pro_obslib.pro
;
;   2009.12.17  T.A.
;回転波長板情報（いまsavefile）
;**************************************************************
function cdio_init
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

diodll='C:\Projects\cprog\VS2005\DIO8\Debug\DIO8.dll'

r=call_external(diodll,'Cdio_Init',/all_value,/cdecl)
return,r

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end

;**************************************************************
pro cdio_exit
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

r=call_external(diodll,'Cdio_Exit',/all_value,/cdecl)

end
;**************************************************************
function cdio_input
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

InByte=call_external(diodll,'Cdio_InByte',/all_value,/cdecl)

Case InByte of
	0:	wpin='Non'
	1:	wpin='JOG(+)中   	JOGing(+)'
	33:	wpin='JOG(+)中   	JOGing(+)'
	2:	wpin='JOG(-)中   	JOGing(-)'
	34:	wpin='JOG(-)中   	JOGing(-)'
	3:	wpin='目的角度移動中	moving to destination'
	35:	wpin='目的角度移動中	moving to destination'
	4:	wpin='機械原点移動中	moving to origin'
	36:	wpin='機械原点移動中	moving to origin'
	32:	wpin='装置原点移動完了  origin'
	6:	wpin='原点復帰していない　not yet find origin'
	7:	wpin='原点復帰していない　not yet find origin'
	8:	wpin='原点復帰していない　not yet find origin'
	9:	wpin='原点復帰していない　not yet find origin'
	10:	wpin='原点復帰していない　not yet find origin'
	12:	wpin='原点復帰していない　not yet find origin'
	13:	wpin='原点復帰していない　not yet find origin'
	14:	wpin='原点復帰していない　not yet find origin'
	15:	wpin='原点復帰していない　not yet find origin'
	38:	wpin='45°(移動完了)'
	39:	wpin='90°(移動完了)'
	40:	wpin='135°(移動完了)'
	41:	wpin='180°(移動完了)'
	42:	wpin='225°(移動完了)'
	44:	wpin='270°(移動完了)'
	45:	wpin='315°(移動完了)'
	46:	wpin='360°(移動完了)'
	47:	wpin='+22.5°(移動完了)'
	17:	wpin='Error'
	49:	wpin='Error'
	18:	wpin='パルス出力異常	abnormality of pulse'
	50:	wpin='パルス出力異常	abnormality of pulse'
	else:	wpin='?'
endcase

return,wpin

end

;**************************************************************
function cdio_outstate
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

outstate=call_external(diodll,'Cdio_OutState',/all_value,/cdecl)

Case outstate of
	0:	wpout='Non'
	1:	wpout='45°移動		go 45°'
	2:	wpout='90°移動		go 90°'
	3:	wpout='135°移動		go 135°'
	4:	wpout='180°移動		go 180°'
	5:	wpout='225°移動		go 225°'
	6:	wpout='270°移動		go 270°'
	7:	wpout='315°移動		go 315°'
	8:	wpout='360°移動		go 360°'
	16:	wpout='原点移動		go origin'
	32:	wpout='JOG(+)'
	48:	wpout='JOG(-)'
	64:	wpout='ゼロセット	set origin'
	96:	wpout='停止		Stop'
	80:	wpout='+22.5°移動	Move +22.5°'
	112:	wpout='装置原点アドレス書き込み'
endcase

return,wpout

end

;**************************************************************
pro cdio_o45
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

OutByte=1
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o90
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

OutByte=2
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o135
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

OutByte=3
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o180
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

OutByte=4
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o225
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

OutByte=5
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o270
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

OutByte=6
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o315
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

OutByte=7
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o360
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

OutByte=8
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_op25
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

OutByte=80
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_opjg
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

OutByte=32
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_omjg
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

OutByte=48
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_ostop
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

OutByte=96
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o0
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

OutByte=16
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_o0set
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

OutByte=64
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;**************************************************************
pro cdio_ow0ad
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

OutByte=112
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

for i=0,9 do begin
	wait,0.2
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output
endfor

OutByte=0
r=call_external(diodll,'Cdio_OutByte',OutByte,/all_value,/cdecl)

end
;============================================================================================
;**************************************************************
function timeinit
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

cd,'C:\Projects\cprog\VS2005\prosilica'
prodll='C:\Projects\cprog\VS2005\prosilica\Debug\prosilica.dll'

caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
second=string(seco,form='(i2.2)')	&	second0=second
while (second eq second0) do begin
	caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
	second=string(seco,form='(i2.2)')
endwhile

msec0=call_external(prodll,'getime',/all_value,/cdecl)
return,msec0
end

;**************************************************************
pro pro_init
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

r=call_external(prodll,'CamInit',/all_value,/cdecl)

end
;**************************************************************
pro pro_exit
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

r=call_external(prodll,'CamFin',/all_value,/cdecl)

end
;**************************************************************
pro pro_setparam
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

p={prosilica_param, $
	expo:		wp.expo,		$		; exposure time [msec]
	gain:		wp.gain,		$		; gain 0～28
	nimg:		wp.nimg, 		$		;  # of image 
	binx:		wp.binx, 		$		; Binning X 1～8
	biny:		wp.biny, 		$		; Binning Y 1～1200
	Height:		wp.Height,	 	$		; Height  max=1200 (biny=1)
	Width:		wp.Width,	 	$		; Width  max=1600 (binx=1)
	RegionX:	wp.RegionX, 		$		; start of region read out,pixel,left edge
	RegionY:	wp.RegionY, 		$		; start of region read out,pixel,top edge
	clock: 		wp.clock, 		$		; TimeStanmpFrequency [Hz]
	timelo:		wp.timelo,		$		; Time stamp, lower 32-bits
	timehi:		wp.timehi,		$		; Time stamp, upper 32-bits
	n_evsample: 	wp.n_evsample 		$		; omake
}

help,p,/st

r=call_external(prodll,'SetParam',wp.expo,wp.gain,wp.binx,wp.biny,wp.Width,wp.Height,wp.RegionX,wp.RegionY,/all_value,/cdecl)

end
;**************************************************************
pro pro_preview,img
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

r=call_external(prodll,'GrabImg',1,/all_value,/cdecl)
r=call_external(prodll,'DivBuf',0,/all_value,/cdecl)
r=call_external(prodll,'GoIdl',img,/cdecl)

tvscl,img

end
;**************************************************************
pro getimg
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

r=call_external(prodll,'GrabImg',wp.nimg,/all_value,/cdecl)

end
;**************************************************************
function gettime
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
yyyymmdd_hhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
	+'_'+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')
msec=call_external(prodll,'getime',/all_value,/cdecl)

time0=msec-msec0
length=strlen(string(time0))
msec=strmid(string(time0),(length-3),3)

time=yyyymmdd_hhmmss+msec
return,time

end

;**************************************************************
pro savefits,obs,pol,sttime,entime
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

outfile=wp.svdir+wp.fname+sttime+'.fits'
print,outfile 

hangle=0l	&	zd=0l
;if (obs ne 'NORMAL') then dstangl,hangl,zd
	dstangl,hangl,zd
;hangle=strcompress(string(hangle),/remove_all)	&	zd=strcompress(string(zd),/remove_all)
rr=wp.r_m+'m'+wp.r_s+'s'
p=wp.p_d+'d'+wp.p_m+'m'
i=wp.i_d+'d'+wp.i_m+'m'+wp.i_s+'s'


r=call_external(prodll,'openfits',outfile,/all_value,/cdecl)
r=call_external(prodll,'WriteImage',wp.nimg,/all_value,/cdecl)
r=call_external(prodll,'addkeywords_prosilica',			$
					obs,		$
					wp.wavelength,	$
					pol,		$
					sttime,		$
					entime,		$
					rr,		$
					p,		$
					i,		$
					hangl,	$
					zd,	$
					'pro_obs',	$
					/all_value,/cdecl)
r=call_external(prodll,'closefits',/all_value,/cdecl)

end
;**************************************************************
pro savestmpfits,sttime
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

outfile=wp.svdir+'stmp'+sttime+'.fits'
print,outfile 

r=call_external(prodll,'TimeStamp',outfile,wp.nimg,/all_value,/cdecl)

end


;**************************************************************
function AIO_start
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

nchan=1

caio_init
;caio_Set,nchan=nchan,rate=400			;maximum sampling rate (1ch) = 1666 (=1sec/0.6msec) 
;p=caio_getinfo() &	help,p,/st 
;caio_start
;caio_trig,nchan
caio_Set,nchan=nchan,rate=2000			;maximum sampling rate (1ch) = 1666 (=1sec/0.6msec) 
p=caio_getinfo() &	help,p,/st 
caio_start

return,p
end
;**************************************************************
function AIO_read
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

nchan=1

;caio_stop
st=caio_status()
ndat=caio_count()
print,st,ndat

dd = caio_read(nchan,ndat,ret=ret)

caio_exit

return,dd
end

;**************************************************************
function AIO_stop1
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

nchan=1

caio_stop
st=caio_status()
ndat=caio_count()
print,st,ndat

dd = caio_read(nchan,ndat,ret=ret)

caio_exit

return,dd
end

;**************************************************************
function NormalObs
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

pro_init
pro_setparam
sttime=gettime()
getimg
entime=gettime()
savefits,'NORMAL','',sttime,entime
savestmpfits,sttime
pro_exit

filename=wp.svdir+wp.fname+sttime+'.fits'
return,filename

end
;**************************************************************
function PolObs,polstate
;pro PolObs,polstate
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

pro_init
pro_setparam
p=AIO_start()
sttime=gettime()
getimg
caio_stop
entime=gettime()
savefits,'POL',polstate,sttime,entime
savestmpfits,sttime
pro_exit
rtwv=AIO_read();	&plot,rtwv
save,file=wp.svdir+'wvplt'+sttime+'.sav',p,rtwv

filename=wp.svdir+wp.fname+sttime+'.fits'
return,filename

end
;**************************************************************
pro PolObs1,polstate
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

pro_init
pro_setparam
p=AIO_start()
sttime=gettime()
getimg
rtwv=AIO_stop1();	&plot,rtwv
entime=gettime()
savefits,'POL',polstate,sttime,entime
savestmpfits,sttime
pro_exit

save,file=wp.svdir+'wvplt'+sttime+'.sav',p,rtwv

end
;**************************************************************
function CalibObs
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

fn=strarr(9)
rdio=cdio_init()
wp.input=cdio_input()
  widget_CONTROL,wd.in,set_value=wp.input
tmp=''
cdio_o45	&	purpose='45'
while (tmp eq '') do begin
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output

	if (wp.input eq '45°(移動完了)') and (purpose eq '45') then begin
		print,'45'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[0]=PolObs('45')
		cdio_o90	&	purpose='90'
print,'===== 45°終了 ====='
	endif
	if (wp.input eq '90°(移動完了)') and (purpose eq '90')  then begin
		print,'90'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[1]=PolObs('90')
		cdio_o135	&	purpose='135'
print,'===== 90°終了 ====='
	endif
	if (wp.input eq  '135°(移動完了)') and (purpose eq '135')  then begin
		print,'135'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[2]=PolObs('135')
		cdio_o180	&	purpose='180'
print,'===== 135°終了 ====='
	endif
	if (wp.input eq  '180°(移動完了)') and (purpose eq '180') then begin
		print,'180'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[3]=PolObs('180')
		cdio_o225	&	purpose='225'
print,'===== 180°終了 ====='
	endif
;	if (wp.input eq  '225°(移動完了)') and (purpose eq '225') then begin
	if (wp.input eq  '135°(移動完了)') and (purpose eq '225') then begin
		print,'225'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[4]=PolObs('225')
		cdio_o270	&	purpose='270'
		;wait,2
		while (wp.input eq  '135°(移動完了)') do wp.input=cdio_input()

print,'===== 225°終了 ====='
	endif
;	if (wp.input eq  '270°(移動完了)') and (purpose eq '270') then begin
	if (wp.input eq  '135°(移動完了)') and (purpose eq '270') then begin
		print,'270'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[5]=PolObs('270')
		cdio_o315	&	purpose='315'
print,'===== 270°終了 ====='
	endif
	if (wp.input eq  '315°(移動完了)') and (purpose eq '315') then begin
		print,'315'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[6]=PolObs('315')
		cdio_o360	&	purpose='360'
print,'===== 315°終了 ====='
	endif
	if (wp.input eq  '360°(移動完了)') and (purpose eq '360') then begin
		print,'0'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[7]=PolObs('0')
		cdio_op25	&	purpose='22.5'
print,'===== 0°終了 ====='
	endif
	if (wp.input eq  '+22.5°(移動完了)') and (purpose eq '22.5')  then begin
MessageBox,'無偏光撮影OK ??'
		print,'non'
		fn[8]=PolObs('')
		tmp='finish'
print,'===== 22.5°終了 ====='
	endif

	;print,wp.input
endwhile
cdio_exit

return,fn
end
;**************************************************************
pro widget_pro_obs_event, ev
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

rdio=1	&	stprm=1
widget_control, ev.id, get_uvalue=uvalue,get_value=value

print,'uvalue=',uvalue

	;r=cdio_init()
	;wp.input=''
	;for i=0,200 do widget_CONTROL,wd.in,set_value=wp.input
	;wp.input=cdio_input()
	;for i=0,200 do widget_CONTROL,wd.in,set_value=wp.input
	;cdio_exit


	if (uvalue eq "nimg") then begin
		if (fix(value) lt 0) or (fix(value) ge 100) then begin 
			;print,'integ must be from 0 to 100'
			MessageBox,'integ must be from 0 to 100'
		endif else begin	&	wp.nimg=fix(value)	&	endelse		&	endif	
	if (uvalue eq "expo") then wp.expo=float(value)
	if (uvalue eq "gain") then wp.gain=fix(value)
	if (uvalue eq "binx") then wp.binx=fix(value)
	if (uvalue eq "biny") then wp.biny=fix(value)
	if (uvalue eq "height") then begin
		if (fix(value) gt 1200/wp.binx) then begin 
			MessageBox,'Height must less than '+strcompress(string(1200/wp.binx),/remove_all)
  			widget_CONTROL,wd.height,set_value=string(wp.Height, form='(i5)')
		endif else begin  &  wp.Height=fix(value)  &  endelse  &  endif	
	if (uvalue eq "width") then begin
		if (fix(value) gt 1600/wp.biny) then begin 
			MessageBox,'Width must less than '+strcompress(string(1600/wp.biny),/remove_all)
  			widget_CONTROL,wd.width,set_value=string(wp.Width, form='(i5)')
		endif else begin  &  wp.Width=fix(value)  &  endelse  &  endif	
	if (uvalue eq "regionx") then begin
		MessageBox,'RegionX Help : Start of region readout, in pixels; left edge.'
		if (fix(value) gt wp.Width) then begin 
			MessageBox,'ReginX must less than Width'
  			widget_CONTROL,wd.regionx,set_value=string(wp.RegionX, form='(i5)')
		endif else begin  &  wp.RegionX=fix(value)  &  endelse  &  endif	
	if (uvalue eq "regiony") then begin
		MessageBox,'RegionY Help : Start of region readout, in pixels; top edge.'
		if (fix(value) gt wp.Height) then begin 
			MessageBox,'ReginY must less than Height'
  			widget_CONTROL,wd.regiony,set_value=string(wp.RegionY, form='(i5)')
		endif else begin  &  wp.RegionY=fix(value)  &  endelse  &  endif	

	if (uvalue eq "svdir") then wp.svdir=value
	if (uvalue eq "fname") then wp.fname=value
	if (uvalue eq "nf") then wp.nf=value
	if (uvalue eq "wavelength") then wp.wavelength=value
	if (uvalue eq "r_m") then wp.r_m=strcompress(string(value),/remove_all)
	if (uvalue eq "r_s") then wp.r_s=strcompress(string(value),/remove_all)
	if (uvalue eq "p_d") then wp.p_d=strcompress(string(value),/remove_all)
	if (uvalue eq "p_m") then wp.p_m=strcompress(string(value),/remove_all)
	if (uvalue eq "i_d") then wp.i_d=strcompress(string(value),/remove_all)
	if (uvalue eq "i_m") then wp.i_m=strcompress(string(value),/remove_all)
	if (uvalue eq "i_s") then wp.i_s=strcompress(string(value),/remove_all)
	
	ev_prevstop=widget_event(wd.bt_pren,/nowait)
	ev_opend=widget_event(wd.opend,/nowait)
	ev_opst=widget_event(wd.bs_wv_op,/nowait)

	;------ Save -------;	
	if (uvalue eq "svo") then begin
		fn=strarr(wp.nf)
		for i=0,wp.nf-1 do begin
print,'===== '+strcompress(string(i+1),/remove_all)+'枚目 ====='
			fn[i]=NormalObs()		
		endfor
		;==表示==;
		window,0,ys=wp.Height,xs=wp.Width
		for i=0,wp.nf-1 do begin
			mreadfits,fn[i],h,img
			for j=0,wp.nimg-1 do begin
				tvscl,img[*,*,j]
				xyouts,0.05,0.05,string(j),/norm
			endfor
		endfor
MessageBox,'!!FINISH NORMAL OBSERVATION!!'
	endif

	if (uvalue eq "svp") then begin
		fn=strarr(wp.nf)
		for i=0,wp.nf-1 do begin
print,'===== '+strcompress(string(i+1),/remove_all)+'枚目 ====='
			fn[i]=PolObs('')		
		endfor
		;==表示==;
		window,0,ys=wp.Height,xs=wp.Width
		for i=0,wp.nf-1 do begin
			mreadfits,fn[i],h,img
			for j=0,wp.nimg-1 do begin
				tvscl,img[*,*,j]
				xyouts,0.05,0.05,string(j),/norm
			endfor
		endfor
MessageBox,'!!偏光観測終了!!'
	endif

	if (uvalue eq "calib") then begin
		fn=CalibObs()
		;==表示==;
		window,0,ys=wp.Height,xs=wp.Width
		for i=0,8 do begin
			mreadfits,fn[i],h,img
			for j=0,wp.nimg-1 do begin
				tvscl,img[*,*,j]
				xyouts,0.05,0.05,string(j),/norm
			endfor
		endfor
MessageBox,'!!キャリブレーション観測終了!!'
	endif
;=========== preview ==========;


	if (uvalue eq 'prev_st') then begin
		pro_init
		pro_setparam
		if (n_elements(windex) eq 0) and (!D.WINDOW eq -1) then begin
			window,0,xs=wp.Width,ys=wp.Height	&	windex=0	&	endif
		if (n_elements(windex) eq 0) and (!D.WINDOW ne -1) then begin
			windex=!D.WINDOW+1	&	window,windex,xs=wp.Width,ys=wp.Height	
		endif else begin  &  window,windex,xs=wp.Width,ys=wp.Height  &  endelse

		img=intarr(wp.Width,wp.Height)
		while ev_prevstop.id eq 0 do begin
			;print,'now previewing!'
			WSET, windex
			pro_preview,img

			ev_prevstop=widget_event(wd.bt_pren,/nowait)
			ev_emp=widget_event(/nowait)
		endwhile
		pro_exit
	endif

;========= profiles ==============;
	if (uvalue eq 'prof') then begin
		if (n_elements(windex) eq 0) and (!D.WINDOW eq -1) then begin
			window,0,xs=wp.Width,ys=wp.Height	&	windex=0	&	endif
		if (n_elements(windex) eq 0) and (!D.WINDOW ne -1) then begin
			windex=!D.WINDOW+1	&	window,windex,xs=wp.Width,ys=wp.Height	
		endif else begin  &  window,windex,xs=wp.Width,ys=wp.Height  &  endelse
		wset,windex
		pro_init
		pro_setparam
		proimg=intarr(wp.Width,wp.Height)
		pro_preview,proimg
		pro_exit
		profiles,proimg
	endif

;========= contec dio==============;
	if (uvalue eq 'diostart') then begin
		;while (ev_opst ne 0) and ev_opend.id eq 0 do begin
		while ev_opend.id eq 0 do begin
			if (rdio ne 0) then rdio=cdio_init()
			wp.input=cdio_input()
  			  widget_CONTROL,wd.in,set_value=wp.input
			wp.output=cdio_outstate()
			  widget_CONTROL,wd.out,set_value=wp.output


			if (uvalue eq 'o45') then cdio_o45 
			if (uvalue eq 'o90') then cdio_o90 
			if (uvalue eq 'o135') then cdio_o135 
			if (uvalue eq 'o180') then cdio_o180 
			if (uvalue eq 'o225') then cdio_o225 
			if (uvalue eq 'o270') then cdio_o270 
			if (uvalue eq 'o315') then cdio_o315 
			if (uvalue eq 'o360') then cdio_o360 
			if (uvalue eq 'op25') then cdio_op25 
			if (uvalue eq 'opjg') then cdio_opjg 
			if (uvalue eq 'omjg') then cdio_omjg 
			if (uvalue eq 'o0') then cdio_o0 
			if (uvalue eq 'o0set') then cdio_o0set 
			if (uvalue eq 'ow0ad') then cdio_ow0ad 
			if (uvalue eq 'ostop') then cdio_ostop 
			uvalue=''

			ev_dio = widget_event(wd.bs_wv_op,/nowait)
			if ev_dio.id ne 0 then $
			WIDGET_CONTROL, get_uvalue=uvalue, ev_dio.id

			ev_opend=widget_event(wd.opend,/nowait)
			ev_emp=widget_event(/nowait)
		endwhile
		cdio_exit
	endif

;====================================;

if (uvalue eq "EXIT") then WIDGET_CONTROL, /destroy, ev.top

end

;************************************************************************
pro widget_pro_obs
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

tmp='                                                             '
wp={widget_param, $
	wavelength:	'10830',	$		; wave length observed [A]
	expo:		50000l,		$		; exposure time [μsec]
	gain:		0,		$		; gain 0～28
	nimg:		100, 		$		;  # of image 
	binx:		2, 		$		; Binning X 1～8
	biny:		2, 		$		; Binning Y 1～1200
	Height:		600,	 	$		; Height  max=1200 (biny=1)
	Width:		800,	 	$		; Width  max=1600 (binx=1)
	RegionX:	0, 		$		; start of region read out,pixel,left edge
	RegionY:	0, 		$		; start of region read out,pixel,top edge
	clock: 		79861111l, 	$		; TimeStanmpFrequency [Hz]
	timelo:		0,		$		; Time stamp, lower 32-bits
	timehi:		0,		$		; Time stamp, upper 32-bits
	svdir:		'C:/data/dst/',	$		; save directory
	fname:		'fname',	$		; head of file name 
	nf:		2,		$		; number of files
	input:		tmp,		$		; contec cdio input 　＊モニター要の文字列メモリ（？）
	output:		tmp,		$		; contec cdio output　＊を確保する必要あり
	r_m:		' ',		$		; RADIUS [arcmin]
	r_s:		' ',		$		; RADIUS [arcsec]
	p_d:		' ',		$		; POLAR ANGLE [arcdeg]
	p_m:		' ',		$		; POLAR ANGLE [arcmin]
	i_d:		' ',		$		; INCLINATION [arcdeg]
	i_m:		' ',		$		; INCLINATION [arcmin]
	i_s:		' ',		$		; INCLINATION [arcsec]
	n_evsample: 	0l 		$		; omake
	}
wd={wd_cdio,	$
	bt_pren:	0l,	$		; プレビューストップ
	in:		0l,	$		; contec DIO 入力情報
	out:		0l,	$		; contec DIO 出力状況
	opend:		0l,	$		; contec DIO 操作ストップ
	bs_wv_op:	0l,	$		; contec DIO 操作スタート
	height:		0l,	$		; prosilica Height
	width:		0l,	$		; prosilica Width
	regionx:	0l,	$		; prosilica RegionX
	regiony:	0l,	$		; prosilica RegionY
	Exit:		0l	$
}
main = WIDGET_BASE(title='Prosilica Observation',/column)

	;== Set Parameter ==;
lab= widget_label(main,value='>>> Set Parameter <<<');,font=2)
bs_sp=widget_base(main, /column, /frame)
	bs_sp2=widget_base(bs_sp, /row)
		lab=widget_label(bs_sp2,value='      expo     : ')
		text=widget_text(bs_sp2,value=string(wp.expo, form='(i5)'), xsize=6, ysize=1, uvalue='expo',/edit)
		gains=strcompress(string(indgen(29)),/remove_all)
		drop=cw_bselector(bs_sp2,gains,label_left='us        gain    : ', uvalue="gain",set_value=0, ysize=1)
		lab=widget_label(bs_sp2,value='      integ   : ')
		text=widget_text(bs_sp2,value=string(wp.nimg, form='(i5)'), xsize=6, ysize=1, uvalue='nimg',/edit)
	bs_sp3=widget_base(bs_sp, /row)
		lab=widget_label(bs_sp3,value='      binx     : ')
		text=widget_text(bs_sp3,value=string(wp.binx, form='(i5)'), xsize=6, ysize=1, uvalue='binx',/edit)
		lab=widget_label(bs_sp3,value='      　　　　biny     　: ')
		text=widget_text(bs_sp3,value=string(wp.biny, form='(i5)'), xsize=6, ysize=1, uvalue='biny',/edit)
	bs_sp4=widget_base(bs_sp, /row)
		lab=widget_label(bs_sp4,value='    Height    : ')
		wd.height=widget_text(bs_sp4,value=string(wp.Height, form='(i5)'), xsize=6, ysize=1, uvalue='height',/edit)
		lab=widget_label(bs_sp4,value='pix   　　　Width　  　: ')
		wd.width=widget_text(bs_sp4,value=string(wp.Width, form='(i5)'), xsize=6, ysize=1, uvalue='width',/edit)
		lab=widget_label(bs_sp4,value='pix')
	bs_sp5=widget_base(bs_sp, /row)
		lab=widget_label(bs_sp5,value='   RegionX　 : ')
		wd.regionx=widget_text(bs_sp5,value=string(wp.RegionX, form='(i5)'), xsize=6, ysize=1, uvalue='regionx',/edit)
		lab=widget_label(bs_sp5,value='    　　　RegionY  　　: ')
		wd.regiony=widget_text(bs_sp5,value=string(wp.RegionY, form='(i5)'), xsize=6, ysize=1, uvalue='regiony',/edit)

	;== Observation ==;
lab_ob = widget_label(main,value='>>> Observation <<<');,font=2)
bs_ob=widget_base(main, /column, /frame)
	bs_sv=widget_base(bs_ob, /column)
		bs_sv0=widget_base(bs_sv, /row)
			lab=widget_label(bs_sv0,value='Save Directory : ')
			text=widget_text(bs_sv0,value=wp.svdir, xsize=45, uvalue='svdir',/edit)
		bs_sv1=widget_base(bs_sv, /row)
			lab=widget_label(bs_sv1,value='   File Name    : ')
			text=widget_text(bs_sv1,value=wp.fname, xsize=10, uvalue='fname',/edit)
			lab=widget_label(bs_sv1,value='          Number of Set : ')
			text=widget_text(bs_sv1,value=string(wp.nf, form='(i5)'), xsize=6, uvalue='nf',/edit)
		bs_sv2=widget_base(bs_sv, /row)
			lab=widget_label(bs_sv2,value=' Wave Length   : ')
			text=widget_text(bs_sv2,value=wp.wavelength, xsize=6, uvalue='wavelength',/edit,	$
					xoffset=100000)
			lab=widget_label(bs_sv2,value='[A]')
		bs_sv3=widget_base(bs_sv, /row)
			lab=widget_label(bs_sv3,value='r :')
			text=widget_text(bs_sv3,value=wp.r_m, xsize=4, uvalue='r_m',/edit,	$
					xoffset=100000)
			lab=widget_label(bs_sv3,value='m')
			text=widget_text(bs_sv3,value=wp.r_s, xsize=4, uvalue='r_s',/edit,	$
					xoffset=100000)
			lab=widget_label(bs_sv3,value='s')

			lab=widget_label(bs_sv3,value='  p :')
			text=widget_text(bs_sv3,value=wp.p_d, xsize=4, uvalue='p_d',/edit,	$
					xoffset=100000)
			lab=widget_label(bs_sv3,value='d')
			text=widget_text(bs_sv3,value=wp.p_m, xsize=4, uvalue='p_m',/edit,	$
					xoffset=100000)
			lab=widget_label(bs_sv3,value='m')

			lab=widget_label(bs_sv3,value='   i :')
			text=widget_text(bs_sv3,value=wp.i_d, xsize=4, uvalue='i_d',/edit,	$
					xoffset=100000)
			lab=widget_label(bs_sv3,value='d')
			text=widget_text(bs_sv3,value=wp.i_m, xsize=4, uvalue='i_m',/edit,	$
					xoffset=100000)
			lab=widget_label(bs_sv3,value='m')
			text=widget_text(bs_sv3,value=wp.i_s, xsize=4, uvalue='i_s',/edit,	$
					xoffset=100000)
			lab=widget_label(bs_sv3,value='s')
		bs_sv4=widget_base(bs_sv, /row)
			bt=widget_button(bs_sv4, value="Get (NOM)", uvalue = "svo",/align_center,xsize=120)
			bt=widget_button(bs_sv4, value="Get (POL)", uvalue = "svp",/align_center,xsize=120)
			bt=widget_button(bs_sv4, value="Get (CAL)", uvalue = "calib",/align_center,xsize=120)
	bs_pr=widget_base(bs_ob, /row)
		bt_prst=WIDGET_BUTTON(bs_pr,uvalue='prev_st',value='Preview Start',/align_center,xsize=123)
		wd.bt_pren=WIDGET_BUTTON(bs_pr,uvalue='prev_en',value='Preview Stop',/align_center,xsize=120)
		bt_prof=WIDGET_BUTTON(bs_pr,uvalue='prof',value='Profiles',/align_center,xsize=123)

lab_ob = widget_label(main,value='>>> 偏光板自動回転装置 <<<');,font=2)
bs_wv=widget_base(main, /column, /frame)
	bs_wv1=widget_base(bs_wv,/row)
		lab = widget_label(bs_wv1,value='OPERATION : ');,font=2)
		bt=widget_button(bs_wv1, value='Initialize', uvalue = 'diostart',/align_center,xsize=60)
		wd.opend=widget_button(bs_wv1, value='Close', uvalue = 'diostop',/align_center,xsize=60)
	lab = widget_label(bs_wv,value='<< STATE >>');,font=2)
	bs_wv_ip=widget_base(bs_wv, /column)
		bs_wv_ip1=widget_base(bs_wv_ip, /row)
			lab = widget_label(bs_wv_ip1,value='     State     = ');,font=2)
			wd.in = widget_label(bs_wv_ip1,value=wp.input)
		bs_wv_ip2=widget_base(bs_wv_ip, /row)
			lab = widget_label(bs_wv_ip2,value='Output State = ');,font=2)
			wd.out = widget_label(bs_wv_ip2,value=wp.output)
	lab = widget_label(bs_wv,value='<< OUTPUT >>');,font=2)
	wd.bs_wv_op=widget_base(bs_wv, /column)
		bs_wv_op1=widget_base(wd.bs_wv_op, /row)
			bt=widget_button(bs_wv_op1, value='45deg', uvalue = 'o45',/align_center,xsize=60)
			bt=widget_button(bs_wv_op1, value='90deg', uvalue = 'o90',/align_center,xsize=60)
			bt=widget_button(bs_wv_op1, value='135deg', uvalue = 'o135',/align_center,xsize=60)
			bt=widget_button(bs_wv_op1, value='180deg', uvalue = 'o180',/align_center,xsize=60)
			bt=widget_button(bs_wv_op1, value='225deg', uvalue = 'o225',/align_center,xsize=60)
			bt=widget_button(bs_wv_op1, value='270deg', uvalue = 'o270',/align_center,xsize=60)
		bs_wv_op2=widget_base(wd.bs_wv_op, /row)
			bt=widget_button(bs_wv_op2, value='315deg', uvalue = 'o315',/align_center,xsize=60)
			bt=widget_button(bs_wv_op2, value='360deg', uvalue = 'o360',/align_center,xsize=60)
			bt=widget_button(bs_wv_op2, value='+22.5deg', uvalue = 'op25',/align_center,xsize=60)
			bt=widget_button(bs_wv_op2, value='JOG(+)', uvalue = 'opjg',/align_center,xsize=60)
			bt=widget_button(bs_wv_op2, value='JOG(-)', uvalue = 'omjg',/align_center,xsize=60)
			bt=widget_button(bs_wv_op2, value='STOP', uvalue = 'ostop',/align_center,xsize=60)
		bs_wv_op3=widget_base(wd.bs_wv_op, /row)
			bt=widget_button(bs_wv_op3, value='ORIGIN', uvalue = 'o0',/align_center,xsize=120)
			bt=widget_button(bs_wv_op3, value='Read Origin Address', uvalue = 'ow0ad',/align_center,xsize=120)
wd.Exit = widget_button(main, value="Exit", uvalue = "EXIT")
widget_control, main, /realize
XMANAGER,'widget_pro_obs',main,modal=modal

end
;**************************************************************
pro MessageBox,kotoba
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

r=call_external(prodll,'CamOpen', kotoba, /PORTABLE )

end
;**************************************************************
function timestamp1,file
;--------------------------------------------------------------
common probslib,wd,wp,windex,diodll,prodll,msec0

mreadfits,file,h,dt

res=dblarr(h.EN_NO-h.ST_NO+1)
for i=h.ST_NO,h.EN_NO do begin
	res[i]=(dt[i,0]*2.^32.+dt[i,1])*1./(h.STMP_HZ*1.)	;[sec]
endfor

return,res

end
