;PRO DSTPOL_ObsLib
;+
;
;  DSTPOL_ObsLib.pro
;
;Functions for observation
;based on DST Poralization script by T. Anan and SMART T3 prosilica observation by T. Kawate
;
;20100908  T.A.
;20101125  T.A.		;hard trigger,MotorPulse,PrevObs,Get1ImageArray
;20131031  T.A.		;exclude AIO from PolIbs
;
;
;
;========================headder==============================;
;function 	timeinit
;function 	gettime
;function	PrevObs
;function	Get1ImageArray
;function 	NormalObs
;function 	PolObs,polstate
;function 	CalibObs
;pro 		MessageBox,kotoba
;Function	MotorPulse,wp
;
;-

;========================include==============================;
@Prosilica_Lib
@CDIO_Lib
@AIO_Lib
;=========================main================================;



;**************************************************************
FUNCTION timeinit
;--------------------------------------------------------------
common obslib,msec0

caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
second=string(seco,form='(i2.2)')	&	second0=second
while (second eq second0) do begin
	caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
	second=string(seco,form='(i2.2)')
endwhile

msec0=GeTime()
return,msec0
END
;**************************************************************
FUNCTION gettime
;--------------------------------------------------------------
common obslib,msec0

caldat,systime(/JULIAN), mon , day , year , hour , minu , seco
yyyymmdd_hhmmss=string(year,form='(i4.4)')+string(mon,form='(i2.2)')+string(day,form='(i2.2)') $
	+'_'+string(hour,form='(i2.2)')+string(minu,form='(i2.2)')+string(seco,form='(i2.2)')
msec=GeTime()

time0=msec-msec0
length=strlen(string(time0))
msec=strmid(string(time0),(length-3),3)

time=yyyymmdd_hhmmss+msec
return,time

END
;**************************************************************
FUNCTION PrevObs,wp,xy,mxmi=mxmi
;--------------------------------------------------------------

if not keyword_set(mxmi) then mxmi=[0,2.^12]
nimg=1
img0=img2iDL(nimg)
img=rebin(img0,800,600)
xy1=[xy[0]*2./wp.binx,xy[1]*2./wp.biny]
mxmi=[0,max(img0[*,xy1[1]])>max(img0[xy1[0],*])>mxmi[1]]


wset,1
chs=1.5
plot,indgen(wp.Width),img0[*,xy1[1]],charsize=chs,yr=mxmi,		$
	xtitle='X [pix]'+' binx='+string(wp.binx,format='(i1)'),	$
	ytitle='[DN]',title='Y = '+string(xy[1],format='(i4)')
oplot,!x.crange,[2.^12,2.^12],line=1
plot,indgen(wp.Height),img0[xy1[0],*],charsize=chs,yr=mxmi,		$
	xtitle='Y [pix]'+' biny='+string(wp.biny,format='(i1)'),	$
	ytitle='[DN]',title='X = '+string(xy[0],format='(i4)')
oplot,!x.crange,[2.^12,2.^12],line=1

wset,0
tvscl,img

return,img

END
;**************************************************************
FUNCTION Get1ImageArray
;--------------------------------------------------------------

nimg=1
img=img2iDL(nimg)
tvscl,img

return,img

END
;**************************************************************
FUNCTION NormalObs,wp
;--------------------------------------------------------------


pro_init
pro_setparam,wp
sttime=gettime()
getimg,wp.nimg
entime=gettime()
;savefits,'NORMAL','',sttime,entime,wp
savefits,'NOM','',sttime,entime,wp
savestmpfits,sttime,wp
pro_exit

filename=wp.svdir+wp.fname+sttime+'.fits'
return,filename

END
;**************************************************************
FUNCTION PolObs,polstate,wp
;--------------------------------------------------------------

pro_init
pro_setparam,wp
;p=AIO_start()						;2013.10.31, anan
sttime=gettime()
hardtrigger_getimg,wp.nimg
;AIO_stop						;2013.10.31, anan
entime=gettime()
savefits,'POL',polstate,sttime,entime,wp
savestmpfits,sttime,wp
pro_exit
;rtwv=AIO_read();	&plot,rtwv			;2013.10.31, anan
;save,file=wp.svdir+'wvplt'+sttime+'.sav',p,rtwv	;2013.10.31, anan

filename=wp.svdir+wp.fname+sttime+'.fits'
return,filename

END
;**************************************************************
FUNCTION CalibObs,wp,wd
;--------------------------------------------------------------

fn=strarr(9)
cdio_init
wp.input=cdio_input()
  widget_CONTROL,wd.in,set_value=wp.input
tmp=''
cdio_o45,wp,wd	&	purpose='45'
while (tmp eq '') do begin
	wp.input=cdio_input()
	  widget_CONTROL,wd.in,set_value=wp.input
	wp.output=cdio_outstate()
	  widget_CONTROL,wd.out,set_value=wp.output

	if (wp.input eq '45°') and (purpose eq '45') then begin
		print,'45'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[0]=PolObs('45',wp)
		cdio_o90,wp,wd	&	purpose='90'
print,'===== 45°終了 ====='
	endif
	if (wp.input eq '90°') and (purpose eq '90')  then begin
		print,'90'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[1]=PolObs('90',wp)
		cdio_o135,wp,wd	&	purpose='135'
print,'===== 90°終了 ====='
	endif
	if (wp.input eq  '135°') and (purpose eq '135')  then begin
		print,'135'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[2]=PolObs('135',wp)
		cdio_o180,wp,wd	&	purpose='180'
print,'===== 135°終了 ====='
	endif
	if (wp.input eq  '180°') and (purpose eq '180') then begin
		print,'180'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[3]=PolObs('180',wp)
		cdio_o225,wp,wd	&	purpose='225'
print,'===== 180°終了 ====='
	endif
;	if (wp.input eq  '225°') and (purpose eq '225') then begin
	if (wp.input eq  '135°') and (purpose eq '225') then begin
		print,'225'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[4]=PolObs('225',wp)
		cdio_o270,wp,wd	&	purpose='270'
		;wait,2
		while (wp.input eq  '135°') do wp.input=cdio_input()

print,'===== 225°終了 ====='
	endif
;	if (wp.input eq  '270°') and (purpose eq '270') then begin
	if (wp.input eq  '135°') and (purpose eq '270') then begin
		print,'270'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[5]=PolObs('270',wp)
		cdio_o315,wp,wd	&	purpose='315'
print,'===== 270°終了 ====='
	endif
	if (wp.input eq  '315°') and (purpose eq '315') then begin
		print,'315'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[6]=PolObs('315',wp)
		cdio_o360,wp,wd	&	purpose='360'
print,'===== 315°終了 ====='
	endif
	if (wp.input eq  '360°') and (purpose eq '360') then begin
		print,'0'	;&wait,5		;wait,5がないと入力信号の重なりが起きる＊ポイント！！
		fn[7]=PolObs('0',wp)
		cdio_op25,wp,wd	&	purpose='22.5'
print,'===== 0°終了 ====='
	endif
	if (wp.input eq  '+22.5°') and (purpose eq '22.5')  then begin
;MessageBox,'無偏光撮影OK ??'
		print,'non'
		fn[8]=PolObs('',wp)
		tmp='finish'
print,'===== 22.5°終了 ====='
	endif

	;print,wp.input
endwhile
cdio_exit

return,fn
END
;**************************************************************
PRO MessageBox,kotoba
;--------------------------------------------------------------
common obslib

cd,'C:\Projects\cprog\VS2005\prosilica'
prodll='C:\Projects\cprog\VS2005\prosilica\Debug\prosilica.dll'
r=call_external(prodll,'CamOpen', kotoba, /PORTABLE )

END
;**************************************************************
Function MotorPulse,wp			;imcomplete
;--------------------------------------------------------------
common obslib

bin0 = wp.binx > wp.biny
bin1 = wp.binx < wp.biny

case bin0 of				; threshfold of frame rate [frames/sec]
	1:thr0=25.
	2:thr0=25.
	4:thr0=60.
	8:thr0=75.
	else:thr0=25.
endcase
case bin1 of				; threshfold of frame rate [frames/sec]
	1:thr1=35.
	2:thr1=35.
	4:thr1=70.
	8:thr1=80.
	else:thr1=80.
endcase

framerate0 = (1.e6/wp.expo) < thr0	; [frames/sec] s
framerate1 = (1.e6/wp.expo) < thr1	; [frames/sec] l

period0 = 16.*(1./framerate0)		; [sec]
period1 = wp.nimg*(1./framerate1)	; [sec]
pulse = string(long([507904./period1,507904./period0]))		; [pulse/sec]

return,pulse

END
