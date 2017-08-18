pro cam_set5

jul =systime(/JULIAN)
CALDAT, jul, Month1, Day1, Year1
	Year1=strtrim(Year1,2)
	Month1=strtrim(Month1,2)
	Day1=strtrim(Day1,2)
		if strlen(Month1) eq 1 then Month1='0'+Month1
		if strlen(Day1) eq 1 then Day1='0'+Day1
date=Year1+Month1+Day1
spawn,'mkdir C:\saveimg\'+date+'\'
sdir='C:\saveimg\'+date+'\'

 ;---------- dllfile ----------;
  dir='C:\Documents and Settings\corona\My Documents\Visual Studio 2005\Projects\cam_set5\debug\'
  dllfile=dir+'cam_set5.dll'
 ;---------- initcamera ----------;
  m_hCam=call_external(dllfile,'createcam',/all_value,/cdecl)

 ;---------- make a window ----------;
  wh=call_external(dllfile,'create_ww',/all_value,/cdecl)

 ;---------- camera cooling ----------;
 stfan=call_external(dllfile,'startfan',m_hCam,/all_value,/cdecl)
 temp=300

;temp2=dialog_input(prompt='temp:')

; stfan=call_external(dllfile,'settemp',m_hCam,temp2,/all_value,/cdecl)
temp2=255
 while (temp gt temp2) do begin
	temp1=temp
	temp=call_external(dllfile,'checktemp',m_hCam,/all_value,/cdecl)
	if temp1 ne temp then print,temp
wait,0.1
 endwhile
okk:
ok=dialog_menu(['OK'],title='Camera temperatur: '+strtrim(temp2,2)+'K')
	ppw=call_external(dllfile,'setpower',m_hCam,/all_value,/cdecl)
stop
 ;---------- input param ----------;
aga:
  param=dialog_input(prompt=['Seq.                      ',$
                           'Exp. (msec)            ',$
                           'Gain. (Most or Least)'],$
		     Nfields=3,title='input obs. param')
if param(0) eq '' then begin
	print,'Please input parms'
	goto,fin
 endif
 ;---------- set param ----------;

	exp=long(param(1));(ms)
	case strtrim(param(2),2) of
	'Most': gain=3;(0:least 3:most)
	'MOST': gain=3;(0:least 3:most)
	'most': gain=3
	'm'   : gain=3
	'M'   : gain=3
	'High': gain=3
	'high': gain=3
	'H'   : gain=3
	'h'   : gain=3
	'3'   : gain=3

	'Least': gain=0;(0:least 3:most)
	'LEAST': gain=0;(0:least 3:most)
	'least': gain=0
	'l'    : gain=0
	'L'    : gain=0
	'Low'  : gain=0
	'low'  : gain=0
	'0'    : gain=0
	endcase

	case strtrim(gain,2) of
		'3':sgain='Most'
		'0':sgain='Least'
	endcase

 ;---------- start cap ----------;
  m_hCam=call_external(dllfile,'startcap',m_hCam,exp,gain,/all_value,/cdecl)

 ;---------- nominal start obs ----------;
		f_exist=findfile(sdir+'pol*.dat',count=nnn)
		if nnn ne 0 then begin
		c_exist=str_sep(f_exist(nnn-1),'_')
			exist=long(c_exist(1))+1
		endif else begin
			exist=0
		endelse

	n=param(0)*6

	  	case strtrim(strlen(strtrim(exist,2)),2) of
		 '1':exx='00'+strtrim(exist,2)
		 '2':exx='0'+strtrim(exist,2)
		 '3':exx=strtrim(exist,2)
		endcase


  openw, 1, sdir+'pol_log_'+exx+'.txt'

	for i=0,n-1 do begin
		seq=long(i)
		disp=call_external(dllfile,'disp_img',m_hCam,wh,/all_value,/cdecl)
		disp=call_external(dllfile,'save_img',m_hCam,seq,exist,long(date),/all_value,/cdecl)

		temp=call_external(dllfile,'checktemp',m_hCam,/all_value,/cdecl)

		seq2=strtrim(seq/6,2)
		case strtrim(strlen(seq2),2) of
		  '1':ssq='0000'+seq2
		  '2':ssq='000'+seq2
		  '3':ssq='00'+seq2
		  '4':ssq='0'+seq2
		  '5':ssq=seq2
		endcase

		pol2=strtrim(seq mod 6,2)
		case pol2 of
		  '0':polm='ipq'
		  '1':polm='imq'
		  '2':polm='ipu'
		  '3':polm='imu'
		  '4':polm='ipv'
		  '5':polm='imv'
		endcase

		ofile='pol_'+exx+'_'+ssq+'_'+polm+'.dat'

		otim=str_sep(systime(),' ')
		otime=otim(4)+'-'+otim(1)+'-'+otim(2)+' '+otim(3)

		log='  '+ofile+'  '+otime+'  '+$
			   strtrim(ssq,2)+'  '+strtrim(exp,2)+'  '+$
			   strtrim(sgain,2)+'  '+strtrim(temp,2)
		print,log
		printf, 1, log
		wait,0.1
	endfor
  close,1

 obs_again=dialog_menu(['Next Obs','Finish Obs'],title='Continue Observation?')
 if obs_again eq 'Next Obs' then goto,aga

fin:
 ;---------- stop obs ----------;
	r3=call_external(dllfile,'stopfan',m_hCam,/all_value,/cdecl)
	m_hCam=call_external(dllfile,'stopcap',m_hCam,/all_value,/cdecl)
	wh=call_external(dllfile,'destroy_ww',wh,/all_value,/cdecl)

end
