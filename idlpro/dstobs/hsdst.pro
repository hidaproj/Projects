;+ 
; calculate grating angle and spectra on DST HS					 
;- 
;**************************************************************
function version
ver='0.1'	; '2015/04/30	t.a., k.i.   from Norikura g1g2.pro
ver='0.2'	; '2015/05/03	k.i.,t.a.    misc.
ver='0.21'	; '2016/09/25	k.i.         mmax=4
return,ver
end


;************************************************************************ 
function hsdst_str       ; return spectrograph constant for DST HS
;------------------------------------------------------------------------ 

g1={grating,	$
	grooves	:	0.,	$ ; grooves per mm
	ba	:	0.,	$ ; blaze angle [deg]
	offset  :	0.	$ ; offset angle [deg]
	}
g=replicate(g1,3)
g[0].grooves=1200.	&	g[0].ba=17.+27./60.
g[1].grooves=1200.	&	g[1].ba=26.+45./60.
g[2].grooves=600.	&	g[2].ba=48.+55./60.

wp={param, $
	grating_number:	1,		$ ; Grating number (1,2,3)
	g:		g,		$ ; grating param
	gx:		206.,		$ ; grating length, mm
	gy:		154.,		$ ; grating height, mm
;
	f_slit:		9638.,		$ ; focal length of slit plates (field lens) [mm]
	f_collimator:	9981.,		$ ; focal length of collimator mirror [mm]
	f_camera:	10002.3,	$ ; focal length of camera mirrors [mm]
;
	port_angle:	[-10.02,-6.003,-1.983,2.038,6.057,10.08], $ ; ga+port_angle=reflection angle [deg] 	
	port_length:	701,		$ ; port length [mm]
;
	port_number:	0,		$ ; port number
	port_x:		0.,		$ ; position in a port, 20 - 720 [mm]
	ga:		0.,		$ ; grating angle [deg]
	wl:		0d,		$ ; wavelength [A]
	order:		0		$ ; order
	}

return,wp

end

;************************************************************************
function wl2ga,wp       ; return Grating angle
;------------------------------------------------------------------------

alpha=wp.port_angle[wp.port_number-1]*!dtor + (wp.port_x - (720.-20.)/2.)/wp.f_camera
aa=wp.order*wp.wl/10.*wp.g[wp.grating_number-1].grooves/2./cos(alpha/2.)*1e-6
if aa le 1 and aa ge -1 then begin
	wp.ga = (asin(aa) - alpha/2. )*!radeg
endif else begin
	print,'cannot calculate asin'
	wp.ga=0.
endelse

return,wp

END

;************************************************************************
function ga2wr,wp       ; return wavelength range in each port
;------------------------------------------------------------------------

mmax=4
wr=fltarr(2,mmax,6)	; (wmin,amax),order,port
for iorder=1,mmax do begin
	for iport=0,5 do begin
		wr[0,iorder-1,iport]=10.*1e6/float(iorder)/float(wp.g[wp.grating_number-1].grooves)*	$
			(sin(wp.ga*!dtor) + sin(wp.ga*!dtor + wp.port_angle[iport]*!dtor  	$
			+ (20.-(720.-20.)/2.)/wp.f_camera))

		wr[1,iorder-1,iport]=10.*1e6/float(iorder)/float(wp.g[wp.grating_number-1].grooves)*	$
			(sin(wp.ga*!dtor) + sin(wp.ga*!dtor + wp.port_angle[iport]*!dtor  	$
			+ (720.-(720.-20.)/2.)/wp.f_camera))
	endfor
endfor

return,wr


END

;************************************************************************
function wl2pos,wp,wl       ; return port number and port position in specified wavelength
;------------------------------------------------------------------------

res=replicate(wp,3)
for iorder=1,3 do begin
	alpha=asin( iorder*wl/10.*float(wp.g[wp.grating_number-1].grooves)*1e-6 - sin(wp.ga*!dtor) )*!radeg - wp.ga
	;port_number=where( 	(alpha ge (wp.port_angle + (-720.+(720.-20.)/2.)/wp.f_camera*!radeg)) and $ 
	;		(alpha le (wp.port_angle + (-20.+(720.-20.)/2.)/wp.f_camera*!radeg)),npos)
	port_number=where( 	(alpha ge (wp.port_angle + (20.-(720.-20.)/2.)/wp.f_camera*!radeg)) and $ 
			(alpha le (wp.port_angle + (720.-(720.-20.)/2.)/wp.f_camera*!radeg)),npos)
	if npos ne 0 then begin
		res[iorder-1].port_number=port_number+1
		x=wp.f_camera*(alpha-wp.port_angle[port_number])*!dtor
		res[iorder-1].port_x=(720.-20.)/2. + x
	endif else begin
		res[iorder-1].port_number=0
		res[iorder-1].port_x=0
	endelse
endfor

return,res

END

;************************************************************************ 
pro hsdisp,p,hswd,wl2=wl2
;------------------------------------------------------------------------ 
;  plot overlapping spectra 
 
	loadct,0
	stretch,255,0				;T.A. for Mac			
	n_groove=p.g[p.grating_number-1].grooves
	blaze=p.g[p.grating_number-1].ba
	offset=p.g[p.grating_number-1].offset
	form1="('wl=',f7.1,',  port=',i1,',  m=',i1,',  Grating=',i1,',  offset=',f6.3)"
	print,p.wl,p.port_number,p.order,p.grating_number,offset

	sp1=atlas(3500.,16000.,wl=wla)
        d=1./n_groove         ; space mm 
        dd = d*cos(blaze*!pi/180.) 

	x0=60 &	dx=30
	y0=30 &	ytop=70
	yh=(hswd.Wy-ytop)/2
	mmax=4	; max order
	dy=(yh-2*y0)/mmax
	
	splen = (hswd.Wx-x0-dx*3)/3.
	sphgt= 20

	;-----  mark position of 2nd line, wl2 -----
	if keyword_set(wl2) then begin
		p2=wl2pos(p,wl2)
		print,'wl=',wl2,' A'
		count=0
		for m=0,n_elements(p2)-1 do begin
			pp1=p2[m]
			if pp1.port_number ne 0 then begin
				k=pp1.port_number-1
				str='port='+string(pp1.port_number,form='(i1)')+ $
					',  x='+string(pp1.port_x,form='(i3)')+ $
					',  m='+string(m+1,form='(i1)')
				print,str
				x1=x0+(2-k/2)*(splen+dx)
				y1=y0+(1-(k mod 2))*yh
				circle,x1+splen*(pp1.port_x-20)/float(p.port_length),y1+dy*m+sphgt/2,sphgt,line=1
				widget_control, hswd.Wl2pos[0], set_value=str
			  	str2='wl='+string(wl2,form='(f7.1)')+'A,  port=' $
					+string(pp1.port_number,form='(i1)') $
					+', x='+string(pp1.port_x,form='(i3)')+',  m='+string(m+1,form='(i1)')
				openw,1,hswd.pfile,/append
				printf,1,str2
				close,1
				count=count+1
			endif
		endfor
		if count eq 0 then begin
			print,wl2,'A not available!'
			widget_control, hswd.Wl2pos[0], set_value='Not available!'
		endif
		return
	endif


	;----- display spectra on port 1-6  ------
	p=wl2ga(p)
	wr=ga2wr(p)

	erase
	for k=0,5 do begin	; port #
		x1=x0+(2-k/2)*(splen+dx)
		y1=y0+(1-(k mod 2))*yh
		xyouts,x1+50,y1+yh-y0-40,'--------  port-'+string(k+1,form='(i1)')+' --------',/dev,chars=1.2
		for m=0,mmax-1 do begin
			wlmin=wr[0,m,k]
			wlmax=wr[1,m,k]
			yc=y1-17
			if wlmin ge min(wla) and wlmax lt max(wla) then begin
				;sp=reverse(sp1(where(wla ge wlmin and wla lt wlmax)))
				sp=sp1(where(wla ge wlmin and wla lt wlmax))
				spd=congrid(reform(sp,n_elements(sp),1),splen,sphgt)
				tv,spd,x1,y1
				xyouts,x1-10,yc,string(wlmin,form='(i5)'),/dev
				xyouts,x1+splen-40,yc,string(wlmax,form='(i5)'),/dev
				wlb=fix(wlmax)/100*100.
				while (wlb gt wlmin and wlb lt wlmax) do begin
					;xb=x1+splen*(wlmax-wlb)/(wlmax-wlmin)
					xb=x1+splen*(wlb-wlmin)/(wlmax-wlmin)
					if xb gt x1+50 and xb lt x1+splen-50 then begin
						draw,xb*[1,1],[y1,y1-5]
						xyouts,xb-20,yc,string(wlb,form='(i5)'),/dev
					endif
					wlb=wlb-100
				endwhile
			endif
			if k+1 eq p.port_number and m+1 eq p.order then begin
				circle,x1+splen*(p.port_x-20)/float(p.port_length),y1+sphgt/2,sphgt
			endif
			if k eq 4 or k eq 5 then begin
				xyouts,10,y1,'m='+string(m+1,form='(i1)'),/dev,chars=1.2
			endif
			y1=y1+dy
		endfor
	endfor

	chars=1.8
        title1='DST-HS: '+string(p.wl,format='(f7.1)')+'A on port-'+string(p.port_number,form='(i1)') $ 
		+',  x='+string(p.port_x,form='(i3)')+'mm' $
                +',  Grating-'+string(p.grating_number,form='(i1)')+', m='+string(p.order,format='(i1)') 
;        title2='G.A.='+string(p.ga,format='(f6.2)')+' deg. ' $ 
	cga=string(fix(p.ga),format='(i2)')+'!uo!n'+string((p.ga mod 1)*60,form='(f4.1)')+"'"
        title2='G.A.='+cga+' (offset='+string(offset,format='(f6.3)')+')';,' $ 
;                +'  Disp.='+string(disp,format='(f6.3)')+' A/mm ' 
        xyouts,x0,hswd.Wy-30,title1,charsize=chars,/dev 
        xyouts,x0,hswd.Wy-60,title2,charsize=chars,/dev 

	openw,1,hswd.pfile
	printf,1,'Grating-'+string(p.grating_number,form='(i1)')+',  GA='+string(p.ga,form='(f5.2)')+'deg.'
	printf,1,'wl='+string(p.wl,form='(f7.1)')+'A,  port='+string(p.port_number,form='(i1)') $
		+', x='+string(p.port_x,form='(i3)')+',  m='+string(p.order,form='(i1)')
	close,1

end 
 
;************************************************************************ 
pro hs_wlview,p,hswd
;------------------------------------------------------------------------ 
;  plot wavelength coveradge

	wlr=[3000.,16000.]
	mmax=4
	yr=[0,mmax*6+3]

	p=wl2ga(p)
	wr=ga2wr(p)

	erase
	plot,wlr,yr,xtitle='wavelength [A]',yrange=yr,ystyle=1+4,xstyle=1+8,/nodata,chars=1.5
	for m=0,mmax-1 do begin
		y0=1+m*6
		xyouts,2500.,y0+3,'m='+string(m+1,form='(i1)'),/data,chars=1.5
		for k=0,5 do begin
			y1=y0+k
			wr1=[wr[0,m,k],wr[1,m,k]]
			oplot,wr1,y1*[1,1],thick=2
		endfor
	endfor

	x0=60
	chars=1.8
	offset=p.g[p.grating_number-1].offset
        title1='DST-HS: '+string(p.wl,format='(f7.1)')+'A on port-'+string(p.port_number,form='(i1)') $ 
		+',  x='+string(p.port_x,form='(i3)')+'mm' $
                +',  Grating-'+string(p.grating_number,form='(i1)')+', m='+string(p.order,format='(i1)') 
        title2='G.A.='+string(p.ga,format='(f6.2)')+' deg. ' $ 
                +' (offset='+string(offset,format='(f6.3)')+')';,' $ 
;                +'  Disp.='+string(disp,format='(f6.3)')+' A/mm ' 
        xyouts,x0,hswd.Wy-30,title1,charsize=chars,/dev 
        xyouts,x0,hswd.Wy-60,title2,charsize=chars,/dev 

end

;************************************************************************ 
function line_pdm,base,lines=mstr
;------------------------------------------------------------------------ 
 
l_cor=['-- Corona --',  $ 
        ' FeXIV  5302.9',       $ 
        ' CaXV   5694.5',               $ 
        ' FeX    6374.5',               $ 
        ' FeXI   7891.9',               $ 
        ' FeXIII 10746.8',      $ 
        ' FeXIII 10797.9'	$
	] 
l_mag=['-- Magnetic --',        $ 
        ' FeI    6337.',        $ 
        ' FeI    6302.5',       $ 
        ' FeI    5250.2'       $ 
        ] 
l_chr=['-- Chromosphere --',    $ 
        ' HeI    10830.',  	$ 
        ' CaII   8662. ',	$ 
        ' CaII   8542.',	$ 
        ' CaII   8498.',  	$ 
        ' HI     6562.8  (Ha)', $ 
        ' HeI    5876.   (D3)', $ 
        ' HI     4861.3  (Hb)', $ 
        ' HI     4340.5  (Hg)', $ 
        ' HI     4101.5  (Hd)', $ 
        ' CaII   3968.5  (H)',  $ 
        ' CaII   3933.7  (K)'	$
	] 
         
mstr=['Line Table',l_cor,l_chr,l_mag,'-- cancel']
ns=n_elements(mstr)

flags=intarr(ns)
flags(0)=1
flags(1)=1 
flags(n_elements(l_cor))=2
flags(1+n_elements(l_cor))=1
flags(n_elements([l_cor,l_chr]))=2
flags(1+n_elements([l_cor,l_chr]))=1
flags(n_elements([l_cor,l_chr,l_mag]))=2
flags(ns-1)=2
desc=replicate({pdst, flags:0, name:''},ns)
desc.name=mstr
desc.flags=flags
wdid = cw_pdmenu(base,desc,return_name=0,uvalue='Cw_pdmenu')

return,wdid
end 
 
;************************************************************************ 
; hsdst_event 
pro hsdst_event, ev 
 
common hsparm, p, wl2
common widget, hswd, lines
 
  widget_control, ev.id, get_uvalue=value 
  if (n_elements(value) eq 0) then value = '' 
  name = strmid(tag_names(ev, /structure_name), 7, 1000) 
  skip_plot=0 
  comdll='c:\nkrprj\cprog\\oscom32.dll'

form1="('wl=',f7.1,',  port=',i1,',  m=',i1,',  Grating=',i1,',  offset=',f6.3)"

case ev.id of 
	hswd.port: begin
	    	p.port_number=ev.value +1
	    	print,'port= ',string(p.port_number,form='(i1)'), ' selected'
	    	end
	hswd.port_x: begin
	    	widget_control, ev.id, get_value=value
	    	p.port_x=float(value)
	    	print,'port_x= ',string(p.port_x,form='(i3)'), ' selected'
	    	end
	hswd.grating: begin
	    	p.grating_number=ev.value+1
	    	print,'grating= ',string(p.grating_number,form='(i1)'), ' selected'
	    	end
	hswd.order: begin
	    	p.order=ev.value+1
	    	print,'order= ',string(p.order,form='(i1)'),' selected'
	    	end
	hswd.line_pdm: begin
		line=lines(ev.value)
		print,line
		if strmid(line,0,1) ne '-' then begin
		        p.wl=float(strmid(lines(ev.value),8,6)) 
		    	widget_control, hswd.Wl, set_value=string(p.wl,form='(f7.1)')
		    	widget_control, hswd.Wlslider, set_value=fix(p.wl)
		endif
	    	end
	hswd.Wlslider: begin
		p.wl=float(ev.value) 
	    	widget_control, hswd.Wl, set_value=string(p.wl,form='(f7.1)')
	    	end
	hswd.Wl: begin
	    	widget_control, ev.id, get_value=value
	    	p.wl=float(value)
	    	widget_control, hswd.Wlslider, set_value=fix(p.wl)
	    	end
	hswd.Wl2: begin
	    	widget_control, ev.id, get_value=value
	    	wl2=float(value) &	wl2=wl2[0]
		if wl2 gt 3000. then hsdisp,p,hswd,wl2=wl2
	    	end
	hswd.line_pdm2: begin
		line=lines(ev.value)
		print,line
		if strmid(line,0,1) ne '-' then begin
		        wl2=float(strmid(lines(ev.value),8,6)) 
		    	widget_control, hswd.Wl2, set_value=string(wl2,form='(f7.1)')
			if wl2 gt 3000. then hsdisp,p,hswd,wl2=wl2
		endif
	    	end
	else: begin
	  case (strmid(name,0,4)) of 
	  "BUTT": begin 
	        case value of 
	        "DONE":  begin 
	          WIDGET_CONTROL, /destroy, ev.top 
	          return 
	          end 
	        "GifSave":  begin 
	          win2gif,'c:\tmp\hs.gif'
		  print,'window saved to  c:\tmp\hs.gif'
	          return 
	          end 
	        "WLGAPLOT": wlgaplot,p 
	        "PPrint": begin 
		  print,'open param file'
		  retn = call_external(comdll,'excom','notepad '+hswd.pfile, value = [0b]) 
	          end 
	        "SHOW": hsdisp,p,hswd
	        "OFFSET": begin 
	          setoff 
	          print,'Offset is set to ',p.offset 
	          end 
	        "WView": hs_wlview,p,hswd
	        else : begin 
	          if (ev.select eq 0) then begin 
	            value = value + ' (released)' 
	          endif 
	          end 
	        endcase 
		end
	  "DRAW": begin 
	        value = string(format="(A,' X(', I0, ')', ' Y(', I0, ')', ' Press('," $ 
	                + "I0, ')', ' Release(', I0, ')')", value, ev.x, ev.y, $ 
	                ev.press, ev.release) 
	        ;print,ev.x,ev.y 
	        end 
	  "LIST": value=value(ev.index) 
	  endcase 
  	end
endcase

end 
 
;************************************************************************ 
; hsdst	

common hsparm, p, wl2
common widget, hswd, lines
logdir='c:\projects\log\' 
logfile=logdir+'hsdst.log' 
pfile=logdir+'hsp.txt'

p=hsdst_str() 

p.grating_number=1	; grating #
p.order=1		; order
p.port_number=1		; port#
p.port_x=370		; position, mm
p.wl=5303.		; wavelength, A
wl2=0.			; wavelength of 2nd line

ff=findfile(logfile)
if ff(0) ne '' then restore,file=logfile 

hswd={hs_widget,	$
	Pfile:		pfile,	$
	Port:		0l,	$
	Port_x:		0l,	$
	Grating:	0l,	$
	Wl:		0l,	$	; wavelength 
	Wlslider:	0l,	$	; wavelength slider
	Line_pdm:	0l,	$
	Line_pdm2:	0l,	$
	Order:		0l,	$
	Wl2:		0l,	$
	Wl2pos:		replicate(0l,3),	$
	;Wx:		1400, 	$	; x-size of drawing window
	Wx:		1250, 	$	; x-size of drawing window, T.A. for Mac, 2015.5.2
	Wy:		500	$	; y-size of      "
	}
if p.wl lt 3500. then p.wl=3500. 

gn=p.grating_number
print,p.wl,p.port_number,p.order,gn,p.g[gn-1].offset,        $ 
        format="('Log=> wl=',f7.1,',  port=',i1,',  m=',i1,',  Grating=',i1,',  offset=',f6.3)" 

;-------------  widget  -------------------------------------------------- 
base = WIDGET_BASE(title='DST HS: ver. '+version(), /column)         ; base window 
b0 = WIDGET_BASE(base, /frame, /row) 
draw = widget_draw(b0, xsize=hswd.Wx, ysize=hswd.Wy, /button_events) 
;b1 = WIDGET_BASE(base, /frame, /row) 
;t0 = widget_button(b1, value="Draw wl vs. G-angle", uvalue = "WLGAPLOT") 
;t01 = widget_button(b1, value="Printout", uvalue = "PPrint") 
 
b2 = WIDGET_BASE(base, /frame, /row, space=5) 
b21= WIDGET_BASE(b2, /column) 
b211= WIDGET_BASE(b21, /row) 
	ports=['1','2','3','4','5','6'] 
	;xmenu,sel,b211,/exclusive,/row,/frame,uvalue=sel,buttons=buttons 
	val=fix(p.port_number)-1
	c1 = widget_label(b211, value='port :') 
	hswd.port = cw_bgroup(b211,ports,/row, uvalue=sel, $
		/no_release,set_value=val,/exclusive, /frame)
	c1 = widget_label(b211, value='  pos.:') 
	hswd.port_x = widget_text(b211, /editable, xsize=10, ysize=1, uvalue='POS', $
		value=string(p.port_x,form='(i3)') )
b212= WIDGET_BASE(b21, /row) 
	c2 = widget_label(b212, value='Gr.No :') 
	Grs = ['1','2','3']
	i=where(p.grating_number eq fix(grs))
	hswd.grating = cw_bgroup(b212,grs,/row, uvalue=sel, $
		/no_release,set_value=i[0],/exclusive, /frame)
	order=['1','2','3','4'] 
	c2 = widget_label(b212, value='   order :') 
	i=where(p.order eq fix(order))
	hswd.Order = cw_bgroup(b212,order,/row, uvalue=sel, $
		/no_release,set_value=i[0],/exclusive, /frame)
b22= WIDGET_BASE(b2, /frame, /column) 
b221= WIDGET_BASE(b22, /frame, /row) 
	t1 = widget_label(b221, value='Wavelength (A) :') 
	hswd.Wl = widget_text(b221, /editable, xsize=10, ysize=1, $
		value=string(p.wl,form='(f7.1)'), uvalue='WL') 
	;t12 = widget_button(b221, value="Table", uvalue = "LINETBL") 
	;xpdmenu,['/Table/ {', '/-- Corona --/','/-- Magnetic --/'],b221 
	hswd.Line_pdm = line_pdm(b221,lines=lines)
	hswd.Wlslider = widget_slider(b22, minimum=3500.,maximum=16000., $ 
	                xsize=150,value=p.wl,scroll=20/20.) 
b24= WIDGET_BASE(b2, /column, /frame) 
b241= WIDGET_BASE(b24, /row) 
	t21 = widget_label(b241, value='2nd line: ') 
	hswd.Wl2 = widget_text(b241, /editable, xsize=10, ysize=1, uvalue='WL2', $
		value=string(wl2)) 
	t21 = widget_label(b241, value=' A') 
	hswd.Line_pdm2 = line_pdm(b241,lines=lines)
	for i=0,0 do begin
		b242= WIDGET_BASE(b24, /row) 
		t21 = widget_label(b242, value='pos: ');-'+string(i+1,form='(i1)')+': ') 
		hswd.wl2pos[i]= widget_text(b242, xsize=25, ysize=0, value='') 
	endfor
b23= WIDGET_BASE(b2, /column) 
	t2 = widget_button(b23, value="Show Spectra", uvalue = "SHOW") 
	t5 = widget_button(b23, value="WView", uvalue = "WView") 
	t3 = widget_button(b23, value="Offset Setting", uvalue = "OFFSET") 

b25= WIDGET_BASE(b2, /column) 
	t6 = widget_button(b25, value="Gif", uvalue = "GifSave") 
	t7 = widget_button(b25, value="Print", uvalue = "PPrint") 
	t4 = widget_button(b2, value="Done", uvalue = "DONE") 
 
 
widget_control, base, /realize 
 
  WIDGET_CONTROL, get_value=window, draw 
;  WIDGET_CONTROL, buttons(4), set_button=1      ; unselect menu button '0' 
;  WIDGET_CONTROL, buttons(0), set_button=0      ; select menu button '20' 
  old_window = !d.window        ;Previous window 
  wset, window 
 
XMANAGER, 'hsdst', base ;, /MODAL 
 
save,p,filename=logfile 
 
end 
