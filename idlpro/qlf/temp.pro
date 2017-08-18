
restore,'corrected.sav'
restore,'dark.sav'
restore,'result.sav'

nwl=n_elements(wl)
dwl=5
mwl=dwl*round(minmax(wl)/dwl)
nwl2=(mwl[1]-mwl[0])/dwl+1
wl2=mwl[0]+dwl*findgen(nwl2)

ret0=aa2s[0,0]+aa2s[1,0]/wl2	;reference ret

dpb=360/9000.                   ;[degree/bit]

wl0=600.                        ;[nm]
ip0=round(interpol(indgen(nwl2),wl2,wl0))

rep=4

wt1=0.1                          ;wait [sec]

nb=5
integ=10

set_plot,'z'

wdef,0,800,600
!p.color=0
!p.background=255

!p.multi=[0,1,2]
yra=[0,3000]
for j=0,nwl2-1 do begin
   print,j
   plot,wl,spec[*,j]-dark,title='orignal setting @'+ $
        string(wl2[j],form='(i4)')+' nm',xtit='nm',yra=yra>1, $
        xs=3,ys=3,psy=3,/ylog
   plots,wl2[j]*[1,1],10.^!y.crange,lines=2
   ssw_legend,'bit '+strtrim(indgen(nb)+1,2)+' = '+string(bits[j,*],form='(i6)'),/clear

   plot,wl,spec[*,j]-dark,xtit='nm',yra=yra, $
        xs=3,ys=3,psy=3
   plots,wl2[j]*[1,1],!y.crange,lines=2
   write_gif,'corrected.gif',tvrd(),/multi

endfor
!p.multi=0

write_gif,'corrected.gif',/close

set_plot,'x'

end
