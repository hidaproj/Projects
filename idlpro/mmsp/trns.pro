;  plot transmission
;	2010.7.29	ki


dir='C:\data\HR2000\'
clearf='clear.sav'
dataf='pol.sav'
darkf='dark.sav'

restore,dir+clearf &	clr=sp1
restore,dir+dataf &	dat=sp1
restore,dir+darkf &	drk=sp1
tr=(dat-drk)/(clr-drk)

plot,wl,tr>0<1


end
