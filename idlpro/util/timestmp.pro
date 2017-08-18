;+
;  timestmp.pro
;	get timestamp of file with time_t type (long)
;	98/01/24	k.i.
;-
;**********************************************************************
function timestmp,file,atime=atime,ctime=ctime,lvalue=lvalue
;file='c:\nkrprj\idlpro\nogis.pro'

dllfile='c:\nkrprj\cprog\oscomdll.dll'


ctimel=0l &	atimel=0l
ctimes='                             '
atimes='                             '
dmy=call_external(dllfile, $
		'timestmp',file,ctimes,atimes,value=[0d,0d,0d], /S_VALUE)

if keyword_set(ctime) then tstr=ctimes else tstr=atimes

if not keyword_set(lvalue) then return,tstr
yr=fix(strmid(tstr,20,4))
mon=strmid(tstr,4,3)
dd=fix(strmid(tstr,8,2))
hh=fix(strmid(tstr,11,2))
mm=fix(strmid(tstr,14,2))
ss=fix(strmid(tstr,17,2))
case mon of 
	  'Jan': m=1
	  'Feb': m=2
	  'Mar': m=3
	  'Apr': m=4
	  'May': m=5
	  'Jun': m=6
	  'Jul': m=7
	  'Aug': m=8
	  'Sep': m=9
	  'Oct': m=10
	  'Nov': m=11
	  'Dec': m=12
endcase

days=julday(m,dd,yr)-julday(1,1,1990)
lvalue=days*3600l*24l+hh*3600l+mm*60l+ss
return,lvalue

end

