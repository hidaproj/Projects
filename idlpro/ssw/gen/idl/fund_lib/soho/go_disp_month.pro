;
;
ssw_path, /eit
eit = ['171', '195', '284', '304']
;
tarr = anytim2ex(anytim2ints( ut_time(), off=-86400*4))
;fid1 = '19' + strmid( ex2fid(tarr), 0, 4)
fid1 = strmid(time2file( tarr, /date), 0, 6)
do_disp_month, fid1
do_disp_month, fid1, /hr
do_disp_month, fid1, /sxt
for i=0,3 do do_disp_month, fid1, eit=eit(i)
;
tarr = anytim2ex(anytim2ints( ut_time() ))
;fid2 = '19' + strmid( ex2fid(tarr), 0, 4)
fid2 = strmid(time2file( tarr, /date), 0, 6)
if (fid1 ne fid2) then begin
    do_disp_month, fid2
    do_disp_month, fid2, /hr
    do_disp_month, fid2, /sxt
    for i=0,3 do do_disp_month, fid2, eit=eit(i)
end
;
out = mk_imgsum_html(outfil='/www/IMAGES/index.html', /header, /titles)
;
end