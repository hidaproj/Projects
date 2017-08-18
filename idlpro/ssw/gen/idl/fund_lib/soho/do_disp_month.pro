pro do_disp_month, months, inpre=inpre, outpre=outpre, hr=hr, year=year, $
	indir=indir, outdir=outdir, sxt=sxt, eit=eit
		
;+
;NAME:
;	do_disp_month
;PURPOSE:
;	A driver program to make the monthly MDI magnetogram GIF file
;SAMPLE CALLING SEQUENCE:
;	do_disp_month, '199611'
;	do_disp_month, inpre='smdi_maglc_re_', outpre='hr_mag_'
;	do_disp_month, year='1997'
;	for i=1991,1997 do do_disp_month, year=strtrim(i,2), /sxt
;	do_disp_month, eit='171'
;HISTORY:
;	Written 17-Nov-96 by M.Morrison
;	19-May-97 (MDM) - Expanded to allow different prefix on the
;			  input and output
;			- Added /hr option
;	20-May-97 (MDM) - Added MAPFILE option
;	22-May-97 (MDM) - Made indir/outdir keywords to allow SXT
;			  to use the driver
;			- Added /sxt keyword
;	30-May-97 (MDM) - Renamed from "go_disp_month" to "do_disp_month"
;			- Added EIT
;	15-Oct-97 (MDM) - Changed to use sub_dirs for MDI
;	12-May-99 (MDM) - Replaced /hosts/pore1/usr/local/etc/httpd/htdocs
;			  with /www
;	 3-Aug-99 (MDM) - Replaced "space.lockheed.com" with "lmsal.com"
;-
;
smin = -50
smax = 50
;if (n_elements(indir) eq 0) then indir = '/hosts/diapason/data14/mdi_summary/daily/maglc'
if (n_elements(indir) eq 0) then indir = file_list('/hosts/diapason/data14/mdi_summary/daily/maglc', '*')
if (n_elements(outdir) eq 0) then outdir = '/www/IMAGES/months_mdi
if (keyword_set(hr)) then begin
    inpre = 'smdi_maglc_re_'
    outpre = 'hr_mag_'
end
if (keyword_set(sxt)) then begin
    indir = file_list('/hosts/sxt1/sxt1data1/sswdb/yohkoh/sxt/fits/', '*')
    outdir = '/www/IMAGES/months_sxt
    inpre = 'sf_fits'
    outpre = 'fd_sxt_'
    smin = 0
    smax = 225
end
if (keyword_set(eit)) then begin
    indir = file_list('/hosts/sxt1/sxt1data1/sswdb/soho/eit/daily_full', '*')
    outdir = '/www/IMAGES/months_eit
    inpre = 'efr_' + eit + '_'
    outpre = 'fd_eit' + eit + '_'
    smin = 0
    smax = 255
    img_scale = 1
end
;
if (n_elements(inpre) eq 0) then inpre = 'smdi_maglc_fd_'
if (n_elements(outpre) eq 0) then outpre = 'fd_mag_'
;
if (n_elements(year) eq 0) then year = '1997'
if (n_elements(months) eq 0) then months = year + string(indgen(12)+1, format='(i2.2)') 
n = n_elements(months)
months2 = strmid(months, 2, 4)
;
gif0 = outpre + months + '.gif'
map0 = outpre + months + '.map'
gif = concat_dir(outdir, gif0)
map = concat_dir(outdir, map0)
html= concat_dir(outdir, outpre + months + '.html')
set_plot, 'z'
for i=0,n-1 do begin
    print, 'Looking for: ' + inpre + months(i)+ '*' + '  in dir(s): ', indir
    infil = file_list(indir, inpre + months(i)+ '*')
    if (infil(0) eq '') then infil = file_list(indir, inpre + months2(i)+ '*')	;no "19" prepended
    if (keyword_set(eit)) then eit_colors, fix(eit)
    fits_disp_month, infil, smin, smax, gif=gif(i), char=0.8, img_scale=img_scale, $
		mappre='rect http://www.lmsal.com/cgi-bin/disp_fits.pl?', mapfil=mapfil
    if (n_elements(mapfil) gt 1) then begin
	prstr, mapfil, file=map(i)
	out = ['<html><head><title>' + outpre + months(i) + '</title></head><body>', $
		'Click on the image to see a full 512x512 sized image using xv and the FITS files <br>', $
		'<a href="' + map0(i) + '"><img src="' + gif0(i) + '" ismap></a>', $
		'</body></html>']
	prstr, out, file=html(i)
    end
end
;
end