;+
; DEC2HEX
;
; INPUTS:
;  decimal : 10�i���̓���
;  keta    : �o�͕����̌��i�������j
;
; HISTORY:
;  2011.07.06, NAGATA
;  2011.07.11, bug fix by T.T.ISHII & S.MORITA
;-
function dec2hex,decimal,keta
;
  tmp = string(decimal,format='(Z)')
  tmp = strlen(strtrim(tmp,1))
  if tmp gt keta then stop,'dec2hex: input exseeds the limit'
;
  if decimal ge 0 then begin
     ret = string(decimal,format='(Z08)')
  endif else begin
     tmp = string(long64(decimal),format='(Z16)')
     ret = strmid(tmp,8,8)
  endelse

  case keta of
     6: begin
        ret = strmid(ret,(8-keta),keta)
     end
     8: begin
        ret=ret
     end
     else: begin
        print,'dec2hex: keta should be 8 or 6'
        ret = 0
     end
  endcase

return,ret

end

;+
; INPUTS:
;     p1 : RA�w��1
;     p2 : RA�w��2
;     p3 : RA�ڕW�ʒu
;     p4 : DEC�w��1
;     p5 : DEC�w��2
;     p6 : DEC�ڕW�ʒu
;     p7 : T1�W�J��
;     p8 : T1�t�H�[�J�X�w��1
;     p9 : T1�t�H�[�J�X�w��2
;     p10: T1�t�H�[�J�X�ڕW�ʒu
;     p11: T1�t���b�g/�N���A
;     p12: T1ND�t�B���^�w��1
;     p13: T1ND�t�B���^�w��2
;
; 2011.07.05 NAGATA
; 2011.07.11, bug fix by T.T.ISHII & S.MORITA
;-
;print,prep_t1buffer(1,1,1000,1,1,-1000,1,1,1,-2000,1,1,1)
function prep_t1buffer,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13
;
tmp = '0'
buff = tmp
for i=0,172 do begin
buff = buff+tmp
endfor

;  p1 : RA�w��1
r = (p1 eq 0) or (p1 eq 1) or (p1 eq 2) or (p1 eq 4) or (p1 eq 8)
if r then begin
   strput,buff,strtrim(string(p1),1),5
endif

;  p2 : RA�w��2
r = (p2 eq 0) or (p2 eq 1) or (p2 eq 2) or (p2 eq 8)
if r then begin
   strput,buff,strtrim(string(p2),1),6
endif

; p3 : RA�ڕW�ʒu
ra = dec2hex(p3,8)              ;8�����ɕϊ�����
strput,buff,ra,7

;  p4 : DEC�w��1
r = (p4 eq 0) or (p4 eq 1) or (p4 eq 2) or (p4 eq 4) or (p4 eq 8)
if r then begin
   strput,buff,strtrim(string(p4),1),15
endif

;  p5 : DEC�w��2
r = (p5 eq 0) or (p5 eq 1) or (p5 eq 2) or (p5 eq 4) or (p5 eq 8)
if r then begin
   strput,buff,strtrim(string(p5),1),16
endif

;  p6 : DEC�ڕW�ʒu
dec = dec2hex(p6,8)              ;8�����ɕϊ�����
strput,buff,dec,17

;  p7 : T1�W�J��
r = (p7 eq 0) or (p7 eq 1) or (p7 eq 2) or (p7 eq 4) or (p7 eq 8)
if r then begin
   strput,buff,strtrim(string(p7),1),25
endif

;  p8 : T1�t�H�[�J�X�w��1
r = (p8 eq 0) or (p8 eq 1) or (p8 eq 2) or (p8 eq 4) or (p8 eq 8)
if r then begin
   strput,buff,strtrim(string(p8),1),26
endif

;  p9 : T1�t�H�[�J�X�w��2
r = (p9 eq 0) or (p9 eq 1) or (p9 eq 8)
if r then begin
   strput,buff,strtrim(string(p9),1),27
endif

;  p10: T1�t�H�[�J�X�ڕW�ʒu
focus = dec2hex(p10,6)              ;6�����ɕϊ�����
strput,buff,focus,28

;  p11: T1�t���b�g/�N���A
r = (p11 eq 0) or (p11 eq 1) or (p11 eq 2) or (p11 eq 4) or (p11 eq 8)
if r then begin
   strput,buff,strtrim(string(p11),1),34
endif

;  p12: T1ND�t�B���^�w��1
r = (p12 eq 0) or (p12 eq 1) or (p12 eq 4) or (p12 eq 5) or (p12 eq 8)
if r then begin
   strput,buff,strtrim(string(p12),1),164
endif

;  p13: T1ND�t�B���^�w��2
r = (p13 eq 0) or (p13 eq 1) or (p13 eq 2) or (p13 eq 3) or (p13 eq 4)
if r then begin
   strput,buff,strtrim(string(p13),1),165
endif

; shift
return, strmid(buff,1,173)+'0'
;
end

