;+
; HEX2DEC
;
; INPUTS:
;  sts_hex : 16�i���̓��
;  digits  : ���16�i���̌��i�������j
;
; HISTORY:
;  2011.08.09, S.MORITA
;-
FUNCTION HEX2DEC, sts_hex, digits

    OUT = 0ll

    READS, sts_hex, OUT, FORMAT = '(Z)'

    IF OUT GT 16l^(digits-1) THEN OUT = OUT - 16ll^digits

RETURN, OUT

END

;***************************************************************************

;+
; PREP_T1STATUS
;
; HISTORY:
;  2011.08.10, S. MORITA
;-
FUNCTION PREP_T1STATUS, sts_ascii

  On_error, 2

IF STRLEN(sts_ascii) NE 182 THEN STOP, 'PREP_T1STATUS: input should be 182 characters.'

SYS  =  {                $
          STS1:      -1, $     ; �V�X�e���X�e�[�^�X1
          STS2:      -1, $     ; �V�X�e���X�e�[�^�X2
          ERR_CODE:  -1  $     ; �G���[�R�[�h(1~255, 0�̓G���[����)
        }

TELESCOP = {                  $
             RA_STS1:   -1,   $ ; RA���1
             RA_STS2:   -1,   $ ; RA���2
             RA_POS:    -1ll, $ ; RA���݈ʒu(0.1�b�p�P��)
             DEC_STS1:  -1,   $ ; DEC���1
             DEC_STS2:  -1,   $ ; DEC���2
             DEC_POS:   -1ll, $ ; DEC���݈ʒu(0.1�b�p�P��)
             GPS_TIM:   -1l   $ ; GPS����(�b�P��)
           }

T1  = {                    $
        LID:          -1,  $ ; T1�W�J��
        FOCUS_STS1:   -1,  $ ; T1�t�H�[�J�X1���1
        FOCUS_STS2:   -1,  $ ; T1�t�H�[�J�X1���2
        FOCUS_POS:    -1l, $ ; T1�t�H�[�J�X1���݈ʒu(0.1um�P��)
        FLAT_STS:     -1,  $ ; T1�t���b�g/�N���A���
        ND_STS:       -1,  $ ; T1ND���
        ND_POINT:     -1   $ ; T1ND���݃|�C���g(1~4�̃|�C���g)
      }

OUT      = {                     $
             SYS:      SYS,      $
             TELESCOP: TELESCOP, $
             T1:       T1        $
           }

;+ === system ===

; �V�X�e���X�e�[�^�X1
OUT.SYS.STS1 = FIX(STRMID(sts_ascii,2,1))

; �V�X�e���X�e�[�^�X2
OUT.SYS.STS2 = FIX(STRMID(sts_ascii,3,1))

; �G���[�R�[�h(1~255, 0�̓G���[����)
OUT.SYS.ERR_CODE = FIX(HEX2DEC(STRMID(sts_ascii,4,2),2))

;- === system ===

;+ === telescope base ===

; RA���1
OUT.TELESCOP.RA_STS1 = FIX(STRMID(sts_ascii,6,1))

; RA���2
OUT.TELESCOP.RA_STS2 = FIX(STRMID(sts_ascii,7,1))

; RA���݈ʒu(0.1�b�p�P��)
OUT.TELESCOP.RA_POS = HEX2DEC(STRMID(sts_ascii,8,8),8)

; DEC���1
OUT.TELESCOP.DEC_STS1 = FIX(STRMID(sts_ascii,16,1))

; DEC���2
OUT.TELESCOP.DEC_STS2 = FIX(STRMID(sts_ascii,17,1))

; DEC���݈ʒu(0.1�b�p�P��)
OUT.TELESCOP.DEC_POS = HEX2DEC(STRMID(sts_ascii,18,8),8)

; GPS����(�b�P��)
OUT.TELESCOP.GPS_TIM = LONG(HEX2DEC(STRMID(sts_ascii,26,6),6))

;- === telescope base ===

;+ === T1 ===

; T1�W�J��
OUT.T1.LID = FIX(STRMID(sts_ascii,32,1))

; T1�t�H�[�J�X���1
OUT.T1.FOCUS_STS1 = FIX(STRMID(sts_ascii,33,1))

; T1�t�H�[�J�X���2
OUT.T1.FOCUS_STS2 = FIX(STRMID(sts_ascii,34,1))

; T1�t�H�[�J�X���݈ʒu(0.1um�P��)
OUT.T1.FOCUS_POS = LONG(HEX2DEC(STRMID(sts_ascii,35,6),6))

; T1�t���b�g/�N���A���
;OUT.T1.FLAT_STS = FIX(STRMID(sts_ascii,41,1))

; T1ND���
;OUT.T1.ND_STS = FIX(STRMID(sts_ascii,171,1))

; T1ND���݃|�C���g(1~4�̃|�C���g)
;OUT.T1.ND_POINT = FIX(STRMID(sts_ascii,172,1))

;- === T1 ===

RETURN, OUT

END

;***************************************************************************

;+
; READ_CURRENT_STATUS
;
; HISTORY:
;  2011.08.09, S. MORITA
;-
FUNCTION READ_CURRENT_STATUS, infil, sts_byt

  On_error, 2

;+==== some parameters
sz_one_sts       = 181            ;[Bytes]
sz_one_line      = sz_one_sts + 3 ;[Bytes]
;-==== some parameters

  sts_byt    = BYTARR(sz_one_line)

  ON_IOERROR, BadDataExceotion

  OPENR,     lun, infil, /GET_LUN
  sts_fstat  = FSTAT(lun)
  POINT_LUN, lun, sts_fstat.size - sz_one_line
  READU,     lun, sts_byt
  FREE_LUN,  lun
  ret = 1
  RETURN, ret

  BadDataExceotion: PRINT, 'READ_CURRENT_STATUS: Problem reading file, Returning...'
  IF N_ELEMENTS(lun) NE 0 THEN FREE_LUN, lun
  ret = 0
  RETURN, ret

END

;***************************************************************************

;+
; GET_T1STATUS
;
; CALLING SEQUENCE : 
;       IDL> print, get_t1status(infil, sts)
;
; INPUTS:
;       infil : �V�X�e������A�v���̏����o��status file�ւ�PATH
;            ex.) infil = 's:sts20110808.txt' 
;
; OUTPUTS:
;       sts : �]�����V�X�e���A�y��T4�֘A�̃X�e�C�^�X
;
; sts.sys.sts1          ; �V�X�e���X�e�[�^�X1
; sts.sys.sts2          ; �V�X�e���X�e�[�^�X2
; sts.sys.err_code      ; �G���[�R�[�h(1~255, 0�̓G���[����)
; sts.telescop.ra_sts1  ; RA���1
; sts.telescop.ra_sts2  ; RA���2
; sts.telescop.ra_pos   ; RA���݈ʒu(0.1�b�p�P��)
; sts.telescop.dec_sts1 ; DEC���1
; sts.telescop.dec_sts2 ; DEC���2
; sts.telescop.dec_pos  ; DEC���݈ʒu(0.1�b�p�P��)
; sts.telescop.gps_tim  ; GPS����(�b�P��)
; sts.t1.lid            ; T1�W�J��
; sts.t1.focus_sts1     ; T1�t�H�[�J�X���1
; sts.t1.focus_sts2     ; T1�t�H�[�J�X���2
; sts.t1.focus_pos      ; T1�t�H�[�J�X���݈ʒu(0.1um�P��)
; sts.t1.flat_sts       ; T1�t���b�g/�N���A���
; sts.t1.nd_sts         ; T1ND���
; sts.t1.nd_point       ; T1ND���݃|�C���g(1~4�̃|�C���g)
;
; MODIFICATION HISTORY: 
;       10 Aug 2011, S. MORITA
;-
;
FUNCTION GET_T1STATUS, infil, sts

  On_error, 2

; infil = './fake_stsyyyymmdd.txt'

  ;+==== some parameters
  sz_one_sts       = 181            ;[Bytes]
  sz_one_line      = sz_one_sts + 3 ;[Bytes]
  STX_BYT          = 2
  ETX_BYT          = 3
  retry_max        = 25
  wait_tim         = 0.02
  ;-==== some parameters

  ;+==== read current status
  ret       = 0
  num_retry = 0
  WHILE 1 DO BEGIN 
    ret = READ_CURRENT_STATUS(infil, sts_byt)

    IF ret EQ 1 THEN               $
      IF (sts_byt[1] NE STX_BYT)   $
      OR (sts_byt[181] NE ETX_BYT) $
      THEN ret = 0

    IF ret EQ 1 THEN BREAK

    num_retry = num_retry + 1
    IF num_retry GT retry_max THEN BREAK
    WAIT, wait_tim
  ENDWHILE
  ;-==== read current status

  IF ret THEN BEGIN
    sts_ascii = STRING(sts_byt[2:180])
    sts_ascii = '00' + sts_ascii + '0'

    sts = PREP_T1STATUS(sts_ascii)
  ENDIF ELSE STOP, 'GET_T1STATUS: Problem reading status file, STOP....'

RETURN, ret

END
