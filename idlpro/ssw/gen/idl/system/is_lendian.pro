function is_lendian
;+
; NAME:
;    IS_LENDIAN.PRO
;
;
; PURPOSE:
;    Many routines (WRITEFITS, ANAFRD, etc) depend on the endianness
;    of the hardware platform.  This routine centralizes this in one
;    place so new platforms may be added at will.
;
;
; CALLING SEQUENCE:
;    little_endian_flag = is_endian()
;
; 
; INPUTS:
;    None.  is_endian checks !version.os for the platform.
;
;
; OUTPUTS:
;    little_endian_flag = 1     ; Machine is little endian
;                       = 0     ; Machine is big endian
;
;
; RESTRICTIONS:
;    If your OS isn't listed, it's automatically big endian.
;    This either means your machine is big endian, or it wasn't
;    included in this missive.
;
;
; MODIFICATION HISTORY:
;    18-Jun-97 - (BNH) - Written (mostly from SLF)
;
;-

;
;  NOTE:  endianness is hardware-dependent, not software.  Hence,
;         Win95, WinNT and Linux should all be the same.  Similarly
;         anything running on an alpha would have the same endian,
;         and anything on a sun box would be the same, be it solaris,
;         sunos, etc.  SO...try to key off this alone.
;


lendlist = 'vax,alpha,mipsel,386i,386,x86'

return, is_member(!VERSION.ARCH, str2arr(lendlist), /ignore_case)

end
