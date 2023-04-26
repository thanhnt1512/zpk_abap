*&---------------------------------------------------------------------*
*& Report ZPG_LOCK_BP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_lock_bp.
DATA flag TYPE char1.
DATA(obj_mds_bupa_lock) =  NEW cl_im_mds_bupa_lock( ).
DATA :ls_mess TYPE mds_ctrls_error.
CALL METHOD obj_mds_bupa_lock->if_ex_bupa_lock~lock
  EXPORTING
    iv_partner = '0001001203'
*   iv_partner_guid =
   iv_enqmode = 'E'
  CHANGING
    cs_error   = ls_mess.

CLEAR ls_mess .
CHECK flag = 'X'.
CALL METHOD obj_mds_bupa_lock->if_ex_bupa_lock~unlock
  EXPORTING
    iv_partner = '0001001203'
*   iv_partner_guid =
*   iv_enqmode = 'E'
  CHANGING
    cs_error   = ls_mess.
