*&---------------------------------------------------------------------*
*& Report ZPG_GET_VARIANT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_get_variant.
DATA :lr_type_gl TYPE fccx_t_range_row.
DATA :hkont TYPE bseg-hkont.


CALL METHOD zcl_utility=>get_tvarv_s(
  EXPORTING
    i_name    = 'ZVAR_TEST'
*   i_na_add  = 'X'
  IMPORTING
    e_t_range = lr_type_gl ).

hkont = '1129999998'.

if hkont In lr_type_gl.
  WRITE 'ok'.
ENDIF.
