*&---------------------------------------------------------------------*
*& Report ZPG_TEST_VAR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_var.
DATA :gr_type_user TYPE fccx_t_range_row.
CALL METHOD zcl_utility=>get_tvarv_s(
  EXPORTING
    i_name    = 'ZVAR_USR'
*    i_na_add  = 'X'
  IMPORTING
    e_t_range = gr_type_user ).

BREAK-POINT.
