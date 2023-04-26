*&---------------------------------------------------------------------*
*& Report ZPG_TEST_ZFIEXXX
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_zfiexxx.
INCLUDE zpg_test_zfiexxx_top.
INCLUDE zpg_test_zfiexxx_f01.

INITIALIZATION.
  CONCATENATE icon_export 'Download Template'
               INTO sscrfields-functxt_01 SEPARATED BY space.
  CONCATENATE icon_export 'hi'
               INTO sscrfields-functxt_02 SEPARATED BY space.

AT SELECTION-SCREEN.
  IF sy-ucomm = 'FC01'.
   MESSAGE 'Success!' TYPE 'I' DISPLAY LIKE 'S'.
  ENDIF.

START-OF-SELECTION.
