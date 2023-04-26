*&---------------------------------------------------------------------*
*& Report ZRFC_TEST_EP2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZRFC_TEST_EP2.
DATA :lw_matnr TYPE matnr,
      lt_tab TYPE TABLE of mara .
 lw_matnr  = 'PROD'.
CALL FUNCTION 'ZGET_MARA' DESTINATION 'EP1CLNT900'
  EXPORTING
    matnr          = lw_matnr
  tables
    mara_tab       = lt_tab
          .
