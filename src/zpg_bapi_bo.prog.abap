*&---------------------------------------------------------------------*
*& Report ZPG_BAPI_BO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_bapi_bo.



DATA: lt_cvis_ei_extern_t TYPE cvis_ei_extern_t,
      lt_return           TYPE  bapiretm.
BREAK-POINT.
CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
  EXPORTING
    i_data   = lt_cvis_ei_extern_t
  IMPORTING
    e_return = lt_return.
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
  EXPORTING
    wait = 'X'.
