*&---------------------------------------------------------------------*
*& Report ZPG_LIST_EV_ALV
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_LIST_EV_ALV.
 DATA t_eve TYPE SLIS_T_EVENT.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = t_eve
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.

  WRITE 'ok'.
