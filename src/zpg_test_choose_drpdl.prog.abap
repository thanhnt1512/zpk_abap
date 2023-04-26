*&---------------------------------------------------------------------*
*& Report ZPG_TEST_CHOOSE_DRPDL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_choose_drpdl.

*DATA: gd_ucomm type sy-ucomm.
*PARAMETERS p_cid TYPE spfli-carrid
*                    AS LISTBOX VISIBLE LENGTH 25
*                    USER-COMMAND UPD.
*PARAMETERS: p_uname type uname.
*AT SELECTION-SCREEN.
**This event is then triggered when a user selects an option from the LISTBOX
*  gd_ucomm = sy-ucomm. "capture user command
*  If sy-ucomm eq 'UPD'. "listbox selection made
**   add code here to process data
*  ENDIF.
*AT SELECTION-SCREEN OUTPUT.
** The OUTPUT event is also trigged to re-draw ABAP report screen allowing it to
** be used to hide, display or deactivate fields. Please note at this point sy-ucomm field
** has been refreshed so you need to use value captured above in gd_ucomm
*  case gd_ucomm.
*    when 'UPD'. "listbox selection made
**     loop at screen code is used to change the display properties of the screen fields.
**     I.e. allows you to hide, display or set as output only fields on the ABAP selection
**     screen
*      LOOP at screen.
**       Use cs as this then captures all elements of the field inc text
*        if screen-name cs 'P_UNAME'.
*          screen-active = 0. "remove field from screen
**          screen-input = 0. "set field as display only
*          modify screen.
*        endif.
*      ENDLOOP.
*  ENDCASE.
"""""""""""""""""""""""""""""""""""""""""
*PARAMETERS p_bukrs(10) type c.
*
*SELECTION-SCREEN: COMMENT /10(65) TEXT-005 FOR FIELD P_BUKRS MODIF ID SC1.
*
*AT SELECTION-SCREEN OUTPUT.
*
*LOOP AT SCREEN.
*
*IF SCREEN-GROUP1 = 'SC1'.
*
*scREEN-INTENSIFIED = 1.
*
*MODIFY SCREEN.
*
*ENDIF.
*
*ENDLOOP.
"""""""""""""""""""""""""""""""
*DATA: lv_count    TYPE i.
*
*SELECTION-SCREEN BEGIN OF BLOCK main.
*PARAMETERS: p_netw    TYPE aufnr  MODIF ID auf.
*PARAMETERS: p_wbs     TYPE i MODIF ID psp.
*SELECTION-SCREEN END OF BLOCK   main.
*
*AT SELECTION-SCREEN ON p_netw.
*  ADD 1 TO lv_count.
*  p_wbs = lv_count.
*  DATA : list_param TYPE TABLE OF rsparamsl_255.
*  list_param = VALUE #( ( selname = 'P_NETW' ) ).
*  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
*    EXPORTING
*      curr_report     = sy-repid
**   IMPORTING
**     SP              =
*    TABLES
*      selection_table = list_param
**     SELECTION_TABLE_255       =
**   EXCEPTIONS
**     NOT_FOUND       = 1
**     NO_REPORT       = 2
**     OTHERS          = 3
*    .
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.
*
*
*AT SELECTION-SCREEN OUTPUT.
*  LOOP AT SCREEN.
*    IF screen-name = 'P_NETW'.
*      screen-required = '2'.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
*
*START-OF-SELECTION.
*  PERFORM main.
*
*
*FORM main.
*  WRITE: 'The value reached ', lv_count.
*ENDFORM.
"""""""""""""""""""""""""""""""""""""""""""""""""
PARAMETERS p_bukrs TYPE t001-bukrs.
PARAMETERS p_bukrs1 TYPE t001-bukrs.
PARAMETERS p_sum RADIOBUTTON GROUP r01 DEFAULT 'X' USER-COMMAND r01.
PARAMETERS p_det RADIOBUTTON GROUP r01.
PARAMETERS p_sub AS CHECKBOX.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'P_BUKRS' OR 'P_BUKRS1'.
        screen-required = '2'.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN ON p_bukrs1 .
  IF sy-ucomm = 'ONLI'.
    IF p_bukrs1 IS INITIAL.
      MESSAGE e055(00).
    ENDIF.
  ENDIF.
