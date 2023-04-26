*&---------------------------------------------------------------------*
*& Include          ZIN_UPLOAD_ASSET_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: lv_ucomm_0100 TYPE sy-ucomm.
  lv_ucomm_0100 = sy-ucomm.
  CLEAR sy-ucomm.
  CASE lv_ucomm_0100.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'TEST'.
      PERFORM execute USING gc_test_mode.
    WHEN 'PARK'.
      PERFORM execute USING gc_park_mode.
    WHEN 'POST'.
      PERFORM execute USING gc_post_mode.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  DATA: lw_ucomm_0200 TYPE sy-ucomm,
        lw_err_0200   TYPE char1,
        lt_data_excel TYPE ztt_upload_asset_cc,
        lt_dynpfields TYPE TABLE OF dynpread.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = SY-REPID
      dynumb               = '0200'
    TABLES
      dynpfields           = lt_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.
  lw_ucomm_0200 = sy-ucomm.
  CLEAR sy-ucomm.
  PERFORM get_author CHANGING lw_err_0200
                              lt_data_excel.
  CHECK lw_err_0200 IS INITIAL.
  CASE lw_ucomm_0200.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM main.
    WHEN 'EXPORT'.
      PERFORM export_template USING lt_data_excel.
    WHEN 'HIST'.
      PERFORM show_history.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  DATA: lw_ucomm_0300 TYPE sy-ucomm.
  lw_ucomm_0300 = sy-ucomm.
  CLEAR sy-ucomm.
  CASE lw_ucomm_0300.
    WHEN 'OK'.
      PERFORM show_history_header.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.
  DATA: lw_ucomm_0400 TYPE sy-ucomm.
  lw_ucomm_0400 = sy-ucomm.
  CLEAR sy-ucomm.
  CASE lw_ucomm_0400.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.
  DATA: lw_ucomm_0500 TYPE sy-ucomm.
  lw_ucomm_0500 = sy-ucomm.
  CLEAR sy-ucomm.
  CASE lw_ucomm_0500.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
