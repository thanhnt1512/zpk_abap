*&---------------------------------------------------------------------*
*& Report ZPG_TEST_RANGE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_range.
DATA: gt_list     TYPE vrm_values.
DATA: gwa_list    TYPE vrm_value.
DATA: gt_values  TYPE TABLE OF dynpread,
      gwa_values TYPE dynpread.

DATA: gv_selected_value(100) TYPE c.
DATA :number TYPE nriv-nrlevel.
*--------------------------------------------------------------*
*Selection-Screen
*--------------------------------------------------------------*
PARAMETERS: list TYPE c AS LISTBOX VISIBLE LENGTH 20 OBLIGATORY.
*--------------------------------------------------------------*
*At Selection Screen
*--------------------------------------------------------------*
AT SELECTION-SCREEN ON list.
  CLEAR: gwa_values, gt_values.
  REFRESH gt_values.
  gwa_values-fieldname = 'LIST'.
  APPEND gwa_values TO gt_values.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-cprog
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = gt_values.

  READ TABLE gt_values INDEX 1 INTO gwa_values.
  IF sy-subrc = 0 AND gwa_values-fieldvalue IS NOT INITIAL.
    READ TABLE gt_list INTO gwa_list
                      WITH KEY key = gwa_values-fieldvalue.
    IF sy-subrc = 0.
      gv_selected_value = gwa_list-text.
    ENDIF.
  ENDIF.
*--------------------------------------------------------------*
*Initialization
*--------------------------------------------------------------*
INITIALIZATION.
  gwa_list-key = '1'.
  gwa_list-text = 'Form'.
  APPEND gwa_list TO gt_list.
  gwa_list-key = '2'.
  gwa_list-text = 'Report'.
  APPEND gwa_list TO gt_list.
  gwa_list-key = '3'.
  gwa_list-text = 'Interface'.
  APPEND gwa_list TO gt_list.
  gwa_list-key = '4'.
  gwa_list-text = 'Conversion'.
  APPEND gwa_list TO gt_list.
  gwa_list-key = '5'.
  gwa_list-text = 'Enhancement'.
  APPEND gwa_list TO gt_list.
  gwa_list-key = '6'.
  gwa_list-text = 'Workflow'.
  APPEND gwa_list TO gt_list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'LIST'
      values          = gt_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
*--------------------------------------------------------------*
*Start of Selection
*--------------------------------------------------------------*
START-OF-SELECTION.
  CASE gv_selected_value.
    WHEN 'Form'.
      PERFORM get_next USING 'ZTEST_NUMR'.
    WHEN 'Report'.
      PERFORM get_next USING 'ZTEST_NUM1'.
    WHEN 'Interface'.
      PERFORM get_next USING 'ZTEST_NUM2'.
    WHEN 'Conversion'.
      PERFORM get_next USING 'ZTEST_NUM3'.
    WHEN 'Enhancement'.
      PERFORM get_next USING 'ZTEST_NUM4'.
    WHEN 'Workflow'.
      PERFORM get_next USING 'ZTEST_NUM5'.
    WHEN OTHERS.
      MESSAGE '...' TYPE 'E'.
  ENDCASE.
*&---------------------------------------------------------------------*
*& Form GET_NEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM get_next USING val TYPE NROBJ.
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = val
*     QUANTITY    = '1'
*     SUBOBJECT   = ' '
*     TOYEAR      = '0000'
*     IGNORE_BUFFER                 = ' '
    IMPORTING
      number      = number
*  EXCEPTIONS
*     INTERVAL_NOT_FOUND            = 1
*     NUMBER_RANGE_NOT_INTERN       = 2
*     OBJECT_NOT_FOUND              = 3
*     QUANTITY_IS_0                 = 4
*     QUANTITY_IS_NOT_1             = 5
*     INTERVAL_OVERFLOW             = 6
*     BUFFER_OVERFLOW               = 7
*     OTHERS      = 8
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  WRITE number.

ENDFORM.
