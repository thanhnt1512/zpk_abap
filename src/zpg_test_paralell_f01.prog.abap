*&---------------------------------------------------------------------*
*& Include          ZPG_TEST_PARALELL_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form GET_RESOURCE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_resource .

  SELECT * FROM bkpf INTO CORRESPONDING FIELDS OF TABLE gt_data UP TO 80 ROWS.
  CALL FUNCTION 'SPBT_INITIALIZE'
    EXPORTING
      group_name   = 'RFCGROUP'
    IMPORTING
      max_pbt_wps  = gw_max_ws
      free_pbt_wps = gw_free_ws.

  gw_max_record = ceil( lines( gt_data ) / ( gw_free_ws - 2 ) ).
  IF lines( gt_data ) <  gw_free_ws.
    gw_max_record = 1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process .
  "Thanhnt
  CALL FUNCTION 'SPTA_PARA_PROCESS_START_2'
    EXPORTING
      server_group             = 'RFCGROUP'
*     MAX_NO_OF_TASKS          =
      before_rfc_callback_form = 'F_BEFORE_RFC'
      in_rfc_callback_form     = 'F_IN_RFC'
      after_rfc_callback_form  = 'F_AFTER_RFC'
      callback_prog            = sy-repid
*     SHOW_STATUS              = ' '
*     RESOURCE_TIMEOUT         = 600
*     TASK_CALL_MODE           = 1
*   CHANGING
*     USER_PARAM               =
*   EXCEPTIONS
*     INVALID_SERVER_GROUP     = 1
*     NO_RESOURCES_AVAILABLE   = 2
*     OTHERS                   = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
"Before rfc
FORM f_before_rfc  USING
                            p_before_rfc_imp TYPE spta_t_before_rfc_imp
                   CHANGING
                            p_before_rfc_exp     TYPE spta_t_before_rfc_exp
                            pt_rfcdata           TYPE spta_t_indxtab
                            p_failed_objects     TYPE spta_t_failed_objects
                            p_objects_in_process TYPE spta_t_objects_in_process
                            p_user_param.

  DATA :it_data_push TYPE TABLE OF bkpf.
  CHECK gt_data IS NOT INITIAL.
  SELECT * FROM @gt_data AS gt_data1_sel INTO TABLE @it_data_push UP TO @gw_max_record ROWS.
  DELETE gt_data FROM 1 TO gw_max_record.

  CALL FUNCTION 'SPTA_INDX_PACKAGE_ENCODE'
    EXPORTING
      data    = it_data_push
    IMPORTING
      indxtab = pt_rfcdata.

  IF it_data_push IS NOT INITIAL.
    p_before_rfc_exp-start_rfc = 'X'.
  ELSE.
    CLEAR  p_before_rfc_exp-start_rfc .
  ENDIF.

ENDFORM.
"In rfc
FORM f_in_rfc
             USING
                p_in_rfc_imp  TYPE spta_t_in_rfc_imp
             CHANGING
                p_in_rfc_exp  TYPE spta_t_in_rfc_exp
                p_rfcdata     TYPE spta_t_indxtab.
  DATA :it_data_clone TYPE TABLE OF bkpf.
  CALL FUNCTION 'SPTA_INDX_PACKAGE_DECODE'
    EXPORTING
      indxtab = p_rfcdata
    IMPORTING
      data    = it_data_clone.

  LOOP AT it_data_clone ASSIGNING FIELD-SYMBOL(<fs_data_clone>).
    <fs_data_clone>-bktxt = 'THANHNT'.
  ENDLOOP.

  CALL FUNCTION 'SPTA_INDX_PACKAGE_ENCODE'
    EXPORTING
      data    = it_data_clone
    IMPORTING
      indxtab = p_rfcdata.

ENDFORM.

"after rfc
FORM f_after_rfc
     USING
      p_rfcdata            TYPE spta_t_indxtab
      p_rfcsubrc           TYPE sy-subrc
      p_rfcmsg             TYPE spta_t_rfcmsg
      p_objects_in_process TYPE spta_t_objects_in_process
      p_after_rfc_imp      TYPE spta_t_after_rfc_imp
   CHANGING
      p_after_rfc_exp      TYPE spta_t_after_rfc_exp
      p_user_param.
  DATA :it_data_clone TYPE TABLE OF bkpf.
  IF p_rfcsubrc IS INITIAL.

    CALL FUNCTION 'SPTA_INDX_PACKAGE_DECODE'
      EXPORTING
        indxtab = p_rfcdata
      IMPORTING
        data    = it_data_clone.
    APPEND LINES OF it_data_clone TO gt_result_collect.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PRINT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM print .
 cl_demo_output=>display_data( gt_result_collect[] ).
ENDFORM.
