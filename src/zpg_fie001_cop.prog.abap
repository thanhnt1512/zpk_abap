*&---------------------------------------------------------------------*
*& Report  ZPG_FIE001 - Upload
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Create by: HuyTD8
*& Create date: 19.07.2019
*&---------------------------------------------------------------------*

INCLUDE ZPG_FIE001_TOP_COP.
*INCLUDE zpg_fie001_top.  " global Data

INCLUDE ZPG_FIE001_F01_COP.
*INCLUDE zpg_fie001_f01.  " FORM-Routines

*  PERFORM init.

START-OF-SELECTION.
  PERFORM split_file_name.
  PERFORM check_log.
  PERFORM upload_data.
  PERFORM get_data.
  PERFORM check_data.

  IF gt_message IS INITIAL.
    gw_check_post = abap_false.
    IF p_ptax IS INITIAL.
      PERFORM process_data.
    ELSE.
*      PERFORM process_data_tax.
    ENDIF.
    IF p_test IS INITIAL.
      PERFORM save_log.
    ENDIF.
  ENDIF.

  IF gt_message IS NOT INITIAL.
    PERFORM message_alv.
  ENDIF.

*&---------------------------------------------------------------------*
*& At selection screen on value request
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_FILE'
    IMPORTING
      file_name  = p_file.

INITIALIZATION.
  CALL METHOD zcl_utility=>get_tvarv_s(
    EXPORTING
      i_name    = 'ZSV_DOCTYPE_PARTNER_GR_UP'
      i_na_add  = 'X'
    IMPORTING
      e_t_range = gr_type_pgr_up ).
  CALL METHOD zcl_utility=>get_tvarv_s(
    EXPORTING
      i_name    = 'ZSV_ACC_PARTNER_GR_UP'
      i_na_add  = 'X'
    IMPORTING
      e_t_range = gr_acc_pgr_up ).
  SELECT * FROM ztb_partner_gr INTO TABLE gt_partner_gr.
  SORT gt_partner_gr BY partner_value.

  CONCATENATE icon_export 'Download Template'
               INTO sscrfields-functxt_01 SEPARATED BY space.
  CASE sy-tcode.
    WHEN 'ZFIE001'.
      sy-title = 'Upload Document ( Park )'.
      gw_form_name = 'VTP_ZFIE001'.
    WHEN 'ZFIE003'.
      sy-title = 'Upload Document ( Post )'.
      gw_form_name = 'VTP_ZFIE003'.
    WHEN OTHERS.
      sy-title = 'Upload Document ( Park )'.
      gw_form_name = 'VTP_ZFIE001'.
  ENDCASE.

AT SELECTION-SCREEN.
  IF sy-ucomm = 'FC01'.
    CALL FUNCTION 'ZXLWB_WORKBENCH'
      EXPORTING
        iv_formname        = gw_form_name
        iv_action          = 'EXPORT'
      EXCEPTIONS
        process_terminated = 1
        OTHERS             = 2.
    IF sy-subrc NE 0 .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
    ENDIF .
  ENDIF.

  "add by HUNGVT

AT SELECTION-SCREEN OUTPUT.
  IF sy-tcode <> 'ZFIE003'.
    LOOP AT SCREEN.
      IF screen-name = 'P_CTGS' OR screen-name = 'P_PTAX'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = '%C007007_1000'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  "end by HUNGVT
