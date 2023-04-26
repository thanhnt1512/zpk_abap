FUNCTION ZFM_ACAC_S4_EXCEL_UPLOAD .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_FILENAME) TYPE  STRING
*"  RAISING
*"      CX_OPENXML_FORMAT
*"      CX_OPENXML_NOT_FOUND
*"----------------------------------------------------------------------
  TYPES t_xline(2048) TYPE x.
  DATA:
    lt_data           TYPE TABLE OF t_xline,
    ld_size           TYPE i,
    lo_xlsx_uploading TYPE REF TO ycl_acac_s4_xlsx_uploading,
    ld_xstring        TYPE xstring,
    lt_upload_result  TYPE TABLE OF acac_s4_upload_result_worklist,
    lt_upload_content TYPE acac_s4_upload_content_t,
    ld_error_occurred TYPE flag,
    lt_return         TYPE bapiret2_t,
    ld_index          TYPE i VALUE 1.

*  FIELD-SYMBOLS:
*    <fs_upload_content>   TYPE acac_upload_content,
*    <fs_excel_value_line> TYPE acac_index_value_pair.

  " clear global values before each upload
*  CLEAR: gd_upload_status, gt_upload_content, gt_upload_result.
*  cl_ace_message_handler=>get_instance( )->reset( ).

  TRY.
      " transform excel file into binary type
      CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
          filename                = iv_filename
          filetype                = 'BIN'
        IMPORTING
          filelength              = ld_size
        TABLES
          data_tab                = lt_data
        EXCEPTIONS
          file_open_error         = 1
          file_read_error         = 2
          no_batch                = 3
          gui_refuse_filetransfer = 4
          invalid_type            = 5
          no_authority            = 6
          unknown_error           = 7
          bad_data_format         = 8
          header_not_allowed      = 9
          separator_not_allowed   = 10
          header_too_long         = 11
          unknown_dp_error        = 12
          access_denied           = 13
          dp_out_of_memory        = 14
          disk_full               = 15
          dp_timeout              = 16
          OTHERS                  = 17.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE cx_acac_xlsx_exception
          MESSAGE ID 'ACAC_EXCEL'
          TYPE 'E'
          NUMBER '003'.
      ENDIF.

      " transform binary to xstring
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = ld_size
        IMPORTING
          buffer       = ld_xstring
        TABLES
          binary_tab   = lt_data
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE cx_acac_xlsx_exception
          MESSAGE ID 'ACAC_EXCEL'
          TYPE 'E'
          NUMBER '003'.
      ENDIF.

      CREATE OBJECT lo_xlsx_uploading.
      " upload the excel file to create or update the ACAC object
      CALL METHOD lo_xlsx_uploading->upload_acac_excel
        EXPORTING
          iv_file_name      = iv_filename
          iv_xfile          = ld_xstring
        IMPORTING
          eb_error_occurred = ld_error_occurred
          et_upload_content = lt_upload_content
          et_upload_result  = lt_upload_result
        CHANGING
          ct_return         = lt_return.

    CATCH cx_acac_xlsx_exception INTO DATA(lx_acac_exception).
      cl_ace_message_handler=>get_instance( )->add_message_inst(
        EXPORTING
          ix_ace        = lx_acac_exception                 " ACE: Exception
      ).
  ENDTRY.

  " lt_return contains the parsing error messages, don't call the result list if parsing error happened
  " parsing error messages will display directly, not in result list.
  IF cl_ace_message_handler=>get_instance( )->check_error_occurred( ) EQ abap_true.
    cl_ace_message_handler=>get_instance( )->display_log( ib_tree    = abap_false
                                                          ib_grid    = abap_false ).
  ELSE.

    " upload result list for every accrual object which inputted in excel
    " for the error message which occurred during creating the accrual object, the result list
    " will show them.
*    gt_upload_result  = lt_upload_result. " this internal table is for ALV display
*    gt_upload_content = lt_upload_content. " this internal table contains much more information like error messages

    " if error occurred then display messages in application log
    IF ld_error_occurred EQ abap_true.
      ROLLBACK WORK.
    ELSE.
      " if no error occurred then commit work
      COMMIT WORK.
*      gd_upload_status = abap_true.
    ENDIF.
    " show the uploaded worklist
    CALL SCREEN 2000.
  ENDIF.
ENDFUNCTION.
