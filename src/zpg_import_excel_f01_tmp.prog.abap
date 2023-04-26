*&---------------------------------------------------------------------*
*& Include          ZPG_IMPORT_EXCEL_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .

  SELECT * FROM ztb_sp_bank_acc INTO @DATA(gs_map) WHERE hbkid = @p_search.
  ENDSELECT.
  SELECT bukrs,
        hbkid,
        bankn
    FROM t012k
  INTO TABLE @DATA(gt_t012k)
  WHERE hbkid = @p_search.
  CALL FUNCTION 'UPLOAD_XLS_FILE_2_ITAB'
    EXPORTING
      i_filename = p_file
    TABLES
      e_itab     = gt_excel
    EXCEPTIONS
      file_error = 1
      OTHERS     = 2.
  IF sy-subrc = 0.
    SELECT * FROM ztb_sp_map_templ INTO TABLE @DATA(gt_templ)
        WHERE hbkid = @p_search.
    SORT gt_templ BY lino.
    READ TABLE gt_templ INTO DATA(gs_templ_1) WITH KEY field_nm = 'SOTK'.
    IF sy-subrc = 0.
      IF gs_templ_1-zcolumn = 0.
        READ TABLE gt_excel INTO DATA(gs_excel_1) INDEX gs_map-accno_rowid.
        IF sy-subrc IS INITIAL.
*          lw_string = gs_map-accno_colid.
          ASSIGN COMPONENT gs_map-accno_colid OF STRUCTURE gs_excel_1 TO <f_acc>.
          lw_acc = <f_acc>.
        ENDIF.
        WRITE gs_map-row_start TO lw_line.
        lw_line = lw_line - 1.
        DELETE gt_excel FROM 1 TO lw_line.
      ELSE.
        WRITE gs_map-row_start TO lw_line.
        lw_line = lw_line - 1.
        DELETE gt_excel FROM 1 TO lw_line.
      ENDIF.
    ENDIF.
  ENDIF.
  LOOP AT gt_templ INTO DATA(gs_templ).
    CASE gs_templ-field_nm.
      WHEN 'LINO'.
        lw_1 = gs_templ-zcolumn.
      WHEN 'LOAI'.
        lw_2 = gs_templ-zcolumn.
      WHEN 'NGAYGIAODICH'.
        lw_3 = gs_templ-zcolumn.
      WHEN 'NGAYHACHTOAN'.
        lw_4 = gs_templ-zcolumn.
      WHEN 'SOTK'.
        lw_5 = gs_templ-zcolumn.
      WHEN 'TENTAIKHOAN'.
        lw_6 = gs_templ-zcolumn.
      WHEN 'NOIDUNG'.
        lw_7 = gs_templ-zcolumn.
      WHEN 'SODUDAU'.
        lw_8 = gs_templ-zcolumn.
      WHEN 'GHINO'.
        lw_9 = gs_templ-zcolumn.
      WHEN 'GHICO'.
        lw_10 = gs_templ-zcolumn.
      WHEN 'SODUCUOI'.
        lw_11 = gs_templ-zcolumn.
      WHEN 'MAKHACHHANG'.
        lw_12 = gs_templ-zcolumn..
      WHEN 'SOREF'.
        lw_13 = gs_templ-zcolumn.
      WHEN 'HTNOPTIEN'.
        lw_14 = gs_templ-zcolumn.
      WHEN 'SOTKNHANTIEN'.
        lw_15 = gs_templ-zcolumn.
      WHEN 'LOAIGIAODICH'.
        lw_16 = gs_templ-zcolumn.
    ENDCASE.
  ENDLOOP.
  LOOP AT gt_excel INTO DATA(gs_excel).
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_32 = lw_string.
    TRANSLATE lw_string TO UPPER CASE.
    gs_data-fico_id = lw_string.
    IF lw_1 = 0.
      CALL FUNCTION 'QF05_RANDOM_INTEGER'
        EXPORTING
          ran_int_max   = 999999
          ran_int_min   = 1
        IMPORTING
          ran_int       = gs_data-lino
        EXCEPTIONS
          invalid_input = 1
          OTHERS        = 2.
    ELSE.
      ASSIGN COMPONENT lw_1 OF STRUCTURE gs_excel TO <f1>.
      gs_data-lino = <f1>.
    ENDIF.
    IF lw_2 <> 0.
      ASSIGN COMPONENT lw_2 OF STRUCTURE gs_excel TO <f2>.
      gs_data-loai = <f2>.
    ENDIF.
    IF lw_5 = 0.
      gs_data-sotk = lw_acc.
    ELSE.
      ASSIGN COMPONENT lw_5 OF STRUCTURE gs_excel TO <f5>.
      gs_data-sotk = <f5>.
    ENDIF.
    IF gs_data-sotk IS INITIAL.
      gs_data-message = 'Đề nghị điền thông tin Số tài khoản'.
    ELSE.
      READ TABLE gt_t012k INTO DATA(gs_t012k) WITH KEY bankn = gs_data-sotk.
      IF sy-subrc IS NOT INITIAL.
        gs_data-message = |{ gs_data-message }, Số tài khoản không tồn tại trên hệ thống|.
      ENDIF.
    ENDIF.
    IF lw_6 <> 0.
      ASSIGN COMPONENT lw_6 OF STRUCTURE gs_excel TO <f6>.
      gs_data-tentaikhoan = <f6>.
    ENDIF.
    IF lw_7 <> 0.
      ASSIGN COMPONENT lw_7 OF STRUCTURE gs_excel TO <f7>.
      gs_data-noidung = <f7>.
    ENDIF.
    IF lw_8 <> 0.
      ASSIGN COMPONENT lw_8 OF STRUCTURE gs_excel TO <f8>.
      gs_data-sodudau = <f8>.
    ENDIF.
    IF lw_9 <> 0.
      ASSIGN COMPONENT lw_9 OF STRUCTURE gs_excel TO <f9>.
      gs_data-ghino = <f9>.
    ENDIF.
    IF lw_10 <> 0.
      ASSIGN COMPONENT lw_10 OF STRUCTURE gs_excel TO <f10>.
      gs_data-ghico = <f10>.
    ENDIF.
    IF gs_data-ghino IS INITIAL AND gs_data-ghico IS INITIAL.
      gs_data-message = |{ gs_data-message }, Đề nghị điền thông tin Ghi có or Ghi nợ|.
    ENDIF.
    IF lw_11 <> 0.
      ASSIGN COMPONENT lw_11 OF STRUCTURE gs_excel TO <f11>.
      gs_data-soducuoi = <f11>.
    ENDIF.
    IF lw_12 <> 0.
      ASSIGN COMPONENT lw_12 OF STRUCTURE gs_excel TO <f12>.
      gs_data-makhachhang = <f12>.
    ENDIF.
    IF lw_13 <> 0.
      ASSIGN COMPONENT lw_13 OF STRUCTURE gs_excel TO <f13>.
      gs_data-soref = <f13>.
      IF gs_data-soref IS INITIAL.
        gs_data-message = |{ gs_data-message }, Đề nghị điền thông tin soref |.
      ENDIF.
    ENDIF.
    IF gs_data-soref IS INITIAL.
      EXIT.
    ENDIF.
    IF lw_14 <> 0.
      ASSIGN COMPONENT lw_14 OF STRUCTURE gs_excel TO <f14>.
      gs_data-htnoptien = <f14>.
    ENDIF.
    IF lw_15 <> 0.
      ASSIGN COMPONENT lw_15 OF STRUCTURE gs_excel TO <f15>.
      gs_data-sotknhantien = <f15>.
    ENDIF.
    IF lw_16 <> 0.
      ASSIGN COMPONENT lw_16 OF STRUCTURE gs_excel TO <f16>.
      gs_data-loaigiaodich = <f16>.
    ENDIF.
    IF lw_3 <> 0.
      ASSIGN COMPONENT lw_3 OF STRUCTURE gs_excel TO <f3>.
      CONCATENATE <f3>+6(4) <f3>+3(2) <f3>+0(2) INTO gs_data-ngaygiaodich.
      IF gs_data-ngaygiaodich IS INITIAL.
        gs_data-message = |{ gs_data-message }, Đề nghị điền thông tin Ngày giao dịch |.
      ELSE.
        CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
          EXPORTING
            date                      = gs_data-ngaygiaodich
          EXCEPTIONS
            plausibility_check_failed = 1
            OTHERS                    = 2.
        IF sy-subrc <> 0.
          gs_data-message = |{ gs_data-message }, Ngày giao dịch Không đúng định dạng date |.
        ENDIF.
      ENDIF.
    ENDIF.
    IF lw_4 <> 0.
      ASSIGN COMPONENT lw_4 OF STRUCTURE gs_excel TO <f4>.
      CONCATENATE <f4>+6(4) <f4>+3(2) <f4>+0(2) INTO gs_data-ngayhachtoan.
      IF gs_data-ngayhachtoan IS NOT INITIAL.
        CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
          EXPORTING
            date                      = gs_data-ngayhachtoan
          EXCEPTIONS
            plausibility_check_failed = 1
            OTHERS                    = 2.
        IF sy-subrc <> 0.
          gs_data-message = |{ gs_data-message }, Ngày hạch toán Không đúng định dạng date |.
        ENDIF.
      ENDIF.
    ENDIF.
    gs_data-import = 'X'.
    IF gs_data-message IS INITIAL.
      gs_data-icon = '@08@'.
    ELSE.
      gs_data-icon = '@0A@'.
    ENDIF.
    APPEND gs_data TO gt_data.
    CLEAR: gs_data, lw_string.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data .
  PERFORM f_layout.
  PERFORM f_fieldcat.
  PERFORM f_display_alv.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_layout .
  x_layout-no_input          = 'X'.
*  x_layout-edit = 'X'.
  x_layout-zebra             = 'X'.
*  x_layout-colwidth_optimize = 'X'.
  x_layout-info_fieldname    = 'COLOR'.
*  x_layout-window_titlebar = gw_title.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_FIELDCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_fieldcat .
  DEFINE lm_fieldcat.
    t_fieldcat-fieldname   = &1.
    t_fieldcat-seltext_l   = &2.
    t_fieldcat-cfieldname = &3.
    t_fieldcat-qfieldname = &4.
    t_fieldcat-no_zero = &5.
    t_fieldcat-edit_mask = &6.
    t_fieldcat-edit = &7.
    t_fieldcat-ctabname = &8.
    t_fieldcat-decimals_out = &9.
*    t_fieldcat-cfieldname = &9.
*    t_fieldcat-ctabname = &11.
*    t_fieldcat-ref_tabname = 'GT_DATA'.
    APPEND t_fieldcat.
    CLEAR t_fieldcat.
  END-OF-DEFINITION.

  lm_fieldcat 'Icon' 'Status' '' '' 'X' '' '' '' ''.
  lm_fieldcat 'message' 'Message' '' '' 'X' '' '' '' '' .
  lm_fieldcat 'lino' 'Lino' '' '' 'X' '' '' '' '' .
  lm_fieldcat 'loai' 'Loại' '' '' 'X' '' 'X' '' '' .
  lm_fieldcat 'ngaygiaodich' 'Ngày giao dịch' '' '' '' '' 'X' '' '' .
  lm_fieldcat 'ngayhachtoan' 'Ngày hạch toán' '' '' 'X' '' 'X' '' '' .
  lm_fieldcat 'sotk' 'Số TK' '' 'BANKN' '' '' 'X' 'T012K' '' .
  lm_fieldcat 'tentaikhoan' 'Tên tài khoản' '' '' 'X' '' 'X' '' '' .
  lm_fieldcat 'noidung' 'Nội dung' '' '' 'X' '' 'X' '' '' .
  lm_fieldcat 'sodudau' 'Số dư đầu' '' '' 'X' '' 'X' '' '0' .
  lm_fieldcat 'ghino' 'Ghi nợ' '' '' 'X' '' 'X' '' '0' .
  lm_fieldcat 'ghico' 'Ghi có' '' '' 'X' '' 'X' '' '0' .
  lm_fieldcat 'soducuoi' 'Số dư cuối' '' '' 'X' '' 'X' '' '0' .
  lm_fieldcat 'makhachhang' 'Mã KH' '' '' 'X' '' 'X' '' '' .
  lm_fieldcat 'soref' 'Soref' '' '' 'X' '' 'X' '' '' .
  lm_fieldcat 'htnoptien' 'Hình thức nộp tiền' '' '' 'X' '' 'X' '' '' .
  lm_fieldcat 'sotknhantien' 'Số TK nhận tiền' '' '' 'X' '' 'X' '' '' .
  lm_fieldcat 'loaigiaodich' 'Loại giao dịch' '' '' 'X' '' 'X' '' '' .
  lm_fieldcat 'fico_id' 'Fico ID' '' '' 'X' '' '' '' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DATA
*&---------------------------------------------------------------------*
FORM f_display_alv .
  d_repid = sy-repid.
  gt_event = VALUE #( ( name = 'DATA_CHANGED' form = 'DATA_CHANGED') ( name = 'USER_COMMAND' form = 'USER_COMMAND') ).
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = d_repid
      is_layout                = x_layout
      it_fieldcat              = t_fieldcat[]
*     i_callback_top_of_page   = 'TOP-OF-PAGE'  "see FORM
      i_callback_pf_status_set = 'SET_STATUS'
      i_callback_user_command  = 'F_USER_COMMAND'
*     i_save                   = 'A'
      it_events                = gt_event
*     it_sort                  = t_sort[]
*     i_grid_title             = lv_header
    TABLES
      t_outtab                 = gt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*  DATA : ref_grid TYPE REF TO cl_gui_alv_grid. "new
*
*      IF ref_grid IS INITIAL.
*        CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*          IMPORTING
*            e_grid = ref_grid.
*      ENDIF.
*
*      IF NOT ref_grid IS INITIAL.
*        CALL METHOD ref_grid->check_changed_data.
*      ENDIF.
ENDFORM.
FORM data_changed ##called
     USING lrc_i_dc TYPE REF TO cl_alv_changed_data_protocol.
  DATA :lw_test TYPE string.
  lw_test = 'abv'.

ENDFORM.
FORM user_command USING r_ucomm LIKE sy-ucomm

rs_selfield TYPE slis_selfield.
  DATA :lw_test TYPE string.
  lw_test = 'abv'.
  DATA:

lv_ref_grid TYPE REF TO cl_gui_alv_grid.
  IF lv_ref_grid IS INITIAL.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = lv_ref_grid.

  ENDIF.

  IF NOT lv_ref_grid IS INITIAL.

    CALL METHOD lv_ref_grid->check_changed_data.

  ENDIF.
ENDFORM.

FORM set_status USING  extab TYPE slis_t_extab.             "#EC CALLED
  SET PF-STATUS 'ZSTATUS1' EXCLUDING extab.
ENDFORM.

FORM f_user_command USING r_ucomm LIKE sy-ucomm
                  rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN 'UPLOAD'.
      PERFORM upload_file.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPLOAD_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM upload_file .
  DATA(gt_check) = gt_data[].
  DELETE gt_check WHERE message IS INITIAL.
  IF gt_check IS NOT INITIAL.
    MESSAGE 'Kiểm tra lại thông tin ' TYPE 'I'.
  ELSE.
    gt_data_import = CORRESPONDING #( gt_data ).
    INSERT ztb_api010_input FROM TABLE gt_data_import.
    MESSAGE 'Thành công! ' TYPE 'I'.
  ENDIF.
ENDFORM.
