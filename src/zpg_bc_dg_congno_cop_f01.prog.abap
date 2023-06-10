
*&---------------------------------------------------------------------*
*& Include          ZPG_BC_DG_CONGNO_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form MAIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM main .
  PERFORM set_data_main.
*  IF sy-tcode = 'ZFIR048'.
  PERFORM display_data_0100.
*  ELSEIF sy-tcode = 'ZFIR048_01'.
*    PERFORM display_data_0300.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_DATA_MAIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_add_toolbar_alv_0100 .
    DATA: ls_toolbar TYPE stb_button.
    MOVE 'IMPORT' TO ls_toolbar-function.
    MOVE icon_next_object TO ls_toolbar-icon.
    MOVE 'Import' TO ls_toolbar-text.
    MOVE 'Import' TO ls_toolbar-quickinfo.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.
    CLEAR ls_toolbar.
    MOVE 'DOWN_TEMP' TO ls_toolbar-function.
    MOVE icon_trend_down TO ls_toolbar-icon.
    MOVE 'Download template' TO ls_toolbar-text.
    MOVE 'Download template' TO ls_toolbar-quickinfo.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.
  METHOD handle_add_toolbar_alv_0200 .
    DATA: ls_toolbar TYPE stb_button.
    MOVE 'SEND_APPROVE' TO ls_toolbar-function.
    MOVE icon_flight TO ls_toolbar-icon.
    MOVE 'Gửi phê duyệt' TO ls_toolbar-text.
    MOVE 'Gửi phê duyệt' TO ls_toolbar-quickinfo.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.
  METHOD handle_add_toolbar_alv_0300.
    DATA: ls_toolbar TYPE stb_button.
    MOVE 'SELECT_ALL' TO ls_toolbar-function.
    MOVE icon_select_all TO ls_toolbar-icon.
    MOVE 'Select all' TO ls_toolbar-text.
    MOVE 'Select all' TO ls_toolbar-quickinfo.
    APPEND ls_toolbar TO e_object->mt_toolbar.
    CLEAR ls_toolbar.
    MOVE 'DESELECT_ALL' TO ls_toolbar-function.
    MOVE icon_deselect_all TO ls_toolbar-icon.
    MOVE 'Deselect all' TO ls_toolbar-text.
    MOVE 'Deselect all' TO ls_toolbar-quickinfo.
    APPEND ls_toolbar TO e_object->mt_toolbar.
    CLEAR ls_toolbar.

    MOVE 'APPROVE' TO ls_toolbar-function.
    MOVE icon_system_okay TO ls_toolbar-icon.
    MOVE 'Duyệt' TO ls_toolbar-text.
    MOVE 'Duyệt' TO ls_toolbar-quickinfo.
    IF p_stt = 1 OR  p_stt = 2.
      MOVE 'X' TO ls_toolbar-disabled.
    ENDIF.
    APPEND ls_toolbar TO e_object->mt_toolbar.
    CLEAR ls_toolbar.
    MOVE 'CANCEL' TO ls_toolbar-function.
    MOVE icon_system_cancel TO ls_toolbar-icon.
    MOVE 'Từ chối' TO ls_toolbar-text.
    MOVE 'Từ chối' TO ls_toolbar-quickinfo.
    IF p_stt = 1 OR  p_stt = 2.
      MOVE 'X' TO ls_toolbar-disabled.
    ENDIF.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.
  METHOD handle_set_title.
    DATA : lw_text TYPE sdydo_text_element.
    g_doc_header->add_text( text  = 'BÁO CÁO KẾ HOẠCH THU CÔNG NỢ THÁNG N' sap_style = cl_dd_area=>heading ).
    g_doc_header->new_line( ).
    g_doc_header->new_line( ).
*    IF p_budat IS NOT INITIAL.
    g_doc_header->new_line( ).
    g_doc_header->new_line( ).
    WRITE gw_budat TO lw_text DD/MM/YYYY.
    g_doc_header->add_text( text  = 'Posting date :' sap_emphasis = cl_dd_area=>strong ).
    g_doc_header->add_text( text  = lw_text sap_emphasis = cl_dd_area=>sans_serif ).
    g_doc_header->add_gap( ).
*    ENDIF.
    IF p_stt IS NOT INITIAL.
      CLEAR lw_text.
      CASE p_stt.
        WHEN 0.
          lw_text = '0 - Chưa duyệt'.
        WHEN 1.
          lw_text = '1 - Đã duyệt'.
        WHEN 2.
          lw_text = '2 - Từ chối duyệt'.
      ENDCASE.
      g_doc_header->new_line( ).
      g_doc_header->new_line( ).
      g_doc_header->add_text( text  = 'Status :' sap_emphasis = cl_dd_area=>strong ).
      g_doc_header->add_text( text  = lw_text sap_emphasis = cl_dd_area=>sans_serif ).
    ENDIF.
    g_doc_header->display_document(  parent = g_container_header ).

  ENDMETHOD.

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'DOWN_TEMP'.
        PERFORM download_temp.
      WHEN 'IMPORT'.
        PERFORM process_import_data.
      WHEN 'SEND_APPROVE'.
        PERFORM send_approve.
      WHEN 'SELECT_ALL'.
        PERFORM select_all.
      WHEN 'DESELECT_ALL'.
        PERFORM deselect_all.
      WHEN 'APPROVE'.
        PERFORM approve.
      WHEN 'CANCEL'.
        PERFORM cancel.
    ENDCASE.
*    IF e_ucomm = 'IMPORT'.
*      PERFORM import.
*    ENDIF.
  ENDMETHOD.


ENDCLASS.
FORM set_data_main .
  p_month = |{ p_month ALPHA = IN }|.
  gw_budat = |{ p_year }{ p_month }01|.
  DEFINE calc_date_in_interval.
    TRY.
*        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*          EXPORTING
*            date = &1
*            days = 0
*            months = &2
*            signum = '-'
*            years = 0
*          IMPORTING
*            calc_date = &3.
    CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
      EXPORTING
        iv_date             = &1
      IMPORTING
        ev_month_begin_date = &2.
    &2 = &2 - 1.
    CATCH cx_root.

    ENDTRY.
  END-OF-DEFINITION.

*  DEFINE set_amount.
*    IF &1 > &2 AND &1 <= &3.
*      &4 = &5.
*    ENDIF.
*  END-OF-DEFINITION.
  PERFORM get_fields_struct.

  DATA lt_coll_data TYPE TABLE OF zst_fir045.
  DATA: faede TYPE faede.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date             = gw_budat
    IMPORTING
      ev_month_begin_date = date.
  due_date = date - 1.

  calc_date_in_interval due_date due_date_n1.
  calc_date_in_interval due_date_n1 due_date_n2.
  calc_date_in_interval due_date_n2 due_date_n3.
  calc_date_in_interval due_date_n3 due_date_n4.
  calc_date_in_interval due_date_n4 due_date_n5.
  calc_date_in_interval due_date_n5 due_date_n6.
  calc_date_in_interval due_date_n6 due_date_n7.

  SELECT zev_cn,prctr,pstcode,ten_cn
  FROM zev_cn
  INTO CORRESPONDING FIELDS OF TABLE @gt_cn.

  SORT gt_cn BY prctr.
  DELETE ADJACENT DUPLICATES FROM gt_cn COMPARING prctr.
  SORT gt_cn BY prctr.
  SELECT * FROM z_test_cdsv3(  p_bukrs = @p_bukrs , p_budat = @gw_budat,
                               p_due_date    = @due_date,
                               p_due_date_n1 = @due_date_n1,
                               p_due_date_n2 = @due_date_n2,
                               p_due_date_n3 = @due_date_n3,
                               p_due_date_n4 = @due_date_n4,
                               p_due_date_n5 = @due_date_n5,
                               p_due_date_n6 = @due_date_n6,
                               p_due_date_n7 = @due_date_n7
                             )
    INTO CORRESPONDING FIELDS OF TABLE @gt_acdoca
    WHERE prctr IN @s_prctr AND kunnr IN @s_kunnr AND racct IN  @s_racct AND segment IN  @s_segm.

*  SORT gt_acdoca BY prctr kunnr racct belnr .
  IF p_stt IS NOT INITIAL.
    SELECT * FROM zdn_congno INTO CORRESPONDING FIELDS OF TABLE @gt_cong_no WHERE trang_thai = @p_stt.
    CASE p_stt.
      WHEN 1 OR 2.
        SORT gt_cong_no BY prctr DESCENDING segment DESCENDING kunnr DESCENDING ngay_duyet DESCENDING tgian_duyet.
      WHEN 0.
        SORT gt_cong_no BY prctr DESCENDING segment DESCENDING kunnr DESCENDING ngay_yc DESCENDING tgian_tao.
    ENDCASE.

    DELETE ADJACENT DUPLICATES FROM gt_cong_no COMPARING prctr segment kunnr.
    SORT gt_cong_no BY prctr segment kunnr.
    LOOP AT gt_acdoca INTO DATA(ls_acdoca).
      READ TABLE gt_cong_no WITH KEY prctr = ls_acdoca-prctr segment = ls_acdoca-segment kunnr = ls_acdoca-kunnr TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc NE 0.
        DELETE gt_acdoca.
      ENDIF.
    ENDLOOP.
  ELSE.
    IF sy-tcode = 'ZFIR048'.
      SELECT * FROM zdn_congno INTO CORRESPONDING FIELDS OF TABLE @gt_cong_no WHERE trang_thai = '1'.
      SORT gt_cong_no BY prctr DESCENDING segment DESCENDING kunnr DESCENDING ngay_duyet DESCENDING tgian_duyet.
      DELETE ADJACENT DUPLICATES FROM gt_cong_no COMPARING prctr segment kunnr.
      SORT gt_cong_no BY prctr segment kunnr.
    ENDIF.
  ENDIF.

*  IF gt_acdoca IS NOT INITIAL.
*    "get data zev_cn
*    SELECT zev_cn,prctr,pstcode,ten_cn
*    FROM zev_cn
*    INTO CORRESPONDING FIELDS OF TABLE @gt_cn.
**    FOR ALL ENTRIES IN @gt_acdoca
**    WHERE prctr = @gt_acdoca-segment.
*    SORT gt_cn BY prctr.
*    DELETE gt_cn WHERE prctr = ''.
*    "get data knb1
*    SELECT kunnr,bukrs,zterm
*    FROM knb1
*    INTO CORRESPONDING FIELDS OF TABLE @gt_knb1
*    FOR ALL ENTRIES IN @gt_acdoca
*    WHERE kunnr = @gt_acdoca-kunnr.
*
**    SORT gt_knb1 BY kunnr.
*  ENDIF.

  "buid data for main
*  PERFORM buid_data_parallel.
*
*  DELETE gt_data1 WHERE cpn_n1 IS INITIAL AND cpn_n2 IS INITIAL AND cpn_n3 IS INITIAL AND cpn_n6 IS INITIAL AND
*                       vpp_n1 IS INITIAL AND vpp_n2 IS INITIAL AND vpp_n3 IS INITIAL AND vpp_n6 IS INITIAL AND
*                       voso_n1 IS INITIAL AND voso_n2 IS INITIAL AND voso_n3 IS INITIAL AND voso_n6 IS INITIAL AND
*                       vtsale_n1 IS INITIAL AND vtsale_n2 IS INITIAL AND vtsale_n3 IS INITIAL AND vtsale_n6 IS INITIAL AND
*                       log_n1 IS INITIAL AND log_n2 IS INITIAL AND log_n3 IS INITIAL AND log_n6 IS INITIAL.
  gt_data = CORRESPONDING #( gt_acdoca ).
  DELETE gt_data WHERE cpn_n1 IS INITIAL AND cpn_n2 IS INITIAL AND cpn_n3 IS INITIAL AND cpn_n6 IS INITIAL AND
                     vpp_n1 IS INITIAL AND vpp_n2 IS INITIAL AND vpp_n3 IS INITIAL AND vpp_n6 IS INITIAL AND
                     voso_n1 IS INITIAL AND voso_n2 IS INITIAL AND voso_n3 IS INITIAL AND voso_n6 IS INITIAL AND
                     vtsale_n1 IS INITIAL AND vtsale_n2 IS INITIAL AND vtsale_n3 IS INITIAL AND vtsale_n6 IS INITIAL AND
                     log_n1 IS INITIAL AND log_n2 IS INITIAL AND log_n3 IS INITIAL AND log_n6 IS INITIAL.

  LOOP at gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
    READ TABLE gt_cn INTO DATA(ls_cn) WITH KEY prctr = <fs_data>-segment BINARY SEARCH.
    if sy-subrc = 0.
      <fs_data>-ten_cn = ls_cn-ten_cn.
      <fs_data>-pstcode = ls_cn-pstcode.
    endif.
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
FORM display_data_0100.
  IF p_stt = 1 OR p_stt IS INITIAL. "trang thai da phe duyet thi hien thi so tien con lai
    PERFORM convert_amount_tab.
    DATA :lt_cong_no_tmp TYPE TABLE OF gty_fir048.
    DATA :ls_cell_color TYPE lvc_s_scol.
    lt_cong_no_tmp = CORRESPONDING #( gt_cong_no ).
    SORT lt_cong_no_tmp BY prctr segment kunnr.
    PERFORM update_amount USING 'X' CHANGING lt_cong_no_tmp.
    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
      READ TABLE lt_cong_no_tmp WITH KEY prctr = <fs_data>-prctr segment = <fs_data>-segment kunnr = <fs_data>-kunnr INTO DATA(ls_cong_no_tmp) BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT gt_dfies_tab INTO DATA(ls_dfies_tab) WHERE rollname = 'FINS_VHCUR12'.
          ASSIGN COMPONENT ls_dfies_tab-fieldname OF STRUCTURE ls_cong_no_tmp TO FIELD-SYMBOL(<fs_data1>).
          IF <fs_data1> <> '0.00'.
            ls_cell_color-fname = ls_dfies_tab-fieldname.
            ls_cell_color-color-col = 7.
            ls_cell_color-color-int = '1'.
            ls_cell_color-color-inv = '0'.
            APPEND ls_cell_color TO <fs_data>-cell_color[].
            CLEAR ls_cell_color.
          ENDIF.
        ENDLOOP.
        PERFORM substract_amount USING ls_cong_no_tmp CHANGING <fs_data>.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF gt_data IS NOT INITIAL.
    CALL SCREEN 0100.
  ELSE.
    MESSAGE 'No data !' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data_0300.
  PERFORM buid_data_0300.
  IF gt_data_import_valid IS NOT INITIAL.
    CALL SCREEN 0300.
  ELSE.
    MESSAGE 'No data !' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SETUP_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> G_CONT_0100
*&      --> G_ALV_0100
*&      --> GT_DATA
*&---------------------------------------------------------------------*
FORM setup_alv USING  lpw_cont_name TYPE char10
                      lpw_struc_name TYPE char30
                      lpw_cont TYPE REF TO cl_gui_custom_container
*                      lpw_alv TYPE REF TO cl_gui_alv_grid
                      lpt_data TYPE ANY TABLE.
  DATA: lt_fcat TYPE lvc_t_fcat.
  PERFORM get_container_common USING lpw_cont_name lpw_struc_name CHANGING lpw_cont lt_fcat.
  PERFORM get_fcat USING lpw_cont_name lpw_struc_name CHANGING lt_fcat.
  PERFORM set_alv_common USING lpw_cont_name lpw_struc_name lpt_data lt_fcat lpw_cont.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CONTAINER_COMMON
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LPW_CONT_NAME
*&      --> LPW_STRUC_NAME
*&      <-- LPW_CONT
*&      <-- LT_FCAT
*&---------------------------------------------------------------------*
FORM get_container_common  USING p_container TYPE char10
                                 p_structure_name TYPE char30
                           CHANGING
                                 c_cont TYPE REF TO cl_gui_custom_container
                                 c_fcat TYPE lvc_t_fcat.
  IF c_cont IS BOUND.
    c_cont->free( ).
  ENDIF.
  c_cont = NEW cl_gui_custom_container( container_name = p_container ).
  IF p_structure_name IS NOT INITIAL.
*   Build field category
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = p_structure_name
      CHANGING
        ct_fieldcat            = c_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      STOP.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_FCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LPW_CONT_NAME
*&      --> LPW_STRUC_NAME
*&      <-- LT_FCAT
*&---------------------------------------------------------------------*
FORM get_fcat  USING lpw_cont_name TYPE char10
                     lpw_structure_name TYPE char30
                CHANGING ct_fcat TYPE lvc_t_fcat.
  CASE lpw_cont_name.
    WHEN 'ALV_0100'.
      PERFORM build_fcat_0100 CHANGING ct_fcat.
    WHEN 'ALV_0200'.
      PERFORM build_fcat_0200 CHANGING ct_fcat.
    WHEN 'ALV_0300'.
      PERFORM build_fcat_0300 CHANGING ct_fcat.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_ALV_COMMON
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LPW_CONT_NAME
*&      --> LPW_STRUC_NAME
*&      --> LPT_DATA
*&      --> LPW_ALV
*&      --> LT_FCAT
*&      --> LPW_CONT
*&---------------------------------------------------------------------*
FORM set_alv_common  USING p_container TYPE char10
                           p_structure_name TYPE char30
                           p_outtab TYPE ANY TABLE
*                           p_alv TYPE REF TO cl_gui_alv_grid
                           pt_fcat TYPE lvc_t_fcat
                           p_cont TYPE REF TO cl_gui_custom_container.
  FREE :g_alv,g_container_header,g_doc_header.
  DATA:ls_layout_src TYPE lvc_s_layo,
       ls_layout     TYPE disvariant.
  DATA :lt_exclude TYPE ui_functions.
  DATA :lo_cont_main TYPE REF TO cl_gui_container.
  DATA(lo_split) = NEW cl_gui_splitter_container( parent  = p_cont rows = 2 columns = 1 ).
  g_container_header = lo_split->get_container( row = 1 column = 1 ).
  g_doc_header = NEW cl_dd_document( style = 'ALV_GRID' ).
  lo_split->set_row_height( id = 1  height = 16 ).
  lo_cont_main = lo_split->get_container( row = 2 column = 1 ).
  g_alv = NEW cl_gui_alv_grid( i_parent = lo_cont_main ).
  DATA(lo_handler) = NEW lcl_event_handler( ).

  IF p_container = 'ALV_0100' .
    SET HANDLER lo_handler->handle_add_toolbar_alv_0100 FOR g_alv.
  ELSEIF p_container = 'ALV_0200' .
    SET HANDLER lo_handler->handle_add_toolbar_alv_0200 FOR g_alv.
  ELSEIF p_container = 'ALV_0300' .
    SET HANDLER lo_handler->handle_add_toolbar_alv_0300 FOR g_alv.
  ENDIF.
  SET HANDLER lo_handler->handle_user_command FOR g_alv.
  SET HANDLER lo_handler->handle_set_title FOR g_alv.

  " Set table for first display
  ls_layout-report = sy-repid.
  ls_layout_src-cwidth_opt = 'X'.
  ls_layout_src-info_fname = 'LINE_COLOR'.
  ls_layout_src-stylefname = 'CELL_STYLE'.
  ls_layout_src-ctab_fname = 'CELL_COLOR'.
  ls_layout_src-sel_mode = 'A'.

  PERFORM exclude_tb_functions CHANGING lt_exclude.

  CALL METHOD g_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = ls_layout_src
      is_variant                    = ls_layout
      i_save                        = 'X'
      i_structure_name              = p_structure_name
      it_toolbar_excluding          = lt_exclude
    CHANGING
      it_outtab                     = p_outtab
      it_fieldcatalog               = pt_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc IS NOT INITIAL.
*        Display system error messgae
    MESSAGE ID   sy-msgid
            TYPE 'E'
            NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  g_alv->list_processing_events(  i_event_name = 'TOP_OF_PAGE' i_dyndoc_id  = g_doc_header ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_FCAT_0100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LPW_CONT_NAME
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM build_fcat_0100 CHANGING ct_fcat TYPE lvc_t_fcat.
  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<lf_fcat>).
    CASE <lf_fcat>-fieldname.
      WHEN 'PRCTR'.
        <lf_fcat>-tooltip = 'PC'.
        <lf_fcat>-coltext = 'PC'.
        <lf_fcat>-col_pos = 1.
      WHEN 'SEGMENT'.
        <lf_fcat>-tooltip = 'Segment'.
        <lf_fcat>-coltext = 'Segment'.
        <lf_fcat>-col_pos = 2.
      WHEN 'PSTCODE'.
        <lf_fcat>-tooltip = 'Mã CN'.
        <lf_fcat>-coltext = 'Mã CN'.
        <lf_fcat>-col_pos = 3.
      WHEN 'TEN_CN'.
        <lf_fcat>-tooltip = 'Tên CN'.
        <lf_fcat>-coltext = 'Tên CN'.
        <lf_fcat>-col_pos = 4.
      WHEN 'KUNNR'.
        <lf_fcat>-tooltip = 'CusID'.
        <lf_fcat>-coltext = 'CusID'.
        <lf_fcat>-col_pos = 5.
      WHEN 'CPN_N1'.
        <lf_fcat>-tooltip = 'CPN - Trong hạn'.
        <lf_fcat>-coltext = 'CPN - Trong hạn'.
        <lf_fcat>-col_pos = 6.
      WHEN 'CPN_N2'.
        <lf_fcat>-tooltip = 'CPN - Quá hạn'.
        <lf_fcat>-coltext = 'CPN - Quá hạn'.
        <lf_fcat>-col_pos = 7.
      WHEN 'CPN_N3'.
        <lf_fcat>-tooltip = 'CPN - Quá hạn 3 tháng'.
        <lf_fcat>-coltext = 'CPN - Quá hạn 3 tháng'.
        <lf_fcat>-col_pos = 8.
      WHEN 'CPN_N6'.
        <lf_fcat>-tooltip = 'CPN - Quá hạn 6 tháng'.
        <lf_fcat>-coltext = 'CPN - Quá hạn 6 tháng'.
        <lf_fcat>-col_pos = 9.
      WHEN 'VPP_N1'.
        <lf_fcat>-tooltip = 'VPP - Trong hạn'.
        <lf_fcat>-coltext = 'VPP - Trong hạn'.
        <lf_fcat>-col_pos = 10.
      WHEN 'VPP_N2'.
        <lf_fcat>-tooltip = 'VPP - Quá hạn'.
        <lf_fcat>-coltext = 'VPP - Quá hạn'.
        <lf_fcat>-col_pos = 11.
      WHEN 'VPP_N3'.
        <lf_fcat>-tooltip = 'VPP - Quá hạn 3 tháng'.
        <lf_fcat>-coltext = 'VPP - Quá hạn 3 tháng'.
        <lf_fcat>-col_pos = 12.
      WHEN 'VPP_N6'.
        <lf_fcat>-tooltip = 'VPP - Quá hạn 6 tháng'.
        <lf_fcat>-coltext = 'VPP - Quá hạn 6 tháng'.
        <lf_fcat>-col_pos = 13.
      WHEN 'VOSO_N1'.
        <lf_fcat>-tooltip = 'VOSO - Trong hạn'.
        <lf_fcat>-coltext = 'VOSO - Trong hạn'.
        <lf_fcat>-col_pos = 14.
      WHEN 'VOSO_N2'.
        <lf_fcat>-tooltip = 'VOSO - Quá hạn'.
        <lf_fcat>-coltext = 'VOSO - Quá hạn'.
        <lf_fcat>-col_pos = 15.
      WHEN 'VOSO_N3'.
        <lf_fcat>-tooltip = 'VOSO - Quá hạn 3 tháng'.
        <lf_fcat>-coltext = 'VOSO - Quá hạn 3 tháng'.
        <lf_fcat>-col_pos = 16.
      WHEN 'VOSO_N6'.
        <lf_fcat>-tooltip = 'VOSO - Quá hạn 6 tháng'.
        <lf_fcat>-coltext = 'VOSO - Quá hạn 6 tháng'.
        <lf_fcat>-col_pos = 17.
      WHEN 'VTSALE_N1'.
        <lf_fcat>-tooltip = 'VTSALE - Trong hạn'.
        <lf_fcat>-coltext = 'VTSALE - Trong hạn'.
        <lf_fcat>-col_pos = 18.
      WHEN 'VTSALE_N2'.
        <lf_fcat>-tooltip = 'VTSALE - Quá hạn'.
        <lf_fcat>-coltext = 'VTSALE - Quá hạn'.
        <lf_fcat>-col_pos = 19.
      WHEN 'VTSALE_N3'.
        <lf_fcat>-tooltip = 'VTSALE - Quá hạn 3 tháng'.
        <lf_fcat>-coltext = 'VTSALE - Quá hạn 3 tháng'.
        <lf_fcat>-col_pos = 20.
      WHEN 'VTSALE_N6'.
        <lf_fcat>-tooltip = 'VTSALE - Quá hạn 6 tháng'.
        <lf_fcat>-coltext = 'VTSALE - Quá hạn 6 tháng'.
        <lf_fcat>-col_pos = 21.
      WHEN 'LOG_N1'.
        <lf_fcat>-tooltip = 'LOG - Trong hạn'.
        <lf_fcat>-coltext = 'LOG - Trong hạn'.
        <lf_fcat>-col_pos = 22.
      WHEN 'LOG_N2'.
        <lf_fcat>-tooltip = 'LOG - Quá hạn'.
        <lf_fcat>-coltext = 'LOG - Quá hạn'.
        <lf_fcat>-col_pos = 23.
      WHEN 'LOG_N3'.
        <lf_fcat>-tooltip = 'LOG - Quá hạn 3 tháng'.
        <lf_fcat>-coltext = 'LOG - Quá hạn 3 tháng'.
        <lf_fcat>-col_pos = 24.
      WHEN 'LOG_N6'.
        <lf_fcat>-tooltip = 'LOG - Quá hạn 6 tháng'.
        <lf_fcat>-coltext = 'LOG - Quá hạn 6 tháng'.
        <lf_fcat>-col_pos = 25.
      WHEN OTHERS.
        <lf_fcat>-tech = 'X'.
        <lf_fcat>-no_out = 'X'.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_FCAT_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM build_fcat_0200  CHANGING ct_fcat TYPE lvc_t_fcat.
  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<lf_fcat>).
    CASE <lf_fcat>-fieldname.
      WHEN 'PRCTR'.
        <lf_fcat>-tooltip = 'PC'.
        <lf_fcat>-coltext = 'PC'.
        <lf_fcat>-col_pos = 1.
      WHEN 'SEGMENT'.
        <lf_fcat>-tooltip = 'Segment'.
        <lf_fcat>-coltext = 'Segment'.
        <lf_fcat>-col_pos = 2.
      WHEN 'PSTCODE'.
        <lf_fcat>-tooltip = 'Mã CN'.
        <lf_fcat>-coltext = 'Mã CN'.
        <lf_fcat>-col_pos = 3.
      WHEN 'TEN_CN'.
        <lf_fcat>-tooltip = 'Tên CN'.
        <lf_fcat>-coltext = 'Tên CN'.
        <lf_fcat>-col_pos = 4.
      WHEN 'KUNNR'.
        <lf_fcat>-tooltip = 'CusID'.
        <lf_fcat>-coltext = 'CusID'.
        <lf_fcat>-col_pos = 5.
      WHEN 'CPN_N1'.
        <lf_fcat>-tooltip = 'CPN - Trong hạn'.
        <lf_fcat>-coltext = 'CPN - Trong hạn'.
        <lf_fcat>-col_pos = 6.
      WHEN 'CPN_N2'.
        <lf_fcat>-tooltip = 'CPN - Quá hạn'.
        <lf_fcat>-coltext = 'CPN - Quá hạn'.
        <lf_fcat>-col_pos = 7.
      WHEN 'CPN_N3'.
        <lf_fcat>-tooltip = 'CPN - Quá hạn 3 tháng'.
        <lf_fcat>-coltext = 'CPN - Quá hạn 3 tháng'.
        <lf_fcat>-col_pos = 8.
      WHEN 'CPN_N6'.
        <lf_fcat>-tooltip = 'CPN - Quá hạn 6 tháng'.
        <lf_fcat>-coltext = 'CPN - Quá hạn 6 tháng'.
        <lf_fcat>-col_pos = 9.
      WHEN 'VPP_N1'.
        <lf_fcat>-tooltip = 'VPP - Trong hạn'.
        <lf_fcat>-coltext = 'VPP - Trong hạn'.
        <lf_fcat>-col_pos = 10.
      WHEN 'VPP_N2'.
        <lf_fcat>-tooltip = 'VPP - Quá hạn'.
        <lf_fcat>-coltext = 'VPP - Quá hạn'.
        <lf_fcat>-col_pos = 11.
      WHEN 'VPP_N3'.
        <lf_fcat>-tooltip = 'VPP - Quá hạn 3 tháng'.
        <lf_fcat>-coltext = 'VPP - Quá hạn 3 tháng'.
        <lf_fcat>-col_pos = 12.
      WHEN 'VPP_N6'.
        <lf_fcat>-tooltip = 'VPP - Quá hạn 6 tháng'.
        <lf_fcat>-coltext = 'VPP - Quá hạn 6 tháng'.
        <lf_fcat>-col_pos = 13.
      WHEN 'VOSO_N1'.
        <lf_fcat>-tooltip = 'VOSO - Trong hạn'.
        <lf_fcat>-coltext = 'VOSO - Trong hạn'.
        <lf_fcat>-col_pos = 14.
      WHEN 'VOSO_N2'.
        <lf_fcat>-tooltip = 'VOSO - Quá hạn'.
        <lf_fcat>-coltext = 'VOSO - Quá hạn'.
        <lf_fcat>-col_pos = 15.
      WHEN 'VOSO_N3'.
        <lf_fcat>-tooltip = 'VOSO - Quá hạn 3 tháng'.
        <lf_fcat>-coltext = 'VOSO - Quá hạn 3 tháng'.
        <lf_fcat>-col_pos = 16.
      WHEN 'VOSO_N6'.
        <lf_fcat>-tooltip = 'VOSO - Quá hạn 6 tháng'.
        <lf_fcat>-coltext = 'VOSO - Quá hạn 6 tháng'.
        <lf_fcat>-col_pos = 17.
      WHEN 'VTSALE_N1'.
        <lf_fcat>-tooltip = 'VTSALE - Trong hạn'.
        <lf_fcat>-coltext = 'VTSALE - Trong hạn'.
        <lf_fcat>-col_pos = 18.
      WHEN 'VTSALE_N2'.
        <lf_fcat>-tooltip = 'VTSALE - Quá hạn'.
        <lf_fcat>-coltext = 'VTSALE - Quá hạn'.
        <lf_fcat>-col_pos = 19.
      WHEN 'VTSALE_N3'.
        <lf_fcat>-tooltip = 'VTSALE - Quá hạn 3 tháng'.
        <lf_fcat>-coltext = 'VTSALE - Quá hạn 3 tháng'.
        <lf_fcat>-col_pos = 20.
      WHEN 'VTSALE_N6'.
        <lf_fcat>-tooltip = 'VTSALE - Quá hạn 6 tháng'.
        <lf_fcat>-coltext = 'VTSALE - Quá hạn 6 tháng'.
        <lf_fcat>-col_pos = 21.
      WHEN 'LOG_N1'.
        <lf_fcat>-tooltip = 'LOG - Trong hạn'.
        <lf_fcat>-coltext = 'LOG - Trong hạn'.
        <lf_fcat>-col_pos = 22.
      WHEN 'LOG_N2'.
        <lf_fcat>-tooltip = 'LOG - Quá hạn'.
        <lf_fcat>-coltext = 'LOG - Quá hạn'.
        <lf_fcat>-col_pos = 23.
      WHEN 'LOG_N3'.
        <lf_fcat>-tooltip = 'LOG - Quá hạn 3 tháng'.
        <lf_fcat>-coltext = 'LOG - Quá hạn 3 tháng'.
        <lf_fcat>-col_pos = 24.
      WHEN 'LOG_N6'.
        <lf_fcat>-tooltip = 'LOG - Quá hạn 6 tháng'.
        <lf_fcat>-coltext = 'LOG - Quá hạn 6 tháng'.
        <lf_fcat>-col_pos = 25.
      WHEN 'THOI_HAN'.
        <lf_fcat>-tooltip = 'Thời hạn cam kết TT'.
        <lf_fcat>-coltext = 'Thời hạn cam kết TT'.
        <lf_fcat>-col_pos = 26.
      WHEN OTHERS.
        <lf_fcat>-tech = 'X'.
        <lf_fcat>-no_out = 'X'.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_FCAT_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM build_fcat_0300  CHANGING ct_fcat TYPE lvc_t_fcat.
  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<lf_fcat>).
    CASE <lf_fcat>-fieldname.
      WHEN 'FLAG'.
        <lf_fcat>-tooltip = 'Select'.
        <lf_fcat>-coltext = 'Select'.
        <lf_fcat>-col_pos = 1.
        <lf_fcat>-checkbox = 'X'.
        <lf_fcat>-edit = 'X'.
      WHEN 'PRCTR'.
        <lf_fcat>-tooltip = 'PC'.
        <lf_fcat>-coltext = 'PC'.
        <lf_fcat>-col_pos = 2.
      WHEN 'SEGMENT'.
        <lf_fcat>-tooltip = 'Segment'.
        <lf_fcat>-coltext = 'Segment'.
        <lf_fcat>-col_pos = 3.
      WHEN 'PSTCODE'.
        <lf_fcat>-tooltip = 'Mã CN'.
        <lf_fcat>-coltext = 'Mã CN'.
        <lf_fcat>-col_pos = 4.
      WHEN 'TEN_CN'.
        <lf_fcat>-tooltip = 'Tên CN'.
        <lf_fcat>-coltext = 'Tên CN'.
        <lf_fcat>-col_pos = 5.
      WHEN 'KUNNR'.
        <lf_fcat>-tooltip = 'CusID'.
        <lf_fcat>-coltext = 'CusID'.
        <lf_fcat>-col_pos = 6.
      WHEN 'CPN_N1'.
        <lf_fcat>-tooltip = 'CPN - Trong hạn'.
        <lf_fcat>-coltext = 'CPN - Trong hạn'.
        <lf_fcat>-col_pos = 7.
      WHEN 'CPN_N2'.
        <lf_fcat>-tooltip = 'CPN - Quá hạn'.
        <lf_fcat>-coltext = 'CPN - Quá hạn'.
        <lf_fcat>-col_pos = 8.
      WHEN 'CPN_N3'.
        <lf_fcat>-tooltip = 'CPN - Quá hạn 3 tháng'.
        <lf_fcat>-coltext = 'CPN - Quá hạn 3 tháng'.
        <lf_fcat>-col_pos = 9.
      WHEN 'CPN_N6'.
        <lf_fcat>-tooltip = 'CPN - Quá hạn 6 tháng'.
        <lf_fcat>-coltext = 'CPN - Quá hạn 6 tháng'.
        <lf_fcat>-col_pos = 10.
      WHEN 'VPP_N1'.
        <lf_fcat>-tooltip = 'VPP - Trong hạn'.
        <lf_fcat>-coltext = 'VPP - Trong hạn'.
        <lf_fcat>-col_pos = 11.
      WHEN 'VPP_N2'.
        <lf_fcat>-tooltip = 'VPP - Quá hạn'.
        <lf_fcat>-coltext = 'VPP - Quá hạn'.
        <lf_fcat>-col_pos = 12.
      WHEN 'VPP_N3'.
        <lf_fcat>-tooltip = 'VPP - Quá hạn 3 tháng'.
        <lf_fcat>-coltext = 'VPP - Quá hạn 3 tháng'.
        <lf_fcat>-col_pos = 13.
      WHEN 'VPP_N6'.
        <lf_fcat>-tooltip = 'VPP - Quá hạn 6 tháng'.
        <lf_fcat>-coltext = 'VPP - Quá hạn 6 tháng'.
        <lf_fcat>-col_pos = 14.
      WHEN 'VOSO_N1'.
        <lf_fcat>-tooltip = 'VOSO - Trong hạn'.
        <lf_fcat>-coltext = 'VOSO - Trong hạn'.
        <lf_fcat>-col_pos = 15.
      WHEN 'VOSO_N2'.
        <lf_fcat>-tooltip = 'VOSO - Quá hạn'.
        <lf_fcat>-coltext = 'VOSO - Quá hạn'.
        <lf_fcat>-col_pos = 16.
      WHEN 'VOSO_N3'.
        <lf_fcat>-tooltip = 'VOSO - Quá hạn 3 tháng'.
        <lf_fcat>-coltext = 'VOSO - Quá hạn 3 tháng'.
        <lf_fcat>-col_pos = 17.
      WHEN 'VOSO_N6'.
        <lf_fcat>-tooltip = 'VOSO - Quá hạn 6 tháng'.
        <lf_fcat>-coltext = 'VOSO - Quá hạn 6 tháng'.
        <lf_fcat>-col_pos = 18.
      WHEN 'VTSALE_N1'.
        <lf_fcat>-tooltip = 'VTSALE - Trong hạn'.
        <lf_fcat>-coltext = 'VTSALE - Trong hạn'.
        <lf_fcat>-col_pos = 19.
      WHEN 'VTSALE_N2'.
        <lf_fcat>-tooltip = 'VTSALE - Quá hạn'.
        <lf_fcat>-coltext = 'VTSALE - Quá hạn'.
        <lf_fcat>-col_pos = 20.
      WHEN 'VTSALE_N3'.
        <lf_fcat>-tooltip = 'VTSALE - Quá hạn 3 tháng'.
        <lf_fcat>-coltext = 'VTSALE - Quá hạn 3 tháng'.
        <lf_fcat>-col_pos = 21.
      WHEN 'VTSALE_N6'.
        <lf_fcat>-tooltip = 'VTSALE - Quá hạn 6 tháng'.
        <lf_fcat>-coltext = 'VTSALE - Quá hạn 6 tháng'.
        <lf_fcat>-col_pos = 22.
      WHEN 'LOG_N1'.
        <lf_fcat>-tooltip = 'LOG - Trong hạn'.
        <lf_fcat>-coltext = 'LOG - Trong hạn'.
        <lf_fcat>-col_pos = 23.
      WHEN 'LOG_N2'.
        <lf_fcat>-tooltip = 'LOG - Quá hạn'.
        <lf_fcat>-coltext = 'LOG - Quá hạn'.
        <lf_fcat>-col_pos = 24.
      WHEN 'LOG_N3'.
        <lf_fcat>-tooltip = 'LOG - Quá hạn 3 tháng'.
        <lf_fcat>-coltext = 'LOG - Quá hạn 3 tháng'.
        <lf_fcat>-col_pos = 25.
      WHEN 'LOG_N6'.
        <lf_fcat>-tooltip = 'LOG - Quá hạn 6 tháng'.
        <lf_fcat>-coltext = 'LOG - Quá hạn 6 tháng'.
        <lf_fcat>-col_pos = 26.
      WHEN 'THOI_HAN'.
        <lf_fcat>-tooltip = 'Thời hạn cam kết TT'.
        <lf_fcat>-coltext = 'Thời hạn cam kết TT'.
        <lf_fcat>-datatype = 'DATS'.
        <lf_fcat>-inttype = 'D'.
        <lf_fcat>-intlen = '000008'.
        <lf_fcat>-dd_outlen = '000010'.
        <lf_fcat>-col_pos = 27.
      WHEN OTHERS.
        <lf_fcat>-tech = 'X'.
        <lf_fcat>-no_out = 'X'.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_TEMP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM download_temp .
  DATA: lw_viewer_suppress TYPE xmark,
        lw_fullpath        TYPE string,
        lw_filename        TYPE string,
        lw_path            TYPE string,
        lw_filter          TYPE string,
        lw_user_action     TYPE i.
  DATA: lw_string TYPE string.
  TABLES sscrfields.
  lw_filter = '(*.XLSX)|*.XLSX|'.
*  lw_filename = 'VTP_ZFIR048'.
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
*     window_title         = i_window_title
      with_encoding        = 'X'
*     initial_directory    = i_initial_directory
      default_file_name    = 'VTP_ZFIR048'
      file_filter          = lw_filter
    CHANGING
      filename             = lw_filename
      path                 = lw_path
      fullpath             = lw_fullpath
      user_action          = lw_user_action
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    RAISE error.
    RETURN.
  ENDIF.
  IF lw_user_action <> cl_gui_frontend_services=>action_ok.
    RETURN.
  ENDIF.
*  gt_data_temp =  CORRESPONDING #( gt_data MAPPING prctr = prctr segment = segment pstcode = pstcode ten_cn = ten_cn EXCEPT ).
  SELECT prctr,segment,pstcode,ten_cn,kunnr FROM @gt_data AS gt_data INTO CORRESPONDING FIELDS OF TABLE @gt_data_temp.

  CALL FUNCTION 'ZXLWB_CALLFORM'
    EXPORTING
      iv_formname        = 'ZFORM_FIR048'
      iv_context_ref     = gt_data_temp
      iv_viewer_suppress = 'X'
      iv_save_as         = lw_fullpath
    EXCEPTIONS
      OTHERS             = 2.
  IF sy-subrc NE 0 .
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
  ENDIF .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPLOAD_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM upload_file.
  PERFORM get_file_name.
  PERFORM upload_data.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESS_IMPORT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_import_data .
  g_alv->refresh_table_display( ).
  CLEAR gw_error.
  PERFORM upload_file.
  CHECK sy-subrc = 0.
  PERFORM check_data_valid CHANGING gw_error.
  CHECK gw_error NE 'X'.
  PERFORM display_data_import.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_FILE_NAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_file_name .
  DATA: lt_files TYPE filetable,
        ls_files LIKE LINE OF lt_files,
        lw_rc    TYPE i.
  CLEAR p_file.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select Upload File'
      default_filename        = '*.xlsx'
*     default_filename        = '*.*'
*     multiselection          = ' '
    CHANGING
      file_table              = lt_files
      rc                      = lw_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
*  ENDIF.
  IF sy-subrc NE 0 .
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
    RETURN.
  ELSE.
    p_file = VALUE #( lt_files[ 1 ]-filename OPTIONAL ).
  ENDIF .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPLOAD_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM upload_data .
  REFRESH gt_data_import[].
  CALL FUNCTION 'UPLOAD_XLS_FILE_2_ITAB'
    EXPORTING
      i_filename = p_file
    TABLES
      e_itab     = gt_data_import
    EXCEPTIONS
      file_error = 1
      OTHERS     = 2.
  IF sy-subrc = 0.
    TRY.
        IF lines( gt_data_import ) > 2.
          DELETE gt_data_import FROM 1 TO 2.
        ENDIF.
      CATCH cx_root.
        MESSAGE 'Upload error' TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA_VALID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_data_valid CHANGING lps_error.
  DATA :lt_mess_err TYPE TABLE OF bapiret2,
        lw_cusid    TYPE kunnr.
  LOOP AT gt_data_import INTO DATA(ls_import).
    IF ls_import-prctr IS INITIAL OR ls_import-segment IS INITIAL OR ls_import-kunnr IS INITIAL OR ls_import-thoi_han IS INITIAL.
      APPEND INITIAL LINE TO lt_mess_err ASSIGNING FIELD-SYMBOL(<fs_mess_err>).
      <fs_mess_err>-type = 'E'.
      <fs_mess_err>-message = |line { sy-tabix + 2 }:PC, Segment, CusId, thời hạn cam kết bắt buộc nhập|.
    ENDIF.
  ENDLOOP.
  IF lt_mess_err IS NOT INITIAL.
    CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
      TABLES
        it_return = lt_mess_err.
*    LEAVE TO SCREEN 0100.
    lps_error = 'X'.
  ENDIF.
  CHECK lt_mess_err IS INITIAL.
  SORT gt_data BY prctr segment kunnr.
  LOOP AT gt_data_import INTO ls_import.
    lw_cusid = |{ ls_import-kunnr ALPHA = IN }|.

    READ TABLE gt_data WITH KEY prctr = ls_import-prctr segment = ls_import-segment kunnr = lw_cusid TRANSPORTING NO FIELDS BINARY SEARCH.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO lt_mess_err ASSIGNING <fs_mess_err>.
      <fs_mess_err>-type = 'E'.
      <fs_mess_err>-message = |line { sy-tabix + 2 }:Dữ liệu PC { ls_import-prctr } - Segment { ls_import-segment } - CusId { ls_import-kunnr } không đúng !|.
    ENDIF.
    CLEAR lw_cusid.
  ENDLOOP.
  CHECK lt_mess_err IS NOT INITIAL.
  CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
    TABLES
      it_return = lt_mess_err.
  lps_error = 'X'.
*  LEAVE TO SCREEN 0100.
*    lt_mess_err = VALUE #( FOR wa IN gt_error_field WHERE ( row = es_row_no-row_id ) ( type = wa-type message = wa-message ) ).
*    CHECK lt_mess_err  IS NOT INITIAL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA_IMPORT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data_import .
  REFRESH :gt_data_import_valid[],gt_data_pd[],gt_data_dn[].
  LOOP AT gt_data_import ASSIGNING FIELD-SYMBOL(<fs_data_import>).
    <fs_data_import>-thoi_han = |{ <fs_data_import>-thoi_han+6(4) }{ <fs_data_import>-thoi_han+3(2) }{ <fs_data_import>-thoi_han+0(2) }|.
  ENDLOOP.
  gt_data_dn = CORRESPONDING #( gt_data_import ).
  PERFORM update_amount USING 'X' CHANGING gt_data_dn.
  gt_data_pd = CORRESPONDING #( gt_data_dn ).
  PERFORM build_row_display.
  IF gt_data_import_valid IS NOT INITIAL.
*    g_alv->refresh_table_display( ).
    CALL SCREEN 0200.
  ELSE.
    MESSAGE 'No Data !' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_ROW_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_row_display.
  DATA : ls_cell_style TYPE lvc_s_styl,
         ls_cell_color TYPE lvc_s_scol.

  SORT gt_data BY prctr kunnr.
  LOOP AT gt_data_dn INTO DATA(ls_data_dn).
    READ TABLE gt_data WITH KEY prctr = ls_data_dn-prctr kunnr = |{ ls_data_dn-kunnr ALPHA = IN }| INTO DATA(ls_data) BINARY SEARCH.
    IF sy-subrc = 0.
      APPEND INITIAL LINE TO gt_data_import_valid ASSIGNING FIELD-SYMBOL(<fs_data_import_valid>).
      MOVE-CORRESPONDING ls_data TO <fs_data_import_valid>.
      <fs_data_import_valid>-thoi_han = ls_data_dn-thoi_han.
      IF sy-tcode = 'ZFIR048_01'.
        ls_cell_style-fieldname = 'FLAG'.
        ls_cell_style-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND ls_cell_style TO <fs_data_import_valid>-cell_style[].
        CLEAR ls_cell_style.
      ENDIF.

      APPEND INITIAL LINE TO gt_data_import_valid ASSIGNING <fs_data_import_valid>.
      MOVE-CORRESPONDING ls_data_dn TO <fs_data_import_valid>.
      <fs_data_import_valid>-waers = 'VND'.
      <fs_data_import_valid>-line_color = 'C410'.
      CLEAR <fs_data_import_valid>-thoi_han.
      IF sy-tcode = 'ZFIR048_01'.
        ls_cell_color-fname = 'FLAG'.
        ls_cell_color-color-col = 7.
        ls_cell_color-color-int = '1'.
        ls_cell_color-color-inv = '0'.
        APPEND ls_cell_color TO <fs_data_import_valid>-cell_color[].
        CLEAR  ls_cell_color.
      ENDIF.

      APPEND INITIAL LINE TO gt_data_import_valid ASSIGNING <fs_data_import_valid>.
      MOVE-CORRESPONDING ls_data TO <fs_data_import_valid>.
      PERFORM substract_amount USING ls_data_dn CHANGING <fs_data_import_valid>.
      <fs_data_import_valid>-line_color = 'C510'.
      IF sy-tcode = 'ZFIR048_01'.
        ls_cell_style-fieldname = 'FLAG'.
        ls_cell_style-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND ls_cell_style TO <fs_data_import_valid>-cell_style[] .
        CLEAR ls_cell_style.

        ls_cell_color-fname = 'FLAG'.
        ls_cell_color-color-col = 0.
        ls_cell_color-color-int = '1'.
        ls_cell_color-color-inv = '0'.
        APPEND ls_cell_color TO <fs_data_import_valid>-cell_color[].
        CLEAR  ls_cell_color.

      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
FORM update_amount USING display2sap TYPE char1
                   CHANGING lpt_tab TYPE ANY TABLE.
  FIELD-SYMBOLS :<fs_data_ass>   TYPE fins_vhcur12,
                 <fs_amount_str> TYPE char23.
  DATA : lw_dec23 TYPE fins_vhcur12.

  IF display2sap = 'X'."convert amount display to amount sap
    LOOP AT lpt_tab ASSIGNING FIELD-SYMBOL(<fs_tab>).
      LOOP AT gt_dfies_tab INTO DATA(ls_dfies_tab) WHERE rollname = 'FINS_VHCUR12'.
        ASSIGN COMPONENT ls_dfies_tab-fieldname OF STRUCTURE <fs_tab> TO <fs_data_ass>.
        <fs_data_ass> = <fs_data_ass> / 100.
      ENDLOOP.
    ENDLOOP.
  ELSE."convert amount to display in table
    LOOP AT lpt_tab ASSIGNING <fs_tab>.
      LOOP AT gt_dfies_tab INTO ls_dfies_tab WHERE rollname = 'FINS_VHCUR12'.
        ASSIGN COMPONENT ls_dfies_tab-fieldname OF STRUCTURE <fs_tab> TO <fs_amount_str>.
        lw_dec23 = <fs_amount_str>.
        WRITE lw_dec23 TO <fs_amount_str> CURRENCY 'VND'.
        CLEAR lw_dec23.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
ENDFORM.

*FORM substract_amount  USING
*                             lp_value TYPE gty_fir048
*                       CHANGING lp_data TYPE gty_fir048.
FORM substract_amount USING lp_value TYPE any
                     CHANGING lp_data TYPE any.
*  DATA : lt_dfies_tab TYPE TABLE OF dfies.
  DATA :ls_cell_color TYPE lvc_s_scol.
  IF sy-subrc = 0.
    LOOP AT gt_dfies_tab INTO DATA(ls_dfies_tab) WHERE rollname = 'FINS_VHCUR12'.
      ASSIGN COMPONENT ls_dfies_tab-fieldname OF STRUCTURE lp_value TO FIELD-SYMBOL(<fs_value>).
      ASSIGN COMPONENT ls_dfies_tab-fieldname OF STRUCTURE lp_data TO FIELD-SYMBOL(<fs_data>).
      <fs_data> = <fs_data> - <fs_value> .
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SEND_APPROVE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM send_approve .
  DATA :lt_cong_no_tab TYPE TABLE OF zdn_congno,
        lw_amount      TYPE zdd_amt_disp.
*        lw_dec23       TYPE fins_vhcur12.

  CHECK gt_data_pd IS NOT INITIAL.
*  LOOP AT gt_data_pd ASSIGNING FIELD-SYMBOL(<fs_data_pd>).
*    <fs_data_pd>-thoi_han = |{ <fs_data_pd>-thoi_han+6(4) }{ <fs_data_pd>-thoi_han+3(2) }{ <fs_data_pd>-thoi_han+0(2) }|.
*  ENDLOOP.
  lt_cong_no_tab = CORRESPONDING #( gt_data_pd ).
  PERFORM update_amount USING '' CHANGING lt_cong_no_tab.
  LOOP AT lt_cong_no_tab ASSIGNING FIELD-SYMBOL(<fs_cong_no>).
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = <fs_cong_no>-guid.
    <fs_cong_no>-kunnr = |{ <fs_cong_no>-kunnr ALPHA = IN }|.
    <fs_cong_no>-thang = p_month.
    <fs_cong_no>-nam = p_year.
    <fs_cong_no>-ngay_yc = sy-datum.
    <fs_cong_no>-nguoi_tao = sy-uname.
    <fs_cong_no>-tgian_tao = sy-timlo.
    <fs_cong_no>-trang_thai = '0'.
*    lw_dec23 = <fs_cong_no>-cpn_n3.
*    WRITE lw_dec23 TO <fs_cong_no>-cpn_n3 CURRENCY 'VND'.
  ENDLOOP.
  INSERT zdn_congno FROM TABLE lt_cong_no_tab.
  COMMIT WORK AND WAIT .
  MESSAGE 'Gửi phê duyệt thành công !' TYPE 'I' DISPLAY LIKE 'S'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> O_DYNDOC_ID
*&---------------------------------------------------------------------*
*FORM event_top_of_page.
*
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUID_DATA_0300
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM buid_data_0300 .
*  DATA: lt_data_import TYPE ztt_fir048.
  PERFORM convert_amount_tab.
  gt_data_dn = CORRESPONDING #( gt_cong_no ).
  PERFORM update_amount USING 'X' CHANGING gt_data_dn.
  PERFORM build_row_display.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERT_AMOUNT_TAB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM convert_amount_tab .
  CHECK gt_cong_no IS NOT INITIAL.
  LOOP AT gt_cong_no ASSIGNING FIELD-SYMBOL(<fs_cong_no>).
    LOOP AT gt_dfies_tab INTO DATA(ls_dfies_tab) WHERE rollname = 'FINS_VHCUR12'.
      ASSIGN COMPONENT ls_dfies_tab-fieldname OF STRUCTURE <fs_cong_no> TO FIELD-SYMBOL(<fs_amount_str>).
      REPLACE ALL OCCURRENCES OF '.' IN <fs_amount_str> WITH ''.
      CONDENSE <fs_amount_str>.
    ENDLOOP.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM exclude_tb_functions CHANGING pt_exclude TYPE ui_functions.
  DATA ls_exclude TYPE ui_func.

* Row manipulation
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_select_all.
  APPEND ls_exclude TO pt_exclude.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_ALL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM select_all .
  LOOP AT gt_data_import_valid ASSIGNING FIELD-SYMBOL(<fs_data_import_valid>) WHERE line_color = 'C410'.
    <fs_data_import_valid>-flag = 'X'.
  ENDLOOP.
  g_alv->refresh_table_display( ).
  g_alv->check_changed_data( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DESELECT_ALL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM deselect_all .
  LOOP AT gt_data_import_valid ASSIGNING FIELD-SYMBOL(<fs_data_import_valid>) WHERE line_color = 'C410'.
    CLEAR <fs_data_import_valid>-flag.
  ENDLOOP.
  g_alv->refresh_table_display( ).
  g_alv->check_changed_data( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPROVE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM approve .
  TYPES :BEGIN OF lty_list_del,
           prctr   TYPE prctr,
           segment TYPE fb_segment,
           kunnr   TYPE kunnr,
         END OF lty_list_del.
  DATA :lt_list_del TYPE TABLE OF lty_list_del.
  LOOP AT gt_data_import_valid INTO DATA(ls_data_import_valid) WHERE flag = 'X'.
    READ TABLE gt_cong_no WITH KEY prctr = ls_data_import_valid-prctr segment = ls_data_import_valid-segment kunnr = ls_data_import_valid-kunnr INTO DATA(ls_cong_no) BINARY SEARCH.
    IF sy-subrc = 0.
      UPDATE zdn_congno SET trang_thai = '1'
                            ngay_duyet = sy-datum
                            tgian_duyet = sy-timlo
                            nguoi_duyet = sy-uname
                        WHERE guid = ls_cong_no-guid.

      COMMIT WORK AND WAIT.
      APPEND INITIAL LINE TO lt_list_del ASSIGNING FIELD-SYMBOL(<fs_list_del>).
      MOVE-CORRESPONDING ls_cong_no TO <fs_list_del>.
    ENDIF.
  ENDLOOP.
  MESSAGE 'Duyệt thành công !' TYPE 'I' DISPLAY LIKE 'S'.
  IF lt_list_del IS NOT INITIAL.
    LOOP AT lt_list_del INTO DATA(ls_list_del).
      DELETE gt_data_import_valid WHERE prctr = ls_list_del-prctr AND segment = ls_list_del-segment AND kunnr = ls_list_del-kunnr.
    ENDLOOP.
    g_alv->refresh_table_display( ).
    g_alv->check_changed_data( ).
  ENDIF.
*  g_alv->refresh_table_display( ).
*  g_alv->check_changed_data( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CANCEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM cancel .
  TYPES :BEGIN OF lty_list_del,
           prctr   TYPE prctr,
           segment TYPE fb_segment,
           kunnr   TYPE kunnr,
         END OF lty_list_del.
  DATA :lt_list_del TYPE TABLE OF lty_list_del.
  LOOP AT gt_data_import_valid INTO DATA(ls_data_import_valid) WHERE flag = 'X'.
    READ TABLE gt_cong_no WITH KEY prctr = ls_data_import_valid-prctr segment = ls_data_import_valid-segment kunnr = ls_data_import_valid-kunnr INTO DATA(ls_cong_no) BINARY SEARCH.
    IF sy-subrc = 0.
      UPDATE zdn_congno SET trang_thai = '2'
                            ngay_duyet = sy-datum
                            tgian_duyet = sy-timlo
                            nguoi_duyet = sy-uname
                        WHERE guid = ls_cong_no-guid.
      COMMIT WORK AND WAIT.
      APPEND INITIAL LINE TO lt_list_del ASSIGNING FIELD-SYMBOL(<fs_list_del>).
      MOVE-CORRESPONDING ls_cong_no TO <fs_list_del>.
    ENDIF.
  ENDLOOP.
  MESSAGE 'Từ chối duyệt thành công !' TYPE 'I' DISPLAY LIKE 'S'.
  IF lt_list_del IS NOT INITIAL.
    LOOP AT lt_list_del INTO DATA(ls_list_del).
      DELETE gt_data_import_valid WHERE prctr = ls_list_del-prctr AND segment = ls_list_del-segment AND kunnr = ls_list_del-kunnr.
    ENDLOOP.
    g_alv->refresh_table_display( ).
    g_alv->check_changed_data( ).
  ENDIF.

ENDFORM.
FORM get_fields_struct.
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = 'ZST_FIR048'
    TABLES
      dfies_tab      = gt_dfies_tab
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUID_DATA_PARALLEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM buid_data_parallel .
  PERFORM get_resource.
  PERFORM process.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_RESOURCE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_resource .
  CALL FUNCTION 'SPBT_INITIALIZE'
    EXPORTING
      group_name   = 'RFCGROUP'
    IMPORTING
      max_pbt_wps  = gw_max_ws
      free_pbt_wps = gw_free_ws.
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
  DATA :task      TYPE string,
        lt_data   LIKE gt_data1,
        lt_acdoca LIKE gt_acdoca.
  IF gw_free_ws > 1.
    gw_free_ws = gw_free_ws * 80 / 100.
    gw_max_record = ceil( lines( gt_acdoca ) / ( gw_free_ws ) ).
    IF lines( gt_acdoca ) <  gw_free_ws.
      gw_max_record = 1.
    ENDIF.
    DATA(snd_jobs) = 0.
    DO gw_free_ws TIMES.
      snd_jobs = snd_jobs + 1.
      task = |task{ snd_jobs }|.
      APPEND LINES OF gt_acdoca FROM  1 TO gw_max_record TO lt_acdoca.

      CALL FUNCTION 'ZFM_FIR048_PARALLEL' STARTING NEW TASK task
        DESTINATION IN GROUP 'RFCGROUP'
        PERFORMING on_end_of_action ON END OF TASK
        EXPORTING
          due_date    = due_date
          due_date_n1 = due_date_n1
          due_date_n2 = due_date_n2
          due_date_n3 = due_date_n3
          due_date_n4 = due_date_n4
          due_date_n5 = due_date_n5
          due_date_n6 = due_date_n6
          due_date_n7 = due_date_n7
*         due_date_n8 = due_date_n8
        TABLES
          acdoca_tab  = lt_acdoca
          knb1_tab    = gt_knb1
          cn_tab      = gt_cn
          data_tab    = lt_data.

      DELETE gt_acdoca FROM 1 TO gw_max_record.
      REFRESH lt_acdoca[].
    ENDDO.
    WAIT UNTIL rcv_jobs >= snd_jobs.

  ELSE.
    CALL FUNCTION 'ZFM_FIR048_PARALLEL'
      EXPORTING
        due_date    = due_date
        due_date_n1 = due_date_n1
        due_date_n2 = due_date_n2
        due_date_n3 = due_date_n3
        due_date_n4 = due_date_n4
        due_date_n5 = due_date_n5
        due_date_n6 = due_date_n6
        due_date_n7 = due_date_n7
*       due_date_n8 = due_date_n8
      TABLES
        acdoca_tab  = gt_acdoca
        knb1_tab    = gt_knb1
        cn_tab      = gt_cn
        data_tab    = gt_data1.
  ENDIF.
ENDFORM.

FORM on_end_of_action USING task.
  DATA :lt_data LIKE gt_data1.

  RECEIVE RESULTS FROM FUNCTION 'ZFM_FIR048_PARALLEL'
   TABLES data_tab  = lt_data.

  LOOP AT lt_data INTO DATA(ls_data).
    COLLECT ls_data INTO gt_data1.
  ENDLOOP.

  rcv_jobs = rcv_jobs + 1.

ENDFORM.
