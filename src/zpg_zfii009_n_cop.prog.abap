*&---------------------------------------------------------------------*
*& Report ZPG_ZFII009_N_COP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_ZFII009_N_COP.
  METHOD zii_si_bkcptx_n_in~si_bkcptx_n_in.
*** **** INSERT IMPLEMENTATION HERE **** ***
    DATA :input_cop TYPE zmt_bkcptx_n_sender.
    TYPES: BEGIN OF lty_tax_line,
             tax_code TYPE mwskz,
             tax_am   TYPE tfm_amountfinanced,
             amt_base TYPE  bapiamtbase.
    TYPES: END OF lty_tax_line.

    TYPES: BEGIN OF lty_fi_doc,
             fi_doc TYPE numc10,
             zurl   TYPE so_text255,
             mau_hd TYPE  text100.
    TYPES: END OF lty_fi_doc.

*    DATA: lt_tax_line TYPE TABLE OF lty_tax_line,
*          ls_tax_line TYPE lty_tax_line.

    DATA: lt_fi_doc TYPE TABLE OF lty_fi_doc,
          ls_fi_doc TYPE lty_fi_doc.

*  DATA: lt_tax TYPE TABLE OF t030k,
*        ls_tax TYPE t030k.

    " Get thông tin Profit center from Mã bưu cục
    DATA: lw_obj_key      TYPE bapiache09-obj_key,
          lw_buccuc(10)   TYPE c,
          lw_sdate        TYPE string,
          lw_post_date    TYPE dats,
          lw_pc           TYPE prctr,
          lw_cost_center  TYPE kostl,
          lw_comcode      TYPE bukrs,
          lw_tax_konts    TYPE saknr,
          lw_tk_hach_toan TYPE char10.

    DATA:
      lt_bkcptx_hdon   TYPE TABLE OF ztb_kcptx_hdon_n,
      lt_bkcptx_hdo_i  TYPE TABLE OF ztb_kcptx_hdi_n,
      lt_bkcptx_hdr    TYPE TABLE OF ztb_bkcptx_hdr_n,
      lt_bkcptx_pb     TYPE TABLE OF ztb_cptx_hd_pb_n,
      ls_bkcptx_hdr_c2 TYPE ztb_bkcptx_hdr_n,
      ls_bkcptx_hdr_c1 TYPE ztb_bkcptx_hdr_n,
      ls_bkcptx_hdon   TYPE ztb_kcptx_hdon_n,
      ls_bkcptx_hdo_i  TYPE ztb_kcptx_hdi_n,
      ls_bkcptx_pb     TYPE ztb_cptx_hd_pb_n,
      lw_hdon_line     TYPE int4,
      lw_hdon_i_line   TYPE int4,
      lw_hdrc2_line    TYPE int4,
      lw_hdrc1_line    TYPE int4,
      lw_pb_i_line     TYPE int4.

    "check date is valid?
    DATA: lw_ngay_hd TYPE dats,
          lw_str     TYPE string,
          lw_str_out TYPE string.
    DATA: ls_header         TYPE bapiache09,
          ls_customercpd    TYPE bapiacpa09,
          lt_accountgl      TYPE TABLE OF bapiacgl09,
          ls_accountgl      TYPE bapiacgl09,
          lt_accountpayable TYPE TABLE OF bapiacap09,
          ls_accountpayable TYPE bapiacap09,
          lt_accounttax     TYPE TABLE OF bapiactx09,
          ls_accounttax     TYPE bapiactx09,
          lt_currencyamount TYPE TABLE OF bapiaccr09,
          ls_amount         TYPE bapiaccr09,
          lt_extension1     TYPE TABLE OF bapiacextc,
          ls_extension1     TYPE bapiacextc,
          lt_ext            TYPE TABLE OF bapiacextc,
          ls_ext            TYPE bapiacextc,
          lt_exten2         TYPE TABLE OF bapiparex.

    DATA: lw_name1     TYPE name1_gp,
          lw_name2     TYPE name1_gp,
          lw_name3     TYPE name1_gp,
          lw_name4     TYPE name1_gp,
          lw_nguoi_ban TYPE text150.

    DATA: lt_pi_header   TYPE zdt_bkcptx_n_sender_header_tab,
          lt_pi_hoa_don  TYPE zdt_bkcptx_n_sender_hoa_do_tab.
*        lt_pi_phanbo   TYPE zdt_bkcptx_sender_phan_bo_tab,
*          lt_item_header TYPE zdt_bkcptx_sender_item_tab1.
    DATA: lw_lines    TYPE i,
          lw_awt      TYPE tfm_amountfinanced, " total amount with tax
          lw_am       TYPE tfm_amountfinanced, " amount tong chi nhanh
          lw_am_pb    TYPE tfm_amountfinanced, " amount tong phan bo
          lw_am_i     TYPE tfm_amountfinanced,
          lw_descr    TYPE string,
          lw_tax_am   TYPE tfm_amountfinanced, " total tax amount
          lw_am_base  TYPE tfm_amountfinanced,
          lw_flag_tax TYPE c. " total amount base
    DATA: lw_tax_code_hd_i TYPE  mwskz,
          lw_tax_code_hd   TYPE  mwskz.

    DATA:
      lt_return  TYPE TABLE OF bapiret2,
      ls_return1 TYPE bapiret2,
      ls_return  TYPE bapiret2,
      lw_objkey  TYPE swo_typeid,
      lw_fi_doc  TYPE numc10.

    DATA: lt_url       TYPE TABLE OF so_text255,
          lw_url_index TYPE so_obj_des,
          lt_fline     TYPE TABLE OF tline.
    "add
    TYPES :BEGIN OF lty_kostl,
             cost_center TYPE kostl,
           END OF lty_kostl.
    TYPES :BEGIN OF lty_prctr,
             prctr TYPE prctr,
           END OF lty_prctr.
    TYPES :BEGIN OF lty_item_hc1,
             item TYPE zdt_bkcptx_n_sender_item_tab1,
           END OF lty_item_hc1.
    TYPES :BEGIN OF lty_phan_bo,
             phan_bo TYPE zdt_bkcptx_n_sender_phan_b_tab,
           END OF  lty_phan_bo.
    TYPES :BEGIN OF lty_cc_prctr_comp,
             cc    TYPE kostl,
             prctr TYPE prctr,
             comp  TYPE bukrs,
           END OF  lty_cc_prctr_comp.
    TYPES :BEGIN OF lty_item_hd,
             item_hd TYPE zdt_bkcptx_n_sender_item_tab,
           END OF lty_item_hd.

    TYPES :BEGIN OF lty_bk_ref_value,
             line_hd_2 TYPE int4,
             line_hd_1 TYPE int4,
             value     TYPE ztb_bkcptx_hdr_n,
           END OF lty_bk_ref_value.

    DATA :lw_posting_date   TYPE dats,
          lt_cc_valid       TYPE TABLE OF lty_kostl,
          lt_pr_valid       TYPE TABLE OF lty_prctr,
          lt_item_header_c1 TYPE TABLE OF lty_item_hc1,
          lt_phan_bo        TYPE TABLE OF lty_phan_bo,
          lt_cc_prctr_comp  TYPE TABLE OF lty_cc_prctr_comp,
          lw_konto          TYPE t001b,
          lt_bk_ref_value   TYPE TABLE OF lty_bk_ref_value,
          lt_item_hd        TYPE TABLE OF lty_item_hd.

    DATA: lw_amount_hd_bkc1    TYPE string,
          lw_tt_amount_hd_bkc1 TYPE string,
          lw_tt_amount_hd_bkc2 TYPE string,
          lw_un                TYPE string.


    DATA:
      lt_konto  TYPE TABLE OF t001b,
      lw_return TYPE bapiret2,
      lw_erc    TYPE sy-subrc.

*   FIELD-SYMBOLS: <fs_bkc2> TYPE zdt_bkcptx_receiver_bkc2.
    DATA :lw_stop TYPE abap_bool.

    TYPES :BEGIN OF lty_mapping_bk,
             bkc1 TYPE string,
             bkc2 TYPE string,
           END OF lty_mapping_bk.

    DATA :lt_mapping_bk  TYPE  TABLE OF lty_mapping_bk.

    FIELD-SYMBOLS: <fs_err_input> TYPE zdt_bkcptx_n_receiver_err_input.
    FIELD-SYMBOLS: <fs_err_acct> TYPE zdt_bkcptx_n_receiver_err_acct.
    FIELD-SYMBOLS: <fs_err_hdon> TYPE zdt_bkcptx_n_receiver_hdon.
    FIELD-SYMBOLS: <fs_err_bck1> TYPE zdt_bkcptx_n_receiver_bkc11.
    "Inset data to tables from input

    LOOP AT input-mt_bkcptx_n_sender-header INTO DATA(ls_header_c2).
      lw_hdrc2_line = sy-tabix.
      MOVE-CORRESPONDING ls_header_c2 TO ls_bkcptx_hdr_c2.
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_16 = ls_bkcptx_hdr_c2-guid.
      ls_bkcptx_hdr_c2-type = '02'.


      "bkc1
      LOOP AT ls_header_c2-item INTO DATA(ls_header_c1).
        lw_hdrc1_line = sy-tabix.
        MOVE-CORRESPONDING ls_header_c1 TO ls_bkcptx_hdr_c1.
        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            ev_guid_16 = ls_bkcptx_hdr_c1-guid.
        ls_bkcptx_hdr_c1-type = '01'.
        ls_bkcptx_hdr_c1-bk_c2 = ls_header_c2-bk_id.
        APPEND ls_bkcptx_hdr_c1 TO lt_bkcptx_hdr.

        "cal amount tct phe duyet
        LOOP AT input-mt_bkcptx_n_sender-hoa_don INTO DATA(ls_hoa_don) WHERE bk1_id = ls_header_c1-bk_id.
          lw_amount_hd_bkc1 = REDUCE #( INIT lw_sum = lw_un FOR lw_data IN ls_hoa_don-item
                                        NEXT lw_sum = lw_sum + lw_data-amount + lw_data-tax_amount + lw_data-phu_phi
                                       ).
          lw_tt_amount_hd_bkc1 = lw_tt_amount_hd_bkc1 + lw_amount_hd_bkc1.
        ENDLOOP.
        lw_tt_amount_hd_bkc2 = lw_tt_amount_hd_bkc2 + lw_tt_amount_hd_bkc1.
        CLEAR :lw_amount_hd_bkc1,lw_tt_amount_hd_bkc1.
      ENDLOOP.
      ls_bkcptx_hdr_c2-tct_pd = lw_tt_amount_hd_bkc2.
      APPEND ls_bkcptx_hdr_c2 TO lt_bkcptx_hdr.
      CLEAR lw_tt_amount_hd_bkc2.
    ENDLOOP.
    "hoa don
    LOOP AT input-mt_bkcptx_n_sender-hoa_don INTO ls_hoa_don.
      lw_hdon_line = sy-tabix.
      MOVE-CORRESPONDING ls_hoa_don TO ls_bkcptx_hdon.
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_16 = ls_bkcptx_hdon-guid_hdon.
      ls_bkcptx_hdon-guid_hdr = lt_bkcptx_hdr[ type = '01' bk_id = ls_hoa_don-bk1_id ]-guid.
      ls_bkcptx_hdon-hdon_id = lw_hdon_line.
      APPEND  ls_bkcptx_hdon TO  lt_bkcptx_hdon.
      "hoa don line
      LOOP AT ls_hoa_don-item INTO DATA(ls_hoa_don_i).
        lw_hdon_i_line = sy-tabix.
        MOVE-CORRESPONDING ls_hoa_don_i TO ls_bkcptx_hdo_i.
        ls_bkcptx_hdo_i-item_line = lw_hdon_i_line.
        ls_bkcptx_hdo_i-guid_hdon = ls_bkcptx_hdon-guid_hdon.
        ls_bkcptx_hdo_i-tax = ls_hoa_don_i-tax_item.
        APPEND ls_bkcptx_hdo_i TO lt_bkcptx_hdo_i.
      ENDLOOP.
      "phan bo
      LOOP AT ls_hoa_don-phan_bo INTO DATA(ls_phan_bo).
        MOVE-CORRESPONDING ls_phan_bo TO ls_bkcptx_pb.
        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            ev_guid_16 = ls_bkcptx_pb-guid_pb.
        ls_bkcptx_pb-guid_hdon = ls_bkcptx_hdon-guid_hdon.
        APPEND ls_bkcptx_pb TO lt_bkcptx_pb.
        CLEAR ls_bkcptx_pb.
      ENDLOOP.

    ENDLOOP.

*    lt_item_hd = CORRESPONDING #( input-ZMT_BKCPTX_N_SENDER-hoa_don ).
*
*    delete lt_item_hd where
*    EXPORT item_hd =  lt_item_hd TO DATABASE indx(nt) CLIENT sy-mandt ID 'ITM'.
    "Add all cost center to internal table from BKC1 and BKC2
    lt_cc_valid = CORRESPONDING #( input-mt_bkcptx_n_sender-header ).
    lt_item_header_c1 = CORRESPONDING #( input-mt_bkcptx_n_sender-header ).
    lt_phan_bo = CORRESPONDING #( input-mt_bkcptx_n_sender-hoa_don ).
    LOOP AT lt_item_header_c1 INTO DATA(ls_item_header_c1).
      LOOP AT ls_item_header_c1-item INTO DATA(ls_item_header_c1_1).
        APPEND INITIAL LINE TO lt_cc_valid ASSIGNING FIELD-SYMBOL(<fs_cc_valid>).
        <fs_cc_valid>-cost_center = ls_item_header_c1_1-cost_center.
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_phan_bo INTO DATA(ls_phan_bo_tab).
      LOOP AT ls_phan_bo_tab-phan_bo INTO DATA(ls_phan_bo_line).
        APPEND INITIAL LINE TO lt_cc_valid ASSIGNING <fs_cc_valid>.
        <fs_cc_valid>-cost_center = ls_phan_bo_line-cc_pb.
      ENDLOOP.
    ENDLOOP.


    "Get profit center from cost center
    SELECT kostl,prctr FROM csks INTO TABLE @DATA(lt_prctr)
    FOR ALL ENTRIES IN @lt_cc_valid
    WHERE kostl = @lt_cc_valid-cost_center AND kokrs = '1000'.
    SORT lt_prctr BY kostl.

    "Get company code from profit center
    SELECT bukrs,prctr FROM cepc_bukrs INTO TABLE @DATA(lt_comcode)
    FOR ALL ENTRIES IN @lt_prctr
    WHERE prctr = @lt_prctr-prctr AND kokrs = '1000'.
    SORT lt_comcode BY prctr.
    "Get t030k
    SELECT * FROM t030k INTO TABLE @DATA(lt_tax)
      WHERE ktopl = '1000'
        AND ktosl = 'VST'
        AND mwskz IN ( 'I0', 'I1', 'I5' ).
*
    "Check bk_date bkc1 and return all error
    SELECT * FROM t001b INTO TABLE gt_open_posting WHERE bukrs = '1000' AND rrcty = '0'.
    DELETE ADJACENT DUPLICATES FROM gt_open_posting COMPARING frye1 frpe1 toye1 tope1.
*

    input_cop = input.

    "Check error
    output-mt_bkcptx_n_receive-ev_error = '1'.
    LOOP AT input-mt_bkcptx_n_sender-header INTO DATA(ls_input_header_c2).
      LOOP AT ls_input_header_c2-item INTO DATA(ls_input_header_c1).
        IF me->check_open_posting_date( |{ ls_input_header_c1-bk_date+0(6) }| ) = abap_false.
          output-mt_bkcptx_n_receive-ev_error = '0'.
          IF <fs_err_input> IS NOT ASSIGNED.
            APPEND INITIAL LINE TO output-mt_bkcptx_n_receive-err_input ASSIGNING <fs_err_input>.
          ENDIF.
          <fs_err_input>-bk_no2 = ls_input_header_c2-bk_id.
          APPEND INITIAL LINE TO <fs_err_input>-bkc1 ASSIGNING FIELD-SYMBOL(<fs_err_input_bkc1>).
          <fs_err_input_bkc1>-bk_no1 = ls_input_header_c1-bk_id.
          APPEND INITIAL LINE TO lt_mapping_bk ASSIGNING FIELD-SYMBOL(<fs_mapping_bk>).
          <fs_mapping_bk>-bkc2 = <fs_err_input>-bk_no2.
          <fs_mapping_bk>-bkc1 = <fs_err_input_bkc1>-bk_no1.
          lw_str = |Bảng kê { ls_input_header_c1-bk_id } đã đến kỳ khóa sổ hạch toán|.
          <fs_err_input_bkc1>-error = lw_str.
          <fs_err_input_bkc1>-err_code = '1'.
        ELSE.
          READ TABLE lt_prctr INTO DATA(ls_prctr) WITH KEY kostl = ls_input_header_c1-cost_center BINARY SEARCH.
          IF sy-subrc NE 0.
            IF <fs_err_input> IS NOT ASSIGNED.
              APPEND INITIAL LINE TO output-mt_bkcptx_n_receive-err_input ASSIGNING <fs_err_input>.
            ENDIF.
            <fs_err_input>-bk_no2 = ls_input_header_c2-bk_id.
            APPEND INITIAL LINE TO <fs_err_input>-bkc1 ASSIGNING <fs_err_input_bkc1>.
            <fs_err_input_bkc1>-bk_no1 = ls_input_header_c1-bk_id.
            APPEND INITIAL LINE TO lt_mapping_bk ASSIGNING <fs_mapping_bk>.
            <fs_mapping_bk>-bkc2 = <fs_err_input>-bk_no2.
            <fs_mapping_bk>-bkc1 = <fs_err_input_bkc1>-bk_no1.
            lw_str = 'Sai mã bưu cục, không tìm thấy profit center phù hợp'.
          ELSE.
            READ TABLE lt_comcode INTO DATA(ls_comcode) WITH KEY prctr = ls_prctr-prctr BINARY SEARCH.
            IF sy-subrc NE 0.
              IF <fs_err_input> IS NOT ASSIGNED.
                APPEND INITIAL LINE TO output-mt_bkcptx_n_receive-err_input ASSIGNING <fs_err_input>.
              ENDIF.
              <fs_err_input>-bk_no2 = ls_input_header_c2-bk_id.
              APPEND INITIAL LINE TO <fs_err_input>-bkc1 ASSIGNING <fs_err_input_bkc1>.
              <fs_err_input_bkc1>-bk_no1 = ls_input_header_c1-bk_id.
              APPEND INITIAL LINE TO lt_mapping_bk ASSIGNING <fs_mapping_bk>.
              <fs_mapping_bk>-bkc2 = <fs_err_input>-bk_no2.
              <fs_mapping_bk>-bkc1 = <fs_err_input_bkc1>-bk_no1.
              lw_str = 'Không tìm thấy Company code từ Profit center'.
            ELSE.
              lw_posting_date = ls_input_header_c1-bk_date.
              CALL FUNCTION 'BKK_RFC_GL_FI_PERIOD_CHECK'
                EXPORTING
                  i_bukrs = ls_comcode-bukrs
                  i_koart = 'K' " Vendor
                  i_budat = lw_posting_date
                IMPORTING
                  e_rc    = lw_erc
                  return  = lw_return
                TABLES
                  t_konto = lt_konto.
              IF sy-subrc NE 0.
                IF <fs_err_input> IS NOT ASSIGNED.
                  APPEND INITIAL LINE TO output-mt_bkcptx_n_receive-err_input ASSIGNING <fs_err_input>.
                ENDIF.
                <fs_err_input>-bk_no2 = ls_input_header_c2-bk_id.
                APPEND INITIAL LINE TO <fs_err_input>-bkc1 ASSIGNING <fs_err_input_bkc1>.
                <fs_err_input_bkc1>-bk_no1 = ls_input_header_c1-bk_id.
                APPEND INITIAL LINE TO lt_mapping_bk ASSIGNING <fs_mapping_bk>.
                <fs_mapping_bk>-bkc2 = <fs_err_input>-bk_no2.
                <fs_mapping_bk>-bkc1 = <fs_err_input_bkc1>-bk_no1.
                lw_str = 'Kỳ kế toán đang đóng. Chứng từ không hợp lệ'.
              ENDIF.
            ENDIF.
          ENDIF.
          "Hoa don
          IF 1 = 2."comment
            DATA(lw_check_out) = abap_false.
            LOOP AT input-mt_bkcptx_n_sender-hoa_don INTO DATA(ls_input_hoa_don) WHERE bk1_id = ls_input_header_c1-bk_id.
              "line hdon
              LOOP AT ls_hoa_don-item INTO ls_hoa_don_i.
                SELECT SINGLE * FROM t001b INTO @DATA(ls_t001b) WHERE vkont <= @ls_hoa_don_i-tk_hach_toan AND bkont >= @ls_hoa_don_i-tk_hach_toan AND bkont <> 'ZZZZZZZZZZ'.
                IF sy-subrc NE 0.
                  lw_check_out = abap_true.
                  IF <fs_err_input> IS NOT ASSIGNED.
                    APPEND INITIAL LINE TO output-mt_bkcptx_n_receive-err_input ASSIGNING <fs_err_input>.
                  ENDIF.
                  <fs_err_input>-bk_no2 = ls_input_header_c2-bk_id.
                  APPEND INITIAL LINE TO <fs_err_input>-bkc1 ASSIGNING <fs_err_input_bkc1>.
                  <fs_err_input_bkc1>-bk_no1 = ls_input_header_c1-bk_id.
                  APPEND INITIAL LINE TO lt_mapping_bk ASSIGNING <fs_mapping_bk>.
                  <fs_mapping_bk>-bkc2 = <fs_err_input>-bk_no2.
                  <fs_mapping_bk>-bkc1 = <fs_err_input_bkc1>-bk_no1.
                  IF lw_str IS INITIAL.
                    lw_str = |Tài khoản hạch toán { ls_hoa_don_i-tk_hach_toan } không hợp lệ |.
                  ELSE.
                    lw_str = |{ lw_str }{ '|' }Tài khoản hạch toán { ls_hoa_don_i-tk_hach_toan } không hợp lệ|.
                  ENDIF.
                  EXIT.
                ENDIF.
              ENDLOOP.
              CHECK lw_check_out = abap_false.
              "phan bo
              LOOP AT ls_hoa_don-phan_bo INTO ls_phan_bo.
                READ TABLE lt_prctr INTO ls_prctr WITH KEY kostl = ls_phan_bo-cc_pb BINARY SEARCH.
                IF sy-subrc NE 0.
                  lw_check_out = abap_true.
                  IF <fs_err_input> IS NOT ASSIGNED.
                    APPEND INITIAL LINE TO output-mt_bkcptx_n_receive-err_input ASSIGNING <fs_err_input>.
                  ENDIF.
                  <fs_err_input>-bk_no2 = ls_input_header_c2-bk_id.
                  APPEND INITIAL LINE TO <fs_err_input>-bkc1 ASSIGNING <fs_err_input_bkc1>.
                  <fs_err_input_bkc1>-bk_no1 = ls_input_header_c1-bk_id.
                  APPEND INITIAL LINE TO lt_mapping_bk ASSIGNING <fs_mapping_bk>.
                  <fs_mapping_bk>-bkc2 = <fs_err_input>-bk_no2.
                  <fs_mapping_bk>-bkc1 = <fs_err_input_bkc1>-bk_no1.
                  IF lw_str IS INITIAL.
                    lw_str = |{ 'Sai mã bưu cục, không tìm thấy profit center phù hợp' }|.
                  ELSE.
                    lw_str = |{ lw_str }{ '|' }{ 'Sai mã bưu cục, không tìm thấy profit center phù hợp' }|.
                  ENDIF.
                  EXIT.
                ELSE.
                  READ TABLE lt_comcode INTO ls_comcode WITH KEY prctr = ls_prctr-prctr BINARY SEARCH.
                  IF sy-subrc NE 0.
                    lw_check_out = abap_true.
                    IF <fs_err_input> IS NOT ASSIGNED.
                      APPEND INITIAL LINE TO output-mt_bkcptx_n_receive-err_input ASSIGNING <fs_err_input>.
                    ENDIF.
                    <fs_err_input>-bk_no2 = ls_input_header_c2-bk_id.
                    APPEND INITIAL LINE TO <fs_err_input>-bkc1 ASSIGNING <fs_err_input_bkc1>.
                    <fs_err_input_bkc1>-bk_no1 = ls_input_header_c1-bk_id.
                    APPEND INITIAL LINE TO lt_mapping_bk ASSIGNING <fs_mapping_bk>.
                    <fs_mapping_bk>-bkc2 = <fs_err_input>-bk_no2.
                    <fs_mapping_bk>-bkc1 = <fs_err_input_bkc1>-bk_no1.
                    IF lw_str IS INITIAL.
                      lw_str = |{ 'Không tìm thấy Company code từ Profit center' }|.
                    ELSE.
                      lw_str = |{ lw_str }{ '|' }{ 'Không tìm thấy Company code từ Profit center' }|.
                    ENDIF.
                  ENDIF.
                  EXIT.
                ENDIF.

              ENDLOOP.
              IF lw_check_out = abap_true.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDIF.
          IF <fs_err_input_bkc1> IS ASSIGNED.
            output-mt_bkcptx_n_receive-ev_error = 0.
            <fs_err_input_bkc1>-error = lw_str.
            <fs_err_input_bkc1>-err_code = '2'.
          ENDIF.
        ENDIF.
      ENDLOOP .
      UNASSIGN <fs_err_input>.
    ENDLOOP.


    "set error input bkc2
    LOOP AT output-mt_bkcptx_n_receive-err_input INTO DATA(ls_err_input).
      READ TABLE lt_bkcptx_hdr WITH KEY type = '02' bk_id = ls_err_input-bk_no2 ASSIGNING FIELD-SYMBOL(<fs_bkcptx_hdr>).
      IF sy-subrc = 0.
        <fs_bkcptx_hdr>-status = 0.
      ENDIF.
    ENDLOOP.

    INSERT ztb_bkcptx_hdr_n FROM TABLE lt_bkcptx_hdr.
    COMMIT WORK AND WAIT.
    INSERT ztb_kcptx_hdon_n FROM TABLE lt_bkcptx_hdon.
    COMMIT WORK AND WAIT.
    INSERT ztb_kcptx_hdi_n FROM TABLE lt_bkcptx_hdo_i.
    COMMIT WORK AND WAIT.
    INSERT ztb_cptx_hd_pb_n FROM TABLE lt_bkcptx_pb.
    COMMIT WORK AND WAIT.

    CLEAR lw_hdon_line.
    LOOP AT lt_mapping_bk INTO DATA(ls_mapping_bk).
      DELETE input_cop-mt_bkcptx_n_sender-header WHERE bk_id = ls_mapping_bk-bkc2.
      DELETE input_cop-mt_bkcptx_n_sender-hoa_don WHERE bk1_id = ls_mapping_bk-bkc1.
    ENDLOOP.

    "Hach toan
    CHECK input_cop-mt_bkcptx_n_sender-hoa_don IS NOT INITIAL.
    DATA : lw_post_sucess   TYPE abap_bool,
           lw_error_hdo_str TYPE string.
    LOOP AT input_cop-mt_bkcptx_n_sender-header INTO ls_header_c2.
      READ TABLE lt_bkcptx_hdr INTO DATA(ls_bkcptx_hdr_c2_u) WITH KEY type = '02' bk_id = ls_header_c2-bk_id.
      CHECK sy-subrc = 0.
      lw_post_sucess = abap_true.
      LOOP AT ls_header_c2-item INTO ls_header_c1 .
        LOOP AT input_cop-mt_bkcptx_n_sender-hoa_don INTO ls_hoa_don WHERE bk1_id = ls_header_c1-bk_id.
          hdon_i = ls_hoa_don-item.
          phan_bo = ls_hoa_don-phan_bo.
          CLEAR : lw_name1,lw_name2,lw_name3,lw_name4,lw_nguoi_ban.
          READ TABLE lt_bkcptx_hdr INTO DATA(ls_bkcptx_hdr) WITH KEY type = '01' bk_id = ls_hoa_don-bk1_id.
          CHECK sy-subrc = 0.
          READ TABLE lt_prctr INTO ls_prctr WITH KEY kostl = ls_bkcptx_hdr-cost_center BINARY SEARCH.
          CHECK sy-subrc = 0.
*         lt_exten2 = VALUE #( ( structure = 'XREF1' valuepart1 = ls_prctr-prctr ) ).
          READ TABLE lt_comcode INTO ls_comcode WITH KEY prctr = ls_prctr-prctr BINARY SEARCH.
          CHECK sy-subrc = 0.
          READ TABLE lt_bkcptx_hdon INTO DATA(ls_bkcptx_hdo) WITH KEY guid_hdr = ls_bkcptx_hdr-guid uuid_hd = ls_hoa_don-uuid_hd.
          CHECK sy-subrc = 0.
          lw_posting_date = CONV #( ls_bkcptx_hdr-bk_date ).
          lw_ngay_hd = ls_hoa_don-ngay_hd.


          ls_header = VALUE #( doc_date   = lw_ngay_hd
                              pstng_date = lw_posting_date
                              doc_type   = 'Z1'
                              doc_status = '4'
                              username   = sy-uname
*                               fis_period = lw_post_date+4(2)
                              comp_code  = ls_comcode-bukrs
                              ref_doc_no = |{ ls_hoa_don-kihieu_hd }#{ ls_hoa_don-so_hd }|
                              header_txt = ls_bkcptx_hdr-bk_id
                               ).
          lw_nguoi_ban = ls_hoa_don-nguoi_ban.
          CALL FUNCTION 'TEXT_SPLIT'
            EXPORTING
              length       = 35
              text         = lw_nguoi_ban
              as_character = 'X'
            IMPORTING
              line         = lw_name1
              rest         = lw_nguoi_ban.

          CALL FUNCTION 'TEXT_SPLIT'
            EXPORTING
              length       = 35
              text         = lw_nguoi_ban
              as_character = 'X'
            IMPORTING
              line         = lw_name2
              rest         = lw_nguoi_ban.
          CALL FUNCTION 'TEXT_SPLIT'
            EXPORTING
              length       = 35
              text         = lw_nguoi_ban
              as_character = 'X'
            IMPORTING
              line         = lw_name3
              rest         = lw_nguoi_ban.
          CALL FUNCTION 'TEXT_SPLIT'
            EXPORTING
              length       = 35
              text         = lw_nguoi_ban
              as_character = 'X'
            IMPORTING
              line         = lw_name4
              rest         = lw_nguoi_ban.

          ls_customercpd = VALUE #( name        = lw_name1
                                    name_2      = lw_name2
                                    name_3      = lw_name3
                                    name_4      = lw_name4
                                    langu_iso   = 'VI'
                                    city        = 'Hà nội'
                                    country     = 'VN'
                                    bank_ctry   = 'VN'
                                    tax_no_1    = ls_hoa_don-mst ).

          "TH khong phan bo
          IF phan_bo[] IS INITIAL.
            CALL METHOD me->posting_without_allocate
              EXPORTING
                posting_date      = lw_posting_date
                cost_center       = CONV #( ls_bkcptx_hdr-cost_center )
                profit_center     = ls_prctr-prctr
                bk_id             = CONV #( ls_bkcptx_hdr-bk_id )
                user_id           = CONV #( ls_bkcptx_hdr-user_id )
                ma_nv_hd          = ls_hoa_don-ma_nv
                tk_cong_no        = ls_hoa_don-tk_cong_no
                tax_amount        = ls_hoa_don-tax_amount
*               phu_phi           =
              IMPORTING
                lt_accountgl      = lt_accountgl
                lt_accountpayable = lt_accountpayable
                lt_accounttax     = lt_accounttax
                lt_currencyamount = lt_currencyamount
                lt_extension1     = lt_extension1
                lt_ext            = lt_ext
              CHANGING
                ls_accountgl      = ls_accountgl
                ls_accountpayable = ls_accountpayable
                ls_accounttax     = ls_accounttax
                ls_extension1     = ls_extension1
                ls_ext            = ls_ext
                ls_amount         = ls_amount.
          ELSE.
            "TH có PB
            CALL METHOD posting_with_allocate
              EXPORTING
                posting_date      = lw_posting_date
                cost_center       = CONV #( ls_bkcptx_hdr-cost_center )
                profit_center     = ls_prctr-prctr
                bk_id             = CONV #( ls_bkcptx_hdr-bk_id )
                user_id           = CONV #( ls_bkcptx_hdr-user_id )
                ma_nv_hd          = ls_hoa_don-ma_nv
                tk_cong_no        = ls_hoa_don-tk_cong_no
                tax_amount        = ls_hoa_don-tax_amount
*               phu_phi           =
              IMPORTING
                lt_accountgl      = lt_accountgl
                lt_accountpayable = lt_accountpayable
                lt_accounttax     = lt_accounttax
                lt_currencyamount = lt_currencyamount
                lt_extension1     = lt_extension1
                lt_ext            = lt_ext.
          ENDIF.


          CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
            EXPORTING
              documentheader = ls_header
              customercpd    = ls_customercpd
            TABLES
              accountgl      = lt_accountgl
              accountpayable = lt_accountpayable
              accounttax     = lt_accounttax
              currencyamount = lt_currencyamount
              extension1     = lt_ext
*             extension2     = lt_exten2
              return         = lt_return.
          READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
          IF sy-subrc = 0.
            output-mt_bkcptx_n_receive-ev_error = '0'.
            DELETE lt_return INDEX 1.
            SORT lt_return.
            DELETE ADJACENT DUPLICATES FROM lt_return COMPARING number.

            IF <fs_err_acct> IS NOT ASSIGNED.
              APPEND INITIAL LINE TO output-mt_bkcptx_n_receive-err_acct ASSIGNING <fs_err_acct>.
            ENDIF.
            <fs_err_acct>-bk_no2 = ls_header_c2-bk_id.

            IF <fs_err_bck1> IS NOT ASSIGNED.
              APPEND INITIAL LINE TO <fs_err_acct>-bkc1 ASSIGNING <fs_err_bck1>.
            ENDIF.
            <fs_err_bck1>-bk_no1 = ls_header_c1-bk_id.

*           IF <fs_err_hdon> IS NOT ASSIGNED.
            APPEND INITIAL LINE TO <fs_err_bck1>-hdon ASSIGNING <fs_err_hdon>.
**           ENDIF.
*            <fs_err_hdon>-kihieu_hd = ls_hoa_don-kihieu_hd.
*            <fs_err_hdon>-so_hd = ls_hoa_don-so_hd.
*            <fs_err_hdon>-mst = ls_hoa_don-mst.
            <fs_err_hdon>-id = ls_hoa_don-uuid_hd.

*           <fs_err_acct_bkc1>-err_code = 2.
            LOOP AT lt_return INTO ls_return WHERE type = 'E'.
              CONCATENATE <fs_err_hdon>-error ls_return-message INTO <fs_err_hdon>-error SEPARATED BY '|'.
              CONCATENATE lw_error_hdo_str <fs_err_hdon>-error  INTO lw_error_hdo_str SEPARATED BY '-'.
            ENDLOOP.
*           ls_bkcptx_hdr-status  = '0'.
*           ls_bkcptx_hdr-status_des =  <fs_err_hdon>-error.
            lw_post_sucess = abap_false.
*           MODIFY ZTB_BKCPTX_HDR_N FROM ls_bkcptx_hdr.
*           COMMIT WORK AND WAIT.

          ELSE.
            CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
              EXPORTING
                documentheader = ls_header
                customercpd    = ls_customercpd
              IMPORTING
                obj_key        = lw_obj_key
              TABLES
                accountgl      = lt_accountgl
                accountpayable = lt_accountpayable
                accounttax     = lt_accounttax
                currencyamount = lt_currencyamount
                extension1     = lt_ext
                extension2     = lt_exten2
                return         = lt_return.

            READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
            IF sy-subrc NE 0.
*             APPEND INITIAL LINE TO output-mt_bkcptx_n_receive-bkc2 ASSIGNING FIELD-SYMBOL(<fs_bkc2_success>).
*             <fs_bkc2_success> = ls_header_c2-bk_id.
              output-mt_bkcptx_n_receive-fi_doc = |{ output-mt_bkcptx_n_receive-fi_doc }/{ lw_obj_key(10) }|.
              ls_bkcptx_hdo-belnr = lw_obj_key(10).
              MODIFY ztb_kcptx_hdon_n FROM ls_bkcptx_hdo.
              COMMIT WORK AND WAIT.
            ENDIF.
          ENDIF.
          CLEAR: ls_header,lw_obj_key,
                 ls_customercpd,
*                ls_bkcptx_hdr,
                 ls_bkcptx_hdo,
                 ls_hoa_don.

          REFRESH: lt_accountgl[],
                   lt_accountpayable[],
                   lt_accounttax[],
                   lt_currencyamount[],
                   lt_ext[],lt_exten2[],
                   lt_return[].
          UNASSIGN <fs_err_hdon>.
        ENDLOOP.
        IF lw_post_sucess = abap_false.
          ls_bkcptx_hdr-status  = '0'.
          ls_bkcptx_hdr-status_des =  lw_error_hdo_str.
          MODIFY ztb_bkcptx_hdr_n FROM ls_bkcptx_hdr.
          COMMIT WORK AND WAIT.
        ENDIF.
        UNASSIGN <fs_err_bck1>.
        CLEAR :lw_error_hdo_str,ls_bkcptx_hdr.
      ENDLOOP.
      UNASSIGN <fs_err_acct>.
      IF lw_post_sucess = abap_false.
        ls_bkcptx_hdr_c2_u-status = '0'.
        MODIFY ztb_bkcptx_hdr_n FROM  ls_bkcptx_hdr_c2_u.
        COMMIT WORK AND WAIT.
      ELSE.
        APPEND INITIAL LINE TO output-mt_bkcptx_n_receive-bk2 ASSIGNING FIELD-SYMBOL(<fs_bkc2_success>).
        <fs_bkc2_success> = ls_header_c2-bk_id.
      ENDIF.
      UNASSIGN <fs_err_acct>.
      CLEAR :lw_post_sucess,ls_bkcptx_hdr_c2_u.
    ENDLOOP.
    CHECK output-mt_bkcptx_n_receive-bk2 IS NOT INITIAL.
    DELETE ADJACENT DUPLICATES FROM output-mt_bkcptx_n_receive-bk2.

  ENDMETHOD.
