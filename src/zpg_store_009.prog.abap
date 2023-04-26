*&---------------------------------------------------------------------*
*& Report ZPG_STORE_009
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_STORE_009.
  METHOD zii_si_bkcptx_ib~si_bkcptx_ib.
*** **** INSERT IMPLEMENTATION HERE **** ***
*Add by THANHNT 20.10.2022 -->Start
*    CALL FUNCTION 'ZFM_BKCPTX'
*      EXPORTING
*        input  = input
*      IMPORTING
*        output = output.
*    RETURN.
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
      lt_bkcptx_hdon   TYPE TABLE OF ztb_bkcptx_hdon,
      lt_bkcptx_hdo_i  TYPE TABLE OF ztb_bkcptx_hdo_i,
      lt_bkcptx_hdr    TYPE TABLE OF ztb_bkcptx_hdr,
      lt_bkcptx_pb     TYPE TABLE OF ztb_cptx_hdo_pb,
      ls_bkcptx_hdr_c2 TYPE ztb_bkcptx_hdr,
      ls_bkcptx_hdr_c1 TYPE ztb_bkcptx_hdr,
      ls_bkcptx_hdon   TYPE ztb_bkcptx_hdon,
      ls_bkcptx_hdo_i  TYPE ztb_bkcptx_hdo_i,
      ls_bkcptx_pb     TYPE ztb_cptx_hdo_pb,
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
          ls_ext            TYPE bapiacextc.

    DATA: lw_name1     TYPE name1_gp,
          lw_name2     TYPE name1_gp,
          lw_name3     TYPE name1_gp,
          lw_name4     TYPE name1_gp,
          lw_nguoi_ban TYPE text150.

    DATA: lt_pi_header   TYPE zdt_bkcptx_sender_header_tab,
          lt_pi_hoa_don  TYPE zdt_bkcptx_sender_hoa_don_tab,
*        lt_pi_phanbo   TYPE zdt_bkcptx_sender_phan_bo_tab,
          lt_item_header TYPE zdt_bkcptx_sender_item_tab1.
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
             item TYPE zdt_bkcptx_sender_item_tab1,
           END OF lty_item_hc1.
    TYPES :BEGIN OF lty_phan_bo,
             phan_bo TYPE zdt_bkcptx_sender_phan_bo_tab,
           END OF  lty_phan_bo.
    TYPES :BEGIN OF lty_cc_prctr_comp,
             cc    TYPE kostl,
             prctr TYPE prctr,
             comp  TYPE bukrs,
           END OF  lty_cc_prctr_comp.
    TYPES :BEGIN OF lty_item_hd,
             item_hd TYPE zdt_bkcptx_sender_item_tab,
           END OF lty_item_hd.

    TYPES :BEGIN OF lty_bk_ref_value,
             line_hd_2 TYPE int4,
             line_hd_1 TYPE int4,
             value     TYPE ztb_bkcptx_hdr,
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

    DATA:
      lt_konto  TYPE TABLE OF t001b,
      lw_return TYPE bapiret2,
      lw_erc    TYPE sy-subrc.

*  lt_phan_bo1 =  VALUE #( FOR data IN input-mt_bkcptx_sender-hoa_don ( phan_bo =  data-phan_bo ) ).
*  LOOP AT lt_phan_bo1 INTO DATA(ls_tesss).
*    DESCRIBE TABLE ls_tesss-phan_bo LINES DATA(test).
*  ENDLOOP.



    "Inset data to tables from input

    LOOP AT input-mt_bkcptx_sender-header INTO DATA(ls_header_c2).
      lw_hdrc2_line = sy-tabix.
      MOVE-CORRESPONDING ls_header_c2 TO ls_bkcptx_hdr_c2.
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_16 = ls_bkcptx_hdr_c2-guid.
      ls_bkcptx_hdr_c2-type = '02'.
      APPEND ls_bkcptx_hdr_c2 TO lt_bkcptx_hdr.

      "bkc1
      LOOP AT ls_header_c2-item INTO DATA(ls_header_c1).
        lw_hdrc1_line = sy-tabix.
        MOVE-CORRESPONDING ls_header_c1 TO ls_bkcptx_hdr_c1.
        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            ev_guid_16 = ls_bkcptx_hdr_c1-guid.
        ls_bkcptx_hdr_c1-type = '01'.
        APPEND ls_bkcptx_hdr_c1 TO lt_bkcptx_hdr.
      ENDLOOP.
    ENDLOOP.
    "hoa don
    LOOP AT input-mt_bkcptx_sender-hoa_don INTO DATA(ls_hoa_don).
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

*    lt_item_hd = CORRESPONDING #( input-mt_bkcptx_sender-hoa_don ).
*
*    delete lt_item_hd where
*    EXPORT item_hd =  lt_item_hd TO DATABASE indx(nt) CLIENT sy-mandt ID 'ITM'.
    "Add all cost center to internal table from BKC1 and BKC2
    lt_cc_valid = CORRESPONDING #( input-mt_bkcptx_sender-header ).
    lt_item_header_c1 = CORRESPONDING #( input-mt_bkcptx_sender-header ).
    lt_phan_bo = CORRESPONDING #( input-mt_bkcptx_sender-hoa_don ).
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
    "Check valid header
    LOOP AT input-mt_bkcptx_sender-header INTO ls_header_c2.
      lw_hdrc2_line = sy-tabix.
      lw_posting_date = ls_header_c2-bk_date.
      cl_isu_date_check=>date_check_plausibility(
      EXPORTING
        x_date = lw_posting_date
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS = 2 ).
      IF sy-subrc <> 0.
        lw_str = |Ngày hạch toán BKC2 { lw_posting_date } không hợp lệ. |.
        output-mt_bkcptx_receiver-ev_error = '0'.
        output-mt_bkcptx_receiver-ev_message = lw_str.
        RETURN.
      ENDIF.

      READ TABLE lt_prctr INTO DATA(ls_prctr) WITH KEY kostl = ls_header_c2-cost_center BINARY SEARCH.
      IF sy-subrc NE 0.
        output-mt_bkcptx_receiver-ev_error = '0'.
        output-mt_bkcptx_receiver-ev_message = 'Sai mã bưu cục, không tìm thấy profit center phù hợp'.
        RETURN.
      ELSE.
        READ TABLE lt_comcode INTO DATA(ls_comcode) WITH KEY prctr = ls_prctr-prctr BINARY SEARCH.
        IF sy-subrc NE 0.
          output-mt_bkcptx_receiver-ev_error = '0'.
          output-mt_bkcptx_receiver-ev_message = 'Không tìm thấy Company code từ Profit center'.
          RETURN.
        ENDIF.
      ENDIF.

      "Check FI period is Open or not

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

      IF lw_erc NE 0.
        output-mt_bkcptx_receiver-ev_error = '0'.
        output-mt_bkcptx_receiver-ev_message = 'Kỳ kế toán đang đóng. Chứng từ không hợp lệ'.
        RETURN.
      ENDIF.
      CLEAR : lw_erc,lw_return.
      REFRESH lt_konto.

      LOOP AT ls_header_c2-item INTO ls_header_c1 .
        lw_hdrc1_line = sy-tabix.
        lw_posting_date = ls_header_c1-bk_date.
        cl_isu_date_check=>date_check_plausibility(
        EXPORTING
          x_date = lw_posting_date
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS = 2 ).
        IF sy-subrc <> 0.
          lw_str = |Ngày hạch toán BKC1 { lw_posting_date } không hợp lệ. |.
          output-mt_bkcptx_receiver-ev_error = '0'.
          output-mt_bkcptx_receiver-ev_message = lw_str.
          RETURN.
        ENDIF.

        READ TABLE lt_prctr INTO ls_prctr WITH KEY kostl = ls_header_c1-cost_center BINARY SEARCH.
        IF sy-subrc NE 0.
          output-mt_bkcptx_receiver-ev_error = '0'.
          output-mt_bkcptx_receiver-ev_message = 'Sai mã bưu cục, không tìm thấy profit center phù hợp'.
          RETURN.
        ELSE.
          READ TABLE lt_comcode INTO ls_comcode WITH KEY prctr = ls_prctr-prctr BINARY SEARCH.
          IF sy-subrc NE 0.
            output-mt_bkcptx_receiver-ev_error = '0'.
            output-mt_bkcptx_receiver-ev_message = 'Không tìm thấy Company code từ Profit center'.
            RETURN.
          ENDIF.

        ENDIF.

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
        IF lw_erc NE 0.
          output-mt_bkcptx_receiver-ev_error = '0'.
          output-mt_bkcptx_receiver-ev_message = 'Kỳ kế toán đang đóng. Chứng từ không hợp lệ'.
          RETURN.
        ENDIF.
        CLEAR : lw_erc,lw_return.
        REFRESH lt_konto.
      ENDLOOP.
    ENDLOOP.
    "Check valid hoa don
    LOOP AT input-mt_bkcptx_sender-hoa_don INTO ls_hoa_don.
      lw_ngay_hd = ls_hoa_don-ngay_hd.
      cl_isu_date_check=>date_check_plausibility(
      EXPORTING
        x_date = lw_ngay_hd
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS = 2 ).
      IF sy-subrc <> 0.
        lw_str = |Ngày hợp đồng {  lw_ngay_hd } không hợp lệ. |.
        output-mt_bkcptx_receiver-ev_error = '0'.
        output-mt_bkcptx_receiver-ev_message = lw_str.
        RETURN.
      ENDIF.

      LOOP AT ls_hoa_don-phan_bo INTO ls_phan_bo.
        READ TABLE lt_prctr INTO ls_prctr WITH KEY kostl = ls_phan_bo-cc_pb BINARY SEARCH.
        IF sy-subrc NE 0.
          output-mt_bkcptx_receiver-ev_error = '0'.
          output-mt_bkcptx_receiver-ev_message = 'Sai mã bưu cục, không tìm thấy profit center phù hợp'.
          RETURN.
        ELSE.
          READ TABLE lt_comcode INTO ls_comcode WITH KEY prctr = ls_prctr-prctr BINARY SEARCH.
          IF sy-subrc NE 0.
            output-mt_bkcptx_receiver-ev_error = '0'.
            output-mt_bkcptx_receiver-ev_message = 'Không tìm thấy Company code từ Profit center'.
            RETURN.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.


    INSERT ztb_bkcptx_hdr FROM TABLE lt_bkcptx_hdr.
    INSERT ztb_bkcptx_hdon FROM TABLE lt_bkcptx_hdon.
    INSERT ztb_bkcptx_hdo_i FROM TABLE lt_bkcptx_hdo_i.
    INSERT ztb_cptx_hdo_pb FROM TABLE lt_bkcptx_pb.
    COMMIT WORK.

    CLEAR lw_hdon_line.
    "Hach toan
    LOOP AT input-mt_bkcptx_sender-hoa_don INTO ls_hoa_don.
      lw_hdon_line = sy-tabix.
      hdon_i = ls_hoa_don-item.
      phan_bo = ls_hoa_don-phan_bo.
*      REFRESH :lt_accountgl,lt_accountpayable,lt_accounttax,lt_currencyamount,lt_extension1,lt_ext.
      CLEAR : lw_name1,lw_name2,lw_name3,lw_name4,lw_nguoi_ban.
      READ TABLE lt_bkcptx_hdr INTO DATA(ls_bkcptx_hdr) WITH KEY type = '01' bk_id = ls_hoa_don-bk1_id.
      CHECK sy-subrc = 0.
      READ TABLE lt_prctr INTO ls_prctr WITH KEY kostl = ls_bkcptx_hdr-cost_center BINARY SEARCH.
      CHECK sy-subrc = 0.
      READ TABLE lt_comcode INTO ls_comcode WITH KEY prctr = ls_prctr-prctr BINARY SEARCH.
      CHECK sy-subrc = 0.
      READ TABLE lt_bkcptx_hdon INTO DATA(ls_bkcptx_hdo) WITH KEY guid_hdr = ls_bkcptx_hdr-guid hdon_id = lw_hdon_line.
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
            bk_id             = ls_bkcptx_hdr-bk_id
            user_id           = ls_bkcptx_hdr-user_id
            ma_nv_hd          = ls_hoa_don-ma_nv
            tk_cong_no        = ls_hoa_don-tk_cong_no
            tax_amount        = ls_hoa_don-tax_amount
*           phu_phi           =
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
            bk_id             = ls_bkcptx_hdr-bk_id
            user_id           = ls_bkcptx_hdr-user_id
            ma_nv_hd          = ls_hoa_don-ma_nv
            tk_cong_no        = ls_hoa_don-tk_cong_no
            tax_amount        = ls_hoa_don-tax_amount
*           phu_phi           =
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
          return         = lt_return.



      READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
      IF sy-subrc = 0.
        output-mt_bkcptx_receiver-ev_message = lw_hdon_line.
        CONDENSE output-mt_bkcptx_receiver-ev_message.
        CONCATENATE 'HD line' output-mt_bkcptx_receiver-ev_message INTO output-mt_bkcptx_receiver-ev_message.
        output-mt_bkcptx_receiver-ev_error = '0'.
        LOOP AT lt_return INTO ls_return
          WHERE type = 'E'.
          CONCATENATE output-mt_bkcptx_receiver-ev_message ls_return-message INTO output-mt_bkcptx_receiver-ev_message SEPARATED BY '|'.
        ENDLOOP.
        ls_bkcptx_hdr-status = output-mt_bkcptx_receiver-ev_error.
        ls_bkcptx_hdr-status_des = output-mt_bkcptx_receiver-ev_message.
        MODIFY ztb_bkcptx_hdr FROM ls_bkcptx_hdr.
        COMMIT WORK AND WAIT.
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
            return         = lt_return.
        READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
        IF sy-subrc = 0.
          output-mt_bkcptx_receiver-ev_message = lw_hdon_line.
          CONDENSE output-mt_bkcptx_receiver-ev_message.
          CONCATENATE 'HD line' output-mt_bkcptx_receiver-ev_message INTO output-mt_bkcptx_receiver-ev_message.
          output-mt_bkcptx_receiver-ev_error = '0'.
          LOOP AT lt_return INTO ls_return
            WHERE type = 'E'.
            CONCATENATE output-mt_bkcptx_receiver-ev_message ls_return-message INTO output-mt_bkcptx_receiver-ev_message SEPARATED BY '|'.
          ENDLOOP.
          ls_bkcptx_hdr-status = output-mt_bkcptx_receiver-ev_error.
          ls_bkcptx_hdr-status_des = output-mt_bkcptx_receiver-ev_message.
          MODIFY ztb_bkcptx_hdr FROM ls_bkcptx_hdr.
          COMMIT WORK AND WAIT.
        ELSE.
*          ztb_bkcptx_hdon
          output-mt_bkcptx_receiver-ev_error = '1'.
          output-mt_bkcptx_receiver-ev_message = |{ output-mt_bkcptx_receiver-ev_message }/{ lw_obj_key(10) }|.
          ls_bkcptx_hdo-belnr = lw_obj_key(10).
          MODIFY ztb_bkcptx_hdon FROM ls_bkcptx_hdo.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
      CLEAR: ls_header,
             ls_customercpd,
             ls_bkcptx_hdr,
             ls_bkcptx_hdo.
      REFRESH: lt_accountgl[],
               lt_accountpayable[],
               lt_accounttax[],
               lt_currencyamount[],
               lt_ext[],
               lt_return[].
    ENDLOOP.
  ENDMETHOD.
