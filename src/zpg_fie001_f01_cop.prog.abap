*&---------------------------------------------------------------------*
*&  Include           ZPG_FIE002_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_data .
  DATA: lw_line TYPE i.
  DATA: lw_leng TYPE i.
  DATA: lw_bp TYPE but000-partner.
  DATA: lw_segment TYPE numc10.
  DATA: lw_de_cre_ind1 TYPE char1.
  IF sy-tcode = 'ZFIE003'. " post
*    CALL FUNCTION 'UPLOAD_XLS_FILE_2_ITAB'
*      EXPORTING
*        i_filename = p_file
*      TABLES
*        e_itab     = gt_upload2
*      EXCEPTIONS
*        file_error = 1
*        OTHERS     = 2.
*    IF sy-subrc = 0.
*      IF gt_upload2 IS NOT INITIAL.
*        DESCRIBE TABLE gt_upload2 LINES lw_line.
*        IF lw_line > 1.
*          DELETE gt_upload2 INDEX 1.
*          MOVE-CORRESPONDING gt_upload2 TO gt_upload.
*        ENDIF.
*      ENDIF.
*    ENDIF.
    CALL FUNCTION 'UPLOAD_XLS_FILE_2_ITAB'
      EXPORTING
        i_filename = p_file
      TABLES
        e_itab     = gt_upload
      EXCEPTIONS
        file_error = 1
        OTHERS     = 2.
    IF sy-subrc = 0.
      IF gt_upload IS NOT INITIAL.
        DESCRIBE TABLE gt_upload LINES lw_line.
        IF lw_line > 1.
          DELETE gt_upload INDEX 1.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE. " park
*    CALL FUNCTION 'UPLOAD_XLS_FILE_2_ITAB'
*      EXPORTING
*        i_filename = p_file
*      TABLES
*        e_itab     = gt_upload
*      EXCEPTIONS
*        file_error = 1
*        OTHERS     = 2.
*    IF sy-subrc = 0.
*      IF gt_upload IS NOT INITIAL.
*        DESCRIBE TABLE gt_upload LINES lw_line.
*        IF lw_line > 1.
*          DELETE gt_upload INDEX 1.
*        ENDIF.
*      ENDIF.
*    ENDIF.
    CALL FUNCTION 'UPLOAD_XLS_FILE_2_ITAB'
      EXPORTING
        i_filename = p_file
      TABLES
        e_itab     = gt_upload2
      EXCEPTIONS
        file_error = 1
        OTHERS     = 2.
    IF sy-subrc = 0.
      IF gt_upload2 IS NOT INITIAL.
        DESCRIBE TABLE gt_upload2 LINES lw_line.
        IF lw_line > 1.
          DELETE gt_upload2 INDEX 1.
          MOVE-CORRESPONDING gt_upload2 TO gt_upload.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF gt_upload IS INITIAL.
    MESSAGE TEXT-e01 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.
    LOOP AT gt_upload ASSIGNING FIELD-SYMBOL(<lf_upload>).
      lw_bp = <lf_upload>-bp.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lw_bp
        IMPORTING
          output = lw_bp.
      <lf_upload>-bp = lw_bp.
      IF <lf_upload>-segment CO ( '0123456789' ) AND <lf_upload>-segment IS NOT INITIAL.
        lw_segment = <lf_upload>-segment.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lw_segment
          IMPORTING
            output = lw_segment.
        <lf_upload>-segment = lw_segment.
      ENDIF.
*      lw_segment = <lf_upload>-segment.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = <lf_upload>-segment
*        IMPORTING
*          output = <lf_upload>-segment.

*      REPLACE ALL OCCURRENCES OF '.' IN <lf_upload>-amt_doccur WITH ''.
*      REPLACE ALL OCCURRENCES OF ',' IN <lf_upload>-amt_doccur WITH '.'.
      CONDENSE <lf_upload>-amt_doccur.

*      REPLACE ALL OCCURRENCES OF '.' IN <lf_upload>-tax_amt WITH ''.
*      REPLACE ALL OCCURRENCES OF ',' IN <lf_upload>-tax_amt WITH '.'.
      CONDENSE <lf_upload>-tax_amt.

      lw_leng = strlen( <lf_upload>-doc_date ).
      IF lw_leng = 10.
        <lf_upload>-doc_date   = |{ <lf_upload>-doc_date+6(4) }{ <lf_upload>-doc_date+3(2) }{ <lf_upload>-doc_date+0(2) }|.
      ELSE.
        <lf_upload>-doc_date = ''.
      ENDIF.
      CLEAR lw_leng.
      lw_leng = strlen( <lf_upload>-pstng_date ).
      IF lw_leng = 10.
        <lf_upload>-pstng_date = |{ <lf_upload>-pstng_date+6(4) }{ <lf_upload>-pstng_date+3(2) }{ <lf_upload>-pstng_date+0(2) }|.
      ELSE.
        <lf_upload>-pstng_date = ''.
      ENDIF.
      CLEAR lw_leng.
      lw_leng = strlen( <lf_upload>-trans_date ).
      IF lw_leng = 10.
        <lf_upload>-trans_date = |{ <lf_upload>-trans_date+6(4) }{ <lf_upload>-trans_date+3(2) }{ <lf_upload>-trans_date+0(2) }|.
      ELSE.
        <lf_upload>-trans_date = ''.
      ENDIF.
      CLEAR lw_leng.
      lw_leng = strlen( <lf_upload>-bline_date ).
      IF lw_leng = 10.
        <lf_upload>-bline_date = |{ <lf_upload>-bline_date+6(4) }{ <lf_upload>-bline_date+3(2) }{ <lf_upload>-bline_date+0(2) }|.
*      ELSE.
*        <lf_upload>-bline_date = ''.
      ENDIF.

      IF <lf_upload>-doc_type = 'DK'.
        <lf_upload>-profit_ctr = <lf_upload>-ref_key_1.
      ENDIF.

      lw_leng = strlen( <lf_upload>-life_start_date ).
      IF lw_leng = 10.
        <lf_upload>-life_start_date = |{ <lf_upload>-life_start_date+6(4) }{ <lf_upload>-life_start_date+3(2) }{ <lf_upload>-life_start_date+0(2) }|.
      ELSE.
        <lf_upload>-life_start_date = ''.
      ENDIF.
      CLEAR lw_leng.

      lw_leng = strlen( <lf_upload>-life_end_date ).
      IF lw_leng = 10.
        <lf_upload>-life_end_date = |{ <lf_upload>-life_end_date+6(4) }{ <lf_upload>-life_end_date+3(2) }{ <lf_upload>-life_end_date+0(2) }|.
      ELSE.
        <lf_upload>-life_end_date = ''.
      ENDIF.
      CLEAR lw_leng.

      CONDENSE <lf_upload>-de_cre_ind1.
      lw_de_cre_ind1 = <lf_upload>-de_cre_ind1.
      TRANSLATE lw_de_cre_ind1 TO UPPER CASE.

      IF lw_de_cre_ind1 = TEXT-004.
        <lf_upload>-de_cre_ind1 = 'S'.
      ELSEIF lw_de_cre_ind1 = TEXT-005.
        <lf_upload>-de_cre_ind1 = 'H'.
      ENDIF.

      " Lam sach du lieu itemtext
      CONDENSE <lf_upload>-item_text.
*      PERFORM check_null_refkey3 USING sy-tabix <lf_upload>.
    ENDLOOP.

    MOVE-CORRESPONDING gt_upload TO gt_data.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data .
  TYPES: BEGIN OF lty_aca_hdr,
           stt     TYPE i,
           line_no TYPE i.
      INCLUDE TYPE ztb_acac_hdr.
  TYPES: END OF lty_aca_hdr.
  TYPES: BEGIN OF lty_aca_item,
           stt     TYPE i,
           line_no TYPE i.
      INCLUDE TYPE ztb_acac_item.
  TYPES: END OF lty_aca_item.
  TYPES: BEGIN OF lty_aca_assgmt,
           stt     TYPE i,
           line_no TYPE i.
      INCLUDE TYPE ztb_acc_assgmt.
  TYPES: END OF lty_aca_assgmt.
  DATA: ls_header   TYPE bapiache09.
  DATA: lt_glacc    TYPE TABLE OF bapiacgl09.
  DATA: ls_glacc    TYPE bapiacgl09.
  DATA: lt_customer TYPE TABLE OF bapiacar09.
  DATA: ls_customer TYPE bapiacar09.
  DATA: lt_vendor   TYPE TABLE OF bapiacap09.
  DATA: ls_vendor   TYPE bapiacap09.
  DATA: lt_return   TYPE TABLE OF bapiret2.
  DATA: lt_cr       TYPE TABLE OF bapiaccr09.
  DATA: ls_cr       TYPE bapiaccr09.
  DATA: lt_criteria TYPE TABLE OF bapiackec9.
  DATA: ls_criteria TYPE bapiackec9.
  DATA: lt_tax TYPE TABLE OF bapiactx09,
        ls_tax TYPE bapiactx09.
  DATA: ls_return TYPE bapiret2,
        obj_type  LIKE bapiache09-obj_type,
        obj_key   LIKE bapiache09-obj_key,
        obj_sys   LIKE bapiache09-obj_sys,
        docnum    LIKE bkpf-belnr.
  DATA: ls_data     TYPE ty_data.
  DATA: ls_message  TYPE ty_mess.
  DATA: lw_index    TYPE i.
  DATA: lw_lines    TYPE i.
  DATA: lw_count    TYPE i.
  DATA: lw_waers    TYPE waers.
  DATA: lw_error    TYPE boolean.
  DATA: lw_acctype2 TYPE boolean.
  DATA: lw_amount   TYPE p DECIMALS 2.
  DATA: r_descr TYPE REF TO cl_abap_structdescr,
        ls_name TYPE abap_compdescr.
  DATA: lt_exten2 TYPE TABLE OF bapiparex,
        ls_exten2 TYPE bapiparex.

  DATA:
    lt_ext            TYPE TABLE OF bapiacextc,
    ls_ext            TYPE bapiacextc,
    ls_fi_acc_doc_ext TYPE zst_fi_acc_doc_ext,
    lv_itemnoacc      TYPE posnr_acc.

  DATA: lw_name TYPE thead-tdname.
  DATA: lt_tline TYPE TABLE OF tline.
  DATA: ls_tline TYPE tline.
  TYPES: BEGIN OF lty_item,
           stt       TYPE numc3,
           item_text TYPE string,
         END OF lty_item.
  DATA: ls_itemtext TYPE lty_item.
  DATA: lt_itemtext TYPE TABLE OF lty_item.

  DATA: lw_fname TYPE thead-tdname.
  DATA: lt_flines TYPE TABLE OF tline.
  DATA: ls_flines TYPE tline.
  DATA: lw_leng TYPE i.
  DATA: lw_type TYPE i.
  DATA: lt_acac_hdr       TYPE TABLE OF lty_aca_hdr,
        lt_acac_item      TYPE TABLE OF lty_aca_item,
        lt_acc_assgmt     TYPE TABLE OF lty_aca_assgmt,
        lt_aca_hdr_store  TYPE TABLE OF ztb_acac_hdr,
        lt_aca_item_store TYPE TABLE OF ztb_acac_item,
        lt_aca_assg_store TYPE TABLE OF ztb_acc_assgmt.

  TYPES: BEGIN OF lty_output,
           id         TYPE i,
           header     TYPE bapiache09,
           onetime    LIKE bapiacpa09,
           glacc      LIKE lt_glacc,
           customer   LIKE lt_customer,
           vendor     LIKE lt_vendor,
           criteria   LIKE lt_criteria,
           tax        LIKE lt_tax,
           cr         LIKE lt_cr,
           ext        LIKE lt_ext,
           exten2     LIKE lt_exten2,
           return     LIKE lt_return,
           mau_hd     TYPE char20,
           item       LIKE lt_itemtext,
           stt        TYPE i,
           acac_hdr   TYPE ztb_acac_hdr,
           acac_item  TYPE ztb_acac_item,
           acc_assgmt TYPE ztb_acc_assgmt,
         END OF lty_output.

  "add by HUNGVT
  TYPES: BEGIN OF lty_new_doc,
           belnr TYPE belnr_d,
           bukrs TYPE bukrs,
           gjahr TYPE gjahr,
         END OF lty_new_doc.
  "end by HUNGVT
  DATA: lt_out TYPE TABLE OF lty_output.
  DATA: ls_out TYPE lty_output.
  DATA: lv_doc_objkey TYPE bapiache09-obj_key,
        lv_line_no    TYPE i,
        lt_new_doc    TYPE TABLE OF lty_new_doc,
        ls_new_doc    TYPE lty_new_doc,
        ls_ctgs_h     TYPE ztb_ctgs_h,
        ls_ctgs_i     TYPE ztb_ctgs_i,
        lt_ctgs_i     TYPE TABLE OF ztb_ctgs_i,
        lv_number     TYPE numc10,
        lv_mact       TYPE ztb_ctgs_h-mact.

  DATA: ls_acac_hdr     TYPE ztb_acac_hdr,
        ls_aca_hdr_tmp  TYPE lty_aca_hdr,
        ls_acac_item    TYPE ztb_acac_item,
        ls_aca_item_tmp TYPE lty_aca_item,
        ls_acc_assgmt   TYPE ztb_acc_assgmt,
        ls_aca_assg_tmp TYPE lty_aca_assgmt.

  CONSTANTS :   lw_flg    VALUE 'X'.  " Flag
*  SORT GT_DATA BY STT.
  " Get field name
  READ TABLE gt_data INTO DATA(ls_fieldname) INDEX 1.
  IF sy-subrc = 0.
    r_descr ?= cl_abap_typedescr=>describe_by_data( ls_fieldname ).
  ENDIF.

  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<lf_data>).
    " Get Vendor/Customer/GL
    READ TABLE gt_skb1 INTO DATA(ls_skb1)
    WITH KEY saknr = <lf_data>-gl_account.
    IF sy-subrc = 0.
      IF ls_skb1-mitkz = 'D'.
        <lf_data>-customer = <lf_data>-bp.
      ELSEIF ls_skb1-mitkz = 'K'.
        <lf_data>-vendor_no = <lf_data>-bp.
      ENDIF.

      IF ls_skb1-fstag = 'G004'.
        lw_type = 1.
      ELSE.
        lw_type = 2.
      ENDIF.
    ENDIF.

    lw_index = sy-tabix.
    FREE lt_cr.
    FREE lt_customer.
    FREE lt_glacc.
    FREE lt_vendor.
    FREE lt_return.
    AT NEW stt.
      CLEAR lv_line_no.
*    " Header
      ls_out-id = lw_index.

      ls_header-comp_code = <lf_data>-comp_code.
      ls_header-doc_date = <lf_data>-doc_date.
      ls_header-pstng_date = <lf_data>-pstng_date.
      ls_header-header_txt = <lf_data>-header_txt.
      ls_header-ref_doc_no = <lf_data>-ref_doc_no.
*      ls_header-neg_postng = <lf_data>-neg_postng. "comment by HUNGVT - prevent negative posting all item
      ls_header-username = sy-uname.
      ls_header-bus_act =  'RFBU'.
      ls_header-fisc_year = <lf_data>-pstng_date+0(4).
      IF <lf_data>-period IS INITIAL.
        ls_header-fis_period = <lf_data>-pstng_date+4(2).
      ELSE.
        ls_header-fis_period = <lf_data>-period.
      ENDIF.
*      IF p_test IS INITIAL.
      IF sy-tcode = 'ZFIE003'.
        ls_header-doc_status = '4'. "Status: Post doc
      ELSE.
        ls_header-doc_status = '3'. "Status: Save as complete
      ENDIF.
      " ref. structure for bapi parameter extensionin/extensionout
      IF <lf_data>-xref1_hd IS NOT INITIAL.
        ls_exten2-structure  = 'XREF1_HD'.
        ls_exten2-valuepart1 = <lf_data>-xref1_hd.  " Reference Key 1 Internal for Document Header
        APPEND ls_exten2 TO ls_out-exten2.
      ENDIF.
      IF <lf_data>-xref2_hd IS NOT INITIAL.
        ls_exten2-structure  = 'XREF2_HD'.
        ls_exten2-valuepart1 = <lf_data>-xref2_hd.  " Reference Key 2 Internal for Document Header
        APPEND ls_exten2 TO ls_out-exten2.
      ENDIF.
*      ENDIF.
      ls_header-doc_type = <lf_data>-doc_type.
      ls_out-header = ls_header.
      lw_waers = <lf_data>-currency.
      lw_count = 0.

      " Mau Hoa Don
      ls_out-mau_hd = <lf_data>-mau_hd.
    ENDAT.

    IF <lf_data>-ref_key_2 IS NOT INITIAL AND <lf_data>-doc_type IN gr_type_pgr_up AND <lf_data>-gl_account IN gr_acc_pgr_up.
      READ TABLE gt_partner_gr INTO DATA(ls_partner_gr)
        WITH KEY partner_value = <lf_data>-ref_key_2
        BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <lf_data>-ref_key_2 = ls_partner_gr-partner_grid.
      ENDIF.
    ENDIF.

    IF p_ptax IS INITIAL OR <lf_data>-tax_code IS INITIAL.
      lw_count = lw_count + 1.
      IF <lf_data>-gl_account IS NOT INITIAL AND <lf_data>-bp IS INITIAL.

*    " ITEM - GL Acc
        MOVE-CORRESPONDING <lf_data> TO ls_glacc.
        IF ls_glacc-itemno_acc IS INITIAL.
          ls_glacc-itemno_acc = lw_count.
        ENDIF.
        ls_glacc-acct_type = 'S'.
*      ls_glacc-de_cre_ind = ''.
*      LS_GLACC-PSTNG_DATE = LS_DATA-PSTNG_DATE.
*      LS_GLACC-PROFIT_CTR = LS_DATA-PROFIT_CTR.
*      APPEND LS_GLACC TO LT_GLACC.
        APPEND ls_glacc TO ls_out-glacc.
        CLEAR ls_glacc.

        " Item text
        ls_itemtext-stt = lw_count.
        ls_itemtext-item_text = <lf_data>-item_text.
        APPEND ls_itemtext TO ls_out-item.
        CLEAR ls_itemtext.

        " Line currency
        MOVE-CORRESPONDING <lf_data> TO ls_cr.
        ls_cr-currency = lw_waers.
        ls_cr-curr_type = '00'.  "Doc currency
        ls_cr-currency_iso = lw_waers.
        IF <lf_data>-de_cre_ind1 = 'H'.
          ls_cr-amt_doccur = ls_cr-amt_doccur * ( -1 ).
        ENDIF.


        IF sy-tcode = 'ZFIE001' AND <lf_data>-de_cre_ind1 <> 'H' AND <lf_data>-gl_account(3) = '242'.
          ls_acac_hdr-bukrs = <lf_data>-comp_code.
          ls_acac_hdr-acac_objtype = '0RENTING'.
          ls_acac_hdr-acac_objnumber = '$$1'.
          ls_acac_hdr-text = <lf_data>-item_text.
          ls_acac_hdr-keydate = sy-datum.
          ls_acac_hdr-ldgrp = '0L'.
          ls_acac_hdr-type = 'T'.
          IF <lf_data>-gl_account = '2421040000' OR <lf_data>-gl_account = '2422040000'.
            ls_acac_item-itemtype = 'COSTS'.
          ELSE.
            ls_acac_item-itemtype = 'OTHERS'.
          ENDIF.
          ls_acac_item-rldnr = '0L'.
          ls_acac_item-life_start_date = <lf_data>-life_start_date.
          ls_acac_item-life_end_date = <lf_data>-life_end_date.
          ls_acac_item-per_doc_type = 'MA'.
          ls_acac_item-rwcurttl = 'VND'.
          ls_acac_item-per_accr_accnt = <lf_data>-gl_account.
          ls_acac_item-per_offstng_accnt = <lf_data>-per_offstng_accnt.
          ls_acac_item-acrmethod = 'LINEAR_DAY'.
          ls_acac_item-total_accr_amnt_wsl = <lf_data>-amt_doccur / 100.

          ls_acc_assgmt-prctr = <lf_data>-profit_ctr.
          ls_acc_assgmt-kostl = <lf_data>-costcenter.

          ls_out-acac_hdr = ls_acac_hdr.
          ls_out-acac_item  = ls_acac_item.
          ls_out-acc_assgmt = ls_acc_assgmt.

          lv_line_no = lv_line_no + 1.
          MOVE-CORRESPONDING ls_acac_hdr TO ls_aca_hdr_tmp.
          ls_aca_hdr_tmp-stt = <lf_data>-stt.
          ls_aca_hdr_tmp-line_no = lv_line_no.
          APPEND ls_aca_hdr_tmp TO lt_acac_hdr.
          MOVE-CORRESPONDING ls_acac_item TO ls_aca_item_tmp.
          ls_aca_item_tmp-stt = <lf_data>-stt.
          ls_aca_item_tmp-line_no = lv_line_no.
          APPEND ls_aca_item_tmp TO lt_acac_item.
          MOVE-CORRESPONDING ls_acc_assgmt TO ls_aca_assg_tmp.
          ls_aca_assg_tmp-stt = <lf_data>-stt.
          ls_aca_assg_tmp-line_no = lv_line_no.
          APPEND ls_aca_assg_tmp TO lt_acc_assgmt.

          CLEAR: ls_acac_hdr,
                 ls_acac_item,
                 ls_acc_assgmt,
                 ls_aca_hdr_tmp,
                 ls_aca_item_tmp,
                 ls_aca_assg_tmp.
        ENDIF.

        ls_cr-tax_amt = 0.
        IF ls_cr-itemno_acc IS INITIAL.
          ls_cr-itemno_acc = lw_count.
        ENDIF.
*      APPEND LS_CR TO LT_CR.
        APPEND ls_cr TO ls_out-cr.
        "add by HUNGVT- check negative posting for tax line
        IF <lf_data>-neg_postng IS NOT INITIAL.
          CLEAR ls_exten2.
          ls_exten2-valuepart2 =  ls_cr-itemno_acc.
          ls_exten2-structure = 'XNEGP'.
          ls_exten2-valuepart1 = 'X'.

          APPEND ls_exten2 TO ls_out-exten2.
        ENDIF.
        "end by HUNGVT - check negative posting for tax line
        CLEAR ls_cr.

      ELSEIF <lf_data>-customer IS NOT INITIAL.

        " ITEM - Customer
        MOVE-CORRESPONDING <lf_data> TO ls_customer.
        IF ls_customer-itemno_acc IS INITIAL.
          ls_customer-itemno_acc = lw_count.
        ENDIF.
        ls_customer-customer = |{ ls_customer-customer ALPHA = IN }|.
        ls_customer-businessplace = ' '.
        lw_acctype2 = 'D'.
*      APPEND LS_CUSTOMER TO LT_CUSTOMER.
        APPEND ls_customer TO ls_out-customer.
        CLEAR ls_customer.

        " Item text
        ls_itemtext-stt = lw_count.
        ls_itemtext-item_text = <lf_data>-item_text.
        APPEND ls_itemtext TO ls_out-item.
        CLEAR ls_itemtext.

        " Line currency
        MOVE-CORRESPONDING <lf_data> TO ls_cr.
        ls_cr-currency = lw_waers.
        ls_cr-curr_type = '00'.  "Doc currency
        ls_cr-currency_iso = lw_waers.
        IF ls_cr-itemno_acc IS INITIAL.
          ls_cr-itemno_acc = lw_count.
        ENDIF.
        IF <lf_data>-de_cre_ind1 = 'H'.
          ls_cr-amt_doccur = ls_cr-amt_doccur * ( -1 ).
        ENDIF.
        ls_cr-tax_amt = 0.
*      APPEND LS_CR TO LT_CR.
        APPEND ls_cr TO ls_out-cr.
        "add by HUNGVT- check negative posting for tax line
        IF <lf_data>-neg_postng IS NOT INITIAL.
          CLEAR ls_exten2.
          ls_exten2-valuepart2 =  ls_cr-itemno_acc.
          ls_exten2-structure = 'XNEGP'.
          ls_exten2-valuepart1 = 'X'.

          APPEND ls_exten2 TO ls_out-exten2.
        ENDIF.
        "end by HUNGVT - check negative posting for tax line
        CLEAR ls_cr.

      ELSEIF <lf_data>-vendor_no IS NOT INITIAL.

        " ITEM - VENDOR
        MOVE-CORRESPONDING <lf_data> TO ls_vendor.
        lw_acctype2 = 'K'.
        IF ls_vendor-itemno_acc IS INITIAL.
          ls_vendor-itemno_acc = lw_count.
        ENDIF.
        ls_vendor-vendor_no = |{ ls_vendor-vendor_no ALPHA = IN }|.
        APPEND ls_vendor TO ls_out-vendor.
        CLEAR ls_vendor.

        " Item text
        ls_itemtext-stt = lw_count.
        ls_itemtext-item_text = <lf_data>-item_text.
        APPEND ls_itemtext TO ls_out-item.
        CLEAR ls_itemtext.

        " Line currency
        MOVE-CORRESPONDING <lf_data> TO ls_cr.
        ls_cr-currency = lw_waers.
        ls_cr-curr_type = '00'.  "Doc currency
        ls_cr-currency_iso = lw_waers.
        IF ls_cr-itemno_acc IS INITIAL.
          ls_cr-itemno_acc = lw_count.
        ENDIF.
        IF <lf_data>-de_cre_ind1 = 'H'.
          ls_cr-amt_doccur = ls_cr-amt_doccur * ( -1 ).
        ENDIF.
        ls_cr-tax_amt = 0.
*      APPEND LS_CR TO LT_CR.
        APPEND ls_cr TO ls_out-cr.
        "add by HUNGVT- check negative posting for tax line
        IF <lf_data>-neg_postng IS NOT INITIAL.
          CLEAR ls_exten2.
          ls_exten2-valuepart2 =  ls_cr-itemno_acc.
          ls_exten2-structure = 'XNEGP'.
          ls_exten2-valuepart1 = 'X'.

          APPEND ls_exten2 TO ls_out-exten2.
        ENDIF.
        "end by HUNGVT - check negative posting for tax line
        CLEAR ls_cr.
      ENDIF.
    ENDIF.
    " ITEM - tax
    IF <lf_data>-tax_code IS NOT INITIAL.
      lw_count = lw_count + 1.
      READ TABLE ls_out-tax INTO DATA(ls_tax_exist)
      WITH KEY tax_code = <lf_data>-tax_code.
      IF sy-subrc <> 0.
        IF ls_tax-itemno_acc IS INITIAL.
          ls_tax-itemno_acc = lw_count.
          lv_itemnoacc = ls_tax-itemno_acc. "add by HUNGVT
          ls_tax-tax_code = <lf_data>-tax_code.
        ENDIF.
        APPEND ls_tax TO ls_out-tax.
        CLEAR ls_tax.
        CLEAR ls_tax_exist.

        IF <lf_data>-item_text IS NOT INITIAL.
          " Item text
          ls_itemtext-stt = lw_count.
          ls_itemtext-item_text = <lf_data>-item_text.
          APPEND ls_itemtext TO ls_out-item.
          CLEAR ls_itemtext.

          CLEAR ls_fi_acc_doc_ext.
          ls_fi_acc_doc_ext-itemno_acc = lw_count.
          ls_fi_acc_doc_ext-zlongtext = <lf_data>-item_text.
          ls_ext = ls_fi_acc_doc_ext.
          APPEND ls_ext TO ls_out-ext.
        ENDIF.

        " Line currency
        MOVE-CORRESPONDING <lf_data> TO ls_cr.
        ls_cr-currency = lw_waers.
        ls_cr-curr_type = '00'.
        ls_cr-currency_iso = lw_waers.
        IF ls_cr-itemno_acc IS INITIAL.
          ls_cr-itemno_acc = lw_count.
        ENDIF.
        IF p_ptax IS INITIAL.
          IF <lf_data>-de_cre_ind1 = 'H'.
            ls_cr-tax_amt = ls_cr-tax_amt * ( -1 ).
            ls_cr-amt_base = ls_cr-amt_doccur * ( -1 ).
            ls_cr-amt_doccur = ls_cr-tax_amt.
          ELSE.
            ls_cr-amt_base = ls_cr-amt_doccur.
            ls_cr-amt_doccur = ls_cr-tax_amt.
          ENDIF.
        ELSE.
          IF <lf_data>-de_cre_ind1 = 'H'.
            ls_cr-tax_amt = ls_cr-tax_amt * ( -1 ).
            ls_cr-amt_doccur = ls_cr-tax_amt.
            ls_cr-amt_base = ls_cr-amt_doccur * ( -1 ).
          ELSE.
            ls_cr-amt_doccur = ls_cr-tax_amt.
            ls_cr-amt_base = ls_cr-amt_doccur.
          ENDIF.
        ENDIF.
*        ls_cr-amt_base = ls_cr-amt_doccur.
*        ls_cr-amt_doccur = ls_cr-tax_amt.
*        ls_cr-tax_amt = 0.
        APPEND ls_cr TO ls_out-cr.
        "add by HUNGVT- check negative posting for tax line
        IF <lf_data>-neg_postng IS NOT INITIAL.
          CLEAR ls_exten2.
          ls_exten2-valuepart2 =  ls_cr-itemno_acc.
          ls_exten2-structure = 'XNEGP'.
          ls_exten2-valuepart1 = 'X'.
          APPEND ls_exten2 TO ls_out-exten2.
        ENDIF.
        "end by HUNGVT - check negative posting for tax line
        CLEAR ls_cr.
      ELSE.
        READ TABLE ls_out-cr ASSIGNING FIELD-SYMBOL(<lf_cr>)
        WITH KEY itemno_acc = ls_tax_exist-itemno_acc.
        IF sy-subrc = 0.
          lv_itemnoacc = ls_tax_exist-itemno_acc.
          IF p_ptax IS INITIAL.
            IF <lf_data>-de_cre_ind1 = 'H'.
              <lf_cr>-tax_amt = <lf_cr>-tax_amt + <lf_data>-tax_amt * ( -1 ).
              <lf_cr>-amt_base = <lf_cr>-amt_base + <lf_data>-amt_doccur * ( -1 ).
              <lf_cr>-amt_doccur = <lf_cr>-tax_amt.
            ELSE.
              <lf_cr>-tax_amt = <lf_cr>-tax_amt + <lf_data>-tax_amt .
              <lf_cr>-amt_base = <lf_cr>-amt_base + <lf_data>-amt_doccur.
              <lf_cr>-amt_doccur = <lf_cr>-tax_amt.
            ENDIF.
          ELSE.
            IF <lf_data>-de_cre_ind1 = 'H'.
              <lf_cr>-tax_amt = <lf_cr>-tax_amt + <lf_data>-tax_amt * ( -1 ).
              <lf_cr>-amt_doccur = <lf_cr>-tax_amt.
              <lf_cr>-amt_base = <lf_cr>-amt_base + <lf_data>-amt_doccur * ( -1 ).
            ELSE.
              <lf_cr>-tax_amt = <lf_cr>-tax_amt + <lf_data>-tax_amt .
              <lf_cr>-amt_doccur = <lf_cr>-tax_amt.
              <lf_cr>-amt_base = <lf_cr>-amt_base + <lf_data>-amt_doccur.
            ENDIF.
          ENDIF.
*          <lf_cr>-tax_amt = <lf_cr>-tax_amt + <lf_data>-tax_amt.
        ENDIF.
      ENDIF.


    ENDIF.
    " Item one time
    IF <lf_data>-name_1 IS NOT INITIAL.
      ls_out-onetime-name = <lf_data>-name_1+0(35).
      ls_out-onetime-name_2 = <lf_data>-name_1+35(35).
      ls_out-onetime-name_3 = <lf_data>-name_1+70(35).
      ls_out-onetime-name_4 = <lf_data>-name_1+105(35).
      ls_out-onetime-langu_iso = 'VI'.
      ls_out-onetime-city = <lf_data>-city.
      ls_out-onetime-street = <lf_data>-street.
      ls_out-onetime-country = 'VN'.
      ls_out-onetime-bank_ctry = 'VN'.
      ls_out-onetime-tax_no_1 = <lf_data>-tax_no_1.
*      move ls_onetime TO ls_out-onetime.
*      CLEAR ls_onetime.
    ENDIF.

    " CO-PA
    IF <lf_data>-kokrs IS NOT INITIAL.
      LOOP AT r_descr->components INTO DATA(ls_field) FROM 39.
        ls_criteria-itemno_acc = lw_count.
        ls_criteria-fieldname = ls_field-name.
        " Value
        IF ls_criteria-fieldname = 'KOKRS'.
          ls_criteria-character = <lf_data>-kokrs.
        ELSEIF ls_criteria-fieldname = 'KNDNR'.
          ls_criteria-character = <lf_data>-kndnr.
        ELSEIF ls_criteria-fieldname = 'ARTNR'.
          ls_criteria-character = <lf_data>-artnr.
        ELSEIF ls_criteria-fieldname = 'FKART'.
          ls_criteria-character = <lf_data>-fkart.
        ELSEIF ls_criteria-fieldname = 'KDPOS'.
          ls_criteria-character = <lf_data>-kdpos.
        ELSEIF ls_criteria-fieldname = 'KAUFN'.
          ls_criteria-character = <lf_data>-kaufn.
        ELSEIF ls_criteria-fieldname = 'WERKS'.
          ls_criteria-character = <lf_data>-werks.
        ELSEIF ls_criteria-fieldname = 'FKBER'.
          ls_criteria-character = <lf_data>-fkber.
        ELSEIF ls_criteria-fieldname = 'VKORG'.
          ls_criteria-character = <lf_data>-vkorg.
        ELSEIF ls_criteria-fieldname = 'VTWEG'.
          ls_criteria-character = <lf_data>-vtweg.
        ELSEIF ls_criteria-fieldname = 'SPART'.
          ls_criteria-character = <lf_data>-spart.
        ELSEIF ls_criteria-fieldname = 'PSPNR'.
          ls_criteria-character = <lf_data>-pspnr.
        ELSEIF ls_criteria-fieldname = 'KSTRG'.
          ls_criteria-character = <lf_data>-kstrg.
        ELSEIF ls_criteria-fieldname = 'PPRCTR'.
          ls_criteria-character = <lf_data>-pprctr.
        ELSEIF ls_criteria-fieldname = 'KMVKRG'.
          ls_criteria-character = <lf_data>-kmvkrg.
        ELSEIF ls_criteria-fieldname = 'KMVTNR'.
          ls_criteria-character = <lf_data>-kmvtnr.
        ELSEIF ls_criteria-fieldname = 'KUNRE'.
          ls_criteria-character = <lf_data>-kunre.
        ELSEIF ls_criteria-fieldname = 'KUNWE'.
          ls_criteria-character = <lf_data>-kunwe.
        ELSEIF ls_criteria-fieldname = 'WWPGR'.
          ls_criteria-character = <lf_data>-wwpgr.
        ELSEIF ls_criteria-fieldname = 'KMVKBU'.
          ls_criteria-character = <lf_data>-kmvkbu.
        ELSEIF ls_criteria-fieldname = 'WWSER'.
          ls_criteria-character = <lf_data>-wwser.
        ELSEIF ls_criteria-fieldname = 'WWLV'.
          ls_criteria-character = <lf_data>-wwlv.
        ENDIF.
        IF NOT ( ls_criteria-fieldname = 'KMVKRG' AND ls_criteria-character IS INITIAL ).
          APPEND ls_criteria TO ls_out-criteria.
        ENDIF.
        CLEAR ls_criteria.
      ENDLOOP.
    ENDIF.

    IF <lf_data>-zrefkey4 IS NOT INITIAL.
      CLEAR ls_fi_acc_doc_ext.
      ls_fi_acc_doc_ext-itemno_acc = lw_count.
      ls_fi_acc_doc_ext-zrefkey4 = <lf_data>-zrefkey4.
      ls_ext = ls_fi_acc_doc_ext.
      APPEND ls_ext TO ls_out-ext.
    ENDIF.

    IF <lf_data>-item_text IS NOT INITIAL.
      CLEAR ls_fi_acc_doc_ext.
      ls_fi_acc_doc_ext-itemno_acc = lw_count.
      ls_fi_acc_doc_ext-zlongtext = <lf_data>-item_text.
      ls_ext = ls_fi_acc_doc_ext.
      APPEND ls_ext TO ls_out-ext.
    ENDIF.

    AT END OF stt.
      ls_out-stt = <lf_data>-stt.
      APPEND ls_out TO lt_out.
      CLEAR ls_out.
    ENDAT.
    CLEAR ls_skb1.
    CLEAR lw_type.
  ENDLOOP.

  LOOP AT lt_out INTO ls_out.

    REFRESH: lt_aca_item_store,
             lt_aca_assg_store.

    lw_index = sy-tabix.
    "add by HUNGVT
    IF ls_out-header-doc_type IS NOT INITIAL.
      "end by HUNGVT
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          documentheader    = ls_out-header
          customercpd       = ls_out-onetime
        TABLES
          accountgl         = ls_out-glacc
          accountreceivable = ls_out-customer
          accountpayable    = ls_out-vendor
          accounttax        = ls_out-tax
          currencyamount    = ls_out-cr
          criteria          = ls_out-criteria
          extension1        = ls_out-ext
          extension2        = ls_out-exten2
          return            = ls_out-return.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = lw_flg.
      ENDIF.
    ENDIF.
    " Message test
*    ls_message-message = |Line { lw_index }: |.
    ls_message-message = |Line { ls_out-stt }: |.
    " In ra messgae data upload
    MOVE-CORRESPONDING ls_data TO ls_message.
    ls_message-line = lw_index.
    READ TABLE gt_tbsl INTO DATA(ls_tbsl)
    WITH KEY shkzg = ls_data-de_cre_ind1
             koart = 'S'.
    IF sy-subrc = 0.
      ls_message-de_cre_ind1 = ls_tbsl-bschl.
    ENDIF.
*    READ TABLE GT_TBSL INTO DATA(LS_TBSL2)
*    WITH KEY SHKZG = LS_DATA-DE_CRE_IND1
*             KOART = LW_ACCTYPE2.
*    IF SY-SUBRC = 0.
*      LS_MESSAGE-DE_CRE_IND1 = LS_TBSL2-BSCHL.
*    ENDIF.
    IF ls_message-currency = 'VND'.
      lw_amount = ls_data-amt_doccur / 100.
    ELSE.
      lw_amount = ls_data-amt_doccur.
    ENDIF.

    WRITE lw_amount CURRENCY ls_message-currency TO ls_message-amt_doccur.
    IF ls_out-return IS NOT INITIAL.
      READ TABLE ls_out-return INTO ls_return INDEX 1.
      IF sy-subrc = 0.
        IF ls_return-type = 'E'.
          DELETE lt_return INDEX 1.
          lw_error = abap_true.
          ls_message-status = 1.
          ls_message-status2 = 1.
        ELSE.
          ls_message-status = 3.
          ls_message-status2 = 3.
        ENDIF.
      ENDIF.
      LOOP AT ls_out-return INTO ls_return.
        IF sy-tabix = 1.
          ls_message-message = |{ ls_message-message } { ls_return-message }|.
        ELSE.
          ls_message-message = |{ ls_message-message }, { ls_return-message }|.
        ENDIF.
      ENDLOOP.
      APPEND ls_message TO gt_message.
      CLEAR ls_message.
    ENDIF.
    IF ls_out-header-doc_type IS INITIAL.
      ls_message-status = 1.
      ls_message-status2 = 1.
      ls_message-message = |{ ls_message-message } document type is blank.|.
      APPEND ls_message TO gt_message.
      CLEAR ls_message.
      lw_error = abap_true.
    ENDIF.
*    LOOP AT ls_out-vendor INTO ls_vendor.
*      IF ls_vendor-gl_account+0(4) = '1411' AND ls_vendor-gl_account+0(5) <> '14116'.
*        IF ls_vendor-ref_key_3 IS INITIAL.
*          ls_message-message = |Line { ls_out-stt }: |.
*          " In ra messgae data upload
*          MOVE-CORRESPONDING ls_data TO ls_message.
*          ls_message-line = lw_index.
*          READ TABLE gt_tbsl INTO ls_tbsl
*          WITH KEY shkzg = ls_data-de_cre_ind1
*                   koart = 'S'.
*          IF sy-subrc = 0.
*            ls_message-de_cre_ind1 = ls_tbsl-bschl.
*          ENDIF.
*          IF ls_message-currency = 'VND'.
*            lw_amount = ls_data-amt_doccur / 100.
*          ELSE.
*            lw_amount = ls_data-amt_doccur.
*          ENDIF.
*          WRITE lw_amount CURRENCY ls_message-currency TO ls_message-amt_doccur.
*
*          ls_message-status = 2.
*          ls_message-status2 = 2.
*          ls_message-message = |{ ls_message-message } refkey 3 của tài khoản '1411*' đang trống .|.
*          APPEND ls_message TO gt_message.
*          CLEAR ls_message.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*    IF
  ENDLOOP.
  CLEAR lw_index.
  CLEAR lw_lines.
  CLEAR ls_return.

  " Post doc
  IF lw_error = abap_false AND p_test IS INITIAL.
    FREE gt_message.
    gw_check_post = abap_false.
    LOOP AT lt_out INTO ls_out.
      lw_index = sy-tabix.
*      READ TABLE GT_MESSAGE ASSIGNING FIELD-SYMBOL(<LF_MESSAGE>)
*      WITH KEY LINE = LW_INDEX.
*      IF SY-SUBRC = 0.
*        <LF_MESSAGE>-MESSAGE = ''.
*      ENDIF.
      "add by HUNGVT
      REFRESH: lt_return.
      IF ls_out-header-doc_type IS NOT INITIAL.
        "end by HUNGVT
        CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
          EXPORTING
            documentheader    = ls_out-header
            customercpd       = ls_out-onetime
          IMPORTING
            obj_key           = lv_doc_objkey
          TABLES
            accountgl         = ls_out-glacc
            accountreceivable = ls_out-customer
            accountpayable    = ls_out-vendor
            accounttax        = ls_out-tax
            currencyamount    = ls_out-cr
            criteria          = ls_out-criteria
            extension1        = ls_out-ext
            extension2        = ls_out-exten2
            return            = lt_return.
        IF sy-subrc = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = lw_flg.
          "add by HUNGVT - tao ctgs
          IF p_ctgs = 'X'.
            CLEAR: ls_new_doc.
            ls_new_doc-belnr = lv_doc_objkey+0(10).
            ls_new_doc-bukrs = lv_doc_objkey+10(4).
            ls_new_doc-gjahr = lv_doc_objkey+14(4).
            APPEND ls_new_doc TO lt_new_doc.
          ENDIF.
          "end by HUNGVT - tao ctgs
*          IF sy-tcode = 'ZFIE001'.
*            PERFORM update_park_doc USING lv_doc_objkey
*                                          ls_out-stt.
*          ENDIF.
          LOOP AT lt_acac_hdr INTO ls_aca_hdr_tmp WHERE stt = ls_out-stt.
*          IF ls_out-acac_hdr IS NOT INITIAL.
            MOVE-CORRESPONDING ls_aca_hdr_tmp TO ls_acac_hdr.
            CALL FUNCTION 'GUID_CREATE'
              IMPORTING
                ev_guid_22 = ls_acac_hdr-guid.
            ls_acac_hdr-belnr = lv_doc_objkey+0(10).
            ls_acac_hdr-bukrs = lv_doc_objkey+10(4).
            ls_acac_hdr-gjahr = lv_doc_objkey+14(4).
            APPEND ls_acac_hdr TO lt_aca_hdr_store.
*           Item
            LOOP AT lt_acac_item INTO ls_aca_item_tmp WHERE stt = ls_out-stt AND line_no = ls_aca_hdr_tmp-line_no.
              CLEAR: ls_acac_item.
              MOVE-CORRESPONDING ls_aca_item_tmp TO ls_acac_item.
              ls_acac_item-guid_hdr = ls_acac_hdr-guid.
              CALL FUNCTION 'GUID_CREATE'
                IMPORTING
                  ev_guid_22 = ls_acac_item-guid. "ls_out-acac_item-guid.
              APPEND ls_acac_item TO lt_aca_item_store.
            ENDLOOP.
*           Assignment
            LOOP AT lt_acc_assgmt INTO ls_aca_assg_tmp WHERE stt = ls_out-stt AND line_no = ls_aca_hdr_tmp-line_no.
              CLEAR ls_acc_assgmt.
              MOVE-CORRESPONDING ls_aca_assg_tmp TO ls_acc_assgmt.
              ls_acc_assgmt-guid_hdr = ls_acac_hdr-guid.
              CALL FUNCTION 'GUID_CREATE'
                IMPORTING
                  ev_guid_22 = ls_acc_assgmt-guid.
              APPEND ls_acc_assgmt TO lt_aca_assg_store.
            ENDLOOP.
*          ENDIF.
            CLEAR: ls_acac_hdr.
          ENDLOOP.

          INSERT ztb_acac_hdr FROM TABLE lt_aca_hdr_store.
          INSERT ztb_acac_item FROM TABLE lt_aca_item_store.
          INSERT ztb_acc_assgmt FROM TABLE lt_aca_assg_store.
          COMMIT WORK.
*
          REFRESH: lt_aca_hdr_store[],
                   lt_aca_item_store[],
                   lt_aca_assg_store[].
*          IF ls_out-acac_hdr IS NOT INITIAL.
*            CALL FUNCTION 'GUID_CREATE'
*              IMPORTING
*                ev_guid_22 = ls_out-acac_hdr-guid.
*            ls_out-acac_hdr-belnr = lv_doc_objkey+0(10).
*            ls_out-acac_hdr-bukrs = lv_doc_objkey+10(4).
*            ls_out-acac_hdr-gjahr = lv_doc_objkey+14(4).
*
*            ls_out-acac_item-guid_hdr = ls_out-acac_hdr-guid.
*            CALL FUNCTION 'GUID_CREATE'
*              IMPORTING
*                ev_guid_22 = ls_out-acac_item-guid.
*
*            ls_out-acc_assgmt-guid_hdr = ls_out-acac_hdr-guid.
*            CALL FUNCTION 'GUID_CREATE'
*              IMPORTING
*                ev_guid_22 = ls_out-acc_assgmt-guid.
*
*            INSERT ztb_acac_hdr FROM ls_out-acac_hdr.
*            INSERT ztb_acac_item FROM ls_out-acac_item.
*            INSERT ztb_acc_assgmt FROM ls_out-acc_assgmt.
*            COMMIT WORK.
*          ENDIF.

        ENDIF.
      ENDIF.
      " Message post
      ls_message-message = |Document { lw_index }: |.
      IF lt_return IS NOT INITIAL.
        READ TABLE lt_return INTO ls_return INDEX 1.
        IF sy-subrc = 0.
          IF ls_return-type = 'E'.
            DELETE lt_return INDEX 1.
            gw_check_post = abap_true.
            ls_message-status = 1.
            ls_message-status2 = 1.
          ELSE.
            ls_message-status = 3.
            ls_message-status2 = 3.
          ENDIF.
        ENDIF.
        LOOP AT lt_return INTO ls_return.
          IF sy-tabix = 1.
            IF ls_message-status2 = 3.
              ls_message-belnr =  ls_return-message_v2+0(10).
              IF sy-tcode = 'ZFIE003'.
                ls_message-message = |{ ls_message-message } Document successfully posted|.
              ELSE.
                ls_message-message = |{ ls_message-message } Document successfully parked|.
              ENDIF.
              " Create text Mau_Hoa_Don
              lw_name = |{ ls_out-header-comp_code }{ ls_message-belnr }{ ls_out-header-fisc_year }|.
              ls_tline-tdformat = '*'.
              ls_tline-tdline = ls_out-mau_hd.
              APPEND ls_tline TO lt_tline.
              CLEAR ls_tline.
              CALL FUNCTION 'CREATE_TEXT'
                EXPORTING
                  fid       = '1040'
                  flanguage = 'E'
                  fname     = lw_name
                  fobject   = 'BELEG'
*                 SAVE_DIRECT       = 'X'
*                 FFORMAT   = '*'
                TABLES
                  flines    = lt_tline
                EXCEPTIONS
                  no_init   = 1
                  no_save   = 2
                  OTHERS    = 3.
              IF sy-subrc <> 0.
* Implement suitable error handling here
              ENDIF.
              CLEAR lw_name.
              FREE lt_tline.

              LOOP AT ls_out-item INTO DATA(ls_item1).
                " length longtext
                CLEAR lw_leng.
                lw_leng = strlen( ls_item1-item_text ).
                CLEAR ls_flines.
                WHILE lw_leng > 132.
                  ls_flines-tdformat = '*'.
                  ls_flines-tdline = ls_item1-item_text+0(132).
                  REPLACE ls_flines-tdline IN ls_item1-item_text WITH ''.
                  APPEND ls_flines TO lt_flines.
                  CLEAR ls_flines.
                  lw_leng = strlen( ls_item1-item_text ).
                ENDWHILE.
                IF lw_leng > 0.
                  ls_flines-tdformat = '*'.
                  ls_flines-tdline = ls_item1-item_text.
                  REPLACE ls_flines-tdline IN ls_item1-item_text WITH ''.
                  APPEND ls_flines TO lt_flines.
                  CLEAR ls_flines.
                ENDIF.
                lw_fname = |{ ls_out-header-comp_code }{ ls_message-belnr }{ ls_out-header-fisc_year }{ ls_item1-stt }|.
                CALL FUNCTION 'CREATE_TEXT'
                  EXPORTING
                    fid       = '0001'
                    flanguage = 'E'
                    fname     = lw_fname
                    fobject   = 'DOC_ITEM'
*                   SAVE_DIRECT       = 'X'
*                   FFORMAT   = '*'
                  TABLES
                    flines    = lt_flines
                  EXCEPTIONS
                    no_init   = 1
                    no_save   = 2
                    OTHERS    = 3.
                IF sy-subrc <> 0.
* Implement suitable error handling here
                ENDIF.
                CLEAR lw_fname.
                FREE lt_flines.
              ENDLOOP.

              EXIT.
            ELSE.
              ls_message-message = |{ ls_message-message }, { ls_return-message }|.
            ENDIF.
          ELSE.
            ls_message-message = |{ ls_message-message }, { ls_return-message }|.
          ENDIF.
        ENDLOOP.
      ENDIF.
      IF ls_out-header-doc_type IS INITIAL.
        ls_message-status = 1.
        ls_message-status2 = 1.
        ls_message-message = |{ ls_message-message } document type is blank.|.
      ENDIF.

      APPEND ls_message TO gt_message.
      CLEAR ls_message.
*      LOOP AT ls_out-vendor INTO ls_vendor.
*        IF ls_vendor-gl_account+0(4) = '1411' AND ls_vendor-gl_account+0(5) <> '14116'.
*          IF ls_vendor-ref_key_3 IS INITIAL.
*            ls_message-message = |Document { lw_index }: |.
*            ls_message-status = 2.
*            ls_message-status2 = 2.
*            ls_message-message = |{ ls_message-message } refkey 3 của tài khoản '1411*' đang trống.|.
*            APPEND ls_message TO gt_message.
*            CLEAR ls_message.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
      CLEAR lw_index.
    ENDLOOP.

    "add by HUNGVT - tao ctgs

    IF p_ctgs = 'X'.

      IF lt_new_doc IS NOT INITIAL.

        LOOP AT lt_new_doc ASSIGNING FIELD-SYMBOL(<fs_new_doc_bukrs>) GROUP BY ( bukrs = <fs_new_doc_bukrs>-bukrs ).
          LOOP AT GROUP <fs_new_doc_bukrs> ASSIGNING FIELD-SYMBOL(<fs_new_doc_gjahr>) GROUP BY ( gjahr = <fs_new_doc_gjahr>-gjahr ).
            IF <fs_new_doc_gjahr>-gjahr = '2021'. "add by HUNGVT - keep existing nr
              CALL FUNCTION 'NUMBER_GET_NEXT'
                EXPORTING
                  nr_range_nr             = '01'
                  object                  = 'ZCTGS2'
                IMPORTING
                  number                  = lv_number
                EXCEPTIONS
                  interval_not_found      = 1
                  number_range_not_intern = 2
                  object_not_found        = 3
                  quantity_is_0           = 4
                  quantity_is_not_1       = 5
                  interval_overflow       = 6
                  buffer_overflow         = 7
                  OTHERS                  = 8.

              IF sy-subrc NE 0.
* Implement suitable error handling here
                EXIT.
              ENDIF.
            ELSE. "new nr per year
              SELECT SINGLE * FROM nriv INTO @DATA(ls_nriv) WHERE object = 'ZCTGS1'
                                                              AND subobject = @<fs_new_doc_bukrs>-bukrs
                                                              AND toyear =  @<fs_new_doc_gjahr>-gjahr.
              IF sy-subrc = 0.
                CALL FUNCTION 'NUMBER_GET_NEXT'
                  EXPORTING
                    nr_range_nr             = ls_nriv-nrrangenr
                    object                  = ls_nriv-object
*                   QUANTITY                = '1'
                    subobject               = ls_nriv-subobject
                    toyear                  = ls_nriv-toyear
*                   IGNORE_BUFFER           = ' '
                  IMPORTING
                    number                  = lv_number
*                   QUANTITY                =
*                   RETURNCODE              =
                  EXCEPTIONS
                    interval_not_found      = 1
                    number_range_not_intern = 2
                    object_not_found        = 3
                    quantity_is_0           = 4
                    quantity_is_not_1       = 5
                    interval_overflow       = 6
                    buffer_overflow         = 7
                    OTHERS                  = 8.

                IF sy-subrc NE 0.
* Implement suitable error handling here
                  EXIT.
                ENDIF.
              ENDIF.
            ENDIF.
            CONCATENATE 'CTGS' lv_number INTO lv_mact.
            ls_ctgs_h-mact = lv_mact.
            ls_ctgs_h-bukrs = <fs_new_doc_bukrs>-bukrs.
            ls_ctgs_h-crdate = sy-datum.
            ls_ctgs_h-gjahr = <fs_new_doc_gjahr>-gjahr.
            ls_ctgs_h-usnam = sy-uname.
            LOOP AT GROUP <fs_new_doc_gjahr> ASSIGNING FIELD-SYMBOL(<fs_new_doc>).
              CLEAR ls_ctgs_i.
              ls_ctgs_i-mact = lv_mact.
              ls_ctgs_i-belnr = <fs_new_doc>-belnr.
              ls_ctgs_i-bukrs = <fs_new_doc_bukrs>-bukrs.
              ls_ctgs_i-gjahr = <fs_new_doc_gjahr>-gjahr.
              APPEND ls_ctgs_i TO lt_ctgs_i.

*              APPEND <fs_data>-belnr TO lt_belnr.
*              <fs_data>-mact = lv_mact.

              UPDATE bkpf SET ccnum = lv_mact
              WHERE belnr = <fs_new_doc>-belnr
                AND bukrs = <fs_new_doc_bukrs>-bukrs
                AND gjahr = <fs_new_doc_gjahr>-gjahr.
            ENDLOOP.

            IF lt_ctgs_i IS NOT INITIAL.
              SELECT bukrs, belnr, gjahr, budat FROM bkpf INTO TABLE @DATA(lt_bkpf1) FOR ALL ENTRIES IN @lt_ctgs_i WHERE belnr = @lt_ctgs_i-belnr
                                                                                AND bukrs = @lt_ctgs_i-bukrs
                                                                                AND gjahr = @lt_ctgs_i-gjahr.
              IF sy-subrc = 0.
                SORT lt_bkpf1 BY budat DESCENDING.
                READ TABLE lt_bkpf1 INTO DATA(ls_bkpf1) INDEX 1.
                CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
                  EXPORTING
                    iv_date           = ls_bkpf1-budat
                  IMPORTING
                    ev_month_end_date = ls_bkpf1-budat.
                ls_ctgs_h-budat = ls_bkpf1-budat.
              ENDIF.
            ENDIF.
            INSERT ztb_ctgs_h FROM ls_ctgs_h.
            INSERT ztb_ctgs_i FROM TABLE lt_ctgs_i.
            COMMIT WORK.
            REFRESH: lt_ctgs_i, lt_bkpf1.
            CLEAR: ls_ctgs_h.
            ls_message-status = 3.
            ls_message-status2 = 3.
            ls_message-message = |Chứng từ ghi sổ { lv_mact } được tạo|.
            APPEND ls_message TO gt_message.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDIF.
    "end by HUNGVT - tao ctgs
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data .
  DATA: ls_message TYPE ty_mess.
  DATA: lw_flg TYPE flag.
  DATA: lw_type TYPE dd01v-datatype.
  DATA: lw_index        TYPE i,
        lw_err          TYPE char1,
        lw_count_debit  TYPE i,
        lw_count_credit TYPE i.
  DATA: lw_leng TYPE i.
  DATA: lw_mess TYPE char255.
  DATA: lw_mess2 TYPE char255.
  DATA: lw_prctr     TYPE prctr.
  DATA: lw_day TYPE dats.

  SORT gt_data BY stt.
  LOOP AT gt_data INTO DATA(ls_data).
    AT NEW stt.
      CLEAR: lw_count_debit,
             lw_count_credit.
      ls_message-message = |Line { ls_data-stt }:|.
    ENDAT.

    lw_index = sy-tabix.

    IF ls_data-de_cre_ind1 = 'S'.
      lw_count_debit = lw_count_debit + 1.
    ELSEIF ls_data-de_cre_ind1 = 'H'.
      lw_count_credit = lw_count_credit + 1.
    ENDIF.

    IF lw_count_debit >= 2 AND lw_count_credit >= 2.
      ls_message-message = |{ ls_message-message }, có nhiều nợ, nhiều có |.
    ENDIF.

*    IF ls_data-gl_account CP '111*'.
*      ls_message-message = |{ ls_message-message }, có tài khoản 111 |.
*    ENDIF.

    CLEAR lw_prctr.

*    ls_message-message = |Line { lw_index }:|.
    " Check exist company code
    READ TABLE gt_t001 INTO DATA(ls_t001)
    WITH KEY bukrs = ls_data-comp_code.
    IF sy-subrc <> 0.
      ls_message-message = |{ ls_message-message }, Company code { ls_data-comp_code } doesn't exist |.
    ENDIF.
    " Check exist required
    IF   ls_data-stt          IS INITIAL
      OR ls_data-doc_date     IS INITIAL
      OR ls_data-pstng_date   IS INITIAL
      OR ls_data-doc_type     IS INITIAL
      OR ls_data-comp_code    IS INITIAL
      OR ls_data-currency     IS INITIAL
      OR ls_data-de_cre_ind1  IS INITIAL
      OR ls_data-amt_doccur   IS INITIAL
      OR ls_data-gl_account   IS INITIAL
      OR ls_data-profit_ctr   IS INITIAL.
      IF NOT ( ls_data-gl_account IS NOT INITIAL OR ls_data-customer IS NOT INITIAL OR ls_data-vendor_no IS NOT INITIAL ).
        ls_message-message = |{ ls_message-message }, Please fill all the required fields in line|.
      ENDIF.
    ENDIF.

    IF ls_data-currency = 'VND'.
      " Check amount
      DATA: lw_amt TYPE bapidoccur.
      lw_amt = trunc( ls_data-amt_doccur ).

      IF lw_amt <> ls_data-amt_doccur.
        ls_message-message = |{ ls_message-message }, Số tiền không được có dấu thập phân|.
      ENDIF.

      lw_amt = trunc( ls_data-tax_amt ).

      IF lw_amt <> ls_data-tax_amt.
        ls_message-message = |{ ls_message-message }, Số tiền thuế không được có dấu thập phân|.
      ENDIF.
    ENDIF.

*   validate date
    PERFORM check_date USING ls_data-doc_date
                       CHANGING lw_err.
    IF lw_err IS NOT INITIAL.
      ls_message-message = |{ ls_message-message }, Ngày chứng từ không hợp lệ|.
    ENDIF.
    PERFORM check_date USING ls_data-pstng_date
                       CHANGING lw_err.
    IF lw_err IS NOT INITIAL.
      ls_message-message = |{ ls_message-message }, Ngày hạch toán không hợp lệ|.
    ENDIF.
    PERFORM check_date USING ls_data-trans_date
                       CHANGING lw_err.
    IF lw_err IS NOT INITIAL.
      ls_message-message = |{ ls_message-message }, Ngày lấy tỉ giá trong hệ thống không hợp lệ|.
    ENDIF.
*    PERFORM check_date USING ls_data-bline_date
*                       CHANGING lw_err.
    IF lw_err IS NOT INITIAL.
      ls_message-message = |{ ls_message-message }, Ngày tính nợ không hợp lệ|.
    ENDIF.
    PERFORM check_date USING ls_data-life_start_date
                       CHANGING lw_err.
    IF lw_err IS NOT INITIAL.
      ls_message-message = |{ ls_message-message }, Ngày bắt đầu phân bổ không hợp lệ|.
    ENDIF.
    PERFORM check_date USING ls_data-life_end_date
                       CHANGING lw_err.
    IF lw_err IS NOT INITIAL.
      ls_message-message = |{ ls_message-message }, Ngày kết thúc phân bổ không hợp lệ|.
    ENDIF.
*
*    " Check 1/3 GL_Acc/Vendor/Cus
*    IF ( ls_data-gl_account IS NOT INITIAL AND ls_data-customer IS INITIAL AND ls_data-vendor_no IS INITIAL )
*      OR ( ls_data-gl_account IS INITIAL AND ls_data-customer IS NOT INITIAL AND ls_data-vendor_no IS INITIAL )
*      OR ( ls_data-gl_account IS INITIAL AND ls_data-customer IS INITIAL AND ls_data-vendor_no IS NOT INITIAL ).
*      ls_message-message = |{ ls_message-message }, Only one column is filled (G/L account or Customer or Vendor)|.
*    ENDIF.
    " Check BP
    IF ls_data-bp IS NOT INITIAL.
      READ TABLE gt_but000 INTO DATA(ls_but000)
      WITH KEY partner = ls_data-bp.
      IF sy-subrc <> 0.
        ls_message-message = |{ ls_message-message }, BP { ls_data-bp } doesn't exist |.
      ENDIF.
    ENDIF.

    " Check tax code
    IF ls_data-tax_code IS NOT INITIAL.
      READ TABLE gt_a003 INTO DATA(ls_a003)
      WITH KEY mwskz = ls_data-tax_code.
      IF sy-subrc <> 0.
        ls_message-message = |{ ls_message-message }, Tax code { ls_data-tax_code } doesn't exist |.
      ENDIF.
    ENDIF.

    IF sy-tcode = 'ZFIE001'. " park
      " Check acrrual data
      IF ls_data-de_cre_ind1 <> 'H' AND ls_data-gl_account IS NOT INITIAL AND ls_data-gl_account(3) = '242'.
        IF ls_data-per_offstng_accnt IS INITIAL
          OR ls_data-life_start_date IS INITIAL
          OR ls_data-life_end_date IS INITIAL OR ls_data-costcenter IS INITIAL.
          ls_message-message = |{ ls_message-message }, Thiếu thông tin phân bổ của Tài khoản 242 |.
        ENDIF.
        IF strlen( ls_data-item_text ) > 75.
          ls_message-message = |{ ls_message-message }, Vượt quá 75 ký tự thông tin diễn giải công thức phân bổ |.
        ENDIF.
      ENDIF.
    ENDIF.

*    IF ls_data-gl_account IS NOT INITIAL AND ls_data-gl_account(3) = '242' AND ls_data-acrobj_id IS INITIAL.
*      ls_message-message = |{ ls_message-message }, Thiếu thông tin Số hợp đồng của Tài khoản 242 |.
*    ENDIF.

    " Check Business Place
    IF ls_data-businessplace IS NOT INITIAL.
      READ TABLE gt_bplace INTO DATA(ls_bplace)
      WITH KEY branch = ls_data-businessplace.
      IF sy-subrc <> 0.
        ls_message-message = |{ ls_message-message }, Business Place { ls_data-businessplace } doesn't exist |.
      ENDIF.
    ENDIF.
    DATA: lw_koart TYPE koart.
    IF ls_data-customer IS NOT INITIAL.
      lw_koart = 'D'.
    ELSEIF ls_data-vendor_no IS NOT INITIAL.
      lw_koart = 'K'.
    ELSE.
      lw_koart = 'S'.
    ENDIF.

    " Check phan quyen segment
    READ TABLE gt_skb1_aut INTO DATA(ls_skb1) WITH KEY saknr = ls_data-gl_account
                                                       bukrs = ls_data-comp_code BINARY SEARCH.
    IF sy-subrc = 0.
      CALL FUNCTION 'AUTHORITY_ACCOUNT'
        EXPORTING
          i_begru      = ls_skb1-begru
          i_koart      = lw_koart
          i_actvt      = '01'
        EXCEPTIONS
          no_authority = 1.
      IF sy-subrc IS NOT INITIAL.
        ls_message-message = |{ ls_message-message }, You are not authorized to post in account  { ls_data-gl_account } |.
      ENDIF.
      "Check baseline date theo Account type add by VIETLT
      IF ls_skb1-mitkz = 'D' OR ls_skb1-mitkz = 'K'.
        IF ls_data-bline_date IS NOT INITIAL.
          CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
            EXPORTING
              date                      = ls_data-bline_date
            EXCEPTIONS
              plausibility_check_failed = 1
              OTHERS                    = 2.
          IF sy-subrc <> 0.
            ls_message-message = |{ ls_message-message }, Baseline Date Không đúng định dạng date |.
          ELSE.
            CALL FUNCTION 'LAST_DAY_OF_MONTHS'
              EXPORTING
                day_in            = ls_data-pstng_date
              IMPORTING
                last_day_of_month = lw_day
              EXCEPTIONS
                day_in_no_date    = 1
                OTHERS            = 2.
            IF ls_data-bline_date > lw_day.
              ls_message-message = |{ ls_message-message },  BaselineDate > Ngày cuối cùng của tháng Posting date |.
            ENDIF.
          ENDIF.
        ELSE.
          ls_message-message = |{ ls_message-message }, Baseline Date rỗng |.
        ENDIF.
      ENDIF.

      IF ls_skb1-fstag = 'G004'.
*        READ TABLE gt_csks INTO DATA(ls_csks)
*        WITH KEY kostl = ls_data-costcenter.
        LOOP AT gt_csks INTO DATA(ls_csks)
          WHERE kostl = ls_data-costcenter
              AND datab <= ls_data-pstng_date
              AND datbi >= ls_data-pstng_date.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          ls_message-message = |{ ls_message-message }, Cost Center { ls_data-costcenter } doesn't exist |.
        ELSE.
          READ TABLE gt_cepc_cc INTO DATA(ls_cepc1) WITH KEY prctr = ls_csks-prctr BINARY SEARCH.
          IF sy-subrc <> 0.
            ls_message-message = |{ ls_message-message }, Profit Center { ls_csks-prctr } doesn't exist |.
          ELSE.
            READ TABLE gt_fagl_segm INTO DATA(ls_segment3)
            WITH KEY segment = ls_cepc1-segment.
            IF sy-subrc <> 0.
              ls_message-message = |{ ls_message-message }, You are not authorized to post in Cost center { ls_data-costcenter } |.
            ENDIF.
            lw_prctr = ls_csks-prctr.
          ENDIF.
        ENDIF.
      ELSE.
        READ TABLE gt_cepc INTO DATA(ls_cepc) WITH KEY prctr = ls_data-profit_ctr BINARY SEARCH.
        IF sy-subrc <> 0.
          ls_message-message = |{ ls_message-message }, Profit Center { ls_data-profit_ctr } doesn't exist |.
        ELSE.
          READ TABLE gt_fagl_segm INTO DATA(ls_segment4)
          WITH KEY segment = ls_cepc-segment.
          IF sy-subrc <> 0.
            ls_message-message = |{ ls_message-message }, You are not authorized to post in Profit center { ls_data-profit_ctr } |.
          ENDIF.
          lw_prctr = ls_data-profit_ctr.
        ENDIF.
      ENDIF.
      IF ( ls_skb1-mitkz = 'D' OR ls_skb1-mitkz = 'K' ).
        IF ls_data-ref_key_1 IS INITIAL.
          ls_message-message = |{ ls_message-message }, Đề nghị điền Profit center vào Reference key 1 line dòng tài khoản công nợ |.
        ELSE.
          READ TABLE gt_cepc INTO DATA(ls_cepc_tmp) WITH KEY prctr = ls_data-ref_key_1 BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            ls_message-message = |{ ls_message-message }, Đề nghị kiểm tra lại mã Profit center { ls_data-ref_key_1 } |.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    "add by VIETLT
    IF ls_data-tax_code IS NOT INITIAL.
      "check số hóa đơn
      IF ls_data-ref_doc_no IS INITIAL.
        ls_message-message = |{ ls_message-message }, Yêu cầu nhập số hóa đơn |.
      ENDIF.

      "check mẫu hóa đơn
      IF ls_data-mau_hd IS INITIAL.
        ls_message-message = |{ ls_message-message }, Yêu cầu nhập mẫu hóa đơn |.
      ENDIF.
    ENDIF.
    "check mã số thu
*    DATA :length TYPE i,str TYPE string,
*          l_xletter TYPE xstring.
*
*    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
*      EXPORTING
*        text   = conv #( ls_data-tax_no_1 )
*      IMPORTING
*        buffer = l_xletter.
*
*    IF ls_data-tax_no_1 IS NOT INITIAL.
*      CALL FUNCTION 'STPU1_HEX_TO_CHAR'
*        EXPORTING
*          hex_string  = conv #( l_xletter )
*        IMPORTING
*          char_string = ls_data-tax_no_1.

*      DATA(length) =  cl_abap_list_utilities=>dynamic_output_length( ls_data-tax_no_1 ).
*      IF ( length <> 10 AND length <> 14 ).
*        ls_message-message = |{ ls_message-message }, Nhập đủ 10 hoặc 14 ký tự cho mã số thuế |.
*      ENDIF.
*    ENDIF.
*    CLEAR length.
    IF ls_data-tax_no_1 IS NOT INITIAL.
      DATA :lw_tax TYPE char16.
      lw_tax = ls_data-tax_no_1.
      CONDENSE lw_tax.
      DATA(length) = strlen( lw_tax ).
      IF ( length <> 10 AND length <> 14 ).
        ls_message-message = |{ ls_message-message }, Nhập đủ 10 hoặc 14 ký tự cho mã số thuế |.
      ENDIF.
    ENDIF.
    CLEAR length.

    "end by VIETLT


*    IF sy-tcode = 'ZFIE003' AND lw_prctr <> 'P0000010'.
*      IF '5,6,7,8'  CS ls_data-gl_account(1) OR ls_data-gl_account(3) = '242'.
*        ls_message-message = |{ ls_message-message }, không được phép hạch toán { ls_data-gl_account }/{ lw_prctr } |.
*      ENDIF.
*    ENDIF.
*    IF ls_data-segment IS NOT INITIAL.
*    READ TABLE gt_cepc INTO DATA(ls_cepc)
*    WITH KEY prctr = ls_data-profit_ctr.
*    IF sy-subrc <> 0.
*      ls_message-message = |{ ls_message-message }, Profit Center { ls_data-profit_ctr } doesn't exist |.
*    ELSE.
*      READ TABLE gt_fagl_segm INTO DATA(ls_segment2)
*      WITH KEY segment = ls_data-segment.
*      IF sy-subrc <> 0.
**          ls_message-message = |{ ls_message-message }, Profit Center { ls_data-profit_ctr } không thuộc segment của user |.
*        MESSAGE TEXT-006 TYPE 'S' DISPLAY LIKE 'E'.
*        LEAVE LIST-PROCESSING.
*      ENDIF.
*    ENDIF.
*    ENDIF.

    " Check segment
    IF ls_data-segment IS NOT INITIAL.
      READ TABLE gt_fagl_segm INTO DATA(ls_segment)
      WITH KEY segment = ls_data-segment.
      IF sy-subrc <> 0.
        ls_message-message = |{ ls_message-message }, Segment { ls_data-segment } doesn't exist |.
      ENDIF.
    ENDIF.
    " Check amount > 0.
    IF ls_data-amt_doccur <= 0.
      ls_message-message = |{ ls_message-message }, Uploaded correctly: Amount in local currency > 0|.
    ENDIF.
    " Check TAX amount >= 0.
    IF ls_data-tax_amt < 0 AND ls_data-tax_amt IS NOT INITIAL..
      ls_message-message = |{ ls_message-message }, Uploaded correctly: Tax Amount in local currency >= 0|.
    ENDIF.
    " Check date
*    IF ls_data-bline_date IS NOT INITIAL.
*      ls_message-message = |{ ls_message-message }, Input Baseline Date must be in the format 'dd/mm/yyyy|.
*    ENDIF.
    IF ls_data-pstng_date IS INITIAL.
      ls_message-message = |{ ls_message-message }, Input Posting Date must be in the format 'dd/mm/yyyy|.
    ENDIF.
    IF ls_data-doc_date IS INITIAL.
      ls_message-message = |{ ls_message-message }, Input Document Date must be in the format 'dd/mm/yyyy|.
    ENDIF.
*    IF ls_data-trans_date IS INITIAL.
*      ls_message-message = |{ ls_message-message }, Input Translation Date must be in the format 'dd/mm/yyyy|.
*    ENDIF.

    IF ls_data-gl_account CP '3335*'.
      IF ls_data-name_1 IS INITIAL OR ls_data-street IS INITIAL OR
         ls_data-city IS INITIAL OR ls_data-tax_no_1 IS INITIAL.
        ls_message-message = |{ ls_message-message }, Đề nghị điền thông tin thuế|.
      ENDIF.
    ENDIF.

    IF ls_data-neg_postng IS NOT INITIAL.
      READ TABLE gt_t003 INTO DATA(ls_t003)
      WITH KEY blart = ls_data-doc_type
      BINARY SEARCH.
      IF sy-subrc IS INITIAL AND ls_t003-xnegp IS INITIAL.
        ls_message-message = |{ ls_message-message }, Document type can't posting with negative|.
      ENDIF.
    ENDIF.
*    add by HUNGVT-check xrefkey3 - add by VIETLT-05.01.2023- check xrefkey3 account = 'H'
    IF sy-uname NE 'HUONGTT'.
      IF ls_data-gl_account+0(4) = '1411' AND ls_data-gl_account+0(5) <> '14116' AND ls_data-de_cre_ind1 = 'H'.
        IF ls_data-ref_key_3 IS NOT INITIAL.
          READ TABLE gt_bseg TRANSPORTING NO FIELDS WITH KEY xref3 = ls_data-ref_key_3
                                                             lifnr = ls_data-bp
                                                             bukrs = ls_data-comp_code.
          IF sy-subrc <> 0.
            ls_message-message = |{ ls_message-message }, reference key 3 does not exist|.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*    end by HUNGVT
    AT END OF stt.
      REPLACE ':, ' IN ls_message-message WITH ': '.
      SPLIT ls_message-message AT ':' INTO lw_mess lw_mess2.
      IF strlen( lw_mess2 ) > 1.
        ls_message-status = 1.
        ls_message-status2 = 1.
        APPEND ls_message TO gt_message.
      ELSE.
        ls_message-status = 3.
        ls_message-status2 = 3.
*      ls_message-message = |Line { lw_index  }: Format OK|.
        ls_message-message = |Line { ls_data-stt  }: Format OK|.
        APPEND ls_message TO gt_message.
      ENDIF.
      CLEAR ls_message.
    ENDAT.
    CLEAR lw_flg.
  ENDLOOP.
  READ TABLE gt_message INTO ls_message
  WITH KEY status = 1.
  IF sy-subrc = 0.
    IF gt_message IS NOT INITIAL.
      gw_check = 1.
      PERFORM message_alv.
    ENDIF.
  ELSE.
    FREE gt_message.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  DATA: lw_where      TYPE string,
        lr_prctr      TYPE RANGE OF prctr,
        lt_data       LIKE gt_data,
        lt_data_xref3 TYPE TABLE OF ty_data. "add by HUNGVT

  SELECT * FROM t003 INTO TABLE gt_t003.
  SORT gt_t003 BY blart.

  IF gt_data IS NOT INITIAL.
    SELECT *
      FROM t001
      INTO TABLE gt_t001
      FOR ALL ENTRIES IN gt_data
      WHERE bukrs = gt_data-comp_code.

    SELECT *
      FROM but000
      INTO TABLE gt_but000
      FOR ALL ENTRIES IN gt_data
      WHERE partner = gt_data-bp.

    SELECT *
      FROM a003
      INTO TABLE gt_a003
      FOR ALL ENTRIES IN gt_data
      WHERE mwskz = gt_data-tax_code
      AND  aland = 'VN'.

    SELECT *
      FROM j_1bbranch
      INTO TABLE gt_bplace
      FOR ALL ENTRIES IN gt_data
      WHERE branch = gt_data-businessplace.

*    SELECT *
**      FROM fagl_segm
*      FROM zddl_segment_zfir001
*      INTO CORRESPONDING FIELDS OF TABLE @gt_fagl_segm
*      FOR ALL ENTRIES IN @gt_data
*      WHERE segment = @gt_data-segment.
*    IF gt_fagl_segm IS INITIAL.
    SELECT *
      FROM zddl_segment_zfir001
      INTO CORRESPONDING FIELDS OF TABLE @gt_fagl_segm.
*    ENDIF.
    DATA(lt_data_tmp) = gt_data[].
    SORT lt_data_tmp BY ref_key_1.
    DELETE ADJACENT DUPLICATES FROM lt_data_tmp COMPARING ref_key_1.
    LOOP AT lt_data_tmp INTO DATA(ls_data).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_data-ref_key_1 ) TO lr_prctr.
    ENDLOOP.
    IF lr_prctr[] IS INITIAL.
      lw_where = 'prctr = gt_data-profit_ctr'.
    ELSE.
      lw_where = '( prctr = gt_data-profit_ctr OR prctr IN lr_prctr )'.
    ENDIF.
    SELECT *
      FROM cepc
      INTO TABLE gt_cepc
      FOR ALL ENTRIES IN gt_data
      WHERE (lw_where)
        AND kokrs = 1000.

    SORT gt_cepc BY prctr.
    " cc
    SELECT *
      FROM csks
      INTO TABLE gt_csks
      FOR ALL ENTRIES IN gt_data
      WHERE kostl = gt_data-costcenter
        AND kokrs = 1000.

    IF gt_csks IS NOT INITIAL.
      SELECT *
        FROM cepc
        INTO TABLE gt_cepc_cc
        FOR ALL ENTRIES IN gt_csks
        WHERE prctr = gt_csks-prctr
          AND kokrs = 1000.
      SORT gt_cepc_cc BY prctr.
    ENDIF.

    SELECT *
       FROM skb1
       INTO TABLE gt_skb1_aut
       FOR ALL ENTRIES IN gt_data
       WHERE saknr = gt_data-gl_account
         AND bukrs = gt_data-comp_code.

    SORT gt_skb1_aut BY saknr bukrs.

    " Xoa line BP = blank
    lt_data = gt_data.
    DELETE lt_data WHERE bp = ''.
    IF lt_data IS NOT INITIAL.
      SELECT *
        FROM skb1
        INTO TABLE gt_skb1
        FOR ALL ENTRIES IN lt_data
        WHERE saknr = lt_data-gl_account
         AND bukrs = lt_data-comp_code.

      " Check loi chua config
      IF gt_skb1 IS NOT INITIAL.
        READ TABLE gt_skb1 INTO DATA(ls_skb1)
        WITH KEY mitkz = ''.
        IF sy-subrc = 0.
          MESSAGE TEXT-e02 TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ENDIF.
    ENDIF.

*GET xrefkey 3
    lt_data_xref3 = gt_data.
    DELETE lt_data_xref3 WHERE gl_account+0(4) <> '1411' OR gl_account+0(5) = '14116'.
    DELETE lt_data_xref3 WHERE ref_key_3 IS INITIAL.
    SORT lt_data_xref3 BY comp_code ASCENDING bp ASCENDING ref_key_3 ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_data_xref3 COMPARING comp_code bp ref_key_3.
    IF lt_data_xref3 IS NOT INITIAL.
      SELECT DISTINCT bukrs, gjahr, belnr, lifnr, xref3   FROM bseg INTO CORRESPONDING FIELDS OF TABLE @gt_bseg
        FOR ALL ENTRIES IN @lt_data_xref3 WHERE bukrs = @lt_data_xref3-comp_code
                                         AND lifnr = @lt_data_xref3-bp
                                         AND xref3 = @lt_data_xref3-ref_key_3.
    ENDIF.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MESSAGE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM message_alv.
  DATA:it_fieldcat TYPE slis_t_fieldcat_alv,
       wa_fieldcat LIKE LINE OF it_fieldcat.
  DATA :program LIKE sy-repid VALUE sy-repid.
  DATA: icon_name(20) TYPE c.
  DEFINE      fieldcat.
    wa_fieldcat-fieldname = &1.
    wa_fieldcat-tabname   = &2.
    wa_fieldcat-seltext_m = &3.
    wa_fieldcat-outputlen = &4.
    wa_fieldcat-icon      = &5.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR  wa_fieldcat.
  END-OF-DEFINITION.
  IF  gw_check = 1.
    fieldcat: "Field   Int Tab  Text
            'STATUS' 'GT_MESSAGE' 'Status' '' 'X',
            'MESSAGE' 'GT_MESSAGE' 'Message' '250' ''.
  ELSEIF p_test IS NOT INITIAL.
    IF sy-tcode = 'ZFIF003'.
      fieldcat: "Field   Int Tab  Text
                  'STATUS' 'GT_MESSAGE' 'Status' '' 'X',
                  'MESSAGE' 'GT_MESSAGE' 'Message' '250' '',
*                'BELNR' 'GT_MESSAGE' 'Document Number' '' 'X',
                  'DOC_DATE' 'GT_MESSAGE' 'Document Date' '' '',
                  'PSTNG_DATE' 'GT_MESSAGE' 'Posting Date' '' '',
                  'HEADER_TXT' 'GT_MESSAGE' 'Document Header Text' '' '',
                  'MAU_HD' 'GT_MESSAGE' 'Mẫu hoá đơn' '' '',
                  'ref_doc_no' 'GT_MESSAGE' 'Reference' '' '',
                  'DOC_TYPE' 'GT_MESSAGE' 'Document Type' '' '',
                  'COMP_CODE' 'GT_MESSAGE' 'Company Code' '' '',
                  'CURRENCY' 'GT_MESSAGE' 'Currency' '' '',
                  'TRANS_DATE' 'GT_MESSAGE' 'Transaction Date' '' '',
                  'XREF1_HD' 'GT_MESSAGE' 'Reference key 1 Header' '' '',
                  'XREF2_HD' 'GT_MESSAGE' 'Reference key 2 Header' '' '',
                  'DE_CRE_IND1' 'GT_MESSAGE' 'Posting Key for Bank account' '' '',
                  'GL_ACCOUNT' 'GT_MESSAGE' 'Account' '' '',
                  'BP' 'GT_MESSAGE' 'BP' '' '',
                  'SP_GL_IND' 'GT_MESSAGE' 'Special GL indicator' '' '',
                  'AMT_DOCCUR' 'GT_MESSAGE' 'Amount in document currency' '' '',
                  'tax_code' 'GT_MESSAGE' 'Tax Code' '' '',
                  'tax_amt' 'GT_MESSAGE' 'Tax Amount' '' '',
                  'businessplace' 'GT_MESSAGE' 'Business Place' '' '',
                  'segment' 'GT_MESSAGE' 'Segment' '' '',
                  'COSTCENTER' 'GT_MESSAGE' 'Cost center' '' '',
                  'PROFIT_CTR' 'GT_MESSAGE' 'Profit center' '' '',
                  'ITEM_TEXT' 'GT_MESSAGE' 'Item text' '' '',
                  'ALLOC_NMBR' 'GT_MESSAGE' 'Assignment' '' '',
                  'PMNTTRMS' 'GT_MESSAGE' 'Payment Term' '' '',
                  'BLINE_DATE' 'GT_MESSAGE' 'Baseline date/ Value date' '' '',
                  'PMNT_BLOCK' 'GT_MESSAGE' 'Payment Block' '' '',
                  'PYMT_METH' 'GT_MESSAGE' 'Payment Method' '' '',
                  'PARTNER_BK' 'GT_MESSAGE' 'Partner Bank Type' '' '',
                  'neg_postng' 'GT_MESSAGE' 'Negative Posting' '' '',
                  'ref_key_1' 'GT_MESSAGE' 'Reference key 1 (Cash Flow ID)' '' '',
                  'ref_key_2' 'GT_MESSAGE' 'Reference key 2' '' '',
                  'ref_key_3' 'GT_MESSAGE' 'Reference key 3' '' '',
                  'name_1' 'GT_MESSAGE' 'Name (Individual Payee)' '' '',
                  'street' 'GT_MESSAGE' 'Street' '' '',
                  'city' 'GT_MESSAGE' 'City' '' '',
                  'tax_no_1' 'GT_MESSAGE' 'Tax number' '' ''.
*            'LINE' 'GT_MESSAGE' 'Line' '' ''.
    ELSE.
      fieldcat: "Field   Int Tab  Text
                  'STATUS' 'GT_MESSAGE' 'Status' '' 'X',
                  'MESSAGE' 'GT_MESSAGE' 'Message' '250' '',
*                'BELNR' 'GT_MESSAGE' 'Document Number' '' 'X',
                  'DOC_DATE' 'GT_MESSAGE' 'Document Date' '' '',
                  'PSTNG_DATE' 'GT_MESSAGE' 'Posting Date' '' '',
                  'HEADER_TXT' 'GT_MESSAGE' 'Document Header Text' '' '',
                  'MAU_HD' 'GT_MESSAGE' 'Mẫu hoá đơn' '' '',
                  'ref_doc_no' 'GT_MESSAGE' 'Reference' '' '',
                  'DOC_TYPE' 'GT_MESSAGE' 'Document Type' '' '',
                  'COMP_CODE' 'GT_MESSAGE' 'Company Code' '' '',
                  'CURRENCY' 'GT_MESSAGE' 'Currency' '' '',
                  'TRANS_DATE' 'GT_MESSAGE' 'Transaction Date' '' '',
                  'DE_CRE_IND1' 'GT_MESSAGE' 'Posting Key for Bank account' '' '',
                  'GL_ACCOUNT' 'GT_MESSAGE' 'Account' '' '',
                  'BP' 'GT_MESSAGE' 'BP' '' '',
                  'SP_GL_IND' 'GT_MESSAGE' 'Special GL indicator' '' '',
                  'AMT_DOCCUR' 'GT_MESSAGE' 'Amount in document currency' '' '',
                  'tax_code' 'GT_MESSAGE' 'Tax Code' '' '',
                  'tax_amt' 'GT_MESSAGE' 'Tax Amount' '' '',
                  'businessplace' 'GT_MESSAGE' 'Business Place' '' '',
                  'segment' 'GT_MESSAGE' 'Segment' '' '',
                  'COSTCENTER' 'GT_MESSAGE' 'Cost center' '' '',
                  'PROFIT_CTR' 'GT_MESSAGE' 'Profit center' '' '',
                  'ITEM_TEXT' 'GT_MESSAGE' 'Item text' '' '',
                  'ALLOC_NMBR' 'GT_MESSAGE' 'Assignment' '' '',
                  'PMNTTRMS' 'GT_MESSAGE' 'Payment Term' '' '',
                  'BLINE_DATE' 'GT_MESSAGE' 'Baseline date/ Value date' '' '',
                  'PMNT_BLOCK' 'GT_MESSAGE' 'Payment Block' '' '',
                  'PYMT_METH' 'GT_MESSAGE' 'Payment Method' '' '',
                  'PARTNER_BK' 'GT_MESSAGE' 'Partner Bank Type' '' '',
                  'neg_postng' 'GT_MESSAGE' 'Negative Posting' '' '',
                  'ref_key_1' 'GT_MESSAGE' 'Reference key 1 (Cash Flow ID)' '' '',
                  'ref_key_2' 'GT_MESSAGE' 'Reference key 2' '' '',
                  'ref_key_3' 'GT_MESSAGE' 'Reference key 3' '' '',
                  'name_1' 'GT_MESSAGE' 'Name (Individual Payee)' '' '',
                  'street' 'GT_MESSAGE' 'Street' '' '',
                  'city' 'GT_MESSAGE' 'City' '' '',
                  'tax_no_1' 'GT_MESSAGE' 'Tax number' '' ''.
*            'LINE' 'GT_MESSAGE' 'Line' '' ''.
    ENDIF.
  ELSE.
    IF gw_check_post = abap_true.
      fieldcat: "Field   Int Tab  Text
                'STATUS' 'GT_MESSAGE' 'Status' '' 'X',
                'BELNR' 'GT_MESSAGE' 'Document Number' '' '',
                'MESSAGE' 'GT_MESSAGE' 'Message' '250' '',
                'DOC_DATE' 'GT_MESSAGE' 'Document Date' '' '',
                'PSTNG_DATE' 'GT_MESSAGE' 'Posting Date' '' '',
                'HEADER_TXT' 'GT_MESSAGE' 'Document Header Text' '' '',
                'MAU_HD' 'GT_MESSAGE' 'Mẫu hoá đơn' '' '',
                'ref_doc_no' 'GT_MESSAGE' 'Reference' '' '',
                'DOC_TYPE' 'GT_MESSAGE' 'Document Type' '' '',
                'COMP_CODE' 'GT_MESSAGE' 'Company Code' '' '',
                'CURRENCY' 'GT_MESSAGE' 'Currency' '' '',
                'TRANS_DATE' 'GT_MESSAGE' 'Transaction Date' '' '',
                'XREF1_HD' 'GT_MESSAGE' 'Reference key 1 Header' '' '',
                'XREF2_HD' 'GT_MESSAGE' 'Reference key 2 Header' '' '',
                'DE_CRE_IND1' 'GT_MESSAGE' 'Posting Key for Bank account' '' '',
                'GL_ACCOUNT' 'GT_MESSAGE' 'Account' '' '',
                'BP' 'GT_MESSAGE' 'BP' '' '',
                'SP_GL_IND' 'GT_MESSAGE' 'Special GL indicator' '' '',
                'AMT_DOCCUR' 'GT_MESSAGE' 'Amount in document currency' '' '',
                'tax_code' 'GT_MESSAGE' 'Tax Code' '' '',
                'tax_amt' 'GT_MESSAGE' 'Tax Amount' '' '',
                'businessplace' 'GT_MESSAGE' 'Business Place' '' '',
                'segment' 'GT_MESSAGE' 'Segment' '' '',
                'COSTCENTER' 'GT_MESSAGE' 'Cost center' '' '',
                'PROFIT_CTR' 'GT_MESSAGE' 'Profit center' '' '',
                'ITEM_TEXT' 'GT_MESSAGE' 'Item text' '' '',
                'ALLOC_NMBR' 'GT_MESSAGE' 'Assignment' '' '',
                'PMNTTRMS' 'GT_MESSAGE' 'Payment Term' '' '',
                'BLINE_DATE' 'GT_MESSAGE' 'Baseline date/ Value date' '' '',
                'PMNT_BLOCK' 'GT_MESSAGE' 'Payment Block' '' '',
                'PYMT_METH' 'GT_MESSAGE' 'Payment Method' '' '',
                'PARTNER_BK' 'GT_MESSAGE' 'Partner Bank Type' '' '',
                'neg_postng' 'GT_MESSAGE' 'Negative Posting' '' '',
                'ref_key_1' 'GT_MESSAGE' 'Reference key 1 (Cash Flow ID)' '' '',
                'ref_key_2' 'GT_MESSAGE' 'Reference key 2' '' '',
                'ref_key_3' 'GT_MESSAGE' 'Reference key 3' '' '',
                'name_1' 'GT_MESSAGE' 'Name (Individual Payee)' '' '',
                'street' 'GT_MESSAGE' 'Street' '' '',
                'city' 'GT_MESSAGE' 'City' '' '',
                'tax_no_1' 'GT_MESSAGE' 'Tax number' '' ''.
    ELSE.
      fieldcat: "Field   Int Tab  Text
                'STATUS' 'GT_MESSAGE' 'Status' '' 'X',
                'BELNR' 'GT_MESSAGE' 'Document Number' '' '',
                'MESSAGE' 'GT_MESSAGE' 'Message' '250' '',
                'DOC_DATE' 'GT_MESSAGE' 'Document Date' '' '',
                'PSTNG_DATE' 'GT_MESSAGE' 'Posting Date' '' '',
                'HEADER_TXT' 'GT_MESSAGE' 'Document Header Text' '' '',
                'MAU_HD' 'GT_MESSAGE' 'Mẫu hoá đơn' '' '',
                'DOC_TYPE' 'GT_MESSAGE' 'Document Type' '' '',
                'COMP_CODE' 'GT_MESSAGE' 'Company Code' '' '',
                'CURRENCY' 'GT_MESSAGE' 'Currency' '' '',
                'TRANS_DATE' 'GT_MESSAGE' 'Transaction Date' '' '',
                'DE_CRE_IND1' 'GT_MESSAGE' 'Posting Key for Bank account' '' '',
                'GL_ACCOUNT' 'GT_MESSAGE' 'Account' '' '',
                'BP' 'GT_MESSAGE' 'BP' '' '',
                'SP_GL_IND' 'GT_MESSAGE' 'Special GL indicator' '' '',
                'AMT_DOCCUR' 'GT_MESSAGE' 'Amount in document currency' '' '',
                'tax_code' 'GT_MESSAGE' 'Tax Code' '' '',
                'tax_amt' 'GT_MESSAGE' 'Tax Amount' '' '',
                'businessplace' 'GT_MESSAGE' 'Business Place' '' '',
                'segment' 'GT_MESSAGE' 'Segment' '' '',
                'COSTCENTER' 'GT_MESSAGE' 'Cost center' '' '',
                'PROFIT_CTR' 'GT_MESSAGE' 'Profit center' '' '',
                'ITEM_TEXT' 'GT_MESSAGE' 'Item text' '' '',
                'ALLOC_NMBR' 'GT_MESSAGE' 'Assignment' '' '',
                'PMNTTRMS' 'GT_MESSAGE' 'Payment Term' '' '',
                'BLINE_DATE' 'GT_MESSAGE' 'Baseline date/ Value date' '' '',
                'PMNT_BLOCK' 'GT_MESSAGE' 'Payment Block' '' '',
                'PYMT_METH' 'GT_MESSAGE' 'Payment Method' '' '',
                'PARTNER_BK' 'GT_MESSAGE' 'Partner Bank Type' '' '',
                'neg_postng' 'GT_MESSAGE' 'Negative Posting' '' '',
                'ref_key_1' 'GT_MESSAGE' 'Reference key 1 (Cash Flow ID)' '' '',
                'ref_key_2' 'GT_MESSAGE' 'Reference key 2' '' '',
                'ref_key_3' 'GT_MESSAGE' 'Reference key 3' '' '',
                'name_1' 'GT_MESSAGE' 'Name (Individual Payee)' '' '',
                'street' 'GT_MESSAGE' 'Street' '' '',
                'city' 'GT_MESSAGE' 'City' '' '',
                'tax_no_1' 'GT_MESSAGE' 'Tax number' '' ''.
    ENDIF.
  ENDIF.
  LOOP AT gt_message ASSIGNING FIELD-SYMBOL(<lf_mess>).
    IF <lf_mess>-status2 = 1.
      icon_name = 'ICON_RED_LIGHT'.
    ELSEIF <lf_mess>-status2 = 2.
      icon_name = 'ICON_YELLOW_LIGHT'.
    ELSE.
      icon_name = 'ICON_GREEN_LIGHT'.
    ENDIF.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = icon_name
        info                  = 'Status'
        add_stdinf            = 'X'
      IMPORTING
        result                = <lf_mess>-status
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
    CLEAR icon_name.
  ENDLOOP.
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program = program
      it_fieldcat        = it_fieldcat
    TABLES
      t_outtab           = gt_message.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_COL_HEADING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM change_col_heading  USING po_alv TYPE REF TO cl_salv_table
                              p_column_name
                              p_short_text TYPE scrtext_s
                              p_medium_text TYPE scrtext_m
                              p_long_text  TYPE scrtext_l
                              p_visible TYPE char1.
  DATA: lo_columns TYPE REF TO cl_salv_columns_table,
        lo_column  TYPE REF TO cl_salv_column_table,
        lo_ex      TYPE REF TO cx_salv_not_found.

  lo_columns = po_alv->get_columns( ).

  TRY.
      lo_column ?= lo_columns->get_column( p_column_name ).
      lo_column->set_short_text( p_short_text ).
      lo_column->set_medium_text( p_medium_text ).
      lo_column->set_long_text( p_long_text ).
      IF p_visible = 'X'.
        lo_column->set_visible( abap_false ).
        lo_column->set_output_length( '20' ).
      ENDIF.
    CATCH cx_salv_not_found INTO lo_ex.
      MESSAGE lo_ex TYPE 'E'.
  ENDTRY.

  IF p_column_name = 'STATUS'.
    TRY.
        lo_columns->set_exception_column( value = 'STATUS' ).
      CATCH cx_salv_data_error.
    ENDTRY.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_log .
  DATA: ls_fie001_file TYPE ztb_fie001_file.
  CALL FUNCTION 'GUID_CREATE'
    IMPORTING
      ev_guid_22 = ls_fie001_file-guid.
  ls_fie001_file-file_name = gw_fname.
  ls_fie001_file-usr_name = sy-uname.
  ls_fie001_file-crdate = sy-datum.
  ls_fie001_file-crtime = sy-uzeit.
  INSERT ztb_fie001_file FROM ls_fie001_file.
  COMMIT WORK AND WAIT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SPLIT_FILE_NAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM split_file_name .
  DATA:
    lw_file LIKE rlgrap-filename,
    lw_off  TYPE int4.
  lw_file = p_file.

  REPLACE ALL OCCURRENCES OF '/' IN lw_file WITH '\'.

  FIND ALL OCCURRENCES OF '\' IN lw_file MATCH OFFSET lw_off.
  IF sy-subrc > 0.
    gw_fname = lw_file.
  ELSE.
    lw_off = lw_off + 1.
    gw_fname = lw_file+lw_off.
  ENDIF.
  TRANSLATE gw_fname TO UPPER CASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_log .
  DATA: lw_mess TYPE text100.

  SELECT SINGLE * FROM ztb_fie001_file INTO @DATA(ls_fie001_file)
    WHERE file_name = @gw_fname.
  IF sy-subrc IS INITIAL.
    lw_mess = TEXT-e03.
    REPLACE '&1' IN lw_mess WITH ls_fie001_file-file_name.
    REPLACE '&2' IN lw_mess WITH ls_fie001_file-usr_name.
    REPLACE '&3' IN lw_mess WITH ls_fie001_file-crdate.
    REPLACE '&4' IN lw_mess WITH ls_fie001_file-crtime.
    MESSAGE lw_mess TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESS_DATA_TAX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_data_tax.
  DATA: lw_index      TYPE sy-tabix,
        lw_message    TYPE char255,
        lw_amount     TYPE zdd_dec23,
        ls_message    TYPE ty_mess,
        lw_newbs      TYPE newbs,
        lw_date_c     TYPE bdc_fval,
        lw_value      TYPE bdc_fval,
        lt_bdcdata    TYPE gty_t_bdcdata,
        lt_bdcmsgcoll TYPE TABLE OF bdcmsgcoll.
  DATA(lt_data) = gt_data[].
  REFRESH: gt_message[].
  SORT lt_data BY stt tax_code DESCENDING.
  LOOP AT lt_data INTO DATA(ls_data).
    lw_index = sy-tabix.
    SELECT SINGLE mitkz
      FROM skb1
      INTO @DATA(lw_mitkz)
      WHERE bukrs = @ls_data-comp_code
        AND saknr = @ls_data-gl_account.
    IF sy-subrc IS INITIAL.
      CASE lw_mitkz.
        WHEN ''.
          IF ls_data-de_cre_ind1 = 'S'.
            lw_newbs = '40'.
          ELSEIF ls_data-de_cre_ind1 = 'H'.
            lw_newbs = '50'.
          ENDIF.
        WHEN 'K'.
          IF ls_data-de_cre_ind1 = 'S'.
            lw_newbs = '21'.
          ELSEIF ls_data-de_cre_ind1 = 'H'.
            lw_newbs = '31'.
          ENDIF.
        WHEN 'D'.
          IF ls_data-de_cre_ind1 = 'S'.
            lw_newbs = '01'.
          ELSEIF ls_data-de_cre_ind1 = 'H'.
            lw_newbs = '11'.
          ENDIF.
        WHEN 'A'.
          IF ls_data-de_cre_ind1 = 'S'.
            lw_newbs = '70'.
          ELSEIF ls_data-de_cre_ind1 = 'H'.
            lw_newbs = '75'.
          ENDIF.
      ENDCASE.
    ENDIF.
    IF ls_data-tax_code IS NOT INITIAL.
      PERFORM bdc_dynpro USING 'SAPMF05A' '0100'
                         CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BDC_CURSOR' 'RF05A-NEWKO'
                        CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BDC_OKCODE' '/00'
                        CHANGING lt_bdcdata.
      PERFORM convert_date_to_char USING ls_data-doc_date
                                   CHANGING lw_date_c.
      PERFORM bdc_field USING 'BKPF-BLDAT' lw_date_c
                        CHANGING lt_bdcdata.
      lw_value = ls_data-doc_type.
      CONDENSE lw_value.
      PERFORM bdc_field USING 'BKPF-BLART' lw_value
                        CHANGING lt_bdcdata.
      lw_value = ls_data-comp_code.
      CONDENSE lw_value.
      PERFORM bdc_field USING 'BKPF-BUKRS' lw_value
                        CHANGING lt_bdcdata.
      PERFORM convert_date_to_char USING ls_data-pstng_date
                                   CHANGING lw_date_c.
      PERFORM bdc_field USING 'BKPF-BUDAT' lw_date_c
                        CHANGING lt_bdcdata.
      lw_value = ls_data-currency.
      CONDENSE lw_value.
      PERFORM bdc_field USING 'BKPF-WAERS' lw_value
                        CHANGING lt_bdcdata.
      lw_value = ls_data-header_txt.
      CONDENSE lw_value.
      PERFORM bdc_field USING 'BKPF-BKTXT' lw_value
                        CHANGING lt_bdcdata.
      lw_value = ls_data-ref_doc_no.
      CONDENSE lw_value.
      PERFORM bdc_field USING 'BKPF-XBLNR' lw_value
                        CHANGING lt_bdcdata.
      PERFORM convert_date_to_char USING ls_data-trans_date
                                   CHANGING lw_date_c.
      PERFORM bdc_field USING 'BKPF-WWERT' lw_date_c
                        CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'FS006-DOCID' '*'
                        CHANGING lt_bdcdata.
      lw_value = lw_newbs.
      CONDENSE lw_value.
      PERFORM bdc_field USING 'RF05A-NEWBS'  lw_value         " Nhap thong tin dong thue suat
                        CHANGING lt_bdcdata.
      lw_value = ls_data-gl_account.
      CONDENSE lw_value.
      PERFORM bdc_field USING 'RF05A-NEWKO' lw_value    " Tai khoan
                        CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BDC_SUBSCR' 'SAPMF05A                                1300APPL_SUB_T'
                        CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BDC_SUBSCR' 'SAPLSEXM                                0200APPL_SUB'
                        CHANGING lt_bdcdata.
      PERFORM bdc_dynpro USING 'SAPMF05A' '0312'
                         CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BDC_CURSOR' 'RF05A-NEWKO'
                        CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BDC_OKCODE' '/00'
                        CHANGING lt_bdcdata.
      lw_amount = ls_data-tax_amt.
      lw_value = lw_amount.
      CONDENSE lw_value.
      PERFORM bdc_field USING 'BSEG-WRBTR' lw_value     "so tien hach toán thue suat
                        CHANGING lt_bdcdata.
      lw_value = ls_data-tax_code.
      CONDENSE lw_value.
      PERFORM bdc_field USING 'BSEG-MWSKZ' lw_value       "loai thue suat
                        CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'RF05A-XSTBA' 'X'
                        CHANGING lt_bdcdata.
    ENDIF.
    IF ls_data-tax_code IS INITIAL.
      lw_value = lw_newbs.
      CONDENSE lw_value.
      PERFORM bdc_field USING 'RF05A-NEWBS' lw_value      " Nhap thong tin dong vendor
                        CHANGING lt_bdcdata.
      lw_value = ls_data-bp.
      CONDENSE lw_value.
      PERFORM bdc_field USING 'RF05A-NEWKO' lw_value   " Tai khoan
                        CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BDC_SUBSCR' 'SAPLKACB                                0001BLOCK'
                        CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'DKACB-FMORE' 'X'
                        CHANGING lt_bdcdata.
*  Nhap profit center
      PERFORM bdc_dynpro USING 'SAPLKACB' '0002'
                         CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BDC_CURSOR' 'COBL-PRCTR'
                        CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BDC_OKCODE' '=ENTE'
                        CHANGING lt_bdcdata.
      lw_value = ls_data-profit_ctr.
      CONDENSE lw_value.
      PERFORM bdc_field USING 'COBL-PRCTR' lw_value
                        CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BDC_SUBSCR' 'SAPLKACB                                9999BLOCK1'
                        CHANGING lt_bdcdata.
      PERFORM bdc_dynpro USING 'SAPMF05A' '0312'
                         CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BDC_SUBSCR' 'SAPMF05A                                0001COST_OBJ'
                        CHANGING lt_bdcdata.
      PERFORM bdc_dynpro USING 'SAPMF05A' '0302'
                         CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BDC_CURSOR' 'BSEG-WRBTR'
                        CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BDC_OKCODE' '=BU'
                        CHANGING lt_bdcdata.
      lw_value = ls_data-gl_account.
      CONDENSE lw_value.
      PERFORM bdc_field USING 'BSEG-HKONT' lw_value     "tai khoan
                        CHANGING lt_bdcdata.
      lw_amount = ls_data-amt_doccur.
      lw_value = lw_amount.
      CONDENSE lw_value.
      PERFORM bdc_field USING 'BSEG-WRBTR' lw_value     "so tien
                        CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BSEG-MWSKZ' '**'
                        CHANGING lt_bdcdata.
      PERFORM convert_date_to_char USING ls_data-doc_date
                                   CHANGING lw_date_c.
      PERFORM bdc_field USING 'BSEG-ZFBDT' lw_date_c
                        CHANGING lt_bdcdata.

      PERFORM bdc_dynpro USING 'SAPMF05A' '0700'
                         CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BDC_CURSOR' 'RF05A-NEWBS'
                        CHANGING lt_bdcdata.
      PERFORM bdc_field USING 'BDC_OKCODE' '/00'
                        CHANGING lt_bdcdata.
    ENDIF.
    AT END OF stt.
      CALL TRANSACTION 'F-02' USING lt_bdcdata MODE 'N'
      MESSAGES INTO lt_bdcmsgcoll.
      IF lt_bdcmsgcoll[] IS NOT INITIAL.
        ls_message-message = |Document { lw_index }: |.
        READ TABLE lt_bdcmsgcoll INTO DATA(ls_bdcmsgcoll) INDEX 1.
        IF sy-subrc = 0.
          IF ls_bdcmsgcoll-msgtyp = 'E'.
*            DELETE lt_return INDEX 1.
*            lw_error = abap_true.
            ls_message-status = 1.
            ls_message-status2 = 1.
          ELSE.
            ls_message-status = 3.
            ls_message-status2 = 3.
          ENDIF.
        ENDIF.
        LOOP AT lt_bdcmsgcoll INTO ls_bdcmsgcoll.
          MESSAGE ID ls_bdcmsgcoll-msgid TYPE ls_bdcmsgcoll-msgtyp NUMBER ls_bdcmsgcoll-msgnr
          INTO lw_message
          WITH ls_bdcmsgcoll-msgv1 ls_bdcmsgcoll-msgv2 ls_bdcmsgcoll-msgv3 ls_bdcmsgcoll-msgv4.
          IF sy-tabix = 1.
            ls_message-message = |{ ls_message-message } { lw_message }|.
          ELSE.
            ls_message-message = |{ ls_message-message }, { lw_message }|.
          ENDIF.
          CLEAR lw_message.
        ENDLOOP.
        APPEND ls_message TO gt_message.
        CLEAR: ls_message.
      ENDIF.
      REFRESH: lt_bdcdata[],
               lt_bdcmsgcoll.
    ENDAT.
    CLEAR: lw_newbs,
           lw_mitkz.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_DYNPRO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM bdc_dynpro USING lpw_prog TYPE bdc_prog
                      lpw_dynpro TYPE bdc_dynr
                CHANGING  lpt_bdcdata TYPE gty_t_bdcdata.
  DATA: ls_bdcdata TYPE bdcdata.
  ls_bdcdata-program = lpw_prog.
  ls_bdcdata-dynpro = lpw_dynpro.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata TO lpt_bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BDC_FIELD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ‘PAYR_VBLNR’
*&      --> WA_FILE–VBLNR
*&---------------------------------------------------------------------*
FORM bdc_field  USING lpw_fnam TYPE fnam_____4
                      lpw_fval TYPE bdc_fval
                CHANGING  lpt_bdcdata TYPE gty_t_bdcdata.
  DATA: ls_bdcdata TYPE bdcdata.
  ls_bdcdata-fnam = lpw_fnam.
  ls_bdcdata-fval = lpw_fval.
  APPEND ls_bdcdata TO lpt_bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERT_DATE_TO_CHAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA_PSTNG_DATE
*&      <-- LW_DATE_C
*&---------------------------------------------------------------------*
FORM convert_date_to_char  USING    lpw_date TYPE dats
                           CHANGING lpw_date_c TYPE bdc_fval.
  CLEAR lpw_date_c.
  CONCATENATE lpw_date+6(2) lpw_date+4(2) lpw_date(4) INTO lpw_date_c SEPARATED BY '.'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_PARK_DOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_DOC_OBJKEY
*&      --> LS_OUT_STT
*&---------------------------------------------------------------------*
FORM update_park_doc  USING lpv_doc_objkey TYPE bapiache09-obj_key
                            lpv_stt TYPE i.
  DATA: lt_bseg_upd TYPE TABLE OF vbsegs,
        lt_tax_info TYPE TABLE OF ftaxp.

  SELECT *
    FROM vbsegs     "bseg
    INTO TABLE @DATA(lt_bseg)
    WHERE bukrs = @lpv_doc_objkey+10(4)
      AND belnr = @lpv_doc_objkey+0(10)
      AND gjahr = @lpv_doc_objkey+14(4).
  IF sy-subrc IS INITIAL.
    LOOP AT lt_bseg ASSIGNING FIELD-SYMBOL(<lfs_bseg>).
      READ TABLE gt_data INTO DATA(ls_data) WITH KEY stt = lpv_stt BINARY SEARCH.
      IF sy-subrc IS INITIAL AND ls_data-acrobj_id IS NOT INITIAL.
        <lfs_bseg>-acrobj_id = ls_data-acrobj_id.
        APPEND <lfs_bseg> TO lt_bseg_upd.
      ENDIF.
    ENDLOOP.
    UPDATE vbsegs FROM TABLE lt_bseg_upd.
    REFRESH: lt_bseg_upd.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA
*&      <-- LW_ERR
*&---------------------------------------------------------------------*
FORM check_date  USING    lpw_date TYPE dats
                 CHANGING lpw_err TYPE char1.
  CLEAR lpw_err.
  CHECK lpw_date IS NOT INITIAL.
  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = lpw_date
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    lpw_err = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_NULL_REFKEY3
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM check_null_refkey3 USING index ls_data TYPE ty_upload_post.
*  DATA :lw_line TYPE int4.
*  IF sy-tcode = 'ZFIE001'.
*    lw_line = 1.
*  ELSEIF sy-tcode = 'ZFIE003'.
*    lw_line = 2.
*  ENDIF.
*  IF ls_data-de_cre_ind1 = 'H' AND ls_data-gl_account+0(3) = '335' AND ls_data-ref_key_3 IS INITIAL.
*    MESSAGE |Nhập thiếu refkey 3 line { index + lw_line }| TYPE 'S' DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING.
*    EXIT.
*  ENDIF.
*ENDFORM.
