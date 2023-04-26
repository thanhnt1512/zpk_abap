METHOD zii_si_zfii017m_bp_in~si_zfii017m_bp_in.
  TYPES :BEGIN OF lty_system_uuid,
           partner_guid TYPE bu_partner_guid_bapi,
         END OF lty_system_uuid.


  DATA: lt_cvis_ei_extern_t TYPE cvis_ei_extern_t,
        ls_cvis_ei_extern   TYPE cvis_ei_extern,
        lt_guid_ins         TYPE TABLE OF lty_system_uuid,
        lt_return           TYPE  bapiretm.

  DATA :lw_fullname(80)    TYPE c,
        lw_fullstreet(300) TYPE c.

  DATA: lt_out_line  TYPE TABLE OF bu_namep_f,
        lw_name_org1 TYPE bu_namep_f,
        lw_name_org2 TYPE bu_namep_f,
        lw_name_org3 TYPE bu_namep_f,
        lw_name_org4 TYPE bu_namep_f.

  DATA: lw_street1 TYPE bu_namep_f,
        lw_street2 TYPE bu_namep_f,
        lw_city1   TYPE bu_namep_f.

  DATA: ls_object_key   TYPE  swo_typeid,
        ls_folder_key   TYPE  soodk,
        ls_url_key      TYPE  soodk,
        ls_note_key     TYPE  soodk,
        lv_url          TYPE  soli-line,
        lt_note_content TYPE  soli_tab,
        ls_note_content TYPE  soli,
        lv_urldes       TYPE  sood1-objdes,
        ls_note_title   TYPE  sood1-objdes,
        ls_payment_info TYPE  ztb_paymt_period,
        lv_column       TYPE  string,
        lv_count        TYPE  char2.
  DATA :date_default TYPE dats VALUE '99991231'.

  SELECT partner,partner_guid FROM but000 INTO TABLE @DATA(lt_guid).
  IF sy-subrc = 0.
    SORT lt_guid BY partner.
  ENDIF.

  ls_cvis_ei_extern-partner-header-object = 'BusinessPartner'.
  ls_cvis_ei_extern-partner-header-object_instance-bpartner = input-mt_zfii017m_bp_in-bp_maintain-partner_number.
  ls_cvis_ei_extern-partner-header-object_task = input-mt_zfii017m_bp_in-bp_maintain-object_task.

  IF  ls_cvis_ei_extern-partner-header-object_task = 'I'.
    ls_cvis_ei_extern-partner-header-object_instance-bpartnerguid = cl_system_uuid=>create_uuid_x16_static( ).
  ELSEIF ls_cvis_ei_extern-partner-header-object_task = 'U'.
    SELECT SINGLE partner_guid FROM but000 INTO @DATA(lv_guid) WHERE partner = @ls_cvis_ei_extern-partner-header-object_instance-bpartner.
    IF sy-subrc IS INITIAL.
      ls_cvis_ei_extern-partner-header-object_instance-bpartnerguid = lv_guid.
    ELSE.
    ENDIF.
    ls_cvis_ei_extern-partner-central_data-common-datax-bp_person-fullname = 'X'.
    ls_cvis_ei_extern-partner-central_data-common-datax-bp_centraldata-title_key = 'X'.
    ls_cvis_ei_extern-partner-central_data-common-datax-bp_centraldata-partnertype = 'X'.
    ls_cvis_ei_extern-partner-central_data-common-datax-bp_group-grouptype = 'X'.
    ls_cvis_ei_extern-partner-central_data-common-datax-bp_organization-name2 = 'X'.
    ls_cvis_ei_extern-partner-central_data-common-datax-bp_organization-name3 = 'X'.
    ls_cvis_ei_extern-partner-central_data-common-datax-bp_organization-name4 = 'X'.

  ENDIF.
  ls_cvis_ei_extern-partner-central_data-common-data-bp_control-grouping = input-mt_zfii017m_bp_in-bp_maintain-grouping.
  IF input-mt_zfii017m_bp_in-bp_maintain-grouping IS INITIAL.
    ls_cvis_ei_extern-partner-central_data-common-data-bp_control-grouping = 'B001'.
  ENDIF.
  ls_cvis_ei_extern-partner-central_data-common-data-bp_control-category = input-mt_zfii017m_bp_in-bp_maintain-category.
  ls_cvis_ei_extern-partner-central_data-common-data-bp_centraldata-partnertype = input-mt_zfii017m_bp_in-bp_maintain-general_data-partnertype.
  ls_cvis_ei_extern-partner-central_data-common-data-bp_centraldata-title_key = input-mt_zfii017m_bp_in-bp_maintain-general_data-title_key.

  IF input-mt_zfii017m_bp_in-bp_maintain-general_data-full_name IS NOT INITIAL.
    lw_fullname = input-mt_zfii017m_bp_in-bp_maintain-general_data-full_name.
    IF input-mt_zfii017m_bp_in-bp_maintain-category = '1'.
      ls_cvis_ei_extern-partner-central_data-common-data-bp_person-fullname = lw_fullname.
    ELSEIF input-mt_zfii017m_bp_in-bp_maintain-category = '2'.
      " Cắt tròn chữ cho thông tin name1, name2, name3, name4
      CALL FUNCTION 'RKD_WORD_WRAP'
        EXPORTING
          textline            = lw_fullname
          outputlen           = 40
        TABLES
          out_lines           = lt_out_line
        EXCEPTIONS
          outputlen_too_large = 1
          OTHERS              = 2.
      IF sy-subrc = 0.
        LOOP AT lt_out_line INTO DATA(ls_out_line).
          CASE sy-tabix.
            WHEN 1.
              lw_name_org2 = ls_out_line.
            WHEN 2.
              lw_name_org3 = ls_out_line.
            WHEN 3.
              lw_name_org4 = ls_out_line.
          ENDCASE.
        ENDLOOP.
        CLEAR ls_out_line.
        REFRESH: lt_out_line.
      ENDIF.
      ls_cvis_ei_extern-partner-central_data-common-data-bp_organization-name2 = lw_name_org2.
      ls_cvis_ei_extern-partner-central_data-common-data-bp_organization-name3 = lw_name_org3.
      ls_cvis_ei_extern-partner-central_data-common-data-bp_organization-name4 = lw_name_org4.
    ENDIF.
  ENDIF.

  "VAT

  IF input-mt_zfii017m_bp_in-bp_maintain-general_data-vat_registration_no IS NOT INITIAL.
    APPEND INITIAL LINE TO ls_cvis_ei_extern-partner-central_data-ident_number-ident_numbers ASSIGNING FIELD-SYMBOL(<fs_ident_numbers>).
    <fs_ident_numbers>-data_key-identificationnumber = input-mt_zfii017m_bp_in-bp_maintain-general_data-vat_registration_no.
    <fs_ident_numbers>-data_key-identificationcategory = 'VATRU'.
    ls_cvis_ei_extern-partner-central_data-ident_number-current_state = 'X'.
    <fs_ident_numbers>-task = input-mt_zfii017m_bp_in-bp_maintain-object_task.
    IF <fs_ident_numbers>-task = 'U'.
      <fs_ident_numbers>-task = 'M'.
    ENDIF.
    UNASSIGN <fs_ident_numbers>.
  ENDIF.

  IF NOT ( input-mt_zfii017m_bp_in-bp_maintain-general_data-identification_category IS INITIAL AND input-mt_zfii017m_bp_in-bp_maintain-general_data-identification_number IS INITIAL ).
    APPEND INITIAL LINE TO ls_cvis_ei_extern-partner-central_data-ident_number-ident_numbers ASSIGNING <fs_ident_numbers>.
    <fs_ident_numbers>-data_key-identificationcategory = input-mt_zfii017m_bp_in-bp_maintain-general_data-identification_category.
    <fs_ident_numbers>-data_key-identificationnumber = input-mt_zfii017m_bp_in-bp_maintain-general_data-identification_number.
    ls_cvis_ei_extern-partner-central_data-ident_number-current_state = 'X'.
    <fs_ident_numbers>-task = input-mt_zfii017m_bp_in-bp_maintain-object_task.
    IF <fs_ident_numbers>-task = 'U'.
      <fs_ident_numbers>-task = 'M'.
    ENDIF.
    UNASSIGN <fs_ident_numbers>.
  ENDIF.

  "ADDRESS

  IF input-mt_zfii017m_bp_in-bp_maintain-address IS NOT INITIAL.
    APPEND INITIAL LINE TO ls_cvis_ei_extern-partner-central_data-address-addresses ASSIGNING FIELD-SYMBOL(<fs_address>).
    APPEND INITIAL LINE TO <fs_address>-data-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<fs_stmp>).
    <fs_stmp>-contact-data-e_mail = input-mt_zfii017m_bp_in-bp_maintain-address-e_mail.
    APPEND INITIAL LINE TO <fs_address>-data-communication-phone-phone ASSIGNING FIELD-SYMBOL(<fs_phone>).
    <fs_phone>-contact-data-telephone = input-mt_zfii017m_bp_in-bp_maintain-address-telephone.
    <fs_phone>-contact-data-r_3_user = 3.
    <fs_phone>-contact-data-consnumber = 1.
    <fs_address>-data-postal-data-country = input-mt_zfii017m_bp_in-bp_maintain-address-country_code.
    <fs_address>-data-postal-data-city_no = input-mt_zfii017m_bp_in-bp_maintain-address-city_code.
    <fs_address>-data-postal-data-distrct_no = input-mt_zfii017m_bp_in-bp_maintain-address-district_code.
    <fs_address>-data-postal-data-regiogroup = input-mt_zfii017m_bp_in-bp_maintain-address-ward_code.
    <fs_address>-data-postal-data-street_no = input-mt_zfii017m_bp_in-bp_maintain-address-street_code.
    <fs_address>-data-postal-data-street = input-mt_zfii017m_bp_in-bp_maintain-address-street_name.
    <fs_address>-data-postal-data-floor = 1.
    IF input-mt_zfii017m_bp_in-bp_maintain-address-house_no IS NOT INITIAL.
      lw_fullstreet = input-mt_zfii017m_bp_in-bp_maintain-address-house_no.
      "cắt tròn chữ cho street_4, street_5, city2, city1 độ dài 40
      CALL FUNCTION 'RKD_WORD_WRAP'
        EXPORTING
          textline            = lw_fullstreet
          outputlen           = 40
        TABLES
          out_lines           = lt_out_line
        EXCEPTIONS
          outputlen_too_large = 1
          OTHERS              = 2.

      IF sy-subrc = 0.
        LOOP AT lt_out_line INTO ls_out_line.
          CASE sy-tabix.
            WHEN 2.
              lw_street1 = ls_out_line.
            WHEN 3.
              lw_street2 = ls_out_line.
            WHEN 4.
              lw_city1 = ls_out_line.
          ENDCASE.
        ENDLOOP.
        REFRESH: lt_out_line.
      ENDIF.
      <fs_address>-data-postal-data-str_suppl1 = lw_street1.
      <fs_address>-data-postal-data-str_suppl2 = lw_street2.
      <fs_address>-data-postal-data-str_suppl3 = lw_city1.
    ENDIF.
  ENDIF.

  "BANKDETAILS

  LOOP AT input-mt_zfii017m_bp_in-bp_maintain-bankdetails INTO DATA(ls_bankdetail).
    APPEND INITIAL LINE TO ls_cvis_ei_extern-partner-central_data-bankdetail-bankdetails ASSIGNING FIELD-SYMBOL(<fs_bankdetails>).
    <fs_bankdetails>-task = ls_bankdetail-task.
    IF <fs_bankdetails>-task = 'M' AND input-mt_zfii017m_bp_in-bp_maintain-object_task = 'U'.
      <fs_bankdetails>-datax-bank_key = 'X'.
      <fs_bankdetails>-datax-bank_ctry = 'X'.
      <fs_bankdetails>-datax-bank_acct = 'X'.
      <fs_bankdetails>-datax-accountholder = 'X'.
      <fs_bankdetails>-datax-bankaccountname = 'X'.
      <fs_bankdetails>-datax-bankdetailvalidfrom = 'X'.
      <fs_bankdetails>-datax-bankdetailvalidto = 'X'.
    ENDIF.
    MOVE-CORRESPONDING ls_bankdetail TO <fs_bankdetails>-data.
  ENDLOOP.

  "CUSTOMER

  IF input-mt_zfii017m_bp_in-bp_maintain-customer_data-task IS NOT INITIAL .
    APPEND INITIAL LINE TO ls_cvis_ei_extern-partner-central_data-role-roles ASSIGNING FIELD-SYMBOL(<fs_role>).
    IF input-mt_zfii017m_bp_in-bp_maintain-object_task = 'I' AND input-mt_zfii017m_bp_in-bp_maintain-customer_data-task = 'I'.
      <fs_role>-task = 'I'.
    ELSEIF input-mt_zfii017m_bp_in-bp_maintain-object_task = 'U' AND input-mt_zfii017m_bp_in-bp_maintain-customer_data-task = 'U'.
      <fs_role>-task = 'U'.
    ENDIF.
    IF <fs_role>-task = 'U'.
      <fs_role>-datax-valid_to = 'X'.
    ENDIF.
    <fs_role>-data_key = 'FLCU01'.
    <fs_role>-data-rolecategory = 'FLCU01'.
    <fs_role>-data-valid_to = input-mt_zfii017m_bp_in-bp_maintain-customer_data-valid_to.
    IF <fs_role>-data-valid_to IS INITIAL.
      <fs_role>-data-valid_to = date_default.
    ENDIF.

    APPEND INITIAL LINE TO ls_cvis_ei_extern-customer-company_data-company ASSIGNING FIELD-SYMBOL(<fs_company_cus>).
    IF input-mt_zfii017m_bp_in-bp_maintain-object_task = 'I' AND  input-mt_zfii017m_bp_in-bp_maintain-customer_data-task = 'I'.
      ls_cvis_ei_extern-customer-header-object_task = 'I'.
    ELSEIF input-mt_zfii017m_bp_in-bp_maintain-object_task = 'U'.
      ls_cvis_ei_extern-customer-header-object_task = input-mt_zfii017m_bp_in-bp_maintain-customer_data-task.
    ENDIF.

    <fs_company_cus>-task = ls_cvis_ei_extern-customer-header-object_task.
    IF <fs_company_cus>-task = 'U'.
      <fs_company_cus>-datax-zterm = 'X'.
      <fs_company_cus>-datax-zwels = 'X'.
    ENDIF.

    <fs_company_cus>-data_key-bukrs = '1000'.
    <fs_company_cus>-data-akont = input-mt_zfii017m_bp_in-bp_maintain-customer_data-reconciliation_account.
    IF <fs_company_cus>-data-akont IS INITIAL.
      <fs_company_cus>-data-akont = '1311101000'.
    ENDIF.

    <fs_company_cus>-data-zterm = input-mt_zfii017m_bp_in-bp_maintain-customer_data-payment_term.
    <fs_company_cus>-data-zwels = input-mt_zfii017m_bp_in-bp_maintain-customer_data-payment_methods.
    ls_cvis_ei_extern-customer-central_data-central-data-kukla = input-mt_zfii017m_bp_in-bp_maintain-customer_data-customer_classification.
    IF input-mt_zfii017m_bp_in-bp_maintain-customer_data-ecommerce_pfm_flag = 'X'.
      ls_cvis_ei_extern-customer-central_data-central-data-konzs = 'SAN'.
    ENDIF.
  ENDIF.

  IF input-mt_zfii017m_bp_in-bp_maintain-customer_data-ecommerce_bp IS NOT INITIAL.
    APPEND INITIAL LINE TO ls_cvis_ei_extern-partner_relation ASSIGNING FIELD-SYMBOL(<fs_partner_relation>).
    IF input-mt_zfii017m_bp_in-bp_maintain-customer_data-task = 'I' AND input-mt_zfii017m_bp_in-bp_maintain-object_task = 'I'.
      <fs_partner_relation>-header-object_task = 'I'.
    ELSEIF input-mt_zfii017m_bp_in-bp_maintain-object_task = 'U'.
      <fs_partner_relation>-header-object_task = input-mt_zfii017m_bp_in-bp_maintain-customer_data-task.
    ENDIF.
    <fs_partner_relation>-central_data-main-task = 'I'.
    <fs_partner_relation>-header-object_instance-partner1-bpartner = input-mt_zfii017m_bp_in-bp_maintain-customer_data-ecommerce_bp.
    <fs_partner_relation>-header-object_instance-partner2-bpartnerguid = ls_cvis_ei_extern-partner-header-object_instance-bpartnerguid.
    <fs_partner_relation>-header-object_instance-relat_category = 'ZBP3'.
  ENDIF.

  IF input-mt_zfii017m_bp_in-bp_maintain-customer_data-all_sorg_flag = 'X'.
    ls_cvis_ei_extern-customer-central_data-central-data-katr1 = 'X'.
  ENDIF.

  LOOP AT input-mt_zfii017m_bp_in-bp_maintain-customer_data-extend_sales_org INTO DATA(ls_extend_sales_org).
    APPEND INITIAL LINE TO ls_cvis_ei_extern-customer-sales_data-sales ASSIGNING FIELD-SYMBOL(<fs_sale>).
    <fs_sale>-data_key-vkorg = ls_extend_sales_org-sales_org.
    <fs_sale>-data_key-vtweg = '01'.
    <fs_sale>-data_key-spart = '01'.
  ENDLOOP.

  "RECONCILIATION
  UNASSIGN <fs_partner_relation>.
  LOOP AT input-mt_zfii017m_bp_in-bp_maintain-customer_data-po_reconciliation INTO DATA(ls_po_reconciliation).
    APPEND INITIAL LINE TO ls_cvis_ei_extern-partner_relation ASSIGNING <fs_partner_relation>.
    IF input-mt_zfii017m_bp_in-bp_maintain-object_task = 'I' AND ls_po_reconciliation-task = 'I'.
      <fs_partner_relation>-header-object_task = 'I'.
    ELSEIF input-mt_zfii017m_bp_in-bp_maintain-object_task = 'U'.
      <fs_partner_relation>-header-object_task = ls_po_reconciliation-task.
    ENDIF.
    IF <fs_partner_relation>-header-object_task = 'M'.
      <fs_partner_relation>-central_data-main-datax-date_from = 'X'.
    ENDIF.
    <fs_partner_relation>-header-object_instance-partner1-bpartner = ls_po_reconciliation-post_office.
    <fs_partner_relation>-header-object_instance-partner2-bpartnerguid = ls_cvis_ei_extern-partner-header-object_instance-bpartnerguid .
    <fs_partner_relation>-header-object_instance-relat_category = 'ZBP1'.
    <fs_partner_relation>-central_data-main-data-date_from = ls_po_reconciliation-valid_from.
    <fs_partner_relation>-header-object_instance-date_to = ls_po_reconciliation-valid_to.
  ENDLOOP.
  "COD
  UNASSIGN <fs_partner_relation>.
  LOOP AT input-mt_zfii017m_bp_in-bp_maintain-customer_data-po_cod INTO DATA(ls_po_cod).
    APPEND INITIAL LINE TO ls_cvis_ei_extern-partner_relation ASSIGNING <fs_partner_relation>.

    IF input-mt_zfii017m_bp_in-bp_maintain-object_task = 'I' AND ls_po_cod-task = 'I'.
      <fs_partner_relation>-header-object_task = 'I'.
    ELSEIF input-mt_zfii017m_bp_in-bp_maintain-object_task = 'U'.
      <fs_partner_relation>-header-object_task = ls_po_cod-task .
    ENDIF.
    IF <fs_partner_relation>-header-object_task = 'M'.
      <fs_partner_relation>-central_data-main-datax-date_from = 'X'.
    ENDIF.
    <fs_partner_relation>-central_data-main-task = <fs_partner_relation>-header-object_task.
    <fs_partner_relation>-header-object_instance-partner1-bpartner = ls_po_cod-post_office.
    <fs_partner_relation>-header-object_instance-partner2-bpartnerguid = ls_cvis_ei_extern-partner-header-object_instance-bpartnerguid .
    <fs_partner_relation>-header-object_instance-relat_category = 'ZBP2'.
    <fs_partner_relation>-central_data-main-data-date_from = ls_po_cod-valid_from.
    <fs_partner_relation>-header-object_instance-date_to = ls_po_cod-valid_to.
  ENDLOOP.

  "VENDOR DATA

  UNASSIGN <fs_role>.
  IF input-mt_zfii017m_bp_in-bp_maintain-vendor_data-task IS NOT INITIAL .
    APPEND INITIAL LINE TO ls_cvis_ei_extern-partner-central_data-role-roles ASSIGNING <fs_role>.
    IF input-mt_zfii017m_bp_in-bp_maintain-object_task = 'I' AND input-mt_zfii017m_bp_in-bp_maintain-vendor_data-task = 'I'.
      <fs_role>-task = 'I'.
    ELSEIF input-mt_zfii017m_bp_in-bp_maintain-object_task = 'U' AND input-mt_zfii017m_bp_in-bp_maintain-vendor_data-task = 'U'.
      <fs_role>-task = 'U'.
    ENDIF.
    IF <fs_role>-task = 'U'.
      <fs_role>-datax-valid_to = 'X'.
    ENDIF.
    <fs_role>-data_key = 'FLVN01'.
    <fs_role>-data-rolecategory = 'FLVN01'.
    <fs_role>-data-valid_to = input-mt_zfii017m_bp_in-bp_maintain-vendor_data-valid_to.
    IF <fs_role>-data-valid_to IS INITIAL.
      <fs_role>-data-valid_to = date_default.
    ENDIF.
    APPEND INITIAL LINE TO ls_cvis_ei_extern-vendor-company_data-company ASSIGNING FIELD-SYMBOL(<fs_company_ven>).
    IF input-mt_zfii017m_bp_in-bp_maintain-object_task = 'I' AND input-mt_zfii017m_bp_in-bp_maintain-vendor_data-task = 'I'.
      ls_cvis_ei_extern-vendor-header-object_task = 'I' .
    ELSEIF input-mt_zfii017m_bp_in-bp_maintain-object_task = 'U'.
      ls_cvis_ei_extern-vendor-header-object_task = input-mt_zfii017m_bp_in-bp_maintain-vendor_data-task.
    ENDIF.
    <fs_company_ven>-task = ls_cvis_ei_extern-vendor-header-object_task.
    IF <fs_company_ven>-task = 'U'.
      <fs_company_ven>-datax-zterm = 'X'.
      <fs_company_ven>-datax-zwels = 'X'.
    ENDIF.
    <fs_company_ven>-data_key-bukrs = '1000'.
    <fs_company_ven>-data-zterm = input-mt_zfii017m_bp_in-bp_maintain-vendor_data-payment_term.
    <fs_company_ven>-data-zwels = input-mt_zfii017m_bp_in-bp_maintain-vendor_data-payment_methods.
  ENDIF.

  APPEND ls_cvis_ei_extern TO lt_cvis_ei_extern_t.

  CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
    EXPORTING
      i_data   = lt_cvis_ei_extern_t
    IMPORTING
      e_return = lt_return.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
  IF sy-subrc IS INITIAL.
    LOOP AT ls_return-object_msg INTO DATA(ls_object_msg).
      APPEND INITIAL LINE TO output-mt_zfii017m_bp_out-message_detail ASSIGNING FIELD-SYMBOL(<fs_messages>).
      MOVE-CORRESPONDING ls_object_msg TO <fs_messages>.
      IF ls_object_msg-type = 'E' OR ls_object_msg-type = 'A'.
        output-mt_zfii017m_bp_out-ev_error = '00'.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF output-mt_zfii017m_bp_out-ev_error = '00'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ENDIF.
  SELECT SINGLE partner FROM but000 INTO @DATA(lv_partner)
         WHERE  partner_guid = @ls_cvis_ei_extern-partner-header-object_instance-bpartnerguid.

  IF sy-subrc = 0.
    output-mt_zfii017m_bp_out-bpartner = lv_partner.
    output-mt_zfii017m_bp_out-ev_error = '01'.
    "ATTACHMENT
    IF input-mt_zfii017m_bp_in-bp_maintain-attachments IS NOT INITIAL.
      LOOP AT input-mt_zfii017m_bp_in-bp_maintain-attachments INTO DATA(ls_attachment).
        ls_object_key = lv_partner.
        lv_url = ls_attachment-url.
        IF ls_attachment-task = 'I'.
          CALL FUNCTION 'ZFM_UPDATE_URL_ALL'
            EXPORTING
              im_object_key  = ls_object_key
              url            = lv_url
              "i_urldes       = lv_urldes
              im_object_type = 'BUS1006'
              mode           = 'I'.

        ELSEIF ls_attachment-task = 'U' AND input-mt_zfii017m_bp_in-bp_maintain-object_task = 'U'.
          "lv_urldes = ls_attachment-obj_descr.
          ls_folder_key-objtp = ls_attachment-attachment_key+0(3).
          ls_folder_key-objyr = ls_attachment-attachment_key+3(2).
          ls_folder_key-objno = ls_attachment-attachment_key+5(12).
          ls_url_key-objtp = ls_attachment-attachment_key+17(3).
          ls_url_key-objyr = ls_attachment-attachment_key+20(2).
          ls_url_key-objno = ls_attachment-attachment_key+22(12).
          CALL FUNCTION 'ZFM_UPDATE_URL_ALL'
            EXPORTING
              im_object_key  = ls_object_key
              url            = lv_url
              "i_urldes       = lv_urldes
              im_object_type = 'BUS1006'
              im_url_key     = ls_url_key
              im_folder_key  = ls_folder_key
              mode           = 'U'.

        ELSEIF ls_attachment-task = 'D' AND input-mt_zfii017m_bp_in-bp_maintain-object_task = 'U'.
          ls_folder_key-objtp = ls_attachment-attachment_key+0(3).
          ls_folder_key-objyr = ls_attachment-attachment_key+3(2).
          ls_folder_key-objno = ls_attachment-attachment_key+5(12).
          ls_url_key-objtp = ls_attachment-attachment_key+17(3).
          ls_url_key-objyr = ls_attachment-attachment_key+20(2).
          ls_url_key-objno = ls_attachment-attachment_key+22(12).
          CALL FUNCTION 'ZFM_UPDATE_URL_ALL'
            EXPORTING
              im_object_key  = ls_object_key
              im_folder_key  = ls_folder_key
              im_object_type = 'BUS1006'
              im_url_key     = ls_url_key
              mode           = 'D'.

        ENDIF.
      ENDLOOP.
    ENDIF.
    "RECOCILIATION_CAL
    IF input-mt_zfii017m_bp_in-bp_maintain-customer_data-reconciliation_cal-ky_ds IS NOT INITIAL.
      SELECT SINGLE * FROM ztb_paymt_period INTO CORRESPONDING FIELDS OF ls_payment_info WHERE
        partner = lv_partner.

      IF sy-subrc <> 0.
        ls_payment_info-partner = lv_partner.
        ls_payment_info-ky_ds = input-mt_zfii017m_bp_in-bp_maintain-customer_data-reconciliation_cal-ky_ds.
      ENDIF.

      IF ls_payment_info-ky_ds = '01' AND input-mt_zfii017m_bp_in-bp_maintain-customer_data-reconciliation_cal-ky_ds = '02'. "week
        lv_count = 1.
        ls_payment_info-ky_ds = input-mt_zfii017m_bp_in-bp_maintain-customer_data-reconciliation_cal-ky_ds.
        DO 31 TIMES.
          lv_column = |{ 'DAY_' }{ lv_count }|.
          ASSIGN COMPONENT lv_column OF STRUCTURE ls_payment_info TO FIELD-SYMBOL(<fs_day>).
          <fs_day> = ' '.
          lv_count = lv_count + 1.
        ENDDO.
      ELSEIF ls_payment_info-ky_ds = '02' AND input-mt_zfii017m_bp_in-bp_maintain-customer_data-reconciliation_cal-ky_ds = '01'. "month
        ls_payment_info-ky_ds = input-mt_zfii017m_bp_in-bp_maintain-customer_data-reconciliation_cal-ky_ds.
        lv_count = 32.
        DO 7 TIMES.
          lv_column = |{ 'DAY_' }{ lv_count }|.
          ASSIGN COMPONENT lv_column OF STRUCTURE ls_payment_info TO <fs_day>.
          <fs_day> = ' '.
          lv_count = lv_count + 1.
        ENDDO.
      ENDIF.

      LOOP AT input-mt_zfii017m_bp_in-bp_maintain-customer_data-reconciliation_cal-days INTO DATA(ls_days).
        lv_column = |{ 'DAY_' }{ ls_days-day }|.
        ASSIGN COMPONENT lv_column OF STRUCTURE ls_payment_info TO <fs_day>.
        <fs_day> = ls_days-flag.
      ENDLOOP.
      CALL FUNCTION 'ENQUEUE_EZ_ZPAYMT_PERIOD'
        EXPORTING
          mode_ztb_paymt_period = 'E'
          mandt                 = sy-mandt
          partner               = ls_payment_info-partner.

      IF sy-subrc <> 0.
        output-mt_zfii017m_bp_out-ev_error = '00'.
      ELSE.
        MODIFY ztb_paymt_period FROM ls_payment_info.
        COMMIT WORK.

        CALL FUNCTION 'DEQUEUE_EZ_ZPAYMT_PERIOD'
          EXPORTING
            mode_ztb_paymt_period = 'E'
            mandt                 = sy-mandt
            partner               = ls_payment_info-partner.

      ENDIF.

    ENDIF.
  ELSE.
    output-mt_zfii017m_bp_out-ev_error = '00'.
  ENDIF.
ENDMETHOD.
