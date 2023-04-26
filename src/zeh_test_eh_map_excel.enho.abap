CLASS lcl_zeh_test_eh_map_excel DEFINITION DEFERRED.
CLASS ycl_acac_s4_xlsx_uploading DEFINITION LOCAL FRIENDS lcl_zeh_test_eh_map_excel.
CLASS lcl_zeh_test_eh_map_excel DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA obj TYPE REF TO lcl_zeh_test_eh_map_excel.   "#EC NEEDED
    DATA core_object TYPE REF TO ycl_acac_s4_xlsx_uploading . "#EC NEEDED
 INTERFACES  IOW_ZEH_TEST_EH_MAP_EXCEL.
    METHODS:
      constructor IMPORTING core_object
                              TYPE REF TO ycl_acac_s4_xlsx_uploading OPTIONAL.
ENDCLASS.
CLASS lcl_zeh_test_eh_map_excel IMPLEMENTATION.
  METHOD constructor.
    me->core_object = core_object.
  ENDMETHOD.

  METHOD iow_zeh_test_eh_map_excel~map_excel_to_acac_structure.
*"------------------------------------------------------------------------*
*" Declaration of Overwrite-method, do not insert any comments here please!
*"
*"methods MAP_EXCEL_TO_ACAC_STRUCTURE
*"  importing
*"    !IT_STRUCTURE type ACAC_S4_STRUC_FIELDNAME_PAIR_T
*"    !IV_ROW_INDEX type I
*"  changing
*"    !CT_UPLOAD_CONTENT type ACAC_S4_UPLOAD_CONTENT_T
*"    !CS_EXCEL_DATA type ACAC_S4_INDEX_VALUE_PAIR
*"  raising
*"    resumable(CX_ACAC_XLSX_EXCEPTION) .
*"------------------------------------------------------------------------*

       DATA: lr_structure      TYPE REF TO data,
          ls_excel_fields   LIKE LINE OF core_object->gt_excel_fields,
          lo_xlsx_parser    TYPE REF TO zcl_acac_s4_xlsx_parse_util,
          ls_accrual_obj    TYPE acac_s4_upload_content,
          ld_amount_field   TYPE fieldname,
          ls_assignment     TYPE acesobj_assgmt_ext,
          ls_amounts        TYPE ace_uploaded_accr_amounts_ext,
          ls_obj_header     TYPE acesobj_ext,
          ls_obj_item       TYPE acesobj_item_ext,
          "ls_acac_param     TYPE acac_parameters,
          lt_amount_curr    TYPE core_object->tt_amount_curr,
          ld_is_amount      TYPE flag,
          ld_length         TYPE i,
          ld_ret_code       TYPE i,
          lv_amount_type    TYPE char1,
          ls_external_field TYPE cl_acac_s4_xlsx_template=>ty_s_ext_int_mapping.

    FIELD-SYMBOLS:
      <fs_m_struc>        TYPE any,
      <fs_m_struc_common> TYPE acac_s4_common_excel_fields,
      <fs_m_struc_custom> TYPE any,
      <fs_value>          TYPE any,
      <fs_data>           TYPE any,
      <fs_accrual_obj>    TYPE acac_s4_upload_content,
      <ft_value_line>     TYPE fac_t_excel_data.

    CREATE OBJECT lo_xlsx_parser.

    " Index to value mapping structure
    " Map the value to the common excel structure by index, which get from the gt_excel_fields according to the fieldname
    " Then move corresponding values from the common excel structure to uploading content structure for saving.
    ASSIGN cs_excel_data-value->* TO <ft_value_line>.

    " loop the structure which contains all fields
    " here, the parameter structure is plain, not name <-> content
    " it incluldes two entries. The first is common fields and another is parameter fields, it lloks like:
    " 1	ACAC_S4_COMMON_EXCEL_FIELDS [Internal Table: FieldNames]
    " 2	ACAC_PARAMETERS [Internal Table: FieldNames]
    LOOP AT it_structure ASSIGNING FIELD-SYMBOL(<fs_abs_structure>).
      " initialize the structures, such as ACAC_S4_COMMON_EXEL_FIELDS and ACAC_PARAMETERS
      CREATE DATA lr_structure TYPE (<fs_abs_structure>-name).
      ASSIGN lr_structure->* TO <fs_m_struc>.

      " For each fields, map the value to it
      " the fieldname is the collection of structure field name
      LOOP AT <fs_abs_structure>-fieldnames INTO DATA(ls_fieldname).
        CLEAR: ld_is_amount, ld_amount_field, ls_external_field.

        " <fs_value> will be filled into later
        ASSIGN COMPONENT ls_fieldname-fieldname OF STRUCTURE <fs_m_struc> TO <fs_value>.
        " for external field
        READ TABLE core_object->gt_external_fields INTO ls_external_field WITH KEY int_field_name = ls_fieldname-fieldname.
        IF sy-subrc NE 0.
          " check whether field in structure is also defined in excel
          READ TABLE core_object->gt_excel_fields WITH KEY excel_field = ls_fieldname-fieldname INTO ls_excel_fields.
        ELSE.
          READ TABLE core_object->gt_excel_fields WITH KEY excel_field = ls_external_field-ext_field_name INTO ls_excel_fields.
        ENDIF.
        " if successful then get the value of the field and check data type for the value
        IF sy-subrc EQ 0.
          " fieldname -> the index in excel file ( just contains the fieldname, not value ) -> get the value from <ft_value_line> by index
          READ TABLE <ft_value_line> ASSIGNING FIELD-SYMBOL(<fs_value_line>) WITH KEY index = ls_excel_fields-index.
          IF sy-subrc EQ 0.
            " convert the value by conversion in dataelement
            IF ls_external_field IS NOT INITIAL.
              ASSIGN <fs_value_line>-value->* TO FIELD-SYMBOL(<fs_t_data>).
              CALL FUNCTION ls_external_field-input_conversion
                EXPORTING
                  input          = <fs_t_data>
                IMPORTING
                  output         = <fs_data>
                EXCEPTIONS
                  not_found      = 1
                  unit_not_found = 2.
              " for WBS Element field
              IF sy-subrc EQ 1.
                RAISE EXCEPTION TYPE cx_acac_xlsx_exception
                  MESSAGE ID 'ACAC_EXCEL'
                  TYPE 'E'
                  NUMBER '032'
                  WITH <fs_t_data> iv_row_index ls_external_field-ext_field_name.
              ENDIF.
              " for Unit of measure field
              IF sy-subrc EQ 2.
                RAISE EXCEPTION TYPE cx_acac_xlsx_exception
                  MESSAGE ID 'ACAC_EXCEL'
                  TYPE 'E'
                  NUMBER '034'
                  WITH <fs_t_data> iv_row_index ls_external_field-ext_field_name.
              ENDIF.
            ELSE.
              " get the original data from excel file. It will be checked before assign to <fs_value>
              ASSIGN <fs_value_line>-value->* TO <fs_data>.
            ENDIF.

            " deal with case sensitive fields
            READ TABLE core_object->gt_fields_case_sensitive WITH KEY table_line = ls_fieldname-fieldname TRANSPORTING NO FIELDS.
            IF sy-subrc NE 0.
              TRANSLATE <fs_data> TO UPPER CASE.
            ENDIF.

            " deal with amount/currency fields
            IF <fs_abs_structure>-name EQ core_object->gc_common_excel_fields.

              " put the relevent amount values into internal table
              " then currency conversion will convert the amount into the correct decimal place
              "       amount_name  TYPE fieldname,
              "       curr_name    TYPE fieldname,
              "       amount_value TYPE string,
              "       curr_value   TYPE string,
              "       round_amount TYPE decfloat16,
              "       decimal      TYPE i,
              CALL METHOD core_object->set_amount_curr_value
                EXPORTING
                  iv_fieldname   = ls_fieldname-fieldname
                  iv_value       = <fs_data>
                  iv_row_index   = iv_row_index
                IMPORTING
                  ev_amount      = ld_is_amount
                CHANGING
                  ct_amount_curr = lt_amount_curr.
*              if ls_fieldname-fieldname
*              DELETE FROM DATABASE indx(ac) ID 'ACACTREE01'.
*              IMPORT <fs_data> TO TOTAL_ACCR_AMNT FROM DATABASE indx(zf) ID 'ZFII009_N'.
            ENDIF.

            " check data type consistency for the field, amount value will check later
            IF ld_is_amount NE abap_true.
              " For date, numberic, amount type
              lo_xlsx_parser->validate_field_type(
                EXPORTING
                  iv_field_value = <fs_data>
                  iv_field_name  = ls_fieldname-fieldname
                  it_fieldinfo   = <fs_abs_structure>-fieldnames
                  iv_row_index   = iv_row_index
                IMPORTING
                  ev_length = ld_length ).

              " in case there are some data type check missing and conversion error occurs
              <fs_value> = <fs_data>.
              IF ls_external_field IS INITIAL.
                TRY.
                  core_object->additional_conversion( CHANGING cv_value = <fs_value> ).
                CATCH cx_sy_conversion_error INTO DATA(lv_message).
                  RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
                    MESSAGE ID 'ACAC_EXCEL'
                    TYPE 'E'
                    NUMBER '018'
                    WITH iv_row_index ls_fieldname-fieldname.
              ENDTRY.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      " the lt_structure will be splitted into common and custom ( parameter )
      " for parameter fields, on specifical validation can't be set
      IF <fs_abs_structure>-name EQ core_object->gd_param_structure.
        " parameters
        ASSIGN <fs_m_struc> TO <fs_m_struc_custom>.
      ELSEIF <fs_abs_structure>-name EQ core_object->gc_common_excel_fields.
        ASSIGN <fs_m_struc> TO <fs_m_struc_common>.
        <fs_m_struc_common>-comp = acacc_comp.
      ENDIF.
    ENDLOOP.

    SORT ct_upload_content BY bukrs acac_objtype acac_objnumber.

*    replace Company Code, Ledger and Cost Center in case of an Test
    IF core_object->mb_test_mode = abap_true AND core_object->mv_ledger_leading_test IS NOT INITIAL AND core_object->mv_ledger_non_leading_test IS NOT INITIAL AND core_object->mv_cost_center_test IS NOT INITIAL AND core_object->mv_prctr_test IS NOT
INITIAL.
      CALL METHOD core_object->if_acac_uploading_test~replace_bukrs_ledger_kostl_pc
        CHANGING
          cd_m_struc_common = <fs_m_struc_common>. " Accrual Object Common Fields in Excel
    ENDIF.

*    move the key value to accrual object struture
    MOVE-CORRESPONDING <fs_m_struc_common> TO ls_accrual_obj.

    " check mandatory fields are filled or not
    CALL METHOD core_object->check_mandatory_fields(
      EXPORTING
        iv_row_index           = iv_row_index
        is_accrual_obj         = ls_accrual_obj
        is_common_excel_fields = <fs_m_struc_common> ).

    " check field overwrite allowed, this field only can has value 'X' or empty
    CALL METHOD core_object->check_overwrite_allowed(
      EXPORTING
        iv_row_index   = iv_row_index
        is_accrual_obj = ls_accrual_obj ).

    " check accrual object number is filled or not if overwrite allowed is set as abap_true in excel
    CALL METHOD core_object->check_accrual_object_number(
      EXPORTING
        iv_row_index   = iv_row_index
        is_accrual_obj = ls_accrual_obj ).

    MOVE-CORRESPONDING ls_accrual_obj TO cs_excel_data-upload_content.

*    prepare for header ( ACESOBJ )
    MOVE-CORRESPONDING <fs_m_struc_common> TO ls_obj_header.
    IF ls_obj_header IS NOT INITIAL.
      MOVE-CORRESPONDING ls_obj_header TO cs_excel_data-upload_content-acesobj.
    ENDIF.

*    prepare for line item ( total and period )
    " check currency
    CALL METHOD core_object->check_currency(
      EXPORTING
        iv_row_index   = iv_row_index
        is_accrual_obj = ls_accrual_obj
        it_amount_curr = lt_amount_curr ).

    " check whether one object object has values both on field Amount and Uploaded Accrual Amount
    " if yes, then throw error.
    " we only sopport end user to upload either period accrual upload or normal accrual upload
    CALL METHOD core_object->check_upload_type(
      EXPORTING
        it_amount_curr = lt_amount_curr
        is_accrual_obj = ls_accrual_obj
        iv_row_index   = iv_row_index
      IMPORTING
        ev_amount_type = lv_amount_type ).

    cs_excel_data-upload_content-type = lv_amount_type.

    " check the mandotory fields again,
    " because some fields checking depends on whether type is upload or total
    CALL METHOD core_object->check_mandatory_fields(
      EXPORTING
        iv_row_index           = iv_row_index
        is_accrual_obj         = ls_accrual_obj
        is_common_excel_fields = <fs_m_struc_common>
        iv_upload_type         = lv_amount_type ).

    " convert the G/L Account by Alpha conversion
    CALL METHOD core_object->glaccount_alpha_conversion
      CHANGING
        cs_account = <fs_m_struc_common>.

    TRY.
*       get either the representative ledger or leading ledger by the ledger group
        CALL METHOD cl_ace_cfg_db_buffer=>get_representative_ledger
          EXPORTING
            iv_ldgrp = <fs_m_struc_common>-ldgrp       " Ledger Group
            iv_bukrs = <fs_m_struc_common>-bukrs       " Company Code
          RECEIVING
            rv_rldnr = DATA(lv_rldnr).                 " Ledger in General Ledger Accounting

        CALL METHOD cl_ace_mdo_services=>check_item_key_allowed
          EXPORTING
            id_comp     = <fs_m_struc_common>-comp                 " Accrual Engine Application Component
            id_itemtype = <fs_m_struc_common>-itemtype             " Accrual Item Type
            id_bukrs    = <fs_m_struc_common>-bukrs                " Company Code
            id_rldnr    = lv_rldnr                                 " Ledger in General Ledger Accounting
            id_ldgrp    = <fs_m_struc_common>-ldgrp.               " Ledger Group
      CATCH cx_ace INTO DATA(lx_ace).
      " configuration miss: Ledger doesn't exist
      RAISE EXCEPTION TYPE cx_acac_xlsx_exception
        MESSAGE ID 'ACE_S4'
        TYPE 'E'
        NUMBER '049'
        WITH <fs_m_struc_common>-ldgrp <fs_m_struc_common>-itemtype <fs_m_struc_common>-bukrs.
    ENDTRY.

    " prepare for item and period amount
    MOVE-CORRESPONDING <fs_m_struc_common> TO ls_obj_item.
    ls_obj_item-rldnr = lv_rldnr.

    MOVE-CORRESPONDING <fs_m_struc_common> TO ls_amounts.
    ls_amounts-rldnr = lv_rldnr.

    CALL METHOD core_object->map_amount_currency_fields(
      EXPORTING
        iv_amount_type = lv_amount_type
        it_amount_curr = lt_amount_curr
        is_accrual_obj = ls_accrual_obj
        iv_row_index   = iv_row_index
      CHANGING
        cs_obj_item    = ls_obj_item
        cs_period_item = ls_amounts
        cs_excel_data  = cs_excel_data ).

    " prepare for currency just for displaying the currency in upload result list screen
    MOVE-CORRESPONDING <fs_m_struc_common> TO cs_excel_data-upload_content-aces_currkeys.

*    prepare for assignment
    MOVE-CORRESPONDING <fs_m_struc_common> TO ls_assignment.
    IF ls_assignment IS NOT INITIAL.
      CALL METHOD core_object->assignment_alpha_conversion
        CHANGING
          cs_assignment = ls_assignment.
      MOVE-CORRESPONDING ls_assignment TO cs_excel_data-upload_content-acesobj_assgmt.
    ENDIF.

*    prepare for parameters
    "MOVE <fs_m_struc_custom> TO ls_acac_param.
    IF <fs_m_struc> IS ASSIGNED.
      CALL METHOD core_object->map_parameter_assignment(
        EXPORTING
          id_acac_param  = <fs_m_struc_custom>
        is_accrual_obj = ls_accrual_obj
      CHANGING
        cs_excel_data  = cs_excel_data ).
    ENDIF.

    " collect the temporary object number
    " prefix $$ is temporary object number
    IF <fs_m_struc_common>-acac_objnumber IS NOT INITIAL
      AND strlen( <fs_m_struc_common>-acac_objnumber ) > 2
      AND <fs_m_struc_common>-acac_objnumber+0(2) = core_object->gc_temp_objnum_prefix.
      " don't insert duplicated object number
      IF NOT line_exists( core_object->gt_temp_created_objnums[ temp_num = <fs_m_struc_common>-acac_objnumber ] ).
        DESCRIBE TABLE core_object->gt_temp_created_objnums LINES DATA(lv_temp_objnum_count).
        APPEND VALUE #( index = lv_temp_objnum_count + 1 temp_num = <fs_m_struc_common>-acac_objnumber ) TO core_object->gt_temp_created_objnums.
      ENDIF.
    ENDIF.

    " check the consistency and modify the upload content
    CALL METHOD core_object->return_consistent_upld_content(
      EXPORTING
        is_accrual_obj    = ls_accrual_obj
      CHANGING
        ct_upload_content = ct_upload_content
        cs_excel_data     = cs_excel_data ).
  ENDMETHOD.
ENDCLASS.
