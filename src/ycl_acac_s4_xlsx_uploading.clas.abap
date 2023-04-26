class YCL_ACAC_S4_XLSX_UPLOADING definition
  public
  create public .

public section.

  interfaces IF_ACAC_UPLOADING .
  interfaces IF_ACAC_UPLOADING_TEST .

  aliases UPLOAD_ACAC_EXCEL
    for IF_ACAC_UPLOADING~UPLOAD_ACAC_EXCEL .
  PROTECTED SECTION.
private section.

  aliases SET_KEYDATE_FOR_TEST
    for IF_ACAC_UPLOADING_TEST~SET_KEYDATE_FOR_TEST .
  aliases SET_LIFECYCLEDATE_FOR_TEST
    for IF_ACAC_UPLOADING_TEST~SET_LIFECYCLEDATE_FOR_TEST .
  aliases SET_OBJECT_NUMBERS_FOR_TEST
    for IF_ACAC_UPLOADING_TEST~SET_OBJECT_NUMBERS_FOR_TEST .
  aliases TT_INDEX_KEYDATE
    for IF_ACAC_UPLOADING_TEST~TT_INDEX_KEYDATE .
  aliases TT_INDEX_LIFE_CYCLE_DATE
    for IF_ACAC_UPLOADING_TEST~TT_INDEX_LIFE_CYCLE_DATE .
  aliases TT_INDEX_OBJECT_NUMBER
    for IF_ACAC_UPLOADING_TEST~TT_INDEX_OBJECT_NUMBER .
  aliases TY_INDEX_KEYDATE
    for IF_ACAC_UPLOADING_TEST~TY_INDEX_KEYDATE .
  aliases TY_INDEX_LIFE_CYCLE_DATE
    for IF_ACAC_UPLOADING_TEST~TY_INDEX_LIFE_CYCLE_DATE .
  aliases TY_INDEX_OBJECT_NUMBER
    for IF_ACAC_UPLOADING_TEST~TY_INDEX_OBJECT_NUMBER .

  types:
    t_tcurx TYPE STANDARD TABLE OF tcurx .
  types:
    t_tcurc TYPE STANDARD TABLE OF tcurc .
  types:
    t_tacac_po_assign TYPE STANDARD TABLE OF tacac_po_assign .
  types:
    t_fieldname TYPE STANDARD TABLE OF fieldname .
  types:
    BEGIN OF ty_amount_curr_map,
      amount_name TYPE fieldname,
      type        TYPE string,
      curr_name   TYPE fieldname,
    END OF ty_amount_curr_map .
  types:
    BEGIN OF ty_excel_field,
      index       TYPE char100,
      excel_field TYPE string,
      is_param    TYPE flag,
    END OF ty_excel_field .
  types:
    tt_excel_field TYPE TABLE OF ty_excel_field .
  types:
    tt_amount_curr_map  TYPE TABLE OF ty_amount_curr_map .
  types:
    BEGIN OF ty_amount_curr,
      amount_name  TYPE fieldname,
      curr_name    TYPE fieldname,
      amount_value TYPE string,
      round_amount TYPE decfloat16,
      curr_value   TYPE string,
      decimal      TYPE i,
      type         TYPE char1,
    END OF ty_amount_curr .
  types:
    tt_amount_curr TYPE TABLE OF ty_amount_curr .
  types:
    BEGIN OF ty_bukrs,
      bukrs TYPE bukrs,
    END OF ty_bukrs .
  types:
    tt_bukrs TYPE TABLE OF ty_bukrs .
  types:
    BEGIN OF ty_temp_created_objnum_map,
      index       TYPE i,
      temp_num    TYPE string,
      created_num TYPE string,
    END OF ty_temp_created_objnum_map .
  types:
    tt_temp_created_objnum_map TYPE TABLE OF ty_temp_created_objnum_map .

  data GT_AMOUNT_CURR_MAP type TT_AMOUNT_CURR_MAP .
  data GT_CURRENCY_CONFIGURATION type T_TCURX .
  data GT_PARAM_ASSIGNMENT type T_TACAC_PO_ASSIGN .
  data GT_PARAM_FIELDS type DD_X031L_TABLE .
  data GD_PARAM_STRUCTURE type TABNAME .
  data GT_FIELDS_CASE_SENSITIVE type T_FIELDNAME .
  data GT_EXCEL_FIELDS type TT_EXCEL_FIELD .
  data GC_CONSTANT_ACAC type ACE_COMP value 'ACAC' ##NO_TEXT.
  data GC_COMMON_EXCEL_FIELDS type TABNAME value 'ACAC_S4_COMMON_EXCEL_FIELDS' ##NO_TEXT.
  data GC_TEMP_OBJNUM_PREFIX type TABNAME value '$$' ##NO_TEXT.
  data GT_PROCESSED_COMPANY_CODE type TT_BUKRS .
  data GT_CURRENCY_TOTAL type T_TCURC .
  data GT_EXTERNAL_FIELDS type CL_ACAC_S4_XLSX_TEMPLATE=>TY_T_EXT_INT_MAPPING .
  data GT_TEMP_CREATED_OBJNUMS type TT_TEMP_CREATED_OBJNUM_MAP .
  data GC_DATE_INITIAL_VALUE type STRING .
  data GT_OBJECT_NUMBERS_TEST type TT_INDEX_OBJECT_NUMBER .
  data GT_KEYDATES_TEST type TT_INDEX_KEYDATE .
  data GT_LIFECYCLEDATES_TEST type TT_INDEX_LIFE_CYCLE_DATE .
  data MB_TEST_MODE type ABAP_BOOL .
  data MV_BUKRS_TEST type BUKRS .
  data MV_LEDGER_LEADING_TEST type RLDNR .
  data MV_LEDGER_NON_LEADING_TEST type RLDNR .
  data MV_COST_CENTER_TEST type KOSTL .
  data MV_PRCTR_TEST type PRCTR .
  data MV_HKONT_OFFESTTING_TEST type HKONT .
  data MV_HKONT_ACCRUALS_TEST type HKONT .

  methods MAP_EXCEL_TO_ACAC_STRUCTURE
    importing
      !IT_STRUCTURE type ACAC_S4_STRUC_FIELDNAME_PAIR_T
      !IV_ROW_INDEX type I
    changing
      !CT_UPLOAD_CONTENT type ACAC_S4_UPLOAD_CONTENT_T
      !CS_EXCEL_DATA type ACAC_S4_INDEX_VALUE_PAIR
    raising
      resumable(CX_ACAC_XLSX_EXCEPTION) .
  methods PARSE_XLSX_MAP_FIELDS
    importing
      !IV_FILE_NAME type STRING
      !IV_XFILE type XSTRING
    changing
      !CT_UPLOAD_CONTENT type ACAC_S4_UPLOAD_CONTENT_T
    raising
      resumable(CX_ACAC_XLSX_EXCEPTION) .
  methods MODIFY_ACAC_OBJECT
    exporting
      !EB_ERROR_OCCURRED type FLAG
      !ET_UPLOAD_RESULT type ACAC_S4_UPLOAD_RESULT_T
    changing
      !CT_UPLOAD_CONTENT type ACAC_S4_UPLOAD_CONTENT_T
    raising
      resumable(CX_ACE) .
  methods STORE_MSG_IN_RETURN
    importing
      !SYST type SYST
    changing
      !CT_RETURN type BAPIRET2_T .
  methods INITIAL_GLOBAL_VARIANT .
  methods INITIALIZE_ACC_OBJ
    importing
      !IV_COMP type ACE_COMP
      !IV_BUKRS type ACE_BUKRS
      !IV_ACCRUAL_TYPE type ACAC_OBJTYPE
      !IV_OBJECT_NUMBER type ACAC_OBJNUMBER
      !IB_HAVE_OBJECTNUMBER type ABAP_BOOL default ' '
      !IV_OVERWRITE_FLAG type XFLAG
    changing
      !CO_ACE_MDO_SUBOBJ type ref to IF_ACE_MDO_SUBOBJ
    raising
      CX_ACE .
  methods VALIDATE_FIELDS
    importing
      !IS_EXCEL_VALUE_LINE type ACAC_S4_INDEX_VALUE_PAIR
      !IT_STRUCTURE type ACAC_STRUC_FIELDNAME_PAIR_T
    raising
      resumable(CX_ACAC_XLSX_EXCEPTION) .
  methods RETURN_CONSISTENT_UPLD_CONTENT
    importing
      !IS_ACCRUAL_OBJ type ACAC_S4_UPLOAD_CONTENT
    changing
      !CT_UPLOAD_CONTENT type ACAC_S4_UPLOAD_CONTENT_T
      !CS_EXCEL_DATA type ACAC_S4_INDEX_VALUE_PAIR
    raising
      resumable(CX_ACAC_XLSX_EXCEPTION) .
  methods SET_AMOUNT_CURR_MAP .
  methods MAP_AMOUNT_CURRENCY_FIELDS
    importing
      !IV_AMOUNT_TYPE type CHAR1
      !IT_AMOUNT_CURR type TT_AMOUNT_CURR
      !IS_ACCRUAL_OBJ type ACAC_S4_UPLOAD_CONTENT
      !IV_ROW_INDEX type I
    changing
      !CS_OBJ_ITEM type ACESOBJ_ITEM_EXT optional
      !CS_PERIOD_ITEM type ACE_UPLOADED_ACCR_AMOUNTS_EXT optional
      !CS_EXCEL_DATA type ACAC_S4_INDEX_VALUE_PAIR
    raising
      resumable(CX_ACAC_XLSX_EXCEPTION) .
  methods MAP_PARAMETER_ASSIGNMENT
    importing
      !ID_ACAC_PARAM type DATA
      !IS_ACCRUAL_OBJ type ACAC_S4_UPLOAD_CONTENT
    changing
      !CS_EXCEL_DATA type ACAC_S4_INDEX_VALUE_PAIR
    raising
      resumable(CX_ACAC_XLSX_EXCEPTION) .
  methods GET_CURRENCY_CONFIGURATION .
  methods GET_PARAMETER_ASSIGNMENT .
  methods GET_PARAM_FIELDS .
  methods SET_CASE_SENSITIVE_FIELDS .
  methods GET_EXCEL_FIELDS
    importing
      !IS_EXCEL_DATA type ACAC_S4_INDEX_VALUE_PAIR
    changing
      !CV_FIELD_EXIST type FLAG
      !CV_FIELD_NAME type STRING .
  methods CHECK_INVALID_FIELD
    importing
      !IT_STRUCTURE type ACAC_STRUC_FIELDNAME_PAIR_T
    changing
      !CV_INVALID type FLAG
      !CV_FIELD type STRING .
  methods SET_AMOUNT_CURR_VALUE
    importing
      !IV_FIELDNAME type FIELDNAME
      !IV_VALUE type ANY
      !IV_ROW_INDEX type I
    exporting
      !EV_AMOUNT type FLAG
    changing
      !CT_AMOUNT_CURR type TT_AMOUNT_CURR
    raising
      resumable(CX_ACAC_XLSX_EXCEPTION) .
  methods CHECK_IS_ACCR_DATA_CONSISTENCY
    importing
      !IS_ACCRUAL_OBJ type ACAC_S4_UPLOAD_CONTENT
      !IS_OBJ_HEADER type ACESOBJ_EXT
      !IS_ASSIGNMENT type ACESOBJ_ASSGMT_EXT
      !IT_AMOUNTS type ACE_UPLOADED_ACCR_AMNTS_EXT_T
      !IT_OBJ_ITEM type ACESOBJ_ITEM_EXT_T
      !IT_PARAMETERS type ACEDS_OBJECT_PARAMETER_EXT_T
    changing
      !CS_UPLOAD_CONTENT type ACAC_S4_UPLOAD_CONTENT
    raising
      resumable(CX_ACAC_XLSX_EXCEPTION) .
  methods CURRENCY_CONVERSION
    importing
      !IV_AMOUNT type BAPICURR-BAPICURR
      !IV_CURRENCY type ACE_DS_CURRENCY
      !IS_ACCRUAL_OBJ type ACAC_S4_UPLOAD_CONTENT
      !IV_ROW_INDEX type I
    changing
      !CV_SAP_AMOUNT type BAPICURR-BAPICURR
    raising
      resumable(CX_ACAC_XLSX_EXCEPTION) .
  methods GLACCOUNT_ALPHA_CONVERSION
    changing
      !CS_ACCOUNT type ACAC_S4_COMMON_EXCEL_FIELDS .
  methods ASSIGNMENT_ALPHA_CONVERSION
    changing
      !CS_ASSIGNMENT type ACESOBJ_ASSGMT_EXT .
  methods CHECK_ERROR_OCCURRED
    importing
      !IT_RETURN type BAPIRET2_T
    exporting
      !EB_ERROR_OCCURRED type FLAG .
  methods COLLECT_UPLOAD_RESULT
    importing
      !IB_LAST type ABAP_BOOL optional
    exporting
      !EB_ERROR_OCCURRED type ABAP_BOOL
    changing
      !CT_UPLOAD_CONTENT type ACAC_S4_UPLOAD_CONTENT_T
      !CT_UPLOAD_RESULT type ACAC_S4_UPLOAD_RESULT_T .
  methods CHECK_ACCRUAL_OBJECT_NUMBER
    importing
      !IV_ROW_INDEX type I
      !IS_ACCRUAL_OBJ type ACAC_S4_UPLOAD_CONTENT
    raising
      resumable(CX_ACAC_XLSX_EXCEPTION) .
  methods CHECK_MANDATORY_FIELDS
    importing
      !IV_ROW_INDEX type I
      !IS_COMMON_EXCEL_FIELDS type ACAC_S4_COMMON_EXCEL_FIELDS
      !IS_ACCRUAL_OBJ type ACAC_S4_UPLOAD_CONTENT
      !IV_UPLOAD_TYPE type CHAR1 optional
    raising
      resumable(CX_ACAC_XLSX_EXCEPTION) .
  methods CHECK_UPLOAD_TYPE
    importing
      !IS_ACCRUAL_OBJ type ACAC_S4_UPLOAD_CONTENT
      !IT_AMOUNT_CURR type TT_AMOUNT_CURR
      !IV_ROW_INDEX type I
    exporting
      !EV_AMOUNT_TYPE type CHAR1
    raising
      resumable(CX_ACAC_XLSX_EXCEPTION) .
  methods CHECK_CURRENCY
    importing
      !IV_ROW_INDEX type I
      !IS_ACCRUAL_OBJ type ACAC_S4_UPLOAD_CONTENT
      !IT_AMOUNT_CURR type TT_AMOUNT_CURR
    raising
      resumable(CX_ACAC_XLSX_EXCEPTION) .
  methods CHECK_OVERWRITE_ALLOWED
    importing
      !IV_ROW_INDEX type I
      !IS_ACCRUAL_OBJ type ACAC_S4_UPLOAD_CONTENT
    raising
      resumable(CX_ACAC_XLSX_EXCEPTION) .
  methods SET_PROCESSED_COMPANY_CODE
    importing
      !IV_BUKRS type BUKRS .
  methods SET_FINAL_MESSAGE
    changing
      !CT_UPLOAD_CONTENT type ACAC_S4_UPLOAD_CONTENT_T
      !CT_UPLOAD_RESULT type ACAC_S4_UPLOAD_RESULT_T .
  methods CHECK_OVERWRITE_FLAG
    changing
      !CT_UPLOAD_CONTENT type ACAC_S4_UPLOAD_CONTENT_T .
  methods FILL_DEFAULT_VALUE_INTO_ITEM
    importing
      !IO_ACE_SUBOBJ type ref to IF_ACE_MDO_SUBOBJ
      !IV_RLDNR type RLDNR optional
    exporting
      !LB_ITEM_EXISTED type ABAP_BOOL
    changing
      !CS_UPLOAD_CONTENT type ACAC_S4_UPLOAD_CONTENT
    raising
      resumable(CX_ACE) .
  methods PREFETCH_ACCRUAL_OBJECTS .
  methods ADDITIONAL_CONVERSION
    changing
      !CV_VALUE type DATA .
  methods ALIGN_ITEMS_IN_SUBOBJ
    importing
      !IO_ACE_MDO_SUBOBJ type ref to IF_ACE_MDO_SUBOBJ
      !IS_UPLOAD_CONTENT type ACAC_S4_UPLOAD_CONTENT
    exporting
      !ET_ACCRUAL_ITEM type ACESOBJ_ITEM_EXT_T .
ENDCLASS.



CLASS YCL_ACAC_S4_XLSX_UPLOADING IMPLEMENTATION.


  METHOD ADDITIONAL_CONVERSION.

    DATA lo_typedescr   TYPE REF TO cl_abap_typedescr.
    DATA lt_ddic        TYPE dd_x031l_table.
    DATA lv_ddic        TYPE LINE OF dd_x031l_table.
    DATA lv_func_name   TYPE funcname.

    lo_typedescr ?= cl_abap_typedescr=>describe_by_data( cv_value ).
    lt_ddic = lo_typedescr->get_ddic_object( ).
    CLEAR: lv_ddic.
    READ TABLE lt_ddic INDEX 1 INTO lv_ddic.
    IF lv_ddic-convexit IS NOT INITIAL.
      lv_func_name = 'CONVERSION_EXIT_' && lv_ddic-convexit && '_INPUT'.
    ENDIF.

    IF lv_func_name IS NOT INITIAL.
      CALL FUNCTION lv_func_name
        EXPORTING
          input  = cv_value
        IMPORTING
          output = cv_value.
    ENDIF.

  ENDMETHOD.


  METHOD ALIGN_ITEMS_IN_SUBOBJ.

    DATA: ls_acesobj_key    TYPE acesobj_key, lt_acesobj_key TYPE acesobj_key_t,
*          lt_accrual_item   TYPE cl_ace_mdo_db_buffer=>tt_db_ace_accritems,
          ls_accrual_item_f TYPE acesobj_item_ext.

    DATA: lt_item_td_all    TYPE if_ace_mdo_types=>tt_item_td_parameters.
    DATA: lt_accrual_item   LIKE et_accrual_item.

    REFRESH et_accrual_item.


    IF io_ace_mdo_subobj->mt_items IS NOT INITIAL.

      LOOP AT io_ace_mdo_subobj->mt_items REFERENCE INTO DATA(ls_item).

        REFRESH lt_accrual_item.

        ls_item->o_item->get_item_td_valid_on_detdate(
          EXPORTING
            id_detdate = acec_highest_date                 " Date to determine time frame
*            id_ledger  =                  " Ledger in General Ledger Accounting
          IMPORTING
*            es_item_td =
            et_item_td = DATA(lt_item_td)
        ).

        LOOP AT lt_item_td ASSIGNING FIELD-SYMBOL(<ls_item_td>).
          CLEAR ls_accrual_item_f.
          ls_accrual_item_f-itemtype = ls_item->itemtype.
          MOVE-CORRESPONDING <ls_item_td> TO ls_accrual_item_f.
          cl_ace_generic_services=>values_column_to_row(
            EXPORTING
              it_column_values = <ls_item_td>-t_total_values
              iv_bukrs         = io_ace_mdo_subobj->ms_subobj_key-bukrs
              iv_rldnr         = <ls_item_td>-rldnr
              iv_val_prefix    = 'TOTAL_ACCR_AMNT_'
              iv_cur_postfix   = 'TTL'
            CHANGING
              cs_row_values = ls_accrual_item_f
           ).
          APPEND ls_accrual_item_f TO et_accrual_item.
        ENDLOOP.

      ENDLOOP.


      LOOP AT is_upload_content-acesobj_item_t INTO DATA(ls_accrual_item) .
        READ TABLE et_accrual_item ASSIGNING FIELD-SYMBOL(<ls_accrual_item>) WITH KEY itemtype = ls_accrual_item-itemtype rldnr = ls_accrual_item-rldnr.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING ls_accrual_item TO <ls_accrual_item>.
        ELSE.
          APPEND ls_accrual_item TO et_accrual_item.
        ENDIF.
      ENDLOOP.

    ELSE.
      APPEND LINES OF is_upload_content-acesobj_item_t TO et_accrual_item.
    ENDIF.

  ENDMETHOD.


  METHOD ASSIGNMENT_ALPHA_CONVERSION.

    DATA lo_structdescr   TYPE REF TO cl_abap_structdescr.
    DATA lt_component   TYPE cl_abap_structdescr=>component_table.
    DATA lo_typedescr   TYPE REF TO cl_abap_typedescr.
    DATA lt_ddic        TYPE dd_x031l_table.
    DATA lv_ddic        TYPE LINE OF dd_x031l_table.
    DATA lv_func_name   TYPE funcname.

    lo_structdescr ?= cl_abap_structdescr=>describe_by_data( cs_assignment ).
    lt_component = lo_structdescr->get_components( ).
    LOOP AT lt_component INTO DATA(ls_component).
      ASSIGN COMPONENT ls_component-name OF STRUCTURE cs_assignment TO FIELD-SYMBOL(<ls_value>).
      IF <ls_value> IS ASSIGNED.
        lo_typedescr ?= cl_abap_typedescr=>describe_by_data( <ls_value> ).
        lt_ddic = lo_typedescr->get_ddic_object( ).
        CLEAR: lv_ddic.
        READ TABLE lt_ddic INDEX 1 INTO lv_ddic.
        IF lv_ddic-convexit IS NOT INITIAL.
          lv_func_name = 'CONVERSION_EXIT_' && lv_ddic-convexit && '_INPUT'.
        ENDIF.
        IF lv_func_name IS NOT INITIAL.
          CALL FUNCTION lv_func_name
            EXPORTING
              input  = <ls_value>
            IMPORTING
              output = <ls_value>.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD CHECK_ACCRUAL_OBJECT_NUMBER.
    DATA: ls_accrual_msg      TYPE acac_accrual_message.

    IF is_accrual_obj-acac_objnumber IS INITIAL AND
       is_accrual_obj-overwrite_allowed EQ abap_true.

      " Enter the accrual object number if overwrite is allowed.
      RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
        MESSAGE ID 'ACAC_EXCEL'
        TYPE 'E'
        NUMBER '020'
        WITH iv_row_index.
    ENDIF.

  ENDMETHOD.


  METHOD CHECK_CURRENCY.

    LOOP AT it_amount_curr INTO DATA(ls_amount_curr).
      READ TABLE gt_currency_total WITH KEY waers = ls_amount_curr-curr_value TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.

        RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
          MESSAGE ID 'ACAC_EXCEL'
          TYPE 'E'
          NUMBER '023'
          WITH iv_row_index.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD CHECK_ERROR_OCCURRED.
* check if there are some error/abort/dump mesg's in the return table:
    LOOP AT it_return  TRANSPORTING NO FIELDS
    WHERE  type = 'E'
    OR     type = 'A'
    OR     type = 'X'.
*   yes, there are error/abort/dump messages:
      eb_error_occurred = acec_con-true.
      EXIT.
    ENDLOOP.
  ENDMETHOD.


  METHOD CHECK_INVALID_FIELD.
    DATA: ls_com_structure TYPE acac_struc_fieldname_pair,
          ls_cus_structure TYPE acac_struc_fieldname_pair.
    FIELD-SYMBOLS: <fs_excel_field> TYPE ty_excel_field.

    READ TABLE it_structure WITH KEY name = gc_common_excel_fields INTO ls_com_structure.
    READ TABLE it_structure WITH KEY name = gd_param_structure INTO ls_cus_structure.

    LOOP AT gt_excel_fields ASSIGNING <fs_excel_field>.
      " skip this check if it's external field
      IF line_exists( gt_external_fields[ ext_field_name = <fs_excel_field>-excel_field ]  ).
        CONTINUE.
      ENDIF.
      READ TABLE ls_com_structure-fieldname WITH KEY fieldname = <fs_excel_field>-excel_field TRANSPORTING NO FIELDS.
      " continue to check parameter field
      CHECK sy-subrc NE 0.
      READ TABLE ls_cus_structure-fieldname WITH KEY fieldname = <fs_excel_field>-excel_field TRANSPORTING NO FIELDS.
      " mark the invalid field once the field is found in common structure and paramter structure
      IF sy-subrc NE 0.
        IF cv_invalid EQ abap_false.
          cv_invalid = abap_true.
          cv_field = <fs_excel_field>-excel_field.
        ENDIF.
      ELSE.
        " mark this field into parameter field
        <fs_excel_field>-is_param = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD CHECK_IS_ACCR_DATA_CONSISTENCY.
    DATA: ls_message            TYPE acac_accrual_message,
          ls_header_cur         TYPE acesobj_ext,
          ls_header_pre         TYPE acesobj_ext,
          lt_item_pre           TYPE acesobj_item_ext_t,
          lt_amount_pre         TYPE ace_uploaded_accr_amnts_ext_t,
          ls_new_upload_content TYPE acac_s4_upload_content.

    " if there are several entries in excel which has same key fields, then the keydate should be in ascending sorted
    IF cs_upload_content-keydate > is_accrual_obj-keydate.
      RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
        MESSAGE ID 'ACAC_EXCEL'
        TYPE 'E'
        NUMBER '005'
        WITH is_accrual_obj-bukrs is_accrual_obj-acac_objtype is_accrual_obj-acac_objnumber.

    ELSEIF cs_upload_content-keydate = is_accrual_obj-keydate.
      " check data consistency for header
      ls_header_cur = cs_upload_content-acesobj.
      ls_header_pre = is_accrual_obj-acesobj.

      IF ls_header_cur <> ls_header_pre.
        RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
          MESSAGE ID 'ACAC_EXCEL'
          TYPE 'E'
          NUMBER '006'
          WITH is_accrual_obj-bukrs is_accrual_obj-acac_objtype is_accrual_obj-acac_objnumber is_accrual_obj-keydate.

      ENDIF.

      " check data consistency for assignment
      IF cs_upload_content-acesobj_assgmt <> is_assignment.
        RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
          MESSAGE ID 'ACAC_EXCEL'
          TYPE 'E'
          NUMBER '007'
          WITH is_accrual_obj-bukrs is_accrual_obj-acac_objtype is_accrual_obj-acac_objnumber is_accrual_obj-keydate.
      ENDIF.

      " check data consistency for parameters
      IF cs_upload_content-acesobj_param_t <> it_parameters.
        RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
          MESSAGE ID 'ACAC_EXCEL'
          TYPE 'E'
          NUMBER '014'
          WITH is_accrual_obj-bukrs is_accrual_obj-acac_objtype is_accrual_obj-acac_objnumber is_accrual_obj-keydate.
      ENDIF.

      " check data consistency for accrual item
      READ TABLE it_obj_item INTO DATA(ls_item_cur) INDEX 1.
      lt_item_pre = cs_upload_content-acesobj_item_t.

      READ TABLE lt_item_pre INTO DATA(ls_item_pre) WITH KEY itemtype = ls_item_cur-itemtype rldnr = ls_item_cur-rldnr.
      IF sy-subrc NE 0.
        APPEND ls_item_cur TO cs_upload_content-acesobj_item_t.
      ELSE.
        IF ls_item_pre <> ls_item_cur.
          RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
            MESSAGE ID 'ACAC_EXCEL'
            TYPE 'E'
            NUMBER '010'
            WITH is_accrual_obj-bukrs is_accrual_obj-acac_objtype is_accrual_obj-acac_objnumber is_accrual_obj-keydate.
        ENDIF.
      ENDIF.

      " check data consistency for period data
      READ TABLE it_amounts INTO DATA(ls_amount_cur) INDEX 1.
      lt_amount_pre = cs_upload_content-ace_uploaded_accr_amounts_t.

      READ TABLE lt_amount_pre INTO DATA(ls_amount_pre) WITH KEY itemtype = ls_amount_cur-itemtype rldnr = ls_amount_cur-rldnr.
      IF sy-subrc NE 0.
        APPEND ls_amount_cur TO cs_upload_content-ace_uploaded_accr_amounts_t.
      ELSE.
        IF ls_amount_pre <> ls_amount_cur.
          RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
            MESSAGE ID 'ACAC_EXCEL'
            TYPE 'E'
            NUMBER '009'
            WITH is_accrual_obj-bukrs is_accrual_obj-acac_objtype is_accrual_obj-acac_objnumber is_accrual_obj-keydate.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD CHECK_MANDATORY_FIELDS.

    " check the mandotory fields which still be required no matter upload and total type
    IF is_common_excel_fields-bukrs IS INITIAL OR
       is_common_excel_fields-acac_objtype IS INITIAL OR
       is_common_excel_fields-itemtype IS INITIAL OR
       is_common_excel_fields-rwcur IS INITIAL.
      " raise exception to abort the process because related checking may be impossible
      " if mandotory fields is initial.
      RAISE EXCEPTION TYPE cx_acac_xlsx_exception
        MESSAGE ID 'ACAC_EXCEL'
        TYPE 'E'
        NUMBER '011'
        WITH iv_row_index.
    ELSE.
      " if user input the ledger in Excel file, the warning message will raise to inform user
      " that ledger will be ignored because only the ledger group is used for accrual engine currently.
*      IF is_common_excel_fields-rldnr IS NOT INITIAL.
*        RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
*          MESSAGE ID 'ACAC_EXCEL'
*          TYPE 'W'
*          NUMBER '029'
*          WITH iv_row_index.
*      ENDIF.
    ENDIF.

    " for leading ledger group, ledger group may be initial.
    IF is_common_excel_fields-ldgrp IS INITIAL.
      cl_ace_cfg_db_buffer=>get_cfg_ldgrp_in_itemtypel(
        EXPORTING
          id_comp           = acacc_comp                        " Accrual Engine Application Component
          id_itemtype       = is_common_excel_fields-itemtype   " Accrual Item Type
          id_bukrs          = is_common_excel_fields-bukrs      " Company Code
        IMPORTING
          et_tace_itemtypel = DATA(lt_tace_itemtypel)           " Component-Specific Program Components
      ).

      READ TABLE lt_tace_itemtypel WITH KEY ldgrp = is_common_excel_fields-ldgrp TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE cx_acac_xlsx_exception
          MESSAGE ID 'ACAC_EXCEL'
          TYPE 'E'
          NUMBER '011'
          WITH iv_row_index.
      ENDIF.
    ENDIF.

    IF iv_upload_type IS SUPPLIED.
      IF iv_upload_type EQ acacc_upload_option-period.
        IF is_common_excel_fields-keydate IS INITIAL OR
           is_common_excel_fields-keydate EQ gc_date_initial_value OR
           ( is_common_excel_fields-overwrite_allowed NE 'X' AND
              ( is_common_excel_fields-life_start_date IS INITIAL OR is_common_excel_fields-life_end_date IS INITIAL ) ).
          " raise exception to abort the process because related checking may be impossible
          " if mandotory fields is initial.
          RAISE EXCEPTION TYPE cx_acac_xlsx_exception
            MESSAGE ID 'ACAC_EXCEL'
            TYPE 'E'
            NUMBER '011'
            WITH iv_row_index.
        ENDIF.

      ELSEIF iv_upload_type EQ acacc_upload_option-total.
        IF is_common_excel_fields-life_start_date IS INITIAL OR
           is_common_excel_fields-life_end_date IS INITIAL OR
           is_common_excel_fields-acrmethod IS INITIAL OR
           ( is_common_excel_fields-overwrite_allowed EQ 'X'
                  AND ( is_common_excel_fields-keydate IS INITIAL OR is_common_excel_fields-keydate EQ gc_date_initial_value ) ).
          " raise exception to abort the process because related checking may be impossible
          " if mandotory fields is initial.
          RAISE EXCEPTION TYPE cx_acac_xlsx_exception
            MESSAGE ID 'ACAC_EXCEL'
            TYPE 'E'
            NUMBER '011'
            WITH iv_row_index.
        ENDIF.
      ELSE.
        " raise exception to abort the process because related checking may be impossible
        " if mandotory fields is initial.
        RAISE EXCEPTION TYPE cx_acac_xlsx_exception
          MESSAGE ID 'ACAC_EXCEL'
          TYPE 'E'
          NUMBER '011'
          WITH iv_row_index.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD CHECK_OVERWRITE_ALLOWED.
    DATA: ls_accrual_msg TYPE acac_accrual_message.

    IF is_accrual_obj-overwrite_allowed NE abap_true AND is_accrual_obj-overwrite_allowed NE abap_false.
      " related overwirte flag
      RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
        MESSAGE ID 'ACAC_EXCEL'
        TYPE 'E'
        NUMBER '021'
        WITH iv_row_index.
    ENDIF.
  ENDMETHOD.


  METHOD CHECK_OVERWRITE_FLAG.

*    Manual Accrual related variables
    DATA: lv_bukrs              TYPE ace_bukrs,
          lv_accrual_type       TYPE acac_objtype,
          lv_object_number      TYPE acac_objnumber,
          ls_acac_object_key    TYPE acac_object_key,
          ls_upload_content_pre LIKE LINE OF ct_upload_content,
          lv_overwrite_allowed  TYPE xflag,
          lv_acegrp             TYPE string,
          lv_group_count        TYPE n VALUE 1.

    DATA: lt_return TYPE bapiret2_t.


    "The accrual object will be grouped by object number, and lines belong to the same object will be saved in one shot
    "Empty acctual object number means always a new object
    LOOP AT ct_upload_content ASSIGNING FIELD-SYMBOL(<ls_upload_content>).
      CLEAR lt_return.
      lv_overwrite_allowed = <ls_upload_content>-overwrite_allowed.

      lv_object_number = <ls_upload_content>-acac_objnumber.
      lv_accrual_type = <ls_upload_content>-acac_objtype.
      lv_bukrs = <ls_upload_content>-bukrs.
      IF lv_object_number IS NOT INITIAL AND (  strlen( lv_object_number ) <= 2  OR lv_object_number+0(2) NE '$$' ).
        ls_acac_object_key-acac_objnumber = lv_object_number.
        ls_acac_object_key-acac_objtype   = lv_accrual_type.
        ls_acac_object_key-bukrs          = lv_bukrs.
        ls_acac_object_key-mandt          = sy-mandt.

        CALL FUNCTION 'ACAC_OBJECT_SELECT'
          EXPORTING
            is_acac_object_key = ls_acac_object_key
          EXCEPTIONS
            not_found          = 1.

        IF sy-subrc EQ 0 AND lv_overwrite_allowed NE 'X'.
          " set the overwrite flag into 'X', when the user try to a existing accrual object with leaving overwrite empty
          MESSAGE e030(acac_excel) INTO sy-lisel.
          cl_ace_generic_services=>save_msg_to_return(
            EXPORTING
              ib_use_syst = abap_true
            CHANGING
              ct_return   = lt_return
          ).
          APPEND LINES OF lt_return TO <ls_upload_content>-ace_accdoc_return_t.
        ELSEIF sy-subrc EQ 1 AND lv_overwrite_allowed EQ 'X'.
          " set the overwrite flag into 'X', when the user try to a existing accrual object with leaving overwrite empty
          MESSAGE e028(acac_excel) INTO sy-lisel.
          cl_ace_generic_services=>save_msg_to_return(
            EXPORTING
              ib_use_syst = abap_true
            CHANGING
              ct_return   = lt_return
          ).
          APPEND LINES OF lt_return TO <ls_upload_content>-ace_accdoc_return_t.
        ENDIF.
      ELSE.
        IF lv_overwrite_allowed EQ 'X'.
          " leave the overwrite flag to empty, when the user upload to create accrual object with flag 'X'
          MESSAGE e028(acac_excel) INTO sy-lisel.
          cl_ace_generic_services=>save_msg_to_return(
            EXPORTING
              ib_use_syst = abap_true
            CHANGING
              ct_return   = lt_return
          ).
          APPEND LINES OF lt_return TO <ls_upload_content>-ace_accdoc_return_t.
        ENDIF.
      ENDIF.

      IF <ls_upload_content>-acac_objnumber IS NOT INITIAL AND
            ( ls_upload_content_pre-bukrs          = <ls_upload_content>-bukrs AND
              ls_upload_content_pre-acac_objtype   = <ls_upload_content>-acac_objtype AND
              ls_upload_content_pre-acac_objnumber = <ls_upload_content>-acac_objnumber ).

        "Flag overwrite allowed should be consistent through all lines belong to one accrual object
        IF ls_upload_content_pre IS NOT INITIAL
              AND ls_upload_content_pre-overwrite_allowed <> <ls_upload_content>-overwrite_allowed.

          " todo: new error message
          MESSAGE e035(acac_excel)
            WITH <ls_upload_content>-bukrs
                 <ls_upload_content>-acac_objtype
                 <ls_upload_content>-acac_objnumber
            INTO sy-lisel.
          cl_ace_generic_services=>save_msg_to_return(
            EXPORTING
              ib_use_syst = abap_true
            CHANGING
              ct_return   = lt_return
          ).
          APPEND LINES OF lt_return TO <ls_upload_content>-ace_accdoc_return_t.
          " Synchronize the error message in the same group
          LOOP AT ct_upload_content ASSIGNING FIELD-SYMBOL(<ls_upload_content_sub>) WHERE acegrp = lv_acegrp.
            APPEND LINES OF lt_return TO <ls_upload_content_sub>-ace_accdoc_return_t.
          ENDLOOP.
        ENDIF.
        <ls_upload_content>-acegrp = lv_acegrp.
      ELSE.
        CONCATENATE 'GRP' lv_group_count INTO lv_acegrp.
        <ls_upload_content>-acegrp = lv_acegrp.
        lv_group_count = lv_group_count + 1.
      ENDIF.
      ls_upload_content_pre = <ls_upload_content>.
    ENDLOOP.


  ENDMETHOD.


  METHOD CHECK_UPLOAD_TYPE.
    DATA: ls_accrual_msg        TYPE acac_accrual_message,
          lv_total_amount_type  TYPE char1,
          lv_upload_amount_type TYPE char1.
    CLEAR: ev_amount_type, lv_total_amount_type, lv_upload_amount_type.
    LOOP AT it_amount_curr INTO DATA(ls_amount_curr) WHERE type = acacc_upload_option-total AND amount_value <> ''.
      lv_total_amount_type = ls_amount_curr-type.
      EXIT.
    ENDLOOP.

    LOOP AT it_amount_curr INTO ls_amount_curr WHERE type = acacc_upload_option-period AND amount_value <> ''.
      lv_upload_amount_type = ls_amount_curr-type.
      EXIT.
    ENDLOOP.

    IF lv_total_amount_type IS NOT INITIAL AND lv_upload_amount_type IS NOT INITIAL.
      RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
        MESSAGE ID 'ACAC_EXCEL'
        TYPE 'E'
        NUMBER '012'
        WITH iv_row_index.

    ELSE.
      IF lv_total_amount_type IS NOT INITIAL.
        ev_amount_type = lv_total_amount_type.
      ELSE.
        ev_amount_type = lv_upload_amount_type.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD COLLECT_UPLOAD_RESULT.


    DATA: ls_upload_result TYPE acac_s4_upload_result_worklist.
    DATA: lt_messages_all  TYPE ace_message_handler_t.
    DATA: ls_message       TYPE LINE OF ace_message_handler_t.
    DATA: lt_return        TYPE bapiret2_t.
    DATA: ls_return        TYPE bapiret2.
    DATA: ld_refkey        TYPE ace_ref_key.

    eb_error_occurred = cl_ace_message_handler=>get_instance( )->check_error_occurred( ).
    lt_messages_all   = cl_ace_message_handler=>get_instance( )->export( ).
    cl_ace_message_handler=>get_instance( )->reset( ).

    IF eb_error_occurred EQ abap_true OR ib_last EQ abap_true.
      SORT ct_upload_content BY acegrp.
      LOOP AT ct_upload_content ASSIGNING FIELD-SYMBOL(<ls_upload_content>).
        AT NEW acegrp.
          CALL FUNCTION 'ACAC_REF_KEY_ENCODE'
            EXPORTING
              id_objtyp    = <ls_upload_content>-acac_objtype
              id_objnumber = <ls_upload_content>-acac_objnumber
            IMPORTING
              ed_ref_key   = ld_refkey.
        ENDAT.

        READ TABLE <ls_upload_content>-acesobj_item_t ASSIGNING FIELD-SYMBOL(<ls_acesobj_item>) INDEX 1.

        REFRESH lt_return.

        LOOP AT lt_messages_all INTO ls_message WHERE comp     = 'ACAC'
                                                  AND bukrs    =  <ls_upload_content>-bukrs
                                                  AND ref_key  = ld_refkey
                                                  AND itemtype = <ls_acesobj_item>-itemtype
                                                  AND ldgrp    = <ls_upload_content>-ldgrp .
          delete lt_messages_all INDEX sy-tabix.
          ls_return-type       = ls_message-msgty.
          ls_return-id         = ls_message-msgid.
          ls_return-number     = ls_message-msgno.
          ls_return-message_v1 = ls_message-msgv1.
          ls_return-message_v2 = ls_message-msgv2.
          ls_return-message_v3 = ls_message-msgv3.
          ls_return-message_v4 = ls_message-msgv4.
          ls_return-message    = ls_message-message.
          APPEND ls_return to lt_return.
        ENDLOOP.

*          collect messages together
        APPEND LINES OF lt_return TO <ls_upload_content>-ace_accdoc_return_t.
*        prepare the upload result list first
        " display the respective currency and amount
        MOVE-CORRESPONDING <ls_upload_content> TO ls_upload_result.
        MOVE-CORRESPONDING <ls_upload_content>-aces_currkeys TO ls_upload_result.
      " as a matter of fact, only one try in the internal table
      " and either item internal table or upload internal table have the entries, so
      " may set the accrual methos by entries existing
        LOOP AT <ls_upload_content>-acesobj_item_t INTO DATA(ls_acesobj_item).
          MOVE-CORRESPONDING ls_acesobj_item TO ls_upload_result.
        ENDLOOP.
        LOOP AT <ls_upload_content>-ace_uploaded_accr_amounts_t INTO DATA(ls_acesobj_per_data).
        MOVE-CORRESPONDING ls_acesobj_per_data TO ls_upload_result.
        ls_upload_result-acrmethod = acacc_accrual_method-upload.
      ENDLOOP.
      " the object number is generated in saving phase, so set the object number here
      IF ib_last EQ abap_true.
          IF <ls_upload_content>-overwrite_allowed NE 'X'.
            ls_upload_result-acac_objnumber = <ls_upload_content>-acac_objnumber.
        ENDIF.
      ENDIF.
      ls_upload_result-message = TEXT-001.
      " add the result and go to next entry
      APPEND ls_upload_result TO ct_upload_result.

        AT END OF acegrp. "END OF acac_objnumber. only one accrual object
          "If still messages for this accrual object are not collected, add them into the first line
          LOOP AT lt_messages_all INTO ls_message WHERE ref_key EQ ld_refkey.
            READ TABLE ct_upload_content ASSIGNING FIELD-SYMBOL(<ls_upload_content_2>)
                    WITH key bukrs = <ls_upload_content>-bukrs
                             acac_objtype = <ls_upload_content>-acac_objtype
                             acac_objnumber = <ls_upload_content>-acac_objnumber.
            IF <ls_upload_content_2> IS ASSIGNED.
              ls_return-type       = ls_message-msgty.
              ls_return-id         = ls_message-msgid.
              ls_return-number     = ls_message-msgno.
              ls_return-message_v1 = ls_message-msgv1.
              ls_return-message_v2 = ls_message-msgv2.
              ls_return-message_v3 = ls_message-msgv3.
              ls_return-message_v4 = ls_message-msgv4.
              ls_return-message    = ls_message-message.
              APPEND ls_return TO <ls_upload_content_2>-ace_accdoc_return_t.
            ENDIF.
          ENDLOOP.
        ENDAT.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD CURRENCY_CONVERSION.
    DATA: ls_message TYPE acac_accrual_message.

    " iv_amount already be round before according to currency's decimal
    CALL FUNCTION 'CURRENCY_AMOUNT_BAPI_TO_SAP'
      EXPORTING
        currency              = iv_currency
        bapi_amount           = iv_amount
      IMPORTING
        sap_amount            = cv_sap_amount
      EXCEPTIONS
        bapi_amount_incorrect = 1.

    IF sy-subrc EQ 1.
      RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
        MESSAGE ID 'ACAC_EXCEL'
        TYPE 'E'
        NUMBER '013'
        WITH iv_row_index.
    ENDIF.
  ENDMETHOD.


  METHOD FILL_DEFAULT_VALUE_INTO_ITEM.
    DATA: lt_fields    TYPE cl_acac_s4_xlsx_template=>ty_t_field_catalog.

    IF cs_upload_content-overwrite_allowed EQ 'X'.
      " Exsited, the accrual object should not be updated again
      READ TABLE cs_upload_content-acesobj_item_t ASSIGNING FIELD-SYMBOL(<ls_uploaded_item>) INDEX 1.
      IF sy-subrc EQ 0.
        READ TABLE io_ace_subobj->mt_items INTO DATA(ls_existed_item) WITH KEY itemtype = <ls_uploaded_item>-itemtype.
        IF sy-subrc EQ 0.
          TRY.
              ls_existed_item-o_item->get_item_td_valid_on_detdate(
                EXPORTING
                  id_detdate = cs_upload_content-keydate
                  id_ledger  = <ls_uploaded_item>-rldnr
                IMPORTING
                  es_item_td = DATA(ls_existed_ledger_item)
              ).
              IF ls_existed_ledger_item IS NOT INITIAL.
                lb_item_existed = abap_true.
              ELSE.
                lb_item_existed = abap_false.
              ENDIF.
            CATCH cx_ace.
              lb_item_existed = abap_false.
          ENDTRY.

          " skip try to copy corresponding value
          IF lb_item_existed = abap_true.
            cl_acac_s4_xlsx_template=>get_individual_fields(
              EXPORTING
                iv_struc_name         = 'ACESOBJ_ITEM_EXT'
              CHANGING
                ct_full_field_catalog = lt_fields
            ).

            LOOP AT lt_fields INTO DATA(ls_field).

              ASSIGN COMPONENT  ls_field-stru_name OF STRUCTURE <ls_uploaded_item> TO FIELD-SYMBOL(<ls_value>).
              IF <ls_value> IS ASSIGNED AND <ls_value> IS INITIAL.
                ASSIGN COMPONENT  ls_field-stru_name OF STRUCTURE ls_existed_ledger_item TO FIELD-SYMBOL(<ls_db_value>).
                IF <ls_db_value> IS ASSIGNED AND <ls_db_value> IS NOT INITIAL.
                  <ls_value> = <ls_db_value>.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
        " end user may not input the accrual method
        IF lb_item_existed = abap_false.
          IF <ls_uploaded_item>-acrmethod IS INITIAL.
            <ls_uploaded_item>-acrmethod = acacc_accrual_method-upload.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      " if the accrual method is empty, set the method into upload by default
      LOOP AT cs_upload_content-acesobj_item_t ASSIGNING <ls_uploaded_item>.
        IF <ls_uploaded_item>-acrmethod IS INITIAL.
          <ls_uploaded_item>-acrmethod = acacc_accrual_method-upload.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD GET_CURRENCY_CONFIGURATION.
    SELECT * FROM tcurx INTO TABLE gt_currency_configuration.
    SELECT * FROM tcurc INTO TABLE gt_currency_total.
  ENDMETHOD.


  METHOD GET_EXCEL_FIELDS.
    DATA: ls_field_name TYPE ty_excel_field.
    FIELD-SYMBOLS: <ft_field_line> TYPE fac_t_excel_data,
                   <fs_field>      TYPE any.

    ASSIGN is_excel_data-value->* TO <ft_field_line>.

    LOOP AT <ft_field_line> ASSIGNING FIELD-SYMBOL(<fs_field_line>).
      ASSIGN <fs_field_line>-value->* TO <fs_field>.
      ls_field_name-index = <fs_field_line>-index.
      ls_field_name-excel_field = <fs_field>.

      TRANSLATE ls_field_name-excel_field TO UPPER CASE.

      READ TABLE gt_excel_fields WITH KEY excel_field = ls_field_name-excel_field TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        cv_field_exist = abap_true.
        cv_field_name  = ls_field_name-excel_field.
      ELSE.
        APPEND ls_field_name TO gt_excel_fields.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD GET_PARAMETER_ASSIGNMENT.
    SELECT objtype acac_parameter FROM tacac_po_assign INTO CORRESPONDING FIELDS OF TABLE gt_param_assignment.
  ENDMETHOD.


  METHOD GET_PARAM_FIELDS.
    DATA: lo_type_desc TYPE REF TO cl_abap_typedescr,
          ls_tacecomp  TYPE tacecomp.

    CALL FUNCTION 'ACE2_TACECOMP_READ'
      EXPORTING
        i_comp     = acacc_comp
      IMPORTING
        e_tacecomp = ls_tacecomp
      EXCEPTIONS
        not_found  = 1.

    gd_param_structure = ls_tacecomp-param_fields.

    CHECK sy-subrc EQ 0.

    CALL METHOD cl_abap_structdescr=>describe_by_name
      EXPORTING
        p_name         = ls_tacecomp-param_fields
      RECEIVING
        p_descr_ref    = lo_type_desc
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2.

    CHECK sy-subrc EQ 0.

    gt_param_fields = lo_type_desc->get_ddic_object( ).
  ENDMETHOD.


  METHOD GLACCOUNT_ALPHA_CONVERSION.
    IF cs_account-fin_accr_accnt IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = cs_account-fin_accr_accnt
        IMPORTING
          output = cs_account-fin_accr_accnt.
    ENDIF.

    IF cs_account-fin_offstng_accnt IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = cs_account-fin_offstng_accnt
        IMPORTING
          output = cs_account-fin_offstng_accnt.
    ENDIF.

    IF cs_account-inc_offstng_accnt IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = cs_account-inc_offstng_accnt
        IMPORTING
          output = cs_account-inc_offstng_accnt.
    ENDIF.

    IF cs_account-inc_accr_accnt IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = cs_account-inc_accr_accnt
        IMPORTING
          output = cs_account-inc_accr_accnt.
    ENDIF.

    IF cs_account-per_accr_accnt IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = cs_account-per_accr_accnt
        IMPORTING
          output = cs_account-per_accr_accnt.
    ENDIF.

    IF cs_account-per_offstng_accnt IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = cs_account-per_offstng_accnt
        IMPORTING
          output = cs_account-per_offstng_accnt.
    ENDIF.
  ENDMETHOD.


  METHOD IF_ACAC_UPLOADING_TEST~REPLACE_BUKRS_LEDGER_KOSTL_PC.

    cd_m_struc_common-bukrs = mv_bukrs_test.
    cd_m_struc_common-kostl = mv_cost_center_test.
    cd_m_struc_common-prctr = mv_prctr_test.
    CASE cd_m_struc_common-ldgrp.
      WHEN '0L'.
        cd_m_struc_common-ldgrp = mv_ledger_leading_test.
      WHEN 'A1'.
        cd_m_struc_common-ldgrp = mv_ledger_non_leading_test.
    ENDCASE.

  ENDMETHOD.


  METHOD IF_ACAC_UPLOADING_TEST~SET_ACCOUNTS.

    mv_hkont_accruals_test   = iv_hkont_accruals.
    mv_hkont_offestting_test = iv_hkont_offestting.

  ENDMETHOD.


  METHOD IF_ACAC_UPLOADING_TEST~SET_BUKRS_LEDGR_KOSTL_PRCTR.

    mv_bukrs_test              = iv_bukrs.
    mv_ledger_leading_test     = iv_ledger_leading.
    mv_ledger_non_leading_test = iv_ledger_non_leading.
    mv_cost_center_test        = iv_cost_center.
    mv_prctr_test              = iv_profit_center.

  ENDMETHOD.


  METHOD IF_ACAC_UPLOADING_TEST~SET_KEYDATE_FOR_TEST.
    CLEAR gt_keydates_test.
    gt_keydates_test = it_keydates.
  ENDMETHOD.


  METHOD IF_ACAC_UPLOADING_TEST~SET_LIFECYCLEDATE_FOR_TEST.
    CLEAR gt_lifecycledates_test.
    gt_lifecycledates_test = it_lifecycledates.
  ENDMETHOD.


  METHOD IF_ACAC_UPLOADING_TEST~SET_OBJECT_NUMBERS_FOR_TEST.
    CLEAR gt_object_numbers_test.
    gt_object_numbers_test = it_object_numbers.
  ENDMETHOD.


  METHOD IF_ACAC_UPLOADING~UPLOAD_ACAC_EXCEL.
    CLEAR: gt_excel_fields, gt_param_fields, gt_temp_created_objnums, et_upload_content, et_upload_result, mb_test_mode.

* Two Kind Error Message:
* 1. For parsing phase, error message will display in error message dialog by the message handler dialog.

* 2. For creating or updating phase, error message will display in upload result list. It will be matched to each line along with excel file.
*
*
*
*    TRY.
*        IF 1 = 2.
*          DATA lo_tdc_ref TYPE REF TO cl_apl_ecatt_tdc_api.
*          lo_tdc_ref = cl_apl_ecatt_tdc_api=>get_instance( i_testdatacontainer = 'FINS_UNIT_ACAC_S4_TC' i_write_access = 'X' i_testdatacontainer_version = '1' ).
*          DATA(lv_param_name) = 'MODIFY'.
*          DATA(lv_variant_name) = 'UT1UPL_0L_A1'.
*          lo_tdc_ref->set_value(
*            EXPORTING
*              i_param_name   = CONV #( lv_param_name )                 " Parameter Name
*              i_variant_name = CONV #( lv_variant_name )              " Variant Name
*              i_param_value  = iv_xfile                 " Variable from which the value is to be copied from
*          ).
*          lo_tdc_ref->commit_changes( ).
*        ENDIF.
*      CATCH cx_root.
*
*    ENDTRY.
****************** Parsing Excel File ******************

*   Determine if the method is called in testmode or not
    mb_test_mode = ib_test_mode.
    TRY.
        " parse the excel file to ACAC structure
        CALL METHOD parse_xlsx_map_fields
          EXPORTING
            iv_file_name      = iv_file_name
            iv_xfile          = iv_xfile
          CHANGING
            ct_upload_content = et_upload_content.

      CATCH BEFORE UNWIND cx_acac_xlsx_exception INTO DATA(lx_acac_xlsx).
        cl_ace_message_handler=>get_instance( )->add_message_inst( ix_ace = lx_acac_xlsx ).
        IF cl_ace_message_handler=>get_instance( )->get_is_resumable( ) = abap_true.
          RESUME.
        ENDIF.
    ENDTRY.


****************** Replace some fields in order to test:  ***********************************
****************** Object Number, Key Date, Start of Life and End of Life  ******************

    " Object number need to be filled in in order to update the accrual object for testing
    IF mb_test_mode EQ abap_true AND gt_object_numbers_test IS NOT INITIAL.
      LOOP AT gt_object_numbers_test INTO DATA(ls_object_number_test).
        READ TABLE et_upload_content ASSIGNING FIELD-SYMBOL(<ls_upload_content>) INDEX ls_object_number_test-index.
        IF sy-subrc EQ 0.
          <ls_upload_content>-acac_objnumber = ls_object_number_test-acac_objnumber.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Key date need updating in order to update the accrual object for testing
    IF mb_test_mode EQ abap_true AND gt_keydates_test IS NOT INITIAL.
      LOOP AT gt_keydates_test INTO DATA(ls_keydate_test).
        READ TABLE et_upload_content ASSIGNING <ls_upload_content> INDEX ls_keydate_test-index.
        IF sy-subrc EQ 0.
          <ls_upload_content>-keydate = ls_keydate_test-keydate.
          READ TABLE <ls_upload_content>-ace_uploaded_accr_amounts_t ASSIGNING FIELD-SYMBOL(<ls_uploaded_accr_amounts_t>) INDEX 1.
          IF sy-subrc EQ 0.
            <ls_uploaded_accr_amounts_t>-period_det_date = ls_keydate_test-keydate.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Start of life and/or end of life need updating in in order to update the accrual object for testing
    IF mb_test_mode EQ abap_true AND gt_lifecycledates_test IS NOT INITIAL.
      LOOP AT gt_lifecycledates_test INTO DATA(ls_lifecycledate_test).
        READ TABLE et_upload_content ASSIGNING <ls_upload_content> INDEX ls_lifecycledate_test-index.
        IF sy-subrc EQ 0.
          <ls_upload_content>-acesobj_item_t[ 1 ]-life_start_date = ls_lifecycledate_test-life_start_date.
          <ls_upload_content>-acesobj_item_t[ 1 ]-life_end_date = ls_lifecycledate_test-life_end_date.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Change account according to the system environment OP/Cloud
    IF mb_test_mode EQ abap_true AND mv_hkont_accruals_test IS NOT INITIAL AND mv_hkont_offestting_test IS NOT INITIAL.
      LOOP AT et_upload_content ASSIGNING FIELD-SYMBOL(<ls_upload_content_account>).
        <ls_upload_content>-acesobj_item_t[ 1 ]-inc_offstng_accnt = mv_hkont_offestting_test.
        <ls_upload_content>-acesobj_item_t[ 1 ]-inc_accr_accnt    = mv_hkont_accruals_test.
      ENDLOOP.
    ENDIF.


****************** Create or Update the Accrual Object ******************

    " the accrual object will be crated if error happened when parsing the excel file.
    CHECK cl_ace_message_handler=>get_instance( )->check_error_occurred( ) EQ abap_false..

    cl_ace_message_handler=>get_instance( )->determine_return_status( IMPORTING eb_message_exists = DATA(lb_message_exists) ).
    IF lb_message_exists EQ abap_true.
      cl_ace_message_handler=>get_instance( )->display_log(
        EXPORTING
          ib_tree        = abap_false
          ib_popup       = abap_true
      ).
      cl_ace_message_handler=>get_instance( )->reset( ).
    ENDIF.



****************** Creating Accrual Object ******************
    SORT et_upload_content BY bukrs.

    " create or update ACAC object
    CALL METHOD modify_acac_object
        IMPORTING
          et_upload_result  = et_upload_result
          eb_error_occurred = eb_error_occurred
        CHANGING
          ct_upload_content = et_upload_content.

    " set the final message. display the successful message if no error happened.
    set_final_message( CHANGING ct_upload_content = et_upload_content
                                ct_upload_result  = et_upload_result ).

  ENDMETHOD.


  METHOD INITIALIZE_ACC_OBJ.

    DATA: lv_new_subobjkey TYPE acesobj_key,
          lv_ref_key       TYPE ace_ref_key,
          lv_ref_subkey    TYPE ace_ref_subkey.

    DATA: lo_acac_mdo_subobj TYPE REF TO cl_acac_mdo_subobj.

     IF iv_overwrite_flag NE 'X'. "New accrual object
      "External Accrual Object Number
      IF iv_object_number IS NOT INITIAL AND iv_accrual_type IS NOT INITIAL.
        lo_acac_mdo_subobj ?= co_ace_mdo_subobj.
        lo_acac_mdo_subobj->check_accr_obj_number_external(
            iv_bukrs     = iv_bukrs                 " Company Code
            iv_objtype   = iv_accrual_type          " Accrual Object Category for Manual Accruals
            iv_objnumber = iv_object_number ).

        " get ref_key and ref_subkey for the concerning accrual object
          CALL FUNCTION 'ACAC_REF_KEY_ENCODE'
            EXPORTING
              id_objtyp    = iv_accrual_type
              id_objnumber = iv_object_number
            IMPORTING
              ed_ref_key   = lv_ref_key.

          CALL FUNCTION 'ACAC_REF_SUBKEY_ENCODE'
            IMPORTING
              ed_ref_subkey = lv_ref_subkey.

        lv_new_subobjkey-ref_key    = lv_ref_key.
          lv_new_subobjkey-ref_subkey = lv_ref_subkey.
      ENDIF.

      lv_new_subobjkey-comp = iv_comp.
      lv_new_subobjkey-bukrs = iv_bukrs.
      co_ace_mdo_subobj->initialize(
        EXPORTING
          is_subobj_key = lv_new_subobjkey
      ).
    ELSE.

*    IF iv_overwrite_flag NE 'X' AND ib_have_objectnumber NE abap_true.
**    1. create a new accrual object
**    3. create a new accural object with given object number
*      IF iv_object_number IS NOT INITIAL.
*        lo_acac_mdo_subobj ?= co_ace_mdo_subobj.
*        lo_acac_mdo_subobj->check_accr_obj_number_external(
*            iv_bukrs     = iv_bukrs                 " Company Code
*            iv_objtype   = iv_accrual_type          " Accrual Object Category for Manual Accruals
*            iv_objnumber = iv_object_number ).
*
*        IF iv_accrual_type IS NOT INITIAL.
*          " get ref_key and ref_subkey for the concerning accrual object
*          CALL FUNCTION 'ACAC_REF_KEY_ENCODE'
*            EXPORTING
*              id_objtyp    = iv_accrual_type
*              id_objnumber = iv_object_number
*            IMPORTING
*              ed_ref_key   = lv_ref_key.
*
*          CALL FUNCTION 'ACAC_REF_SUBKEY_ENCODE'
*            IMPORTING
*              ed_ref_subkey = lv_ref_subkey.
*          lv_new_subobjkey-ref_key = lv_ref_key.
*          lv_new_subobjkey-ref_subkey = lv_ref_subkey.
*        ENDIF.
*      ENDIF.
*
*      lv_new_subobjkey-comp = iv_comp.
*      lv_new_subobjkey-bukrs = iv_bukrs.
*      co_ace_mdo_subobj->initialize(
*        EXPORTING
*          is_subobj_key = lv_new_subobjkey
*      ).
*    ELSE.
      " get ref_key and ref_subkey for the concerning accrual object
      CALL FUNCTION 'ACAC_REF_KEY_ENCODE'
        EXPORTING
          id_objtyp    = iv_accrual_type
          id_objnumber = iv_object_number
        IMPORTING
          ed_ref_key   = lv_ref_key.

      CALL FUNCTION 'ACAC_REF_SUBKEY_ENCODE'
        IMPORTING
          ed_ref_subkey = lv_ref_subkey.
*    2. update a existed accrual object with existed object number
      " if ref_key and ref_subkey are provided, update the related accrual object
      TRY.
          co_ace_mdo_subobj->initialize_from_db_buffer(
            EXPORTING
              iv_ref_key    = lv_ref_key
              iv_ref_subkey = lv_ref_subkey ).
        CATCH cx_ace INTO DATA(lx_ace).
          RAISE EXCEPTION TYPE cx_ace
            MESSAGE ID 'ACAC_EXCEL'
            TYPE 'E'
            NUMBER '028'
            EXPORTING
              gv_comp       = iv_comp
              gv_bukrs      = iv_bukrs
              gv_ref_key    = lv_ref_key
              gv_ref_subkey = lv_ref_subkey.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD INITIAL_GLOBAL_VARIANT.
    " all parameter fields
    get_parameter_assignment( ).
    " parameter fields of Accrual Object
    get_param_fields( ).
    " set the fields that unnecessary to convert upper
    set_case_sensitive_fields( ).
    " decimal place
    get_currency_configuration( ).
    " mapping amount to currency
    set_amount_curr_map( ).
  ENDMETHOD.


  METHOD MAP_AMOUNT_CURRENCY_FIELDS.
    DATA: ls_obj_item_tmp    TYPE acesobj_item_ext,
          ls_period_item_tmp TYPE ace_uploaded_accr_amounts_ext,
          ls_amount_curr_map TYPE ty_amount_curr_map,
          ld_sap_amount      TYPE bapicurr-bapicurr,
          ld_bapi_amount     TYPE bapicurr-bapicurr,
          ls_amount_curr     TYPE ty_amount_curr,
          lv_totl_curr_name  TYPE string,
          lv_upld_curr_name  TYPE string.

    IF iv_amount_type = acacc_upload_option-total.
      ls_obj_item_tmp = cs_obj_item.
      CLEAR: ls_obj_item_tmp-rldnr, ls_obj_item_tmp-itemtype.
      IF ls_obj_item_tmp IS NOT INITIAL.
        LOOP AT gt_amount_curr_map INTO ls_amount_curr_map WHERE type = iv_amount_type.
          CLEAR ld_sap_amount.
          READ TABLE it_amount_curr INTO ls_amount_curr WITH KEY amount_name = ls_amount_curr_map-amount_name.
          IF sy-subrc EQ 0.
            TRY.
                ld_bapi_amount = ls_amount_curr-round_amount.
                CALL METHOD currency_conversion
                  EXPORTING
                    iv_amount      = ld_bapi_amount
                    iv_currency    = CONV #( ls_amount_curr-curr_value )
                    is_accrual_obj = is_accrual_obj
                    iv_row_index   = iv_row_index
                  CHANGING
                    cv_sap_amount  = ld_sap_amount.

                IF ld_sap_amount IS NOT INITIAL.
                  ASSIGN COMPONENT ls_amount_curr_map-amount_name OF STRUCTURE cs_obj_item TO FIELD-SYMBOL(<fs_totl_amount>).
                  <fs_totl_amount> = ld_sap_amount.
                ENDIF.
                CONCATENATE ls_amount_curr_map-curr_name cl_acac_s4_xlsx_template=>gc_total_curr_suffix INTO lv_totl_curr_name.
                ASSIGN COMPONENT lv_totl_curr_name OF STRUCTURE cs_obj_item TO FIELD-SYMBOL(<fs_totl_currency>).
                <fs_totl_currency> = ls_amount_curr-curr_value.
              CATCH cx_sy_conversion_overflow INTO DATA(lv_message).
                RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
                  MESSAGE ID 'ACAC_EXCEL'
                  TYPE 'E'
                  NUMBER '019'
                  WITH iv_row_index.
            ENDTRY.
          ENDIF.
        ENDLOOP.

*      ls_obj_item-date_from = ls_accrual_obj-keydate.
        APPEND cs_obj_item TO cs_excel_data-upload_content-acesobj_item_t.
      ENDIF.
    ELSE.
      cs_period_item-period_det_date = is_accrual_obj-keydate.
      ls_period_item_tmp = cs_period_item.
      CLEAR: ls_period_item_tmp-rldnr, ls_period_item_tmp-itemtype, ld_bapi_amount, ld_sap_amount, ls_amount_curr.
      IF ls_period_item_tmp IS NOT INITIAL.
        LOOP AT gt_amount_curr_map INTO ls_amount_curr_map WHERE type = iv_amount_type.
          CLEAR ld_sap_amount.
          READ TABLE it_amount_curr INTO ls_amount_curr WITH KEY amount_name = ls_amount_curr_map-amount_name.
          IF sy-subrc EQ 0.
            TRY.
                ld_bapi_amount = ls_amount_curr-round_amount.
                CALL METHOD currency_conversion
                  EXPORTING
                    iv_amount      = ld_bapi_amount
                    iv_currency    = CONV #( ls_amount_curr-curr_value )
                    is_accrual_obj = is_accrual_obj
                    iv_row_index   = iv_row_index
                  CHANGING
                    cv_sap_amount  = ld_sap_amount.

                IF ld_sap_amount IS NOT INITIAL.
                  ASSIGN COMPONENT ls_amount_curr_map-amount_name OF STRUCTURE cs_period_item TO FIELD-SYMBOL(<fs_upld_amount>).
                  <fs_upld_amount> = ld_sap_amount.
                ENDIF.
                " currency must be set on matter periodic and total
                CONCATENATE ls_amount_curr_map-curr_name cl_acac_s4_xlsx_template=>gc_upload_curr_suffix INTO lv_upld_curr_name.
                ASSIGN COMPONENT lv_upld_curr_name OF STRUCTURE cs_period_item TO FIELD-SYMBOL(<fs_upld_currency>).
                <fs_upld_currency> = ls_amount_curr-curr_value.
                " item must be hold the currency
                CONCATENATE ls_amount_curr_map-curr_name cl_acac_s4_xlsx_template=>gc_total_curr_suffix INTO lv_totl_curr_name.
                ASSIGN COMPONENT lv_totl_curr_name OF STRUCTURE cs_obj_item TO <fs_totl_currency>.
                <fs_totl_currency> = ls_amount_curr-curr_value.

              CATCH cx_sy_conversion_overflow INTO lv_message.
                RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
                  MESSAGE ID 'ACAC_EXCEL'
                  TYPE 'E'
                  NUMBER '019'
                  WITH iv_row_index.
            ENDTRY.
          ENDIF.
        ENDLOOP.
        APPEND cs_period_item TO cs_excel_data-upload_content-ace_uploaded_accr_amounts_t.
        " add the empty item item for creating new periodic accrual object
        APPEND cs_obj_item TO cs_excel_data-upload_content-acesobj_item_t.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD MAP_EXCEL_TO_ACAC_STRUCTURE.
    DATA: lr_structure      TYPE REF TO data,
          ls_excel_fields   LIKE LINE OF gt_excel_fields,
          lo_xlsx_parser    TYPE REF TO zcl_acac_s4_xlsx_parse_util,
          ls_accrual_obj    TYPE acac_s4_upload_content,
          ld_amount_field   TYPE fieldname,
          ls_assignment     TYPE acesobj_assgmt_ext,
          ls_amounts        TYPE ace_uploaded_accr_amounts_ext,
          ls_obj_header     TYPE acesobj_ext,
          ls_obj_item       TYPE acesobj_item_ext,
          "ls_acac_param     TYPE acac_parameters,
          lt_amount_curr    TYPE tt_amount_curr,
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
        READ TABLE gt_external_fields INTO ls_external_field WITH KEY int_field_name = ls_fieldname-fieldname.
        IF sy-subrc NE 0.
          " check whether field in structure is also defined in excel
          READ TABLE gt_excel_fields WITH KEY excel_field = ls_fieldname-fieldname INTO ls_excel_fields.
        ELSE.
          READ TABLE gt_excel_fields WITH KEY excel_field = ls_external_field-ext_field_name INTO ls_excel_fields.
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
            READ TABLE gt_fields_case_sensitive WITH KEY table_line = ls_fieldname-fieldname TRANSPORTING NO FIELDS.
            IF sy-subrc NE 0.
              TRANSLATE <fs_data> TO UPPER CASE.
            ENDIF.

            " deal with amount/currency fields
            IF <fs_abs_structure>-name EQ gc_common_excel_fields.

              " put the relevent amount values into internal table
              " then currency conversion will convert the amount into the correct decimal place
              "       amount_name  TYPE fieldname,
              "       curr_name    TYPE fieldname,
              "       amount_value TYPE string,
              "       curr_value   TYPE string,
              "       round_amount TYPE decfloat16,
              "       decimal      TYPE i,
              CALL METHOD set_amount_curr_value
                EXPORTING
                  iv_fieldname   = ls_fieldname-fieldname
                  iv_value       = <fs_data>
                  iv_row_index   = iv_row_index
                IMPORTING
                  ev_amount      = ld_is_amount
                CHANGING
                  ct_amount_curr = lt_amount_curr.
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
                  additional_conversion( CHANGING cv_value = <fs_value> ).
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
      IF <fs_abs_structure>-name EQ gd_param_structure.
        " parameters
        ASSIGN <fs_m_struc> TO <fs_m_struc_custom>.
      ELSEIF <fs_abs_structure>-name EQ gc_common_excel_fields.
        ASSIGN <fs_m_struc> TO <fs_m_struc_common>.
        <fs_m_struc_common>-comp = acacc_comp.
      ENDIF.
    ENDLOOP.

    SORT ct_upload_content BY bukrs acac_objtype acac_objnumber.

*    replace Company Code, Ledger and Cost Center in case of an Test
    IF mb_test_mode = abap_true AND mv_ledger_leading_test IS NOT INITIAL AND mv_ledger_non_leading_test IS NOT INITIAL AND mv_cost_center_test IS NOT INITIAL AND mv_prctr_test IS NOT INITIAL.
      CALL METHOD if_acac_uploading_test~replace_bukrs_ledger_kostl_pc
        CHANGING
          cd_m_struc_common = <fs_m_struc_common>. " Accrual Object Common Fields in Excel
    ENDIF.

*    move the key value to accrual object struture
    MOVE-CORRESPONDING <fs_m_struc_common> TO ls_accrual_obj.

    " check mandatory fields are filled or not
    CALL METHOD check_mandatory_fields(
      EXPORTING
        iv_row_index           = iv_row_index
        is_accrual_obj         = ls_accrual_obj
        is_common_excel_fields = <fs_m_struc_common> ).

    " check field overwrite allowed, this field only can has value 'X' or empty
    CALL METHOD check_overwrite_allowed(
      EXPORTING
        iv_row_index   = iv_row_index
        is_accrual_obj = ls_accrual_obj ).

    " check accrual object number is filled or not if overwrite allowed is set as abap_true in excel
    CALL METHOD check_accrual_object_number(
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
    CALL METHOD check_currency(
      EXPORTING
        iv_row_index   = iv_row_index
        is_accrual_obj = ls_accrual_obj
        it_amount_curr = lt_amount_curr ).

    " check whether one object object has values both on field Amount and Uploaded Accrual Amount
    " if yes, then throw error.
    " we only sopport end user to upload either period accrual upload or normal accrual upload
    CALL METHOD check_upload_type(
      EXPORTING
        it_amount_curr = lt_amount_curr
        is_accrual_obj = ls_accrual_obj
        iv_row_index   = iv_row_index
      IMPORTING
        ev_amount_type = lv_amount_type ).

    cs_excel_data-upload_content-type = lv_amount_type.

    " check the mandotory fields again,
    " because some fields checking depends on whether type is upload or total
    CALL METHOD check_mandatory_fields(
      EXPORTING
        iv_row_index           = iv_row_index
        is_accrual_obj         = ls_accrual_obj
        is_common_excel_fields = <fs_m_struc_common>
        iv_upload_type         = lv_amount_type ).

    " convert the G/L Account by Alpha conversion
    CALL METHOD glaccount_alpha_conversion
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

    CALL METHOD map_amount_currency_fields(
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
      CALL METHOD assignment_alpha_conversion
        CHANGING
          cs_assignment = ls_assignment.
      MOVE-CORRESPONDING ls_assignment TO cs_excel_data-upload_content-acesobj_assgmt.
    ENDIF.

*    prepare for parameters
    "MOVE <fs_m_struc_custom> TO ls_acac_param.
    IF <fs_m_struc> IS ASSIGNED.
      CALL METHOD map_parameter_assignment(
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
      AND <fs_m_struc_common>-acac_objnumber+0(2) = gc_temp_objnum_prefix.
      " don't insert duplicated object number
      IF NOT line_exists( gt_temp_created_objnums[ temp_num = <fs_m_struc_common>-acac_objnumber ] ).
        DESCRIBE TABLE gt_temp_created_objnums LINES DATA(lv_temp_objnum_count).
        APPEND VALUE #( index = lv_temp_objnum_count + 1 temp_num = <fs_m_struc_common>-acac_objnumber ) TO gt_temp_created_objnums.
      ENDIF.
    ENDIF.

    " check the consistency and modify the upload content
    CALL METHOD return_consistent_upld_content(
      EXPORTING
        is_accrual_obj    = ls_accrual_obj
      CHANGING
        ct_upload_content = ct_upload_content
        cs_excel_data     = cs_excel_data ).

  ENDMETHOD.


  METHOD MAP_PARAMETER_ASSIGNMENT.

    DATA: lt_param_assignment TYPE TABLE OF tacac_po_assign,
          lt_parameter_msg    TYPE bapiret2_t,
          ld_param_upload     TYPE flag,
          ls_parameters       TYPE aceds_object_parameter_ext.

    IF id_acac_param IS NOT INITIAL.
      " filter the parameter fields which assign to current accural object type
      LOOP AT gt_param_assignment INTO DATA(ls_param_assignment) WHERE objtype = is_accrual_obj-acac_objtype.
        READ TABLE gt_param_fields WITH KEY fieldname = ls_param_assignment-acac_parameter TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          APPEND ls_param_assignment TO lt_param_assignment.
        ELSE.
          " message raised
          " field is removed from structure but it is still in img
        ENDIF.
      ENDLOOP.

      " check the parameter field which not assign to acccural objec type whether filled in
      " if yes, insert warning message in result list (!IMPORTANT, don't display message immidetetly)
      LOOP AT gt_param_assignment INTO ls_param_assignment GROUP BY ls_param_assignment-acac_parameter.
        ASSIGN COMPONENT ls_param_assignment-acac_parameter OF STRUCTURE id_acac_param TO FIELD-SYMBOL(<fs_param_value>).
        IF <fs_param_value> IS ASSIGNED AND <fs_param_value> IS NOT INITIAL.
          IF NOT line_exists( lt_param_assignment[ acac_parameter = ls_param_assignment-acac_parameter ] ).
            RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
              MESSAGE ID 'ACAC_EXCEL'
              TYPE 'W'
              NUMBER '016'
              WITH ls_param_assignment-acac_parameter is_accrual_obj-acac_objtype.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND LINES OF lt_parameter_msg TO cs_excel_data-upload_content-ace_accdoc_return_t.


      CLEAR ls_param_assignment.
      LOOP AT lt_param_assignment INTO ls_param_assignment.
        READ TABLE gt_excel_fields WITH KEY excel_field = ls_param_assignment-acac_parameter TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          ld_param_upload = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF ld_param_upload = abap_true.
        LOOP AT lt_param_assignment INTO ls_param_assignment.
          ASSIGN COMPONENT ls_param_assignment-acac_parameter OF STRUCTURE id_acac_param TO <fs_param_value>.
          IF sy-subrc EQ 0 AND <fs_param_value> IS ASSIGNED AND <fs_param_value> IS NOT INITIAL.
            ls_parameters-param_name = ls_param_assignment-acac_parameter.
            ls_parameters-content = CONV string( <fs_param_value> ).
            APPEND ls_parameters TO cs_excel_data-upload_content-acesobj_param_t.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD MODIFY_ACAC_OBJECT.

*    Manual Accrual related variables
    DATA: lv_bukrs                 TYPE ace_bukrs,
          lv_accrual_type          TYPE acac_objtype,
          lv_object_number         TYPE acac_objnumber,
          lv_comp                  TYPE ace_comp VALUE acacc_comp,
          lo_acac_mdo_subobj       TYPE REF TO cl_acac_mdo_subobj,
          lo_ace_mdo_subobj        TYPE REF TO if_ace_mdo_subobj,
          lt_upld_perdata_itemtype TYPE if_ace_mdo_types=>tt_uploaded_per_amounts,
          ls_upld_perdata_itemtype TYPE if_ace_mdo_types=>ty_uploaded_per_amounts.

*    Accrual Engine related variables
    DATA: lv_keydate            TYPE ace_datefrom,
          ls_acesobj_assgmt     TYPE acesobj_assgmt_attributes,
          lt_acesobj_param      TYPE aceds_param_t,
          ls_acac_object_key    TYPE acac_object_key,
          ls_acac_object        TYPE acac_objects,
          ls_ace_posting_params TYPE ace_posting_params,
          lv_posting_keydate    TYPE ace_keydate.

    DATA: lt_return         TYPE bapiret2_t,
          lb_error_occurred TYPE flag,
          lx_ace            TYPE REF TO cx_ace.

    DATA: lt_upload_content     LIKE ct_upload_content.
    DATA: lt_upload_content_all LIKE ct_upload_content.
    DATA: ls_upload_content_pre LIKE LINE OF ct_upload_content.
    DATA: lb_new_object         TYPE abap_bool.
    DATA: lb_end_of_new_obj     TYPE abap_bool.


    "The accrual object will be grouped by object number, and lines belong to the same object will be saved in one shot
    "Empty acctual object number means always a new object
    SORT ct_upload_content BY bukrs acac_objtype  acac_objnumber.

    " check the overwrite allowed flag whehter set correctly and consistent
    CALL METHOD check_overwrite_flag
      CHANGING
        ct_upload_content = ct_upload_content.


    CLEAR ls_upload_content_pre.
    LOOP AT ct_upload_content ASSIGNING FIELD-SYMBOL(<ls_upload_content>).
      IF <ls_upload_content>-ace_accdoc_return_t IS NOT INITIAL.
        APPEND <ls_upload_content> TO lt_upload_content_all.
        eb_error_occurred = abap_true.
      ELSE.

        " If object number is entered and is different from last line, it's a new object
      IF <ls_upload_content>-acac_objnumber IS NOT INITIAL AND
            ( ls_upload_content_pre-bukrs          = <ls_upload_content>-bukrs AND
              ls_upload_content_pre-acac_objtype   = <ls_upload_content>-acac_objtype AND
              ls_upload_content_pre-acac_objnumber = <ls_upload_content>-acac_objnumber ).
        lb_new_object = abap_false.
      ELSE.
        lb_new_object = abap_true.
      ENDIF.

      "All initialization for a single object should happen only once
      IF lb_new_object = abap_true.

        REFRESH lt_upload_content.
        CLEAR: lv_bukrs,  lv_accrual_type, lv_object_number .

        lv_bukrs          = <ls_upload_content>-bukrs.
        lv_accrual_type   = <ls_upload_content>-acac_objtype.
        lv_object_number  = <ls_upload_content>-acac_objnumber.
        IF strlen( <ls_upload_content>-acac_objnumber ) > 2
              AND <ls_upload_content>-acac_objnumber+0(2) = '$$'.
          CLEAR lv_object_number.
          ENDIF.

*      start the process of create or update accrual object
*      it will terminate the process once error level message found
          lo_ace_mdo_subobj = cl_ace_mdo_subobj=>get_subobj_obj(
            iv_bukrs = lv_bukrs
            iv_component = lv_comp ).

          CHECK lo_ace_mdo_subobj IS BOUND.
          lo_acac_mdo_subobj ?= lo_ace_mdo_subobj.

          CLEAR lv_keydate.
          lv_keydate = <ls_upload_content>-keydate.
          lo_ace_mdo_subobj->set_detdate( id_detdate = lv_keydate ).

        TRY .
          " if any error raised it will be collected and loop will go to next iteration
          " initialize the accrual object
          initialize_acc_obj(
            EXPORTING
              iv_comp              = lv_comp                 " Accrual Engine Application Component
              iv_bukrs             = lv_bukrs                " Company Code
              iv_accrual_type      = lv_accrual_type         " Accrual Object Category for Manual Accruals
              iv_object_number     = lv_object_number        " Object Number of an Accrual Object
*              ib_have_objectnumber = lb_have_objectnumber
              iv_overwrite_flag    = <ls_upload_content>-overwrite_allowed       " New Input Values
            CHANGING
              co_ace_mdo_subobj = lo_ace_mdo_subobj        " Master Data Object for Accruals at Runtime
           ).
          CATCH BEFORE UNWIND cx_ace INTO lx_ace.
            cl_ace_message_handler=>get_instance( )->add_message_inst( lx_ace ).
            IF cl_ace_message_handler=>get_instance( )->get_is_resumable( ) EQ abap_true.
              RESUME.
            ENDIF.
        ENDTRY.

        " Manual Accrual Object header
        lo_acac_mdo_subobj->acac_header_set(
          EXPORTING
            id_objtype   =   lv_accrual_type               " Accrual Engine Application Component
            id_objnumber =   lv_object_number              " Object Number of an Accrual Object
            id_text      =   <ls_upload_content>-text      " Descriptive Text for Accrual Objects
            id_resperson =   <ls_upload_content>-acac_resperson
          ).
        ENDIF.

      ls_upload_content_pre = <ls_upload_content>.

      "------START processing for single line belongs to one accrual object ------

      TRY.

*          prepare to update items
          IF <ls_upload_content>-type EQ acacc_upload_option-total.

            align_items_in_subobj(
              EXPORTING
                is_upload_content = <ls_upload_content>                 " Upload Excel Content for S4
                io_ace_mdo_subobj  = lo_ace_mdo_subobj
              IMPORTING
                et_accrual_item   = DATA(lt_accrual_item)                 " Accrual Object Item (External)
            ).

            lo_ace_mdo_subobj->items_modify(
              EXPORTING
                id_datefrom = lv_keydate
                it_items_ti = lt_accrual_item
             ).
          ENDIF.

*          prepare to update period data
*          but with periodic template, accrual method may be ignored, so the method need to fill in by default.
          IF <ls_upload_content>-type EQ acacc_upload_option-period.

            fill_default_value_into_item(
              EXPORTING
                io_ace_subobj = lo_ace_mdo_subobj
*                iv_rldnr      = lv_rldnr
              CHANGING
                cs_upload_content = <ls_upload_content> ).

            align_items_in_subobj(
              EXPORTING
                is_upload_content = <ls_upload_content>                 " Upload Excel Content for S4
                io_ace_mdo_subobj  = lo_ace_mdo_subobj
              IMPORTING
                et_accrual_item   = lt_accrual_item                 " Accrual Object Item (External)
            ).

            lo_ace_mdo_subobj->items_modify(
              EXPORTING
                id_datefrom = lv_keydate
                it_items_ti = lt_accrual_item
             ).

            READ TABLE <ls_upload_content>-ace_uploaded_accr_amounts_t INTO DATA(ls_uploaded_accr_amounts) INDEX 1.
            IF sy-subrc = 0.
              CLEAR: ls_upld_perdata_itemtype, lt_upld_perdata_itemtype.
              MOVE-CORRESPONDING ls_uploaded_accr_amounts TO ls_upld_perdata_itemtype.
              ls_upld_perdata_itemtype-ledger = ls_uploaded_accr_amounts-rldnr.
              APPEND ls_upld_perdata_itemtype TO lt_upld_perdata_itemtype.

              READ TABLE lo_ace_mdo_subobj->mt_items INTO DATA(ls_item) WITH TABLE KEY itemtype = ls_uploaded_accr_amounts-itemtype.
              CALL METHOD ls_item-o_item->perioddata_upload( it_uploaded_per_amounts = lt_upld_perdata_itemtype ).

              " propose the period data if the review is activited.
              cl_ace_cfg_db_buffer=>get_review_activate_curtyp(
                EXPORTING
                  id_comp                      = acacc_comp                          " Accrual Engine Application Component
                  id_itemtype                  = ls_uploaded_accr_amounts-itemtype   " Accrual Item Type
                  id_bukrs                     = <ls_upload_content>-bukrs           " Company Code
                  id_rldnr                     = ls_uploaded_accr_amounts-rldnr      " Ledger in General Ledger Accounting
                IMPORTING
                  eb_review_active            =  DATA(lb_review_type)                " Review Process for Periodic Accrual Amounts is Active
              ).
              IF lb_review_type EQ abap_true.
                READ TABLE ls_item-o_item->mt_item_per INTO DATA(ls_item_per) WITH KEY rldnr = ls_uploaded_accr_amounts-rldnr.
                ls_item_per-o_item_per->propose(
                  EXPORTING
                    id_period_det_date = <ls_upload_content>-keydate                              " Period Determination Date
                    id_review_cat      = if_ace_mdo_types=>itemtype_category-normal_itemtype
                    id_activity        = if_ace_mdo_types=>act_propose
                    ib_auth_check      = abap_true
                ).
              ENDIF.
            ENDIF.
          ENDIF.

          CLEAR: ls_acesobj_assgmt, lt_acesobj_param.
          " prepare to update assignments
          IF <ls_upload_content>-acesobj_assgmt IS NOT INITIAL.
            MOVE-CORRESPONDING <ls_upload_content>-acesobj_assgmt TO ls_acesobj_assgmt.
            lo_ace_mdo_subobj->assignments_modify(
                id_datefrom    = lv_keydate
                id_testrun     = space
                is_assignments = ls_acesobj_assgmt
            ).
          ENDIF.

          " update parameters
          IF <ls_upload_content>-acesobj_param_t IS NOT INITIAL.
            lt_acesobj_param = <ls_upload_content>-acesobj_param_t.
            lo_ace_mdo_subobj->parameters_modify(
                id_datefrom = lv_keydate
                it_params   = lt_acesobj_param ).
          ENDIF.

          " if opening posting is required, posting paramenter need to be generated
          IF lo_ace_mdo_subobj->get_inc_posting_required( ) EQ 'X'.

            IF <ls_upload_content>-overwrite_allowed NE 'X'.
              READ TABLE <ls_upload_content>-acesobj_item_t ASSIGNING FIELD-SYMBOL(<ls_acesobj_item>) INDEX 1.
              lv_posting_keydate = <ls_acesobj_item>-life_start_date.
            ELSE.
              lv_posting_keydate = <ls_upload_content>-keydate.
            ENDIF.

            ls_ace_posting_params-bldat = lv_posting_keydate.
            ls_ace_posting_params-budat = lv_posting_keydate.
            CALL FUNCTION 'FI_PERIOD_DETERMINE'
              EXPORTING
                i_budat       = lv_posting_keydate
                i_bukrs       = <ls_upload_content>-bukrs
              IMPORTING
                e_gjahr       = ls_ace_posting_params-gjahr
                e_poper       = ls_ace_posting_params-poper
              EXCEPTIONS
                error_message = 1
                OTHERS        = 2.
            IF sy-subrc NE 0.
              RAISE RESUMABLE EXCEPTION TYPE cx_ace
                MESSAGE ID 'ACAC_EXCEL'
                TYPE 'E'
                NUMBER '033'.
            ENDIF.
          ENDIF.

        CATCH BEFORE UNWIND cx_ace INTO lx_ace.
          cl_ace_message_handler=>get_instance( )->add_message_inst( lx_ace ).
          IF cl_ace_message_handler=>get_instance( )->get_is_resumable( ) EQ abap_true.
            RESUME.
          ENDIF.
      ENDTRY.

      "Collecting lines for a single accrual object
      INSERT <ls_upload_content> INTO TABLE lt_upload_content.

      "------END processing for single line belongs to one accrual object ------


      AT END OF acac_objnumber.
        lb_end_of_new_obj = abap_true. "Used to determine whether current line is the last one of an accrual object
      ENDAT.

      "How to determine that the current line is the last one of an accrual object
      "1. The accrual object number is empty
      "2. Determined by statement AT END OF acac_objnumber (Flag lb_end_of_new_obj = 'X')
      "If it's last line of an accrual object, save this object and process the messages
      IF <ls_upload_content>-acac_objnumber IS INITIAL OR lb_end_of_new_obj = abap_true.
        TRY .
            lo_ace_mdo_subobj->save(
            EXPORTING
              id_savemode = if_ace_mdo_types=>cv_savemode_commit
              id_comp_specific_data_source = if_ace_mdo_types=>ty_comp_specific_data_source-source_propose
              ib_cobl_check = abap_true
              is_posting_params = ls_ace_posting_params
           ).
          CATCH BEFORE UNWIND cx_ace INTO lx_ace.
          cl_ace_message_handler=>get_instance( )->add_message_inst( lx_ace ).
          IF cl_ace_message_handler=>get_instance( )->get_is_resumable( ) EQ abap_true.
            RESUME.
          ENDIF.
      ENDTRY.

          IF sy-subrc EQ 0.
          LOOP AT lt_upload_content ASSIGNING FIELD-SYMBOL(<ls_upload_content_2>).
            IF lv_object_number IS INITIAL.
              <ls_upload_content_2>-acac_objnumber = lo_acac_mdo_subobj->ms_acac_obj_header-acac_objnumber.
            ENDIF.
          ENDLOOP.
          ENDIF.

**      Collect the message and check whether error happened. if yes, terminate process and go to next entry
*          me->collect_upload_result(
*            EXPORTING
**            it_return         = lt_messages                 " Return parameter table
*              ib_last           = abap_true                 " Incicate whether last time to check
*            IMPORTING
*              eb_error_occurred = lb_error_occurred
*            CHANGING
*              ct_upload_content = lt_upload_content                 " Upload Excel Content for S4
*              ct_upload_result  = et_upload_result                 " Upload Result Worklist
*          ).
          APPEND LINES OF lt_upload_content TO lt_upload_content_all.
        ENDIF.

        "Reset it at end of each line processing
        lb_end_of_new_obj = abap_false.
      ENDIF.
    ENDLOOP.

* Collect the message and check whether error happened. if yes, terminate process and go to next entry
    me->collect_upload_result(
      EXPORTING
*       it_return         = lt_messages               " Return parameter table
            ib_last           = abap_true                 " Incicate whether last time to check
        IMPORTING
          eb_error_occurred = lb_error_occurred
        CHANGING
        ct_upload_content = lt_upload_content_all     " Upload Excel Content for S4
        ct_upload_result  = et_upload_result          " Upload Result Worklist
    ).

      IF lb_error_occurred = abap_true.
        eb_error_occurred = abap_true.
      ENDIF.


    ct_upload_content = lt_upload_content_all.

  ENDMETHOD.


  METHOD PARSE_XLSX_MAP_FIELDS.

    DATA: lo_xlsx_parser             TYPE REF TO cl_acac_s4_xlsx_parse_util,
          lv_file_string             TYPE xstring,
          lt_xlsx_table_original     TYPE fac_t_excel_data,
          lt_xlsx_table              TYPE acac_s4_index_value_pair_t,
          ls_structure               TYPE acac_struc_fieldname_pair,
          lt_structure               TYPE acac_struc_fieldname_pair_t,
          lt_excel_data_type_msg     TYPE bapiret2_t,
          lt_excel_accr_specific_msg TYPE acac_accrual_message_t,
          ls_excel_accr_specific_msg TYPE acac_accrual_message,
          ld_field_exist             TYPE flag,
          ld_field_name              TYPE string,
          ld_invalid_field           TYPE flag,
          ld_ret_code                TYPE i,
          ld_index                   TYPE i VALUE 1.

    FIELD-SYMBOLS:
      <fs_upload_content>   TYPE acac_s4_upload_content,
      <fs_excel_value_line> TYPE acac_s4_index_value_pair.

    " initailize the global variables ( Parameter fields, Sensitive, Currency )
    initial_global_variant( ).

    " structure name to included field names
    lt_structure = VALUE #( ( name = gc_common_excel_fields )
                            ( name = gd_param_structure ) ).

    CREATE OBJECT lo_xlsx_parser.

    " add the field information to every field cells
    CALL METHOD lo_xlsx_parser->describe_structures
      CHANGING
        ct_structure = lt_structure.

    " transform xstring to recognized data
    TRY .
        CALL METHOD lo_xlsx_parser->transform_xstring_2_tab
          EXPORTING
            ix_file  = iv_xfile
          IMPORTING
            et_table = lt_xlsx_table_original.
      CATCH cx_openxml_not_found cx_openxml_format INTO DATA(lo_execl_parsing_exception).
        RAISE EXCEPTION TYPE cx_acac_xlsx_exception
          MESSAGE ID 'ACAC_EXCEL'
          TYPE 'E'
          NUMBER '003'.
    ENDTRY.

    " some fields need to be replaced with relevant external fields
    cl_acac_s4_xlsx_template=>get_extint_mapping(
      IMPORTING et_ext_int_mapping = gt_external_fields
    ).

    " check the layout of excel file and delete the label row
    " ct_table data structure:
    " index            value
    " 1                internal table       A     BUKRS
    " 1                internal table       B     ACAC_OBJTYPE
    " ...
    " 2                internal table       A     1000
    " 2                internal table       B     WORKORDER
    " ...
    CALL METHOD lo_xlsx_parser->check_excel_layout
      CHANGING
        ct_table = lt_xlsx_table_original.

    lt_xlsx_table = CORRESPONDING #( BASE ( lt_xlsx_table ) lt_xlsx_table_original ).

    " mapping excel data to structure and check excel content
    LOOP AT lt_xlsx_table ASSIGNING <fs_excel_value_line>.
      " skip the field line, implictly defined rule that the first line always the fields
      " first line means the first line of the excel content
      " doesn't means the first line of excel
      " ------------ EXCEL ------------
      " the first line is deleted in fact in checking layout function, so the first line is fields line currently
      " line 1: line of Fields   -> this line means the first line of excel content
      " line 2: accruals content
      IF ld_index EQ 1.
        " validate the fields in excel file
        CALL METHOD validate_fields
          EXPORTING
            is_excel_value_line = <fs_excel_value_line>
            it_structure        = lt_structure.
        IF sy-subrc EQ 0.
          " just hard code to skip the first line
          ld_index = 2.
          CONTINUE.
        ENDIF.
      ENDIF.

      " ld_index = 2 means to proceed mapping the content to structure
      " this method will map the data to the structure saving API for each line in excel file
      CALL METHOD map_excel_to_acac_structure
        EXPORTING
          it_structure      = lt_structure
          iv_row_index      = CONV i( <fs_excel_value_line>-index )
        CHANGING
          ct_upload_content = ct_upload_content
          cs_excel_data     = <fs_excel_value_line>.
    ENDLOOP.
  ENDMETHOD.


  method PREFETCH_ACCRUAL_OBJECTS.
  endmethod.


  METHOD RETURN_CONSISTENT_UPLD_CONTENT.

    DATA: lt_pre_upload_content TYPE acac_s4_upload_content_t,
          lv_max_keydate        TYPE ace_effdate.

    lt_pre_upload_content = ct_upload_content.
*    for create a new accrual object
*    IF is_accrual_obj-acac_objnumber IS INITIAL.
*      APPEND cs_excel_data-upload_content TO ct_upload_content.
*    ELSE.
*      if it is existed, check whether consisitant and add the items or period data to internal table.
*      if not, raise error message
    " check whether same accrual object's information is consistency or not
    LOOP AT lt_pre_upload_content ASSIGNING FIELD-SYMBOL(<ls_upload_content>)
      WHERE bukrs = is_accrual_obj-bukrs AND
      acac_objtype = is_accrual_obj-acac_objtype AND
      acac_objnumber = is_accrual_obj-acac_objnumber AND
      acac_objnumber <> ''.
*        AT NEW keydate.
      CALL METHOD check_is_accr_data_consistency
        EXPORTING
          is_accrual_obj    = is_accrual_obj
          is_obj_header     = cs_excel_data-upload_content-acesobj
          is_assignment     = cs_excel_data-upload_content-acesobj_assgmt
          it_amounts        = cs_excel_data-upload_content-ace_uploaded_accr_amounts_t
          it_obj_item       = cs_excel_data-upload_content-acesobj_item_t
          it_parameters     = cs_excel_data-upload_content-acesobj_param_t
        CHANGING
          cs_upload_content = <ls_upload_content>.
*        ENDAT.
*        IF lv_max_keydate < <ls_upload_content>-keydate.
*          lv_max_keydate = <ls_upload_content>-keydate.
*        ENDIF.
    ENDLOOP.
*      IF sy-subrc <> 0.
    APPEND cs_excel_data-upload_content TO ct_upload_content.
*      ELSE.
**        check the keydate, if it is more than the current keydate, add the entry
*        IF lv_max_keydate < cs_excel_data-upload_content-keydate.
*          APPEND cs_excel_data-upload_content TO ct_upload_content.
*        ENDIF.
*      ENDIF.
*    ENDIF.

  ENDMETHOD.


  METHOD SET_AMOUNT_CURR_MAP.
    gt_amount_curr_map = VALUE #( ( amount_name = 'TOTAL_ACCR_AMNT_WSL' type = acacc_upload_option-total curr_name = 'RWCUR' )
                                  ( amount_name = 'TOTAL_ACCR_AMNT_HSL' type = acacc_upload_option-total curr_name = 'RHCUR' )
                                  ( amount_name = 'TOTAL_ACCR_AMNT_KSL' type = acacc_upload_option-total curr_name = 'RKCUR' )
                                  ( amount_name = 'TOTAL_ACCR_AMNT_OSL' type = acacc_upload_option-total curr_name = 'ROCUR' )
                                  ( amount_name = 'TOTAL_ACCR_AMNT_VSL' type = acacc_upload_option-total curr_name = 'RVCUR' )
                                  ( amount_name = 'TOTAL_ACCR_AMNT_BSL' type = acacc_upload_option-total curr_name = 'RBCUR' )
                                  ( amount_name = 'TOTAL_ACCR_AMNT_CSL' type = acacc_upload_option-total curr_name = 'RCCUR' )
                                  ( amount_name = 'TOTAL_ACCR_AMNT_DSL' type = acacc_upload_option-total curr_name = 'RDCUR' )
                                  ( amount_name = 'TOTAL_ACCR_AMNT_ESL' type = acacc_upload_option-total curr_name = 'RECUR' )
                                  ( amount_name = 'TOTAL_ACCR_AMNT_FSL' type = acacc_upload_option-total curr_name = 'RFCUR' )
                                  ( amount_name = 'TOTAL_ACCR_AMNT_GSL' type = acacc_upload_option-total curr_name = 'RGCUR' )
                                  ( amount_name = 'UPLOADED_ACCR_AMNT_WSL' type = acacc_upload_option-period curr_name = 'RWCUR' )
                                  ( amount_name = 'UPLOADED_ACCR_AMNT_HSL' type = acacc_upload_option-period curr_name = 'RHCUR' )
                                  ( amount_name = 'UPLOADED_ACCR_AMNT_KSL' type = acacc_upload_option-period curr_name = 'RKCUR' )
                                  ( amount_name = 'UPLOADED_ACCR_AMNT_OSL' type = acacc_upload_option-period curr_name = 'ROCUR' )
                                  ( amount_name = 'UPLOADED_ACCR_AMNT_VSL' type = acacc_upload_option-period curr_name = 'RVCUR' )
                                  ( amount_name = 'UPLOADED_ACCR_AMNT_BSL' type = acacc_upload_option-period curr_name = 'RBCUR' )
                                  ( amount_name = 'UPLOADED_ACCR_AMNT_CSL' type = acacc_upload_option-period curr_name = 'RCCUR' )
                                  ( amount_name = 'UPLOADED_ACCR_AMNT_DSL' type = acacc_upload_option-period curr_name = 'RDCUR' )
                                  ( amount_name = 'UPLOADED_ACCR_AMNT_ESL' type = acacc_upload_option-period curr_name = 'RECUR' )
                                  ( amount_name = 'UPLOADED_ACCR_AMNT_FSL' type = acacc_upload_option-period curr_name = 'RFCUR' )
                                  ( amount_name = 'UPLOADED_ACCR_AMNT_GSL' type = acacc_upload_option-period curr_name = 'RGCUR' ) ).
  ENDMETHOD.


  METHOD SET_AMOUNT_CURR_VALUE.
    DATA: ls_amount_curr_map  TYPE ty_amount_curr_map,
          ls_amount_curr      TYPE ty_amount_curr,
          ls_tcurx            TYPE tcurx,
          lv_value_at_currdec TYPE i,
          lv_sign             TYPE char1,
          lv_input            TYPE decfloat16.
    FIELD-SYMBOLS: <fs_amount_curr> TYPE ty_amount_curr.

    " deal with amount
    READ TABLE gt_amount_curr_map WITH KEY amount_name = iv_fieldname INTO ls_amount_curr_map.
    IF sy-subrc EQ 0.
      ls_amount_curr-amount_name = iv_fieldname.
      ls_amount_curr-curr_name = ls_amount_curr_map-curr_name.
      ls_amount_curr-amount_value = iv_value.
      ls_amount_curr-type = ls_amount_curr_map-type.
      APPEND ls_amount_curr TO ct_amount_curr.

      ev_amount = abap_true.
    ENDIF.

    " deal with currency
    READ TABLE gt_amount_curr_map WITH KEY curr_name = iv_fieldname INTO ls_amount_curr_map.
    IF sy-subrc EQ 0.
      READ TABLE gt_currency_configuration WITH KEY currkey = iv_value INTO ls_tcurx.
      IF sy-subrc NE 0.
        " in case currency is not configured in customizing, set 2 as default
        ls_tcurx-currdec = 2.
      ENDIF.
      " round the amount value
      LOOP AT ct_amount_curr ASSIGNING <fs_amount_curr> WHERE round_amount IS INITIAL AND curr_name = iv_fieldname.
        <fs_amount_curr>-curr_value = iv_value.
        <fs_amount_curr>-decimal = ls_tcurx-currdec.
        TRY.
            lv_input = <fs_amount_curr>-amount_value.

            CALL FUNCTION 'ROUND'
              EXPORTING
                decimals      = ls_tcurx-currdec
                input         = lv_input
                sign          = 'X'
              IMPORTING
                output        = <fs_amount_curr>-round_amount
              EXCEPTIONS
                input_invalid = 1
                overflow      = 2
                type_invalid  = 3
                OTHERS        = 4.

            IF sy-subrc NE 0.
              RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
                MESSAGE ID 'ACAC_EXCEL'
                TYPE 'E'
                NUMBER '019'
                WITH iv_row_index <fs_amount_curr>-amount_name.
            ENDIF.
          CATCH cx_sy_conversion_no_number INTO DATA(lv_message).
            RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
              MESSAGE ID 'ACAC_EXCEL'
              TYPE 'E'
              NUMBER '019'
              WITH iv_row_index <fs_amount_curr>-amount_name.
        ENDTRY.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD SET_CASE_SENSITIVE_FIELDS.
    gt_fields_case_sensitive = VALUE #( ( 'TEXT' ) ).
  ENDMETHOD.


  METHOD SET_FINAL_MESSAGE.
    DATA: lt_return       TYPE bapiret2_t,
          lt_final_return TYPE bapiret2_t,
          lb_success_flag TYPE abap_bool VALUE abap_true.

    LOOP AT ct_upload_content ASSIGNING FIELD-SYMBOL(<ls_upload_content>).
      READ TABLE <ls_upload_content>-ace_accdoc_return_t WITH KEY type = 'E' TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        lb_success_flag = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.

    LOOP AT ct_upload_content ASSIGNING <ls_upload_content>.
      CLEAR: lt_return, lt_final_return.
      READ TABLE ct_upload_result ASSIGNING FIELD-SYMBOL(<ls_upload_result_2>) INDEX sy-tabix.

      READ TABLE <ls_upload_content>-ace_accdoc_return_t WITH KEY type = 'E' TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
        IF lb_success_flag EQ abap_true.
          MESSAGE s027(acac_excel) INTO sy-lisel
            WITH <ls_upload_content>-bukrs <ls_upload_content>-acac_objtype <ls_upload_content>-acac_objnumber.
          <ls_upload_result_2>-status_icon = icon_led_green.
        ELSE.
          MESSAGE w031(acac_excel) INTO sy-lisel
            WITH <ls_upload_content>-bukrs <ls_upload_content>-acac_objtype <ls_upload_content>-acac_objnumber.
          <ls_upload_result_2>-status_icon = icon_led_yellow.
        ENDIF.
        cl_ace_generic_services=>save_msg_to_return(
          EXPORTING
            ib_use_syst = abap_true
          CHANGING
            ct_return   = lt_return
        ).
        APPEND LINES OF lt_return TO <ls_upload_content>-ace_accdoc_return_t.
      ELSE.
        " write back the temporary objcet number if contains error in the same one object
        <ls_upload_result_2>-status_icon = icon_led_red.
        READ TABLE gt_temp_created_objnums INTO DATA(ls_temp_created_objnums) WITH KEY temp_num = <ls_upload_content>-acac_objnumber.
        IF sy-subrc EQ 0.
          LOOP AT ct_upload_result ASSIGNING FIELD-SYMBOL(<ls_upload_result>) WHERE acac_objnumber = ls_temp_created_objnums-created_num.
            <ls_upload_result>-acac_objnumber = ls_temp_created_objnums-temp_num.
          ENDLOOP.
        ENDIF.
      ENDIF.

      " remove the dupulicated error message
      LOOP AT <ls_upload_content>-ace_accdoc_return_t INTO DATA(ls_ace_accdoc_return).
        IF NOT line_exists( lt_final_return[
            id = ls_ace_accdoc_return-id
            number = ls_ace_accdoc_return-number
            message_v1 = ls_ace_accdoc_return-message_v1
            message_v2 = ls_ace_accdoc_return-message_v2
            message_v3 = ls_ace_accdoc_return-message_v3
            message_v4 = ls_ace_accdoc_return-message_v4 ] ).
          APPEND ls_ace_accdoc_return TO lt_final_return.
        ENDIF.
      ENDLOOP.
      CLEAR <ls_upload_content>-ace_accdoc_return_t.
      APPEND LINES OF lt_final_return TO <ls_upload_content>-ace_accdoc_return_t.
    ENDLOOP.
  ENDMETHOD.


  METHOD SET_PROCESSED_COMPANY_CODE.
    READ TABLE gt_processed_company_code WITH KEY bukrs = iv_bukrs TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      APPEND iv_bukrs TO gt_processed_company_code.
    ENDIF.
  ENDMETHOD.


  METHOD STORE_MSG_IN_RETURN.
    DATA:
      ls_syst_msg TYPE  ace_syst_msg_fields,
      ls_return   TYPE  bapiret2.

    MOVE-CORRESPONDING   syst  TO  ls_syst_msg.

* convert message into BAPI structure:
    CALL FUNCTION 'ACE_MSG_TO_RETURN_CONVERT'
      EXPORTING
        is_syst_msg_fields = ls_syst_msg
      IMPORTING
        es_return          = ls_return.

    APPEND  ls_return  TO  ct_return.
  ENDMETHOD.


  METHOD VALIDATE_FIELDS.

    DATA: ld_field_exist   TYPE flag,
          ld_field_name    TYPE string,
          ld_invalid_field TYPE flag.

*    Duplication check
    " get the all excel fields from excel upload file, and check whether duplicated column
    CALL METHOD get_excel_fields
      EXPORTING
        is_excel_data  = is_excel_value_line
      CHANGING
        cv_field_exist = ld_field_exist
        cv_field_name  = ld_field_name.
    IF ld_field_exist EQ abap_true.
      " duplicated
      RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
        MESSAGE ID 'ACAC_EXCEL'
        TYPE 'E'
        NUMBER '004'
        WITH ld_field_name.
    ELSE.

*    Exist check
      " try to find invalid field and mark the parameter field at the same time
      CALL METHOD check_invalid_field
        EXPORTING
          it_structure = it_structure
        CHANGING
          cv_invalid   = ld_invalid_field
          cv_field     = ld_field_name.
      IF ld_invalid_field EQ abap_true.
        " invalid
        RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
          MESSAGE ID 'ACAC_EXCEL'
          TYPE 'E'
          NUMBER '015'
          WITH ld_field_name.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
