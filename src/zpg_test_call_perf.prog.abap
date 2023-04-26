*&---------------------------------------------------------------------*
*& Report ZPG_TEST_CALL_PERF
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_call_perf.
DATA  :gd_keydate          TYPE ace_effdate,
       gs_input_parameters TYPE ace_user_interface_s4,
       lo_mdo_subobj_item  TYPE REF TO cl_ace_mdo_subobj_item,
       iv_rldnr            TYPE fins_ledger,
       iv_keydate          TYPE ace_effdate,
       gs_item             TYPE acesobj_item.
DATA: lt_period            TYPE if_ace_mdo_types=>tt_calculated_values.
DATA: lo_mdo_subobj      TYPE REF TO if_ace_mdo_subobj,
      ls_subobj_item_ref TYPE if_ace_mdo_types=>ty_subobj_item_ref.
DATA:  ls_mdo_subobj TYPE ace_ui_mdo_subobj.
DATA :lt_crcy_field    TYPE cl_ace_cfg_db_buffer=>tt_visible_crcy_field,
      lt_amount_prefix TYPE ace_fieldname_t,
      ls_period_alv    TYPE aceexpl_cal_val,
      lt_period_alv    TYPE TABLE OF aceexpl_cal_val.
DATA : lt_visible_crcy_field  TYPE cl_ace_cfg_db_buffer=>tt_visible_crcy_field,
       lt_editable_crcy_field TYPE cl_ace_cfg_db_buffer=>tt_visible_crcy_field,
       lt_hide_crcy_field     TYPE cl_ace_cfg_db_buffer=>tt_visible_crcy_field.
DATA : cs_life_info  TYPE ace_subobj_life_info.

FIELD-SYMBOLS: <fs_period_alv>        TYPE aceexpl_cal_val,
               <fs_period_alv_before> TYPE aceexpl_cal_val.
gd_keydate  = sy-datum.
gs_input_parameters-comp = 'ACAC'.
gs_input_parameters-bukrs = '1000'.
gs_input_parameters-ref_key = '0RENTING  0000000000000000000045'.
gs_input_parameters-ref_subkey = '1'.
gs_input_parameters-itemtype = 'COSTS'.
gs_input_parameters-rldnr = '0L'.
gs_input_parameters-date_to = '99991231'.
gs_input_parameters-date_from = '00010101'.

APPEND 'BAL_' TO lt_amount_prefix.
APPEND 'ACT_' TO lt_amount_prefix.
APPEND 'CUM_' TO lt_amount_prefix.

*ls_mdo_subobj-comp = gs_input_parameters-comp.
*ls_mdo_subobj-bukrs = gs_input_parameters-bukrs.
*ls_mdo_subobj-ref_key = gs_input_parameters-ref_key.
*ls_mdo_subobj-ref_subkey = gs_input_parameters-ref_subkey.
ls_mdo_subobj-o_subobj = cl_ace_mdo_subobj=>get_subobj_obj(
  iv_bukrs     = gs_input_parameters-bukrs
  iv_component = gs_input_parameters-comp
).

ls_mdo_subobj-o_subobj->initialize_from_db_buffer(
  EXPORTING
    iv_ref_key    = gs_input_parameters-ref_key
    iv_ref_subkey = gs_input_parameters-ref_subkey
).
ls_mdo_subobj-o_subobj->get_subobj_life_date( IMPORTING ev_vality_from = cs_life_info-vality_from
                                             ev_vality_to   = cs_life_info-vality_to ).


* get life cycle status

ls_mdo_subobj-o_subobj->get_life_cycle_status( IMPORTING ev_status        = cs_life_info-lc_status
                                                ev_premtre_fdate = cs_life_info-premtre_fdate ).
*INSERT ls_mdo_subobj INTO gt_mdo_subobj INDEX lv_index.
lo_mdo_subobj = ls_mdo_subobj-o_subobj.
READ TABLE lo_mdo_subobj->mt_items INTO ls_subobj_item_ref WITH TABLE KEY itemtype = gs_input_parameters-itemtype.
IF sy-subrc <> 0.
  RETURN.
ENDIF.

lo_mdo_subobj_item = CAST cl_ace_mdo_subobj_item( ls_subobj_item_ref-o_item ).
*PERFORM return_mdo_subobj_item IN PROGRAM  saplace_user_interface_s4 USING gs_input_parameters
*                                     gd_keydate
*                               CHANGING lo_mdo_subobj_item .
iv_keydate = gd_keydate.
iv_rldnr = '0L'.
lo_mdo_subobj_item->if_ace_mdo_subobj_item~get_item_by_ledger(
  EXPORTING
    iv_ledger       = iv_rldnr
    iv_detdate      = iv_keydate
  IMPORTING
    es_acesobj_item = gs_item
).
"
cl_ace_cfg_db_buffer=>get_visible_currency(
  EXPORTING
    id_comp                = gs_item-comp
    id_itemtype            = gs_item-itemtype
    id_bukrs               = gs_item-bukrs
    id_rldnr               = gs_item-rldnr
  IMPORTING
    et_visible_crcy_fields = lt_visible_crcy_field
    et_editable_crcy_fields = lt_editable_crcy_field
    et_cal_hide_crcy_fields = lt_hide_crcy_field
  EXCEPTIONS
    not_found              = 1                " Not Found
    OTHERS                 = 2 ).

APPEND LINES OF lt_visible_crcy_field TO lt_crcy_field.
APPEND LINES OF lt_editable_crcy_field TO lt_crcy_field.
APPEND LINES OF lt_hide_crcy_field TO lt_crcy_field.
"
lo_mdo_subobj_item->if_ace_mdo_subobj_item~get_calculated_values(
EXPORTING
  iv_rldnr = gs_item-rldnr
RECEIVING
  rt_values = lt_period
).
LOOP AT lt_period INTO DATA(ls_period).

  ls_period_alv-itemtype          = gs_item-itemtype.
  ls_period_alv-rldnr             = gs_item-rldnr.
  ls_period_alv-ldgrp             = ls_period-ldgrp.
  ls_period_alv-period_start_date = ls_period-date_from.
  ls_period_alv-period_end_date   = ls_period-date_to.

  cl_ace_generic_services=>values_column_to_row(
  EXPORTING
    it_column_values      = ls_period-t_values
    iv_bukrs              = gs_item-bukrs
    iv_rldnr              = gs_item-rldnr
    iv_val_prefix         = 'BAL_'
    iv_cur_postfix        = ''
  CHANGING
    cs_row_values         = ls_period_alv ).

  cl_ace_mdo_services=>cal_calculated_bal_amnt_multi(
    EXPORTING
      it_crcy_field      = lt_crcy_field
      iv_period_end_date = ls_period-date_to
      it_amount_prefix   = lt_amount_prefix
      is_item            = gs_item
    CHANGING
    cs_period_alv      = ls_period_alv ).

  APPEND ls_period_alv TO lt_period_alv.
ENDLOOP.

cl_ace_generic_services=>values_column_to_row(
  EXPORTING
    it_column_values      = ls_period-t_values
    iv_bukrs              = gs_item-bukrs
    iv_rldnr              = gs_item-rldnr
    iv_val_prefix         = 'BAL_'
    iv_cur_postfix        = ''
  CHANGING
    cs_row_values         = ls_period_alv ).

cl_ace_mdo_services=>cal_calculated_bal_amnt_multi(
  EXPORTING
    it_crcy_field      = lt_crcy_field
    iv_period_end_date = ls_period-date_to
    it_amount_prefix   = lt_amount_prefix
    is_item            = gs_item
  CHANGING
  cs_period_alv      = ls_period_alv ).

SORT lt_period_alv BY period_start_date ASCENDING.

LOOP AT lt_period_alv ASSIGNING <fs_period_alv>.
  DATA lv_period_tabix TYPE sy-tabix.
  lv_period_tabix = sy-tabix.
  LOOP AT lt_crcy_field INTO DATA(ls_crcy_field).
    " Amount Calculation

    ASSIGN COMPONENT 'ACT_' && ls_crcy_field-fieldname OF STRUCTURE <fs_period_alv> TO FIELD-SYMBOL(<fs_act_amount>).
    ASSIGN COMPONENT 'BAL_' && ls_crcy_field-fieldname OF STRUCTURE <fs_period_alv> TO FIELD-SYMBOL(<fs_bal_amount>).
    ASSIGN COMPONENT 'CUM_' && ls_crcy_field-fieldname OF STRUCTURE <fs_period_alv> TO FIELD-SYMBOL(<fs_cum_amount>).
*      ASSIGN COMPONENT 'REMAIN_' && ls_visible_crcy_field-fieldname OF STRUCTURE <fs_period_alv> TO FIELD-SYMBOL(<fs_remain_amount>).

    IF <fs_act_amount> IS NOT ASSIGNED OR <fs_bal_amount> IS NOT ASSIGNED OR <fs_cum_amount> IS NOT ASSIGNED. "OR <fs_remain_amount> IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.

    IF lv_period_tabix = 1. "first line
      <fs_act_amount> = <fs_bal_amount>.
    ELSE.
      UNASSIGN <fs_period_alv_before>.
      READ TABLE lt_period_alv ASSIGNING <fs_period_alv_before> INDEX lv_period_tabix - 1.
      IF <fs_period_alv_before> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT 'BAL_' && ls_crcy_field-fieldname OF STRUCTURE <fs_period_alv_before> TO FIELD-SYMBOL(<fs_bal_amount_before>).
      IF <fs_bal_amount_before> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      <fs_cum_amount> = <fs_bal_amount_before>.
      <fs_act_amount> = <fs_bal_amount> - <fs_cum_amount>.

    ENDIF.

*      " Currency Assignment
*      ASSIGN COMPONENT ls_crcy_field-fieldname_curr OF STRUCTURE <fs_period_alv> TO FIELD-SYMBOL(<fs_crcy_show>).
*      ASSIGN COMPONENT ls_crcy_field-fieldname_curr && 'TTL' OF STRUCTURE gs_item TO FIELD-SYMBOL(<fs_crcy_item>).
*      IF <fs_crcy_show> IS ASSIGNED AND <fs_crcy_item> IS ASSIGNED.
*        <fs_crcy_show> = <fs_crcy_item>.
*      ENDIF.

  ENDLOOP.
ENDLOOP.


WRITE :'ok'.
