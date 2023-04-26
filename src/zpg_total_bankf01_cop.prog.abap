*&---------------------------------------------------------------------*
*& Include          ZPG_TOTAL_DETAIL_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data.
DATA: lw_zref2_hd TYPE char16.
DATA: lt_api_total TYPE TABLE OF gty_api010.
DATA: s_belnr TYPE RANGE OF ztb_sophu_log-belnr,
      wa_belnr LIKE LINE OF s_belnr.

TYPES: BEGIN OF gty_total,
        rbukrs TYPE bukrs,
        gjahr TYPE gjahr,
        racct TYPE racct,
        xblnr TYPE xblnr,
        belnr TYPE belnr,
        drcrk TYPE shkzg,
        bldat TYPE dats,
        budat TYPE dats,
        total TYPE fins_vhcur12,
       END OF gty_total.
DATA: gt_total TYPE TABLE OF gty_total.
DATA: lt_total TYPE TABLE OF gty_total.

  SELECT
      api~lino,
      api~ngaygiaodich ,
      api~ngayhachtoan,
      api~soref,
      api~sotk,
      log~belnr,
      log~no_post,
      api~ghico,
      api~ghino
    FROM ztb_api010_input as api
    INNER JOIN ztb_sophu_log as log
    ON log~fico_id_d = api~fico_id
    AND log~stt_d = api~lino
    AND log~budat = api~ngayhachtoan
    INTO TABLE @gt_api
    WHERE sotk IN @s_bankn
    AND ngayhachtoan IN @s_budat.
   SORT gt_api BY soref sotk.

  DELETE gt_api WHERE sotk = ''.
  CHECK gt_api IS NOT INITIAL.

  LOOP AT gt_api INTO DATA(ls_api_total).
    wa_belnr-sign = 'I'.
    wa_belnr-option = 'EQ'.
    wa_belnr-low = ls_api_total-belnr.
    APPEND wa_belnr TO s_belnr.
    COLLECT ls_api_total INTO lt_api_total.
    CLEAR ls_api_total.
  ENDLOOP.
  gt_api[] = lt_api_total[].
  SELECT t012k~bukrs,
         t012~bankl,
         t012k~bankn,
         t012k~hbkid,
         t012k~hkont
    FROM t012k
    INNER JOIN t012
    ON t012~bukrs   = t012k~bukrs
    AND t012~hbkid  = t012k~hbkid
    INTO TABLE @DATA(gt_bank)
    FOR ALL ENTRIES IN @gt_api
    WHERE t012k~bukrs = @p_bukrs
    AND t012k~bankn = @gt_api-sotk
    AND t012~bankl  IN @s_bankl.

  DELETE ADJACENT DUPLICATES FROM s_belnr.
  SELECT ac~rbukrs,
         ac~gjahr,
         ac~racct,
         ac~drcrk,
         ac~bldat,
         ac~budat,
         SUM( ac~hsl ) AS total
    FROM  acdoca AS ac
    INTO CORRESPONDING FIELDS OF TABLE @gt_total
    WHERE ac~rldnr = '0L'
    AND ac~awref_rev = ''
    AND ac~budat IN @s_budat
    AND ac~rbukrs = @p_bukrs
    AND ac~belnr in @s_belnr
    GROUP BY ac~rbukrs, ac~gjahr, ac~racct, ac~drcrk, ac~bldat, ac~budat
    ORDER BY ac~rbukrs, ac~gjahr, ac~racct, ac~bldat, ac~budat ,ac~drcrk.
  LOOP AT gt_api INTO DATA(gs_api).
    READ TABLE gt_bank INTO DATA(gs_bank) WITH KEY bankn = gs_api-sotk.
    IF sy-subrc IS INITIAL.
      gs_data-bukrs = gs_bank-bukrs.
      gs_data-bankl = gs_bank-bankl.
      gs_data-gl_account = gs_bank-hkont.
    ENDIF.
    gs_data-waers = 'VND'.
    gs_data-bankn = gs_api-sotk.
    gs_data-soref = gs_api-soref.
    gs_data-statement_date = gs_api-ngaygiaodich.
    gs_data-posting_date = gs_api-ngayhachtoan.
    gs_data-total_api_h = gs_api-ghico.
    READ TABLE gt_total INTO DATA(gs_total_h) WITH KEY belnr = gs_api-belnr bldat = gs_api-ngaygiaodich budat = gs_api-ngayhachtoan drcrk = 'S'.
    IF sy-subrc IS INITIAL.
      gs_data-total_fico_h = gs_total_h-total * 100.
    ENDIF.
    gs_data-total_api_s = gs_api-ghino.
    READ TABLE gt_total INTO DATA(gs_total_s) WITH KEY belnr = gs_api-belnr bldat = gs_api-ngaygiaodich budat = gs_api-ngayhachtoan drcrk = 'H'.
    IF sy-subrc IS INITIAL.
      gs_data-total_fico_s = gs_total_s-total * 100.
    ENDIF.
    IF gs_data-total_api_h < 0.
      gs_data-total_api_h = gs_data-total_api_h * -1.
    ENDIF.
    IF gs_data-total_api_s < 0.
      gs_data-total_api_s = gs_data-total_api_s * -1.
    ENDIF.
    IF gs_data-total_fico_h < 0.
      gs_data-total_fico_h = gs_data-total_fico_h * -1.
    ENDIF.
    IF gs_data-total_fico_s < 0.
      gs_data-total_fico_s = gs_data-total_fico_s * -1.
    ENDIF.
    gs_data-total_diff_h = gs_data-total_api_h - gs_data-total_fico_h.
    gs_data-total_diff_s = gs_data-total_api_s - gs_data-total_fico_s.
    APPEND gs_data TO gt_data.
    CLEAR: gs_data.
  ENDLOOP.
SORT gt_data by bankn statement_date posting_date.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_alv .
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
  x_layout-colwidth_optimize = 'X'.
  x_layout-info_fieldname    = 'COLOR'.
  x_layout-window_titlebar = gw_title.
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
    t_fieldcat-checkbox = &8.
    t_fieldcat-do_sum = &9.
*    t_fieldcat-ref_tabname = 'GT_DATA'.
    APPEND t_fieldcat.
    CLEAR t_fieldcat.
    END-OF-DEFINITION.

  lm_fieldcat 'BUKRS' 'Company Code' '' '' 'X' '' '' '' ''.
  lm_fieldcat 'BANKL' 'Bank Key' '' '' '' '' '' '' ''.
  lm_fieldcat 'BANKN' 'Bank account' '' '' '' '' '' '' ''.
  lm_fieldcat 'GL_ACCOUNT' 'GL acct' '' '' '' '' '' '' ''.
  lm_fieldcat 'STATEMENT_DATE' 'Statement Date' '' '' 'X' '' '' '' ''.
  lm_fieldcat 'POSTING_DATE' 'Posting Date' '' '' 'X' '' '' '' ''.
  lm_fieldcat 'SOREF' 'SOREF' '' '' 'X' '' '' '' ''.
  lm_fieldcat 'TOTAL_API_H' 'Báo có (dữ liệu Portal gửi)' 'WAERS' '' 'X' '' '' '' 'X'.
  lm_fieldcat 'TOTAL_FICO_H' 'Báo có (dữ liệu SAP hạch toán)' 'WAERS' '' 'X' '' '' '' 'X'.
  lm_fieldcat 'TOTAL_DIFF_H' 'Chênh lệch' 'WAERS' '' 'X' '' '' '' 'X'.
  lm_fieldcat 'TOTAL_API_S' 'Báo nợ (dữ liệu Portal gửi)' 'WAERS' '' 'X' '' '' '' 'X'.
  lm_fieldcat 'TOTAL_FICO_S' 'Báo nợ (dữ liệu SAP hạch toán)' 'WAERS' '' 'X' '' '' '' 'X'.
  lm_fieldcat 'TOTAL_DIFF_S' 'Chênh lệch' 'WAERS' '' 'X' '' '' '' 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DISPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_display_alv .
  DATA: lv_callback_pf_status_set TYPE  slis_formname.

  d_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = d_repid
      is_layout                = x_layout
      it_fieldcat              = t_fieldcat[]
*     i_callback_top_of_page   = 'TOP-OF-PAGE'  "see FORM
      i_callback_pf_status_set = 'SET_STATUS'
      i_callback_user_command  = 'F_USER_COMMAND'
      i_save                   = 'A'
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
ENDFORM.
