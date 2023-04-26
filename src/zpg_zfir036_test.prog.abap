*&---------------------------------------------------------------------*
*& Report ZPG_ZFIR036_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_zfir036_test.

TYPES :BEGIN OF gty_total, "thanhnt
         bp           TYPE bu_partner,
         open_debit   TYPE wertv12,
         open_credit  TYPE wertv12,
         total_debit  TYPE wertv12,
         total_credit TYPE wertv12,
         end_debit    TYPE wertv12,
         end_credit   TYPE wertv12,
         rhcur        TYPE waers,
       END OF gty_total.
DATA :gt_total TYPE TABLE OF gty_total.
DATA :ls_total LIKE line of  gt_total.

SELECT *
   FROM zddl_fir026( p_date = '20210105',
                     p_bukrs = '1000' )
   INTO TABLE @DATA(lt_data)
   WHERE ( am_credit <> '0' OR am_debit <> '0' )
   AND racct LIKE '141%' AND  segment = 'P0000000'
   AND bp like '%14024'or bp LIKE '%18297'.

LOOP AT lt_data INTO DATA(ls_data).
  MOVE-CORRESPONDING ls_data TO ls_total.
  IF ls_data-budat < '20210105'.
    ls_total-open_credit = ls_data-am_credit.
    ls_total-open_debit = ls_data-am_debit.
  ELSEIF ls_data-budat = '20210105'.
    ls_total-total_credit = ls_data-am_credit.
    ls_total-total_debit = ls_data-am_debit.
  ENDIF.
  COLLECT ls_total INTO gt_total.
  CLEAR ls_total.
ENDLOOP.

DATA: lw_open_am TYPE wertv12,
      lw_end_am  TYPE wertv12.
LOOP AT gt_total ASSIGNING FIELD-SYMBOL(<fs_total>).
  lw_open_am = <fs_total>-open_credit + <fs_total>-open_debit.
  IF lw_open_am >= 0.
    CLEAR <fs_total>-open_credit.
    <fs_total>-open_debit = lw_open_am.
  ELSE.
    CLEAR <fs_total>-open_debit.
    <fs_total>-open_credit = lw_open_am.
  ENDIF.
  CLEAR lw_open_am.
  lw_end_am = <fs_total>-open_credit + <fs_total>-open_debit +
  <fs_total>-total_credit + <fs_total>-total_debit.
  IF lw_end_am < 0.
    <fs_total>-end_credit = lw_end_am.
  ELSE.
    <fs_total>-end_debit = lw_end_am.
  ENDIF.
  CLEAR lw_end_am.

  <fs_total>-open_credit = abs( <fs_total>-open_credit ).
*    <lfs_temp>-total_credit = abs( <lfs_temp>-total_credit )." comment by HUNGVT - change sign total_credit
  <fs_total>-total_credit =  <fs_total>-total_credit * -1. "add by HUNGVT - change sign total credit
  <fs_total>-end_credit = abs( <fs_total>-end_credit ).
ENDLOOP.

BREAK-POINT.
