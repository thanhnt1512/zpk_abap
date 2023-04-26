*&---------------------------------------------------------------------*
*& Include ZPG_TOTAL_DETAIL_TOP                     - Report ZPG_TOTAL_DETAIL
*&---------------------------------------------------------------------*
REPORT ZPG_TOTAL_DETAIL.
TABLES: t012, t012k, acdoca.


SELECTION-SCREEN BEGIN OF BLOCK A WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs LIKE t012k-bukrs OBLIGATORY.
  SELECT-OPTIONS: s_bankl FOR t012-bankl.
  SELECT-OPTIONS: s_bankn FOR t012k-bankn.
  SELECT-OPTIONS: s_budat FOR acdoca-budat.
SELECTION-SCREEN END OF BLOCK A.

TYPES: BEGIN OF gty_data,
        bukrs TYPE bukrs,
        bankl TYPE t012-bankl,
        bankn TYPE t012k-bankn,
        gl_account    TYPE t012k-hkont,
        statement_date TYPE dats,
        posting_date   TYPE dats,
        soref          TYPE char16,
        total_api_h   TYPE p DECIMALS 0,
        total_fico_h  TYPE p DECIMALS 0,
        total_diff_h  TYPE p DECIMALS 0,
        total_api_s   TYPE p DECIMALS 0,
        total_fico_s  TYPE p DECIMALS 0,
        total_diff_s  TYPE p DECIMALS 0,
        waers         TYPE waers,
       END OF gty_data.

DATA: gt_data TYPE TABLE OF gty_data,
      gs_data TYPE gty_data.

TYPES: BEGIN OF gty_api010,
        lino          TYPE int4,
        ngaygiaodich  TYPE dats,
        ngayhachtoan TYPE dats,
        soref         TYPE char16,
        sotk          TYPE t012k-bankn,
        belnr         TYPE belnr_d,
        no_post       TYPE char1,
        ghico         TYPE zdd_dmbtr,
        ghino         TYPE zdd_dmbtr,
       END OF gty_api010.
       DATA: gt_api TYPE TABLE OF gty_api010,
             gt_api_total TYPE TABLE OF gty_api010 .


DATA: d_repid    LIKE sy-repid,
      t_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      x_layout   TYPE slis_layout_alv,
      gw_title       TYPE sy-title.
