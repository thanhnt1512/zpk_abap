*&---------------------------------------------------------------------*
*& Include ZPG_IMPORT_EXCEL_TOP                     - Report ZPG_IMPORT_EXCEL
*&---------------------------------------------------------------------*
REPORT ZPG_IMPORT_EXCEL.

TABLES: t012k.

SELECTION-SCREEN BEGIN OF BLOCK A WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file LIKE rlgrap-filename MODIF ID a.
  PARAMETERS: p_search TYPE t012k-hbkid MATCHCODE OBJECT ZAK_SEARCH_HELP.
SELECTION-SCREEN END OF BLOCK A.

TYPES: BEGIN OF gty_excel,
          1 TYPE string,
          2 TYPE string,
          3 TYPE string,
          4 TYPE string,
          5 TYPE string,
          6 TYPE string,
          7 TYPE string,
          8 TYPE string,
          9 TYPE string,
          10 TYPE string,
          11 TYPE string,
          12 TYPE string,
          13 TYPE string,
          14 TYPE string,
          15 TYPE string,
          16 TYPE string,
          17 TYPE string,
       END OF gty_excel.

DATA: gt_excel TYPE TABLE OF gty_excel.

DATA: gt_data TYPE TABLE OF ztb_api010_ipex,
      gs_data TYPE ztb_api010_ipex.
DATA: gt_data_import TYPE TABLE OF ztb_api010_input,
      gs_data_import TYPE ztb_api010_input.
 DATA: lw_leng TYPE i.
 FIELD-SYMBOLS: <f_acc> TYPE ANY.
 FIELD-SYMBOLS: <f1> TYPE ANY, <f2> TYPE ANY, <f3> TYPE ANY, <f4> TYPE ANY, <f5> TYPE ANY, <f6> TYPE ANY, <f7> TYPE ANY, <f8> TYPE ANY, <f9> TYPE ANY, <f10> TYPE ANY, <f11> TYPE ANY,
                <f12> TYPE ANY, <f13> TYPE ANY, <f14> TYPE ANY, <f15> TYPE ANY, <f16> TYPE ANY, <f17> TYPE ANY.
 DATA: lw_acc TYPE char16.
 DATA: lw_line TYPE char5.
 DATA :lw_string TYPE guid_32.
 DATA: lw_1 TYPE char3,
       lw_2 TYPE char3,
       lw_3 TYPE char3,
       lw_4 TYPE char3,
       lw_5 TYPE char3,
       lw_6 TYPE char3,
       lw_7 TYPE char3,
       lw_8 TYPE char3,
       lw_9 TYPE char3,
       lw_10 TYPE char3,
       lw_11 TYPE char3,
       lw_12 TYPE char3,
       lw_13 TYPE char3,
       lw_14 TYPE char3,
       lw_15 TYPE char3,
       lw_16 TYPE char3
       .

DATA: d_repid    LIKE sy-repid,
      t_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      x_layout   TYPE slis_layout_alv.

DATA :gt_event TYPE SLIS_T_EVENT.
