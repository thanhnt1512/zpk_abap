*&---------------------------------------------------------------------*
*& Include          ZIN_EVTP_REPORT_TOP
*&---------------------------------------------------------------------*
*REPORT zpg_fir033.
TABLES: acdoca.
TYPES:
  BEGIN OF ty_data,
    kunnr             TYPE kunnr,
    kunnr_name        TYPE text200,
    "amount_fw         TYPE fins_vhcur12,
    am_m7_pr    TYPE fins_vhcur12,
    am_m8_pr    TYPE fins_vhcur12,
    am_m9_pr    TYPE fins_vhcur12,
    am_m10_pr   TYPE fins_vhcur12,
    am_m11_pr   TYPE fins_vhcur12,
    am_m12_pr   TYPE fins_vhcur12,
    amount_m1         TYPE fins_vhcur12,
    amount_m2         TYPE fins_vhcur12,
    amount_m3         TYPE fins_vhcur12,
    amount_m4         TYPE fins_vhcur12,
    amount_m5         TYPE fins_vhcur12,
    amount_m6         TYPE fins_vhcur12,
    amount_m7         TYPE fins_vhcur12,
    amount_m8         TYPE fins_vhcur12,
    amount_m9         TYPE fins_vhcur12,
    amount_m10        TYPE fins_vhcur12,
    amount_m11        TYPE fins_vhcur12,
    amount_m12        TYPE fins_vhcur12,
    amount_tt         TYPE fins_vhcur12,
    rhcur             TYPE fins_currh,
    prctr             TYPE prctr,
    prctr_name        TYPE ltext,
    segment           TYPE fb_segment,
    partner_grid      TYPE zdt_partner_grid,
    partner_grid_name TYPE zdt_lname,
    partner_grid_val  TYPE zdt_partner_grval,
    payment_term      TYPE knb1-zterm,
  END OF ty_data.

DATA: gt_data TYPE TABLE OF ty_data.
DATA: d_repid    LIKE sy-repid,
      t_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      x_layout   TYPE slis_layout_alv.
DATA: gw_segm  TYPE fb_segment,
      gw_prctr TYPE prctr,
      gw_kunnr TYPE kunnr.

*SELECTION-SCREEN SKIP.
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_racct TYPE acdoca-racct OBLIGATORY,
            p_budat TYPE budat OBLIGATORY.
SELECT-OPTIONS:
  s_pc    FOR gw_prctr,
  s_segm  FOR gw_segm,
  s_kunnr FOR gw_kunnr.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_finace TYPE text30,
            p_direct TYPE text30,
            p_acc_cq TYPE text30,
            p_acc_ch TYPE text30.
SELECTION-SCREEN END OF BLOCK b2.
