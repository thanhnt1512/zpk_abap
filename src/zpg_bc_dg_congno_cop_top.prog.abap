*&---------------------------------------------------------------------*
*& Include          ZPG_BC_DG_CONGNO_TOP
*&---------------------------------------------------------------------*
REPORT zpg_bc_dg_congno.
TABLES: knb1, acdoca, zev_cn.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_bukrs TYPE acdoca-rbukrs OBLIGATORY,
*              p_gjahr TYPE acdoca-gjahr OBLIGATORY,
*            p_budat TYPE dats DEFAULT sy-datum,
            p_year  TYPE bseg-gjahr OBLIGATORY,
            p_month TYPE char2 OBLIGATORY,
            p_stt   TYPE zdd_ttpd.
SELECT-OPTIONS: s_kunnr FOR knb1-kunnr NO INTERVALS,
                s_segm  FOR acdoca-segment NO INTERVALS OBLIGATORY,
                s_prctr FOR acdoca-prctr,
                s_racct FOR acdoca-racct.
SELECTION-SCREEN END OF BLOCK b1.
TYPES: BEGIN OF gty_excel,
         prctr     TYPE string,
         segment   TYPE string,
         pstcode   TYPE string,
         ten_cn    TYPE string,
         kunnr     TYPE string,
         cpn_n1    TYPE string,
         cpn_n2    TYPE string,
         cpn_n3    TYPE string,
         cpn_n6    TYPE string,
         vpp_n1    TYPE string,
         vpp_n2    TYPE string,
         vpp_n3    TYPE string,
         vpp_n6    TYPE string,
         voso_n1   TYPE string,
         voso_n2   TYPE string,
         voso_n3   TYPE string,
         voso_n6   TYPE string,
         vtsale_n1 TYPE string,
         vtsale_n2 TYPE string,
         vtsale_n3 TYPE string,
         vtsale_n6 TYPE string,
         log_n1    TYPE string,
         log_n2    TYPE string,
         log_n3    TYPE string,
         log_n6    TYPE string,
         thoi_han  TYPE string,
       END OF gty_excel.

TYPES :BEGIN OF gty_acdoca,
         prctr     TYPE acdoca-prctr,
         segment   TYPE acdoca-segment,
         kunnr     TYPE acdoca-kunnr,
         racct     TYPE acdoca-racct,
         waers     TYPE waers,
         cpn_n1    TYPE fins_vhcur12,
         cpn_n2    TYPE fins_vhcur12,
         cpn_n3    TYPE fins_vhcur12,
         cpn_n6    TYPE fins_vhcur12,
         vpp_n1    TYPE fins_vhcur12,
         vpp_n2    TYPE fins_vhcur12,
         vpp_n3    TYPE fins_vhcur12,
         vpp_n6    TYPE fins_vhcur12,
         voso_n1   TYPE fins_vhcur12,
         voso_n2   TYPE fins_vhcur12,
         voso_n3   TYPE fins_vhcur12,
         voso_n6   TYPE fins_vhcur12,
         vtsale_n1 TYPE fins_vhcur12,
         vtsale_n2 TYPE fins_vhcur12,
         vtsale_n3 TYPE fins_vhcur12,
         vtsale_n6 TYPE fins_vhcur12,
         log_n1    TYPE fins_vhcur12,
         log_n2    TYPE fins_vhcur12,
         log_n3    TYPE fins_vhcur12,
         log_n6    TYPE fins_vhcur12,
       END OF gty_acdoca.

TYPES :BEGIN OF gty_cn,
         zev_cn  TYPE zev_cn-zev_cn,
         prctr   TYPE zev_cn-prctr,
         pstcode TYPE zev_cn-pstcode,
         ten_cn  TYPE zev_cn-ten_cn,
       END OF gty_cn.
TYPES :BEGIN OF gty_knb1,
         kunnr TYPE knb1-kunnr,
         bukrs TYPE knb1-bukrs,
         zterm TYPE knb1-zterm,
       END OF gty_knb1.
TYPES :BEGIN OF gty_fir048.
    INCLUDE STRUCTURE zst_fir048.
TYPES :cell_style  TYPE lvc_t_styl.
TYPES :cell_color  TYPE lvc_t_scol.
TYPES:END OF gty_fir048.

TYPES :BEGIN OF gty_fir045.
    INCLUDE STRUCTURE zst_fir045.
*TYPES :cell_style  TYPE lvc_t_styl.
TYPES :cell_color  TYPE lvc_t_scol.
TYPES:END OF gty_fir045.

TYPES : tt_fir048 TYPE TABLE OF gty_fir048,
        tt_fir045 TYPE TABLE OF gty_fir045.
DATA: gt_excel TYPE TABLE OF gty_excel,
      gs_excel TYPE gty_excel.

DATA: gt_dfies_tab TYPE TABLE OF dfies.
DATA: gt_data1       TYPE TABLE OF zst_fir045,
      gt_data        TYPE TABLE OF gty_fir045,
      gt_data_temp   TYPE TABLE OF gty_excel,
      gt_data_import TYPE TABLE OF gty_excel,
      gt_data_pd     TYPE TABLE OF zst_fir048,
      gs_data        TYPE zst_fir045,
      gt_acdoca      TYPE TABLE OF gty_acdoca,
      gt_cn          TYPE TABLE OF gty_cn,
      gt_knb1        TYPE TABLE OF gty_knb1.
*      gt_acdoca      TYPE TABLE OF zst_fir048_acdoca,
*      gt_knb1        TYPE TABLE OF zst_fir048_knb1,
*      gt_cn          TYPE TABLE OF zev_cn.
TYPES :tt_acdoca LIKE gt_acdoca,
       tt_cn     LIKE gt_cn,
       tt_knb1   LIKE gt_knb1.
DATA: g_alv              TYPE REF TO cl_gui_alv_grid,
      g_container_header TYPE REF TO cl_gui_container,
      g_doc_header       TYPE REF TO cl_dd_document.
DATA :gt_cong_no        TYPE TABLE OF zdn_congno,
      gw_postg_date_str TYPE string,
      gw_stt_str        TYPE string,
      gw_budat          TYPE budat.
DATA : p_file TYPE rlgrap-filename.
DATA : gw_error TYPE char1.
FIELD-SYMBOLS: <fs_fieldcat> TYPE lvc_t_fcat.

DATA :gt_data_dn           TYPE TABLE OF gty_fir048,
      gt_data_import_valid TYPE TABLE OF gty_fir048.

DATA :g_container_0100 TYPE REF TO cl_gui_custom_container,
      g_container_0200 TYPE REF TO cl_gui_custom_container,
      g_container_0300 TYPE REF TO cl_gui_custom_container.

DATA :gw_max_ws     TYPE int4,
      gw_free_ws    TYPE int4,
      gw_max_record TYPE int4.

DATA: date         TYPE dats,
      due_date     TYPE dats,
      due_date_n1  TYPE sy-datum,
      due_date_n2  TYPE sy-datum,
      due_date_n3  TYPE sy-datum,
      due_date_n4  TYPE sy-datum,
      due_date_n5  TYPE sy-datum,
      due_date_n6  TYPE sy-datum,
      due_date_n7  TYPE sy-datum,
      due_date_n8  TYPE sy-datum,
      due_date_n12 TYPE sy-datum.

DATA :rcv_jobs  TYPE i.


TYPES :BEGIN OF gty_data_parallel,
         BEGIN OF importing,
           acdoca LIKE gt_acdoca,
           knb1   LIKE gt_knb1,
           zev_cn LIKE gt_cn,
           BEGIN OF date,
             due_date    TYPE dats,
             due_date_n1 TYPE sy-datum,
             due_date_n2 TYPE sy-datum,
             due_date_n3 TYPE sy-datum,
             due_date_n4 TYPE sy-datum,
             due_date_n5 TYPE sy-datum,
             due_date_n6 TYPE sy-datum,
             due_date_n7 TYPE sy-datum,
*             due_date_n8  TYPE sy-datum,
*             due_date_n12 TYPE sy-datum,
           END OF date,
         END OF importing,
         BEGIN OF exporting,
           data LIKE gt_data1,
         END OF exporting,
       END OF gty_data_parallel.
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
*    METHODS : handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
*      IMPORTING e_row e_column es_row_no.
    METHODS : handle_add_toolbar_alv_0100 FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive.
    METHODS : handle_add_toolbar_alv_0200 FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive.
    METHODS : handle_add_toolbar_alv_0300 FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive.
    METHODS : handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
    METHODS:  handle_set_title FOR EVENT top_of_page OF cl_gui_alv_grid
      IMPORTING e_dyndoc_id.
ENDCLASS.
