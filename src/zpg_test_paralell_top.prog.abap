*&---------------------------------------------------------------------*
*& Include          ZPG_TEST_PARALELL_TOP
*&---------------------------------------------------------------------*
REPORT zpg_test_paralell.
DATA :gw_max_ws  TYPE int4,
      gw_free_ws TYPE int4.
DATA :gt_data TYPE TABLE OF bkpf,
      gw_max_record TYPE int4.
DATA :gt_result_collect TYPE TABLE of bkpf.
