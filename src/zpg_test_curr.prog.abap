*&---------------------------------------------------------------------*
*& Report ZPG_TEST_CURR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_TEST_CURR.
DATA :lw_res TYPE BAPICURR-BAPICURR..
DATA :lw_cur_root TYPE BAPICURR-BAPICURR.
DATA :lw_cur_root1 TYPE WMTO_S-AMOUNT,
      lw_un TYPE TCURC-WAERS.
DATA :lw_rest TYPE WMTO_S-AMOUNT.
lw_cur_root = 3500000.
lw_un = 'VND'.

WRITE lw_cur_root to lw_res  CURRENCY lw_un.
WRITE lw_res .
