*&---------------------------------------------------------------------*
*& Report ZPG_TEST_GET_SUM_STR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_TEST_GET_SUM_STR.

DATA :lw_str TYPE NDJAR.

lw_str = '100'.
DATA(lw_str2) = lw_str * 12.
WRITE lw_str2.
