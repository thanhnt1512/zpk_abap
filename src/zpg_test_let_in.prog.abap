*&---------------------------------------------------------------------*
*& Report ZPG_TEST_LET_IN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_TEST_LET_IN.
DATA :lw_string.
LET x = 'hi' in lw_string = x.
WRITE lw_string.
