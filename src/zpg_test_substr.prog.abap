*&---------------------------------------------------------------------*
*& Report ZPG_TEST_SUBSTR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_TEST_SUBSTR.
DATA :lw_str TYPE string.
lw_str = '123456'.
DATA(result) =  substring( val = substring( val = lw_str off = 0 len = strlen( lw_str ) - 1 )  off = 0 len = strlen( lw_str ) - 2  ).
write result.
