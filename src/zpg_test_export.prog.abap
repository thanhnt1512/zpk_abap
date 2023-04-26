*&---------------------------------------------------------------------*
*& Report ZPG_TEST_EXPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_TEST_EXPORT.
TYPES:
  BEGIN OF line_type,
    col1 TYPE i,
    col2 TYPE i,
  END OF line_type,
  tab_type TYPE STANDARD TABLE OF line_type WITH EMPTY KEY.

DATA(wa_indx) = VALUE demo_indx_blob(
  timestamp = CONV #( sy-datum && sy-uzeit )
  userid    = sy-uname ).

DATA(itab) = VALUE tab_type(
  FOR i = 1 UNTIL i > 100 ( col1 = i col2 = i ** 2 )  ).

EXPORT tab = itab
  TO DATABASE demo_indx_blob(sq)
  FROM wa_indx
  ID 'TABLE'.
