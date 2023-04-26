*&---------------------------------------------------------------------*
*& Report ZPG_TEST_SELECT_PACKAGE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_select_package.
SELECT * FROM usr01 INTO TABLE @DATA(lt_usr01) PACKAGE SIZE 10.
  CHECK 1 = 1.
ENDSELECT.
