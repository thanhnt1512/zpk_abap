*&---------------------------------------------------------------------*
*& Report ZPG_TEST_MEMORY1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_memory1.
DATA :test  TYPE string VALUE 'hello world',
      test2 TYPE string.
DATA :matnr    TYPE matnr,
      mara_tab TYPE TABLE OF mara,
      flag TYPE char1.
*EXPORT test TO MEMORY ID  'hi'.
flag = 'X'.
EXPORT flag FROM 'X' to DATABASE indx(zf) id 'ZFII'.
matnr = 'PROD'.
CALL FUNCTION 'ZGET_MARA'
  EXPORTING
    matnr    =  matnr
  tables
    mara_tab = mara_tab.
