*&---------------------------------------------------------------------*
*& Report ZPG_TEST_EV_SEL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_TEST_EV_SEL.
PARAMETER p_bukrs TYPE t001-bukrs.
TYPES: BEGIN OF ty_t001,
bukrs TYPE t001-bukrs,
butxt TYPE t001-butxt,
ort01 TYPE t001-ort01,
END OF ty_t001.
DATA: wa_t001 TYPE ty_t001,
it_t001 TYPE TABLE OF ty_t001.
SELECT SINGLE bukrs butxt ort01 FROM t001
INTO wa_t001 WHERE bukrs =
p_bukrs.
CALL FUNCTION 'OPEN_FORM'
EXPORTING
form = 'ZFEB26'.
CALL FUNCTION 'WRITE_FORM'
EXPORTING
element = 'SATISH'
window = 'MAIN'.
CALL FUNCTION 'CLOSE_FORM'.
