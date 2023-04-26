*&---------------------------------------------------------------------*
*& Report ZPG_TEST_MEMORY2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_TEST_MEMORY2.
*DATA :test2 TYPE string.
*SUBMIT ZPG_TEST_MEMORY1 AND RETURN.
*import test to TEST2 FROM MEMORY ID 'hi'.
*WRITE test2.
set PARAMETER ID 'MAT' FIELD '2345'.
