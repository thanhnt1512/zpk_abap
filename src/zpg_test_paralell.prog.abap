*&---------------------------------------------------------------------*
*& Report ZPG_TEST_PARALELL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE  zpg_test_paralell_top.
INCLUDE  zpg_test_paralell_f01.

START-OF-SELECTION.
  PERFORM get_resource.
  PERFORM process.
  PERFORM print.
