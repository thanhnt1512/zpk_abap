*&---------------------------------------------------------------------*
*& Report ZPG_TOTAL_BANK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE ZPG_TOTAL_BANKTOP_COP.
*INCLUDE ZPG_TOTAL_BANKTOP                       .    " Global Data
INCLUDE ZPG_TOTAL_BANKF01_COP.
*INCLUDE ZPG_TOTAL_BANKF01                       .  " FORM-Routines

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM display_alv.
