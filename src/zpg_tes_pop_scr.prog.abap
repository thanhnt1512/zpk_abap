*&---------------------------------------------------------------------*
*& Report ZPG_TES_POP_SCR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_TES_POP_SCR.
SELECTION-SCREEN BEGIN OF SCREEN 1001 AS WINDOW.
PARAMETERS: p_bukrs TYPE bukrs.
SELECTION-SCREEN END OF SCREEN 1001.

START-OF-SELECTION.
CALL SELECTION-SCREEN 1001 STARTING AT 10 10.
IF sy-subrc = 0.
  MESSAGE i006(aq) WITH 'Code' p_bukrs 'is entered'.
ENDIF.
