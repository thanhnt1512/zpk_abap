*&---------------------------------------------------------------------*
*& Include ZPG_UPD_BSEGTOP                          - Report ZPG_UPD_BSEG
*&---------------------------------------------------------------------*
REPORT ZPG_UPD_BSEG.

TABLES: sscrfields.
PARAMETERS:P_FILE   TYPE RLGRAP-FILENAME LOWER CASE DEFAULT '' MODIF ID G2.

DATA: GT_BSEG    TYPE STANDARD TABLE OF ZST_TEST_FIELD,
      LS_BSEG    TYPE  ZST_TEST_FIELD.


* ALV OOP
DATA: G_CONTAINER         TYPE SCRFNAME VALUE 'CC-ALV',
      G_CUSTOM_CONTAINER  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_GRID              TYPE REF TO CL_GUI_ALV_GRID,
      GT_FIELDCAT         TYPE LVC_T_FCAT,
      GS_FIELDCAT         TYPE LVC_S_FCAT,
      GT_EXCLUDE          TYPE UI_FUNCTIONS,
      GS_LAYOUT           TYPE LVC_S_LAYO.
