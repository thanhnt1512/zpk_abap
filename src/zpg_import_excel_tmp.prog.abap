*&---------------------------------------------------------------------*
*& Report ZPG_IMPORT_EXCEL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE ZPG_IMPORT_EXCEL_TOP_TMP.
*INCLUDE zpg_import_excel_top                    .    " Global Data
INCLUDE ZPG_IMPORT_EXCEL_F01_TMP.
*INCLUDE zpg_import_excel_f01                   .  " FORM-Routines

 AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_FILE'
    IMPORTING
      file_name  = p_file.

START-OF-SELECTION.
 PERFORM get_data.
 PERFORM display_data.
