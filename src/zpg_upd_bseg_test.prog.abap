*&---------------------------------------------------------------------*
*& Report ZPG_UPD_BSEG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE ZPG_UPD_BSEGTOP_TEST.
*INCLUDE ZPG_UPD_BSEGTOP                         .    " Global Data

INCLUDE ZPG_UPD_BSEGO01_TEST.
*INCLUDE ZPG_UPD_BSEGO01                         .  " PBO-Modules
INCLUDE ZPG_UPD_BSEGI01_TEST.
*INCLUDE ZPG_UPD_BSEGI01                         .  " PAI-Modules
INCLUDE ZPG_UPD_BSEGF01_TEST.
*INCLUDE ZPG_UPD_BSEGF01                         .  " FORM-Routines

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM FRM_GET_FILE CHANGING P_FILE.

*AT SELECTION-SCREEN.
*  IF sy-ucomm = 'FC01'.
*    CALL FUNCTION 'ZXLWB_WORKBENCH'
*      EXPORTING
*        iv_formname        = 'TOOL_ULP_BASELINE'
*        iv_action          = 'EXPORT'
*      EXCEPTIONS
*        process_terminated = 1
*        OTHERS             = 2.
*    IF sy-subrc NE 0 .
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
*    ENDIF .
*  ENDIF.

START-OF-SELECTION.
PERFORM MAIN_PROG.
