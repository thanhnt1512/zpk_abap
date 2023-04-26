*----------------------------------------------------------------------*
***INCLUDE F124_SET_GLOBAL_FLAGSF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  set_global_flags
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_global_flags .

*------ EBPP active ? --------------------------------------------------

  CALL FUNCTION 'APAR_EBPP_CHECK_ACTIVE'
    IMPORTING
      e_active = gd_ebpp_active.

*------ Public sector active?-------------------------------------861169
  CALL FUNCTION 'GET_R3_EXTENSION_SWITCH'                   "861169
  EXPORTING i_structure_package = 'EA-PS'                   "861169
  IMPORTING  e_active            = gb_eaps_active           "861169
  EXCEPTIONS not_existing        = 1                        "926772
             object_not_existing = 2                        "926772
             no_extension_object = 3.                       "926772
  IF sy-subrc NE 0.                                         "926772
    CLEAR gb_eaps_active.                                   "926772
  ENDIF.                                                    "926772


ENDFORM.                    " set_global_flags
