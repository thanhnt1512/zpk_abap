*&---------------------------------------------------------------------*
*& Include          ZIN_VALID_DATE
*&---------------------------------------------------------------------*
DATA:it_fieldcat TYPE TABLE OF slis_fieldcat_alv,
     ls_layout   TYPE slis_layout_alv.

SELECT bseg~bukrs,bseg~belnr,bseg~gjahr,buzei,zfbdt FROM bseg  INNER JOIN bkpf
ON bseg~bukrs = bkpf~bukrs AND bseg~belnr = bkpf~belnr AND  bseg~gjahr = bkpf~gjahr
 INTO TABLE @DATA(lt_bseg)
UP TO 20 ROWS.

*DELETE lt_bseg WHERE zfbdt >= '20190101'.
*
*LOOP AT lt_bseg INTO DATA(ls_bseg).
*  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
*    EXPORTING
*      date                      = ls_bseg-zfbdt
*    EXCEPTIONS
*      plausibility_check_failed = 1
*      OTHERS                    = 2.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*    APPEND ls_bseg TO lt_invalid_bseg[].
*  ENDIF.
*ENDLOOP.

"build layout
ls_layout-colwidth_optimize = 'X'.

"build fieldcate
PERFORM build_fieldcat USING:
           'BUKRS' 'BSEG' 'Company Code',
           'BELNR' 'BSEG' 'Document Number',
           'GJAHR' 'BSEG' 'Fiscal Year',
           'BUZEI' 'BSEG' 'Line item',
           'ZFBDT' 'BSEG' 'Baseline Payment'.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
*    i_callback_program       = 'ZPG_CHECK_VALID_DATE'
    it_fieldcat              = it_fieldcat
    is_layout                = ls_layout
    i_callback_pf_status_set = 'SET_STATUS'
  TABLES
*   t_outtab                 = lt_invalid_bseg
    t_outtab                 = lt_bseg
* EXCEPTIONS
*   PROGRAM_ERROR            = 1
*   OTHERS                   = 2
  .
FORM set_status USING  extab TYPE slis_t_extab.             "#EC CALLED
  SET PF-STATUS 'STANDARD' EXCLUDING extab.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_fieldcat USING l_field l_tab l_text.
  APPEND INITIAL LINE TO it_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).
  <fs_fieldcat>-fieldname      = l_field.
  <fs_fieldcat>-tabname        = l_tab.
  <fs_fieldcat>-seltext_m      = l_text.
ENDFORM.
