*&---------------------------------------------------------------------*
*& Report ZPG_CHECK_VALID_DATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_check_valid_date_cop.

DATA:it_fieldcat TYPE TABLE OF slis_fieldcat_alv,
     ls_layout   TYPE slis_layout_alv.

SELECT bseg~bukrs,bseg~belnr,bseg~gjahr,buzei,zfbdt FROM bseg  INNER JOIN bkpf
ON bseg~bukrs = bkpf~bukrs AND bseg~belnr = bkpf~belnr AND  bseg~gjahr = bkpf~gjahr
WHERE hkont LIKE '1311103%'AND xreversed NE 'X' AND ( dats_is_valid( zfbdt ) = '0' OR zfbdt < '20190101' OR zfbdt > '20221231' )
APPENDING TABLE @DATA(lt_bseg).

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
   i_callback_program      = 'ZPG_CHECK_VALID_DATE_COP'
    it_fieldcat             = it_fieldcat
    is_layout               = ls_layout
    i_callback_user_command = 'USER_COMMAND'
  TABLES
*   t_outtab                = lt_invalid_bseg
    t_outtab                = lt_bseg
* EXCEPTIONS
*   PROGRAM_ERROR           = 1
*   OTHERS                  = 2
  .
IF sy-subrc <> 0.
  WRITE 'Something went wrong!'.
ENDIF.
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
FORM user_command USING ucomm    LIKE sy-ucomm
                        selfield TYPE slis_selfield.

  CASE ucomm.
*   Double Click
  WRITE 'ok'.

  ENDCASE.

ENDFORM.
