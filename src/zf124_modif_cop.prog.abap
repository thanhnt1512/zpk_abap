*----------------------------------------------------------------------*
***INCLUDE F124_MODIF_SCREEN_CLF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MODIF_SCREEN_CL
*&---------------------------------------------------------------------*
*       Excuted at selection screen output as PBO
*----------------------------------------------------------------------*

FORM modif_screen_cl .
*Get TCODE
*Special screen for ledger group specific clearing

  clear gv_tcode.                                        "deco
  if cl_fin_ui_deco=>mv_mode = abap_false.               "deco
     CALL 'GET_PARAM_TCOD' ID 'PTCOD' FIELD gv_tcode.    "#EC CI_CCALL
  else.                                                  "deco
    gv_tcode = cl_fin_ui_deco=>mv_param_tcode.           "deco
  endif.                                                 "deco

  IF gv_tcode = 'F13L'.                                        "1376249
    CALL FUNCTION 'FAGL_CHECK_GLFLEX_ACTIVE'
      IMPORTING
        e_glflex_active = gv_newgl_active.

    IF gv_newgl_active = 'X'.
      x_lg = 2.
      LOOP AT SCREEN.
        IF screen-group1 = 'CL'.
          screen-invisible = '1'.
          screen-active = '0'.
          MODIFY SCREEN.                                       "1987699
        ENDIF.
      ENDLOOP.
      gv_title = text-t01.
      SET TITLEBAR '001' WITH gv_title.
    ELSE.
      MESSAGE e027(fagl_posting).
*   Ledgergruppenspezifisches Ausgleichen nicht möglich; New GL nicht aktive
    ENDIF.
  ELSEIF gv_tcode = 'F.13' or gv_tcode = 'F13E'.               "1376249
    x_lg = 1.
    LOOP AT SCREEN.
      IF screen-group1 = 'LG' AND x_lgclr IS INITIAL.          "1987699
        screen-invisible = '1'.
        screen-active = '0'.
        MODIFY SCREEN.                                         "1987699
      ENDIF.
    ENDLOOP.
  ELSE.                                                        "1376249
* other tcodes like SE38, SA38, SM37
    CALL FUNCTION 'FAGL_CHECK_GLFLEX_ACTIVE'
      IMPORTING
        e_glflex_active = gv_newgl_active.
    IF gv_newgl_active = char_x.
* X_LG is set dependent on parameter X_LGCLR in Form check_xfield
      CLEAR x_lg.
    ELSE.
      x_lg = 1.
      LOOP AT SCREEN.
        IF screen-group1 = 'LG' AND x_lgclr IS INITIAL.        "1987699
          screen-invisible = '1'.
          screen-active = '0'.
          MODIFY SCREEN.                                       "1987699
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " MODIF_SCREEN_CL
*&---------------------------------------------------------------------*
*&      Form  CALL_F1_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_f1_help .
* reworked by note 2274001
   Data: Ld_dokname(20) type c.

   Check sy-ucomm = 'FC01'.

   If X_lg = 2.
      Ld_dokname = 'FAGL_SAPF124_L'.
   Else.
      Ld_dokname = 'FAGL_SAPF124'.
   Endif.
   Call function 'DSYS_SHOW_FOR_F1HELP'
   exporting Dokclass = 'TX'
             Dokname  = Ld_dokname
   exceptions Class_unknown    = 1
              Object_not_found = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
   Endif.
Endform.                    " CALL_F1_HELP
