*----------------------------------------------------------------------*
***INCLUDE F124_AUSLAUFENDE_WAEHRUNG_MF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  auslaufende_waehrung_merken
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_MSGID  text
*      -->P_SY_MSGTY  text
*      -->P_SY_MSGNO  text
*      -->P_BSID_WAERS  text
*----------------------------------------------------------------------*
FORM AUSLAUFENDE_WAEHRUNG_MERKEN USING    P_MSGID LIKE SY-MSGID
                                          P_MSGTY LIKE SY-MSGTY
                                          P_MSGNO
                                          P_WAERS.
  GT_AUSLWAER-MSGID = P_MSGID.
  GT_AUSLWAER-MSGTY = P_MSGTY.
  GT_AUSLWAER-MSGNO = P_MSGNO.
  GT_AUSLWAER-WAERS = P_WAERS.
  COLLECT GT_AUSLWAER.

ENDFORM.                    " auslaufende_waehrung_merken
*&---------------------------------------------------------------------*
*&      Form  currency_get_subsequent
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_AWAERS  text
*      -->P_AUGDT  text
*      -->P_BSID_BUKRS  text
*      <--P_LD_NEW_WAERS  text
*----------------------------------------------------------------------*
FORM CURRENCY_GET_SUBSEQUENT USING    ID_AWAERS LIKE BKPF-WAERS
                                      ID_AUGDT  LIKE SY-DATUM
                                      ID_BUKRS LIKE T001-BUKRS
                             CHANGING ED_NEW_WAERS LIKE BKPF-WAERS.

  CALL FUNCTION 'CURRENCY_GET_SUBSEQUENT'
       EXPORTING
            CURRENCY     = ID_AWAERS
            PROCESS      = 'SAPF124E'
            DATE         = ID_AUGDT
            BUKRS        = ID_BUKRS
       IMPORTING
            CURRENCY_NEW = ED_NEW_WAERS.

ENDFORM.                    " currency_get_subsequent
*&---------------------------------------------------------------------*
*&      Form  currency_expiration_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_AWAERS  text
*      -->P_AUGDT  text
*      -->P_BSID_BUKRS  text
*----------------------------------------------------------------------*
FORM CURRENCY_EXPIRATION_CHECK USING    ID_AWAERS LIKE BKPF-WAERS
                                        ID_AUGDT LIKE SY-DATUM
                                        ID_BUKRS LIKE T001-BUKRS
                               CHANGING ED_IGNORE_DOC.
  DATA: LD_SAVE_SUBRC LIKE SY-SUBRC.
  CLEAR ED_IGNORE_DOC.
  READ TABLE GT_TCURE WITH KEY CURC_OLD = ID_AWAERS
                      TRANSPORTING NO FIELDS.         "note2251669
  IF SY-SUBRC EQ 0.                                   "note2251669
    CALL FUNCTION 'CURRENCY_EXPIRATION_CHECK'
       EXPORTING
            CURRENCY         = ID_AWAERS
            DATE             = ID_AUGDT
            OBJECT           = 'BKPF'
            BUKRS            = ID_BUKRS
       EXCEPTIONS
            WARNING_OCCURRED = 1
            ERROR_OCCURRED   = 2
            OTHERS           = 3.
  IF SY-SUBRC = 1 OR SY-SUBRC = 2.
    LD_SAVE_SUBRC = SY-SUBRC.
    PERFORM AUSLAUFENDE_WAEHRUNG_MERKEN USING SY-MSGID SY-MSGTY
                                              SY-MSGNO
                                              ID_AWAERS.
    IF LD_SAVE_SUBRC = 2.
      ED_IGNORE_DOC = 'X'.
    ENDIF.
  ENDIF.
  ENDIF.                                              "note2251669
ENDFORM.                    " currency_expiration_check
*&---------------------------------------------------------------------*
*&      Form  currency_check_for_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CURRENCY_CHECK_FOR_PROCESS.
  DATA: LD_ALL_BUKRS TYPE XFELD.

  clear gt_tcur_bukrs.
  refresh gt_tcur_bukrs.

  CALL FUNCTION 'CURRENCY_CHECK_FOR_PROCESS'
       EXPORTING
            PROCESS                = 'SAPF124E'
       IMPORTING
            ALL_BUKRS              = LD_ALL_BUKRS
       TABLES
            T_BUKRS                = GT_TCUR_BUKRS
       EXCEPTIONS
            PROCESS_NOT_MAINTAINED = 1
            OTHERS                 = 2.

  IF SY-SUBRC = 0 AND LD_ALL_BUKRS = SPACE.
*----- Prozess für einige Buchungskreise gepflegt -------------
    GD_READ_TCUR_BUKRS = 'X'.
  ELSEIF SY-SUBRC = 1.
*----- Prozess nicht gepflegt, Nachfolgewährung sinnlos -------
    GD_NO_GET_SUBSEQUENT = 'X'.
  ENDIF.

ENDFORM.                    " currency_check_for_process
*
FORM READ_TCURE .                                     "note2251669
  select * from tcure into table gt_tcure.
ENDFORM.                    " READ_TCURE              "note2251669
