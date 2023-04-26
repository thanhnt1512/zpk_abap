*-----------------------------------------------------------------------
*      FI - Maschinelles Ausgleichen
*-----------------------------------------------------------------------
*
Report Sapf124 line-size 166 no standard page heading           "1029245
message-id Fg.                                                  "1029245

INCLUDE ZF124TOP_COP.
*INCLUDE ZF124TOP.
*INCLUDE f124top.
INCLUDE ZINCLF124_COP.
*INCLUDE inclf124.           "general constants
INCLUDE ZF124_AU_COP.
*INCLUDE ZF124_AUSLAUFENDE_WAEHRUNG_MF1.
*INCLUDE f124_auslaufende_waehrung_mf01.
INCLUDE ZF124_CHECK_COP.
*INCLUDE ZF124_CHECK_PAYMENT_METHODF01.
*INCLUDE f124_check_payment_methodf01.
INCLUDE ZF124_SET_GLOBAL_COP.
*INCLUDE ZF124_SET_GLOBAL_FLAGSF01.
*INCLUDE f124_set_global_flagsf01.
INCLUDE ZF124_DETAIL_COP.
*INCLUDE ZF124_DETAIL_LIST.
*INCLUDE f124_detail_list.   "Data and form routines for the detail list
INCLUDE ZF124_SHORT_LIST.
*INCLUDE f124_short_list.    "Data and form routines for the short list
INCLUDE ZF124_LOGS_COP.
*INCLUDE ZF124_LOGS.
*INCLUDE f124_logs.          "Data and form routines for the logs
INCLUDE ZF124_MERGE_COP.
*INCLUDE ZF124_MERGE.
*INCLUDE f124_merge.         "Main subroutines
INCLUDE ZF124_MODIF_COP.
*INCLUDE ZF124_MODIF_SCREEN_CLF01.
*Include F124_modif_screen_clf01.                                "1029245
INCLUDE ZF124_SET_INFO_COP.
*INCLUDE ZF124_SET_INFO_ICONF01.
*Include F124_set_info_iconf01.                                  "1029245
*------------------------------------------Initilization----------------
INITIALIZATION.
  Bhdgd-Inifl = '0'.                                            "1029245
  bhdgd-lines = sy-linsz.              " Zeilenbreite aus Report
  bhdgd-uname = sy-uname.              " Benutzername
  bhdgd-repid = sy-repid.              " Name des ABAP-Programmes
  bhdgd-line1 = sy-title.              " Titel des ABAP-Programmes
  bhdgd-separ = space.                 " Keine Listseparation
  PERFORM set_info_icon.               " Info Icon will be set.
  PERFORM fill_xf123_if123.

*------------------------------------------At selection-screen output-------------
AT SELECTION-SCREEN OUTPUT.
  bsis-waers = zwaers.

*----------------- Load screen depending on the selected clearing method-----------------------
*-----------------Not Parked in Enhancementpackage --------------------------------------------
*-----------------Cause: MODIF ID works not correctly with Enhancement ------------------------
 PERFORM modif_screen_cl.


AT SELECTION-SCREEN.
* Checks only when not multiple selection choosen
  CHECK NOT ( sy-ucomm CP '%0++' ).
  PERFORM call_f1_help.
  CHECK NOT ( sy-ucomm EQ 'FC01').
* Check parameters
  PERFORM check_xfield.
  PERFORM check_bukrs.
  PERFORM gjvtab_init.
  PERFORM check_authority.
  PERFORM check_augdt.
  IF NOT augdt IS INITIAL.
    PERFORM gjvtab_check.
    LOOP AT i001.
      monat = bmonat.                                           "1541804
      CHECK i001-bukrs IN bukrx.
      CLEAR gejahr.
*     Ignore company codes without fiscal year variant
      CHECK i001-periv NE space.
*     Ignore company codes without variant for posting periods
      CHECK i001-opvar NE space.
*     Determine posting period
      PERFORM periode_ermitteln USING i001-bukrs
                                      augdt
                                      gejahr
                                      monat.                    "1121415
      IF NOT bmonat IS INITIAL                                  "1121415
      AND    monat NE bmonat.
        MESSAGE w000 WITH i001-bukrs bmonat augdt monat.        "1121415
      ENDIF.
*     check posting period
      Clear Rtc.                                                "2238211
      PERFORM periode_pruefen USING i001-bukrs
                                    augdt
                                    gejahr
                                    monat                       "1121415
                                    'X'                         "1112148
                                    ' '                         "1438569
                           changing Rtc.                        "2238211
      If Rtc <> 0.                                              "2238211
         Gt_001no-Bukrs = I001-Bukrs.                           "2238211
         Append Gt_001no.                                       "2238211
      Endif.                                                    "2238211
      IF zwaers NE space.
        CALL FUNCTION 'CURRENCY_EXPIRATION_CHECK'
          EXPORTING
            currency         = zwaers
            date             = augdt
            object           = 'BKPF'
            bukrs            = i001-bukrs
          EXCEPTIONS
            warning_occurred = 1
            error_occurred   = 2.
        IF sy-subrc = 1.
          MESSAGE w895(fg) WITH zwaers i001-bukrs.
        ELSEIF sy-subrc = 2.
          MESSAGE e895(fg) WITH zwaers i001-bukrs.
        ENDIF.
      ENDIF.
    ENDLOOP.
                                                                "1121415
  ENDIF.
  IF xauslw = 'X'.
    CALL FUNCTION 'CURRENCY_CHECK_FOR_PROCESS'
      EXPORTING
        process                = 'SAPF124E'
      EXCEPTIONS
        process_not_maintained = 1.
    IF sy-subrc <> 0.
      MESSAGE i896(fg).
    ENDIF.
  ENDIF.
* determine GR/IR and cash discount clearing accounts
  IF x_saknr = 'X'.
    PERFORM select_t030.
  ENDIF.
* customizing in TF123 maintained?
  PERFORM check_rules.
* Warning at update run
  PERFORM check_echtl.
* Determine list format
  PERFORM init_list.

*------------------------------------------At selection-screen on Zwaers
AT SELECTION-SCREEN ON zwaers.
  PERFORM check_waehrung.
*------------------------------------------Start-of-selection-----------
Start-of-selection.
* Call from Fiori Application job
  IF x_applog IS NOT INITIAL.
    EXIT.
  ENDIF.
Clear Bsis-Waers.
* Check for enqueues in REGUS                                    "681786
Perform Regus_pruefen.                                    "681786
* keep start time
Perform Acc_init_log.
  If sy-binpt = space.                                          "1223511
     Perform Start_schedman.                                    "1081370
     Commit work.                                               "1081370
  Endif.                                                        "1223511
Perform Set_global_flags.
  PERFORM read_tcure.                                       "note2251669
  If Xauslw = 'X'.
*   principal check for expiring currencies
   Perform Currency_check_for_process.
Endif.
* Fill TDEBI, TKREDI, TSAKO
Perform Kontotabellen_fuellen.
* process customer accounts
Perform Debi_verarbeiten.
* process vendor accounts
Perform Kredi_verarbeiten.
* process GL accounts
IF  xausbel  = space                                            "1410838
AND xnausbel = space.
* AND x_lgclr  = 'X'.                                           "1608455
  gd_do_del = 'X'.
ENDIF.
Perform Sako_verarbeiten.
  If sy-binpt = space.                                          "1223511
     Perform End_schedman.                                      "1081370
     Commit work.                                               "1081370
  Endif.                                                        "1223511
                                                                "1029245
*------------------------------------------End-of-selection-------------
End-of-selection.
If Flg_liste = char_2.
   Perform Ausgabe_gesamtsumme.                                 "1029245
Endif.
Perform Acc_ausgabe_addition_log.                               "1029245
If X_fehler = 'X'.                                              "1029245
   Perform Acc_ausgabe_fault_log.                               "1029245
Endif.                                                          "1029245
*------------------------------------------Top of page----------"1029245
Top-of-page.                                                    "1029245
Perform Top_of_page.                                            "1029245

INCLUDE ZF124_REDUCE_COP.
*INCLUDE ZF124_REDUCE_TSAKOF01.
*INCLUDE f124_reduce_tsakof01.
