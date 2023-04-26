*&---------------------------------------------------------------------*
*&  Include           F124_LOGS
*&---------------------------------------------------------------------*
                                                                "1029245
*&---------------------------------------------------------------------*
*&      Form  acc_init_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM acc_init_log .
  DATA:
    ld_date(20) TYPE c,
    ld_time(20) TYPE c,
    ld_uzeit    type syuzeit,                                   "1369923
    lv_bukrs    TYPE bukrs.                                     "2106636

  If 'Z' > '9'.                                                 "2106636
    lv_bukrs = 'ZZZZ'.                                          "2106636
  Else.                                                         "2106636
    lv_bukrs = '9999'.                                          "2106636
  ENDIF.                                                        "2106636

* Startdatum und Startzeit, sowie Benutzer an das Log Hängen.
  CLEAR iprot.
  iprot-msgid = 'I'.
  iprot-bukrs = lv_bukrs.                                       "2106636
  iprot-koart = 'Z'.                                            "2106636
  iprot-buzei = '300'.                                          "2106636
  CONCATENATE text-300 sy-uname INTO Iprot-Text0 SEPARATED BY ' '.
  COLLECT iprot.
  CLEAR iprot.                                                  "2106636
  iprot-msgid = 'I'.                                            "2106636
  iprot-bukrs = lv_bukrs.                                       "2106636
  iprot-koart = 'Z'.                                            "2106636
  iprot-buzei = '301'.                                          "2106636
  Bhdgd-Datum = sy-datum.                                       "1029245
  Write Bhdgd-Datum dd/mm/yyyy to Ld_date.                      "1029245
  Ld_uzeit = sy-uzeit.                                          "1369923
  Bhdgd-Zeit = Ld_uzeit.                                        "1369923
  Write Ld_uzeit to Ld_time.                                    "1369923
  CONCATENATE text-301 ld_date '        ' text-302 ld_time
         INTO Iprot-Text0 SEPARATED BY ' '.

  COLLECT iprot.
ENDFORM.                    " acc_init_log
*---------------------------------------------------------------------*
*       FORM RECORD_FILL                                              *
*---------------------------------------------------------------------*
*       new in this include with note 1029245
*       Zusatzprotokoll fuellen                                       *
*       Systemmeldungen werden immer gefüllt und ausgegeben           *
*---------------------------------------------------------------------*
Form Record_fill using Id_txt    type any
                       Id_bukrs  type Bukrs
                       Id_koart  type Koart
                       Id_konto  type any
                       Id_hkont  type Hkont
                       Id_belnr  type Belnr_d
                       Id_buzei  type any
                       Id_msgid  type c.
   Clear Iprot.
   Iprot-Bukrs = Id_bukrs.
   Iprot-Koart = Id_koart.
   Iprot-Konto = Id_konto.
   Iprot-Hkont = Id_hkont.
   Iprot-Belnr = Id_belnr.
   Iprot-Buzei = Id_buzei.
   Iprot-Msgid = Id_msgid.
   Case Id_msgid.
   When char_l or char_x.
      X_zprt_sub = 'X'.
   Endcase.
   Case Id_txt.
   When 'MSG'.                                                  "2238211
      Message id sy-msgid type sy-msgty number sy-msgno         "2238211
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into Iprot-Text0."2238211
   When '     '.
      Iprot-Text0 = text-101.
   When 'QSSHB'.
      Iprot-Text0 = text-104.
   When 'AUGSB'.
      Iprot-Text0 = text-133.
   When 'AUBLA'.
      Iprot-Text0 = text-134.
   When 'QSSKZ'.
      Iprot-Text0 = text-105.
   When 'BSID '.
      Iprot-Text0 = text-110.
   When 'BSIK '.
      Iprot-Text0 = text-111.
   When 'BSIS '.
      Iprot-Text0 = text-112.
   When 'BKPF '.
      Iprot-Text0 = text-113.
      Replace '&1' with Id_bukrs into Iprot-Text0.
      Replace '&2' with Id_belnr into Iprot-Text0.
   When 'BSEG '.
      Iprot-Text0 = text-114.
      Replace '&1' with Id_bukrs into Iprot-Text0.
      Replace '&2' with Id_belnr into Iprot-Text0.
      Replace '&3' with Id_buzei into Iprot-Text0.
   When 'BSEG_ADD'.
      Iprot-Text0 = text-115.
      Replace '&1' with Id_bukrs into Iprot-Text0.
      Replace '&2' with Id_belnr into Iprot-Text0.
      Replace '&3' with Id_buzei into Iprot-Text0.
   When 'BUKRS'.                                                "2238211
      Clear Iprot-Buzei.                                        "2238211
      Case Id_buzei.                                            "2238211
      When '02'.                                                "2238211
         Message e112 with Id_bukrs into Iprot-Text0.           "2238211
      When '03'.                                                "2238211
         Message e113 with Id_bukrs into Iprot-Text0.           "2238211
      Endcase.                                                  "2238211
   When 'LOCK1'.
      Iprot-Text0 = text-120.          "Konto ist gesperrt
   When 'LOCK2'.
      Iprot-Text0 = text-121.          "Systemfehler beim Sperren - Prog
   When 'LOCK3'.
      Iprot-Text0 = text-122.          " Sperreintrag in Tabelle REGUS
   When 'LOCK6'.
      Iprot-Text0 = text-126.          " Sperreintrag in Tabelle T042X
   When 'RULES'.
      Iprot-Text0 = text-125.          "Keine Regeln in Tabelle TF123
   When 'SOHA '.
      Iprot-Text0 = text-127.
   When 'ZUONR'.
      Iprot-Text0 = text-132.
   When 'NOCLEAR'.
      Iprot-Text0 = text-043.
   When 'NOCLEARTOL'.
      Iprot-Text0 = text-046.
   When 'ENQ'.
      Iprot-Text0 = text-051.
   When 'LG'.
      Iprot-Text0 = text-t03.
   When 'NOLG'.
      Iprot-Text0 = text-t04.
   When OTHERS.
      Exit.
   Endcase.

   Collect Iprot.
Endform.                               "RECORD_FILL
*&--------------------------------------------------------------------*
*&      Form  log_top_of_list
*&--------------------------------------------------------------------*
*       completely reworked by note 1029245
*---------------------------------------------------------------------*
Form Log_top_of_list.
   Bhdgd-Inifl = 0.
   Describe table i001 lines sy-tfill.
   If sy-tfill > 1.
      Clear Bhdgd-Bukrs.
   Endif.
   Bhdgd-Line1 = sy-title.
   If X_echtl eq char_x.
      Concatenate text-007 text-099 into Bhdgd-Line2 separated by ' '.
   Else.
      Concatenate text-008 text-099 into Bhdgd-Line2 separated by ' '.
   Endif.
   Perform Batch-heading(Rsbtchh0).
   Format color col_heading intensified.
   Uline.
   If X_zprt_sub = 'X'.
      Write:/01(01) sy-vline,
             02(80) text-207,          "Protokolltext
            104(01) sy-vline,
            105(05) text-200,          "Buchungskreis
            110(01) sy-vline,
            111(05) text-203,          "Kontoart
            116(01) sy-vline,
            117(10) text-204,          "Kontonummer (Debi, Kredi, Sako)
            127(01) sy-vline,
            128(10) text-215,          "Abstimmkontonummer
            138(01) sy-vline,
            139(10) text-205,          "Belegnummer
            149(01) sy-vline,
            150(03) text-206,          "Buchungszeile
            166(01) sy-vline.
   Else.
      Write:/01(01) sy-vline,
             02(80) text-207,          "Zusatzprotokoll
            166(01) sy-vline.
   Endif.
   Uline.
Endform.                    "log_top_of_list

*&--------------------------------------------------------------------*
*&      Form  acc_ausgabe_addition_log                          "1029245
*&--------------------------------------------------------------------*
*       completely reworked by note 1029245
*---------------------------------------------------------------------*
Form Acc_ausgabe_addition_log.
   Data: Ld_date(20) type c,
         Ld_time(20) type c,
         Ld_char80(80) type c.

   Flg_liste = '4'.
   New-page.
   Format color col_normal intensified off.
   If Gd_ebpp_active = 'X' and Gd_ebpp_mess = 'X'.
     Clear Iprot.                                              "2106636
     Iprot-Msgid = 'I'.
      Iprot-Text0 = text-320.
      Collect Iprot.
   Endif.
   Clear Iprot.
   If 'Z' > '9'.                                               "2106636
      Iprot-bukrs = 'ZZZZ'.                                    "2106636
   Else.                                                       "2106636
      Iprot-bukrs = '9999'.                                    "2106636
   ENDIF.                                                      "2106636
   Iprot-koart = 'Z'.                                          "2106636
   Iprot-buzei = '303'.                                        "2106636
   Iprot-msgid = 'I'.
   Write sy-datum dd/mm/yyyy to Ld_date.
   Write sy-uzeit to Ld_time.
   Concatenate text-303 Ld_date '        ' text-304 ld_time
   Into Iprot-Text0 separated by ' '.
   Collect Iprot.
   Sort Iprot.
   Loop at Iprot.
      If Iprot-koart = 'Z'.                                    "2106636
        Clear Iprot-bukrs.                                     "2106636
        Clear Iprot-koart.                                     "2106636
        Clear Iprot-buzei.                                     "2106636
      Endif.                                                   "2106636
      Case Iprot-Msgid.
      When char_s
      or   char_i.
         Write:    /01(01) sy-vline,
                           Iprot-Text0 under text-207.
         If X_zprt_sub = 'X'.
            Write: 104(01) sy-vline,
                   110(01) sy-vline,
                   116(01) sy-vline,
                   127(01) sy-vline,
                   138(01) sy-vline,
                   149(01) sy-vline.
         Endif.
         Write     166(01) sy-vline.
      When char_l.
         Write:/01(01) sy-vline,
                  (05) Iprot-Bukrs under text-200,
                  (05) Iprot-Koart under text-203,
                  (10) Iprot-Hkont under text-215,
                  (10) Iprot-Konto under text-204,
                       Iprot-Text0 under text-207,
               104(01) sy-vline,
               110(01) sy-vline,
               116(01) sy-vline,
               127(01) sy-vline,
               138(01) sy-vline,
               149(01) sy-vline,
               166(01) sy-vline.
      When char_x.
         Write:/01(01) sy-vline,
                  (05) Iprot-Bukrs under text-200,
                  (05) Iprot-Koart under text-203,
                  (10) Iprot-Hkont under text-215,
                  (10) Iprot-Konto under text-204,
                       Iprot-Belnr under text-205,
                       Iprot-Buzei under text-206,
                       Iprot-Text0 under text-207,
               104(01) sy-vline,
               110(01) sy-vline,
               116(01) sy-vline,
               127(01) sy-vline,
               138(01) sy-vline,
               149(01) sy-vline,
               166(01) sy-vline.
      Endcase.
      At last.
         Uline.
      Endat.
   Endloop.
   Describe table Gt_auslwaer lines sy-tfill.
   Check sy-tfill > 0.

   Read table Gt_auslwaer with key Msgty = 'E'.
   If sy-subrc = 0.
      Write: /01(01) sy-vline,
              02     text-305,
             166(01) sy-vline.
      Loop at Gt_auslwaer where Msgty = 'E'.
         Concatenate Gt_auslwaer-Waers Ld_char80
         into Ld_char80 separated by space.
      Endloop.
      Write: /01(01) sy-vline,
                     Ld_char80,
             132(01) sy-vline.
      Write: /01(01) sy-vline,
              02     text-306,
             166(01) sy-vline.
   Else.
      Write: /01(01) sy-vline,
              02     text-307,
             166(01) sy-vline.
      Loop at Gt_auslwaer where Msgty = 'W'.
         Concatenate Gt_auslwaer-Waers Ld_char80
         into Ld_char80 separated by space.
      Endloop.
      Write: /01(01) sy-vline,
                     Ld_char80,
             166(01) sy-vline.
   Endif.
   Uline.
Endform.                    "acc_ausgabe_addition_log

*&--------------------------------------------------------------------*
*&      Form  acc_ausgabe_fault_log
*&--------------------------------------------------------------------*
*       completely reworked by note 1029245
*---------------------------------------------------------------------*
Form Acc_ausgabe_fault_log.
   Flg_liste = '5'.
   New-page.
   If X_fehl_sub = 'X'.
      Sort Fehlprot by Bukrs Koart Konto Hkont Waers.
      Loop at Fehlprot.
         At new Waers.
            Format color col_group intensified off.
            Write: /01(01) sy-vline,
                      (05) Fehlprot-Bukrs under text-200,
                      (05) Fehlprot-Koart under text-203,
                      (10) Fehlprot-Konto under text-204,
                      (10) Fehlprot-Hkont under text-215,
                      (05) Fehlprot-Waers under text-216,
                   166(01) sy-vline.
            Format color col_normal intensified off.
         Endat.
         Write:    /01(01) sy-vline,
                      (08) Fehlprot-Dyname under text-229,
                      (04) Fehlprot-Dynumb under text-218,
                      (03) Fehlprot-Msgtyp under text-219,
                      (03) Fehlprot-Msgid  under text-220,
                      (03) Fehlprot-Msgnr  under text-221,
                      (99) Fehlprot-Text   under text-226,
                   166(01) sy-vline.
      Endloop.
      Uline.
   Else.
      Clear Fehlprot.
      If X_echtl = 'X'.
         If X_ausglv = 'X'.
            Fehlprot-Text = text-128.  " Beim Ausgleichen keine Fehler
         Else.
            Fehlprot-Text = text-130.  " Es wurden keine Ausgl.vorg.
         Endif.
      Else.
         Fehlprot-Text = text-129." Im Testl keine Fehl. beim Ausgl.
      Endif.
*      If No_tcode = 'X'. "No tcode found in RFDT.               "1274109
*         Message e029(Fagl_posting) into Fehlprot-Text.         "1274109
*      Endif.                                                    "1274109
      Format color col_normal intensified off.
      Write: /01 sy-vline,
                 Fehlprot-Text under text-098,
             166 sy-vline.
      Uline.
   Endif.
Endform.                    "acc_ausgabe_fault_log
*&--------------------------------------------------------------------*
*&      Form  log_top_of_list_fault
*&--------------------------------------------------------------------*
*       completely reworked by note 1029245
*---------------------------------------------------------------------*
Form Log_top_of_list_fault.
   Bhdgd-Inifl = 0.
   Describe table i001 lines sy-tfill.
   If sy-tfill > 1.
      Clear Bhdgd-Bukrs.
   Endif.
   Bhdgd-Line1 = sy-title.
   If X_echtl = char_x.
      Concatenate text-007 text-098 into Bhdgd-Line2 separated by ' '.
   Else.
      Concatenate text-008 text-098 into Bhdgd-Line2 separated by ' '.
   Endif.
   Perform Batch-heading(Rsbtchh0).
   Uline.
   If X_fehl_sub = 'X'.
      Format color col_group intensified on.
      Write:/01(01) sy-vline,
             02(05) text-200,          "Buchungskreis
             08(05) text-203,          "Kontoart
             14(10) text-204,          "Kontonummer (Debi, Kredi, Sako)
             25(10) text-215,          "Abstimmkontonummer
             36(05) text-212,
            166(01) sy-vline.
      Format color col_heading intensified on.
      Write:/01(01) sy-vline,
             02(08) text-229,
             11(04) text-218,
             16(03) text-219,
             21(03) text-220,
             25(03) text-221,
             29(99) text-226,
            166(01) sy-vline.
   Else.
      Format color col_heading intensified on.
      Write:/01(01) sy-vline,
             02(90) text-098,
            166(01) sy-vline.
   Endif.
   Uline.
Endform.                    "log_top_of_list_fault
*&---------------------------------------------------------------------*
*&      Form  START_SCHEDMAN
*&---------------------------------------------------------------------*
*       created by note 1081370
*----------------------------------------------------------------------*
Form Start_schedman.
   Data: Ls_detail  like Schedman_detail_user,
         Lt_selkrit like Schedman_selkrit occurs 0 with header line,
         Lt_param   like Schedman_selkrit occurs 3 with header line,
         Ls_witem   like scma_witem.

   Ls_detail-Repid = sy-repid.
   Ls_detail-Activity = '16'.
   Ls_detail-Testflag = X_testl.
   Lt_param-Structure = 'RFPDO'.
   Lt_param-Optio = 'EQ'.
   Lt_selkrit-Structure = 'BSEG'.
   Lt_selkrit-Field     = 'BUKRS'.
   Loop at Bukrx.
      Move-corresponding Bukrx to Lt_selkrit.
      Append Lt_selkrit.
   Endloop.
   Lt_selkrit-Structure = 'BSEG'.
   Lt_selkrit-Field     = 'GJAHR'.
   Loop at Gjahx.
      Move-corresponding Gjahx to Lt_selkrit.
      Append Lt_selkrit.
   Endloop.
   Lt_selkrit-Structure = 'BSIS'.
   Lt_selkrit-Field     = 'ZUONR'.
   Loop at So_zuonr.
      Move-corresponding So_zuonr to Lt_selkrit.
      Append Lt_selkrit.
   Endloop.
   Lt_selkrit-Structure = 'BSEG'.
   Lt_selkrit-Field     = 'BELNR'.
   Loop at Docnr.
      Move-corresponding Docnr to Lt_selkrit.
      Append Lt_selkrit.
   Endloop.
   Lt_selkrit-Structure = 'BKPF'.
   Lt_selkrit-Field     = 'BUDAT'.
   Loop at Postdate.
      Move-corresponding Postdate to Lt_selkrit.
      Append Lt_selkrit.
   Endloop.
   If X_kunnr = 'X'.
      Clear Lt_selkrit.
      Lt_selkrit-Structure = 'BSEG'.
      Lt_selkrit-Field     = 'KUNNR'.
      Loop at Kontd.
         Move-corresponding Kontd to Lt_selkrit.
         Append Lt_selkrit.
      Endloop.
      If sy-subrc <> 0.
         Lt_selkrit-Sign = 'I'.
         Lt_selkrit-Optio = 'EQ'.
         Append Lt_selkrit.
      Endif.
      If X_shbkn = 'X'.
         Clear Lt_selkrit.
         Lt_selkrit-Structure = 'BSEG'.
         Lt_selkrit-Field     = 'UMSKZ'.
         Loop at Shbkd.
            Move-corresponding Shbkd to Lt_selkrit.
            Append Lt_selkrit.
         Endloop.
         If sy-subrc <> 0.
            Lt_selkrit-Sign = 'I'.
            Lt_selkrit-Optio = 'EQ'.
            Append Lt_selkrit.
         Endif.
      Endif.
   Endif.
   If X_lifnr = 'X'.
      Clear Lt_selkrit.
      Lt_selkrit-Structure = 'BSEG'.
      Lt_selkrit-Field     = 'LIFNR'.
      Loop at Kontk.
         Move-corresponding Kontk to Lt_selkrit.
         Append Lt_selkrit.
      Endloop.
      If sy-subrc <> 0.
         Lt_selkrit-Sign = 'I'.
         Lt_selkrit-Optio = 'EQ'.
         Append Lt_selkrit.
      Endif.
      If X_shblf = 'X'.
         Clear Lt_selkrit.
         Lt_selkrit-Structure = 'BSEG'.
         Lt_selkrit-Field     = 'UMSKZ'.
         Loop at Shbkk.
            Move-corresponding Shbkk to Lt_selkrit.
            Append Lt_selkrit.
         Endloop.
         If sy-subrc <> 0.
            Lt_selkrit-Sign = 'I'.
            Lt_selkrit-Optio = 'EQ'.
            Append Lt_selkrit.
         Endif.
      Endif.
   Endif.
   If X_saknr = 'X'.
      Clear Lt_selkrit.
      Lt_selkrit-Structure = 'BSEG'.
      Lt_selkrit-Field     = 'SAKNR'.
      Loop at Konts.
         Move-corresponding Konts to Lt_selkrit.
         Append Lt_selkrit.
      Endloop.
      If sy-subrc <> 0.
         Lt_selkrit-Sign = 'I'.
         Lt_selkrit-Optio = 'EQ'.
         Append Lt_selkrit.
      Endif.
      Lt_param-Entry = 1.
      Lt_param-Field = 'F124SOBE'.
      Lt_param-Low = Xsobwere.
      Append Lt_param.
   Endif.
   Lt_selkrit-Structure = 'BKPF'.
   Lt_selkrit-Field = 'LDGRP'.
   Loop at P_lg.
      Move-corresponding P_lg to Lt_selkrit.
      Append Lt_selkrit.
   Endloop.
   If Xaugdt = 'X'.
      Lt_param-Entry = 2.
      Lt_param-Field = 'F124DATE'.
      Lt_param-Low   = Xaugdt.
      Append Lt_param.
   Else.
      Lt_param-Entry = 2.
      Lt_param-Structure = 'BSEG'.
      Lt_param-Field = 'AUGDT'.
      Lt_param-Low = Augdt.
      Append Lt_param.
   Endif.
   If not Zwaers is initial.
      Lt_param-Entry = 3.
      Lt_param-Structure = 'BSIS'.
      Lt_param-Field     = 'WAERS'.
      Append Lt_param.
   Endif.
   Lt_param-Structure = 'RFPDO'.
   If not Xsobebvk is initial.
      Lt_param-Entry = 3.
      Lt_param-Field = 'F124BVK'.
      Append Lt_param.
   Endif.
   Clear Ls_witem.
   Ls_witem-Wf_witem = Wf_witem.
   Ls_witem-Wf_wlist = Wf_wlist.
   Call function 'KPEP_MONI_INIT_RECORD'
   exporting Ls_detail  = Ls_detail
             Ls_witem   = Ls_witem
   importing Ls_key     = Gs_key
   tables    Lt_selkrit = Lt_selkrit
             Lt_param   = Lt_param.
Endform.                    " START_SCHEDMAN
*&---------------------------------------------------------------------*
*&      Form  END_SCHEDMAN
*&---------------------------------------------------------------------*
*       created by note 1081370
*----------------------------------------------------------------------*
Form End_schedman.
   Data: Ld_aplstat    like Smmain-Aplstat value '0',
         Ls_scma_event like Scma_event.
   Include Schedman_events.

   Loop at Fehlprot where Msgtyp <> 'S'.
      Case Fehlprot-Msgtyp.
      When 'W'.
         Ld_aplstat = '2'.
      When 'E'.
         Ld_aplstat = '4'.
         Exit.
      Endcase.
   Endloop.
   Ls_scma_event-Wf_event = Cs_wf_events-Finished.
   Ls_scma_event-Wf_witem = Wf_witem.
   Ls_scma_event-Wf_okey  = Wf_okey.
   Call function 'KPEP_MONI_CLOSE_RECORD'
   exporting Ls_key        = Gs_key
             Ls_scma_event = Ls_scma_event
   changing  Ld_aplstat    = Ld_aplstat.
Endform.                    " END_SCHEDMAN
