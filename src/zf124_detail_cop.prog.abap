*&---------------------------------------------------------------------*
*&  Include           F124_DETAIL_LIST
*&
*& This include contains data structures and form routines needed by
*&   the reports sapf124 for displaying the detaillists
*&   showing the clearing info.
*&---------------------------------------------------------------------*
                                                                "1029245
*&--------------------------------------------------------------------*
*&      Form  acc_append_data
*&--------------------------------------------------------------------*
Form Acc_append_data using Id_itab    type c                    "1029245
                           Id_augdt   like Bkpf-Budat           "1029245
                           Id_augblnr like bdcmsgcoll-msgv1     "1029245
                           Id_message type string.              "1029245

   Data: Ld_augblnr type bdcmsgcoll-msgv1,                      "1029245
         Ld_augdt   type Bkpf-Budat.                            "1029245

   Ld_augdt = Id_augdt.                                         "1029245
   If Id_augblnr = ''.                                          "1029245
      Case Id_message.                                          "1029245
      When 'NOCLEAR'                                            "1029245
      or   'NOCLEARTOL'                                         "1029245
      or   'ENQ'                                                "1029245
      or   'NOTZERO'.                                           "1029245
         Clear: Ld_augdt, Ld_augblnr.                           "1029245
      Endcase.
   Else.
      Ld_augblnr = Id_augblnr.                                  "1029245
   Endif.
   If ( Id_message = 'NOCLEAR'                                  "1029245
     or Id_message = 'NOCLEARTOL'                               "1029245
     or Id_message = 'ENQ' )                                    "1029245
   and X_fehler = 'X'.
     Perform Record_fill                                        "1029245
     using Id_message Gs_dlist-Bukrs Id_itab Gs_dlist-Accnr     "1029245
           Gs_dlist-Hkont Gs_dlist-Belnr Gs_dlist-Buzei char_x. "1029245
   Endif.
   If  ( X_testl = 'X' )                                        "1029245
   and ( ( Xausbel = 'X' )  and ( Ld_augdt is not initial )     "1029245
      or ( Xnausbel = 'X' ) and ( Ld_augdt is initial     ) )   "1029245
   or  ( X_testl = ' ' )                                        "1029245
   and ( ( Xausbel = 'X' )  and ( Ld_augblnr is not initial )   "1029245
      or ( Xnausbel = 'X' ) and ( Ld_augblnr is initial     ) )."1029245
      Perform Write_liste                                       "1029245
      using Id_itab Ld_augdt Ld_augblnr.                        "1029245
   Endif.                                                       "1029245
   Refresh Gt_dlist.                                            "1029245
Endform.                    "acc_append_data                    "1029245

*&--------------------------------------------------------------------*
*&      Form  acc_top_of_page
*&--------------------------------------------------------------------*
*&      completely reworked by note 1029245
*&--------------------------------------------------------------------*
Form Acc_top_of_page using Id_koart like Bseg-Koart.
   Statics: St_dfies type standard table of Dfies
            with header line.

   If St_dfies[] is initial.
      Call function 'DDIF_FIELDINFO_GET'
      exporting Tabname   = 'FAGL_S_SAPF124_LIST1'
      tables    Dfies_tab = St_dfies.
   Endif.
   Clear Bhdgd-line2.
   If X_echtl eq char_x.
      Concatenate text-007 text-030 into Bhdgd-Line2 separated by ' '.
   Else.
      Concatenate text-008 text-030 into Bhdgd-Line2 separated by ' '.
   Endif.
   Case Id_koart.
   When 'D'.
      Bhdgd-Bukrs = Tdebi-Bukrs.
   When 'K'.
      Bhdgd-Bukrs = Tkredi-Bukrs.
   When 'S'.
      Bhdgd-Bukrs = Tsako-Bukrs.
   Endcase.
   Perform Batch-heading(Rsbtchh0).
   Perform Acc_fill_header using Id_koart.
   Format color col_heading intensified."
   Uline.
   Write:/01(01) sy-vline.
   Loop at St_dfies.
      Case St_dfies-Fieldname.
      When 'BELNR'.
         Write: 02(10) St_dfies-Reptext,
                12(01) sy-vline.
      When 'BUZEI'.
         Write: 13(03) St_dfies-Reptext,
                16(01) sy-vline.
      When 'AUGDT'.
         Write: 17(10) St_dfies-Reptext,
                27(01) sy-vline.
      When 'AUGBL'.
         Write: 28(10) St_dfies-Reptext,
                38(01) sy-vline.
      When 'UMSKZ'.
         Write: 39(02) St_dfies-Reptext,
                41(01) sy-vline.
      When 'WAERS'.
         Write: 42(05) St_dfies-Reptext,
                47(01) sy-vline.
      When 'WRBTR'.
         Write: 48(31) St_dfies-Reptext,
                79(01) sy-vline.
      Endcase.
   Endloop.
   Case Id_koart.
   When 'D'.
      If not Xbsidgr-Krit1 is initial.
         Read table Itext with key Kritx = Xbsidgr-Krit1 binary search.
         Write 80(24) Itext-Textx.
      Endif.
      Write 104(01) sy-vline.
      If not Xbsidgr-Krit2 is initial.
         Read table Itext with key Kritx = Xbsidgr-Krit2 binary search.
         Write 105(18) Itext-Textx.
      Endif.
      Write 123(01) sy-vline.
      If not Xbsidgr-Krit3 is initial.
         Read table Itext with key Kritx = Xbsidgr-Krit3 binary search.
         Write 124(16) Itext-Textx.
      Endif.
      Write 140(01) sy-vline.
      If not Xbsidgr-Krit4 is initial.
         Read table Itext with key Kritx = Xbsidgr-Krit4 binary search.
         Write 141(14) Itext-Textx.
      Endif.
      Write 155(01) sy-vline.
      If not Xbsidgr-Krit5 is initial.
         Read table Itext with key Kritx = Xbsidgr-Krit5 binary search.
         Write 156(10) Itext-Textx.
      Endif.
   When 'K'.
      If not Xbsikgr-Krit1 is initial.
         Read table Itext with key Kritx = Xbsikgr-Krit1 binary search.
         Write 80(24) Itext-Textx.
      Endif.
      Write 104(01) sy-vline.
      If not Xbsikgr-Krit2 is initial.
         Read table Itext with key Kritx = Xbsikgr-Krit2 binary search.
         Write 105(18) Itext-Textx.
      Endif.
      Write 123(01) sy-vline.
      If not Xbsikgr-Krit3 is initial.
         Read table Itext with key Kritx = Xbsikgr-Krit3 binary search.
         Write 124(16) Itext-Textx.
      Endif.
      Write 140(01) sy-vline.
      If not Xbsikgr-Krit4 is initial.
         Read table Itext with key Kritx = Xbsikgr-Krit4 binary search.
         Write 141(14) Itext-Textx.
      Endif.
      Write 155(01) sy-vline.
      If not Xbsikgr-Krit5 is initial.
         Read table Itext with key Kritx = Xbsikgr-Krit5 binary search.
         Write 156(10) Itext-Textx.
      Endif.
   When 'S'.
      If not Xbsisgr-Krit1 is initial.
         Read table Itext with key Kritx = Xbsisgr-Krit1 binary search.
         Write 80(24) Itext-Textx.
      Endif.
      Write 104(01) sy-vline.
      If not Xbsisgr-Krit2 is initial.
         Read table Itext with key Kritx = Xbsisgr-Krit2 binary search.
         Write 105(18) Itext-Textx.
      Endif.
      Write 123(01) sy-vline.
      If not Xbsisgr-Krit3 is initial.
         Read table Itext with key Kritx = Xbsisgr-Krit3 binary search.
         Write 124(16) Itext-Textx.
      Endif.
      Write 140(01) sy-vline.
      If not Xbsisgr-Krit4 is initial.
         Read table Itext with key Kritx = Xbsisgr-Krit4 binary search.
         Write 141(14) Itext-Textx.
      Endif.
      Write 155(01) sy-vline.
      If not Xbsisgr-Krit5 is initial.
         Read table Itext with key Kritx = Xbsisgr-Krit5 binary search.
         Write 156(10) Itext-Textx.
      Endif.
   Endcase.
   Write: 166(01) sy-vline.
   Uline.
Endform.                    "top_of_list
*&--------------------------------------------------------------------*
*&      Form  acc_fill_header
*&--------------------------------------------------------------------*
*&      completely reworked by note 1029245
*---------------------------------------------------------------------*
Form Acc_fill_header using Id_koart like Bseg-Koart.
   Data: Ls_dfies type Dfies.

   Write: / '*'.
   Call function 'DDIF_FIELDINFO_GET'
   exporting Tabname   = 'KNB1'
             Fieldname = 'BUKRS'
   tables    Dfies_tab = Dfiestab.
   Read table Dfiestab index 1 into Ls_dfies.
   Write: / Ls_dfies-Scrtext_m.
   Case Id_koart.
   When 'D'.
      Write Tdebi-Bukrs intensified off.
   When 'K'.
      Write Tkredi-Bukrs intensified off.
   When 'S'.
      Write Tsako-Bukrs intensified off.
   Endcase.
   Call function 'DDIF_FIELDINFO_GET'
   exporting Tabname   = 'BSEG'
             Fieldname = 'KOART'
   tables    Dfies_tab = Dfiestab.
   Read table Dfiestab index 1 into Ls_dfies.
   Write: / Ls_dfies-Scrtext_m, Id_koart intensified off.
   Call function 'DDIF_FIELDINFO_GET'
   exporting Tabname   = 'RFXPO'
             Fieldname = 'KONTO'
   tables    Dfies_tab = Dfiestab.
   Read table Dfiestab index 1 into Ls_dfies.
   Write: / Ls_dfies-Scrtext_m.
   Case Id_koart.
   When 'D'.
      Write Tdebi-Kunnr no-zero intensified off.
   When 'K'.
      Write Tkredi-Lifnr no-zero intensified off.
   When 'S'.
      Write Tsako-Hkont no-zero intensified off.
   Endcase.
   Call function 'DDIF_FIELDINFO_GET'
   exporting Tabname   = 'BSEG'
             Fieldname = 'HKONT'
   tables    Dfies_tab = Dfiestab.
   Read table Dfiestab index 1 into Ls_dfies.
   Write: / Ls_dfies-Scrtext_m.
   Case Id_koart.
   When 'D'.
      Write Xbsidgr-Hkont no-zero intensified off.
    When 'K'.
      Write Xbsikgr-Hkont no-zero intensified off.
    When 'S'.
      Write Xbsisgr-Hkont no-zero intensified off.
   Endcase.
Endform.                    "acc_fill_header

*&--------------------------------------------------------------------*
*&      Form  Write_liste
*&--------------------------------------------------------------------*
*&      completely new by note 1029245
*---------------------------------------------------------------------*
Form Write_liste using Id_koart   type c
                       Id_augdt   like Bkpf-Budat
                       Id_augblnr like Bdcmsgcoll-Msgv1.
   Data: Lb_betraege like Boole-Boole.

   Case Id_koart.
   When 'D'.
      Lb_betraege = Betraeged.
   When 'K'.
      Lb_betraege = Betraegek.
   When 'S'.
      Lb_betraege = Betraeges.
   Endcase.
   If x_lg ne 2.                                               "N1416585
     Loop at Gt_dlist into Gs_dlist.
      Perform Record_document_neu
      using Id_augdt
            Id_augblnr
            Gs_dlist-Belnr
            Gs_dlist-Buzei
            Gs_dlist-Umskz
            Gs_dlist-Xcurr
            Gs_dlist-Shkzg
            Gs_dlist-Xamnt
            Gs_dlist-Bedg1
            Gs_dlist-Bedg2
            Gs_dlist-Bedg3
            Gs_dlist-Bedg4
            Gs_dlist-Bedg5
            Lb_betraege
            Gs_dlist-Krit1
            Gs_dlist-Krit2
            Gs_dlist-Krit3
            Gs_dlist-Krit4
            Gs_dlist-Krit5
            Gs_dlist-Waer1                                      "1817592
            Gs_dlist-Waer2                                      "1817592
            Gs_dlist-Waer3                                      "1817592
            Gs_dlist-Waer4                                      "1817592
            Gs_dlist-Waer5.                                     "1817592

   Endloop.
   Perform Check_summe using Id_koart Id_augdt Id_augblnr.
   ELSE.                                                       "N1416585
     Loop at Gt_dlist into Gs_dlist.
      Perform Record_document_lg
      using Id_augdt
            Id_augblnr
            Gs_dlist-Belnr
            Gs_dlist-Buzei
            Gs_dlist-Ldgrp
            Gs_dlist-Xcurr
            Gs_dlist-Shkzg
            Gs_dlist-Xamnt
            Gs_dlist-Bedg1
            Gs_dlist-Bedg2
            Gs_dlist-Bedg3
            Gs_dlist-Bedg4
            Gs_dlist-Bedg5
            Lb_betraege
            Gs_dlist-Krit1
            Gs_dlist-Krit2
            Gs_dlist-Krit3
            Gs_dlist-Krit4
            Gs_dlist-Krit5
            Gs_dlist-Waer1                                     "1817592
            Gs_dlist-Waer2                                     "1817592
            Gs_dlist-Waer3                                     "1817592
            Gs_dlist-Waer4                                     "1817592
            Gs_dlist-Waer5.                                    "1817592
     Endloop.
     Perform Check_summe_lg using Id_augdt Id_augblnr.
   ENDIF.                                                      "N1416585
Endform.
*&--------------------------------------------------------------------*
*&      Form  Record_document_neu
*&--------------------------------------------------------------------*
*&      completely new by note 1029245
*---------------------------------------------------------------------*
Form Record_document_neu using Id_augdt like Bsid-Augdt
                               Id_augbl like Bdcmsgcoll-Msgv1
                               Id_belnr like Bsid-Belnr
                               Id_buzei like Bsid-Buzei
                               Id_umskz like Bsid-Umskz
                               Id_waers like Bsid-Waers
                               Id_shkzg like Bsid-Shkzg
                               Id_betra type Wrbtr_x8
                               Id_bedg1 like Xbsid-Bedg1
                               Id_bedg2 like Xbsid-Bedg2
                               Id_bedg3 like Xbsid-Bedg3
                               Id_bedg4 like Xbsid-Bedg4
                               Id_bedg5 like Xbsid-Bedg5
                               Ib_betraege type c
                               Id_krit1 like Xbsid-Krit1
                               Id_krit2 like Xbsid-Krit2
                               Id_krit3 like Xbsid-Krit3
                               Id_krit4 like Xbsid-Krit4
                               Id_krit5 like Xbsid-Krit5
                               Id_waer1 type waers              "1817592
                               Id_waer2 type waers              "1817592
                               Id_waer3 type waers              "1817592
                               Id_waer4 type waers              "1817592
                               Id_waer5 type waers.             "1817592

   Data: Ld_betragn like Bsega-Wrshb.

   If Id_shkzg = 'S'.
      Ld_betragn = Id_betra.
   Else.
      Ld_betragn = - Id_betra.
   Endif.
   Format color col_key intensified.
   Write:    /01(01) sy-vline,
              02(10) Id_belnr,
              12(01) sy-vline,
              13(03) Id_buzei,
              16(01) sy-vline.
   Format color col_normal intensified off.
   If not Id_augdt is initial.
      Write   17(10) Id_augdt color col_positive intensified on.
   Endif.
   Write:     27(01) sy-vline.
   If not Id_augbl is initial.
      Write   28(10) Id_augbl color col_positive intensified on.
   Endif.
   Write:     38(01) sy-vline,
              39(02) Id_umskz,
              41(01) sy-vline,
              42(05) Id_waers,
              47(01) sy-vline,
              48(31) Ld_betragn currency Id_waers,
              79(01) sy-vline.
   If Ib_betraege = ' '.
      Write:  80(24) Id_bedg1,
              104(01) sy-vline,
              105(18) Id_bedg2,
             123(01) sy-vline,
             124(16) Id_bedg3,
             140(01) sy-vline,
             141(14) Id_bedg4,
             155(01) sy-vline,
             156(10) Id_bedg5,
             166(01) sy-vline.
   Else.
      If Id_krit1 ne space and Id_bedg1 ne space.
         Perform Betrag_ausgeben using Id_krit1 Id_bedg1 Id_waer1 '1'.
      Else.
         Write 80(24) Id_bedg1.
      Endif.
      Write 104(01) sy-vline.
      If Id_krit2 ne space and Id_bedg2 ne space.
         Perform Betrag_ausgeben using Id_krit2 Id_bedg2 Id_waer2 '2'.
      Else.
         Write 105(18) Id_bedg2.
      Endif.
      Write 123(01) sy-vline.
      If Id_krit3 ne space and Id_bedg3 ne space.
         Perform Betrag_ausgeben using Id_krit3 Id_bedg3 Id_waer3 '3'.
      Else.
         Write 124(16) Id_bedg3.
      Endif.
      Write 140(01) sy-vline.
      If Id_krit4 ne space and Id_bedg4 ne space.
         Perform Betrag_ausgeben using Id_krit4 Id_bedg4 Id_waer4 '4'.
      Else.
         Write 141(14) Id_bedg4.
      Endif.
      Write 155(01) sy-vline.
      If Id_krit5 ne space and Id_bedg5 ne space.
         Perform Betrag_ausgeben using Id_krit5 Id_bedg5 Id_waer5 '5'.
      Else.
         Write 156(10) Id_bedg5.
      Endif.
      Write 166(01) sy-vline.
   Endif.
Endform.                               " RECORD_DOCUMENT_NEU
*&--------------------------------------------------------------------*
*&      Form  Betrag_ausgeben
*&--------------------------------------------------------------------*
*&      completely new by note 1029245
*&      change by note 1817592:
*&      Form Check_betragsfelder now called in Form DETERMINE_CUR
*&      in order to populate gs_dlist-waer* =  Id_waers
*---------------------------------------------------------------------*
Form Betrag_ausgeben using Id_krit Id_wert Id_waers Char_1.
*----- Betragsfelder müssen währungsgerecht dargestellt werden ---------
*----- nur Felder aus BSIS, BSIK, BSID mit Referenztabelle aus diesen
*----- Tabellen oder T001 werden währungsgerecht dargestellt -----------
*----- Bei anderen Feldern wäre das Nachlesen der Währung zu teuer -----

*   Perform Check_betragsfelder using Id_krit.                 "1817592
   IF NOT Id_waers IS INITIAL.
*   If X_betrag = 'X'.                                         "1817592
      Betragfeld = Id_wert.
*      Assign (Curr) to <Fb>.
*      Write <Fb> to Currw.
      Case Char_1.
      When '1'.
         Write:  80(24) Betragfeld currency Id_waers.
      When '2'.
         Write:  105(18) Betragfeld currency Id_waers.
      When '3'.
         Write: 124(16) Betragfeld currency Id_waers.
      When '4'.
         Write: 141(14) Betragfeld currency Id_waers.
      When '5'.
         Write: 156(10) Betragfeld Currency Id_waers.
      Endcase.
   Else.
      Case Char_1.
      When '1'.
         Write:  80(24) Id_wert.
      When '2'.
         Write:  105(18) Id_wert.
      When '3'.
         Write: 124(16) Id_wert.
      When '4'.
         Write: 141(14) Id_wert.
      When '5'.
         Write: 156(10) Id_wert.
      Endcase.
   Endif.
Endform.                               " BETRAG_AUSGEBEN
*&--------------------------------------------------------------------*
*&      Form  Check_summe
*&--------------------------------------------------------------------*
*&      completely new by note 1029245
*---------------------------------------------------------------------*
Form Check_summe using Id_koart type c
                       Id_augdt like Bkpf-Budat
                       Id_augbl like Bdcmsgcoll-Msgv1.

   Format color col_total intensified off.
   Write:      /01(01) sy-vline,
                02(10) char_*,
                12(01) sy-vline,
                13(03) space,
                16(01) sy-vline.
   If not Id_augdt is initial.
      Write     17(10) Id_augdt.
   Endif.
   Write:       27(01) sy-vline,
                28(10) Id_augbl,
                38(01) sy-vline.

   Case Id_koart.
   When 'D'.
      If not Min_bel is initial and X_echtl = 'X'.             "N1445228
        clear: Xbsidgr-Bedg1, Xbsidgr-Bedg2, Xbsidgr-Bedg3,
               Xbsidgr-Bedg4, Xbsidgr-Bedg5.
      Endif.                                                   "N1445228
      Write:    39(02) Xbsidgr-Umskz,
                41(01) sy-vline,
                42(05) Xbsidgr-Xcurr,
                47(01) sy-vline,
                48(31) Xbsidgr-Bet_bw currency Xbsidgr-Xcurr,
                79(01) sy-vline.
      If Betraeged = ' '.
         Write: 80(24) Xbsidgr-Bedg1,
                104(01) sy-vline,
                105(18) Xbsidgr-Bedg2,
               123(01) sy-vline,
               124(16) Xbsidgr-Bedg3,
               140(01) sy-vline,
               141(14) Xbsidgr-Bedg4,
               155(01) sy-vline,
               156(10) Xbsidgr-Bedg5,
               166(01) sy-vline.
      Else.
         If Xbsidgr-Krit1 ne space and Xbsidgr-Bedg1 ne space.
            Perform Betrag_ausgeben
            using Xbsidgr-Krit1 Xbsidgr-Bedg1
                  Gs_dlist-Waer1 '1'.                           "1817592
         Else.
            Write 80(24) Xbsidgr-Bedg1.
         Endif.
         Write 104(01) sy-vline.
         If Xbsidgr-Krit2 ne space and Xbsidgr-Bedg2 ne space.
            Perform Betrag_ausgeben
            using Xbsidgr-Krit2 Xbsidgr-Bedg2
                  Gs_dlist-Waer2 '2'.                           "1817592
         Else.
            Write 105(18) Xbsidgr-Bedg2.
         Endif.
         Write 123(01) sy-vline.
         If Xbsidgr-Krit3 ne space and Xbsidgr-Bedg3 ne space.
            Perform Betrag_ausgeben
            using Xbsidgr-Krit3 Xbsidgr-Bedg3
                  Gs_dlist-Waer3 '3'.                           "1817592
         Else.
            Write 124(16) Xbsidgr-Bedg3.
         Endif.
         Write 140(01) sy-vline.
         If Xbsidgr-Krit4 ne space and Xbsidgr-Bedg4 ne space.
            Perform Betrag_ausgeben
            using Xbsidgr-Krit4 Xbsidgr-Bedg4
                  Gs_dlist-Waer4 '4'.                          "1817592
         Else.
            Write 141(14) Xbsidgr-Bedg4.
         Endif.
         Write 155(01) sy-vline.
         If Xbsidgr-Krit5 ne space and Xbsidgr-Bedg5 ne space.
            Perform Betrag_ausgeben
            using Xbsidgr-Krit5 Xbsidgr-Bedg5
                  Gs_dlist-Waer5 '5'.                          "1817592
         Else.
            Write 156(10) Xbsidgr-Bedg5.
         Endif.
         Write 166(01) sy-vline.
      Endif.
   When 'K'.
      If not Min_bel is initial and X_echtl = 'X'.             "N1445228
        clear: Xbsikgr-Bedg1, Xbsikgr-Bedg2, Xbsikgr-Bedg3,
               Xbsikgr-Bedg4, Xbsikgr-Bedg5.
      Endif.                                                   "N1445228
      Write:    39(02) Xbsikgr-Umskz,
                41(01) sy-vline,
                42(05) Xbsikgr-Xcurr,
                47(01) sy-vline,
                48(31) Xbsikgr-Bet_bw currency Xbsikgr-Xcurr,
                79(01) sy-vline.
      If Betraegek = ' '.
         Write: 80(24) Xbsikgr-Bedg1,
                104(01) sy-vline,
                105(18) Xbsikgr-Bedg2,
               123(01) sy-vline,
               124(16) Xbsikgr-Bedg3,
               140(01) sy-vline,
               141(14) Xbsikgr-Bedg4,
               155(01) sy-vline,
               156(10) Xbsikgr-Bedg5,
               166(01) sy-vline.
      Else.
         If Xbsikgr-Krit1 ne space and Xbsikgr-Bedg1 ne space.
            Perform Betrag_ausgeben
            using Xbsikgr-Krit1 Xbsikgr-Bedg1
                  Gs_dlist-Waer1 '1'.                           "1817592
         Else.
            Write 80(24) Xbsikgr-Bedg1.
         Endif.
         Write 104(01) sy-vline.
         If Xbsikgr-Krit2 ne space and Xbsikgr-Bedg2 ne space.
            Perform Betrag_ausgeben
            using Xbsikgr-Krit2 Xbsikgr-Bedg2
                  Gs_dlist-Waer2 '2'.                           "1817592
         Else.
            Write 105(18) Xbsikgr-Bedg2.
         Endif.
         Write 123(01) sy-vline.
         If Xbsikgr-Krit3 ne space and Xbsikgr-Bedg3 ne space.
            Perform Betrag_ausgeben
            using Xbsikgr-Krit3 Xbsikgr-Bedg3
                  Gs_dlist-Waer3 '3'.                           "1817592
         Else.
            Write 124(16) Xbsikgr-Bedg3.
         Endif.
         Write 140(01) sy-vline.
         If Xbsikgr-Krit4 ne space and Xbsikgr-Bedg4 ne space.
            Perform Betrag_ausgeben
            using Xbsikgr-Krit4 Xbsikgr-Bedg4
                  Gs_dlist-Waer4 '4'.                           "1817592
         Else.
            Write 141(14) Xbsikgr-Bedg4.
         Endif.
         Write 155(01) sy-vline.
         If Xbsikgr-Krit5 ne space and Xbsikgr-Bedg5 ne space.
            Perform Betrag_ausgeben
            using Xbsikgr-Krit5 Xbsikgr-Bedg5
                  Gs_dlist-Waer5 '5'.                           "1817592
         Else.
            Write 156(10) Xbsikgr-Bedg5.
         Endif.
         Write 166(01) sy-vline.
      Endif.
   When 'S'.
      If not Min_bel is initial and X_echtl = 'X'.             "N1445228
        clear: Xbsisgr-Bedg1, Xbsisgr-Bedg2, Xbsisgr-Bedg3,
               Xbsisgr-Bedg4, Xbsisgr-Bedg5.
      Endif.                                                   "N1445228
      Write:    39(02) space,
                41(01) sy-vline,
                42(05) Xbsisgr-Xcurr,
                47(01) sy-vline,
                48(31) Xbsisgr-Bet_tw currency Xbsisgr-Xcurr,
                79(01) sy-vline.
      If Betraeges = ' '.
         Write: 80(24) Xbsisgr-Bedg1,
                104(01) sy-vline,
                105(18) Xbsisgr-Bedg2,
               123(01) sy-vline,
               124(16) Xbsisgr-Bedg3,
               140(01) sy-vline,
               141(14) Xbsisgr-Bedg4,
               155(01) sy-vline,
               156(10) Xbsisgr-Bedg5,
               166(01) sy-vline.
      Else.
         If Xbsisgr-Krit1 ne space and Xbsisgr-Bedg1 ne space.
            Perform Betrag_ausgeben
            using Xbsisgr-Krit1 Xbsisgr-Bedg1
                  Gs_dlist-Waer1 '1'.                           "1817592
         Else.
            Write 80(24) Xbsisgr-Bedg1.
         Endif.
         Write 104(01) sy-vline.
         If Xbsisgr-Krit2 ne space and Xbsisgr-Bedg2 ne space.
            Perform Betrag_ausgeben
            using Xbsisgr-Krit2 Xbsisgr-Bedg2
                  Gs_dlist-Waer2 '2'.                           "1817592
         Else.
            Write 105(18) Xbsisgr-Bedg2.
         Endif.
         Write 123(01) sy-vline.
         If Xbsisgr-Krit3 ne space and Xbsisgr-Bedg3 ne space.
            Perform Betrag_ausgeben
            using Xbsisgr-Krit3 Xbsisgr-Bedg3
                  Gs_dlist-Waer3 '3'.                           "1817592
         Else.
            Write 124(16) Xbsisgr-Bedg3.
         Endif.
         Write 140(01) sy-vline.
         If Xbsisgr-Krit4 ne space and Xbsisgr-Bedg4 ne space.
            Perform Betrag_ausgeben
            using Xbsisgr-Krit4 Xbsisgr-Bedg4
                  Gs_dlist-Waer4 '4'.                           "1817592
         Else.
            Write 141(14) Xbsisgr-Bedg4.
         Endif.
         Write 155(01) sy-vline.
         If Xbsisgr-Krit5 ne space and Xbsisgr-Bedg5 ne space.
            Perform Betrag_ausgeben
            using Xbsisgr-Krit5 Xbsisgr-Bedg5
                  Gs_dlist-Waer5 '5'.                           "1817592
         Else.
            Write 156(10) Xbsisgr-Bedg5.
         Endif.
         Write 166(01) sy-vline.
      Endif.
   Endcase.
   Uline.
Endform.                               " CHECK_SUMME
*-----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  ACC_TOP_OF_PAGE_LG
*&---------------------------------------------------------------------*
*       created by note 1416585
*       header in detail list for clearing specific to ledger group
*       similiar to Form Acc_top_of_page
*----------------------------------------------------------------------*
FORM ACC_TOP_OF_PAGE_LG .
   Statics: St_dfies type standard table of Dfies
            with header line.

   If St_dfies[] is initial.
      Call function 'DDIF_FIELDINFO_GET'
      exporting Tabname   = 'FAGL_S_SAPF124_LIST1'
      tables    Dfies_tab = St_dfies.
   Endif.
   Clear Bhdgd-line2.
   If X_echtl eq char_x.
      Concatenate text-007 text-030 into Bhdgd-Line2 separated by ' '.
   Else.
      Concatenate text-008 text-030 into Bhdgd-Line2 separated by ' '.
   Endif.
   Bhdgd-Bukrs = Tsako-Bukrs.
   Perform Batch-heading(Rsbtchh0).
   Perform Acc_fill_header using 'S'.
   Format color col_heading intensified."
   Uline.
   Write:/01(01) sy-vline.
   Loop at St_dfies.
      Case St_dfies-Fieldname.
      When 'BELNR'.
         Write: 02(10) St_dfies-Reptext,
                12(01) sy-vline.
      When 'BUZEI'.
         Write: 13(03) St_dfies-Reptext,
                16(01) sy-vline.
      When 'AUGDT'.
         Write: 17(10) St_dfies-Reptext,
                27(01) sy-vline.
      When 'AUGBL'.
         Write: 28(10) St_dfies-Reptext,
                38(01) sy-vline.
      When 'LDGRP'.
         Write: 39(04) St_dfies-Reptext,
                43(01) sy-vline.
      When 'WAERS'.
         Write: 44(05) St_dfies-Reptext,
                49(01) sy-vline.
      When 'WRBTR'.
         Write: 50(31) St_dfies-Reptext,
                81(01) sy-vline.
      Endcase.
   Endloop.
   If not Xbsisgr-Krit1 is initial.
     Read table Itext with key Kritx = Xbsisgr-Krit1 binary search.
     Write 82(20) Itext-Textx.
   Endif.
   Write 102(01) sy-vline.
   If not Xbsisgr-Krit2 is initial.
     Read table Itext with key Kritx = Xbsisgr-Krit2 binary search.
     Write 103(18) Itext-Textx.
   Endif.
   Write 121(01) sy-vline.
   If not Xbsisgr-Krit3 is initial.
     Read table Itext with key Kritx = Xbsisgr-Krit3 binary search.
     Write 122(16) Itext-Textx.
   Endif.
   Write 138(01) sy-vline.
   If not Xbsisgr-Krit4 is initial.
     Read table Itext with key Kritx = Xbsisgr-Krit4 binary search.
     Write 139(14) Itext-Textx.
   Endif.
   Write 153(01) sy-vline.
   If not Xbsisgr-Krit5 is initial.
     Read table Itext with key Kritx = Xbsisgr-Krit5 binary search.
     Write 154(12) Itext-Textx.
   Endif.
   Write: 166(01) sy-vline.
   Uline.


ENDFORM.                    " ACC_TOP_OF_PAGE_LG
*&---------------------------------------------------------------------*
*&      Form  RECORD_DOCUMENT_LG
*&---------------------------------------------------------------------*
*    completely new by note 1416585 clearing specific to ledgergroup
*----------------------------------------------------------------------*

FORM RECORD_DOCUMENT_LG  USING Id_augdt like Bsid-Augdt
                               Id_augbl like Bdcmsgcoll-Msgv1
                               Id_belnr like Bsid-Belnr
                               Id_buzei like Bsid-Buzei
                               Id_ldgrp like Bkpf-Ldgrp
                               Id_waers like Bsid-Waers
                               Id_shkzg like Bsid-Shkzg
                               Id_betra type Wrbtr_x8
                               Id_bedg1 like Xbsid-Bedg1
                               Id_bedg2 like Xbsid-Bedg2
                               Id_bedg3 like Xbsid-Bedg3
                               Id_bedg4 like Xbsid-Bedg4
                               Id_bedg5 like Xbsid-Bedg5
                               Ib_betraege type c
                               Id_krit1 like Xbsid-Krit1
                               Id_krit2 like Xbsid-Krit2
                               Id_krit3 like Xbsid-Krit3
                               Id_krit4 like Xbsid-Krit4
                               Id_krit5 like Xbsid-Krit5
                               Id_waer1 type waers              "1817592
                               Id_waer2 type waers              "1817592
                               Id_waer3 type waers              "1817592
                               Id_waer4 type waers              "1817592
                               Id_waer5 type waers.             "1817592

  Data: Ld_betragn like Bsega-Wrshb.

  If Id_shkzg = 'S'.
    Ld_betragn = Id_betra.
  Else.
    Ld_betragn = - Id_betra.
  Endif.
  Format color col_key intensified.
  Write:    /01(01) sy-vline,
             02(10) Id_belnr,
             12(01) sy-vline,
             13(03) Id_buzei,
             16(01) sy-vline.
  Format color col_normal intensified off.
  If not Id_augdt is initial.
    Write   17(10) Id_augdt color col_positive intensified on.
  Endif.
  Write:     27(01) sy-vline.
  If not Id_augbl is initial.
    Write    28(10) Id_augbl color col_positive intensified on.
  Endif.
  Write:     38(01) sy-vline,
             39(04) Id_ldgrp,
             43(01) sy-vline,
             44(05) Id_waers,
             49(01) sy-vline,
             50(31) Ld_betragn currency Id_waers,
             81(01) sy-vline.
  If Ib_betraege = ' '.
    Write:   82(20) Id_bedg1,
             102(01) sy-vline,
             103(18) Id_bedg2,
            121(01) sy-vline,
            122(16) Id_bedg3,
            138(01) sy-vline,
            139(14) Id_bedg4,
            153(01) sy-vline,
            154(12) Id_bedg5,
            166(01) sy-vline.
  Else.
    If Id_krit1 ne space and Id_bedg1 ne space.
      Perform Betrag_ausgeben_lg
              using Id_krit1 Id_bedg1 Id_waer1 '1'.
    Else.
      Write  82(20) Id_bedg1.
    Endif.
    Write    102(01) sy-vline.
    If Id_krit2 ne space and Id_bedg2 ne space.
      Perform Betrag_ausgeben_lg
              using Id_krit2 Id_bedg2 Id_waer2 '2'.
    Else.
      Write  103(18) Id_bedg2.
    Endif.
    Write   121(01) sy-vline.
    If Id_krit3 ne space and Id_bedg3 ne space.
      Perform Betrag_ausgeben_lg
              using Id_krit3 Id_bedg3 Id_waer3 '3'.
    Else.
      Write 122(16) Id_bedg3.
    Endif.
    Write   138(01) sy-vline.
    If Id_krit4 ne space and Id_bedg4 ne space.
      Perform Betrag_ausgeben_lg
              using Id_krit4 Id_bedg4 Id_waer4 '4'.
    Else.
      Write 139(14) Id_bedg4.
    Endif.
    Write   153(01) sy-vline.
    If Id_krit5 ne space and Id_bedg5 ne space.
      Perform Betrag_ausgeben_lg
              using Id_krit5 Id_bedg5 Id_waer5 '5'.
    Else.
      Write 154(12) Id_bedg5.
    Endif.
    Write   166(01) sy-vline.
   Endif.

ENDFORM.                    " RECORD_DOCUMENT_LG
*&---------------------------------------------------------------------*
*&      Form  BETRAG_AUSGEBEN_LG
*&---------------------------------------------------------------------*
*       completely new by not*
*       change by note 1817592:
*       Form Check_betragsfelder now called in Form DETERMINE_CUR
*       in order to populate gs_dlist-waer* =  Id_waers
*----------------------------------------------------------------------*
*
FORM BETRAG_AUSGEBEN_LG USING Id_krit Id_wert Id_waers Char_1.
*----- Betragsfelder müssen währungsgerecht dargestellt werden ---------
*----- nur Felder aus BSIS, BSIK, BSID mit Referenztabelle aus diesen
*----- Tabellen oder T001 werden währungsgerecht dargestellt -----------
*----- Bei anderen Feldern wäre das Nachlesen der Währung zu teuer -----

*   Perform Check_betragsfelder using Id_krit.                  "1817592
*   If X_betrag = 'X'.                                          "1817592
    IF NOT Id_waers IS INITIAL.                                 "1817592
      Betragfeld = Id_wert.
*      Assign (Curr) to <Fb>.                                   "1817592
*      Write <Fb> to Currw.                                     "1817592
      Case Char_1.
        When '1'.
         Write:  69(18) Betragfeld currency Id_waers.
        When '2'.
         Write:  103(18) Betragfeld currency Id_waers.
        When '3'.
         Write: 122(16) Betragfeld currency Id_waers.
        When '4'.
         Write: 139(14) Betragfeld currency Id_waers.
        When '5'.
         Write: 154(12) Betragfeld Currency Id_waers.
      Endcase.
   Else.
      Case Char_1.
        When '1'.
         Write:  69(18) Id_wert.
        When '2'.
         Write:  103(18) Id_wert.
        When '3'.
         Write: 122(16) Id_wert.
        When '4'.
         Write: 139(14) Id_wert.
        When '5'.
         Write: 154(12) Id_wert.
      Endcase.
   Endif.
ENDFORM.                    " BETRAG_AUSGEBEN_LG
*&---------------------------------------------------------------------*
*&      Form  CHECK_SUMME_LG
*&---------------------------------------------------------------------*
*  completely new by note 1416585 for clearing specific to ledgergroup
*  based on Check_summe
*----------------------------------------------------------------------*
FORM CHECK_SUMME_LG  USING Id_augdt like Bkpf-Budat
                           Id_augbl like Bdcmsgcoll-Msgv1.

  Format color col_total intensified off.
  Write:      /01(01) sy-vline,
               02(10) char_*,
               12(01) sy-vline,
               13(03) space,
               16(01) sy-vline.
  If not Id_augdt is initial.
    Write      17(10) Id_augdt.
  Endif.
  Write:       27(01) sy-vline,
               28(10) Id_augbl,
               38(01) sy-vline.
  Write:       39(04) Xbsisgr-ldgrp,
               43(01) sy-vline,
               44(05) Xbsisgr-Xcurr,
               49(01) sy-vline,
               50(31) Xbsisgr-Bet_tw currency Xbsisgr-Xcurr,
               81(01) sy-vline.
  If Betraeges = ' '.
    Write:     82(20) Xbsisgr-Bedg1,
              102(01) sy-vline,
              103(18) Xbsisgr-Bedg2,
              121(01) sy-vline,
              122(16) Xbsisgr-Bedg3,
              138(01) sy-vline,
              139(14) Xbsisgr-Bedg4,
              153(01) sy-vline,
              154(12) Xbsisgr-Bedg5,
              166(01) sy-vline.
  Else.
    If Xbsisgr-Krit1 ne space and Xbsisgr-Bedg1 ne space.
      Perform Betrag_ausgeben_lg
              using Xbsisgr-Krit1 Xbsisgr-Bedg1 Gs_dlist-Waer1 '1'.
    Else.
      Write    82(20) Xbsisgr-Bedg1.
    Endif.
    Write      102(01) sy-vline.
    If Xbsisgr-Krit2 ne space and Xbsisgr-Bedg2 ne space.
      Perform Betrag_ausgeben_lg
              using Xbsisgr-Krit2 Xbsisgr-Bedg2 Gs_dlist-Waer2 '2'.
    Else.
      Write    103(18) Xbsisgr-Bedg2.
    Endif.
    Write     121(01) sy-vline.
    If Xbsisgr-Krit3 ne space and Xbsisgr-Bedg3 ne space.
      Perform Betrag_ausgeben_lg
              using Xbsisgr-Krit3 Xbsisgr-Bedg3 Gs_dlist-Waer3 '3'.
    Else.
      Write   122(16) Xbsisgr-Bedg3.
    Endif.
    Write     138(01) sy-vline.
    If Xbsisgr-Krit4 ne space and Xbsisgr-Bedg4 ne space.
      Perform Betrag_ausgeben_lg
              using Xbsisgr-Krit4 Xbsisgr-Bedg4 Gs_dlist-Waer4 '4'.
    Else.
      Write   139(14) Xbsisgr-Bedg4.
    Endif.
    Write     153(01) sy-vline.
    If Xbsisgr-Krit5 ne space and Xbsisgr-Bedg5 ne space.
      Perform Betrag_ausgeben_lg
              using Xbsisgr-Krit5 Xbsisgr-Bedg5 Gs_dlist-Waer5 '5'.
    Else.
       Write 154(12) Xbsisgr-Bedg5.
    Endif.
       Write 166(01) sy-vline.
    Endif.
    Uline.

ENDFORM.                    " CHECK_SUMME_LG
