*&---------------------------------------------------------------------*
*&  Include           F124TOP
*&---------------------------------------------------------------------*

TABLES:
  bkpf,                          "Belegkopf
  bseg,                          "Belegposition
  bsid,                          "Posten Debitor
  bsik,                          "Posten Kreditor
  bsis,                          "Posten Sachkonten
  faglbsis,                      "Posten Sackkonten mit Ledgergroppenspez. OP-Verwaltung
  dd03l,                         "Test der Bedingungen
  ekpo,                          "Einkaufsbelegposition
  mkpf,                          "Belegkopf Materialbeleg
  regus,                         "Kontensperrtabelle
  tcurc,
  tf123,                         "Zusatzregel für SAPF124
  t001,                          "Buchungskreise (Kontoplan)
  t030,                          "Kontenfindung
  t042x,                         "Durch Zahlpr. gesperr. Bukrse
  t100,                             " Nachrichten
  bhdgd,                            "Batch-Heading
  sscrfields.                    "Function key
*        icon.                           "Icon
DATA: zaehler_xbsisgr TYPE i,
      x_exit(1)       TYPE c.

DATA: x_lg TYPE i VALUE 1.             "Load the creen always with classic clearing screen.
"1 => Classic; 2 => LG pecific;
DATA: gv_newgl_active TYPE xfeld.

DATA: tcode_id(20) TYPE c VALUE 'F13',  " Notice tcode for backgroud processing
      no_tcode(1)  TYPE c.               " Write Error message in protocol if no tcode found in RFDT

TYPE-POOLS: mrm, icon.

TYPES: BEGIN OF ys_xbsis.
    INCLUDE STRUCTURE bsis.
*------ DEFINITION DER ZUSATZBEDINUGNEN
TYPES: bedg1(24) TYPE c,                  "Inhalt der Bedingung TF123-
       bedg2(24) TYPE c,                 "Inhalt der Bedingung TF123-
       bedg3(24) TYPE c,                 "Inhalt der Bedingung TF123-
       bedg4(24) TYPE c,                 "Inhalt der Bedingung TF123-
       bedg5(24) TYPE c,                 "Inhalt der Bedingung TF123-
       krit1(35) TYPE c,                 "Kriterium 1 (Tab-Feldname)
       krit2(35) TYPE c,                 "Kriterium 2 (Tab-Feldname)
       krit3(35) TYPE c,                 "Kriterium 3 (Tab-Feldname)
       krit4(35) TYPE c,                 "Kriterium 4 (Tab-Feldname)
       krit5(35) TYPE c,                 "Kriterium 5 (Tab-Feldname)
       ldgrp     TYPE fagl_ldgrp,        "Ledger group         "1401545
       xcurr     LIKE bsis-pswsl,
       xamnt     TYPE wrbtr_x8,                             "972624
       END OF ys_xbsis.

TYPES: yt_xbsis TYPE STANDARD TABLE OF ys_xbsis.
TYPES: BEGIN OF ys_dlist,                                   "1029245
         bukrs     LIKE bseg-bukrs,                         "1029245
         koart     LIKE bseg-koart,                         "1029245
         accnr     LIKE bseg-kunnr,                         "1029245
         hkont     LIKE bseg-hkont,                         "1029245
         belnr     LIKE bseg-belnr,                         "1029245
         buzei     LIKE bseg-buzei,                         "1029245
         umskz     LIKE bseg-umskz,                         "1029245
         ldgrp     LIKE bkpf-ldgrp,                         "1416585
         xcurr     LIKE bsis-pswsl,                         "1029245
         shkzg     LIKE bseg-shkzg,                         "1029245
         xamnt     TYPE wrbtr_x8,                           "1029245
         bedg1(24) TYPE c,                                  "1029245
         bedg2(24) TYPE c,                                  "1029245
         bedg3(24) TYPE c,                                  "1029245
         bedg4(24) TYPE c,                                  "1029245
         bedg5(24) TYPE c,                                  "1029245
         krit1(35) TYPE c,                                  "1029245
         krit2(35) TYPE c,                                  "1029245
         krit3(35) TYPE c,                                  "1029245
         krit4(35) TYPE c,                                  "1029245
         krit5(35) TYPE c,                                  "1029245
         waer1     TYPE waers,                              "1817592
         waer2     TYPE waers,                              "1817592
         waer3     TYPE waers,                              "1817592
         waer4     TYPE waers,                              "1817592
         waer5     TYPE waers,                              "1817592
       END OF ys_dlist,                                     "1029245
       yt_dlist TYPE STANDARD TABLE OF ys_dlist.            "1029245

TYPES: BEGIN OF ys_ldprp,                                   "1416585
         ldgrp   TYPE fagl_ldgrp,                           "1416585
         leading TYPE boole_d,                              "1416585
       END OF ys_ldprp.                                     "1416585
DATA: gs_ldgrp TYPE ys_ldprp,                               "1416585
      gt_ldgrp TYPE TABLE OF ys_ldprp.                      "1416585

DATA  tabix4(4) TYPE n.
DATA  feldn17(17) TYPE n.
DATA  fctkey LIKE sscrfields-ucomm.
*----- ermittelte Ausgleichsdaten aus jüngstem Beleg mit Ergebnis, ob
*----- die Prüfungen mit fi_period_determine, .._check OK waren
DATA: BEGIN OF augdttab OCCURS 1,
        bukrs   LIKE t001-bukrs,
        augdt   LIKE bseg-augdt,
        xnok(1) TYPE c,
      END OF augdttab.
TYPES: BEGIN OF ys_rate,                                    "2528573
         koart TYPE koart,
         bukrs TYPE bukrs,
         blart TYPE blart,
         kurst TYPE kurst_003,
         kursf TYPE kursf,
*          xkursx TYPE x_fxr_ratex, "not supported for FB1*
       END OF ys_rate.                                      "2528573

*------ Ausgleichsdaten für Batch-Input -------------------------------
DATA  BEGIN OF bdcdtab OCCURS 1.
INCLUDE STRUCTURE bdcdata.
DATA  END OF bdcdtab.
*----- Historie zum Einkaufsbeleg (alle bisher gelesenen) --------------
DATA: BEGIN OF iekbe OCCURS 10.
    INCLUDE STRUCTURE ekbe.
DATA: END OF iekbe.
*----- Historie zum Einkaufsbeleg (nur für eine Bestellung) ------------
DATA: BEGIN OF ekbetab OCCURS 10.
    INCLUDE STRUCTURE ekbe.
DATA: END OF ekbetab.
*----- Positionssummen aus Bestellentwickung ---------------------------
DATA: BEGIN OF iekbes OCCURS 10.
    INCLUDE STRUCTURE ekbes.
DATA: END OF iekbes.
*----- WE/RE - Zuordnung aus Bestellentwicklung ------------------------
DATA: BEGIN OF iekbez OCCURS 10.
    INCLUDE STRUCTURE ekbez.
DATA: END OF iekbez.
*----- Einkaufsfelder Bestandsführung Bezugsnebenkosten ----------------
DATA: BEGIN OF iekbnk OCCURS 10.
    INCLUDE STRUCTURE ekbnk.
DATA: END OF iekbnk.
*----- Historie zum Einkaufsbeleg - Bezugsnebenkosten ------------------
DATA: BEGIN OF iekbz OCCURS 10.
    INCLUDE STRUCTURE ekbz.
DATA: END OF iekbz.
*----- WE/RE-Konten ----------------------------------------------------
DATA: BEGIN OF i030_were OCCURS 30.
    INCLUDE STRUCTURE t030.
DATA: END OF i030_were.
DATA: BEGIN OF ibsis OCCURS 100,                            "454904
        ebeln LIKE bseg-ebeln,                              "454904
        ebelp LIKE bseg-ebelp,                              "454904
        bsis  LIKE bsis,                                    "454904
        ldgrp TYPE fagl_ldgrp.                                   " Ledger group
DATA: END OF ibsis.                                         "454904

DATA: BEGIN OF iwere OCCURS 10,
        bukrs LIKE t001-bukrs,
        hkont LIKE t030h-hkont,
      END OF iwere.
*------ Tabelle aller aufgetretenen Fehler beim Lauf des SAPF124 ------
DATA: BEGIN OF fehlprot OCCURS 1,
        bukrs     LIKE t001-bukrs,     "Buchungskreis
        koart     LIKE bseg-koart,     "Kontoart
        konto(10) TYPE c,              "Kontonr Debi, Kredi, Sachk
        hkont     LIKE bseg-hkont,     "Hauptbuchkonto
        waers     LIKE bsid-waers,     "Währung
        text(90)  TYPE c,              " Fehlertext
        dyname    LIKE bdcmsgcoll-dyname, "Batch-Input Modulname
        dynumb    LIKE bdcmsgcoll-dynumb, "Batch-Input Dynpronr
        msgtyp    LIKE bdcmsgcoll-msgtyp, "Nachr.typ E, S, usw.
        msgid     LIKE bdcmsgcoll-msgid,  "Nachr.ID
        msgnr     LIKE bdcmsgcoll-msgnr,  "Nachr.nr
      END OF fehlprot.
*------ Tabelle der Anwenderbedingungen mit den zugehörigen Tabellen ---
DATA: BEGIN OF if123 OCCURS 100,
        ktopl     LIKE tf123-ktopl,
        koart     LIKE tf123-koart,    "Kontoart
        kont1     LIKE tf123-kont1,    "Untere Kontonummer
        kont2     LIKE tf123-kont1,    "Obere  Kontonummer
        bedg1     LIKE tf123-bedg1,    "1. Bedingung (z.B. BKTXT)
        bedg2     LIKE tf123-bedg2,    "2. Bedingung (z.B ZUONR)
        bedg3     LIKE tf123-bedg3,    "3. Bedingung
        bedg4     LIKE tf123-bedg4,    "4. Bedingung
        bedg5     LIKE tf123-bedg5,    "5. Bedingung
        tabl1(16) TYPE c,              "Tabelle für 1.Bed. z.B. BKPF 4.7
        tabl2(16) TYPE c,              "Tabelle für 2.Bed. z.B. BSID 4.7
        tabl3(16) TYPE c,              "Tabelle für 3.Bedingung      4.7
        tabl4(16) TYPE c,              "Tabelle für 4.Bedingung      4.7
        tabl5(16) TYPE c,              "Tabelle für 5.Bedingung      4.7
      END OF if123.
*------ IPROT (Protokolltabelle) --------------------------------------
DATA: BEGIN OF iprot OCCURS 500,
        bukrs     LIKE bsis-bukrs,     "Buchungskreis
        koart     LIKE bseg-koart,     "Kontoart
        konto     LIKE bsid-kunnr,     "Kontonr (Debi, Kredi, Sako)
        hkont     LIKE bsis-hkont,     "Kontonummer Abstimmkonto
        belnr     LIKE bsis-belnr,     "Belegnummer
        buzei     LIKE bsis-buzei,     "Belegzeile
        text0(80) TYPE c,              "Fehlertext
        msgid(1)  TYPE c,              "Nachrichtentype S, X
      END OF iprot.
*------ Tabelle der durch REGUS-Eintrag gesperrten Konten ------------
DATA: BEGIN OF iregus OCCURS 100.
    INCLUDE STRUCTURE regus.
DATA: END OF iregus.
*------ ITEXT  (Texte der Datenelemente) ------------------------------
DATA: BEGIN OF itext OCCURS 50,
        kritx(47) TYPE c,                                   "1769121
        textx(15) TYPE c,
      END OF itext.
*------ ITEXT  (Texte der Datenelemente, Ergänzung ) -------------------
DATA: BEGIN OF itexterg OCCURS 50,
        table     LIKE if123-tabl1,
        bedng     LIKE if123-bedg1,
        kritx(35) TYPE c,
        textx(15) TYPE c,
      END OF itexterg.
*------ Selektierte Buchungskreise ------------------------------------
DATA: BEGIN OF i001 OCCURS 10.
    INCLUDE STRUCTURE t001.
DATA: END OF i001.
*------ Nicht benutzbare Buchungskreise ------------------------"2238211
DATA: BEGIN OF gt_001no OCCURS 10,                          "2238211
        bukrs TYPE bukrs,                                   "2238211
      END OF gt_001no.                                      "2238211
*------ SKV-Konten  (dort darf nicht ausgeglichen werden) -------------
DATA: BEGIN OF i030_skv OCCURS 30,
        bukrs LIKE t001-bukrs,
        konts LIKE t030-konts,     " Kontonummer in Fixkontentab
      END OF i030_skv.
*------ Aufgetretene Fehler bei Call Transaction ----------------------
DATA  BEGIN OF messtab OCCURS 10.
INCLUDE STRUCTURE bdcmsgcoll.
DATA  END   OF messtab.
*------ Belegkopf Eingangsrechnung MM (für Sonderbearbeitung WE/RE) ----
DATA: rbkptab LIKE rbkp.
*------ Geschäftsjahresvarianten ---------------------------------------
DATA: BEGIN OF gjvtab OCCURS 1,
        periv LIKE t009-periv,
      END   OF gjvtab.
*------ TABL (Tabelle der Tabellen zum Nachlesen) ---------------------
DATA: BEGIN OF tabl,
        ntabl    TYPE p VALUE 3,      "Zahl der Tabellen in TABL
        tab1(16) TYPE c VALUE '     ', "Frei für BSID, BSIK, BSIS    4.7
        tab2(16) TYPE c VALUE 'BKPF ', "Belegkopf                    4.7
        tab3(16) TYPE c VALUE 'BSEG ', "Belegpositionen              4.7
      END OF tabl.
*------ Debitorenkonten ------------------------------------------------
DATA: BEGIN OF tdebi OCCURS 1,
        bukrs    LIKE t001-bukrs,
        kunnr    LIKE bsid-kunnr,     " Debitor
        shkzg    LIKE bsid-shkzg,     " Soll/Haben Kennzeichen
        wt_newwt LIKE t001-wt_newwt,     "quellensteuerpflichtig 500429
      END OF tdebi.
*------ Kreditorenkonten -----------------------------------------------
DATA: BEGIN OF tkredi OCCURS 1,
        bukrs LIKE t001-bukrs,
        lifnr LIKE bsik-lifnr,     " Kreditor
        shkzg LIKE bsik-shkzg,
*        segmt LIKE bseg-segment, "edit thanhnt 10.02.2023

        " Soll/Haben Kennzeichen
      END OF tkredi.
*------ Sachkonten -----------------------------------------------------
DATA: BEGIN OF tsako OCCURS 1,
        bukrs  LIKE t001-bukrs,
        hkont  LIKE bsis-hkont,     " Sachkontonummer
        shkzg  LIKE bsis-shkzg,
        xlgclr LIKE skb1-xlgclr,    " Ledger group specific clearing
      END OF tsako.
DATA:  BEGIN OF xaccdn  OCCURS 100.
    INCLUDE STRUCTURE accdn.
DATA:  END   OF xaccdn.
DATA:  BEGIN OF ixaccdn  OCCURS 100.
    INCLUDE STRUCTURE accdn.
DATA:  END   OF ixaccdn.
*------ offene Debitorenposten mit Zusatzbedingungen -------------------
DATA: BEGIN OF xbsid OCCURS 1.
    INCLUDE STRUCTURE bsid.
*------ DEFINITION DER ZUSATZBEDINUGNEN --------------------------------
DATA: bedg1(24) TYPE c,                "Inhalt der 1.Bedingung TF123-
      bedg2(24) TYPE c,              "Inhalt der 2.Bedingung TF123-
      bedg3(24) TYPE c,              "Inhalt der 3.Bedingung TF123-
      bedg4(24) TYPE c,              "Inhalt der 4.Bedingung TF123-
      bedg5(24) TYPE c,              "Inhalt der 5.Bedingung TF123-
      krit1(35) TYPE c,              "Kriterium 1, z.B. BKPF-BKTXT
      krit2(35) TYPE c,              "Kriterium 2, z.B BSID-ZUONR
      krit3(35) TYPE c,              "Kriterium 3
      krit4(35) TYPE c,              "Kriterium 4
      krit5(35) TYPE c,              "Kriterium 5
      xcurr     LIKE bsid-waers,
      xamnt     TYPE wrbtr_x8,                              "972624
      END OF xbsid.
*------ Gruppierungstabelle Debitorenposten mit Zusatzbed. ------------
DATA: BEGIN OF xbsidgr OCCURS 1,
        hkont     LIKE bsid-hkont,     "Abstimm-/SHB-Konto bei D/K
        krit1(35) TYPE c,              "Kriterium 1 (Tab-Feldname) "1321544
        krit2(35) TYPE c,              "Kriterium 2 (Tab-Feldname) "1321544
        krit3(35) TYPE c,              "Kriterium 3 (Tab-Feldname) "1321544
        krit4(35) TYPE c,              "Kriterium 4 (Tab-Feldname) "1321544
        krit5(35) TYPE c,              "Kriterium 5 (Tab-Feldname) "1321544
        umskz     LIKE bsid-umskz,     "Sonderhauptbuchkennzeichen
        xcurr     LIKE bsid-waers,     "Währungsschlüssel
        bet_bw    TYPE aflex17d2o18n,  "Summe in Belegwährung    "AFLE Enablement: p 8 with output length 18
*------ DEFINITION DER ZUSATZBEDINUGNEN -------------------------------
        bedg1(24) TYPE c,              "Inhalt der Bedingung TF123-
        bedg2(24) TYPE c,              "Inhalt der Bedingung TF123-
        bedg3(24) TYPE c,              "Inhalt der Bedingung TF123-
        bedg4(24) TYPE c,              "Inhalt der Bedingung TF123-
        bedg5(24) TYPE c,              "Inhalt der Bedingung TF123- "1321544
      END OF xbsidgr.
*------ offene Kreditorenposten mit Zusatzbedingungen -----------------
DATA: BEGIN OF xbsik OCCURS 2.
    INCLUDE STRUCTURE bsik.
*------ DEFINITION DER ZUSATZBEDINUGNEN
DATA: bedg1(24) TYPE c,                "Inhalt der Bedingung TF123-
      bedg2(24) TYPE c,              "Inhalt der Bedingung TF123-
      bedg3(24) TYPE c,              "Inhalt der Bedingung TF123-
      bedg4(24) TYPE c,              "Inhalt der Bedingung TF123-
      bedg5(24) TYPE c,              "Inhalt der Bedingung TF123-
      krit1(35) TYPE c,              "Kriterium 1 (Tab-Feldname)
      krit2(35) TYPE c,              "Kriterium 2 (Tab-Feldname)
      krit3(35) TYPE c,              "Kriterium 3 (Tab-Feldname)
      krit4(35) TYPE c,              "Kriterium 4 (Tab-Feldname)
      krit5(35) TYPE c,              "Kriterium 5 (Tab-Feldname)
      xcurr     LIKE bsik-waers,
      xamnt     TYPE wrbtr_x8,

      "thanhnt                         "972624
      segment   TYPE string,
      END OF xbsik.
*------ Gruppierungstabelle Kreditorenposten mit Zusatzbed. -----------
DATA: BEGIN OF xbsikgr OCCURS 1,
        hkont     LIKE bsik-hkont,     "Abstimm-/SHB-Konto bei D/K
        krit1(35) TYPE c,              "Kriterium 1 (Tab-Feldname) "1321544
        krit2(35) TYPE c,              "Kriterium 2 (Tab-Feldname) "1321544
        krit3(35) TYPE c,              "Kriterium 3 (Tab-Feldname) "1321544
        krit4(35) TYPE c,              "Kriterium 4 (Tab-Feldname) "1321544
        krit5(35) TYPE c,              "Kriterium 5 (Tab-Feldname) "1321544
        umskz     LIKE bsik-umskz,     "Sonderhauptbuchkennzeichen
        xcurr     LIKE bsik-waers,     "Währungsschlüssel
        bet_bw    TYPE aflex17d2o18n,  "Summe in Belegwährung    "AFLE Enablement: p 8 with output length 18
*------ DEFINITION DER ZUSATZBEDINUGNEN -------------------------------
        bedg1(24) TYPE c,              "Inhalt der Bedingung TF123-
        bedg2(24) TYPE c,              "Inhalt der Bedingung TF123-
        bedg3(24) TYPE c,              "Inhalt der Bedingung TF123-
        bedg4(24) TYPE c,              "Inhalt der Bedingung TF123-
        bedg5(24) TYPE c,              "Inhalt der Bedingung TF123- "1321544,
        segment   TYPE string, "add thanhnt,
      END   OF xbsikgr.
*------- offene Sachkontenposten mit Zusatzbedingungen -----------------
DATA: xbsis        TYPE yt_xbsis WITH HEADER LINE,
      xbsiscopy    TYPE yt_xbsis WITH HEADER LINE,
      xbsiscopys   TYPE yt_xbsis WITH HEADER LINE,
      xbsiscopyh   TYPE yt_xbsis WITH HEADER LINE,
      xbsiscopyeq  TYPE yt_xbsis WITH HEADER LINE,
      xbsiscopytol TYPE yt_xbsis WITH HEADER LINE.
*------ Gruppierungstabelle Sachkontenposten mit Zusatzbed. -----------
DATA: BEGIN OF xbsisgr OCCURS 1,
        hkont     LIKE bsis-hkont,     "Abstimm-/SHB-Konto bei D/K
        krit1(35) TYPE c,              "Kriterium 1 (Tab-Feldname) "1321544
        krit2(35) TYPE c,              "Kriterium 2 (Tab-Feldname) "1321544
        krit3(35) TYPE c,              "Kriterium 3 (Tab-Feldname) "1321544
        krit4(35) TYPE c,              "Kriterium 4 (Tab-Feldname) "1321544
        krit5(35) TYPE c,              "Kriterium 5 (Tab-Feldname) "1321544
        ldgrp     TYPE fagl_ldgrp,     "Ledger group               "1401545
        xcurr     LIKE bsis-waers,     "Schlüssel für Transaktionwäh.
        bet_tw    TYPE aflex17d2o18n,  "Summe für die Fortschreibung    "AFLE Enablement: p 8 with output length 18
*------ DEFINITION DER ZUSATZBEDINUGNEN -------------------------------
        bedg1(24) TYPE c,              "Inhalt der Bedingung TF123-
        bedg2(24) TYPE c,              "Inhalt der Bedingung TF123-
        bedg3(24) TYPE c,              "Inhalt der Bedingung TF123-
        bedg4(24) TYPE c,              "Inhalt der Bedingung TF123-
        bedg5(24) TYPE c,              "Inhalt der Bedingung TF123- "1321544
      END   OF xbsisgr.

*------ XF123 (Kopie der Kriterientabelle TF123 -----------------------
DATA: BEGIN OF xf123  OCCURS 100.
    INCLUDE STRUCTURE tf123.
DATA: END OF xf123.
DATA: gt_tcure TYPE TABLE OF tcure.                   "note2251669
*------ Tabelle der ausgleichbaren Debitorenposten --------------------
*------ Übergabe an Ausgleichstransaktion FB1D -------------------------
DATA  BEGIN OF ybsid OCCURS 1.
INCLUDE STRUCTURE bsid.
DATA  END OF ybsid.
*------ Puffertabelle der auszugleichenden Belege (aus YBSID) ----------
DATA  BEGIN OF pbsid OCCURS 1.
INCLUDE STRUCTURE bsid.
DATA  END OF pbsid.
*------ Tabelle der ausgleichbaren Kreditorenposten -------------------
*------ Übergabe an Ausgleichstransaktion FB1K -------------------------
DATA  BEGIN OF ybsik OCCURS 1.
INCLUDE STRUCTURE bsik.
DATA  END OF ybsik.
*------ Puffertabelle der auszugleichenden Belege (aus YBSIK) ----------
DATA  BEGIN OF pbsik OCCURS 1.
INCLUDE STRUCTURE bsik.
DATA  END OF pbsik.
*------ Tabelle der ausgleichbaren Sachkontenposten -------------------
*------ Übergabe an Ausgleichstransaktion FB1S -------------------------
DATA  BEGIN OF ybsis OCCURS 1.
INCLUDE STRUCTURE bsis.
DATA  END OF ybsis.
*------ Puffertabelle der auszugleichenden Belege (aus YBSIS) ----------
DATA  BEGIN OF pbsis OCCURS 1.
INCLUDE STRUCTURE bsis.
DATA  END OF pbsis.
DATA  BEGIN OF zbsis OCCURS 1.
INCLUDE STRUCTURE bsis.
DATA  END OF zbsis.
DATA  BEGIN OF zbseg OCCURS 1.
INCLUDE STRUCTURE bseg.
DATA  END OF zbseg.
DATA: gs_dlist  TYPE ys_dlist,                              "1029245
      gt_dlist  TYPE yt_dlist,                              "1029245
      gt_dlist2 TYPE yt_dlist.                              "1029245
*-----------------------------------------------------------------------
*       HILFSFELDER
*-----------------------------------------------------------------------
DATA: actvt(2)        TYPE c,              "Aktivität
      augblnr         LIKE bdcmsgcoll-msgv1,
      awaers          LIKE bkpf-waers,     "Ausgleichswährung
      authorit(2)     TYPE c,              "Berechtigung
      awref           LIKE acchd-awref,    "Referenzbelegnummer
      aworg           LIKE acchd-aworg,    "Referenzorganisationseinh.
      awtyp           LIKE acchd-awtyp,
      awsys           LIKE acchd-awsys,
      bedgx           LIKE if123-bedg1,
      betraeged       LIKE boole-boole,    "Sind Betragsfelder in TF123?
      betraegek       LIKE boole-boole,    "Sind Betragsfelder in TF123?
      betraeges       LIKE boole-boole,    "Sind Betragsfelder in TF123?
      betragfeld      LIKE bsid-pswbt,     "Ausgabefeld von Beträgen
      bsik_subrc      LIKE sy-subrc,
      bsid_subrc      LIKE sy-subrc,
      bsis_subrc      LIKE sy-subrc,
      bukreis         LIKE t001-bukrs,
      cha500(500)     TYPE c,              "Textfeld
      cnt_ap          TYPE i,              "Zähler der ausgleichb. Posten
      cnt_eap         TYPE i,              "tatsächl. ausgegl. Posten
      cnt_op          TYPE i,              "Zähler der offenen Posten
      ctixaccdn       TYPE i,              "Zähler Einträge in ixaccdn
      ct_pbsid        TYPE i,                "Zähler für Einträge in PBSID
      ct_pbsik        TYPE i,                "Zähler für Einträge in PBSIK
      ct_pbsis        TYPE i,                "Zähler für Einträge in PBSIS
      counter         TYPE i,              " Zähler
      curr(35)        TYPE c,              " Ausgabe Beträge: Tabellfeld
      currw           LIKE bsis-waers,     " Ausgabe Beträge: Feldinhalt
      datum1          LIKE sy-datum,       "Datum: Start d. Programmlauf
      docok           LIKE boole-boole,
      enqsubrc        LIKE sy-subrc,       "sy-subrc nach dem Sperren
      flag_if123      TYPE c,              "Flag: Kriterium in IF123 gef.
      flg_bkpf(1)     TYPE c,              "BKPF muß gelesen werden
      flg_bseg(1)     TYPE c,              "BSEG muß gelesen werden
      flg_bseg_add    TYPE boole_d,   "BSEG field in BSEG_ADD?  1416585
      flg_bkpf_ldgrp  TYPE boole_d,   "BKPF-LDGRP is in TF123   1416585
      flg_bkpf_other  TYPE boole_d, "Other BKPF fields in TF123  1416585
      flg_liste       TYPE c,              "Flag: Überschrift d. Listanz.
      gb_eaps_active  TYPE c,        "Payment directives possible  861169
      gd_ebpp_active  TYPE c,
      gd_ebpp_mess    TYPE c,
      gb_xcopy        TYPE c,              "Flag XBSISCOPY           "443350
      gejahr          LIKE bkpf-gjahr,
      hilfbet_tw      LIKE xbsisgr-bet_tw,
      icount          TYPE p,              "Zähler
      gd_del_fr_tabix TYPE i,                               "1410838
      gd_del_to_tabix TYPE i,                               "1410838
      gd_do_del       TYPE xfeld,                           "1410838
      koart           LIKE bseg-koart,     "Hilfsfeld für Kontoart
      ktopl           LIKE t001-ktopl,     "Kontenplan
      krit1           LIKE xbsid-krit1,    "Kriterium 1 (Tab-Feldname)
      krit2           LIKE xbsid-krit2,    "Kriterium 2 (Tab-Feldname)
      krit3           LIKE xbsid-krit3,    "Kriterium 3 (Tab-Feldname)
      krit4           LIKE xbsid-krit4,    "Kriterium 4 (Tab-Feldname)
      krit5           LIKE xbsid-krit5,    "Kriterium 5 (Tab-Feldname)
      last_bukrs      LIKE t001-bukrs,
      last_hkont      LIKE bsis-hkont,
      last_kunnr      LIKE bsid-kunnr,
      last_lifnr      LIKE bsik-lifnr,
      last_pbukrs     LIKE t001-bukrs,
      last_phkont     LIKE bsis-hkont,
      last_tabix      LIKE sy-tabix,
      last_tabl       LIKE dfies-tabname,
      last_fnam       LIKE dfies-fieldname,
      mod(1)          TYPE c,              " für call transaction
      monat           LIKE bkpf-monat,
      new_bukr(1)     TYPE c,              " Ausgabe Überschrift Kurzlist
      old_wrbtr       LIKE bsis-wrbtr,
      percent         TYPE p DECIMALS 1,   "Hilfsfeld für Prozentberechn
      rtc             LIKE sy-subrc,
      rcbkpf          LIKE sy-subrc,
      rcappend        LIKE sy-subrc,
      rcekpo          LIKE sy-subrc,
      rcekbe          LIKE sy-subrc,
      rciekbe         LIKE sy-subrc,
      rcmkpf          LIKE sy-subrc,
      rcrbkp          LIKE sy-subrc,
      savebetrag_tw   LIKE xbsisgr-bet_tw,
      save_augdt      LIKE bkpf-budat,                      "985181
      save_hkont      LIKE bsis-hkont,
      save_kunnr      LIKE tdebi-kunnr,
      save_lifnr      LIKE tkredi-lifnr,
      save2_konto     LIKE bseg-hkont,     "Hilfsfeld für Kontonummer
      savetabix       LIKE sy-tabix,
      shkzg           LIKE bsis-shkzg,     "Soll/Haben Kennzeichen
      spercent        TYPE p DECIMALS 1,   "% OPS/EAPS
      suinf(1)        TYPE c,              "Ausgabe der Summenzeile
      sum1_ap         TYPE i,              "Summe der ausgleichb.Posten
      sum1_eap        TYPE i,              "Summe tatsächl. ausgegl. Post
      sum1_op         TYPE i,              "Summe  der offenen Posten
      sumd_ap         TYPE i,              "Ges. summe APS (Debitoren)
      sumd_eap        TYPE i,              "Ges.summe EAPS (Debitoren)
      sumd_op         TYPE i,              " Ges. summe OPS (Debitoren)
      sumg_ap         TYPE i,              " Ges. summe APs
      sumg_eap        TYPE i,              "Ges.summe tatsächl.ausgegli.
      sumg_op         TYPE i,              "Ges. summe OPS
      sumk_ap         TYPE i,              "Ges.summe APS (Kreditoren)
      sumk_eap        TYPE i,              "Ges.su tats. ausgegl. Kredipo
      sumk_op         TYPE i,              "Ges.summe OPS Kredi
      sums_ap         TYPE i,              "Ges.summe APS Sachk
      sums_eap        TYPE i,              "Ges.su tats. ausgegl. Sachkpo
      sums_op         TYPE i,              "Ges.summe OPS Sachk
      summe(9)        TYPE p,
      gv_tcode        LIKE sy-tcode,       " Transaktionscode
      gv_title(120)   TYPE c,              " Titel of the program
      tabix           LIKE sy-tabix,
      tablx           LIKE if123-tabl1,
      upd(1)          TYPE c,              " Verbuchung synch od. asynchr
      uzeit1          LIKE sy-uzeit,       "Zeit: Start des Programmlauf
      varueb(132)     TYPE c,              "Variable Überschrift
      xwrbtr(1)       TYPE c,
      xsobeerf        TYPE c,              "Sonderbearbeitung erfolgreich
      x_betrag        TYPE c,          "Betragsfeld als Grupp-krit.? "418389
      x_ausglv        TYPE c,              "wurde Ausgl.vorgang versucht?
      x_echtl         TYPE c,              " Echtlauf
*      X_FEHLER    type C,              " Ausgabe Fehlerprotokoll
      x_fehl_sub      TYPE c,              " 2.Überschrift im Fehlerprot
      x_zprt_sub      TYPE c,              "Unterüberschrift Zusatzprot
      xf124e          TYPE c.       " Arbeite ich als SAPF124E ?
DATA: gb_xaubl LIKE t000f-xaubl,   "Authority-check for doc.type  516329
      gb_xaugs LIKE t000f-xaugs.   "Authority-check for bus.area  516329
*-----------------------------------------------------------------------
*       FIELD-SYMBOLE
*-----------------------------------------------------------------------
FIELD-SYMBOLS: <f>,                 "#EC * "für Überschrift Detailliste
               <f1>,                   "Inhalt von IF123-BEDG1
               <f2>,                   "Inhalt von IF123-BEDG2
               <f3>,                   "Inhalt von IF123-BEDG3
               <f4>,                   "Inhalt von IF123-BEDG4
               <f5>,                   "Inhalt von IF123-BEDG5
               <fb>.                   "Inhalt von Betragsfeldern
DATA  BEGIN OF dfiestab OCCURS 1.
INCLUDE STRUCTURE dfies.
DATA  END   OF dfiestab.
DATA  BEGIN OF tdfies OCCURS 1.
INCLUDE STRUCTURE dfies.
DATA  END   OF tdfies.

DATA: rbkpv TYPE mrm_rbkpv.
DATA  BEGIN OF gt_selavis OCCURS 1.
INCLUDE STRUCTURE cosel.
DATA  END   OF gt_selavis.

*----- Auslaufende Währungen -------------------------------
DATA: BEGIN OF gt_auslwaer OCCURS 1,
        msgid LIKE sy-msgid,
        msgty LIKE sy-msgty,
        msgno LIKE sy-msgno,
        waers LIKE bkpf-waers,
      END   OF gt_auslwaer.
DATA:  BEGIN OF gt_tcur_bukrs OCCURS 1.
    INCLUDE STRUCTURE tcur_bukrs.
DATA:  END   OF gt_tcur_bukrs.
DATA: gd_read_tcur_bukrs   TYPE xfeld,
      gd_no_get_subsequent TYPE xfeld.

DATA: fcode   TYPE sy-ucomm,
      gd_icon TYPE smp_dyntxt.

* for Schedule Manager                                          "1081370
DATA: gs_key LIKE schedman_key.                             "1081370

SELECTION-SCREEN FUNCTION KEY 1.

*-----------------------------------------------------------------------
*        Selektionsparameter
*-----------------------------------------------------------------------

*---------- Block 1 ----------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE TEXT-p01.

SELECT-OPTIONS: bukrx   FOR bseg-bukrs,
                gjahx   FOR bseg-gjahr.
SELECT-OPTIONS: so_zuonr FOR bsis-zuonr.
SELECT-OPTIONS: docnr   FOR bseg-belnr.
SELECT-OPTIONS: postdate FOR bkpf-budat.
SELECTION-SCREEN SKIP 1.
PARAMETERS      x_kunnr LIKE rfpdo-f123kunn MODIF ID cl.
PARAMETERS      x_shbkn LIKE rfpdo-f123shbk MODIF ID cl.
SELECT-OPTIONS: shbkd FOR bseg-umskz MODIF ID cl.           "355602
SELECT-OPTIONS: kontd   FOR bseg-kunnr MATCHCODE OBJECT debi MODIF ID cl.
PARAMETER:      x_avisd LIKE rfpdo-f124avis MODIF ID cl.
SELECTION-SCREEN SKIP 1.
PARAMETERS:     x_lifnr LIKE rfpdo-f123lifn MODIF ID cl.
PARAMETERS:     x_shblf LIKE rfpdo-f123shbl MODIF ID cl.
SELECT-OPTIONS: x_hkont FOR bsik-hkont NO INTERVALS NO-EXTENSION.
SELECT-OPTIONS: shbkk   FOR bseg-umskz MODIF ID cl.         "355602
SELECT-OPTIONS: kontk   FOR bseg-lifnr MATCHCODE OBJECT kred MODIF ID cl.
SELECTION-SCREEN SKIP 1.
PARAMETERS:     x_saknr LIKE rfpdo-f123sakn MODIF ID cl.
*select-options: KONTS_LG   for BSEG-SAKNR matchcode object SAKO.
SELECT-OPTIONS: konts   FOR bseg-saknr MATCHCODE OBJECT sako.
PARAMETERS:     xsobwere LIKE rfpdo-f124sobe MODIF ID cl.
PARAMETERS:     x_lgclr  LIKE rfpdo-f124xlgclr MODIF ID lg. "N1376249
SELECT-OPTIONS:  p_lg    FOR  bkpf-ldgrp MODIF ID lg.

PARAMETERS:     pzbsisgr LIKE rfpdo-f124count.
*PARAMETERS:     TESTWERE LIKE RFPDO-F124SOBE.

*-----------------End of block 1----------------------------------------
SELECTION-SCREEN END OF BLOCK 1.

*---------- Block 2 ----------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE TEXT-p02.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) TEXT-045 FOR FIELD augdt.
PARAMETERS:     augdt    LIKE bkpf-budat DEFAULT sy-datlo.
SELECTION-SCREEN COMMENT 48(09) TEXT-044 FOR FIELD bmonat.
PARAMETERS:     bmonat   LIKE bkpf-monat.
SELECTION-SCREEN END OF LINE.
PARAMETERS      xaugdt  LIKE rfpdo-f124date.
PARAMETERS:     zwaers   LIKE bsis-waers MODIF ID cl.
PARAMETERS:     xsobebvk LIKE rfpdo-f124bvk MODIF ID cl.
PARAMETERS:     xauslw   LIKE rfpdo-f124alw MODIF ID cl.
PARAMETERS:     xtol     LIKE rfpdo-f124tol.                "1414931
PARAMETERS      xtoleb  LIKE rfpdo-f124toleb.               "1414931
PARAMETERS      xnkon   LIKE rfpdo-f124nkon.
PARAMETERS:     x_testl LIKE rfpdo-f123test DEFAULT 'X'.
PARAMETERS:     min_bel LIKE rfpdo-f124minbel.
*---------- Block 3 ----------------------------------------------------
SELECTION-SCREEN END   OF BLOCK 2.

SELECTION-SCREEN BEGIN OF BLOCK 3 WITH FRAME TITLE TEXT-p03.
PARAMETERS:
  xausbel  LIKE rfpdo-f124demf DEFAULT 'X',
  xnausbel LIKE rfpdo-f124deof DEFAULT 'X'.
PARAMETERS:    x_fehler LIKE rfpdo-f124aube DEFAULT 'X'.
PARAMETERS:    x_applog TYPE flag NO-DISPLAY.  "SFin FIORI Job
SELECTION-SCREEN END   OF BLOCK 3.
INCLUDE rkasmawf. "for Schedule Manager                         "1081370
