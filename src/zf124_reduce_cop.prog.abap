*----------------------------------------------------------------------*
***INCLUDE F124_REDUCE_TSAKOF01.
*----------------------------------------------------------------------*
*
FORM reduce_tsako using  pd_tfill_ori TYPE sytfill.

  DATA: BEGIN OF skb1tab,
          bukrs  LIKE skb1-bukrs,
          saknr  LIKE skb1-saknr,
          xlgclr LIKE skb1-xlgclr, " Ledger group specific clearing
        END OF skb1tab.
* reduced set of accounts (which really have open items to be cleared)
  TYPES: BEGIN OF yt_tsako_red,
           bukrs  LIKE t001-bukrs,
           hkont  LIKE bsis-hkont,     " Sachkontonummer
           shkzg  LIKE bsis-shkzg,
           xlgclr LIKE skb1-xlgclr,    " Ledger group specific clearing
         END OF yt_tsako_red.

  DATA:  lt_tsakotab_red TYPE HASHED TABLE OF yt_tsako_red WITH UNIQUE KEY bukrs hkont shkzg xlgclr,
         ls_tsakotab_red TYPE yt_tsako_red,
         ld_tfill_red    TYPE sytfill.

*currently only supported if ledger specific clearing is not chosen
  CHECK x_lgclr  = space.

* select skb1 with join bseg for normal open items (not ledger specific)
* compare bsis_ddl (in case of future support of ledger specific: compare faglbsis_ddl)
  SELECT skb1~bukrs skb1~saknr skb1~xlgclr FROM skb1
                                           INNER JOIN bseg
                                           ON   bseg~bukrs = skb1~bukrs AND
                                                bseg~hkont = skb1~saknr
                                           INTO skb1tab
                         WHERE skb1~bukrs   IN bukrx
                         AND   skb1~saknr   IN konts
                         AND   skb1~xopvw   EQ 'X'    "#EC CI_SGLSELECT
                         AND   skb1~xlgclr  EQ ''
                         AND   bseg~zuonr   IN so_zuonr
                         AND   bseg~gjahr   IN gjahx
                         AND   bseg~belnr   IN docnr
                         AND   bseg~h_budat IN postdate
                         AND   bseg~awtyp   <> 'GLYEC'
                         AND (
                               ( bseg~koart = 'D' AND bseg~xhres = 'X' ) OR
                               ( bseg~koart = 'K' AND bseg~xhres = 'X' ) OR
                               ( bseg~koart = 'A' AND bseg~xhres = 'X' ) OR
                               ( bseg~koart = 'M' AND bseg~xkres = 'X' ) OR
                               ( bseg~koart = 'S' AND bseg~xkres = 'X'   AND bseg~xlgclr = '' ) )
                         AND bseg~h_bstat <> 'D'
                         AND bseg~h_bstat <> 'M'
                         AND bseg~augbl = space
                         AND bseg~xopvw EQ char_x. "#EC CI_BUFFJOIN
    ls_tsakotab_red-bukrs = skb1tab-bukrs.
    ls_tsakotab_red-hkont = skb1tab-saknr.
    ls_tsakotab_red-shkzg = 'H'.
    ls_tsakotab_red-xlgclr = skb1tab-xlgclr.
    INSERT ls_tsakotab_red INTO TABLE  lt_tsakotab_red.
    ls_tsakotab_red-bukrs = skb1tab-bukrs.
    ls_tsakotab_red-hkont = skb1tab-saknr.
    ls_tsakotab_red-shkzg = 'S'.
    ls_tsakotab_red-xlgclr = skb1tab-xlgclr.
    INSERT ls_tsakotab_red INTO TABLE lt_tsakotab_red.
  ENDSELECT.

  DESCRIBE TABLE lt_tsakotab_red LINES ld_tfill_red.
  IF ld_tfill_red EQ 0.
*----- nothing to be cleared --> avoid later "empty" selects on BSIS
    REFRESH tsako.
*----- provide message -------------------------------
    PERFORM record_fill
        USING 'BSIS ' space space space space space space char_s.
    EXIT.
  ELSE.
    IF pd_tfill_ori = ld_tfill_red.
*----- continue with tsako, no reduction necessary
      EXIT.
    ELSE.
      LOOP AT tsako.
        READ TABLE lt_tsakotab_red WITH KEY bukrs  = tsako-bukrs
                                            hkont  = tsako-hkont
                                            shkzg  = tsako-shkzg
                                            xlgclr = tsako-xlgclr TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          CONTINUE.
        ELSE.
*----- provide message for this account and reduce tsako
          PERFORM record_fill
              USING 'BSIS ' tsako-bukrs koart tsako-hkont space
                    space space char_l.
          DELETE tsako.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
