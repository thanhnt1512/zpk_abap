*&---------------------------------------------------------------------*
*& Include          ZIN_EVTP_REPORT_F01
*&---------------------------------------------------------------------*

FORM get_data.
  DATA: ls_data LIKE LINE OF gt_data,
        lt_data LIKE gt_data.
  DATA: "lw_dayfw TYPE dats,
    lw_day7_last  TYPE dats,
    lw_day8_last  TYPE dats,
    lw_day9_last  TYPE dats,
    lw_day10_last TYPE dats,
    lw_day11_last TYPE dats,
    lw_day12_last TYPE dats,
    lw_day1       TYPE dats,
    lw_day2       TYPE dats,
    lw_day3       TYPE dats,
    lw_day4       TYPE dats,
    lw_day5       TYPE dats,
    lw_day6       TYPE dats,
    lw_day7       TYPE dats,
    lw_day8       TYPE dats,
    lw_day9       TYPE dats,
    lw_day10      TYPE dats,
    lw_day11      TYPE dats,
    lw_day12      TYPE dats.
  DATA: lw_partner_grid TYPE zdt_partner_grid.
  DATA :lw_budat_last1   TYPE int4,
        lw_budat_last(4) TYPE n.
  lw_budat_last1 = p_budat(4) - 1.
  lw_budat_last = lw_budat_last1.

  "2020
  "CONCATENATE lw_budat_last '07' '01' INTO lw_dayfw.
  CONCATENATE lw_budat_last '08' '01' INTO lw_day7_last.
  lw_day7_last = lw_day7_last - 1.
  CONCATENATE lw_budat_last '09' '01' INTO lw_day8_last.
  lw_day8_last = lw_day8_last - 1.
  CONCATENATE lw_budat_last '10' '01' INTO lw_day9_last.
  lw_day9_last = lw_day9_last - 1.
  CONCATENATE lw_budat_last '11' '01' INTO lw_day10_last.
  lw_day10_last = lw_day10_last - 1.
  CONCATENATE lw_budat_last '12' '01' INTO lw_day11_last.
  lw_day11_last = lw_day11_last - 1.
  CONCATENATE lw_budat_last '12' '31' INTO lw_day12_last.

  "2021
  CONCATENATE p_budat(4) '02' '01' INTO lw_day1.
  lw_day1 = lw_day1 - 1.
  CONCATENATE p_budat(4) '03' '01' INTO lw_day2.
  lw_day2 = lw_day2 - 1.
  CONCATENATE p_budat(4) '04' '01' INTO lw_day3.
  lw_day3 = lw_day3 - 1.
  CONCATENATE p_budat(4) '05' '01' INTO lw_day4.
  lw_day4 = lw_day4 - 1.
  CONCATENATE p_budat(4) '06' '01' INTO lw_day5.
  lw_day5 = lw_day5 - 1.
  CONCATENATE p_budat(4) '07' '01' INTO lw_day6.
  lw_day6 = lw_day6 - 1.
  CONCATENATE p_budat(4) '08' '01' INTO lw_day7.
  lw_day7 = lw_day7 - 1.
  CONCATENATE p_budat(4) '09' '01' INTO lw_day8.
  lw_day8 = lw_day8 - 1.
  CONCATENATE p_budat(4) '10' '01' INTO lw_day9.
  lw_day9 = lw_day9 - 1.
  CONCATENATE p_budat(4) '11' '01' INTO lw_day10.
  lw_day10 = lw_day10 - 1.
  CONCATENATE p_budat(4) '12' '01' INTO lw_day11.
  lw_day11 = lw_day11 - 1.
  CONCATENATE p_budat(4) '12' '31' INTO lw_day12.

  SELECT ac~rbukrs, ac~gjahr, ac~belnr, ac~docln, ac~kunnr, ac~prctr, ac~buzei, ac~hsl, ac~rhcur, bs~zfbdt
    FROM acdoca AS ac
     INNER JOIN bseg AS bs
      ON bs~bukrs = ac~rbukrs
      AND bs~gjahr = ac~gjahr
      AND bs~belnr = ac~belnr
      AND bs~buzei = ac~buzei
      INTO TABLE @DATA(lt_acdoca)
    WHERE ac~racct = @p_racct AND
          ac~budat <= @p_budat AND
          ac~prctr IN @s_pc AND
          ac~segment IN @s_segm AND
          ac~kunnr IN @s_kunnr AND
          ac~kunnr <> '' AND
          ac~koart = 'D'
          AND ac~rldnr = '0L'
          AND ( ac~augdt = '00000000' OR ac~augdt > @p_budat ).
  LOOP AT lt_acdoca INTO DATA(ls_acdoca).

    CLEAR ls_data.
    ls_data-kunnr = ls_acdoca-kunnr.
    ls_data-rhcur = ls_acdoca-rhcur.
    ls_data-prctr = ls_acdoca-prctr.

    "AMOUNT IN 2020
*    IF ls_acdoca-zfbdt < lw_dayfw AND ls_acdoca-zfbdt IS NOT INITIAL .
*      ls_data-amount_fw = ls_acdoca-hsl.
*    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day7_last AND ls_acdoca-zfbdt IS NOT INITIAL. "AND ls_acdoca-zfbdt >= lw_dayfw.
      ls_data-am_m7_pr = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day8_last AND ls_acdoca-zfbdt > lw_day7_last.
      ls_data-am_m8_pr = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day9_last AND ls_acdoca-zfbdt > lw_day8_last.
      ls_data-am_m9_pr = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day10_last AND ls_acdoca-zfbdt > lw_day9_last.
      ls_data-am_m10_pr = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day11_last AND ls_acdoca-zfbdt > lw_day10_last.
      ls_data-am_m11_pr = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day12_last AND ls_acdoca-zfbdt > lw_day11_last.
      ls_data-am_m12_pr = ls_acdoca-hsl.
    ENDIF.

    "AMOUNT IN 2021
    IF ls_acdoca-zfbdt <= lw_day1 AND ls_acdoca-zfbdt > lw_day12_last.
      ls_data-amount_m1 = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day2 AND ls_acdoca-zfbdt > lw_day1.
      ls_data-amount_m2 = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day3  AND ls_acdoca-zfbdt > lw_day2.
      ls_data-amount_m3 = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day4  AND ls_acdoca-zfbdt > lw_day3.
      ls_data-amount_m4 = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day5  AND ls_acdoca-zfbdt > lw_day4.
      ls_data-amount_m5 = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day6  AND ls_acdoca-zfbdt > lw_day5.
      ls_data-amount_m6 = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day7  AND ls_acdoca-zfbdt > lw_day6.
      ls_data-amount_m7 = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day8  AND ls_acdoca-zfbdt > lw_day7.
      ls_data-amount_m8 = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day9  AND ls_acdoca-zfbdt > lw_day8.
      ls_data-amount_m9 = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day10  AND ls_acdoca-zfbdt > lw_day9.
      ls_data-amount_m10 = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day11  AND ls_acdoca-zfbdt > lw_day10.
      ls_data-amount_m11 = ls_acdoca-hsl.
    ENDIF.
    IF ls_acdoca-zfbdt <= lw_day12  AND ls_acdoca-zfbdt > lw_day11.
      ls_data-amount_m12 = ls_acdoca-hsl.
    ENDIF.
*    IF ls_data-amount_m1 IS NOT INITIAL OR
*       ls_data-amount_m2 IS NOT INITIAL OR
*       ls_data-amount_m3 IS NOT INITIAL OR
*       ls_data-amount_m4 IS NOT INITIAL OR
*       ls_data-amount_m5 IS NOT INITIAL OR
*       ls_data-amount_m6 IS NOT INITIAL OR
*       ls_data-amount_m7 IS NOT INITIAL OR
*       ls_data-amount_m8 IS NOT INITIAL OR
*       ls_data-amount_m9 IS NOT INITIAL OR
*       ls_data-amount_m10 IS NOT INITIAL OR
*       ls_data-amount_m11 IS NOT INITIAL OR
*       ls_data-amount_m12 IS NOT INITIAL.
    COLLECT ls_data INTO gt_data.
*    ENDIF.
  ENDLOOP.

  lt_data[] = gt_data[].
  SORT lt_data BY kunnr.
  DELETE ADJACENT DUPLICATES FROM lt_data COMPARING kunnr.

  DATA lt_bp_ext TYPE TABLE OF bu_bpext.

  LOOP AT lt_data INTO ls_data.
    APPEND ls_data-kunnr TO lt_bp_ext.
  ENDLOOP.

  IF gt_data[] IS NOT INITIAL.
    SELECT * FROM cepct INTO TABLE @DATA(lt_cepct)
      FOR ALL ENTRIES IN @gt_data
      WHERE prctr = @gt_data-prctr.
    SORT lt_cepct BY prctr.
    SELECT  partner,
            type,
            name_first,
            name_last,
            name_org2,
            name_org3,
            name_org4 FROM but000 INTO TABLE @DATA(lt_but0)
      FOR ALL ENTRIES IN @lt_data
      WHERE partner = @lt_data-kunnr.
    SORT lt_but0 BY partner.

    SELECT * FROM ztb_part_gr_map INTO TABLE @DATA(lt_part_gr_map)
      FOR ALL ENTRIES IN @lt_bp_ext
      WHERE ev_partner1 = @lt_bp_ext-table_line.
    SORT lt_part_gr_map BY ev_partner1.
    IF lt_part_gr_map IS NOT INITIAL.
      SELECT * FROM ztb_partner_gr INTO TABLE @DATA(lt_partner_gr).
*        FOR ALL ENTRIES IN @lt_part_gr_map
*        WHERE partner_grid = @lt_part_gr_map-partner_grid.
      SORT lt_partner_gr BY partner_grid.
    ENDIF.
    "add by thanhnt 13/6/2022
    SELECT kunnr,zterm FROM knb1 INTO TABLE @DATA(lt_zterm) FOR ALL ENTRIES IN @lt_acdoca
    WHERE kunnr = @lt_acdoca-kunnr.
    SORT lt_zterm BY kunnr.
    "end
  ENDIF.

  CLEAR ls_data.
  ls_data-kunnr_name = 'Tổng cộng'.
  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
    READ TABLE lt_zterm WITH KEY kunnr = <fs_data>-kunnr INTO DATA(ls_zterm) BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_data>-payment_term = ls_zterm-zterm .
      CLEAR ls_zterm.
    ENDIF.
    <fs_data>-amount_tt = <fs_data>-amount_m1 + <fs_data>-amount_m2 + <fs_data>-amount_m3
          + <fs_data>-amount_m4 + <fs_data>-amount_m5 + <fs_data>-amount_m6 + <fs_data>-amount_m7 + <fs_data>-amount_m8
          + <fs_data>-amount_m9 + <fs_data>-amount_m10 + <fs_data>-amount_m11 + <fs_data>-amount_m12 + <fs_data>-am_m7_pr + <fs_data>-am_m8_pr
          + <fs_data>-am_m9_pr + <fs_data>-am_m10_pr + <fs_data>-am_m11_pr + <fs_data>-am_m12_pr.
    READ TABLE lt_but0 INTO DATA(ls_but0)
    WITH KEY partner = <fs_data>-kunnr
    BINARY SEARCH.
    IF sy-subrc = 0.
      CASE ls_but0-type.
        WHEN 1.
          <fs_data>-kunnr_name = |{ ls_but0-name_first } { ls_but0-name_last }|.
        WHEN 2 OR 3.
          <fs_data>-kunnr_name = |{ ls_but0-name_org2 } { ls_but0-name_org3 } { ls_but0-name_org4 }|.
      ENDCASE.
    ENDIF.

    READ TABLE lt_part_gr_map INTO DATA(ls_part_gr_map)
    WITH KEY ev_partner1 = <fs_data>-kunnr
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_part_gr_map-partner_grid
        IMPORTING
          output = lw_partner_grid.
      READ TABLE lt_partner_gr INTO DATA(ls_partner_gr)
      WITH KEY partner_grid = lw_partner_grid
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_data>-partner_grid = ls_partner_gr-partner_grid.
        <fs_data>-partner_grid_val = ls_partner_gr-partner_value.
        <fs_data>-partner_grid_name = ls_partner_gr-name.
      ENDIF.
    ENDIF.

    READ TABLE lt_cepct INTO DATA(ls_cepct)
    WITH KEY prctr = <fs_data>-prctr
    BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_data>-prctr_name = ls_cepct-ltext.
    ENDIF.
    ls_data-rhcur = 'VND'.
    "ls_data-amount_fw = ls_data-amount_fw + <fs_data>-amount_fw.
*    ls_data-am_m7_pr = ls_data-am_m7_pr + <fs_data>-am_m7_pr.
*    ls_data-am_m8_pr = ls_data-am_m8_pr + <fs_data>-am_m8_pr.
*    ls_data-am_m9_pr = ls_data-am_m9_pr + <fs_data>-am_m9_pr.
*    ls_data-am_m10_pr = ls_data-am_m10_pr + <fs_data>-am_m10_pr.
*    ls_data-am_m11_pr = ls_data-am_m11_pr + <fs_data>-am_m11_pr.
*    ls_data-am_m12_pr = ls_data-am_m12_pr + <fs_data>-am_m12_pr.
*    ls_data-amount_m1 = ls_data-amount_m1 + <fs_data>-amount_m1.
*    ls_data-amount_m2 = ls_data-amount_m2 + <fs_data>-amount_m2.
*    ls_data-amount_m3 = ls_data-amount_m3 + <fs_data>-amount_m3.
*    ls_data-amount_m4 = ls_data-amount_m4 + <fs_data>-amount_m4.
*    ls_data-amount_m5 = ls_data-amount_m5 + <fs_data>-amount_m5.
*    ls_data-amount_m6 = ls_data-amount_m6 + <fs_data>-amount_m6.
*    ls_data-amount_m7 = ls_data-amount_m7 + <fs_data>-amount_m7.
*    ls_data-amount_m8 = ls_data-amount_m8 + <fs_data>-amount_m8.
*    ls_data-amount_m9 = ls_data-amount_m9 + <fs_data>-amount_m9.
*    ls_data-amount_m10 = ls_data-amount_m10 + <fs_data>-amount_m10.
*    ls_data-amount_m11 = ls_data-amount_m11 + <fs_data>-amount_m11.
*    ls_data-amount_m12 = ls_data-amount_m12 + <fs_data>-amount_m12.
*    ls_data-amount_tt = ls_data-amount_tt + <fs_data>-amount_tt.
  ENDLOOP.
  DATA lw_sum TYPE fins_vhcur12.
  ls_data-am_m7_pr = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-am_m7_pr ).
  ls_data-am_m8_pr = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-am_m8_pr ).
  ls_data-am_m9_pr = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-am_m9_pr ).
  ls_data-am_m10_pr = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-am_m10_pr ).
  ls_data-am_m11_pr = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-am_m11_pr ).
  ls_data-am_m12_pr = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-am_m12_pr ).
  ls_data-amount_m1 = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-amount_m1 ).
  ls_data-amount_m2 = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-amount_m2 ).
  ls_data-amount_m3 = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-amount_m3 ).
  ls_data-amount_m4 = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-amount_m4 ).
  ls_data-amount_m5 = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-amount_m5 ).
  ls_data-amount_m6 = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-amount_m6 ).
  ls_data-amount_m7 = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-amount_m7 ).
  ls_data-amount_m8 = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-amount_m8 ).
  ls_data-amount_m9 = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-amount_m9 ).
  ls_data-amount_m10 = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-amount_m10 ).
  ls_data-amount_m11 = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-amount_m11 ).
  ls_data-amount_m12 = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-amount_m12 ).
  ls_data-amount_tt = REDUCE fins_vhcur12( INIT lv_x = lw_sum FOR lw_total IN gt_data NEXT lv_x = lv_x + lw_total-amount_tt ).

  IF gt_data IS NOT INITIAL.
    APPEND ls_data TO gt_data.
  ENDIF.
  DELETE gt_data WHERE am_m7_pr IS INITIAL
                   AND am_m8_pr IS INITIAL AND am_m9_pr IS INITIAL
                   AND am_m10_pr IS INITIAL AND am_m11_pr IS INITIAL
                   AND am_m12_pr IS INITIAL AND amount_m1 IS INITIAL AND amount_m2 IS INITIAL
                   AND amount_m3 IS INITIAL AND amount_m4 IS INITIAL AND amount_m5 IS INITIAL
                   AND amount_m6 IS INITIAL AND amount_m7 IS INITIAL AND amount_m8 IS INITIAL
                   AND amount_m9 IS INITIAL AND amount_m10 IS INITIAL AND amount_m11 IS INITIAL
                   AND amount_m12 IS INITIAL AND amount_tt IS INITIAL .

ENDFORM.

FORM display_alv .
  PERFORM f_layout.
  PERFORM f_fieldcat.
  PERFORM f_display_alv TABLES gt_data.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_layout .
  x_layout-no_input          = 'X'.
  x_layout-zebra             = 'X'.
  x_layout-colwidth_optimize = 'X'.
  x_layout-info_fieldname    = 'COLOR'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fieldcat .
  DEFINE lm_fieldcat.
    t_fieldcat-fieldname   = &1.
    t_fieldcat-seltext_m   = &2.
    t_fieldcat-cfieldname = &3.
    t_fieldcat-qfieldname = &4.
    t_fieldcat-no_zero = &5.
    t_fieldcat-edit_mask = &6.
    APPEND t_fieldcat.
    CLEAR t_fieldcat.
  END-OF-DEFINITION.

  lm_fieldcat 'KUNNR' 'CustID' '' '' 'X' ''.
  lm_fieldcat 'KUNNR_NAME' 'CustName' '' '' 'X' ''.
  "lm_fieldcat 'AMOUNT_FW' 'Sodu' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AM_M7_PR' 'T07 N-1' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AM_M8_PR' 'T08 N-1' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AM_M9_PR' 'T09 N-1' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AM_M10_PR' 'T10 N-1' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AM_M11_PR' 'T11 N-1' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AM_M12_PR' 'T12 N-1' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AMOUNT_M1' 'T01 N' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AMOUNT_M2' 'T02 N' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AMOUNT_M3' 'T03 N' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AMOUNT_M4' 'T04 N' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AMOUNT_M5' 'T05 N' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AMOUNT_M6' 'T06 N' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AMOUNT_M7' 'T07 N' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AMOUNT_M8' 'T08 N' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AMOUNT_M9' 'T09 N' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AMOUNT_M10' 'T10 N' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AMOUNT_M11' 'T11 N' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AMOUNT_M12' 'T12 N' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'AMOUNT_TT' 'Total' 'RHCUR' '' 'X' ''.
  lm_fieldcat 'RHCUR' 'Loại tiền' '' '' 'X' ''.
  lm_fieldcat 'PRCTR' 'SiteID' '' '' 'X' ''.
  lm_fieldcat 'PRCTR_NAME' 'SiteName' '' '' 'X' ''.
  lm_fieldcat 'PARTNER_GRID' 'Partner Group ID' '' '' 'X' ''.
  lm_fieldcat 'PARTNER_GRID_VAL' 'Partner Group Value' '' '' 'X' ''.
  lm_fieldcat 'PARTNER_GRID_NAME' 'Partner Group Name' '' '' 'X' ''.
  lm_fieldcat 'PAYMENT_TERM' 'Partment Term' '' '' 'X' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_LOG  text
*----------------------------------------------------------------------*
FORM f_display_alv  TABLES pt_data.
  d_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = d_repid
      is_layout                = x_layout
      it_fieldcat              = t_fieldcat[]
*     i_callback_top_of_page   = 'TOP-OF-PAGE'  "see FORM
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'F_USER_COMMAND'
      i_save                   = 'X'
*     it_sort                  = t_sort[]
*     i_grid_title             = lv_header
    TABLES
      t_outtab                 = pt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

FORM f_user_command USING  ucomm        LIKE sy-ucomm
                           ps_selfield  TYPE slis_selfield.
  CASE ucomm.
    WHEN 'EXPORT_EXCEL'.
      PERFORM f_export_excel.
    WHEN 'EXPORT'.
      PERFORM export_excel.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
  ENDCASE.
ENDFORM.

FORM set_pf_status USING lv_extab TYPE slis_t_extab.        "#EC *
  SET PF-STATUS 'GUI_002'.
ENDFORM.                    "SET_PF_STATUS

*&---------------------------------------------------------------------*
*& Form F_EXPORT_EXCEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_export_excel .
  DATA:
    lv_filename TYPE string,
    lv_fullpath TYPE string,
    lv_path     TYPE string,
    lv_action   TYPE i,
    lv_file     TYPE string.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Please select the location'
      default_extension = 'XLSX'
      default_file_name = lv_file
      file_filter       = '*.XLSX'
    CHANGING
      filename          = lv_filename
      path              = lv_path
      fullpath          = lv_fullpath
      user_action       = lv_action
    EXCEPTIONS
      cntl_error        = 1
      error_no_gui      = 2
      OTHERS            = 3.
  IF sy-subrc IS INITIAL.
* save file to desktop
    CALL FUNCTION 'ZXLWB_CALLFORM'
      EXPORTING
        iv_formname        = 'ZXLS_FIR033'
*       iv_context_ref     = ls_gl_bal_xlwb
        iv_viewer_suppress = 'X'
        iv_save_as         = lv_fullpath
      EXCEPTIONS
        OTHERS             = 2.
    IF sy-subrc NE 0 .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
    ENDIF .

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXPORT_EXCEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM export_excel .
*  DATA: lv_num            TYPE char10,
*        lw_amount         TYPE zdd_amt_disp,
*        lw_formname       TYPE char20,
*        ls_data_excel     TYPE zst_fir033_excel,
*        ls_data_excel_dtl TYPE zst_fir033_excel_dtl,
*        lw_date_fr        TYPE char10,
*        lw_date_to        TYPE char10,
*        lt_segmt          TYPE STANDARD TABLE OF fagl_segmt.
** signal
*  ls_data_excel-sign_finace = p_finace.
*  ls_data_excel-sign_direct = p_direct.
*  ls_data_excel-sign_acc_cq = p_acc_cq.
*  ls_data_excel-sign_acc_ch = p_acc_ch.
*
**  convert amount
*  LOOP AT gt_data INTO DATA(ls_data) WHERE kunnr_name NE 'Tổng cộng'.
*    MOVE-CORRESPONDING ls_data TO ls_data_excel_dtl.
*    ls_data_excel_dtl-stt = sy-tabix.
*
**    lw_amount = ls_data-amount_fw.
**    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
**      EXPORTING
**        currency        = ls_data-rhcur
**        amount_internal = lw_amount
**      IMPORTING
**        amount_display  = lw_amount.
**    IF sy-subrc IS INITIAL.
**      ls_data_excel_dtl-amount_fw = lw_amount.
**    ENDIF.
*
*    lw_amount = ls_data-am_m7_pr.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-am_m7_pr = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-am_m8_pr.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-am_m8_pr = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-am_m9_pr.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-am_m9_pr = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-am_m10_pr.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-am_m10_pr = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-am_m11_pr.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-am_m11_pr = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-am_m12_pr.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-am_m12_pr = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-amount_m1.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-amount_m1 = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-amount_m2.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-amount_m2 = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-amount_m3.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-amount_m3 = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-amount_m4.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-amount_m4 = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-amount_m5.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-amount_m5 = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-amount_m6.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-amount_m6 = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-amount_m7.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-amount_m7 = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-amount_m8.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-amount_m8 = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-amount_m9.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-amount_m9 = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-amount_m10.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-amount_m10 = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-amount_m11.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-amount_m11 = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-amount_m12.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-amount_m12 = lw_amount.
*    ENDIF.
*
*    lw_amount = ls_data-amount_tt.
*    CALL FUNCTION 'ZFM_CONV_AMOUNT_SAP_TO_DISPLAY'
*      EXPORTING
*        currency        = ls_data-rhcur
*        amount_internal = lw_amount
*      IMPORTING
*        amount_display  = lw_amount.
*    IF sy-subrc IS INITIAL.
*      ls_data_excel_dtl-amount_tt = lw_amount.
*    ENDIF.
*
*    APPEND ls_data_excel_dtl TO ls_data_excel-detail.
*    CLEAR ls_data_excel_dtl.
*  ENDLOOP.
**  get segment description
*  SELECT *
*    FROM fagl_segmt
*    INTO TABLE lt_segmt
*    WHERE langu = sy-langu
*      AND segment IN s_segm.
*  IF sy-subrc IS INITIAL.
*    IF lines( lt_segmt ) = 1.
*      READ TABLE lt_segmt INTO DATA(ls_segmt) INDEX 1.
*      CONCATENATE TEXT-003 ls_segmt-name INTO ls_data_excel-segmt_nm SEPARATED BY space.
*    ENDIF.
*  ENDIF.
** get account
*  CONCATENATE TEXT-005 p_racct INTO ls_data_excel-racct_nm SEPARATED BY space.
** get posting date
**  IF s_date-high IS INITIAL.
*  CONCATENATE p_budat+6(2) p_budat+4(2) p_budat(4) INTO lw_date_fr SEPARATED BY '/'.
*  CONCATENATE TEXT-004 lw_date_fr INTO ls_data_excel-psdt_c SEPARATED BY space.
**  ELSE.
**    CONCATENATE s_date-low+6(2) s_date-low+4(2) s_date-low(4) INTO lw_date_fr SEPARATED BY '/'.
**    CONCATENATE s_date-high+6(2) s_date-high+4(2) s_date-high(4) INTO lw_date_to SEPARATED BY '/'.
**    CONCATENATE TEXT-014 lw_date_fr TEXT-015 lw_date_to INTO ls_data_excel-psdt_c SEPARATED BY space.
**  ENDIF.
** total
*  IF lines( ls_data_excel-detail ) EQ 0.
*    APPEND INITIAL LINE TO ls_data_excel-detail.
*  ELSE.
*    DESCRIBE TABLE ls_data_excel-detail LINES lv_num.
*    CONDENSE lv_num.
*    CONCATENATE '=SUM(R[-1]C:R[-' lv_num ']C)' INTO ls_data_excel-total.
*  ENDIF.
*
** export
*  CALL FUNCTION 'ZXLWB_CALLFORM'
*    EXPORTING
*      iv_formname    = 'ZXLS_FIR033_EXP'
*      iv_context_ref = ls_data_excel
**     iv_viewer_suppress = 'X'
**     iv_viewer_title = gs_report_list-rp_des
**     iv_save_as     = lv_fullpath
*    EXCEPTIONS
*      OTHERS         = 2.
*  IF sy-subrc NE 0 .
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
*  ENDIF .
ENDFORM.
