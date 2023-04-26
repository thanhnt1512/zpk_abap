*&---------------------------------------------------------------------*
*& Report ZPG_TEST_COMMENT_SEL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_TEST_COMMENT_SEL.SELECTION-SCREEN COMMENT /1(50) comm1 MODIF ID mg1.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN COMMENT /1(10) comm2.
SELECTION-SCREEN ULINE /10(10).
PARAMETERS: r1 RADIOBUTTON GROUP rad1,
            r2 RADIOBUTTON GROUP rad1,
            r3 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN ULINE /1(50).

AT SELECTION-SCREEN OUTPUT.
  comm1 = 'Selection Screen'.
  comm2 = 'Select one'.
  LOOP AT SCREEN INTO DATA(screen_wa).
    IF screen_wa-group1 = 'MG1'.
       screen_wa-intensified = '1'.
      MODIFY SCREEN FROM screen_wa.
    ENDIF.
  ENDLOOP.
