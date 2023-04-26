*&---------------------------------------------------------------------*
*& Report ZPG_TEST_COLLECT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_collect.

TYPES :BEGIN OF lty_vbseg,
         ausbk TYPE  ausbk,
         belnr TYPE belnr_d,
         gjahr TYPE  gjahr,
         shkzg TYPE shkzg,
         dmbtr TYPE dmbtr,
       END OF lty_vbseg.
DATA :lt_data TYPE STANDARD TABLE OF lty_vbseg .
DATA :lt_temp TYPE TABLE OF lty_vbseg.
DATA :ls_data LIKE LINE OF lt_data.

lt_temp = VALUE #( ( ausbk = '1000'  belnr = '0400000015' gjahr = '2022' )
                    ( ausbk = '1000'  belnr = '0400000030' gjahr = '2022' )
                    ( ausbk = '1000'  belnr = '0400001282' gjahr = '2021' )
                  ).
SELECT * FROM vbsegs INTO CORRESPONDING FIELDS OF ls_data FOR ALL ENTRIES IN lt_temp
WHERE ausbk = lt_temp-ausbk  AND belnr = lt_temp-belnr AND gjahr = lt_temp-gjahr.
  COLLECT ls_data INTO lt_data.
ENDSELECT.

CLEAR ls_data.
SELECT * FROM vbsegk INTO CORRESPONDING FIELDS OF ls_data FOR ALL ENTRIES IN lt_temp
WHERE ausbk = lt_temp-ausbk  AND belnr = lt_temp-belnr AND gjahr = lt_temp-gjahr.
  COLLECT ls_data INTO lt_data.
ENDSELECT.

SORT lt_data DESCENDING BY ausbk belnr gjahr dmbtr.
DELETE ADJACENT DUPLICATES FROM lt_data COMPARING ausbk belnr gjahr.
CALL SCREEN '0100'.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STT'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE to SCREEN 0.
  ENDCASE.
ENDMODULE.
