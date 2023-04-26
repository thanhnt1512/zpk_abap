*&---------------------------------------------------------------------*
*& Report ZPG_TEST_DATE_SUBTRACT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_TEST_DATE_SUBTRACT.

*DATA :E_MONATE  TYPE  KOMP-ANZ_MONATE.
*
*
*CALL FUNCTION 'MONTHS_BETWEEN_TWO_DATES_NEW'
*  EXPORTING
*    i_datum_bis             = '20231210'
*    i_datum_von             = '20231010'
**   I_KZ_INCL_BIS           = ' '
**   I_KZ_VOLLE_MONATE       = 'X'
* IMPORTING
*   E_MONATE                = E_MONATE
*          .
*DATA :lw_format_data TYPE string.
*lw_format_data = COnv char12( E_MONATE ).
*WRITE lw_format_data.

DATA :lw_month TYPe int8.

CALL FUNCTION 'HR_99S_MONTHS_BETWEEN_DATES'
  EXPORTING
    p_begda        = '20230101'
    p_endda        = '20220201'
*   P_COMPL        = ' '
 IMPORTING
   P_MONTHS       = lw_month
          .

write lw_month.
