*&---------------------------------------------------------------------*
*& Report ZPG_TEST_RANGE_DATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_TEST_RANGE_DATE.
DATA :check type abap_bool,
      var1 type string,
      var2 TYPE string,
      var3 type string,
      test type string.
var1 = '02'.
var2 = '20'.
var3 = '10'.
test = '2019' * '02' / '2020'.
if var1 < var2.
  WRITE test.
ENDIF.




*CALL FUNCTION 'GM_VALIDATE_DATE_RANGE'
*  EXPORTING
*    i_from_date                    =
*    i_to_date                      =
*  TABLES
*    t_daterange                    =
** EXCEPTIONS
**   RANGES_OVERLAP                 = 1
**   RANGE_HAS_HOLES                = 2
**   CONTINUOUS_BUT_EXCESSIVE       = 3
**   OTHERS                         = 4
*          .
*IF sy-subrc <> 0.
** Implement suitable error handling here
*ENDIF.
*
*CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
*  EXPORTING
**   BEGDA                   =
*    endda                   =
**   TAB_MODE                = ' '
**   IV_LEAP_YEAR_MODE       =
** IMPORTING
**   DAYS                    =
**   C_WEEKS                 =
**   C_MONTHS                =
**   C_YEARS                 =
**   WEEKS                   =
**   MONTHS                  =
**   YEARS                   =
**   D_MONTHS                =
**   MONTH_TAB               =
*          .
