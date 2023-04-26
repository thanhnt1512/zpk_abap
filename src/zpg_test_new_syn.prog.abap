*&---------------------------------------------------------------------*
*& Report ZPG_TEST_NEW_SYN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_new_syn.
*****************************EXAMPLE 1
*TYPES: BEGIN OF lty_itab1,
*         key   TYPE char10,
*         value TYPE char10,
*         date  TYPE sy-datum,
*         time  TYPE uzeit,
*       END OF lty_itab1.
*DATA lt_itab1 TYPE STANDARD TABLE OF  lty_itab1.
*
*TYPES: BEGIN OF lty_itab2,
*         keyvalue TYPE char20,
*         date     TYPE sy-datum,
*       END OF lty_itab2.
*
*DATA lt_itab2 TYPE STANDARD TABLE OF  lty_itab2.
*
**Populate values to lt_itab1
*lt_itab1 = VALUE #(
*( key = 'A' value =  001 date = '08/28/2021' time = '1:00:00 PM' )
*( key = 'B' value =  002 date = '08/29/2021' time = '2:00:00 PM' )  ) .
*
*cl_demo_output=>display( lt_itab1 ).
*lt_itab2 = VALUE #( FOR lwa IN lt_itab1
*              LET base = VALUE lty_itab2( keyvalue = |{ lwa-key }{ lwa-value }| )
*              IN ( CORRESPONDING #( BASE ( base ) lwa ) ) ).
*
*cl_demo_output=>display( lt_itab2 ).

*****************************EXAMPLE 2
*DATA:
*  BEGIN OF struct1,
*    col1 TYPE i VALUE 11,
*    col2 TYPE i VALUE 12,
*  END OF struct1.
*
*DATA:
*  BEGIN OF struct2,
*    col2 TYPE i VALUE 22,
*    col3 TYPE i VALUE 23,
*  END OF struct2.
*
*  struct2 = CORRESPONDING #( BASE ( struct2 ) struct1 ).
*****************************EXAMPLE 3
TYPES:
   BEGIN OF struc,
     col1 TYPE i,
     col2 TYPE i,
   END OF struc.

DATA(rnd) = cl_abap_random_int=>create(
  seed = CONV i( sy-uzeit ) min = 1 max = 10 ).

DO 5 TIMES.
*  DATA(struc) = VALUE struc(
*    LET x = rnd->get_next( )
*        y = x * x
*        z = sy-index * 1000 IN col1 = x + z
*                               col2 = y + z ).
    DATA(struc) = VALUE struc( col1 = 12 col2 = 23 ).
  cl_demo_output=>write( struc ).
ENDDO.
cl_demo_output=>display( ).
