*&---------------------------------------------------------------------*
*& Report ZPG_TEST_COMPARE_2_TAB
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_TEST_COMPARE_2_TAB.
*TYPES :BEGIN OF lty_tab,
*  col1 TYPE string,
*  col2 TYPE string,
*  col3 TYPE string,
*  END OF lty_tab.
*DATA : lt_tab1 TYPE TABLE of lty_tab.
*DATA : lt_tab2 TYPE TABLE of lty_tab.
*DATA : lt_res TYPE TABLE of lty_tab.
*lt_tab1 = VALUE #( ( col1 = '2' col2 = '3' col3 = '4' ) ( col1 = '5' col2 = '6' col3 = '7' ) ).
*lt_tab2 = VALUE #( ( col1 = '2' col2 = '3' col3 = '4' ) ( col1 = '9' col2 = '10' col3 = '11' ) ).
*lt_res = FILTER #( lt_tab2 in lt_tab1 WHERE col1 = col1 AND col2 = col2 AND col3 = col3 ).
*
*WITH NON-UNIQUE

DATA: BEGIN OF struct,
            col1 TYPE c LENGTH 1,
            col2 TYPE c LENGTH 1,
          END OF struct.

    DATA itab LIKE STANDARD TABLE OF struct
         WITH NON-UNIQUE KEY col1
         WITH UNIQUE sorted KEY sec_key COMPONENTS col2.

    DATA jtab LIKE itab.

    DATA(out) = cl_demo_output=>new( ).

    itab = VALUE #( ( col1 = 'A' col2 = '1' )
                    ( col1 = 'A' col2 = '2' )
                    ( col1 = 'B' col2 = '4' )
                    ( col1 = 'B' col2 = '3' )
                     ).

    LOOP AT itab INTO struct.
      APPEND struct TO jtab.
    ENDLOOP.
    out->write_data( jtab ).

    CLEAR jtab.
    SORT itab BY col2 DESCENDING.

    LOOP AT itab INTO struct.
      APPEND struct TO jtab.
    ENDLOOP.
    out->write_data( jtab ).

    CLEAR jtab.
    LOOP AT itab INTO struct USING KEY sec_key.
      APPEND struct TO jtab.
    ENDLOOP.
    out->write_data( jtab ).

    out->display( ).
