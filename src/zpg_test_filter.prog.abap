*&---------------------------------------------------------------------*
*& Report ZPG_TEST_FILTER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_TEST_FILTER.
    types: BEGIN OF ts_line1
              , src_sys TYPE sysysid
              , field1 TYPE char10
              , field2 TYPE char10
          , END OF ts_line1
          , tt_line1 TYPE STANDARD TABLE OF ts_line1
            with NON-UNIQUE SORTED KEY PR COMPONENTS src_sys
          .

    types: BEGIN OF ts_line2
              , src_sys TYPE sysysid
          , END OF ts_line2
          , tt_line2 TYPE STANDARD TABLE OF ts_line2
          .

    data lt_itab1 TYPE tt_line1.
    data lt_itab2 TYPE tt_line2.


    lt_itab1 = VALUE #(  ( src_sys = 'ONE' field1 = 'F11' field2 = 'F21' )
                                ( src_sys = 'TWO' field1 = 'F12' field2 = 'F22' )
                                ( src_sys = 'THREE' field1 = 'F13' field2 = 'F23' )
                                ( src_sys = 'FOUR' field1 = 'F14' field2 = 'F24' ) ).


    lt_itab2 = VALUE #( ( src_sys = 'TWO' ) ( src_sys = 'FOUR' ) ( src_sys = 'FOUR' ) ).

""" to delete records that are not in lt_itab2
data(lt_itab10) = FILTER #( lt_itab1 USING key pr IN lt_itab2  WHERE src_sys = src_sys ).


""" to save records that are not in lt_itab2
    data(lt_itab11) = FILTER #(
       lt_itab1 USING KEY pr EXCEPT IN lt_itab2
       where src_sys = src_sys
    ).

    cl_demo_output=>display_data( lt_itab11[] ).
    cl_demo_output=>display_data( lt_itab2[] ).
