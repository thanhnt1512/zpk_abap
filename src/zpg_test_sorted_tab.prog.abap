*&---------------------------------------------------------------------*
*& Report ZPG_TEST_SORTED_TAB
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_sorted_tab.
*TYPES: BEGIN OF ts_line1
*          , src_sys TYPE sysysid
*          , field1 TYPE char10
*          , field2 TYPE char10
*      , END OF ts_line1
*      , tt_line1 TYPE STANDARD TABLE OF ts_line1
*        WITH UNIQUE SORTED KEY pr COMPONENTS src_sys field1.
*      .
*DATA :lt_itab1 TYPE tt_line1.
*lt_itab1 = VALUE #(  ( src_sys = 'ONE' field1 = 'F11' field2 = 'F21' )
*                    ( src_sys = 'TWO' field1 = 'F12' field2 = 'F22' )
*                    ( src_sys = 'THREE' field1 = 'F13' field2 = 'F23' )
*                    ( src_sys = 'ANES' field1 = 'F11' field2 = 'F24' ) ).
*GET RUN TIME FIELD DATA(lw_time1).
**READ TABLE lt_itab1 WITH TABLE KEY pr COMPONENTS src_sys = 'ANES' field1 = 'F11' INTO DATA(ls_itab1).
*READ TABLE lt_itab1 WITH KEY src_sys = 'ANES' INTO DATA(ls_itab1).
*GET RUN TIME FIELD DATA(lw_time2).
*DATA(lw_time3) = lw_time2 - lw_time1.
*lw_time3 = lw_time3 / 1000000.
*WRITE lw_time3.
**BREAK-POINT.
**cl_demo_output=>display_data( lt_itab1[] ).


*TYPES : tt_ac TYPE SORTED TABLE OF acdoca WITH NON-UNIQUE SORTED KEY key COMPONENTS belnr." docln.
DATA :lt_data_acdoca_with_uni TYPE SORTED TABLE OF acdoca WITH UNIQUE KEY rclnt rldnr rbukrs gjahr belnr docln, " docln.
      lt_data_acdoca_stand    TYPE TABLE OF acdoca.
SELECT * FROM acdoca INTO CORRESPONDING FIELDS OF TABLE lt_data_acdoca_with_uni .
SELECT * FROM acdoca INTO CORRESPONDING FIELDS OF TABLE lt_data_acdoca_stand .


GET RUN TIME FIELD DATA(lw_time3).
READ TABLE lt_data_acdoca_with_uni WITH TABLE KEY belnr = 'B000000081' TRANSPORTING NO FIELDS.
GET RUN TIME FIELD DATA(lw_time4).
DATA(lw_time6) = lw_time4 - lw_time3.
GET RUN TIME FIELD DATA(lw_time1).
READ TABLE lt_data_acdoca_stand WITH KEY belnr = 'B000000081' TRANSPORTING NO FIELDS.
GET RUN TIME FIELD DATA(lw_time2).
DATA(lw_time5) = lw_time2 - lw_time1.
*DATA(lw_time3) = lw_time2 - lw_time1.
*lw_time3 = lw_time3 / 1000000.
*WRITE lw_time3.

BREAK-POINT.
