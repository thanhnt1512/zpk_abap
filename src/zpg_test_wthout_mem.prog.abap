*&---------------------------------------------------------------------*
*& Report ZPG_TEST_WTHOUT_MEM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_wthout_mem.
*TYPES ty_t_land1 TYPE STANDARD TABLE OF land1 WITH KEY table_line.
*
*SELECT * FROM kna1 INTO TABLE @data(t_kna1).
*
*DATA(t_land1) =  VALUE ty_t_land1(
*    FOR GROUPS land1 OF wa IN t_kna1
*    GROUP BY wa-land1 ASCENDING
**    WITHOUT MEMBERS
*    ( land1 ) ).
*BREAK-POINT.
*SELECT *
*       FROM spfli
*       INTO TABLE @DATA(spfli_tab).
*
*LOOP AT spfli_tab INTO DATA(wa)
*                  GROUP BY wa-carrid
*                  WITHOUT MEMBERS
*                  INTO DATA(key).
*  cl_demo_output=>write( key ).
*ENDLOOP.
*cl_demo_output=>display( ).

*TYPES:
*  BEGIN OF ty_customer,
*    customer TYPE char10,
*    NAME     TYPE char30,
*    city     TYPE char30,
*    route    TYPE char10,
*  END   OF ty_customer.
*TYPES: tt_customers TYPE SORTED TABLE OF ty_customer
*          WITH UNIQUE KEY customer.
*
*TYPES: tt_citys TYPE STANDARD TABLE OF char30 WITH EMPTY KEY.
*
*DATA(t_customres) =
*VALUE tt_customers(
*  ( customer = 'C0001' NAME = 'Test Customer 1' city = 'NY' route = 'R0001' )
*  ( customer = 'C0002' NAME = 'Customer 2'      city = 'LA' route = 'R0003' )
*  ( customer = 'C0003' NAME = 'Good Customer 3' city = 'DFW' route = 'R0001' )
*  ( customer = 'C0004' NAME = 'Best Customer 4' city = 'CH' route = 'R0003' )
*  ( customer = 'C0005' NAME = 'So So Customer 5' city = 'NY' route = 'R0001' )
*).
*
*
** Simply get the unique Routes, use WITHOUT MEMBERS
*LOOP AT t_customres INTO DATA(ls_cust_2)
*     GROUP BY  ( route = ls_cust_2-route )
*      ASCENDING
**      WITHOUT MEMBERS
*      REFERENCE INTO DATA(route_group_2).
*
*  WRITE: / route_group_2->route.
*
*
*ENDLOOP.
CLASS demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
  PRIVATE SECTION.
    TYPES: BEGIN OF line,
             col1 TYPE c LENGTH 1,
             col2 TYPE i,
             col3 TYPE i,
           END OF line,
           i_tab    TYPE HASHED TABLE OF line
                 WITH UNIQUE KEY col1 col2,
           tt_total TYPE STANDARD TABLE OF line WITH EMPTY KEY,
           tt_total1 TYPE STANDARD TABLE OF line .

    CLASS-METHODS sum IMPORTING line       TYPE line
                                base       TYPE line
                      RETURNING VALUE(sum) TYPE line.
ENDCLASS.

CLASS demo IMPLEMENTATION.
  METHOD main.
    DATA itab TYPE i_tab.

    itab = VALUE #(
      FOR j = 1 UNTIL j > 3
       ( col1 = 'A'
         col2 = j
         col3 = j ** 2 )
       ( col1 = 'B'
         col2 = 2 * j
         col3 = ( 2 * j ) ** 2 ) ).
    DATA(out) = cl_demo_output=>new( ).

*    DATA(total) = REDUCE line( INIT t = VALUE line( )
*                               FOR GROUPS OF <line> IN itab GROUP BY <line>-col1
*                               NEXT t = VALUE #( BASE t ( REDUCE line( INIT s = VALUE line( ) FOR line IN GROUP <line> NEXT s = VALUE line( BASE s  col1 = line-col1
*                                                                                                                          col2 = line-col2 + s-col2
*                                                                                                                          col3 = line-col2 + s-col2
*                                                                                                                          )
*                                                   )
*                               )
*                               )
*                               ).
******************************************************************************
*    DATA(total) = REDUCE line( INIT t = VALUE line( )
*                               FOR GROUPS OF <line> IN itab GROUP BY <line>-col1 DESCENDING
*                               LET tmp =  REDUCE line(  INIT s = VALUE line( )
*                                                                        FOR data IN GROUP <line>
*                                                                        NEXT s = VALUE line( BASE s col1 = data-col1
*                                                                                             col2 = data-col2 + s-col2
*                                                                                             col3 = data-col3 + s-col3
*                                                                                             )
*
*
*                                                       )
**                                   o = out->display( tmp )
*
*
*
*                               IN
*                               NEXT t = VALUE #( BASE t ( col1 = tmp-col1
*                                                      col2 = tmp-col2
*                                                      col3 = tmp-col3  ) )
*
*                             ).

*************************************************************************************
 DATA(total) =  VALUE tt_total(
                               FOR GROUPS OF <line> IN itab GROUP BY <line>-col1 DESCENDING
                               LET tmp =  REDUCE line(  INIT s = VALUE line( )
                                                                        FOR data IN GROUP <line>
                                                                        NEXT s = VALUE line( BASE s col1 = data-col1
                                                                                             col2 = data-col2 + s-col2
                                                                                             col3 = data-col3 + s-col3
                                                                                             )
                                                      )
                               IN ( tmp )
                               ).



*out->
*     DATA :lw_string TYPE string VALUE 'AZGGGG'.
*     if lw_string cp  'Z*'.
*       WRITE 'ok'.
*     ENDIF.
*    DATA(out) = cl_demo_output=>new( )->write( itab )->line( ).
*
*    DATA(total) = REDUCE line(
*      INIT t = VALUE line( )
*      FOR GROUPS OF <line> IN itab GROUP BY <line>-col1 ASCENDING
*      LET sum = REDUCE line( INIT s = VALUE line( )
*                             FOR line IN GROUP <line>
*                             NEXT s = sum( EXPORTING line = line
*                                                     base = s ) )
*          group = VALUE i_tab( FOR <wa> IN GROUP <line> ( <wa> ) )
*          o = out->write( group )->line( )->write( sum )->line( )
*      IN
*      NEXT t = sum( EXPORTING
*                          line = VALUE line( BASE sum col1 = '*' )
*                          base = t ) ).
*
*    out->line( )->display( total ).

*    cl_demo_output=>display_data( itab[] ).
 DATA :lt_tt TYPE tt_total1.
    lt_tt[] = total[].
    cl_demo_output=>display_data( total[] ).
  ENDMETHOD.
  METHOD sum.
    sum = VALUE #( BASE base
                   col1 = line-col1
                   col2 = sum-col2 + line-col2
                   col3 = sum-col3 + line-col3 ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  demo=>main( ).
