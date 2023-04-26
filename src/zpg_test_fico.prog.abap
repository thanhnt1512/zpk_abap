*&---------------------------------------------------------------------*
*& Report ZPG_TEST_FICO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE ZPG_TEST_FICOTOP                        .    " Global Data

* INCLUDE ZPG_TEST_FICOO01                        .  " PBO-Modules
* INCLUDE ZPG_TEST_FICOI01                        .  " PAI-Modules
 INCLUDE ZPG_TEST_FICOF01                        .  " FORM-Routines
 INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_segm-low.
  DATA: BEGIN OF lvalue_tab OCCURS 0,
          segment LIKE zsql_segment-segment,
          name    LIKE zsql_segment-name,
        END OF lvalue_tab.

  SELECT DISTINCT segment,
                  name
  FROM zddl_segment_zfir031
  INTO CORRESPONDING FIELDS OF TABLE @lvalue_tab
  ORDER BY segment.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'SEGMENT'
      dynprofield = 'S_SEGM'
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      value_org   = 'S'
    TABLES
      value_tab   = lvalue_tab.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM display_alv.
