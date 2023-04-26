*&---------------------------------------------------------------------*
*& Report ZPG_RAISE_MESS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_raise_mess.
*MESSAGE S079(/FTI/INTERNAL_ERROR) DISPLAY LIKE 'E'.
****************
CLASS lcl_test DEFINITION.
  PUBLIC SECTION.
    DATA :lw_number TYPE int1.
    METHODS :constructor IMPORTING number TYPE int1,
      check.

*  METHOD :check.

ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD constructor.
    lw_number  = number.
  ENDMETHOD.

  METHOD check.
    TRY.
        IF lw_number = 0.
          RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
            MESSAGE ID 'ACAC_EXCEL'
            TYPE 'E'
            NUMBER '036'
            WITH '2123124'.
        ENDIF.

      CATCH BEFORE UNWIND cx_acac_xlsx_exception INTO DATA(lx_acac_xlsx).
        cl_ace_message_handler=>get_instance( )->add_message_inst( ix_ace = lx_acac_xlsx ).
        IF cl_ace_message_handler=>get_instance( )->get_is_resumable( ) = abap_true.
          RESUME.
        ENDIF.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


START-OF-SELECTION.

  DATA: ex  TYPE REF TO cx_acac_xlsx_exception,
        mes TYPE string.

  DATA(obj) = NEW lcl_test( 0 ).

  TRY.
      obj->check( ).
*    CATCH BEFORE UNWIND cx_acac_xlsx_exception INTO DATA(lx_acac_xlsx).
*      cl_ace_message_handler=>get_instance( )->add_message_inst( ix_ace = lx_acac_xlsx ).
*      IF cl_ace_message_handler=>get_instance( )->get_is_resumable( ) = abap_true.
*        RESUME.
*      ENDIF.



  ENDTRY.
**************

*TRY .
*    IF 1 = 2.
**      RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
**        MESSAGE ID 'ACAC_EXCEL'
**        TYPE 'E'
**        NUMBER '035'
**        WITH '2123124'.
*      RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
*        EXPORTING
*          gv_comp = '1000'.
*
*    ENDIF.
*  CATCH BEFORE UNWIND cx_acac_xlsx_exception INTO DATA(lx_acac_xlsx).
*    cl_ace_message_handler=>get_instance( )->add_message_inst( ix_ace = lx_acac_xlsx ).
*    IF cl_ace_message_handler=>get_instance( )->get_is_resumable( ) = abap_true.
*      RESUME.
*    ENDIF.
*
**  CATCH BEFORE UNWIND cx_acac_xlsx_exception INTO DATA(lx_acac_xlsx).
**    cl_ace_message_handler=>get_instance( )->add_message_inst( ix_ace = lx_acac_xlsx ).
**    IF cl_ace_message_handler=>get_instance( )->get_is_resumable( ) = abap_true.
**      RESUME.
**    ENDIF.
*
*ENDTRY.
