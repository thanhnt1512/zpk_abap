*&---------------------------------------------------------------------*
*& Report ZPG_TEST_CALL_WEB_SV
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_call_web_sv.
DATA(proxy_test) = NEW ztntco_calculator_soap( ).
DATA: input  TYPE ZTNTADD_SOAP_IN,
      output TYPE ZTNTADD_SOAP_OUT.
input-int_a = 1.
input-int_b = 1.
TRY.
CALL METHOD proxy_test->add
  EXPORTING
    input  = input
  IMPORTING
    output = output
    .
*CALL METHOD proxy_test->divide
*  EXPORTING
*    input  = input
*  IMPORTING
*    output = output.
 CATCH cx_ai_system_fault .
ENDTRY.

BREAK-POINT.
