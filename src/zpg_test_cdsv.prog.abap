*&---------------------------------------------------------------------*
*& Report zpg_test_cdsv
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_cdsv.
select sum(  WAABWC ) FROM z_test_cdsv1
INTO  @data(lt_data).
cl_demo_output=>display_data( lt_data ).
"DESCRIBE TABLE lt_data LINES DATA(lw_lines).
"write lw_lines.
