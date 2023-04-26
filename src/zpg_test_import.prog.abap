*&---------------------------------------------------------------------*
*& Report ZPG_TEST_IMPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_import.
*TYPES:
*  BEGIN OF line_type,
*    col1 TYPE i,
*    col2 TYPE i,
*  END OF line_type,
*  tab_type TYPE STANDARD TABLE OF line_type WITH EMPTY KEY.
*DATA :lt_tab TYPE tab_type.
*IMPORT tab1 = lt_tab FROM DATABASE demo_indx_blob(sq) ID 'TABLE'.
RANGES: r_burks FOR aceobj-bukrs,
        r_objnm FOR acac_objects-acac_objnumber.
DATA: gt_period TYPE TABLE OF aceexpl_cal_val.
DATA: l_flg TYPE c VALUE 'X'.

r_objnm-sign = 'I'.
r_objnm-option = 'EQ'.
r_objnm-low = '0000000000000000000005'.
APPEND r_objnm.


r_burks-sign = 'I'.
r_burks-option = 'EQ'.
r_burks-low = '1000'.
APPEND r_burks.
EXPORT l_flg TO MEMORY ID 'ZVTP_CALL_ACACTREE02'.
SUBMIT acac_basisdata_sel_for_main WITH so_bukrs IN r_burks
                                   WITH so_objnm IN r_objnm
                                   AND RETURN.
*SUBMIT acac_basisdata_sel_for_main WITH so_bukrs EQ '1000'
*                                   WITH so_objnm EQ '0000000000000000000005'
*                                   AND RETURN.
IMPORT gt_period_cal_val TO gt_period
          FROM DATABASE indx(st) CLIENT sy-mandt ID 'ZEI_GET_DATA_MANUAL_ACCRUAL'.
DELETE FROM DATABASE indx(st) ID 'ZEI_GET_DATA_MANUAL_ACCRUAL'.

WRITE 'ok'.
