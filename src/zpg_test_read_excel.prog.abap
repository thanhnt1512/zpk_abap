*&---------------------------------------------------------------------*
*& Report ZPG_TEST_READ_EXCEL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_read_excel.
DATA gs_date(10).
*TRY.
*DATA:
*  lt_excel TYPE kcde_intern,
*  ls_excel TYPE kcde_intern_struc,
*  lv_error TYPE string.
*
*CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
*  EXPORTING
*    filename    = 'C:\Users\Admin\Downloads\Telegram Desktop\upload.xlsx'
*    i_begin_col = 1
*    i_begin_row = 1
*    i_end_col   = 4
*    i_end_row   = 9999
*  TABLES
*    intern      = lt_excel.
** EXCEPTIONS
**   INCONSISTENT_PARAMETERS       = 1
**   UPLOAD_OLE  = 2
**   OTHERS      = 3
*  .
*IF sy-subrc <> 0.
** Implement suitable error handling here
*ENDIF.
DATA pt_data TYPE TABLE OF zst_tool_excel.
CALL FUNCTION 'UPLOAD_XLS_FILE_2_ITAB'
  EXPORTING
    i_filename = 'C:\Users\Admin\Downloads\Telegram Desktop\upload.xlsx'
  TABLES
    e_itab     = pt_data
  EXCEPTIONS
    file_error = 1
    OTHERS     = 2.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.
