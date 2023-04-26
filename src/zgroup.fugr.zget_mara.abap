FUNCTION ZGET_MARA.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(MATNR) TYPE  MATNR
*"  TABLES
*"      MARA_TAB STRUCTURE  MARA
*"----------------------------------------------------------------------
select * from mara WHERE matnr = @MATNR INTO CORRESPONDING FIELDS OF TABLE @MARA_TAB .
*DATA :test type string.
*IMPORT test FROM MEMORY id 'hi'.
CALL FUNCTION 'ZGET_MARA1'
  EXPORTING
    matnr          = MATNR
  tables
    mara_tab       = MARA_TAB
          .

ENDFUNCTION.
