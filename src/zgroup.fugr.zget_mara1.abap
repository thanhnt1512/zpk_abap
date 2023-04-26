FUNCTION ZGET_MARA1.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(MATNR) TYPE  MATNR
*"  TABLES
*"      MARA_TAB STRUCTURE  MARA
*"--------------------------------------------------------------------
select * from mara WHERE matnr = @MATNR INTO CORRESPONDING FIELDS OF TABLE @MARA_TAB .
DATA :test1 type string,
      flag1 TYPE char1.
*IMPORT test1 FROM MEMORY id 'hi'.
IMPORT flag to flag1 from DATABASE indx(zf) id 'ZFII'.
DELETE FROM DATABASE indx(zf) id 'ZFII'.
ENDFUNCTION.
