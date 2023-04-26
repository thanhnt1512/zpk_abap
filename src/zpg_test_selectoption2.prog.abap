*&---------------------------------------------------------------------*
*& Report ZPG_TEST_SELECTOPTION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_selectoption2.
*DATA :name TYPE RANGE OF but000-name.
*DATA input_name TYPE string.
*input_name = 'Nguyễn Thanh Bình'.
*REPLACE ALL OCCURRENCES OF '*' IN input_name WITH '%'.
*SELECT partner,name1_text FROM but000 INTO TABLE @DATA(lt_name) WHERE
*  name1_text LIKE @input_name.
*
**IF lt_name IS NOT INITIAL.
**  LOOP AT  lt_name  INTO DATA(ls_name).
**    APPEND INITIAL LINE TO name ASSIGNING <fs_name>.
**    <fs_name>-sign   = 'I'.   "I = include, E = exclude
**    <fs_name>-option = 'EQ'.  "EQ, BT, NE ....
**    <fs_name>-low    = ls_name-name1_text.
**  ENDLOOP.
**ENDIF.
**
**SELECT partner,name1_text FROM
*WRITE 'dd'.
DATA : name_input TYPE string,
       name_compare TYPE string,
     expression type string.
name_input = '*ty trách*'.
name_compare = 'Công ty trách nghiệm'.
REPLACE ALL OCCURRENCES OF '*' IN name_input WITH '.*'.
CONCATENATE '^' name_input '$' INTO expression.
FIND REGEX expression IN name_compare.
IF sy-subrc = 0.
  WRITE :'true'.
ENDIF.
