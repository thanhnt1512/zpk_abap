*&---------------------------------------------------------------------*
*& Report ZPG_TEST_POPUP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_TEST_POPUP.
*DATA :return TYPE CHAR1.
*DATA: wa_fields LIKE sval,
*      li_fields LIKE sval OCCURS 0 WITH HEADER LINE.
*
*MOVE 'KONA' TO wa_fields-tabname.
*MOVE 'KNUMA' TO wa_fields-fieldname.
*APPEND wa_fields TO li_fields.
*
*MOVE 'BSEG' TO wa_fields-tabname.
*MOVE 'BUKRS' TO wa_fields-fieldname.
*APPEND wa_fields TO li_fields. " add more fields if required
*
*
*CALL FUNCTION 'POPUP_GET_VALUES'
*  EXPORTING
**   NO_VALUE_CHECK        = ' '
*    popup_title  = 'Test title'
*    start_column = '5'
*    start_row    = '1'
*  IMPORTING
*    returncode   = return
*  TABLES
*    fields       = li_fields
**     EXCEPTIONS
**   ERROR_IN_FIELDS       = 1
**   OTHERS       = 2
*  .
*
*
*LOOP AT li_fields.
*  WRITE:/ li_fields-value.
*ENDLOOP.
*WRITE return
*.

TYPE-POOLS: rsds.

DATA: ls_field  TYPE rsdsfields,
      lt_field  TYPE STANDARD TABLE OF rsdsfields,
      l_selid   TYPE dynselid,
      l_repid   TYPE sy-repid,
      ls_pfkey  TYPE rsdspfkey,
      lt_trange TYPE rsds_trange.

START-OF-SELECTION.

  ls_field-tablename = 'T001'.
  ls_field-fieldname = 'BUKRS'.
*  ls_field-type = 'S'.
  APPEND ls_field TO lt_field.

  CALL FUNCTION 'FREE_SELECTIONS_INIT'
    EXPORTING
      kind         = 'F'
    IMPORTING
      selection_id = l_selid
    TABLES
      fields_tab   = lt_field[].

  ls_pfkey-program = sy-repid.

  CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
    EXPORTING
      selection_id    = l_selid
      title           = 'Select Company'
      as_window       = 'X'
      pfkey           = ls_pfkey
      tree_visible    = ''
    IMPORTING
      field_ranges    = lt_trange
    TABLES
      fields_tab      = lt_field[]
    EXCEPTIONS
      internal_error  = 1
      no_action       = 2
      selid_not_found = 3
      illegal_status  = 4.

  WRITE sy-subrc.
