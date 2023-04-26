class ZCL_ACAC_S4_XLSX_PARSE_UTIL definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ys_structure.
  TYPES name        TYPE string.
  TYPES fieldname   TYPE dd_x031l_table .
  TYPES value       TYPE REF TO data .
  TYPES END OF ys_structure .
  types:
    yt_structure TYPE STANDARD TABLE OF ys_structure .
  types:
    BEGIN OF ys_name_value.
  TYPES name TYPE string.
  TYPES value TYPE ref to data.
  TYPES END OF ys_name_value .
  types:
    yt_name_value TYPE TABLE OF ys_name_value .

  methods CHECK_EXCEL_LAYOUT
    changing
      !CT_TABLE type FAC_T_EXCEL_DATA
    raising
      CX_ACAC_XLSX_EXCEPTION .
  methods TRANSFORM_XSTRING_2_TAB
    importing
      !IX_FILE type XSTRING
    exporting
      !ET_TABLE type FAC_T_EXCEL_DATA
    raising
      CX_OPENXML_NOT_FOUND
      CX_OPENXML_FORMAT .
  methods DESCRIBE_STRUCTURES
    changing
      !CT_STRUCTURE type ACAC_STRUC_FIELDNAME_PAIR_T .
  methods VALIDATE_FIELD_TYPE
    importing
      !IV_FIELD_VALUE type ANY
      !IV_FIELD_NAME type FIELDNAME
      !IV_ROW_INDEX type I
      !IT_FIELDINFO type DD_X031L_TABLE optional
    exporting
      !EV_LENGTH type I
    raising
      resumable(CX_ACAC_XLSX_EXCEPTION) .
protected section.
private section.

  types:
    BEGIN OF ooxml_worksheet.
  TYPES name        TYPE string.
  TYPES id          TYPE string.
  TYPES location    TYPE string.
  TYPES END OF ooxml_worksheet .
  types:
    ooxml_worksheets TYPE STANDARD TABLE OF ooxml_worksheet .
  types:
    BEGIN OF t_struc_numfmtid.
  TYPES id          TYPE i.
  TYPES formatcode  TYPE string.
  TYPES END OF t_struc_numfmtid .
  types:
    t_numfmtids TYPE STANDARD TABLE OF t_struc_numfmtid .
  types:
    t_dd04l TYPE STANDARD TABLE OF dd04l .
  types:
    t_dd03l TYPE STANDARD TABLE OF dd03l .

  data GT_FIELDINFO type DD_X031L_TABLE .
  data DATEFORMAT1904 type ABAP_BOOL .
  constants GC_ID_CHAR type C value 'C' ##NO_TEXT.
  constants GC_ID_NUMC type C value 'N' ##NO_TEXT.
  constants GC_ID_STRING type C value 'g' ##NO_TEXT.
  constants GC_ID_DATE type C value 'D' ##NO_TEXT.
  constants GC_ID_PNUM type C value 'P' ##NO_TEXT.
  constants GC_ID_INT type C value 'b' ##NO_TEXT.
  data GT_DD04L type T_DD04L .
  data GT_DD03L type T_DD03L .

  methods ADD_ADDITIONAL_FORMAT
    changing
      !CT_NUMFMTIDS type T_NUMFMTIDS .
  methods GET_ATTR_FROM_NODE
    importing
      !IV_NAME type STRING
      !IO_NODE type ref to IF_IXML_NODE
    returning
      value(RV_VALUE) type STRING .
  methods CONVERT_CELL_VALUE_BY_NUMFMT
    importing
      !IV_CELL_VALUE type STRING
      !IV_NUMBER_FORMAT type STRING
    returning
      value(EV_FORMATTED_VALUE) type STRING .
  methods CONVERT_SER_VAL_TO_DATE_TIME
    importing
      !IV_SERIAL_VALUE_STRING type STRING
    exporting
      !EV_DATE type D
      !EV_TIME type T .
  methods CONVERT_LONG_TO_DATE
    importing
      !IV_DATE_STRING type STRING
    returning
      value(RV_DATE) type D .
  methods CONVERT_DEC_TIME_TO_HHMMSS
    importing
      !IV_DEC_TIME_STRING type STRING
    returning
      value(RV_TIME) type T .
ENDCLASS.



CLASS ZCL_ACAC_S4_XLSX_PARSE_UTIL IMPLEMENTATION.


  METHOD ADD_ADDITIONAL_FORMAT.
    DATA ls_format TYPE t_struc_numfmtid.

    " Filling all OOXML predifended, language independend number ls_formats !Not a straight numbering!
    " According to 'Office Open XML Part 4 - Markup Language Reference, 3.8.30'
    ls_format-id = 0.
    ls_format-formatcode = `General`.                       "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 1.
    ls_format-formatcode = `0`.                             "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 2.
    ls_format-formatcode = `0.00`.                          "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 3.
    ls_format-formatcode = `#,##0`.                         "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 4.
    ls_format-formatcode = `#,##0.00`.                      "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 9.
    ls_format-formatcode = `0%`.                            "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 10.
    ls_format-formatcode = `0.00%`.                         "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 11.
    ls_format-formatcode = `0.00E+00`.                      "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 12.
    ls_format-formatcode = `# ?/?`.                         "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 13.
    ls_format-formatcode = `# ??/??`.                       "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 14.
    ls_format-formatcode = `mm-dd-yy`.                      "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 15.
    ls_format-formatcode = `d-mmm-yy`.                      "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 16.
    ls_format-formatcode = `d-mmm`.                         "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 17.
    ls_format-formatcode = `mmm-yy`.                        "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 18.
    ls_format-formatcode = `h:mm AM/PM`.                    "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 19.
    ls_format-formatcode = `h:mm:ss AM/PM`.                 "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 20.
    ls_format-formatcode = `h:mm`.                          "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 21.
    ls_format-formatcode = `h:mm:ss`.                       "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 22.
    ls_format-formatcode = `m/d/yy h:mm`.                   "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 37.
    ls_format-formatcode = `#,##0;(#,##0)`.                 "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 38.
    ls_format-formatcode = `#,##0;[Red](#,##0)`.            "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 39.
    ls_format-formatcode = `#,##0.00;(#,##0.00)`.           "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 40.
    ls_format-formatcode = `#,##0.00;[Red](#,##0.00)`.      "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 45.
    ls_format-formatcode = `mm:ss`.                         "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 46.
    ls_format-formatcode = `[h]:mm:ss`.                     "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 47.
    ls_format-formatcode = `mmss.0`.                        "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 48.
    ls_format-formatcode = `##0.0E+0`.                      "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
    ls_format-id = 49.
    ls_format-formatcode = `@`.                             "#EC NOTEXT
    APPEND ls_format TO ct_numfmtids.
  ENDMETHOD.


  METHOD CHECK_EXCEL_LAYOUT.

    DATA: lt_excel_row_field TYPE fac_t_excel_data,
          lt_excel_row_label TYPE fac_t_excel_data.

    " Check the excel file layout, raise the exception once layout is incorrect
    " 1. Label and Technical Name must be matched each other
    " 2. ...
    DESCRIBE TABLE ct_table LINES DATA(lv_count).
    " the file includes two lines at least
    IF lv_count < 2.
      RAISE EXCEPTION TYPE cx_acac_xlsx_exception
        MESSAGE ID 'ACAC_EXCEL'
        TYPE 'E'
        NUMBER '022'.
    ENDIF.

    " // is comment symbol, if statement have prefix '//', delete it.
    LOOP AT ct_table INTO DATA(ls_table).
      DATA(lt_remark) = CAST fac_t_excel_data( ls_table-value )->*.
      DATA(ls_remark) = CAST string( lt_remark[ 1 ]-value )->* .
      IF ls_remark IS NOT INITIAL
        AND strlen( ls_remark ) > 2
        AND ls_remark+0(2) EQ cl_acac_s4_xlsx_template=>gc_comment_prefix.
        DELETE ct_table INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    " the number of fields must equal to the number of labels
    " label row
    lt_excel_row_label = CAST fac_t_excel_data( ct_table[ 1 ]-value )->*.
    " field row
    lt_excel_row_field = CAST fac_t_excel_data( ct_table[ 2 ]-value )->*.

    IF lines( lt_excel_row_label ) NE lines( lt_excel_row_field ).
      RAISE EXCEPTION TYPE cx_acac_xlsx_exception
        MESSAGE ID 'ACAC_EXCEL'
        TYPE 'E'
        NUMBER '022'.
    ENDIF.

    " delete the label row
    DELETE ct_table INDEX 1.

  ENDMETHOD.


  METHOD CONVERT_CELL_VALUE_BY_NUMFMT.
    DATA: lv_clean_format TYPE string,
          lv_date         TYPE d.
*          lv_time         TYPE t,
*          lv_hour(2)      TYPE c,
*          lv_min(2)       TYPE c,
*          lv_sec(2)       TYPE c.

    ev_formatted_value = ''.
    lv_clean_format = iv_number_format.
    REPLACE REGEX '"[^"]*"' IN lv_clean_format WITH ''.
    REPLACE REGEX '\[Red\]' IN lv_clean_format WITH ''.

    IF cl_abap_matcher=>matches( pattern = '.*(y+|m+|d+|h+|s+).*' text = lv_clean_format ) = abap_true.

      convert_ser_val_to_date_time(
        EXPORTING
          iv_serial_value_string = iv_cell_value
        IMPORTING
          ev_date                = lv_date ).

      ev_formatted_value = lv_date.
    ENDIF.

    IF ev_formatted_value = ''.
      ev_formatted_value = iv_cell_value.
    ENDIF.
  ENDMETHOD.


  METHOD CONVERT_DEC_TIME_TO_HHMMSS.
    DATA: lv_dec_time   TYPE decfloat16,
          lv_hour       TYPE i,
          lv_hour_str   TYPE string,
          lv_minute     TYPE i,
          lv_minute_str TYPE string,
          lv_second     TYPE decfloat16.

    TRY.
        lv_dec_time = iv_dec_time_string.
      CATCH cx_root.
        " Cannot convert string to dec float... leaving undone
        rv_time = iv_dec_time_string.
        EXIT.
    ENDTRY.

    lv_dec_time = frac( lv_dec_time ). " Make sure that only the fraction is considered

    " Thanks to Excel, we have to round at this point to be compliant
    lv_dec_time = round( val = lv_dec_time dec = 15 ).

    lv_dec_time = lv_dec_time * 24.
    lv_hour = floor( lv_dec_time ).
    lv_dec_time = ( lv_dec_time - lv_hour ) * 60.
    lv_minute = floor( lv_dec_time ).
    lv_second = round( val = ( ( lv_dec_time - lv_minute ) * 60 ) dec = 3 ).
    IF lv_second >= 60.
      lv_second = 0.
      lv_minute = lv_minute + 1.
    ENDIF.

    IF lv_hour < 10.
      lv_hour_str = '0' && lv_hour.
    ELSE.
      lv_hour_str = lv_hour.
    ENDIF.
    cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = lv_hour_str ).

    IF lv_minute < 10.
      lv_minute_str = '0' && lv_minute.
    ELSE.
      lv_minute_str = lv_minute.
    ENDIF.
    cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = lv_minute_str ).

    rv_time = lv_hour_str && lv_minute_str &&  lv_second .
  ENDMETHOD.


  METHOD CONVERT_LONG_TO_DATE.
    DATA lv_num_days TYPE i.

    lv_num_days = floor( iv_date_string ).

    IF me->dateformat1904 = abap_false.
      " 1900 based
      rv_date = '18991231'.
      IF iv_date_string > 59.
        " Microsoft thinks the year 1900 is a leap year... it is not!
        rv_date = rv_date + lv_num_days - 1.
      ELSE.
        " From 1899-12-31 to 1900-02-28 Microsoft guesses the correct rv_date
        rv_date = rv_date + lv_num_days.
      ENDIF.
      " 1904 based
    ELSE.
      rv_date = '19040101'.
      rv_date = rv_date + lv_num_days.
    ENDIF.
  ENDMETHOD.


  METHOD CONVERT_SER_VAL_TO_DATE_TIME.
    DATA:
      lv_date_str  TYPE string,
      lv_time_str  TYPE string,
      lv_date_time TYPE decfloat34.

    TRY.
        lv_date_time = iv_serial_value_string.
      CATCH cx_root.
        "Not able to interpret as dec value
        EXIT.
    ENDTRY.
    lv_date_str = floor( lv_date_time ).
    IF lv_date_str NE '0'.
      ev_date = convert_long_to_date( lv_date_str ).
    ENDIF.

    lv_time_str = frac( lv_date_time ).
    IF lv_time_str NE '0'.
      ev_time = convert_dec_time_to_hhmmss( lv_time_str ).
    ENDIF.
  ENDMETHOD.


METHOD DESCRIBE_STRUCTURES.
  DATA: lt_fieldinfo       TYPE dd_x031l_table,
        lo_type_desc       TYPE REF TO cl_abap_typedescr.
  LOOP AT ct_structure ASSIGNING FIELD-SYMBOL(<fs_structure>).
    lo_type_desc = cl_abap_structdescr=>describe_by_name( <fs_structure>-name ).
    lt_fieldinfo = lo_type_desc->get_ddic_object( ).
    <fs_structure>-fieldname = lt_fieldinfo.
  ENDLOOP.
ENDMETHOD.


  METHOD GET_ATTR_FROM_NODE.
    DATA:
      lo_attrib_map TYPE REF TO if_ixml_named_node_map,
      lo_attribute  TYPE REF TO if_ixml_attribute,
      lo_attr_node  TYPE REF TO if_ixml_node.

    TYPE-POOLS ixml.

    CHECK io_node IS NOT INITIAL.

    lo_attrib_map = io_node->get_attributes( ).
    CHECK lo_attrib_map IS NOT INITIAL.

    lo_attr_node = lo_attrib_map->get_named_item( iv_name ).
    CHECK lo_attr_node IS NOT INITIAL.

    lo_attribute ?= lo_attr_node->query_interface( ixml_iid_attribute ).
    rv_value = lo_attribute->get_value( ).
  ENDMETHOD.


  METHOD TRANSFORM_XSTRING_2_TAB.
    DATA:
      lo_xlsx_doc        TYPE REF TO cl_xlsx_document,
      lo_workbookpart    TYPE REF TO cl_xlsx_workbookpart,
      lx_workbook        TYPE xstring,
      lt_worksheets      TYPE ooxml_worksheets,
      lo_worksheetpart   TYPE REF TO cl_xlsx_sharedstringspart,
      lx_file            TYPE xstring,
      lt_shared_string   TYPE TABLE OF string,
      lx_sheet           TYPE xstring,
      lv_dim             TYPE string,
      lo_worksheet       TYPE REF TO cl_openxml_part,
      lo_ixml            TYPE REF TO if_ixml,
      lo_streamfactory   TYPE REF TO if_ixml_stream_factory,
      lo_parser          TYPE REF TO if_ixml_parser,
      lo_istream         TYPE REF TO if_ixml_istream,
      lo_node            TYPE REF TO if_ixml_node,
      lo_xml_document    TYPE REF TO if_ixml_document,
      lo_row             TYPE REF TO if_ixml_node,
      lo_rows            TYPE REF TO if_ixml_node_collection,
      lo_row_iterator    TYPE REF TO if_ixml_node_iterator,
      lv_row_index       TYPE string,
      lo_row_element     TYPE REF TO if_ixml_element,
      lo_col             TYPE REF TO if_ixml_node,
      lo_cols            TYPE REF TO if_ixml_node_collection,
      lo_col_iterator    TYPE REF TO if_ixml_node_iterator,
      lv_col_index       TYPE string,
      lo_cell_element    TYPE REF TO if_ixml_element,
      lo_col_element     TYPE REF TO if_ixml_element,
      lv_cell_value      TYPE string,
      lv_cell_index      TYPE string,
      lo_xlsx_stylesheet TYPE REF TO cl_xlsx_stylespart,
      lx_styles          TYPE xstring,
      lt_cellxfs         TYPE STANDARD TABLE OF string,
      lv_style_id        TYPE string,
      lt_numfmtids       TYPE t_numfmtids,
      ls_numfmtids       LIKE LINE OF lt_numfmtids,
      ls_df1904          TYPE string,
      lt_fieldinfo       LIKE gt_fieldinfo,
      lr_type_desc       TYPE REF TO cl_abap_typedescr,
      ls_line            TYPE fac_s_excel_data,
      ls_cell            TYPE fac_s_excel_data.

    FIELD-SYMBOLS: <fs_celldata> TYPE any,
                   <ft_linedata> TYPE fac_t_excel_data.

    IF ix_file IS INITIAL.
      RAISE EXCEPTION TYPE cx_openxml_format
        EXPORTING
          textid = cx_openxml_format=>cx_openxml_empty.
    ENDIF.

    "===== get worksheet =====
    lo_xlsx_doc = cl_xlsx_document=>load_document( ix_file ).
    lo_workbookpart = lo_xlsx_doc->get_workbookpart( ).
    lx_workbook =  lo_workbookpart->get_data( ).
    CALL TRANSFORMATION acac_xl_get_worksheets
          SOURCE XML lx_workbook
          RESULT worksheets = lt_worksheets.
    IF lt_worksheets IS INITIAL.
      RAISE EXCEPTION TYPE cx_openxml_format
        EXPORTING
          textid = cx_openxml_format=>cx_openxml_empty.
    ENDIF.

    "===== get worksheet data =====
    lo_worksheetpart = lo_workbookpart->get_sharedstringspart( ).
    lx_file = lo_worksheetpart->get_data( ).
    CALL TRANSFORMATION acac_xl_get_shared_strings
            SOURCE XML lx_file
            RESULT shared_strings = lt_shared_string.

    "===== get style and format =====
    lo_xlsx_stylesheet = lo_workbookpart->get_stylespart( ).
    lx_styles = lo_xlsx_stylesheet->get_data( ).
    CALL TRANSFORMATION acac_xl_get_cellxfs
        SOURCE XML lx_styles
        RESULT numfmids = lt_cellxfs.
    CALL TRANSFORMATION acac_xl_get_numfmtids
        SOURCE XML lx_styles
        RESULT numfmts = lt_numfmtids.
    me->add_additional_format( CHANGING ct_numfmtids = lt_numfmtids ).
    CALL TRANSFORMATION acac_xl_get_date_format
        SOURCE XML lx_workbook
        RESULT dateformat_1904 = ls_df1904.
    IF ls_df1904 = '1'.
      me->dateformat1904 = abap_true.
    ELSE.
      me->dateformat1904 = abap_false.
    ENDIF.

    "===== get shared strings =====
    lo_worksheet = lo_workbookpart->get_part_by_id( lt_worksheets[ 1 ]-location ).
    lx_sheet = lo_worksheet->get_data( ).
    CALL TRANSFORMATION acac_xl_get_sheet_dimension
      SOURCE XML lx_sheet
      RESULT dimension = lv_dim.

    "===== parse document to xml =====
    lo_ixml   = cl_ixml=>create( ).
    lo_streamfactory = lo_ixml->create_stream_factory( ).
    lo_istream = lo_streamfactory->create_istream_xstring( lx_sheet ).
    lo_xml_document = lo_ixml->create_document( ).
    lo_parser = lo_ixml->create_parser( stream_factory = lo_streamfactory
                                        istream        = lo_istream
                                        document       = lo_xml_document ).
    lo_parser->parse( ).

    "========== Process Begin ==========
    "===== get rows from xml =====
    lo_rows = lo_xml_document->get_elements_by_tag_name_ns( name = 'row'  uri = if_acac_xl_types=>ns_ooxml_ssheet_main ).  "get all data rows
    lo_row_iterator = lo_rows->create_iterator( ).
    lo_row = lo_row_iterator->get_next( ).

    "===== loop rows and process =====
    WHILE lo_row IS NOT INITIAL.
      " 1. get line number
      lv_row_index = me->get_attr_from_node( iv_name  = 'r' io_node  = lo_row ).  " get row index

      " 2. create line data.
      ls_line-index = lv_row_index.
      CREATE DATA ls_line-value TYPE fac_t_excel_data.
      ASSIGN ls_line-value->* TO <ft_linedata>.

      " 3. loop cols from rol =====
      lo_row_element ?= lo_row->query_interface( ixml_iid_element ).
      lo_cols = lo_row_element->get_elements_by_tag_name_ns( name = 'c' uri = if_acac_xl_types=>ns_ooxml_ssheet_main ).  " get all columns
      IF lo_cols IS INITIAL.
        "step to next col
        lo_col = lo_col_iterator->get_next( ).
        CONTINUE.
      ENDIF.
      lo_col_iterator = lo_cols->create_iterator( ).
      lo_col = lo_col_iterator->get_next( ).

      "===== loop cols =====
      WHILE lo_col IS NOT INITIAL.
        lo_col_element ?= lo_col->query_interface( ixml_iid_element ).
        lo_cell_element = lo_col_element->find_from_name_ns( name = 'v'  uri = if_acac_xl_types=>ns_ooxml_ssheet_main ). "get cell from colums
        IF lo_cell_element IS INITIAL.
          "step to next col
          lo_col = lo_col_iterator->get_next( ).
          CONTINUE.
        ENDIF.

        " 1. get cell value
        lv_cell_value = lo_cell_element->get_value( ).
        IF get_attr_from_node( iv_name = 't' io_node = lo_col ) = 's'.  " get cell value's data type, 's' means string
          READ TABLE lt_shared_string INDEX ( lv_cell_value + 1 ) INTO lv_cell_value.
        ENDIF.
        " transform the date format
        READ TABLE lt_cellxfs INTO lv_style_id INDEX ( get_attr_from_node( iv_name = 's' io_node = lo_col ) + 1 ).
        READ TABLE lt_numfmtids INTO ls_numfmtids WITH KEY id = lv_style_id.
        lv_cell_value = convert_cell_value_by_numfmt( iv_cell_value = lv_cell_value iv_number_format = ls_numfmtids-formatcode ).

        REPLACE ALL OCCURRENCES OF REGEX '^\s*|\s*$' IN lv_cell_value WITH ''.

        IF lv_cell_value IS INITIAL.
          "step to next col
          lo_col = lo_col_iterator->get_next( ).
          CONTINUE.
        ENDIF.

        " 2. get cell index and col index
        lv_cell_index = me->get_attr_from_node( iv_name  = 'r' io_node  = lo_col ).
        DATA(lv_length) = strlen( lv_cell_index ) - strlen( lv_row_index ).
        lv_col_index = lv_cell_index(lv_length).

        " 3. set cell index and data
        ls_cell-index = lv_col_index.
        CREATE DATA ls_cell-value TYPE string.
        ASSIGN ls_cell-value->* TO <fs_celldata>.
        <fs_celldata> = lv_cell_value.

        " 4. add cell into line
        INSERT ls_cell INTO TABLE <ft_linedata>.

        " 5. step to next col
        lo_col = lo_col_iterator->get_next( ).
      ENDWHILE.
      "===== loop cols end =====

      " 4. add line to table
      IF <ft_linedata> IS NOT INITIAL.
        INSERT ls_line INTO TABLE et_table.
*        INSERT ls_line INTO TABLE mt_excel.
      ENDIF.

      " 5. step to next row
      lo_row = lo_row_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.


METHOD validate_field_type.
  DATA: ls_fieldinfo   TYPE x031l,
        lv_field_type  TYPE inttype,
        lv_input       TYPE decfloat16,
        lv_output      TYPE decfloat16,
        lv_amt_dec_len TYPE i,
        lv_intlen      TYPE intlen,
        lv_date        TYPE sy-datum.

  READ TABLE it_fieldinfo WITH KEY fieldname = iv_field_name INTO ls_fieldinfo.
  IF ls_fieldinfo IS NOT INITIAL.
    lv_field_type = ls_fieldinfo-exid.
    lv_intlen = CONV intlen( ls_fieldinfo-exlength ).
  ENDIF.

  IF lv_field_type EQ gc_id_date.
    lv_date = iv_field_value.
    IF lv_date = 0.
      RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
        MESSAGE ID 'ACAC_EXCEL'
        TYPE 'E'
        NUMBER '000'
        WITH iv_row_index iv_field_name.
    ENDIF.
  ENDIF.

  "is field character-like?
  IF lv_field_type EQ gc_id_char OR lv_field_type EQ gc_id_numc OR lv_field_type EQ gc_id_string
  OR lv_field_type EQ gc_id_date OR lv_field_type EQ gc_id_int.

    "Check length
    IF strlen( iv_field_value ) GT lv_intlen.
      ev_length = lv_intlen.
      RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
        MESSAGE ID 'ACAC_EXCEL'
        TYPE 'E'
        NUMBER '001'
        WITH iv_row_index iv_field_name.
    ENDIF.

    "In case of numeric, check whether entry is numeric
    IF lv_field_type EQ gc_id_numc OR lv_field_type EQ gc_id_date.
      IF iv_field_value CN '0123456789'.
        RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
          MESSAGE ID 'ACAC_EXCEL'
          TYPE 'E'
          NUMBER '017'
          WITH iv_row_index iv_field_name.
      ENDIF.
    ENDIF.
  ENDIF.

  " deal with amount
  IF lv_field_type EQ gc_id_pnum.
    lv_amt_dec_len = CONV i( ls_fieldinfo-decimals ).

    TRY.
        lv_input = iv_field_value.
      CATCH cx_sy_conversion_no_number INTO DATA(ld_message).
        RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
          MESSAGE ID 'ACAC_EXCEL'
          TYPE 'E'
          NUMBER '019'
          WITH iv_row_index iv_field_name.
    ENDTRY.

    CALL FUNCTION 'ROUND'
      EXPORTING
        decimals      = lv_amt_dec_len
        input         = lv_input
        sign          = '+'
      IMPORTING
        output        = lv_output
      EXCEPTIONS
        input_invalid = 1
        overflow      = 2
        type_invalid  = 3
        OTHERS        = 4.

    IF sy-subrc NE 0.
      RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
        MESSAGE ID 'ACAC_EXCEL'
        TYPE 'E'
        NUMBER '019'
        WITH iv_row_index iv_field_name.
    ENDIF.

  ENDIF.

  "add thanhnt
  CHECK iv_field_name = 'ACAC_RESPERSON'.

  DATA :lw_belnr TYPE bkpf-belnr,
        lw_year  TYPE bkpf-gjahr.
*        lw_belnr_str TYPE string,
*        lw_year_str  TYPE string.


  lw_belnr = substring( val = iv_field_value off = 0 len = strlen( iv_field_value ) - 2 ).
  lw_year = |20{ substring( val = iv_field_value off = strlen( iv_field_value ) - 2 len =  2 ) }|.

  SELECT SINGLE belnr FROM bkpf WHERE glvor = 'RFBU' AND xreversed <> 'X'
  AND belnr = @lw_belnr AND gjahr = @lw_year
  INTO @DATA(ls_belnr).

  IF sy-subrc NE 0.
    RAISE RESUMABLE EXCEPTION TYPE cx_acac_xlsx_exception
      MESSAGE ID 'ACAC_EXCEL'
      TYPE 'E'
      NUMBER '036'
      WITH lw_belnr.
  ENDIF.


ENDMETHOD.
ENDCLASS.
