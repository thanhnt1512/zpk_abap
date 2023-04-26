*&---------------------------------------------------------------------*
*& Report ZPG_TEST_NAV_REUSE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_test_nav_reuse.TYPE-POOLS slis.
TABLES ekko.
SELECT-OPTIONS s_ebeln FOR ekko-ebeln.
* Declare the data internal table
TYPES: BEGIN OF ty_ekko,
         ebeln TYPE ekko-ebeln,
         bedat TYPE ekko-bedat,
         lifnr TYPE ekko-lifnr,
       END OF ty_ekko.
DATA it_ekko TYPE TABLE OF ty_ekko.
DATA it_ekpo TYPE TABLE OF ekpo.
* filling the data internal table
SELECT ebeln bedat lifnr FROM ekko INTO TABLE it_ekko WHERE ebeln IN
s_ebeln.
* Declare the field catalog
DATA: it_fcat TYPE slis_t_fieldcat_alv,
      wa_fcat LIKE LINE OF it_fcat.
* Filling the field catalog
wa_fcat-fieldname = 'EBELN'.
wa_fcat-col_pos = '1'.
wa_fcat-seltext_m = 'PUR.DOC'.
wa_fcat-emphasize = 'C310'.
APPEND wa_fcat TO it_fcat.
CLEAR wa_fcat.
wa_fcat-fieldname = 'BEDAT'.
wa_fcat-col_pos = '2'.
wa_fcat-seltext_m = 'DOC DATE'.
APPEND wa_fcat TO it_fcat.
CLEAR wa_fcat.
wa_fcat-fieldname = 'LIFNR'.
wa_fcat-col_pos = '3'.
wa_fcat-seltext_m = 'VENDOR'.
APPEND wa_fcat TO it_fcat.
CLEAR wa_fcat.
* Declare the event internal table
DATA: it_event TYPE slis_t_event,
      wa_event LIKE LINE OF it_event.
* Filling the event internal table
wa_event-name = 'USER_COMMAND'.
wa_event-form = 'UC'.
* Perform UC using sy-ucomm slis_selfield
APPEND wa_event TO it_event.

wa_event-name = 'TOP_OF_PAGE'.
wa_event-form = 'UC1'.
* Perform UC using sy-ucomm slis_selfield
APPEND wa_event TO it_event.
* Display the output
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_callback_program = sy-cprog
    it_fieldcat        = it_fcat
    it_events          = it_event
  TABLES
    t_outtab           = it_ekko.
FORM uc USING a LIKE sy-ucomm b TYPE slis_selfield.
  IF b-fieldname = 'EBELN'.
    SELECT * FROM ekpo INTO TABLE it_ekpo WHERE ebeln = b-value.
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_structure_name = 'EKPO'
      TABLES
        t_outtab         = it_ekpo.
  ENDIF.
ENDFORM.
FORM uc1.
  WRITE 'hiii'.
ENDFORM.
