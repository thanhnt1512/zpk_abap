**&---------------------------------------------------------------------*
**& Report ZPG_TEST_SELECT
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
REPORT zpg_test_select.
*DATA: ls_input  TYPE  zdt_zfii017l_bp_in,
*      lt_output TYPE  zdt_zfii017l_bp_out_bp_lis_tab,
*      ls_output LIKE LINE OF lt_output.
*
*TYPES :BEGIN OF lty_but000,
*         partner TYPE bu_partner,
*         name1_text TYPE but000-name1_text,
*         bpkind TYPE but000-bpkind,
*       END OF lty_but000.
*
*TYPES :BEGIN OF lty_adrc,
*         partner    TYPE bu_partner,
*         addrnumber TYPE ad_addrnum,
*       END OF lty_adrc.
*
*TYPES :BEGIN OF lty_city,
*         partner    TYPE bu_partner,
*         addrnumber TYPE ad_addrnum,
*       END OF lty_city.
*DATA: lt_city TYPE TABLE OF lty_city.
*
*TYPES :BEGIN OF lty_regiogroup,
*         partner    TYPE bu_partner,
*         addrnumber TYPE ad_addrnum,
*       END OF lty_regiogroup.
*DATA: lt_regiogroup TYPE TABLE OF lty_regiogroup.
*
*TYPES :BEGIN OF lty_streetcode,
*         partner    TYPE bu_partner,
*         addrnumber TYPE ad_addrnum,
*       END OF lty_streetcode.
*DATA: lt_streetcode TYPE TABLE OF lty_streetcode.
*
*TYPES :BEGIN OF lty_cityp,
*         partner    TYPE bu_partner,
*         addrnumber TYPE ad_addrnum,
*       END OF lty_cityp.
*DATA: lt_cityp TYPE TABLE OF lty_cityp.
*
*TYPES :BEGIN OF lty_adrc1,
*         str_suppl2 TYPE adrc-str_suppl2,
*         street     TYPE adrc-street,
*         floor      TYPE adrc-floor,
*         addrnumber TYPE ad_addrnum,
*       END OF lty_adrc1.
*
*TYPES :BEGIN OF lty_adrccity,
*         city_code  TYPE adrc-city_code,
*         addrnumber TYPE ad_addrnum,
*       END OF lty_adrccity.
*
*TYPES :BEGIN OF lty_adrcregiogroup,
*         regiogroup TYPE adrc-regiogroup,
*         addrnumber TYPE ad_addrnum,
*       END OF lty_adrcregiogroup.
*DATA:lt_adrcregiogroup TYPE TABLE OF lty_adrcregiogroup.
*
*TYPES :BEGIN OF lty_adrcstreetcode,
*         streetcode TYPE adrc-streetcode,
*         addrnumber TYPE ad_addrnum,
*       END OF lty_adrcstreetcode.
*DATA:lt_adrcstreetcode TYPE TABLE OF lty_adrcstreetcode.
*
*TYPES :BEGIN OF lty_adrccityp,
*         cityp_code TYPE adrc-city_code,
*         addrnumber TYPE ad_addrnum,
*       END OF lty_adrccityp.
*DATA: lt_adrccityp TYPE TABLE OF lty_adrccityp.
*
*TYPES :BEGIN OF lty_but021_fs,
*         partner    TYPE bu_partner,
*         addrnumber TYPE ad_addrnum,
*       END OF lty_but021_fs.
*
*TYPES :BEGIN OF lty_adr2,
*         addrnumber TYPE ad_addrnum,
*         tel_number TYPE adr2-tel_number,
*       END OF lty_adr2.
*
*TYPES :BEGIN OF lty_but0id,
*         partner TYPE bu_partner,
*       END OF lty_but0id.
*
*TYPES :BEGIN OF lty_but100,
*         role    TYPE but100-role,
*         partner TYPE bu_partner,
*       END OF lty_but100.
*
*TYPES: BEGIN OF lty_zbp,
*         partner2 TYPE but050-partner2,
*         partner1 TYPE but050-partner1,
*       END OF lty_zbp.
*DATA: lt_zbp TYPE TABLE OF lty_zbp.
*
*DATA : lt_but000    TYPE TABLE OF lty_but000,
*       ls_but000    LIKE LINE OF lt_but000,
*       lt_adrc      TYPE TABLE OF lty_adrc,
*       lt_adrc1     TYPE TABLE OF lty_adrc1,
*       lt_adrccity  TYPE TABLE OF lty_adrccity,
*       lt_but021_fs TYPE TABLE OF lty_but021_fs,
*       ls_but021_fs LIKE LINE OF lt_but021_fs,
*       lt_adr2      TYPE TABLE OF lty_adr2,
*       lt_but0id    TYPE TABLE OF lty_but0id,
*       lt_but100    TYPE TABLE OF lty_but100.
*DATA : input_fullname TYPE string ,
*       input_bpkind TYPE string.
*
*
*
*
*input_fullname = 'Nguyễn Thanh Bình' .
*input_bpkind = 'T1'.
*data: fullname type range of but000-name1_text,  "range table
*      wa_name like line of fullname.     "work area for range table
*
*wa_name-sign   = 'I'.   "I = include, E = exclude
*wa_name-option = 'EQ'.  "EQ, BT, NE ....
*wa_name-low    = input_fullname.
**wa_name-high   = input_fullname.  "not needed unless using the BT option
*append wa_name to fullname.
**clear wa_name.
**wa_name-sign   = 'I'.   "I = include, E = exclude
**wa_name-option = 'EQ'.  "EQ, BT, NE ....
**wa_name-low    = input_fullname.
***wa_name-high   =   "not needed unless using the BT option
**append wa_name to fullname.
*CONCATENATE '%' input_fullname '%' INTO input_fullname.
*
*SELECT partner name1_text bpkind FROM but000 INTO CORRESPONDING FIELDS OF TABLE lt_but000
*  WHERE name1_text in fullname.
**SELECT partner name1_text bpkind FROM but000 INTO CORRESPONDING FIELDS OF TABLE lt_but000
**  WHERE name1_text like input_fullname and  bpkind = input_bpkind .
**if input_fullname is INITIAL.
**  DELETE lt_but000 WHERE name1_text = ''.
**ENDIF.
**if input_bpkind is INITIAL.
**  DELETE lt_but000 WHERE name1_text = ''.
**ENDIF.
*
**
**select  partner addrnumber FROM but021_fs INTO CORRESPONDING FIELDS OF TABLE lt_but021_fs
**  FOR ALL ENTRIES IN lt_but000
**  WHERE partner = lt_but000-partner.
*cl_demo_output=>display_data( lt_but000[] ).
DATA : partner_number    TYPE bu_partner,
       full_name         TYPE bu_name1tx,
*        FULL_ADDRESS
       telephone         TYPE ad_tlnmbr,
       ident_number      TYPE bu_id_number,
       category          TYPE bu_bpkind,
       city_code         TYPE ad_citynum,
       district_code     TYPE ad_citypnm,
       ward_code         TYPE regiogroup,
       street_code       TYPE ad_strnum,
       bp_role           TYPE bp_role,
       sales_org         TYPE vkorg,
       sales_office      TYPE vkbur,
       po_reconciliation TYPE bu_partner,
       po_cod            TYPE bu_partner,
       ecommerce_bp      TYPE bu_partner.
DATA :lt_search_result_companies TYPE TABLE OF addr1_val,
      lt_search_result_persons   TYPE TABLE OF addr2_val.


DATA:
  lw_telephone                TYPE  bapibus1006_comm-telephone,
  lw_email                    TYPE  bapibus1006_comm-e_mail,
  lw_url                      TYPE  bapibus1006_comm-url,
  ls_addressdata              TYPE  bapibus1006_addr_search,
  ls_centraldata              TYPE  bapibus1006_central_search,
  lw_all_businesspartnerroles TYPE  bapibus1006_x-mark,
  lw_valid_date               TYPE  bapi_bupa_valid_date,
  lw_iv_req_mask              TYPE  bapibus1006_mask-iv_req_mask,
  lw_error                    TYPE  bapibus1006_mask-iv_req_mask.
*
*lw_telephone = TELEPHONE.
*lw_email = 'lampv@viettel.com.vn'.

full_name = '' .
category = ''.
TYPES :BEGIN OF lty_search,
         name_search     TYPE RANGE OF but000-name1_text,
         category_search TYPE RANGE OF but000-bpkind,
       END OF lty_search.
DATA :ls_search TYPE lty_search.

IF full_name IS NOT INITIAL.
  CONCATENATE '%' full_name '%' INTO full_name.
  SELECT name1_text FROM but000 INTO TABLE @DATA(lt_name)
                   WHERE name1_text LIKE @full_name.
  IF lt_name IS NOT INITIAL.
    LOOP AT lt_name INTO DATA(ls_name).
      APPEND INITIAL LINE TO ls_search-name_search ASSIGNING FIELD-SYMBOL(<fs_name_search>).
      <fs_name_search>-sign   = 'I'.   "I = include, E = exclude
      <fs_name_search>-option = 'EQ'.  "EQ, BT, NE ....
      <fs_name_search>-low    = ls_name-name1_text.
      UNASSIGN <fs_name_search>.
    ENDLOOP.
  ENDIF.

  APPEND INITIAL LINE TO ls_search-name_search ASSIGNING <fs_name_search>.
  <fs_name_search>-sign   = 'I'.   "I = include, E = exclude
  <fs_name_search>-option = 'EQ'.  "EQ, BT, NE ....
  <fs_name_search>-low    = full_name.
ENDIF.
IF ls_search-name_search IS NOT INITIAL.
  FREE ls_search-name_search.
ENDIF.

IF category IS NOT INITIAL.
  APPEND INITIAL LINE TO ls_search-category_search ASSIGNING FIELD-SYMBOL(<fs_category_search>).
  <fs_category_search>-sign   = 'I'.   "I = include, E = exclude
  <fs_category_search>-option = 'EQ'.  "EQ, BT, NE ....
  <fs_category_search>-low    = category.
ENDIF.
IF ls_search-category_search IS NOT INITIAL.
  FREE ls_search-category_search.
ENDIF.
*IF category IS NOT INITIAL.
*  wa_search-sign   = 'I'.   "I = include, E = exclude
*  wa_search-option = 'EQ'.  "EQ, BT, NE ....
*  wa_search-low    = category.
*  APPEND wa_search TO data_search.
*ENDIF.
**wa_name-high   = input_fullname.  "not needed unless using the BT option
IF NOT ( ls_search-name_search IS INITIAL AND ls_search-category_search IS INITIAL ).
  WRITE :'d'.
ELSE.
  SELECT * FROM but000 INTO TABLE @DATA(lt_but000)
    WHERE name1_text IN @ls_search-name_search AND bpkind IN @ls_search-category_search.
ENDIF.

*  WHERE

WRITE :'d'.
