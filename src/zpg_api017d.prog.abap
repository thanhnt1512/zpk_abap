  METHOD zii_si_zfii017d_bp_in1~si_zfii017d_bp_in1.

    TYPES : BEGIN OF lty_input,
              bpartner TYPE bu_partner,
            END OF lty_input.

    TYPES: BEGIN OF lty_instid_a,
             instid_a TYPE srgbtbrel-instid_a,
           END OF lty_instid_a.

    TYPES: BEGIN OF lty_instid_b,
             instid_a TYPE srgbtbrel-instid_a,
             reltype  TYPE srgbtbrel-reltype,
             instid_b TYPE srgbtbrel-instid_b,
           END OF lty_instid_b.

    DATA bpartner TYPE bapibus1006_head-bpartner.

    DATA : lt_instid_a       TYPE TABLE OF lty_instid_a,
           lt_instid_b       TYPE TABLE OF lty_instid_b,
           ls_instid_a       LIKE LINE OF  lt_instid_a,
           ls_document_id    TYPE sofolenti1-doc_id,
           ls_document_data  TYPE sofolenti1,
           lt_object_content TYPE TABLE OF solisti1.


    DATA : lt_output TYPE zdt_zfii017d_bp_out_bp_det_tab,
           ls_output LIKE LINE OF lt_output,
           lt_input  TYPE TABLE OF lty_input,
           ls_input  LIKE LINE OF lt_input,
           lv_count  TYPE char2.

    DATA :lt_but000         TYPE TABLE OF but000,
          lt_but0id         TYPE TABLE OF but0id,
          lt_but021_fs      TYPE TABLE OF but021_fs,
          lt_payment_period TYPE TABLE OF ztb_paymt_period,
          lt_kna1           TYPE TABLE OF kna1,
          lt_knvv           TYPE TABLE OF knvv,
          lt_knb1           TYPE TABLE OF knb1,
          lt_lfb1           TYPE TABLE OF lfb1,
          lt_bnka           TYPE TABLE OF bnka,
          lt_t052u          TYPE TABLE OF t052u,
          lt_t042z          TYPE TABLE OF t042z.

    DATA :addressdata             TYPE bapibus1006_address,
          businesspartnerroles    TYPE TABLE OF bapibus1006_bproles,
          relationships           TYPE TABLE OF bapibus1006_relations,
          bankdetails             TYPE TABLE OF bapibus1006_bankdetails,
          ls_bankdetail_out       TYPE zdt_zfii017d_bp_out_bankdetail,
          centraldataperson       TYPE bapibus1006_central_person,
          centraldataorganization TYPE bapibus1006_central_organ.

    DATA :date_default TYPE dats VALUE '99991231'.

    DATA: lt_dd07v  TYPE TABLE OF dd07v,
          lt_dd07v1 TYPE TABLE OF dd07v.

    "GET  ALL KEY BUSINESS PARTNER INPUT
    LOOP AT input-mt_zfii017d_bp_in-bp_detail INTO DATA(ls_input1).
      IF ls_input1-bpartner IS NOT INITIAL.
        ls_input-bpartner = ls_input1-bpartner.
        APPEND ls_input TO lt_input.
        CLEAR : ls_input , ls_input1.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' "Conversion exit ALPHA, external->internal
        EXPORTING
          input  = <fs_input>-bpartner
        IMPORTING
          output = <fs_input>-bpartner.
    ENDLOOP.

    IF lt_input IS INITIAL.
      output-mt_zfii017d_bp_out-ev_error = '00'.
      RETURN.
    ENDIF.
* Get Domain-Values from BU_TYPE
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname    = 'BU_TYPE'
      TABLES
        values_tab = lt_dd07v
      EXCEPTIONS
        OTHERS     = 1.
* Get Domain-Values from ZDD_KY_DS
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname    = 'ZDD_KY_DS'
      TABLES
        values_tab = lt_dd07v1
      EXCEPTIONS
        OTHERS     = 1.

    "GET DATA FROM TABLE BUT000
    SELECT  partner bu_group bpkind type  title name1_text
            name_org2 name_org3 name_org4
            FROM but000 INTO CORRESPONDING FIELDS OF TABLE lt_but000
            FOR ALL ENTRIES IN lt_input
            WHERE partner = lt_input-bpartner.
    SORT lt_but000 BY partner.

    "GET DATA FROM TABLE BUT0ID
    SELECT partner type idnumber FROM but0id INTO CORRESPONDING FIELDS OF TABLE lt_but0id
                                 FOR ALL ENTRIES IN lt_input
                                 WHERE partner = lt_input-bpartner.
    SORT lt_but0id BY partner type.

    "GET DATA FROM TABLE BUT021_FS
    SELECT partner addrnumber  FROM but021_fs INTO CORRESPONDING FIELDS OF TABLE lt_but021_fs
                               FOR ALL ENTRIES IN lt_input
                               WHERE partner = lt_input-bpartner.
    SORT lt_but021_fs BY partner.


    "GET DATA FROM TABLE BNKA
    SELECT banks bankl banka FROM bnka INTO CORRESPONDING FIELDS OF TABLE lt_bnka.
    SORT lt_bnka BY banks bankl.

    "GET DATA FROM TABLE ZTB_PAYMT_PERIOD
    SELECT * FROM ztb_paymt_period INTO TABLE lt_payment_period
             FOR ALL ENTRIES IN lt_input
             WHERE partner = lt_input-bpartner.
    SORT lt_payment_period BY partner.


    "GET DATA FROM TABLE KNA1
    SELECT kunnr kukla konzs katr1  FROM kna1 INTO CORRESPONDING FIELDS OF TABLE lt_kna1
                                    FOR ALL ENTRIES IN lt_input
                                    WHERE kunnr = lt_input-bpartner.
    SORT lt_kna1 BY kunnr.


    "GET DATA FROM TABLE KNVV
    SELECT kunnr vkorg FROM knvv INTO CORRESPONDING FIELDS OF TABLE lt_knvv
                       FOR ALL ENTRIES IN lt_input
                       WHERE kunnr = lt_input-bpartner.
    SORT lt_knvv BY kunnr.

    "GET DATA FROM TABLE KNB1
    SELECT kunnr akont zterm zwels FROM knb1  INTO CORRESPONDING FIELDS OF TABLE lt_knb1
                             FOR ALL ENTRIES IN lt_input
                             WHERE kunnr = lt_input-bpartner AND bukrs = '1000'.
    SORT lt_knb1 BY kunnr.

    "GET DATA FROM TABLE LFB1
    SELECT lifnr zterm zwels FROM lfb1 INTO CORRESPONDING FIELDS OF TABLE lt_lfb1
                             FOR ALL ENTRIES IN lt_input
                             WHERE lifnr = lt_input-bpartner AND bukrs = '1000'.
    SORT lt_lfb1 BY lifnr.

    "GET DATA FROM TABLE T052U
    SELECT zterm text1 FROM t052u INTO CORRESPONDING FIELDS OF TABLE lt_t052u
                       WHERE spras = sy-langu.
    SORT lt_t052u BY zterm.

    "GET DATA FROM TABLE T042Z
    SELECT zlsch text1 FROM t042z INTO CORRESPONDING FIELDS OF TABLE lt_t042z
                      WHERE land1 = 'VN'.
    SORT lt_t042z BY zlsch.

    "GET DATA FROM TB002
    SELECT txt15,bu_group FROM tb002 INTO TABLE @DATA(lt_group_name) WHERE spras = @sy-langu.
    SORT lt_group_name BY bu_group.

    "GET DATA FROM TB004T
    SELECT text40,bpkind FROM tb004t INTO TABLE @DATA(lt_category_desc) WHERE spras = @sy-langu.
    SORT lt_category_desc BY bpkind.

    "GET DATA FROM TSAD3T
    SELECT title,title_medi FROM tsad3t INTO TABLE @DATA(lt_title) WHERE langu = @sy-langu.
    SORT lt_title BY title.

    "GET DATA FROM TB039B
    SELECT type,text FROM tb039b INTO TABLE @DATA(lt_general_category_desc) WHERE spras = @sy-langu.
    SORT lt_general_category_desc BY type.

    "GET DATA FROM ADR6
    SELECT addrnumber,smtp_addr FROM adr6 INTO TABLE @DATA(lt_email).
    SORT lt_email BY addrnumber.

    "GET DATA FROM adr2
    SELECT addrnumber,tel_number FROM adr2 INTO TABLE @DATA(lt_telephone) WHERE r3_user = 1.
    SORT lt_telephone BY addrnumber.

    "GET DATA FROM t005t
    SELECT land1,landx FROM t005t INTO TABLE @DATA(lt_country_name) WHERE spras = @sy-langu.
    SORT lt_country_name BY land1.

    "GET DATA FROM adrcityt
    SELECT city_name, country, city_code FROM adrcityt INTO TABLE @DATA(lt_city_name) WHERE langu = @sy-langu.
    SORT lt_city_name BY country city_code.

    "GET DATA FROM adrcityprt
    SELECT city_part,city_code,cityp_code,country FROM adrcityprt INTO TABLE @DATA(lt_district_name).
    SORT lt_district_name BY city_code cityp_code country.

    "GET DATA FROM  adrreggrpt
    SELECT descript,regiogroup FROM adrreggrpt INTO TABLE @DATA(lt_ward_name) WHERE langu = @sy-langu.
    SORT lt_ward_name BY regiogroup.

    "GET DATA FROM TVKOT
    SELECT VKORG,VTEXT FROM TVKOT INTO TABLE @DATA(LT_ORG_DESC) WHERE SPRAS = @SY-langu.
    SORT LT_ORG_DESC BY VKORG.


    "GET DATA FROM TABLE SRGBTBREL
    LOOP AT lt_input INTO DATA(ls_instid_a1).
      ls_instid_a-instid_a = ls_instid_a1-bpartner.
      APPEND ls_instid_a TO lt_instid_a.
      CLEAR : ls_instid_a,ls_instid_a1.
    ENDLOOP.
    IF lt_instid_b[] IS NOT INITIAL.
      SELECT instid_a reltype instid_b FROM srgbtbrel INTO TABLE lt_instid_b
                                                      FOR ALL ENTRIES IN lt_instid_a
                                                                   WHERE instid_a = lt_instid_a-instid_a
                                                                     AND typeid_a = 'BUS1006'
                                                                     AND catid_a = 'BO'.
    ENDIF.

    LOOP AT lt_input INTO ls_input.
      bpartner = ls_input-bpartner.
      CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
        EXPORTING
          businesspartner = bpartner
        IMPORTING
          addressdata     = addressdata.

      CALL FUNCTION 'BAPI_BUPA_BANKDETAILS_GET'
        EXPORTING
          businesspartner = bpartner
        TABLES
          bankdetails     = bankdetails.

      CALL FUNCTION 'BAPI_BUPA_ROLES_GET_2'
        EXPORTING
          businesspartner      = bpartner
        TABLES
          businesspartnerroles = businesspartnerroles.

      CALL FUNCTION 'BUPA_RELATIONSHIPS_GET'
        EXPORTING
          iv_partner       = bpartner
        TABLES
          et_relationships = relationships.

      READ TABLE lt_but000 INTO DATA(ls_but000) WITH KEY partner = ls_input-bpartner BINARY SEARCH.
      IF ls_but000 IS NOT INITIAL.
        ls_output-partner_number = ls_but000-partner.
        ls_output-grouping =  ls_but000-bu_group.
        IF ls_output-grouping IS NOT INITIAL.
          READ TABLE lt_group_name INTO DATA(ls_grouping_name) WITH KEY bu_group = ls_output-grouping BINARY SEARCH.
          IF ls_grouping_name IS NOT INITIAL.
            ls_output-grouping_name = ls_grouping_name-txt15.
            CLEAR ls_grouping_name.
          ENDIF.
        ENDIF.

        ls_output-category = ls_but000-bpkind.
        READ TABLE lt_category_desc INTO DATA(ls_category_desc) WITH KEY bpkind = ls_but000-bpkind.
        IF ls_category_desc IS NOT INITIAL.
          ls_output-category_desc = ls_category_desc-text40.
          CLEAR ls_category_desc.
        ENDIF.

        ls_output-general_data-partnertype = ls_but000-type.
        IF ls_output-general_data-partnertype IS NOT INITIAL.
          READ TABLE lt_dd07v INTO DATA(ls_dd07v) WITH KEY domvalue_l = ls_output-general_data-partnertype BINARY SEARCH.
          ls_output-general_data-partnertype_desc = ls_dd07v-ddtext.
        ENDIF.

        ls_output-general_data-title_key = ls_but000-title.

        IF ls_output-general_data-title_key IS NOT INITIAL.
          READ TABLE lt_title INTO DATA(ls_title) WITH KEY title = ls_output-general_data-title_key BINARY SEARCH.
          IF ls_title IS NOT INITIAL.
            ls_output-general_data-title = ls_title-title_medi.
            CLEAR ls_title.
          ENDIF.
        ENDIF.

        IF ls_but000-type = '1'.
          ls_output-general_data-full_name = ls_but000-name1_text.
        ELSEIF ls_but000-type = '2'.
          CONCATENATE ls_but000-name_org2  ls_but000-name_org3  ls_but000-name_org4 INTO ls_output-general_data-full_name.
        ENDIF.

        CLEAR ls_but000.
      ENDIF.
      READ TABLE lt_but0id INTO DATA(ls_but0id) WITH KEY partner = ls_input-bpartner type = 'VATRU' BINARY SEARCH.
      IF ls_but0id IS NOT INITIAL.
        ls_output-general_data-vat_registration_no = ls_but0id-idnumber.
        CLEAR ls_but0id.
      ENDIF.

      LOOP AT lt_but0id INTO ls_but0id WHERE partner = ls_input-bpartner AND type <> 'VATRU'.
        ls_output-general_data-identification_category = ls_but0id-type.
        CLEAR ls_but0id.
        EXIT.
      ENDLOOP.

      READ TABLE lt_general_category_desc INTO DATA(ls_general_category_desc) WITH KEY type = ls_output-general_data-identification_category BINARY SEARCH.
      IF ls_general_category_desc IS NOT INITIAL.
        ls_output-general_data-identification_category_desc = ls_general_category_desc-text.
        CLEAR ls_general_category_desc.
      ENDIF.

      LOOP AT lt_but0id INTO ls_but0id WHERE partner = ls_input-bpartner AND type <> 'VATRU' .
        ls_output-general_data-identification_number = ls_but0id-idnumber.
        CLEAR ls_but0id.
        EXIT.
      ENDLOOP.

      READ TABLE lt_but021_fs INTO DATA(ls_but021_fs) WITH KEY partner = ls_input-bpartner BINARY SEARCH.
      IF ls_but021_fs IS NOT INITIAL.
        READ TABLE lt_email INTO DATA(ls_email) WITH KEY addrnumber = ls_but021_fs-addrnumber BINARY SEARCH.
        IF ls_email IS NOT INITIAL.
          ls_output-address-e_mail = ls_email-smtp_addr.
          CLEAR ls_email.
        ENDIF.
        READ TABLE lt_telephone INTO DATA(ls_telephone) WITH KEY addrnumber = ls_but021_fs-addrnumber BINARY SEARCH.
        IF ls_telephone IS NOT INITIAL.
          ls_output-address-telephone = ls_telephone-tel_number.
          CLEAR ls_telephone.
        ENDIF.
        CLEAR ls_but021_fs.
      ENDIF.
      "ADDRESS
      IF addressdata IS NOT INITIAL.
        ls_output-address-country_code = addressdata-country.
        READ TABLE lt_country_name INTO DATA(ls_country_name) WITH KEY land1 = addressdata-country BINARY SEARCH.
        IF ls_country_name IS NOT INITIAL.
          ls_output-address-country_name = ls_country_name-landx.
          CLEAR ls_country_name.
        ENDIF.

        ls_output-address-city_code = addressdata-city_no.
        READ TABLE lt_city_name INTO DATA(ls_city_name) WITH KEY country = addressdata-country city_code = addressdata-city_no BINARY SEARCH.
        IF ls_city_name IS NOT INITIAL.
          ls_output-address-city_name = ls_city_name-city_name.
          CLEAR ls_city_name.
        ENDIF.

        ls_output-address-district_code = addressdata-distrct_no.
        READ TABLE lt_district_name INTO DATA(ls_district_name) WITH KEY city_code = addressdata-city_no cityp_code = addressdata-distrct_no country = addressdata-country BINARY SEARCH.
        IF ls_district_name IS NOT INITIAL.
          ls_output-address-district_name = ls_district_name-city_part.
          CLEAR ls_district_name.
        ENDIF.

        ls_output-address-ward_code = addressdata-regiogroup.
        READ TABLE lt_ward_name INTO DATA(ls_ward_name) WITH KEY regiogroup = ls_output-address-ward_code BINARY SEARCH.
        IF ls_ward_name IS NOT INITIAL.
          ls_output-address-ward_name = ls_ward_name-descript.
          CLEAR ls_ward_name.
        ENDIF.

        IF addressdata-floor = 1.
          ls_output-address-street_code = addressdata-street_no.
          ls_output-address-street_name = addressdata-street.
          CONCATENATE addressdata-str_suppl1  addressdata-str_suppl2  addressdata-str_suppl3  addressdata-location INTO ls_output-address-house_no.
        ELSE.
          CONCATENATE  addressdata-street  addressdata-str_suppl1  addressdata-str_suppl2  addressdata-str_suppl3 INTO ls_output-address-house_no.
        ENDIF.
      ENDIF.
      "ATTACHMENT
      IF lt_instid_b IS NOT INITIAL.
        LOOP AT lt_instid_b INTO DATA(ls_instid_b) WHERE instid_a = bpartner.
          ls_document_id = ls_instid_b-instid_b.
          CALL FUNCTION 'SO_DOCUMENT_READ_API1'
            EXPORTING
              document_id    = ls_document_id
            IMPORTING
              document_data  = ls_document_data
            TABLES
              object_content = lt_object_content.
          APPEND INITIAL LINE TO ls_output-attachments ASSIGNING FIELD-SYMBOL(<fs_attachment>).
          <fs_attachment>-attachment_key = ls_document_id.
          IF ls_instid_b-reltype = 'URL'.
*            <fs_attachment>-url = ls_document_data-obj_descr.
            LOOP AT lt_object_content INTO DATA(ls_object_content).
              <fs_attachment>-url = ls_object_content-line.
              SHIFT <fs_attachment>-url BY 5 PLACES.
              CLEAR ls_object_content.
            ENDLOOP.
          ENDIF.
          UNASSIGN <fs_attachment>.
          CLEAR :ls_instid_b,ls_document_id,ls_document_data.
          REFRESH lt_object_content[].
        ENDLOOP.
      ENDIF.
      "BANKDETAILS
      IF bankdetails IS NOT INITIAL.
        LOOP AT bankdetails INTO DATA(ls_bankdetail).
          MOVE-CORRESPONDING ls_bankdetail TO ls_bankdetail_out.
          READ TABLE lt_bnka INTO DATA(ls_bnka) WITH KEY banks = ls_bankdetail_out-bank_ctry bankl = ls_bankdetail_out-bank_key BINARY SEARCH.
          IF ls_bnka IS NOT INITIAL.
            ls_bankdetail_out-bank_name = ls_bnka-banka.
            CLEAR ls_bnka.
          ENDIF.
          ls_bankdetail_out-data_key = ls_bankdetail-bankdetailid.
          APPEND ls_bankdetail_out TO ls_output-bankdetails.
          CLEAR : ls_bankdetail_out,ls_bankdetail.
        ENDLOOP.
      ENDIF.
      "CUSTOMER DATA
      IF businesspartnerroles IS NOT INITIAL.
        ls_output-customer_data-valid_from = date_default.
        ls_output-customer_data-valid_to = date_default.
        LOOP AT businesspartnerroles INTO DATA(ls_businesspartnerrole) WHERE partnerrole = 'FLCU01'.
          IF ls_businesspartnerrole-valid_from IS NOT INITIAL.
            ls_output-customer_data-valid_from = ls_businesspartnerrole-valid_from.
          ENDIF.
          IF ls_output-customer_data-valid_to IS NOT INITIAL.
            ls_output-customer_data-valid_to = ls_businesspartnerrole-valid_to.
          ENDIF.
          CLEAR ls_businesspartnerrole.
        ENDLOOP.
      ENDIF.
      READ TABLE lt_knb1 INTO DATA(ls_knb1) WITH KEY kunnr = bpartner.
      IF ls_knb1 IS NOT INITIAL.
        ls_output-customer_data-payment_term = ls_knb1-zterm.
        ls_output-customer_data-payment_methods = ls_knb1-zwels.
        CLEAR ls_knb1.
      ENDIF.
*      SELECT SINGLE zterm FROM knb1 INTO ls_output-customer_data-payment_term
*                   WHERE kunnr = bpartner AND bukrs = '1000'.
      READ TABLE lt_t052u INTO DATA(ls_t052u) WITH KEY zterm = ls_output-customer_data-payment_term BINARY SEARCH.
      IF ls_t052u IS NOT INITIAL.
        ls_output-customer_data-payment_term_desc = ls_t052u-text1.
        CLEAR ls_t052u.
      ENDIF.

*      SELECT SINGLE zwels FROM knb1 INTO ls_output-customer_data-payment_methods
*                   WHERE kunnr = bpartner AND bukrs = '1000'.
      READ TABLE lt_t042z INTO DATA(ls_t042z) WITH KEY zlsch = ls_output-customer_data-payment_methods BINARY SEARCH.
      IF ls_t042z IS NOT INITIAL.
        ls_output-customer_data-payment_methods_desc = ls_t042z-text1.
        CLEAR ls_t042z.
      ENDIF.
      IF strlen( ls_output-customer_data-payment_methods ) = 2.
        READ TABLE lt_t042z INTO ls_t042z WITH KEY zlsch = ls_output-customer_data-payment_methods+1(1) BINARY SEARCH.
        IF ls_t042z IS NOT INITIAL.
          CONCATENATE ls_output-customer_data-payment_methods_desc '|' ls_t042z-text1 INTO ls_output-customer_data-payment_methods_desc.
          CLEAR ls_t042z.
        ENDIF.
      ENDIF.
      "
      READ TABLE lt_payment_period INTO DATA(ls_payment_period) WITH KEY partner = bpartner BINARY SEARCH.
      IF ls_payment_period IS NOT INITIAL.
        ls_output-customer_data-reconciliation_cal-type = ls_payment_period-ky_ds.
        lv_count = 1.
        DO 38 TIMES.
          CONCATENATE 'DAY_' lv_count INTO DATA(col_name).
          ASSIGN COMPONENT col_name OF STRUCTURE ls_payment_period TO FIELD-SYMBOL(<fs_flag>).
          IF <fs_flag> = 'X'.
            APPEND INITIAL LINE TO ls_output-customer_data-reconciliation_cal-days ASSIGNING FIELD-SYMBOL(<fs_day>).
            <fs_day>-day = match( val   = col_name
                      regex = '[^_]*$' ).
            <fs_day>-flag = 'X'.
          ENDIF.
          lv_count = lv_count + 1.
        ENDDO.
        CLEAR ls_payment_period.
      ENDIF.
      READ TABLE lt_dd07v1 INTO DATA(ls_dd07v1) WITH KEY domvalue_l = ls_output-customer_data-reconciliation_cal-type.
      IF ls_dd07v1 IS NOT INITIAL.
        ls_output-customer_data-reconciliation_cal-type_desc = ls_dd07v1-ddtext.
        CLEAR ls_dd07v1.
      ENDIF.
      READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = bpartner BINARY SEARCH.
      IF ls_kna1 IS NOT INITIAL.
        ls_output-customer_data-all_sorg_flag = ls_kna1-katr1.
        ls_output-customer_data-customer_classification = ls_kna1-kukla.
        IF ls_kna1-konzs = 'SAN'.
          ls_output-customer_data-ecommerce_pfm_flag = 'X'.
        ENDIF.
        CLEAR ls_kna1.
      ENDIF.
      SELECT SINGLE vtext FROM tkukt INTO ls_output-customer_data-customer_classification_desc
                          WHERE spras = sy-langu
                          AND kukla = ls_output-customer_data-customer_classification.
      READ TABLE relationships INTO DATA(ls_relationships) WITH KEY relationshipcategory = 'ZBP3' partner1 = bpartner .
      IF ls_relationships IS NOT INITIAL.
        ls_output-customer_data-ecommerce_bp = ls_relationships-partner2.
      ENDIF.
      IF ls_output-customer_data-ecommerce_bp IS NOT INITIAL.
        CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
          EXPORTING
            businesspartner         = ls_relationships-partner2
          IMPORTING
            centraldataperson       = centraldataperson
            centraldataorganization = centraldataorganization.
        CLEAR ls_relationships.
      ENDIF.

      IF centraldataperson IS NOT INITIAL.
        ls_output-customer_data-ecommerce_bp_name =  centraldataperson-fullname.
      ELSEIF centraldataorganization IS NOT INITIAL.
        CONCATENATE centraldataorganization-name2 centraldataorganization-name3 centraldataorganization-name4
                    INTO ls_output-customer_data-ecommerce_bp_name.
      ENDIF.
      CLEAR : centraldataperson,centraldataorganization.
*      IF ls_kna1 IS NOT INITIAL.
*        ls_output-customer_data-all_sorg_flag = ls_kna1-katr1.
*        CLEAR ls_kna1.
*      ENDIF.
      IF lt_knvv IS NOT INITIAL.
        LOOP AT lt_knvv INTO DATA(ls_knvv) WHERE kunnr = bpartner.
          APPEND INITIAL LINE TO ls_output-customer_data-extend_sales_org ASSIGNING FIELD-SYMBOL(<fs_extend_sales_org>).
          <fs_extend_sales_org>-sales_org = ls_knvv-vkorg.
          READ TABLE LT_ORG_DESC INTO DATA(LS_ORG_DESC) WITH KEY VKORG = ls_knvv-vkorg.
          IF LS_ORG_DESC IS NOT INITIAL.
            <fs_extend_sales_org>-sales_org_desc = LS_ORG_DESC-vtext.
            CLEAR LS_ORG_DESC.
          ENDIF.
          CLEAR ls_knvv.
          UNASSIGN <fs_extend_sales_org>.
        ENDLOOP.
      ENDIF.
      READ TABLE lt_knb1 INTO ls_knb1 WITH KEY kunnr = bpartner BINARY SEARCH.
      IF ls_knb1 IS NOT INITIAL.
        ls_output-customer_data-reconciliation_account = ls_knb1-akont.
        CLEAR ls_knb1.
      ENDIF.
      "PO COD , RECONCILIATION_ACCOUNT
      IF relationships IS NOT INITIAL.
        LOOP AT relationships INTO ls_relationships WHERE partner1 = bpartner.
          IF ls_relationships-relationshipcategory = 'ZBP1'.
            APPEND INITIAL LINE TO ls_output-customer_data-po_reconciliation ASSIGNING FIELD-SYMBOL(<fs_po_reconciliation>).
            <fs_po_reconciliation>-post_office = ls_relationships-partner2.
            IF <fs_po_reconciliation>-post_office IS NOT INITIAL.
              CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
                EXPORTING
                  businesspartner         = ls_relationships-partner2
                IMPORTING
                  centraldataperson       = centraldataperson
                  centraldataorganization = centraldataorganization.
            ENDIF.
            IF centraldataperson IS NOT INITIAL.
              <fs_po_reconciliation>-post_office_name =  centraldataperson-fullname.
            ELSEIF centraldataorganization IS NOT INITIAL.
              CONCATENATE centraldataorganization-name2 centraldataorganization-name3 centraldataorganization-name4
                          INTO <fs_po_reconciliation>-post_office_name.
            ENDIF.
            <fs_po_reconciliation>-valid_from = ls_relationships-validfromdate.
            <fs_po_reconciliation>-valid_to = ls_relationships-validuntildate.
            UNASSIGN <fs_po_reconciliation>.
            CLEAR : centraldataperson , centraldataorganization.
          ELSEIF ls_relationships-relationshipcategory = 'ZBP2'.
            APPEND INITIAL LINE TO ls_output-customer_data-po_cod ASSIGNING FIELD-SYMBOL(<fs_po_cod>).
            <fs_po_cod>-post_office = ls_relationships-partner2.
            IF <fs_po_cod>-post_office IS NOT INITIAL.
              CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
                EXPORTING
                  businesspartner         = ls_relationships-partner2
                IMPORTING
                  centraldataperson       = centraldataperson
                  centraldataorganization = centraldataorganization.
            ENDIF.
            IF centraldataperson IS NOT INITIAL.
              <fs_po_cod>-post_office_name =  centraldataperson-fullname.
            ELSEIF centraldataorganization IS NOT INITIAL.
              CONCATENATE centraldataorganization-name2 centraldataorganization-name3 centraldataorganization-name4
                          INTO <fs_po_cod>-post_office_name.
            ENDIF.
            <fs_po_cod>-valid_from = ls_relationships-validfromdate.
            <fs_po_cod>-valid_to = ls_relationships-validuntildate.
            UNASSIGN <fs_po_cod>.
            CLEAR : centraldataperson , centraldataorganization.
          ENDIF.
        ENDLOOP.
      ENDIF.
      "VENDOR
      READ TABLE lt_lfb1 INTO DATA(ls_lfb1) WITH KEY lifnr = bpartner BINARY SEARCH.
      IF ls_lfb1 IS NOT INITIAL.
        ls_output-vendor_data-payment_term = ls_lfb1-zterm.
        ls_output-vendor_data-payment_methods = ls_lfb1-zwels.
        CLEAR ls_lfb1.
      ENDIF.
      READ TABLE lt_t052u INTO ls_t052u WITH KEY zterm = ls_output-vendor_data-payment_term BINARY SEARCH.
      IF ls_t052u IS NOT INITIAL.
        ls_output-vendor_data-payment_term_desc = ls_t052u-text1.
        CLEAR ls_t052u.
      ENDIF.

      READ TABLE lt_t042z INTO ls_t042z WITH KEY zlsch = ls_output-vendor_data-payment_methods BINARY SEARCH.
      IF ls_t042z IS NOT INITIAL.
        ls_output-vendor_data-payment_methods_desc = ls_t042z-text1.
        CLEAR ls_t042z.
      ENDIF.
      IF strlen( ls_output-vendor_data-payment_methods ) = 2.
        READ TABLE lt_t042z INTO ls_t042z WITH KEY zlsch = ls_output-vendor_data-payment_methods+1(1) BINARY SEARCH.
        IF ls_t042z IS NOT INITIAL.
          CONCATENATE ls_output-vendor_data-payment_methods_desc '|' ls_t042z-text1 INTO ls_output-vendor_data-payment_methods_desc.
          CLEAR ls_t042z.
        ENDIF.
      ENDIF.
      IF ls_output IS NOT INITIAL.
        APPEND ls_output TO lt_output.
      ENDIF.
      CLEAR : ls_output , bpartner,addressdata,centraldataperson,centraldataorganization.
      REFRESH : bankdetails[] , businesspartnerroles[],relationships[].
    ENDLOOP.
    output-mt_zfii017d_bp_out-bp_detail = lt_output[].
    output-mt_zfii017d_bp_out-ev_error = '01'.
  ENDMETHOD.
