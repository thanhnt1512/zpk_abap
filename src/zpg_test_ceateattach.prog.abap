*&---------------------------------------------------------------------*
*& Report ZPG_TEST_CEATEATTACH
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPG_TEST_CEATEATTACH.
*PARAMETERS : p_opp_id TYPE crmt_object_id,     " Opportunity ID
*p_url    TYPE char100 LOWER CASE.             " External URL
*
*DATA : lv_opp_guid        TYPE crmt_object_guid,
*wa_bus_obj     TYPE sibflporb,
*lt_url         TYPE sdokcntascs,
*wa_url         TYPE sdokcntasc,
*lt_prop        TYPE sdokproptys,
*wa_prop        TYPE sdokpropty,
*wa_loio        TYPE skwf_io,
*wa_phio        TYPE skwf_io,
*wa_error       TYPE skwf_error.
*
*CHECK p_url IS NOT INITIAL AND p_opp_id IS NOT INITIAL.
*
****
*CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*EXPORTING
*input  = p_opp_id
*IMPORTING
*output = p_opp_id.
*
****
*SELECT SINGLE guid
*FROM crmd_orderadm_h
*INTO lv_opp_guid
*WHERE object_id = p_opp_id.
*
****
*wa_url-line = p_url.
*APPEND wa_url TO lt_url.
*
****
*wa_bus_obj-instid = lv_opp_guid.  "(Opportunity GUID)
*wa_bus_obj-typeid = 'BUS2000111'. "(For Opportunity)
*wa_bus_obj-catid  = 'BO'.         "(Business Object)
*
****
*wa_prop-name = 'KW_RELATIVE_URL'. "The name of URL
*wa_prop-value = p_opp_id.
*APPEND wa_prop TO lt_prop.
*
*wa_prop-name = 'CONTENT_URL'.
*wa_prop-value = p_url.
*APPEND wa_prop TO lt_prop.
*
*wa_prop-name = 'LANGUAGE'.
*wa_prop-value = sy-langu.
*APPEND wa_prop TO lt_prop.
*
*wa_prop-name = 'DESCRIPTION'.
*wa_prop-value = 'Opportunity External URL'.
*APPEND wa_prop TO lt_prop.
*
****
*CALL METHOD cl_crm_documents=>create_url
*EXPORTING
*url             = lt_url
*properties      = lt_prop
*business_object = wa_bus_obj
*IMPORTING
*loio            = wa_loio
*phio            = wa_phio
*error           = wa_error.
*
****
*IF wa_error IS INITIAL.
*WRITE : 'URl added to Oppty : ', p_opp_id .
*ELSE.
*MESSAGE ID wa_error-id TYPE wa_error-type NUMBER wa_error-no
*WITH wa_error-v1 wa_error-v1 wa_error-v1 wa_error-v1.
*ENDIF.
SELECT *
       FROM spfli
       INTO TABLE @DATA(spfli_tab).

LOOP AT spfli_tab INTO DATA(wa)
                  GROUP BY ( key1 = wa-carrid
                             indx = GROUP INDEX
                             size = GROUP SIZE )
                  INTO data(key).
  cl_demo_output=>write( |{ key-size } { key-key1
                        } | ).
ENDLOOP.
cl_demo_output=>display( ).
