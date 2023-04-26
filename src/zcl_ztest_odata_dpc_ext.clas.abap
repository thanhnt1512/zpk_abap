class ZCL_ZTEST_ODATA_DPC_EXT definition
  public
  inheriting from ZCL_ZTEST_ODATA_DPC
  create public .

public section.
protected section.

  methods BPS_GET_ENTITYSET
    redefinition .
  methods BPS_GET_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZTEST_ODATA_DPC_EXT IMPLEMENTATION.


  METHOD bps_get_entity.
**TRY.
*CALL METHOD SUPER->BPS_GET_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_request_object       =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**  IMPORTING
**    er_entity               =
**    es_response_context     =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    DATA ls_entity Like er_entity.
    io_tech_request_context->get_converted_keys( IMPORTING ES_KEY_VALUES = ls_entity ).
*    APPEND INITIAL LINE TO er_entity ASSIGNING FIELD-SYMBOL(<fs_entity>).
    er_entity-partner = ls_entity-partner.

  ENDMETHOD.


  METHOD bps_get_entityset.
**TRY.
*CALL METHOD SUPER->BPS_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
*    select PARTNER FROM but000 INTO CORRESPONDING FIELDS OF TABLE et_entityset UP TO 10 ROWS.
    DATA lv_entityset_name TYPE string.
    lv_entityset_name = io_tech_request_context->get_entity_set_name( ).
    APPEND INITIAL LINE TO  et_entityset ASSIGNING FIELD-SYMBOL(<fs_entityset>).
    <fs_entityset>-partner = '00001234'.
*    et_entityset[ 1 ]-partner = '00001234'.
  ENDMETHOD.
ENDCLASS.
