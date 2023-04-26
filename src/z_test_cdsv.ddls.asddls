@AbapCatalog.sqlViewName: 'Z_TEST_CDSV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Z_TEST_CDSV'
define view Z_TEST_CDSV1 as select from t001 association [1..1] to bkpf as _bkpf
on t001.bukrs =_bkpf.bukrs 
{
    key bukrs,
    butxt,
    ort01,
    land1,
    cast( waabw as abap.int4  ) as WAABWC,
    waers,
    _bkpf.kursf,
    _bkpf.belnr,
    _bkpf
    
}
where bukrs = 'ZA01'

