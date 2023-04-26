@AbapCatalog.sqlViewName: 'ZV_TEST_CDSV2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Z_TEST_CDSV2'
define view Z_TEST_CDSV2 as select from bkpf 
association [0..1] to t001 as _t001
on bkpf.bukrs  = _t001.bukrs
{
    key bukrs,
    belnr,
    gjahr,
    cpudt,
    _t001.butxt,
    _t001.ort01,
    _t001
}
where bukrs = 'ZA01'
