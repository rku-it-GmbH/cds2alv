@AbapCatalog.sqlViewName: 'ZICDSALVPARAMS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PARAMETERS der generierten Programme für CDS-Views'
@VDM.viewType: #BASIC
define view ZI_CDS_ALV_Parameters as select from zcds_alv_params 
association[1..1] to ZI_CDS_ALV_Programs as _Program on _Program.ProgramName = $projection.ProgramName
{
    key progname as ProgramName,
    key sel_name as SelectionName,
    cds_view     as CdsView,
    parname      as ParameterName,
    _Program
}
