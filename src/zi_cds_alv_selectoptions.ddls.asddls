@AbapCatalog.sqlViewName: 'ZICDSALVSELOPTS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'SELECT-OPTIONS of generated programs for CDS Views'
@VDM.viewType: #BASIC
define view ZI_CDS_ALV_SelectOptions as select from zcds_alv_selopts 
association[1..1] to ZI_CDS_ALV_Programs as _Program on _Program.ProgramName = $projection.ProgramName
{
    key progname as ProgramName,
    key sel_name as SelectionName,
    cds_view     as CdsView,
    fieldname    as FieldName,
    _Program
}
