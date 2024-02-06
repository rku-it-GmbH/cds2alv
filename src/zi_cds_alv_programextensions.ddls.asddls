@AbapCatalog.sqlViewName: 'ZICDSALVPROGEXT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Programmerweiterungen für einen CDS-View'
@VDM.viewType: #BASIC
define view ZI_CDS_ALV_ProgramExtensions as select from zcds_alv_progext
association[1..1] to ZI_CDS_ALV_Programs as _Program on _Program.CdsView = $projection.CdsView
{
    key cds_view       as CdsView,
    key extension_name as ExtensionName,
    active             as Active,
    _Program
}
