@AbapCatalog.sqlViewName: 'ZICDSALVPROGEXT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Programmerweiterungen für einen CDS-View'
@VDM.viewType: #BASIC
define view ZI_CDS_ALV_ProgramExtensions as select from zcds_alv_progext {
    key cds_view as CdsView,
    key extension_name as ExtensionName,
    active as Active
}
