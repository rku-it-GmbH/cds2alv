@AbapCatalog.sqlViewName: 'ZICDSALVSELOPTS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'SELECT-OPTIONS der generierten Programme für CDS-Views'
@VDM.viewType: #BASIC
define view ZI_CDS_ALV_SelectOptions as select from zcds_alv_selopts {
    key progname as ProgramName,
    key sel_name as SelectionName,
    cds_view as CdsView,
    fieldname as FieldName
}
