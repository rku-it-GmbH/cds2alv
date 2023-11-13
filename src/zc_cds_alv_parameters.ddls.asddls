@AbapCatalog.sqlViewName: 'ZCCDSALVPARAMS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PARAMETERS der generierten Programme für CDS-Views'
@VDM.viewType: #CONSUMPTION
define view ZC_CDS_ALV_Parameters as select from ZI_CDS_ALV_Parameters {
    key ProgramName,
    key SelectionName,
    CdsView,
    ParameterName
}
