@AbapCatalog.sqlViewName: 'ZCCDSVALVSELOPTS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'SELECT-OPTIONS der generierten Programme für CDS-Views'
@VDM.viewType: #CONSUMPTION
define view ZC_CDS_ALV_SelectOptions as select from ZI_CDS_ALV_SelectOptions  {
    @UI.lineItem: [{ position: 1 }]
    key ProgramName,
    @UI.lineItem: [{ position: 2 }]
    key SelectionName,
    @UI.lineItem: [{ position: 3 }]
    CdsView,
    @UI.lineItem: [{ position: 4 }]
    FieldName
}
