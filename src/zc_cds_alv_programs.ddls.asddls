@AbapCatalog.sqlViewName: 'ZCCDSALVPROGRAM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Programme zur Anzeige von CDS-Views'
define view ZC_CDS_ALV_Programs as select from ZI_CDS_ALV_Programs {
    @UI.lineItem: [{ position: 1 }]
    key CdsView,
    @UI.lineItem: [{ position: 2 }]
    ProgramName,
    @UI.lineItem: [{ position: 3 }]
    SelectionScreen,
    @UI.lineItem: [{ position: 4 }]
    Author,
    @UI.lineItem: [{ position: 5 }]
    GeneratedAt,
    @UI.lineItem: [{ position: 6 }]
    NoGeneration
}
