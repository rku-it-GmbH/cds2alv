@AbapCatalog.sqlViewName: 'ZCCDSALVPROGRAM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Programme zur Anzeige von CDS-Views'
define view ZC_CDS_ALV_Programs as select from ZI_CDS_ALV_Programs 
association[0..*] to ZC_CDS_ALV_Parameters        as _Parameters    on _Parameters.ProgramName    = $projection.ProgramName
association[0..*] to ZC_CDS_ALV_SelectOptions     as _SelectOptions on _SelectOptions.ProgramName = $projection.ProgramName
association[0..*] to ZC_CDS_ALV_ProgramExtensions as _Extensions    on _Extensions.CdsView        = $projection.CdsView
{
    @Consumption.filter.selectionType: #RANGE
    @Consumption.semanticObject: 'CDSView'
    @UI.lineItem: [{ position: 10 }, { type: #FOR_INTENT_BASED_NAVIGATION, semanticObjectAction: 'ShowContent', label: 'Start Report' }]
    key CdsView,
    @UI.lineItem: [{ position: 20 }]
    ProgramName,
    @UI.lineItem: [{ position: 30 }]
    SelectionScreen,
    @UI.lineItem: [{ position: 40 }]
    Author,
    @UI.lineItem: [{ position: 50 }]
    GeneratedAt,
    @UI.lineItem: [{ position: 60 }]
    NoGeneration,
    _Parameters,
    _SelectOptions,
    _Extensions
}
