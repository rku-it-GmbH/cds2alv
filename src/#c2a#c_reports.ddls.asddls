@AbapCatalog.sqlViewName: '/C2A/C_REPORT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Programs for displaying CDS views'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #CONSUMPTION
define view /C2A/C_Reports as select from /C2A/I_Reports
    association [0..*] to /C2A/C_ReportParameters    as _Parameters    on _Parameters.CdsView    = $projection.CdsView
    association [0..*] to /C2A/C_ReportSelectOptions as _SelectOptions on _SelectOptions.CdsView = $projection.CdsView
    association [0..*] to /C2A/C_ReportExtensions    as _Extensions    on _Extensions.CdsView    = $projection.CdsView
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
    @UI.lineItem: [{ position: 70 }]
    AddFunctionsDisplayMode,
    /* Associations */
    _Extensions,
    _Parameters,
    _SelectOptions
}
