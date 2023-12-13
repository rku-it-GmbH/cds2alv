@AbapCatalog.sqlViewName: 'ZCCDSALVPROGEXT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Program Extensions for a CDS View'
@VDM.viewType: #CONSUMPTION
define view ZC_CDS_ALV_ProgramExtensions as select from ZI_CDS_ALV_ProgramExtensions
association[1..1] to ZC_CDS_ALV_Programs as _Program on _Program.CdsView = $projection.CdsView
{   
    @Consumption.filter.selectionType: #RANGE
    @Consumption.semanticObject: 'CDSView'
    @UI.lineItem: [{ position: 10 }, { type: #FOR_INTENT_BASED_NAVIGATION, semanticObjectAction: 'ShowContent', label: 'Start Report' }]
    key CdsView,
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 20 }]
    key ExtensionName,
    @UI.lineItem: [{ position: 30 }]
    Active,
    /* Associations */
    _Program
}
