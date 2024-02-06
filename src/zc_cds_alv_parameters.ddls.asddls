@AbapCatalog.sqlViewName: 'ZCCDSALVPARAMS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PARAMETERS der generierten Programme für CDS-Views'
@VDM.viewType: #CONSUMPTION
define view ZC_CDS_ALV_Parameters as select from ZI_CDS_ALV_Parameters 
association[1..1] to ZC_CDS_ALV_Programs as _Program on _Program.ProgramName = $projection.ProgramName
{   
    @UI.lineItem: [{ position: 10 }]
    key ProgramName,
    @UI.lineItem: [{ position: 20 }]
    key SelectionName,
    @Consumption.filter.selectionType: #RANGE
    @Consumption.semanticObject: 'CDSView'
    @UI.lineItem: [{ position: 30 }, { type: #FOR_INTENT_BASED_NAVIGATION, semanticObjectAction: 'ShowContent', label: 'Start Report' }]
    CdsView,
    @UI.lineItem: [{ position: 40 }]
    ParameterName,
    _Program
}
