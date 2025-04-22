@AbapCatalog.sqlViewName: '/C2A/C_NAV_EXIT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Custom Implementations (Exits) for Intent-Based Navigation'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #CONSUMPTION
define view /C2A/C_NavigationExits as select from /C2A/I_NavigationExits
{   
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 10 }]
    key SemanticObject,
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 20 }]
    key SemanticAction,
    @Consumption.filter.selectionType: #RANGE
    @Consumption.semanticObject: 'CDSView'
    @UI.lineItem: [{ position: 30 }, { type: #FOR_INTENT_BASED_NAVIGATION, semanticObjectAction: 'ShowContent', label: 'Start Report' }]
    key CdsView,
    @Consumption.semanticObject: 'AbapClass'
    @UI.lineItem: [{ position: 40, type: #WITH_INTENT_BASED_NAVIGATION, semanticObjectAction: 'CallEditor' }]
    ImplementingClass
}
