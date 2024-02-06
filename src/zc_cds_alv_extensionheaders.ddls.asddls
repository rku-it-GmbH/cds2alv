@AbapCatalog.sqlViewName: 'ZCCDSALVEXTHDR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Erweiterungen von Reports für CDS-Views'
@VDM.viewType: #CONSUMPTION
define view ZC_CDS_ALV_ExtensionHeaders as select from ZI_CDS_ALV_ExtensionHeaders 
association[0..*] to ZC_CDS_ALV_ExtensionParameters as _Parameters on _Parameters.ExtensionName = $projection.ExtensionName
{
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 10 }]
    key ExtensionName,
    @Consumption.filter.selectionType: #SINGLE
    @UI.lineItem: [{ position: 40 }]
    AlternativeSelection,
    @Consumption.filter.selectionType: #SINGLE
    @UI.lineItem: [{ position: 60 }]
    AlternativeDisplay,
    @Consumption.semanticObject: 'AbapClass'
    @UI.lineItem: [{ position: 30, type: #WITH_INTENT_BASED_NAVIGATION, semanticObjectAction: 'CallEditor' }]
    ImplementingClass,
    @UI.lineItem: [{ position: 20 }]
    _Text[ 1: Language = $session.system_language ].ExtensionText,
    @UI.lineItem: [{ position: 50 }]
    _Text[ 1: Language = $session.system_language ].SelectionText,
    @UI.lineItem: [{ position: 70 }]
    _Text[ 1: Language = $session.system_language ].DisplayText,
    /* Associations */
    _Parameters
}
