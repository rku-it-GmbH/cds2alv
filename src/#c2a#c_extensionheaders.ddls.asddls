@AbapCatalog.sqlViewName: '/C2A/C_EXT_HDR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report extensions for CDS views: Header data'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #CONSUMPTION
define view /C2A/C_ExtensionHeaders as select from /C2A/I_ExtensionHeaders
    association [0..*] to /C2A/C_ExtensionParameters as _Parameters on _Parameters.ExtensionName = $projection.ExtensionName
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
    _Text.ExtensionText,
    @UI.lineItem: [{ position: 50 }]
    _Text.SelectionText,
    @UI.lineItem: [{ position: 70 }]
    _Text.DisplayText,
    /* Associations */
    _Parameters
}
