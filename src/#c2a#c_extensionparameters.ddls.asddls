@AbapCatalog.sqlViewName: '/C2A/C_EXT_PAR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report extensions for CDS views: Parameters'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #CONSUMPTION
define view /C2A/C_ExtensionParameters as select from /C2A/I_ExtensionParameters
    association [1..1] to /C2A/C_ExtensionHeaders as _Header on _Header.ExtensionName = $projection.ExtensionName
{
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 10 }]
    key ExtensionName,
    @UI.lineItem: [{ position: 20 }]
    key ParameterName,
    @UI.lineItem: [{ position: 40 }]
    DbField,
    @UI.lineItem: [{ position: 50 }]
    HasValueHelp,
    @UI.lineItem: [{ position: 60 }]
    HasHelp,
    @UI.lineItem: [{ position: 70 }]
    AttributeName,
    @UI.lineItem: [{ position: 30 }]
    _Text.ParameterText,
    /* Associations */
    _Header
}
