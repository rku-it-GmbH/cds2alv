@AbapCatalog.sqlViewName: 'ZCCDSALVEXTPAR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Parameter einer Programmerweiterung'
@VDM.viewType: #CONSUMPTION
define view ZC_CDS_ALV_ExtensionParameters as select from ZI_CDS_ALV_ExtensionParameters 
association[1..1] to ZC_CDS_ALV_ExtensionHeaders as _Header on _Header.ExtensionName = $projection.ExtensionName
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
    _Text[ 1: Language = $session.system_language ].ParameterText as ParameterText,
    /* Associations */
    _Header
}
