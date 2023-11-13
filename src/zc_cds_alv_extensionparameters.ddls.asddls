@AbapCatalog.sqlViewName: 'ZCCDSALVEXTPAR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Parameter einer Programmerweiterung'
@VDM.viewType: #CONSUMPTION
define view ZC_CDS_ALV_ExtensionParameters as select from ZI_CDS_ALV_ExtensionParameters {
    key ExtensionName,
    key ParameterName,
    DbField,
    HasValueHelp,
    HasHelp,
    AttributeName,
    _Text[ 1: Language = $session.system_language ].ParameterText as ParameterText,
    /* Associations */
    _Text
}
