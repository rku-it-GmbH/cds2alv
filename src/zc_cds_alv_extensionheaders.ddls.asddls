@AbapCatalog.sqlViewName: 'ZCCDSALVEXTHDR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Erweiterungen von Reports für CDS-Views'
@VDM.viewType: #CONSUMPTION
define view ZC_CDS_ALV_ExtensionHeaders as select from ZI_CDS_ALV_ExtensionHeaders {
    key ExtensionName,
    AlternativeSelection,
    AlternativeDisplay,
    ImplementingClass,
    _Text[ 1: Language = $session.system_language ].ExtensionText,
    _Text[ 1: Language = $session.system_language ].SelectionText,
    _Text[ 1: Language = $session.system_language ].DisplayText,
    /* Associations */
    _Text
}
