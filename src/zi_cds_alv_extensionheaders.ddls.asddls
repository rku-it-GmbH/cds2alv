@AbapCatalog.sqlViewName: 'ZICDSALVEXTHDR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Erweiterungen von Reports für CDS-Views'
@VDM.viewType: #BASIC
define view ZI_CDS_ALV_ExtensionHeaders as select from zcds_alv_exthdr 
association[0..*] to ZI_CDS_ALV_ExtensionHeaderText as _Text on _Text.ExtensionName = $projection.ExtensionName
association[0..*] to ZI_CDS_ALV_ExtensionParameters as _Parameters on _Parameters.ExtensionName = $projection.ExtensionName
{
    key extension_name    as ExtensionName,
    alternative_selection as AlternativeSelection,
    alternative_display   as AlternativeDisplay,
    implementing_class    as ImplementingClass,
    _Text,
    _Parameters
}
