@AbapCatalog.sqlViewName: '/C2A/I_EXT_HDR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report extensions for CDS views: Header data'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view /C2A/I_ExtensionHeaders as select from /c2a/ext_hdr
    association [1..1] to /C2A/I_ExtensionHeaderTexts as _Text       on _Text.ExtensionName       = $projection.ExtensionName
                                                                    and _Text.Language            = $session.system_language
    association [0..*] to /C2A/I_ExtensionParameters  as _Parameters on _Parameters.ExtensionName = $projection.ExtensionName
{
    key extension_name    as ExtensionName,
    alternative_selection as AlternativeSelection,
    alternative_display   as AlternativeDisplay,
    implementing_class    as ImplementingClass,
    _Text,
    _Parameters
}
