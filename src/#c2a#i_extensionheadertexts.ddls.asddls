@AbapCatalog.sqlViewName: '/C2A/I_EXT_HDRT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report extensions for CDS views: Texts'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view /C2A/I_ExtensionHeaderTexts as select from /c2a/ext_hdrt
    association [1..1] to /C2A/I_ExtensionHeaders as _Header on _Header.ExtensionName = $projection.ExtensionName
{
    key language       as Language,
    key extension_name as ExtensionName,
    extension_text     as ExtensionText,
    selection_text     as SelectionText,
    display_text       as DisplayText,
    _Header
}
