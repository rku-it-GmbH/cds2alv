@AbapCatalog.sqlViewName: '/C2A/I_EXT_PAR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report extensions for CDS views: Parameters'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view /C2A/I_ExtensionParameters as select from /c2a/ext_par
    association [1..1] to /C2A/I_ExtensionParameterTexts as _Text   on _Text.ExtensionName   = $projection.ExtensionName
                                                                   and _Text.ParameterName   = $projection.ParameterName 
                                                                   and _Text.Language        = $session.system_language
    association [1..1] to /C2A/I_ExtensionHeaders        as _Header on _Header.ExtensionName = $projection.ExtensionName
{
    key extension_name as ExtensionName,
    key parameter_name as ParameterName,
    db_field           as DbField,
    has_value_help     as HasValueHelp,
    has_help           as HasHelp,
    attribute_name     as AttributeName,
    _Text,
    _Header
}
