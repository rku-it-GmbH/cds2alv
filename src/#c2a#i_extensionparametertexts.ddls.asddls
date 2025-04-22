@AbapCatalog.sqlViewName: '/C2A/I_EXT_PART'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Report extensions for CDS views: Parameter texts'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view /C2A/I_ExtensionParameterTexts as select from /c2a/ext_part
    association [1..1] to /C2A/I_ExtensionParameters as _Parameter on _Parameter.ExtensionName = $projection.ExtensionName
                                                                  and _Parameter.ParameterName = $projection.ParameterName
{
    key language       as Language,
    key extension_name as ExtensionName,
    key parameter_name as ParameterName,
    parameter_text     as ParameterText,
    _Parameter
}
