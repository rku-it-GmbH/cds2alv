@AbapCatalog.sqlViewName: 'ZICDSALVEXTPAR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Parameter einer Programmerweiterung'
@VDM.viewType: #BASIC
define view ZI_CDS_ALV_ExtensionParameters as select from zcds_alv_extpar
association[1..1] to ZI_CDS_ALV_ExtensionHeaders    as _Header on _Header.ExtensionName = $projection.ExtensionName
association[0..*] to ZI_CDS_ALV_ExtensionParamTexts as _Text   on _Text.ExtensionName   = $projection.ExtensionName
                                                              and _Text.ParameterName   = $projection.ParameterName 
{
    key extension_name as ExtensionName,
    key parameter_name as ParameterName,
    db_field           as DbField,
    has_value_help     as HasValueHelp,
    has_help           as HasHelp,
    attribute_name     as AttributeName,
    _Header,
    _Text
}
