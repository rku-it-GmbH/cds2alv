@AbapCatalog.sqlViewName: 'ZICDSALVEXTPART'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Parametertexte einer Programmerweiterung'
@VDM.viewType: #BASIC
define view ZI_CDS_ALV_ExtensionParamTexts as select from zcds_alv_extpart 
{
    key spras          as Language,
    key extension_name as ExtensionName,
    key parameter_name as ParameterName,
    parameter_text     as ParameterText
}
