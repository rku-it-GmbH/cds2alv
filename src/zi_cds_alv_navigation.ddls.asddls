@AbapCatalog.sqlViewName: 'ZICDSALVNAV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Intent-Based Navigation'
@VDM.viewType: #BASIC
define view ZI_CDS_ALV_Navigation as select from zcds_alv_nav 
{
    key semantic_object as SemanticObject,
    key semantic_action as SemanticAction,
    function            as Function,
    default_parameter   as DefaultParameter,
    conversion_exit     as ConversionExit,
    object_type         as ObjectType,
    object_method       as ObjectMethod,
    transaction_code    as TransactionCode,
    parameter_id        as ParameterId,
    class               as Class,
    method              as Method,
    method_parameter    as MethodParameter,
    mass_processing     as MassProcessing,
    refresh_after       as RefreshAfter
}
