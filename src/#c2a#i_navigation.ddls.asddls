@AbapCatalog.sqlViewName: '/C2A/I_NAV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Intent-Based Navigation for the CDS ALV Framework'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view /C2A/I_Navigation as select from /c2a/navigation
{
    key semantic_object  as SemanticObject,
    key semantic_action  as SemanticAction,
    function             as Function,
    default_parameter    as DefaultParameter,
    conversion_exit      as ConversionExit,
    object_type          as ObjectType,
    object_method        as ObjectMethod,
    transaction_code     as TransactionCode,
    parameter_id         as ParameterId,
    class                as Class,
    method               as Method,
    method_parameter     as MethodParameter,
    mass_processing      as MassProcessing,
    refresh_after_action as RefreshAfterAction
}
