@AbapCatalog.sqlViewName: 'ZCCDSALVNAV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Intent-Based Navigation'
@VDM.viewType: #CONSUMPTION
define view ZC_CDS_ALV_Navigation as select from ZI_CDS_ALV_Navigation {
    key SemanticObject,
    key SemanticAction,
    Function,
    DefaultParameter,
    ConversionExit,
    ObjectType,
    ObjectMethod,
    TransactionCode,
    ParameterId
}
