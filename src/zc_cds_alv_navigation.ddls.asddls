@AbapCatalog.sqlViewName: 'ZCCDSALVNAV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Intent-Based Navigation'
@VDM.viewType: #CONSUMPTION
define view ZC_CDS_ALV_Navigation as select from ZI_CDS_ALV_Navigation {
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 10 }]
    key SemanticObject,
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 20 }]
    key SemanticAction,
    @Consumption.semanticObject: 'FunctionModule'
    @UI.lineItem: [{ position: 30, type: #WITH_INTENT_BASED_NAVIGATION, semanticObjectAction: 'Display' }]
    Function,
    @UI.lineItem: [{ position: 40 }]
    DefaultParameter,
    @UI.lineItem: [{ position: 50 }]
    ConversionExit,
    @Consumption.semanticObject: 'BusinessObject'
    @UI.lineItem: [{ position: 60, type: #WITH_INTENT_BASED_NAVIGATION, semanticObjectAction: 'Display' }]
    ObjectType,
    @UI.lineItem: [{ position: 70 }]
    ObjectMethod,
    @Consumption.semanticObject: 'TransactionCode'
    @UI.lineItem: [{ position: 80, type: #WITH_INTENT_BASED_NAVIGATION, semanticObjectAction: 'Display' }]
    TransactionCode,
    @UI.lineItem: [{ position: 90 }]
    ParameterId,
    @Consumption.semanticObject: 'AbapClass'
    @UI.lineItem: [{ position: 100, type: #WITH_INTENT_BASED_NAVIGATION, semanticObjectAction: 'CallEditor' }]
    Class,
    @UI.lineItem: [{ position: 110 }]
    Method,
    @UI.lineItem: [{ position: 120 }]
    MethodParameter,
    @Consumption.filter.selectionType: #SINGLE
    @UI.lineItem: [{ position: 130 }]
    MassProcessing,
    @Consumption.filter.selectionType: #SINGLE
    @UI.lineItem: [{ position: 140 }]
    RefreshAfter
}
