@AbapCatalog.sqlViewName: '/C2A/C_IOC_CLIF'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Implementation registry for the IoC Container'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view /C2A/C_IocContainerRegistry as select from /C2A/I_IocContainerRegistry
{
    @Consumption.filter.selectionType: #RANGE
    @Consumption.semanticObject: 'AbapInterface'
    @UI.lineItem: [{ position: 10, type: #WITH_INTENT_BASED_NAVIGATION, semanticObjectAction: 'CallEditor' }]
    key Interface,
    @Consumption.filter.selectionType: #RANGE
    @Consumption.semanticObject: 'AbapClass'
    @UI.lineItem: [{ position: 20, type: #WITH_INTENT_BASED_NAVIGATION, semanticObjectAction: 'CallEditor' }]
    Class
}
