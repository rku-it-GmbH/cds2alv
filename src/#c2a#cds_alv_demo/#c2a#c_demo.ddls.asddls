@AbapCatalog.sqlViewName: '/C2A/CDEMO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Demo Consumption View'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.semanticKey: ['DemoKey']
@VDM.viewType: #CONSUMPTION
define view /C2A/C_Demo as select from /C2A/I_Demo
{   
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 1 }]
    @UI.selectionField: [{ position: 1, qualifier: 'Main' }]
    key DemoKey,
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 2, criticality: 'Criticality' }]
    @UI.selectionField: [{ position: 2, qualifier: 'Main' }]
    DemoText,
    @Consumption.filter.selectionType: #INTERVAL
    @UI.lineItem: [{ position: 3 }]
    @UI.selectionField: [{ position: 3, qualifier: 'Sub' }]
    DemoInteger,
    @UI.lineItem: [{ position: 4 }]
    DemoFloat,
    @UI.lineItem: [{ position: 5 }]
    @Semantics.amount.currencyCode: 'DemoCurrency'
    DemoAmount,
    @UI.hidden: true
    DemoCurrency,
    @UI.lineItem: [{ position: 6 }]
    @Semantics.quantity.unitOfMeasure: 'DemoUnit'
    DemoQuantity,
    @UI.hidden: true
    DemoUnit,
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 8 }]
    @UI.selectionField: [{ position: 4 }]
    DemoEmail,
    @UI.lineItem: [{ position: 9, type: #WITH_URL, url: 'DemoUrl' }]
    @Semantics.url.mimeType: 'DemoMimeType'
    DemoUrl,
    @UI.lineItem: [{ position: 10 }]
    DemoMimeType,
    @Consumption.filter.defaultValue: '3'
    @Consumption.filter.selectionType: #INTERVAL
    @UI.lineItem: [{ position: 11 }]
    @UI.selectionField: [{ position: 5, qualifier: 'Sub' }]
    Criticality,
    @Consumption.hidden: true
    'X' as Invisible
}
