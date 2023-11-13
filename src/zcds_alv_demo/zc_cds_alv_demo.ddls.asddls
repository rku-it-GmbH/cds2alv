@AbapCatalog.sqlViewName: 'ZCCDSALVDEMO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption-View für Demoprogramm'
@ObjectModel.semanticKey: ['DemoKey']
@VDM.viewType: #CONSUMPTION
define view ZC_CDS_ALV_Demo as select from ZI_CDS_ALV_Demo {
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 1 }]
    @UI.selectionField: [{ position: 1, qualifier: 'Main' }]
    key demo_key as DemoKey,
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 2, criticality: 'Criticality' }]
    @UI.selectionField: [{ position: 2, qualifier: 'Main' }]
    demo_text as DemoText,
    @Consumption.filter.selectionType: #INTERVAL
    @UI.lineItem: [{ position: 3 }]
    @UI.selectionField: [{ position: 3, qualifier: 'Sub' }]
    demo_integer as DemoInteger,
    @UI.lineItem: [{ position: 4 }]
    demo_float as DemoFloat,
    @UI.lineItem: [{ position: 5 }]
    @Semantics.amount.currencyCode: 'DemoCurrency'
    demo_amount as DemoAmount,
    @UI.hidden
    demo_currency as DemoCurrency,
    @UI.lineItem: [{ position: 6 }]
    @Semantics.quantity.unitOfMeasure: 'DemoUnit'
    demo_quantity as DemoQuantity,
    @Consumption.semanticObject: 'UnitOfMeasure'
    @UI.hidden
    @UI.lineItem: [{ type: #FOR_INTENT_BASED_NAVIGATION, semanticObjectAction: 'Maintain', label: 'Units of Measure' }]
    demo_unit as DemoUnit,
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 8 }]
    @UI.selectionField: [{ position: 4 }]
    demo_email as DemoEmail,
    @UI.lineItem: [{ position: 9, type: #WITH_URL, url: 'DemoUrl' }]
    @Semantics.url.mimeType: 'DemoMimeType'
    demo_url as DemoUrl,
    @UI.lineItem: [{ position: 10 }]
    demo_mime_type as DemoMimeType,
    @Consumption.filter.defaultValue: '3'
    @Consumption.filter.selectionType: #INTERVAL
    @UI.lineItem: [{ position: 11 }]
    @UI.selectionField: [{ position: 5, qualifier: 'Sub' }]
    criticality as Criticality,
    @Consumption.hidden: true
    'X' as Invisible
}
