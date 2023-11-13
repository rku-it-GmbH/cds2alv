@AbapCatalog.sqlViewName: 'ZICDSALVDEMO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Demo interface view'
//@ObjectModel.compositionRoot: true
@ObjectModel.modelCategory: #BUSINESS_OBJECT
@ObjectModel.representativeKey: 'demo_key'
@ObjectModel.semanticKey: ['demo_key']
@ObjectModel.writeActivePersistence: 'ZCDS_ALV_DEMO'
@ObjectModel.transactionalProcessingEnabled: true
@ObjectModel.deleteEnabled: true
@ObjectModel.updateEnabled: true
@VDM.viewType: #BASIC
define view ZI_CDS_ALV_Demo as select from zcds_alv_demo {
   @EndUserText.label: 'Key'
   @EndUserText.quickInfo: 'Key'
   @ObjectModel.readOnly: true
   key demo_key,
   @EndUserText.label: 'Text'
   @EndUserText.quickInfo: 'Text'
       demo_text,
   @EndUserText.label: 'Integer'
   @EndUserText.quickInfo: 'Integer'
   @DefaultAggregation: #MAX
       demo_integer,
   @EndUserText.label: 'Float'
   @EndUserText.quickInfo: 'Float'
   @DefaultAggregation: #AVG
       demo_float,
   @EndUserText.label: 'Amount'
   @EndUserText.quickInfo: 'Amount'
   @DefaultAggregation: #SUM
   @Semantics.amount.currencyCode: 'demo_currency'
       demo_amount,
   @EndUserText.label: 'Currency'
   @EndUserText.quickInfo: 'Currency'
   @Semantics.currencyCode: true
       demo_currency,
   @EndUserText.label: 'Quantity'
   @EndUserText.quickInfo: 'Quantity'
   @DefaultAggregation: #SUM
   @Semantics.quantity.unitOfMeasure: 'demo_unit'
       demo_quantity,
   @EndUserText.label: 'Unit'
   @EndUserText.quickInfo: 'Unit'
   @Semantics.unitOfMeasure: true
       demo_unit,
   @EndUserText.label: 'Email address'
   @EndUserText.quickInfo: 'Email address'
   @Semantics.eMail.address: true
       demo_email,
   @EndUserText.label: 'URL'
   @EndUserText.quickInfo: 'URL'
   @Semantics.url.mimeType: 'demo_mime_type'
       demo_url,
   @EndUserText.label: 'MIME-Typ'
   @EndUserText.quickInfo: 'MIME-Typ'
       demo_mime_type,
   @DefaultAggregation: #MIN
       criticality
}
