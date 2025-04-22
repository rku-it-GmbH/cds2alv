@AbapCatalog.sqlViewName: '/C2A/IDEMO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Demo Interface View'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view /C2A/I_Demo as select from /c2a/demo
{
    key demo_key   as DemoKey,
    demo_text      as DemoText,
    demo_integer   as DemoInteger,
    demo_float     as DemoFloat,
    demo_amount    as DemoAmount,
    demo_currency  as DemoCurrency,
    demo_quantity  as DemoQuantity,
    demo_unit      as DemoUnit,
    demo_email     as DemoEmail,
    demo_url       as DemoUrl,
    demo_mime_type as DemoMimeType,
    criticality    as Criticality
}
