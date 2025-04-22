@AbapCatalog.sqlViewName: '/C2A/I_IOC_CLIF'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Implementation registry for the IoC Container'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view /C2A/I_IocContainerRegistry as select from /c2a/ioc_clif
{
    key interface as Interface,
    class         as Class
}
