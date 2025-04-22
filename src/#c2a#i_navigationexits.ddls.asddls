@AbapCatalog.sqlViewName: '/C2A/I_NAV_EXIT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Custom Implementations (Exits) for Intent-Based Navigation'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view /C2A/I_NavigationExits as select from /c2a/nav_exit
{
    key semantic_object as SemanticObject,
    key semantic_action as SemanticAction,
    key cds_view        as CdsView,
    implementing_class  as ImplementingClass
}
