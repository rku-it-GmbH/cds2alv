@AbapCatalog.sqlViewName: 'ZICDSALVEXTHDRT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Erweiterungen von Reports für CDS-Views (Texte)'
@VDM.viewType: #BASIC
define view ZI_CDS_ALV_ExtensionHeaderText as select from zcds_alv_exthdrt 
{
    key spras          as Language,
    key extension_name as ExtensionName,
    extension_text     as ExtensionText,
    selection_text     as SelectionText,
    display_text       as DisplayText
}
