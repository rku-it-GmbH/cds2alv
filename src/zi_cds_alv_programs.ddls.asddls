@AbapCatalog.sqlViewName: 'ZICDSALVPROGRAM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Programme zur Anzeige von CDS-Views'
@VDM.viewType: #BASIC
define view ZI_CDS_ALV_Programs as select from zcds_alv_program 
association[0..*] to ZI_CDS_ALV_Parameters        as _Parameters     on _Parameters.ProgramName    = $projection.ProgramName
association[0..*] to ZI_CDS_ALV_SelectOptions     as _SelectOptions  on _SelectOptions.ProgramName = $projection.ProgramName
association[0..*] to ZI_CDS_ALV_ProgramExtensions as _Extensions     on _Extensions.CdsView        = $projection.CdsView
{
    key cds_view  as CdsView,
    progname      as ProgramName,
    dynpro        as SelectionScreen,
    author        as Author,
    generated_at  as GeneratedAt,
    no_generation as NoGeneration,
    _Parameters,
    _SelectOptions,
    _Extensions
}
