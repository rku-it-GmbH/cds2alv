@AbapCatalog.sqlViewName: 'ZICDSALVPROGRAM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Programme zur Anzeige von CDS-Views'
@VDM.viewType: #BASIC
define view ZI_CDS_ALV_Programs as select from zcds_alv_program {
    key cds_view as CdsView,
    progname as ProgramName,
    dynpro as SelectionScreen,
    author as Author,
    generated_at as GeneratedAt,
    no_generation as NoGeneration
}
