@AbapCatalog.sqlViewName: '/C2A/I_REPORT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Programs for displaying CDS views'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view /C2A/I_Reports as select from /c2a/report
    association [0..*] to /C2A/I_ReportParameters    as _Parameters    on _Parameters.CdsView    = $projection.CdsView
    association [0..*] to /C2A/I_ReportSelectOptions as _SelectOptions on _SelectOptions.CdsView = $projection.CdsView
    association [0..*] to /C2A/I_ReportExtensions    as _Extensions    on _Extensions.CdsView    = $projection.CdsView
{
    key cds_view               as CdsView,
    progname                   as ProgramName,
    dynpro                     as SelectionScreen,
    author                     as Author,
    generated_at               as GeneratedAt,
    no_generation              as NoGeneration,
    add_functions_display_mode as AddFunctionsDisplayMode,
    _Parameters,
    _SelectOptions,
    _Extensions
}
