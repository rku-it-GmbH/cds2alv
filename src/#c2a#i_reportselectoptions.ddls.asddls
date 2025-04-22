@AbapCatalog.sqlViewName: '/C2A/I_REP_SELOP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'SELECT-OPTIONS in a generated program'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view /C2A/I_ReportSelectOptions as select from /c2a/rep_selopts
    association [1..1] to /C2A/I_Reports as _Report on _Report.CdsView = $projection.CdsView
{
    key progname as ProgramName,
    key sel_name as SelectionName,
    cds_view     as CdsView,
    fieldname    as Fieldname,
    _Report
}
