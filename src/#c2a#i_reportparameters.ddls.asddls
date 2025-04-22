@AbapCatalog.sqlViewName: '/C2A/I_REP_PARAM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PARAMETERS in a generated program'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view /C2A/I_ReportParameters as select from /c2a/rep_params
    association [1..1] to /C2A/I_Reports as _Report on _Report.CdsView = $projection.CdsView
{
    key progname as ProgramName,
    key sel_name as SelectionName,
    cds_view     as CdsView,
    parname      as ParameterName,
    _Report
}
