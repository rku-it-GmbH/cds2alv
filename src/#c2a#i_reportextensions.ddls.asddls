@AbapCatalog.sqlViewName: '/C2A/I_REP_EXT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Program extensions of a generated report for a CDS view'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view /C2A/I_ReportExtensions as select from /c2a/rep_ext
    association [1..1] to /C2A/I_Reports as _Report on _Report.CdsView = $projection.CdsView
{
    key cds_view       as CdsView,
    key extension_name as ExtensionName,
    active             as Active,
    activated_on       as ActivatedOn,
    _Report
}
