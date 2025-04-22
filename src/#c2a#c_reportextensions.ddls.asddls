@AbapCatalog.sqlViewName: '/C2A/C_REP_EXT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Program extensions of a generated report for a CDS view'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #CONSUMPTION
define view /C2A/C_ReportExtensions as select from /C2A/I_ReportExtensions
    association [1..1] to /C2A/C_Reports as _Report on _Report.CdsView = $projection.CdsView
{   
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 10 }]
    key CdsView,
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 20 }]
    key ExtensionName,
    @UI.lineItem: [{ position: 30 }]
    Active,
    @UI.lineItem: [{ position: 40 }]
    ActivatedOn,
    /* Associations */
    _Report
}
