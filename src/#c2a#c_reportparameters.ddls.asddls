@AbapCatalog.sqlViewName: '/C2A/C_REP_PARAM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PARAMETERS in a generated program'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #CONSUMPTION
define view /C2A/C_ReportParameters as select from /C2A/I_ReportParameters
    association [1..1] to /C2A/C_Reports as _Report on _Report.CdsView = $projection.CdsView
{   
    @UI.lineItem: [{ position: 10 }]
    key ProgramName,
    @UI.lineItem: [{ position: 20 }]
    key SelectionName,
    @Consumption.filter.selectionType: #RANGE
    @UI.lineItem: [{ position: 30 }]
    CdsView,
    @UI.lineItem: [{ position: 40 }]
    ParameterName,
    /* Associations */
    _Report
}
