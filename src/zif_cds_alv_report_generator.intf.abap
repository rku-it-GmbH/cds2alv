"! This interface is responsible for genrating reports for CDS views.
"! The default implementation uses a geneartion strategy to write source and textpool
"! and handles the generation and persistence.
INTERFACE zif_cds_alv_report_generator PUBLIC.
  "! generates a report for a CDS view and persists the program information
  "! @parameter i_cds_view          | CDS view
  "! @raising   zcx_cds_alv_message | Errors during report generation are propagated
  METHODS generate_report
    IMPORTING i_cds_view TYPE ddstrucobjname
    RAISING   zcx_cds_alv_message.
ENDINTERFACE.
