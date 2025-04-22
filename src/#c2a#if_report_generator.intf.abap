"! This interface is responsible for genrating reports for CDS views.
"! The default implementation uses a geneartion strategy to write source and textpool
"! and handles the generation and persistence.
INTERFACE /c2a/if_report_generator PUBLIC.
  "! generates a report for a CDS view and persists the program information
  "! @parameter i_cds_view            | CDS view
  "! @raising   /c2a/cx_error_message | Errors during report generation are propagated
  METHODS generate_report
    IMPORTING i_cds_view TYPE /c2a/cds_view_name
    RAISING   /c2a/cx_error_message.
ENDINTERFACE.
