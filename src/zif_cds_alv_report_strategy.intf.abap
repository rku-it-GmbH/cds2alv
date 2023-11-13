"! This interface provides methods to write the source and textpool for report generation.
"! It is called by the report generator.
INTERFACE zif_cds_alv_report_strategy PUBLIC.
  "! writes the source code and collects the program info.
  "! @parameter r_program | relevant information on the program, including name, parameters, select-options and source code
  METHODS write_source
    RETURNING VALUE(r_program) TYPE zcds_alv_program_info.

  "! writes the textpool for the generated report
  "! @parameter r_textpool |
  "! @parameter rt_textpool | collection of all text elements
  METHODS write_textpool
    RETURNING VALUE(r_textpool) TYPE zcds_alv_textpool_tab.
ENDINTERFACE.
