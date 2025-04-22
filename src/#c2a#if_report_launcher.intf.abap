"! This interface provides abilities to launch generated reports for CDS views.
"! In the default implementation the generation is ensured by calling the report generator,
"! if no report exists or the report was generated before the latest modification of the view.
INTERFACE /c2a/if_report_launcher PUBLIC.
  "! starts the report for a CDS view.
  "! @parameter i_cds_view            | CDS view
  "! @parameter i_in_split_screen     | if true, the selection screen is displayed above the ALV grid
  "! @raising   /c2a/cx_error_message | Errors during report processing are propagated
  METHODS start_report_for_view
    IMPORTING i_cds_view        TYPE /c2a/cds_view_name
              i_in_split_screen TYPE abap_bool DEFAULT abap_false
    RAISING   /c2a/cx_error_message.

  "! starts the report for a CDS view using a FORALL table based on an association
  "! that was previously stored in ABAP memory.
  "! @parameter i_source_view         | source CDS view
  "! @parameter i_target_view         | target CDS view
  "! @parameter i_association_name    | CDS association used for the FORALL condition
  "! @parameter i_source_parameters   | source CDS view parameters
  "! @parameter i_target_parameters   | target CDS view parameters
  "! @parameter i_forall_table        | FORALL table
  "! @raising   /c2a/cx_error_message | Errors during report processing are propagated
  METHODS start_report_for_association
    IMPORTING i_source_view       TYPE /c2a/cds_view_name
              i_target_view       TYPE /c2a/cds_view_name
              i_association_name  TYPE ddassociationname
              i_source_parameters TYPE /c2a/cds_parameter_tab
              i_target_parameters TYPE /c2a/cds_parameter_tab
              i_forall_table      TYPE STANDARD TABLE
    RAISING   /c2a/cx_error_message.

  "! Runs the report in dark mode and saves the data and metadata for use in a calling program
  "! The default implementation uses CL_SALV_BS_RUNTIME_INFO to this end
  "! @parameter i_cds_view            | CDS View
  "! @parameter i_variant             | Report variant
  "! @parameter i_selection_table     | Selection Table
  "! @parameter e_ref_to_table        | Reference to the data table
  "! @parameter e_table_descriptor    | Table Descriptor
  "! @parameter e_layout              | ALV layout
  "! @parameter e_field_catalog       | ALV field catalog
  "! @parameter e_filter              | ALV filters
  "! @parameter e_sort                | ALV sort criteria
  "! @raising   /c2a/cx_error_message | Errors during report execution are propagated
  "! @parameter i_parameters          | Selection screen values
  METHODS run_in_dark_mode
    IMPORTING i_cds_view         TYPE /c2a/cds_view_name
              i_variant          TYPE raldb_vari OPTIONAL
              i_selection_table  TYPE rsparams_tt OPTIONAL
    EXPORTING e_ref_to_table     TYPE REF TO data
              e_table_descriptor TYPE REF TO cl_abap_tabledescr
              e_layout           TYPE lvc_s_layo
              e_field_catalog    TYPE lvc_t_fcat
              e_filter           TYPE lvc_t_filt
              e_sort             TYPE lvc_t_sort
    RAISING   /c2a/cx_error_message.
ENDINTERFACE.
