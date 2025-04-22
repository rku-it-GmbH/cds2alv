"! This interface provides access to the database tables of the framework itself.
INTERFACE /c2a/if_persistence PUBLIC.
  "! checks the existence of a report for a CDS View
  "! @parameter i_cds_view | CDS view
  "! @parameter r_exists   | true, if a report for the CDS view exists
  METHODS exists_report_for_cds_view
    IMPORTING i_cds_view      TYPE /c2a/cds_view_name
    RETURNING VALUE(r_exists) TYPE abap_bool.

  "! reads all information of a report for a CDS view.
  "! This includes the report name, generation timestamp, parameters, select options, and the source code.
  "! As of now, this method does not provide information on report extensions.
  "! @parameter i_cds_view            | CDS view
  "! @parameter r_program_info        | collection of all program info
  "! @raising   /c2a/cx_error_message | Occurs when no report exists for the CDS view
  METHODS get_report_for_cds_view
    IMPORTING i_cds_view            TYPE /c2a/cds_view_name
    RETURNING VALUE(r_program_info) TYPE /c2a/report_info
    RAISING   /c2a/cx_error_message.

  "! reads header data of a report for a CDS view.
  "! @parameter i_cds_view            | CDS view
  "! @parameter r_program_header      | header data
  "! @raising   /c2a/cx_error_message | Occurs when no report exists for the CDS view
  METHODS get_report_header
    IMPORTING i_cds_view              TYPE /c2a/cds_view_name
    RETURNING VALUE(r_program_header) TYPE /c2a/report_header
    RAISING   /c2a/cx_error_message.

  "! provides a table of all available targets for Intent-Based navigation.
  "! @parameter r_navigation_table | table of navigation targets
  METHODS get_intent_based_navigation
    RETURNING VALUE(r_navigation_table) TYPE /c2a/navigation_tab.

  "! saves all information of a report for a CDS view.
  "! This includes the report name, generation timestamp, parameters and select options.
  "! Activation of report extensions is done by the method SET_REPORT_EXTENSIONS.
  "! @parameter i_cds_view            | CDS view
  "! @parameter i_program_info        | collection of all program info
  "! @raising   /c2a/cx_error_message | Occurs when the program info cannot be saved
  METHODS save_report_for_cds_view
    IMPORTING i_cds_view     TYPE /c2a/cds_view_name
              i_program_info TYPE /c2a/report_info
    RAISING   /c2a/cx_error_message.

  "! retrieves available report extensions for a CDS view.
  "! @parameter i_cds_view       | CDS view
  "! @parameter i_only_active    | only retrieve active extensions
  "! @parameter i_only_selection | only retrieve extensions providing an alternative selection
  "! @parameter i_only_display   | only retrieve extensions providing an alternative display
  "! @parameter r_extensions     | table of report extensions
  METHODS get_report_extensions
    IMPORTING i_cds_view          TYPE /c2a/cds_view_name
              i_only_active       TYPE abap_bool DEFAULT abap_true
              i_only_selection    TYPE abap_bool DEFAULT abap_false
              i_only_display      TYPE abap_bool DEFAULT abap_false
    RETURNING VALUE(r_extensions) TYPE /c2a/report_extension_tab.

  "! sets the activation flag of report extensions for a CDS view.
  "! @parameter i_cds_view            | CDS view
  "! @parameter i_extensions          | table of report extensions
  "! @raising   /c2a/cx_error_message | Error during database update are propagated
  METHODS set_report_extensions
    IMPORTING i_cds_view   TYPE /c2a/cds_view_name
              i_extensions TYPE zcds_alv_program_extensions
    RAISING   /c2a/cx_error_message.

  "! retrieves a list of additional parameters of a report extension.
  "! It is used by the generation strategy to add these to the selection screen.
  "! @parameter i_extension_name | report extension
  "! @parameter r_parameters     | table of additional parameters for the selection screen
  METHODS get_extension_parameters
    IMPORTING i_extension_name    TYPE /c2a/report_extension_name
    RETURNING VALUE(r_parameters) TYPE /c2a/extension_parameter_tab.

  "! provides a table of all exit classes for intent-based navigation
  "! @parameter r_navigation_exits | table of navigation exit classes
  METHODS get_navigation_exits
    RETURNING VALUE(r_navigation_exits) TYPE /c2a/navigation_exit_tab.

  "! provides the next available number for a generated program
  "! @parameter i_cds_view            | CDS view
  "! @parameter r_number              | next available number
  "! @raising   /c2a/cx_error_message | Error during numbering are propagated
  METHODS get_next_program_number
    IMPORTING i_cds_view      TYPE /c2a/cds_view_name
    RETURNING VALUE(r_number) TYPE /c2a/view_program_counter
    RAISING   /c2a/cx_error_message.
ENDINTERFACE.
