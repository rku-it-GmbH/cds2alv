"! This interface provides several methods to select data of a CDS view from the database.
"! Ths default implentation uses the SADL framework for the selection.
INTERFACE /c2a/if_selection PUBLIC.
  "! Performs the previous selection once again to refresh the data.
  "! @parameter c_result_table        | Results of the database selection
  "! @raising   /c2a/cx_error_message | Errors during database selection are propagated
  METHODS perform_reselection
    CHANGING c_result_table TYPE ANY TABLE
    RAISING  /c2a/cx_error_message.

  "! Performs a database selection with given conditions.
  "! In the default implementation the conditions are entered via the selection screen.
  "! @parameter i_condition_provider  | Condition provider, e.g. selection screen
  "! @parameter e_result_table        | Results of the database selection
  "! @parameter e_number_all_hits     | Number of all entries in the CDS view
  "! @raising   /c2a/cx_error_message | Errors during database selection are propagated
  METHODS perform_selection
    IMPORTING i_condition_provider TYPE REF TO /c2a/if_condition_provider
    EXPORTING e_result_table       TYPE ANY TABLE
              e_number_all_hits    TYPE i
    RAISING   /c2a/cx_error_message.

  "! Performs a database selection with a FORALL table and FORALL conditions derived from a CDS association
  "! @parameter i_condition_provider  | Condition provider, e.g. selection screen
  "! @parameter i_source_view         | source CDS view
  "! @parameter i_association_name    | CDS association
  "! @parameter i_source_parameters   | source CDS view parameters
  "! @parameter i_forall_table        | FORALL table
  "! @parameter e_result_table        | Results of the database selection
  "! @parameter e_number_all_hits     | Number of all entries in the CDS view
  "! @raising   /c2a/cx_error_message | Errors during database selection are propagated
  METHODS perform_selection_forall
    IMPORTING i_condition_provider TYPE REF TO /c2a/if_condition_provider
              i_source_view        TYPE /c2a/cds_view_name
              i_association_name   TYPE ddassociationname
              i_source_parameters  TYPE /c2a/cds_parameter_tab
              i_forall_table       TYPE ANY TABLE
    EXPORTING e_result_table       TYPE ANY TABLE
              e_number_all_hits    TYPE i
    RAISING   /c2a/cx_error_message.
ENDINTERFACE.
