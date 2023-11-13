"! This class provides instances of all classes that are used in the framework.
"! The dependencies are resolved by the factory itself.
INTERFACE zif_cds_alv_factory PUBLIC.
  "! Provides an instance for authority checks
  "! @parameter r_authority_checker | Instance of the authority checker
  "! @raising   zcx_cds_alv_message | Implementation cannot be instantiated
  METHODS get_authority_checker
    RETURNING VALUE(r_authority_checker) TYPE REF TO zif_cds_alv_authority_check
    RAISING   zcx_cds_alv_message.

  "! Provides an adapter for calls to the BOPF framework
  "! @parameter i_cds_view          | The CDS View
  "! @parameter r_bopf_handler      | Instance of the BOPF handler class
  "! @raising   zcx_cds_alv_message | Implementation cannot be instantiated
  METHODS get_bopf_handler
    IMPORTING i_cds_view            TYPE ddstrucobjname
    RETURNING VALUE(r_bopf_handler) TYPE REF TO zif_cds_alv_bopf_handler
    RAISING   zcx_cds_alv_message.

  "! Provides the builder which is used to build the ALV grid and the event handler.
  "! @parameter i_cds_view          | The CDS View
  "! @parameter r_builder           | Instance of the builder class
  "! @raising   zcx_cds_alv_message | Implementation cannot be instantiated
  METHODS get_builder
    IMPORTING i_cds_view       TYPE ddstrucobjname
    RETURNING VALUE(r_builder) TYPE REF TO zif_cds_alv_grid_builder
    RAISING   zcx_cds_alv_message.

  "! Provides access to DDIC information on CDS Views and related objects.
  "! @parameter r_ddic_access       | Instance of the DDIC access class
  "! @raising   zcx_cds_alv_message | Implementation cannot be instantiated
  METHODS get_ddic_access
    RETURNING VALUE(r_ddic_access) TYPE REF TO zif_cds_alv_ddic_access
    RAISING   zcx_cds_alv_message.

  "! Provides a provider for report extensions.
  "! It is implemented as a separate class to separate the creation of core and extension objects.
  "! @parameter r_extension_provider | Instance of the extension provider
  "! @raising   zcx_cds_alv_message  | Implementation cannot be instantiated
  METHODS get_extension_provider
    RETURNING VALUE(r_extension_provider) TYPE REF TO zif_cds_alv_extension_provider
    RAISING   zcx_cds_alv_message.

  "! Provides the strategy for writing the source code and text pool of a report for a CDS View.
  "! @parameter i_cds_view          | The CDS View
  "! @parameter r_strategy          | Instance of the generation strategy
  "! @raising   zcx_cds_alv_message | Implementation cannot be instantiated
  METHODS get_generation_strategy
    IMPORTING i_cds_view        TYPE ddstrucobjname
    RETURNING VALUE(r_strategy) TYPE REF TO zif_cds_alv_report_strategy
    RAISING   zcx_cds_alv_message.

  "! Provides capability to launch the generated reports for CDS views.
  "! If the report does not exist yet, it is generated on the fly.
  "! @parameter r_launcher          | Instance of the launcher class
  "! @raising   zcx_cds_alv_message | Implementation cannot be instantiated
  METHODS get_launcher
    RETURNING VALUE(r_launcher) TYPE REF TO zif_cds_alv_report_launcher
    RAISING   zcx_cds_alv_message.

  "! Provides access to the memory to offer an interface between the separate reports.
  "! For instance, it is used to provide the FORALL table for navigation via association.
  "! @parameter r_memory            | Instance of the memory class
  "! @raising   zcx_cds_alv_message | Implementation cannot be instantiated
  METHODS get_memory
    RETURNING VALUE(r_memory) TYPE REF TO zif_cds_alv_memory
    RAISING   zcx_cds_alv_message.

  "! Provides Intent-Based Navigation and navigation via association.
  "! @parameter r_navigation        | Instance of the navigator class
  "! @raising   zcx_cds_alv_message | Implementation cannot be instantiated
  METHODS get_navigation
    RETURNING VALUE(r_navigation) TYPE REF TO zif_cds_alv_navigation
    RAISING   zcx_cds_alv_message.

  "! Provides access to the persistence which manages the framework's Z tables.
  "! @parameter r_persistence       | Instance of the persistence class
  "! @raising   zcx_cds_alv_message | Implementation cannot be instantiated
  METHODS get_persistence
    RETURNING VALUE(r_persistence) TYPE REF TO zif_cds_alv_persistence
    RAISING   zcx_cds_alv_message.

  "! Provides the controller for the generated report of a CDS View.
  "! @parameter i_cds_view          | The CDS View
  "! @parameter r_controller        | Instance of the controller class
  "! @raising   zcx_cds_alv_message | Implementation cannot be instantiated
  METHODS get_report_controller
    IMPORTING i_cds_view          TYPE ddstrucobjname
    RETURNING VALUE(r_controller) TYPE REF TO zif_cds_alv_report_controller
    RAISING   zcx_cds_alv_message.

  "! Provides a generator, which generates reports for CDS Views based on a generation strategy.
  "! @parameter r_generator         | Instance of the report generator class
  "! @raising   zcx_cds_alv_message | Implementation cannot be instantiated
  METHODS get_report_generator
    RETURNING VALUE(r_generator) TYPE REF TO zif_cds_alv_report_generator
    RAISING   zcx_cds_alv_message.

  "! Provides database access to a CDS View.
  "! In the default implementation the SADL framework is used for the selection.
  "! @parameter i_cds_view          | The CDS View
  "! @parameter r_selection         | Instance of the selection class
  "! @raising   zcx_cds_alv_message | Implementation cannot be instantiated
  METHODS get_selection
    IMPORTING i_cds_view         TYPE ddstrucobjname
    RETURNING VALUE(r_selection) TYPE REF TO zif_cds_alv_selection
    RAISING   zcx_cds_alv_message.

  "! Provides a view instance for the selection screen handling.
  "! @parameter i_cds_view          | The CDS View
  "! @parameter r_selection_screen  | Instance of the selection screen class
  "! @raising   zcx_cds_alv_message | Implementation cannot be instantiated
  METHODS get_selection_screen
    IMPORTING i_cds_view                TYPE ddstrucobjname
    RETURNING VALUE(r_selection_screen) TYPE REF TO zif_cds_alv_selection_screen
    RAISING   zcx_cds_alv_message.

  "! Provides a table container for the contents of a CDS View
  "! @parameter i_cds_view          | The CDS View
  "! @parameter r_table_container   | Instance of the table container
  "! @raising   zcx_cds_alv_message | Implementation cannot be instantiated
  METHODS get_table_container
    IMPORTING i_cds_view               TYPE ddstrucobjname
    RETURNING VALUE(r_table_container) TYPE REF TO zif_cds_alv_table_container
    RAISING   zcx_cds_alv_message.

  "! Provides value help for the elements of the CDS View on the selection screen and in the ALV grid.
  "! @parameter i_cds_view          | The CDS View
  "! @parameter r_value_help        | Instance of the value help class
  "! @raising   zcx_cds_alv_message | Implementation cannot be instantiated
  METHODS get_value_help
    IMPORTING i_cds_view          TYPE ddstrucobjname
    RETURNING VALUE(r_value_help) TYPE REF TO zif_cds_alv_value_help
    RAISING   zcx_cds_alv_message.
ENDINTERFACE.
