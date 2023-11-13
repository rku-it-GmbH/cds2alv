"! This interface provides methods to inrteract with the contents of the selection screen.
"! It is used to make these contents available to the other classes of the framework.
INTERFACE zif_cds_alv_selection_screen PUBLIC.
  INTERFACES zif_cds_alv_condition_provider.

  ALIASES get_parameters FOR zif_cds_alv_condition_provider~get_parameters.
  ALIASES get_selections FOR zif_cds_alv_condition_provider~get_selections.

  "! synchronizes the internal state of the object with the content of the selection screen.
  "! This method needs to be called during AT SELECTION-SCREEN or AT SELECTION-SCREEN OUTPUT
  "! to make sure the object always returns the most recent state.
  "! @raising zcx_cds_alv_message | Errors while reading the selection screen are propagated
  METHODS read_selection_screen
    RAISING zcx_cds_alv_message.

  "! retrieves the content of a parameter or select-option
  "! @parameter i_sel_name       | screen name
  "! @parameter e_parameter      | value of the parameter
  "! @parameter e_select_options | value of the select-options
  METHODS get_dynpro_field
    IMPORTING i_sel_name       TYPE rsscr_name
    EXPORTING e_parameter      TYPE any
              e_select_options TYPE rsds_selopt_t.

  "! sets a new value for a parameter or select option
  "! @parameter i_sel_name          | screen name
  "! @parameter i_parameter         | new value of the parameter
  "! @parameter i_select_options    | new value of the select-options
  "! @raising   zcx_cds_alv_message | Occurs when name or value is invalid
  METHODS set_dynpro_field
    IMPORTING i_sel_name       TYPE rsscr_name
              i_parameter      TYPE any           OPTIONAL
              i_select_options TYPE rsds_selopt_t OPTIONAL
    RAISING   zcx_cds_alv_message.

  "! Applies restrictions to the select-options.
  "! The default implementation evaluates the following CDS annotations:
  "! <br/> - Consumption.filter.selectionType
  "! <br/> - Consumption.filter.multipleSelection
  "! @raising zcx_cds_alv_message |  Occurs when restrictions cannot be applied
  METHODS apply_restriction
    RAISING zcx_cds_alv_message.

  "! Applies new texts to the parameters and select-options.
  "! @raising zcx_cds_alv_message | Occurs when texts cannot be applied
  METHODS apply_selection_texts
    RAISING zcx_cds_alv_message.

  "! Displays a value help dialog for a given screen field.
  "! The default implementation evaluates the following CDS annotations:
  "! <br/> - Consumption.valueHelp
  "! <br/> - Consumption.valueHelpDefinition
  "! <br/>
  "! Furthermore the report extensions are called for value helps for extension parameters.
  "! @parameter i_sel_name          | screen name
  "! @parameter c_value             | value of the current screen field
  "! @raising   zcx_cds_alv_message | Errors during value help processing ar propagated
  METHODS value_help_for_field
    IMPORTING i_sel_name TYPE rsscr_name
    CHANGING  c_value    TYPE any
    RAISING   zcx_cds_alv_message.

  "! Modifies the Screen, extensions are called for custom screen modifications.
  "! @raising zcx_cds_alv_message | Errors during screen modification are propagated
  METHODS modify_screen
    RAISING zcx_cds_alv_message.
ENDINTERFACE.
