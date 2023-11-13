"! This interface provides value help capabilites for elements of a CDS view.
INTERFACE zif_cds_alv_value_help PUBLIC.
  "! Display a value help for an element of a CDS view.
  "! The default implementation evaluates the following CDS annotations:
  "! <br/> - Consumption.valueHelp
  "! <br/> - Consumption.valueHelpDefinition
  "! @parameter i_fieldname         | CDS view element
  "! @parameter i_parameters        | CDS view parameters
  "! @parameter i_field_ranges      | additional conditions for the value help fields
  "! @parameter i_selected_row      | current row within the ALV grid
  "! @parameter i_display           | if true, the value help is displayed in read-only mode
  "! @parameter e_processed         | true, if a value help for the field exists and was successfully processed
  "! @parameter c_value             | value of the screen field
  "! @raising   zcx_cds_alv_message | Errors during value help processing are propagated
  METHODS value_help_for_element
    IMPORTING i_fieldname    TYPE fieldname
              i_parameters   TYPE zcds_alv_parameters OPTIONAL
              i_field_ranges TYPE rsds_frange_t       OPTIONAL
              i_selected_row TYPE any                 OPTIONAL
              i_display      TYPE abap_bool           DEFAULT abap_false
    EXPORTING e_processed    TYPE abap_bool
    CHANGING  c_value        TYPE any
    RAISING   zcx_cds_alv_message.

  "! Display a value help for a parameter of a CDS view.
  "! The default implementation evaluates the following CDS annotation:
  "! <br/> - Consumption.valueHelpDefinition
  "! @parameter i_parname           | CDS view parameter
  "! @parameter i_display           | if true, the value help is displayed in read-only mode
  "! @parameter e_processed         | true, if a value help for the field exists and was successfully processed
  "! @parameter c_value             | value of the screen field
  "! @raising   zcx_cds_alv_message | Errors during value help processing are propagated
  METHODS value_help_for_parameter
    IMPORTING i_parname   TYPE ddparname
              i_display   TYPE abap_bool DEFAULT abap_false
    EXPORTING e_processed TYPE abap_bool
    CHANGING  c_value     TYPE any
    RAISING   zcx_cds_alv_message.
ENDINTERFACE.
