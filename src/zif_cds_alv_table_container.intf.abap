"! This interface provides capabilities to store a table of data from a CDS view
"! together with additional information like RTTI descriptors and state (editable/read-only).
"! The default implementation handles many special fields used in the ALV grid display (style, color, index, etc.).
INTERFACE zif_cds_alv_table_container PUBLIC.
  "! Returns a reference to the current container content.
  "! @parameter r_ref_to_table | Pointer to the table
  METHODS get_ref_to_table
    RETURNING VALUE(r_ref_to_table) TYPE REF TO data.

  "! Returns the current container content.
  "! The table type can be created using the method GET_TABLE_DESCRIPTOR.
  "! @parameter e_table | The current table
  METHODS get_table
    EXPORTING VALUE(e_table) TYPE STANDARD TABLE.

  "! Sets the content of the table container.
  "! The table must be of the internal type used by the container, which can be created using the method GET_TABLE_DESCRIPTOR.
  "! @parameter i_ref_to_table      | Pointer to the table
  "! @raising   zcx_cds_alv_message | Occurs when the table type does not match the container's expectations.
  METHODS set_ref_to_table
    IMPORTING i_ref_to_table TYPE REF TO data
    RAISING   zcx_cds_alv_message.

  "! Puts a given table into the container.
  "! The table must either be a table of the CDS view's DDIC type
  "! or of the internal type used by the container, which can be created using the method GET_TABLE_DESCRIPTOR.
  "! The table is passed by value to prevent strange behavior that can occur if I_TABLE is the current table.
  "! It should be possible to call this method with the current table to recalculate line indices, criticality, etc.
  "! @parameter i_table             | The table to put into the container
  "! @raising   zcx_cds_alv_message | Occurs when the table type does not match the container's expectations.
  METHODS set_table
    IMPORTING VALUE(i_table) TYPE STANDARD TABLE
    RAISING   zcx_cds_alv_message.

  "! Returns a RTTS descriptor for the internal table type used by the container.
  "! It can be used to create tables as parameters for GET_TABLE or SET_TABLE.
  "! @parameter r_table_descriptor | RTTS descriptor for the internal table type used by the container
  METHODS get_table_descriptor
    RETURNING VALUE(r_table_descriptor) TYPE REF TO cl_abap_tabledescr.

  "! Returns a RTTS descriptor for the internal table line type used by the container.
  "! @parameter r_line_descriptor | RTTS descriptor for the internal table line type used by the container
  METHODS get_line_descriptor
    RETURNING VALUE(r_line_descriptor) TYPE REF TO cl_abap_structdescr.

  "! Returns the names of special columns that are added to the DDIC structure
  "! and are relevant for the ALV Layout and Field Catalog
  "! @parameter r_special_columns | Fieldnames of the special columns
  METHODS get_special_columns
    RETURNING VALUE(r_special_columns) TYPE zcds_alv_special_columns.

  "! Switches between display and change mode.
  "! @parameter i_alv_grid          | The ALV Grid that display the table
  "! @raising   zcx_cds_alv_message | Occurs when Update is not enabled
  METHODS toggle_change_mode
    IMPORTING i_alv_grid TYPE REF TO cl_gui_alv_grid
    RAISING   zcx_cds_alv_message.

  "! Returns whether the table is in edit mode.
  "! @parameter r_in_edit_mode | Table is in edit mode
  METHODS is_in_edit_mode
    RETURNING VALUE(r_in_edit_mode) TYPE abap_bool.
ENDINTERFACE.
