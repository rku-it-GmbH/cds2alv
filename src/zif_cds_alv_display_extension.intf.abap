"! This interface allows to call different logic instead of display the ALV Grid.
"! For instance, it can be used to store the table as an extract during background processing.
INTERFACE zif_cds_alv_display_extension PUBLIC.
  "! Calls the alternative display.
  "! It is called during report processing at START-OF-SELECTION instead of the ALV Grid display,
  "! when the respective alternative display is selected on the selection screen.
  "! @parameter i_selection_screen  | provides access to the contents of the selection screen
  "! @parameter i_table_container   | contains the table itself and RTTI descriptors
  "! @parameter i_builder           | ALV Grid Builder
  "! @raising   zcx_cds_alv_message | Allows propagation of errors
  METHODS alternative_display DEFAULT IGNORE
    IMPORTING i_selection_screen TYPE REF TO zif_cds_alv_selection_screen
              i_table_container  TYPE REF TO zif_cds_alv_table_container
              i_builder          TYPE REF TO zif_cds_alv_grid_builder
    RAISING   zcx_cds_alv_message.

ENDINTERFACE.
