"! This interface allows to call different logic instead of display the ALV Grid.
"! For instance, it can be used to store the table as an extract during background processing.
INTERFACE /c2a/if_display_extension PUBLIC.
  "! Calls the alternative display.
  "! It is called during report processing at START-OF-SELECTION instead of the ALV Grid display,
  "! when the respective alternative display is selected on the selection screen.
  "! @parameter i_selection_screen    | provides access to the contents of the selection screen
  "! @parameter i_table_container     | contains the table itself and RTTI descriptors
  "! @parameter i_builder             | ALV Grid Builder
  "! @raising   /c2a/cx_error_message | Allows propagation of errors
  METHODS alternative_display DEFAULT IGNORE
    IMPORTING i_selection_screen TYPE REF TO /c2a/if_selection_screen
              i_table_container  TYPE REF TO /c2a/if_table_container
              i_builder          TYPE REF TO /c2a/if_grid_builder
    RAISING   /c2a/cx_error_message.
ENDINTERFACE.
