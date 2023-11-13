"! This interface serves to create an ALV grid and its event handler
"! based on the annotations of a CDS View.
INTERFACE zif_cds_alv_grid_builder PUBLIC.
  "! creates the ALV grid and its event handler.
  "! @parameter i_container         | the GUI container for the ALV grid
  "! @parameter i_table_container   | the table container with the data to be displayed
  "! @parameter e_alv_grid          | the ALV grid
  "! @raising   zcx_cds_alv_message | Errors during the setup of the ALV grid are propagated
  METHODS create_alv_grid
    IMPORTING i_container       TYPE REF TO cl_gui_container
              i_table_container TYPE REF TO zif_cds_alv_table_container
    EXPORTING e_alv_grid        TYPE REF TO cl_gui_alv_grid
    RAISING   zcx_cds_alv_message.

  "! assembles the metadata for the ALV grid without actually setting it up.
  "! For instance, it is used for the XML export function.
  "! @parameter i_table_container   | the table container
  "! @parameter e_variant           | Display Variant
  "! @parameter e_layout            | ALV layout
  "! @parameter e_fieldcatalog      | ALV field catalog
  "! @parameter e_sort_order        | ALV sort order
  "! @parameter e_filters           | ALV filters
  "! @raising   zcx_cds_alv_message | Errors during metadata assembly are propagated
  METHODS get_metadata
    IMPORTING i_table_container TYPE REF TO zif_cds_alv_table_container
    EXPORTING e_variant         TYPE disvariant
              e_layout          TYPE lvc_s_layo
              e_fieldcatalog    TYPE lvc_t_fcat
              e_sort_order      TYPE lvc_t_sort
              e_filters         TYPE lvc_t_filt
    RAISING   zcx_cds_alv_message.

  "! registers an alternative selection handler to be called by the 'Refresh' function
  "! @parameter i_selection_handler | handler for alternative selection
  METHODS register_alternative_selection
    IMPORTING i_selection_handler TYPE REF TO zif_cds_alv_select_extension.

  METHODS get_gui_title
    RETURNING VALUE(r_title) TYPE sytitle.
ENDINTERFACE.
