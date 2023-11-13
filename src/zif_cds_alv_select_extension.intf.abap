"! This interface allows a report extension to provide an alternative selection.
"! If it is called from the selection screen it will be executed instead of the selection via SADL.
"! A possible implementation may be loading data from an existing extract.
INTERFACE zif_cds_alv_select_extension PUBLIC.
  "! Alternative selection from the database
  "! @parameter i_selection_screen  | Selection screen
  "! @parameter i_table_container   | Table container
  "! @parameter e_result_table      | Results of the database selection
  "! @parameter e_no_display        | if true, no data will be displayed (e.g. the implementation downloads the data to Excel)
  "! @raising   zcx_cds_alv_message | Errors during alternative selection are propagated
  METHODS alternative_selection DEFAULT IGNORE
    IMPORTING i_cds_view         TYPE ddstrucobjname
              i_selection_screen TYPE REF TO zif_cds_alv_selection_screen
              i_table_container  TYPE REF TO zif_cds_alv_table_container
    EXPORTING e_result_table     TYPE STANDARD TABLE
              e_no_display       TYPE abap_bool
    RAISING   zcx_cds_alv_message.

  "! Alternative selection from the database called by the 'Refresh' function.
  "! @parameter i_cds_view |
  "! @parameter i_selection_screen  | Selection screen
  "! @parameter i_table_container   | Table container
  "! @parameter c_result_table      | Results of the database selection
  "! @raising   zcx_cds_alv_message | Errors during alternative selection are propagated
  METHODS alternative_reselection DEFAULT IGNORE
    IMPORTING i_cds_view         TYPE ddstrucobjname
              i_selection_screen TYPE REF TO zif_cds_alv_selection_screen
              i_table_container  TYPE REF TO zif_cds_alv_table_container
    CHANGING  c_result_table     TYPE STANDARD TABLE
    RAISING   zcx_cds_alv_message.
ENDINTERFACE.
