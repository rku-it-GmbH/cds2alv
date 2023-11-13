"! This interface contains event handling methods
"! for all events of CL_GUI_ALV_GRID that are relevant for the framework.
INTERFACE zif_cds_alv_grid_event_handler PUBLIC.
  DATA grid TYPE REF TO cl_gui_alv_grid READ-ONLY.

  "! FOR EVENT onf1 OF cl_gui_alv_grid
  METHODS on_help_request FOR EVENT onf1 OF cl_gui_alv_grid
    IMPORTING er_event_data es_row_no e_fieldname.

  "! FOR EVENT onf4 OF cl_gui_alv_grid
  METHODS on_value_request FOR EVENT onf4 OF cl_gui_alv_grid
    IMPORTING er_event_data es_row_no et_bad_cells e_display e_fieldname e_fieldvalue.

  "! FOR EVENT data_changed OF cl_gui_alv_grid
  METHODS on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
    IMPORTING er_data_changed e_onf4 e_onf4_after e_onf4_before e_ucomm.

  "! FOR EVENT before_user_command OF cl_gui_alv_grid
  METHODS on_before_user_command FOR EVENT before_user_command OF cl_gui_alv_grid
    IMPORTING e_ucomm.

  "! FOR EVENT user_command OF cl_gui_alv_grid
  METHODS on_user_command FOR EVENT user_command OF cl_gui_alv_grid
    IMPORTING e_ucomm.

  "! FOR EVENT after_user_command OF cl_gui_alv_grid
  METHODS on_after_user_command FOR EVENT after_user_command OF cl_gui_alv_grid
    IMPORTING e_ucomm e_saved e_not_processed.

  "! FOR EVENT double_click OF cl_gui_alv_grid
  METHODS on_double_click FOR EVENT double_click OF cl_gui_alv_grid
    IMPORTING e_row e_column es_row_no.

  "! FOR EVENT context_menu_request OF cl_gui_alv_grid
  METHODS on_context_menu_request FOR EVENT context_menu_request OF cl_gui_alv_grid
    IMPORTING e_object.

  "! FOR EVENT menu_button OF cl_gui_alv_grid
  METHODS on_menu_button FOR EVENT menu_button OF cl_gui_alv_grid
    IMPORTING e_object e_ucomm.

  "! FOR EVENT toolbar OF cl_gui_alv_grid
  METHODS on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
    IMPORTING e_object e_interactive.

  "! FOR EVENT hotspot_click OF cl_gui_alv_grid
  METHODS on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
    IMPORTING e_row_id e_column_id es_row_no.

  "! FOR EVENT after_refresh OF cl_gui_alv_grid.
  METHODS on_after_refresh FOR EVENT after_refresh OF cl_gui_alv_grid.

  "! FOR EVENT button_click OF cl_gui_alv_grid
  METHODS on_button_click FOR EVENT button_click OF cl_gui_alv_grid
    IMPORTING es_col_id es_row_no.

  "! FOR EVENT data_changed_finished OF cl_gui_alv_grid
  METHODS on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
    IMPORTING e_modified et_good_cells.
ENDINTERFACE.
