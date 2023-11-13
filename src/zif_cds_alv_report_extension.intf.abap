"! This interface defines possible extension points for reports
"! that are generated using the default generation strategy.
INTERFACE zif_cds_alv_report_extension PUBLIC.

  INTERFACES zif_cds_alv_display_extension.
  INTERFACES zif_cds_alv_select_extension.

  ALIASES alternative_display     FOR zif_cds_alv_display_extension~alternative_display.
  ALIASES alternative_selection   FOR zif_cds_alv_select_extension~alternative_selection.
  ALIASES alternative_reselection FOR zif_cds_alv_select_extension~alternative_reselection.

  DATA extension_name TYPE zcds_alv_report_extension_name READ-ONLY.

  "! event INITIALIZATION <br/>
  "! For instance, this could be used to load user-specific selection variants
  METHODS initialization DEFAULT IGNORE
    IMPORTING i_selection_screen TYPE REF TO zif_cds_alv_selection_screen
    RAISING   zcx_cds_alv_message.

  "! event AT SELECTION-SCREEN OUTPUT <br/>
  "! For instance, this could be used to set an external GUI status
  METHODS modify_screen DEFAULT IGNORE
    IMPORTING i_selection_screen TYPE REF TO zif_cds_alv_selection_screen
    RAISING   zcx_cds_alv_message.

  "! event AT SELECTION-SCREEN <br/>
  "! For instance, this could be used to handle the commands of an external GUI status
  METHODS handle_user_command DEFAULT IGNORE
    IMPORTING i_selection_screen TYPE REF TO zif_cds_alv_selection_screen
              i_user_command     TYPE sy-ucomm
    RAISING   zcx_cds_alv_message.

  "! event AT SELECTION-SCREEN ON HELP-REQUEST <br/>
  "! This can be used to display custom help content.
  METHODS show_help DEFAULT IGNORE
    IMPORTING i_selection_screen TYPE REF TO zif_cds_alv_selection_screen
              i_sel_name         TYPE rsscr_name
    RAISING   zcx_cds_alv_message.

  "! event AT SELECTION-SCREEN ON VALUE-REQUEST <br/>
  "! This can be used to display a custom value help.
  "!
  "! @parameter i_selection_screen |
  "! @parameter i_sel_name |
  "! @parameter c_value |
  "! @raising zcx_cds_alv_message |
  METHODS value_help DEFAULT IGNORE
    IMPORTING i_selection_screen TYPE REF TO zif_cds_alv_selection_screen
              i_sel_name         TYPE rsscr_name
    CHANGING  c_value            TYPE any
    RAISING   zcx_cds_alv_message.

ENDINTERFACE.
