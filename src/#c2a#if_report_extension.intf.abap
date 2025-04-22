"! This interface defines possible extension points for reports
"! that are generated using the default generation strategy.
INTERFACE /c2a/if_report_extension PUBLIC.
  INTERFACES /c2a/if_display_extension.
  INTERFACES /c2a/if_selection_extension.

  ALIASES alternative_display     FOR /c2a/if_display_extension~alternative_display.
  ALIASES alternative_reselection FOR /c2a/if_selection_extension~alternative_reselection.
  ALIASES alternative_selection   FOR /c2a/if_selection_extension~alternative_selection.

  DATA extension_name TYPE zcds_alv_report_extension_name READ-ONLY.

  "! event INITIALIZATION <br/>
  "! For instance, this could be used to load user-specific selection variants
  "!
  "! @parameter i_selection_screen    | Selection Screen
  "! @raising   /c2a/cx_error_message | Error in Selection Screen Processing
  METHODS initialization DEFAULT IGNORE
    IMPORTING i_selection_screen TYPE REF TO /c2a/if_selection_screen
    RAISING   /c2a/cx_error_message.

  "! event AT SELECTION-SCREEN OUTPUT <br/>
  "! For instance, this could be used to set an external GUI status
  "!
  "! @parameter i_selection_screen    | Selection Screen
  "! @raising   /c2a/cx_error_message | Error in Selection Screen Processing
  METHODS modify_screen DEFAULT IGNORE
    IMPORTING i_selection_screen TYPE REF TO /c2a/if_selection_screen
    RAISING   /c2a/cx_error_message.

  "! event AT SELECTION-SCREEN <br/>
  "! For instance, this could be used to handle the commands of an external GUI status
  "!
  "! @parameter i_selection_screen    | Selection Screen
  "! @parameter i_user_command        | User Command
  "! @raising   /c2a/cx_error_message | Error in Selection Screen Processing
  METHODS handle_user_command DEFAULT IGNORE
    IMPORTING i_selection_screen TYPE REF TO /c2a/if_selection_screen
              i_user_command     TYPE sy-ucomm
    RAISING   /c2a/cx_error_message.

  "! event AT SELECTION-SCREEN ON HELP-REQUEST <br/>
  "! This can be used to display custom help content.
  "!
  "! @parameter i_selection_screen    | Selection Screen
  "! @parameter i_sel_name            | Screen Name
  "! @raising   /c2a/cx_error_message | Error in Selection Screen Processing
  METHODS show_help DEFAULT IGNORE
    IMPORTING i_selection_screen TYPE REF TO /c2a/if_selection_screen
              i_sel_name         TYPE rsscr_name
    RAISING   /c2a/cx_error_message.

  "! event AT SELECTION-SCREEN ON VALUE-REQUEST <br/>
  "! This can be used to display a custom value help.
  "!
  "! @parameter i_selection_screen    | Selection Screen
  "! @parameter i_sel_name            | Screen Name
  "! @parameter c_value               | Screen Value
  "! @raising   /c2a/cx_error_message | Error in Selection Screen Processing
  METHODS value_help DEFAULT IGNORE
    IMPORTING i_selection_screen TYPE REF TO /c2a/if_selection_screen
              i_sel_name         TYPE rsscr_name
    CHANGING  c_value            TYPE any
    RAISING   /c2a/cx_error_message.
ENDINTERFACE.
