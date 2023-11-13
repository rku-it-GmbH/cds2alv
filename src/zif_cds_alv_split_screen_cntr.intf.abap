"! This interface serves to handle additional user commands from the split screen view.
"! These are largely similar to the user commands of the selection screen in full screen mode,
"! yet they need their own implementation.
INTERFACE zif_cds_alv_split_screen_cntr PUBLIC.
  CONSTANTS:
    "! User commands of the split screen
    BEGIN OF ok_code,
      "! selection (ONLI in full screen mode)
      select TYPE string VALUE 'SELECT' ##NO_TEXT,
    END OF ok_code.

  "! Event handler for the specific user commands of the split screen.
  "! @parameter i_ok_code           | user command
  "! @raising   zcx_cds_alv_message | Errors during comamnd handling are propagated
  METHODS handle_user_command
    IMPORTING i_ok_code TYPE cua_code
    RAISING   zcx_cds_alv_message.
ENDINTERFACE.
